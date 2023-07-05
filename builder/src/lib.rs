use proc_macro::TokenStream;
use quote::{format_ident, quote};
use syn::{parse_macro_input, Data, DataStruct, DeriveInput, Fields, FieldsNamed, LitStr};

#[proc_macro_derive(Builder, attributes(builder))]
pub fn derive(input: TokenStream) -> TokenStream {
    let ast = parse_macro_input!(input as DeriveInput);

    let name = &ast.ident;
    let builder_ident = format_ident!("{}Builder", name);
    let fields = if let Data::Struct(DataStruct {
        fields: Fields::Named(FieldsNamed { ref named, .. }, ..),
        ..
    }) = ast.data
    {
        named
    } else {
        unimplemented!()
    };

    // define *Builder struct
    let builder_fields = fields.iter().map(|f| {
        let name = &f.ident;
        let ty = &f.ty;
        if ty_inner_type("Option", ty).is_some() || builder_of(f).is_some() {
            quote! { #name: #ty }
        } else {
            quote! { #name: ::std::option::Option<#ty> }
        }
    });

    // impl `builder()` method
    let build_empty = fields.iter().map(|f| {
        let name = &f.ident;
        if builder_of(f).is_some() {
            quote! { #name: ::std::vec::Vec::new() }
        } else {
            quote! { #name: ::std::option::Option::None }
        }
    });

    // impl set method for each fields
    let set_methods = fields.iter().map(|f| {
        let name = f.ident.as_ref().unwrap();
        let ty = &f.ty;
        let (arg_ty, value) = if let Some(inner_ty) = ty_inner_type("Option", ty) {
            (inner_ty, quote! { ::std::option::Option::Some(#name) })
        } else if builder_of(f).is_some() {
            (ty, quote! { #name })
        } else {
            (ty, quote! { ::std::option::Option::Some(#name) })
        };
        let set_method = quote! {
            pub fn #name(&mut self, #name: #arg_ty) -> &mut Self {
                self.#name = #value;
                self
            }
        };

        match extend_method(f) {
            None => set_method,
            Some((true, extend_method)) => extend_method,
            Some((false, extend_method)) => {
                // safe to generate both!
                quote! {
                    #set_method
                    #extend_method
                }
            }
        }
    });

    // impl `build()` method
    let build_fields = fields.iter().map(|f| {
        let name = &f.ident;
        if ty_inner_type("Option", &f.ty).is_some() || builder_of(f).is_some() {
            quote! {
                #name: self.#name.clone()
            }
        } else {
            quote! {
                #name: self.#name.clone().ok_or(concat!(stringify!(#name), " is not set"))?
            }
        }
    });

    // Combine all the generated tokens into a single `proc_macro2::TokenStream`
    TokenStream::from(quote! {
        pub struct #builder_ident {
            #(#builder_fields),*
        }

        impl #name {
            pub fn builder() -> #builder_ident {
                #builder_ident {
                    #(#build_empty),*
                }
            }
        }

        impl #builder_ident {

            #(#set_methods)*

            pub fn build(&mut self) -> ::std::result::Result<#name, ::std::boxed::Box<dyn ::std::error::Error>> {
                ::std::result::Result::Ok(#name {
                    #(#build_fields),*
                })
            }
        }
    })
}

fn ty_inner_type<'a>(wrapper: &str, ty: &'a syn::Type) -> Option<&'a syn::Type> {
    if let syn::Type::Path(ref p) = ty {
        if p.path.segments.len() != 1 || p.path.segments[0].ident != wrapper {
            return None;
        }

        if let syn::PathArguments::AngleBracketed(ref inner_ty) = p.path.segments[0].arguments {
            if inner_ty.args.len() != 1 {
                return None;
            }

            let inner_ty = inner_ty.args.first().unwrap();
            if let syn::GenericArgument::Type(ref t) = inner_ty {
                return Some(t);
            }
        }
    }
    None
}

fn builder_of(f: &syn::Field) -> Option<&syn::Attribute> {
    f.attrs.iter().find(|&attr| attr.path().is_ident("builder"))
}

fn extend_method(f: &syn::Field) -> Option<(bool, proc_macro2::TokenStream)> {
    let name = f.ident.as_ref().unwrap();
    let g = builder_of(f)?;

    let mut extend_name: LitStr = LitStr::new("", proc_macro2::Span::call_site());
    match g.parse_nested_meta(|meta| {
        if meta.path.is_ident("each") {
            let value = meta.value()?;
            let s: LitStr = value.parse()?;
            extend_name = s;
            Ok(())
        } else {
            Err(syn::Error::new_spanned(
                meta.path,
                "expected `builder(each = \"...\")`",
            ))
        }
    }) {
        Err(e) => Some((false, e.to_compile_error())),
        Ok(()) => {
            let inner_ty = ty_inner_type("Vec", &f.ty).unwrap();
            let extend_ident = syn::Ident::new(&extend_name.value(), extend_name.span());
            let method = quote! {
                pub fn #extend_ident(&mut self, #extend_ident: #inner_ty) -> &mut Self {
                    self.#name.push(#extend_ident);
                    self
                }
            };
            Some((&extend_ident == name, method))
        }
    }
}
