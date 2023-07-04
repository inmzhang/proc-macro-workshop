use proc_macro::TokenStream;
use quote::{format_ident, quote};
use syn::{parse_macro_input, Data, DataStruct, DeriveInput, Fields, FieldsNamed};

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
        if ty_inner_type("Option", ty).is_some() {
            quote! { #name: #ty }
        } else {
            quote! { #name: ::std::option::Option<#ty> }
        }
    });

    // impl `builder()` method
    let build_empty = fields.iter().map(|f| {
        let name = &f.ident;
        quote! { #name: ::std::option::Option::None }
    });

    // impl set method for each fields
    let set_methods = fields.iter().map(|f|{
        let name = f.ident.as_ref().unwrap();
        let ty = &f.ty;
        let arg_ty = if let Some(inner_ty) = ty_inner_type("Option", ty) {
            inner_ty
        } else {
            ty
        };
        quote! {
            pub fn #name(&mut self, #name: #arg_ty) -> &mut Self {
                self.#name = Some(#name);
                self
            }
        }
    });
    
    // impl `build()` method
    let build_fields = fields.iter().map(|f| {
        let name = &f.ident;
        if ty_inner_type("Option", &f.ty).is_some() {
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
