use proc_macro::TokenStream;
use quote::{format_ident, quote};
use syn::{parse_macro_input, Data, DataStruct, DeriveInput, Fields, Type};

#[proc_macro_derive(Builder, attributes(builder))]
pub fn derive(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as DeriveInput);

    let name = input.ident;
    let builder_ident = format_ident!("{}Builder", &name);
    if let Data::Struct(DataStruct {
        fields: Fields::Named(named_fields, ..),
        ..
    }) = input.data
    {
        let fields_ident_ty = named_fields.named.iter().map(|f| (f.ident.as_ref(), &f.ty));
        let fields_ident = fields_ident_ty.clone().map(|f| f.0.unwrap());

        // define *Builder struct
        let builder_fields = fields_ident_ty.clone().map(|field| {
            let name = field.0.unwrap();
            let ty = field.1;
            let opt_ty = is_optional(ty).unwrap_or(ty);
            quote! {
                #name: Option<#opt_ty>
            }
        });

        let builder_struct = quote! {
            pub struct #builder_ident {
                #(#builder_fields),*
            }
        };

        // impl `builder()` method
        let idents = fields_ident.clone();
        let builder_method = quote! {
            impl #name {
                pub fn builder() -> #builder_ident {
                    #builder_ident {
                        #(#idents: None),*
                    }
                }
            }
        };

        // impl methods on the builder for setting a value of each of the struct fields
        let set_methods_iterator = fields_ident_ty.clone().map(|field| {
            let name = field.0.unwrap();
            let ty = field.1;
            let ty = is_optional(ty).unwrap_or(ty);
            quote! {
                fn #name(&mut self, #name: #ty) -> &mut Self {
                    self.#name = Some(#name);
                    self
                }
            }
        });

        // impl `build()` method
        let optional_idents = fields_ident_ty.clone().filter_map(|field| {
            let name = field.0.unwrap();
            let ty = field.1;
            is_optional(ty).map(|_| name)
        });
        let nonoptional_idents = fields_ident_ty.clone().filter_map(|field| {
            let name = field.0.unwrap();
            let ty = field.1;
            if is_optional(ty).is_some() {
                None
            } else {
                Some(name)
            }
        });
        let build_method = quote! {
            pub fn build(&mut self) -> Result<#name, Box<dyn std::error::Error>> {
                Ok(#name {
                    #(#nonoptional_idents: self.#nonoptional_idents.clone().take().ok_or("missing field")?),*,
                    #(#optional_idents: self.#optional_idents.clone()),*
                })
            }
        };

        let set_methods = quote! {
            impl #builder_ident {
                #(#set_methods_iterator)*
                #build_method
            }
        };

        // Combine all the generated tokens into a single `proc_macro2::TokenStream`
        TokenStream::from(quote! {
            #builder_struct
            #builder_method
            #set_methods
        })
    } else {
        panic!("Only structs with named fields are supported")
    }
}

fn is_optional(ty: &Type) -> Option<&Type> {
    if let syn::Type::Path(type_path) = ty {
        let path = &type_path.path;
        if path.segments.len() != 1 {
            return None;
        }
        if let Some(segment) = path.segments.last() {
            if segment.ident == "Option" {
                if let syn::PathArguments::AngleBracketed(args) = &segment.arguments {
                    if args.args.len() != 1 {
                        return None;
                    }
                    if let syn::GenericArgument::Type(ty) = args.args.first().unwrap() {
                        return Some(ty);
                    }
                }
            }
        }
    }
    None
}
