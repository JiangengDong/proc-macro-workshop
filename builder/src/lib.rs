use core::panic;

use proc_macro::TokenStream;
use quote::{format_ident, quote};
use syn::{parse_macro_input, Data, DataStruct, DeriveInput, Fields};

#[proc_macro_derive(Builder)]
pub fn derive(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as DeriveInput);
    let struct_name = &input.ident;
    let builder_name = format_ident!("{}Builder", struct_name);

    let fields = match &input.data {
        Data::Struct(DataStruct {
            fields: Fields::Named(fields),
            ..
        }) => &fields.named,
        _ => panic!("expected a struct with named fields"),
    };
    let field_names = fields.iter().map(|field| &field.ident).collect::<Vec<_>>();
    let field_types = fields.iter().map(|field| &field.ty).collect::<Vec<_>>();

    let struct_impl = quote! {
        impl #struct_name {
            pub fn builder() -> #builder_name {
                #builder_name {
                    #(#field_names: None),*
                }
            }
        }
    };
    let builder_define = quote! {
        pub struct #builder_name {
            #(#field_names: Option<#field_types>),*
        }
    };
    let builder_impl = quote! {
        impl #builder_name {
            #(
                pub fn #field_names(&mut self, #field_names: #field_types) -> &mut Self {
                    self.#field_names = Some(#field_names);
                    self
                }

            )*
            pub fn build(&self) -> Result<#struct_name, Box<dyn std::error::Error>> {
                #(
                    if self.#field_names.is_none() {
                        Err(format!("{} is not defined. ", stringify!(#field_names)).into())
                    } else
                )* {
                    Ok(#struct_name {
                        #(#field_names: self.#field_names.to_owned().unwrap()),*
                    })
                }
            }
        }
    };

    let output = quote! {
        #struct_impl

        #builder_define

        #builder_impl
    };

    output.into()
}
