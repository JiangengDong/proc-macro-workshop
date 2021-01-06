use core::panic;

use proc_macro::TokenStream;
use proc_macro2::{Ident, Span};
use quote::quote;
use syn::{parse_macro_input, Data, DataStruct, DeriveInput, Fields};

#[proc_macro_derive(Builder)]
pub fn derive(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as DeriveInput);
    let struct_name = &input.ident;
    let builder_name = Ident::new(&format!("{}Builder", struct_name), Span::call_site());

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
        }
    };

    let output = quote! {
        #struct_impl
        #builder_define
        #builder_impl
    };

    output.into()
}
