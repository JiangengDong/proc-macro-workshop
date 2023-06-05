use proc_macro::TokenStream;
use proc_macro2::Ident;
use quote::format_ident;
use quote::quote;
use syn::spanned::Spanned;
use syn::AngleBracketedGenericArguments;
use syn::Attribute;
use syn::Field;
use syn::GenericArgument;
use syn::LitStr;
use syn::PathArguments;
use syn::PathSegment;
use syn::{parse_macro_input, DeriveInput};
use syn::{Data, Path, Type, TypePath};

#[proc_macro_derive(Builder, attributes(builder))]
pub fn derive(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as DeriveInput);

    generate(input).into()
}

fn unwrap_single_segment_path(path: &Path) -> Option<&PathSegment> {
    match path {
        Path {
            leading_colon: None,
            segments,
        } if segments.len() == 1 => Some(&segments[0]),
        _ => None,
    }
}

fn split_single_argument_generic(segment: &PathSegment) -> Option<(&Ident, &Type)> {
    match segment {
        PathSegment {
            ident,
            arguments: PathArguments::AngleBracketed(AngleBracketedGenericArguments { args, .. }),
        } if args.len() == 1 => match &args[0] {
            GenericArgument::Type(ty) => Some((ident, ty)),
            _ => None,
        },
        _ => None,
    }
}

fn parse_builder_attr(attr: &Attribute) -> syn::Result<Ident> {
    let mut each_ident = None;
    attr.parse_nested_meta(|meta| {
        if meta.path.is_ident("each") {
            let value = meta.value()?;
            let s = value.parse::<LitStr>()?;
            each_ident = Some(format_ident!("{}", s.value()));
            Ok(())
        } else {
            Err(meta.error(
                r#"expected `builder(each = "...")`"#,
            ))
        }
    })?;
    Ok(each_ident.unwrap())
}

enum FieldKind<'a> {
    Option {
        ident: &'a Ident,
        ty: &'a Type,
    },
    Vec {
        ident: &'a Ident,
        each_ident: Ident,
        ty: &'a Type,
    },
    Other {
        ident: &'a Ident,
        ty: &'a Type,
    },
}

impl<'a> FieldKind<'a> {
    fn from_syn(field: &'a Field) -> syn::Result<Self> {
        let ident = field
            .ident
            .as_ref()
            .expect("Tuple structs are not supported");

        let other_kind = Self::Other {
            ident,
            ty: &field.ty,
        };

        match &field.ty {
            Type::Path(TypePath { qself: None, path }) => {
                if let Some((container, inner)) =
                    unwrap_single_segment_path(path).and_then(split_single_argument_generic)
                {
                    if container == "Option" {
                        Ok(Self::Option { ident, ty: inner })
                    } else if container == "Vec" {
                        let mut builder_attrs = field
                            .attrs
                            .iter()
                            .filter(|attr| attr.path().is_ident("builder"));
                        match (builder_attrs.next(), builder_attrs.next()) {
                            (None, None) => Ok(other_kind),
                            (Some(attr), None) => {
                                parse_builder_attr(attr).map(|each_ident| Self::Vec {
                                    ident,
                                    each_ident,
                                    ty: inner,
                                })
                            }
                            _ => Err(syn::Error::new(
                                field.span(),
                                format!("Found more than one `#[builder(...)]` on field {ident}"),
                            )),
                        }
                    } else {
                        Ok(other_kind)
                    }
                } else {
                    Ok(other_kind)
                }
            }
            Type::Path(_) => Ok(other_kind),
            _ => Err(syn::Error::new(
                field.span(),
                format!("Field {ident}'s type is not supported"),
            )),
        }
    }

    fn ident(&self) -> &Ident {
        match self {
            FieldKind::Option { ident, .. }
            | FieldKind::Vec { ident, .. }
            | FieldKind::Other { ident, .. } => ident,
        }
    }

    fn init_expr(&self) -> proc_macro2::TokenStream {
        match self {
            FieldKind::Option { .. } => quote!(::std::option::Option::None),
            FieldKind::Vec { .. } => quote!(::std::vec::Vec::new()),
            FieldKind::Other { .. } => quote!(::std::option::Option::None),
        }
    }

    fn builder_field_type(&self) -> proc_macro2::TokenStream {
        match self {
            FieldKind::Option { ty, .. } => quote!(::std::option::Option<#ty>),
            FieldKind::Vec { ty, .. } => quote!(::std::vec::Vec<#ty>),
            FieldKind::Other { ty, .. } => quote!(::std::option::Option<#ty>),
        }
    }

    fn setter_function(&self) -> proc_macro2::TokenStream {
        match self {
            FieldKind::Option { ident, ty } => quote! {
                pub fn #ident(&mut self, #ident: #ty) -> &mut Self {
                    self.#ident = ::std::option::Option::Some(#ident);
                    self
                }
            },
            FieldKind::Vec {
                ident,
                each_ident,
                ty,
            } => {
                if *ident == each_ident {
                    quote! {
                        pub fn #ident(&mut self, #ident: #ty) -> &mut Self {
                            self.#ident.push(#ident);
                            self
                        }
                    }
                } else {
                    quote! {
                        pub fn #ident(&mut self, #ident: Vec<#ty>) -> &mut Self {
                            self.#ident = #ident;
                            self
                        }

                        pub fn #each_ident(&mut self, #each_ident: #ty) -> &mut Self {
                            self.#ident.push(#each_ident);
                            self
                        }
                    }
                }
            }
            FieldKind::Other { ident, ty } => quote! {
                pub fn #ident(&mut self, #ident: #ty) -> &mut Self {
                    self.#ident = ::std::option::Option::Some(#ident);
                    self
                }
            },
        }
    }

    fn unwrap_stmt(&self) -> proc_macro2::TokenStream {
        match self {
            FieldKind::Option { ident, .. } => quote! {
                let #ident = self.#ident.take();
            },
            FieldKind::Vec { ident, .. } => quote! {
                let #ident = ::std::mem::take(&mut self.#ident);
            },
            FieldKind::Other { ident, .. } => quote! {
                let #ident = self.#ident.take().ok_or("Missing field")?;
            },
        }
    }
}

fn generate(input: DeriveInput) -> proc_macro2::TokenStream {
    let raw_struct_ident = input.ident;
    let builder_ident = format_ident!("{}Builder", &raw_struct_ident);
    let Data::Struct(data) = input.data else {panic!("Not a struct")};
    match data
        .fields
        .iter()
        .map(FieldKind::from_syn)
        .collect::<syn::Result<Vec<_>>>()
    {
        Ok(fields) => {
            let field_idents = fields.iter().map(|field| field.ident()).collect::<Vec<_>>();
            let field_init_exprs = fields.iter().map(|field| field.init_expr());
            let field_types = fields.iter().map(|field| field.builder_field_type());
            let field_setters = fields.iter().map(|field| field.setter_function());
            let field_unwrap_stmts = fields.iter().map(|field| field.unwrap_stmt());

            quote! {
                impl #raw_struct_ident {
                    pub fn builder() -> #builder_ident {
                        #builder_ident {
                            #(#field_idents: #field_init_exprs),*
                        }
                    }
                }
                pub struct #builder_ident {
                    #(#field_idents: #field_types),*
                }
                impl #builder_ident {
                    #(#field_setters)*

                    pub fn build(&mut self) -> ::std::result::Result<#raw_struct_ident, ::std::boxed::Box<dyn std::error::Error>> {
                        #(#field_unwrap_stmts)*

                        Ok(#raw_struct_ident {
                            #(#field_idents),*
                        })
                    }
                }
            }
        }
        Err(e) => e.to_compile_error(),
    }
}
