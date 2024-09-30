extern crate proc_macro;

use proc_macro::TokenStream;

use quote::{format_ident, quote};
use syn::parse::{Parse, ParseStream};
use syn::{
    parse_macro_input, Attribute, Data, DeriveInput, Fields, GenericArgument, GenericParam,
    Generics, Ident, Path, PathArguments, PathSegment, Token, Type, TypePath,
};

/// Assumes there's no where clause.
fn create_type_from_def(ident: &Ident, generics: &Generics) -> Type {
    let mut args = generics.params.iter().map(|param| match param {
        GenericParam::Lifetime(lf) => GenericArgument::Lifetime(lf.lifetime.clone()),
        GenericParam::Type(ty) => GenericArgument::Type(Type::Path(TypePath {
            qself: None,
            path: Path {
                leading_colon: None,
                segments: vec![PathSegment {
                    ident: ty.ident.clone(),
                    arguments: PathArguments::None,
                }]
                .into_iter()
                .collect(),
            },
        })),
        GenericParam::Const(c) => GenericArgument::Const({
            let ident = &c.ident;
            let expanded = quote! { #ident };

            syn::parse2(expanded).unwrap()
        }),
    });

    if let Some(first) = args.next() {
        syn::parse2(quote! {
            #ident<#first, #(#args),*>
        })
        .unwrap()
    } else {
        syn::parse2(quote! {
            #ident
        })
        .unwrap()
    }
}

#[proc_macro_derive(CloneWithNode)]
pub fn derive_clone_with_node(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as DeriveInput);

    let name = &input.ident;
    let generics = &input.generics;
    let where_clause = &generics.where_clause;

    match &input.data {
        Data::Enum(enum_data) => {
            let variant1 = enum_data.variants.iter().map(|v| {
                let v_name = &v.ident;
                let arg_name = v
                    .fields
                    .iter()
                    .enumerate()
                    .map(|(i, _)| format_ident!("v{i}"));

                let arg_name2 = arg_name.clone();

                let maybe_args = if v.fields.is_empty() {
                    quote! {}
                } else {
                    quote! {(#(#arg_name),*)}
                };

                let maybe_args2 = if v.fields.is_empty() {
                    quote! {}
                } else {
                    quote! {(#(#arg_name2.clone_with_node()),*)}
                };

                quote! {
                    Self::#v_name #maybe_args => Self::#v_name #maybe_args2
                }
            });

            let variant2 = enum_data.variants.iter().map(|v| {
                let v_name = &v.ident;
                let arg_name = v
                    .fields
                    .iter()
                    .enumerate()
                    .map(|(i, _)| format_ident!("v{i}"));

                let arg_name2 = arg_name.clone();

                let maybe_args = if v.fields.is_empty() {
                    quote! {}
                } else {
                    quote! {(#(#arg_name),*)}
                };

                let maybe_args2 = if v.fields.is_empty() {
                    quote! {}
                } else {
                    quote! {(#(#arg_name2.clone_without_node()),*)}
                };

                quote! {
                    Self::#v_name #maybe_args => Self::#v_name #maybe_args2
                }
            });

            let expanded = quote! {
                impl #generics CloneWithNode for #name #generics #where_clause {
                    fn clone_with_node(&mut self) -> Self {
                        match self {
                            #(#variant1),*
                        }
                    }

                    fn clone_without_node(&self) -> Self {
                        match self {
                            #(#variant2),*
                        }
                    }
                }
            };

            expanded.into()
        }
        Data::Struct(struct_data) => {
            let variant1;
            let variant2;

            match &struct_data.fields {
                Fields::Named(fields) => {
                    variant1 = fields.named.iter().map(|f| {
                        let f_name1 = f.ident.clone().unwrap();
                        let f_name2 = f_name1.clone();

                        quote! {
                            #f_name1: self.#f_name2.clone_with_node()
                        }
                    });

                    variant2 = fields.named.iter().map(|f| {
                        let f_name1 = f.ident.clone().unwrap();
                        let f_name2 = f_name1.clone();

                        quote! {
                            #f_name1: self.#f_name2.clone_without_node()
                        }
                    });
                }
                _ => unimplemented!("Only named fields supported."),
            }

            let expanded = quote! {
                impl #generics CloneWithNode for #name #generics #where_clause {
                    fn clone_with_node(&mut self) -> Self {
                        Self {
                            #(#variant1),*
                        }
                    }

                    fn clone_without_node(&self) -> Self {
                        Self {
                            #(#variant2),*
                        }
                    }
                }
            };

            expanded.into()
        }
        _ => panic!("unsupported"),
    }
}

enum ParseAttr {
    Token,
    Standard,
    FirstToken(Type),
}

impl Parse for ParseAttr {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let ident: Ident = input.parse()?;

        Ok(match ident.to_string().as_str() {
            "token" => Self::Token,
            "standard" => Self::Standard,
            "first_token" => {
                let _: Token![=] = input.parse()?;
                Self::FirstToken(input.parse()?)
            }
            &_ => panic!("invalid def"),
        })
    }
}

impl From<&Vec<Attribute>> for ParseAttr {
    fn from(value: &Vec<Attribute>) -> Self {
        value
            .iter()
            .find(|a| a.path().is_ident("parse"))
            .map_or(ParseAttr::Standard, |x| x.parse_args().unwrap())
    }
}

#[proc_macro_derive(Parse, attributes(parse))]
pub fn derive_parse(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as DeriveInput);
    let attr = ParseAttr::from(&input.attrs);

    let name = create_type_from_def(&input.ident, &input.generics);
    let generics = &input.generics;

    let expanded = match &attr {
        ParseAttr::Token => match &input.data {
            Data::Struct(_) => quote! {
                impl #generics Parse for #name {
                    type FirstToken = #name;

                    fn parse<'t, I: Iterator<Item = &'t Token> + Clone>(
                        input: &mut crate::parser::InputStream<'t, I>,
                    ) -> Result<Self, Error> {
                        match input.get_token()? {
                            Token::#name(tok) => Ok(tok.clone()),
                            t => Err(Error::InvalidToken { token: t.clone() }),
                        }
                    }

                    fn get_span(&self) -> Span {
                        self.span
                    }
                }
            },
            Data::Enum(data) => {
                let variant = data.variants.iter().map(|x| &x.ident);

                quote! {
                    impl #generics Parse for #name {
                        type FirstToken = #name;

                        fn parse<'t, I: Iterator<Item = &'t Token> + Clone>(
                            input: &mut crate::parser::InputStream<'t, I>,
                        ) -> Result<Self, Error> {
                            match input.get_token()? {
                                Token::#name(tok) => Ok(tok.clone()),
                                t => Err(Error::InvalidToken { token: t.clone() }),
                            }
                        }

                        fn get_span(&self) -> Span {
                            match self {
                                #(Self::#variant(v) => v.span,)*
                            }
                        }
                    }
                }
            }
            Data::Union(_) => panic!("cannot be a token"),
        },
        ParseAttr::FirstToken(_) | ParseAttr::Standard => match &input.data {
            Data::Enum(data) => {
                let variant = data.variants.iter().map(|x| &x.ident);
                let variant_span = variant.clone();

                let first_token = if let ParseAttr::FirstToken(first) = &attr {
                    quote! {#first}
                } else {
                    let mut variant_type = data.variants.iter().map(|x| match &x.fields {
                        Fields::Unnamed(fields) => &fields.unnamed.first().unwrap().ty,
                        _ => unreachable!(),
                    });

                    let first_type = variant_type.next().unwrap();

                    let mut first_token = quote! {
                        <#first_type as Parse>::FirstToken
                    };

                    for ty in variant_type {
                        first_token = quote! {
                            TokenOr<Maybe<#first_token>, <#ty as Parse>::FirstToken>
                        }
                    }

                    first_token
                };

                quote! {
                    impl #generics Parse for #name {
                        type FirstToken = #first_token;

                        fn parse<'t, I: Iterator<Item = &'t Token> + Clone>(
                            input: &mut crate::parser::InputStream<'t, I>,
                        ) -> Result<Self, Error> {
                            input.expect_token()?;

                            #(
                                if let Some(v) = input.parse()? {
                                    Ok(Self::#variant(v))
                                } else
                            )* {
                                Err(Error::InvalidToken { token: input.get_token()?.clone() })
                            }
                        }

                        fn get_span(&self) -> Span {
                            match self {
                                #(Self::#variant_span(v) => v.get_span(),)*
                            }
                        }
                    }
                }
            }
            Data::Struct(data) => {
                let field = match &data.fields {
                    Fields::Named(fields) => fields.named.iter().map(|x| x.ident.as_ref().unwrap()),
                    _ => unreachable!("not allowed"),
                };
                let field_span = field.clone();

                let first_token = if let ParseAttr::FirstToken(first) = &attr {
                    quote! {#first}
                } else {
                    let mut field_type = match &data.fields {
                        Fields::Named(fields) => fields.named.iter().map(|x| &x.ty),
                        _ => unreachable!("not allowed"),
                    };

                    let first_type = field_type.next().unwrap();

                    let mut first_token = quote! {
                        <#first_type as Parse>::FirstToken
                    };

                    for ty in field_type {
                        first_token = quote! {
                            TokenOr<#first_token, <#ty as Parse>::FirstToken>
                        }
                    }

                    first_token
                };

                quote! {
                    impl #generics Parse for #name {
                        type FirstToken = #first_token;

                        fn parse<'t, I: Iterator<Item = &'t Token> + Clone>(
                            input: &mut crate::parser::InputStream<'t, I>,
                        ) -> Result<Self, Error> {
                            Ok(Self {
                                #(#field: input.parse()?,)*
                            })
                        }

                        fn get_span(&self) -> Span {
                            Span::empty()
                                #(.join(self.#field_span.get_span()))*
                        }
                    }
                }
            }
            _ => todo!(),
        },
    };

    expanded.into()
}
