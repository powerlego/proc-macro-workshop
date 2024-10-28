use std::usize;

use proc_macro::TokenStream;
use quote::{format_ident, quote, ToTokens};
use syn::parse::{Parse, Parser};
use syn::parse_quote;
use syn::spanned::Spanned;
mod bits;
mod gen;

#[proc_macro_attribute]
pub fn bitfield(_args: TokenStream, input: TokenStream) -> TokenStream {
    let input = syn::parse_macro_input!(input as syn::Item);

    TokenStream::from(bits::expand(input))
}

#[proc_macro]
pub fn gen(_: TokenStream) -> TokenStream {
    TokenStream::from(gen::generate())
}

#[proc_macro_derive(Basic)]
pub fn derive_basic(input: TokenStream) -> TokenStream {
    let ast = syn::parse_macro_input!(input as syn::DeriveInput);
    let syn::Data::Struct(_) = &ast.data
    else {
        return TokenStream::from(
            syn::Error::new(proc_macro2::Span::call_site(), "Struct is required").to_compile_error(),
        );
    };

    let name = &ast.ident;
    let generics = &ast.generics;

    if generics.type_params().count() > 0 {
        return TokenStream::from(
            syn::Error::new(proc_macro2::Span::call_site(), "Type generics are not allowed").to_compile_error(),
        );
    }
    if !generics
        .const_params()
        .all(|param| param.ident == "WIDTH" || param.ident == "ACC" || param.ident == "SIZE")
    {
        return TokenStream::from(
            syn::Error::new(
                proc_macro2::Span::call_site(),
                "WIDTH, ACC, and SIZE const generics required",
            )
            .to_compile_error(),
        );
    }

    let (impl_generics, ty_generics, where_clause) = generics.split_for_impl();

    quote! {
        impl #impl_generics Basic<WIDTH, ACC> for #name #ty_generics #where_clause {}
    }
    .into()
}

macro_rules! format_err {
    ($obj:expr, $($format:tt)+) => {{
        let msg = format!($($format)+);
        syn::Error::new_spanned($obj.to_token_stream(),msg)
    }};
}

macro_rules! abort {
    ($obj:expr, $($format:tt)+) => {{
        return Err(format_err!($obj, $($format)+));
    }};
}

#[derive(Clone)]
enum AttrValue {
    LitStr(syn::LitStr),
    Expr(syn::Expr),
}

#[derive(Clone)]
struct SetGetAttr {
    name:  syn::Ident,
    value: AttrValue,
}

impl Parse for SetGetAttr {
    fn parse(input: syn::parse::ParseStream<'_>) -> syn::Result<Self> {
        let name: syn::Ident = input.parse()?;
        let value = if input.peek(syn::Token![=]) {
            // `name = value` attributes.
            let assign_token = input.parse::<syn::Token![=]>()?; // skip '='
            if input.peek(syn::LitStr) {
                let lit: syn::LitStr = input.parse()?;
                AttrValue::LitStr(lit)
            }
            else {
                match input.parse::<syn::Expr>() {
                    Ok(expr) => AttrValue::Expr(expr),

                    Err(_) => abort! {
                        assign_token,
                        "expected `string literal` or `expression` after `=`"
                    },
                }
            }
        }
        else {
            return Err(syn::Error::new(
                proc_macro2::Span::call_site(),
                "Expected name value pairs",
            ));
        };

        Ok(Self { name, value })
    }
}

#[proc_macro_derive(SetGet, attributes(set_get))]
pub fn derive_set_get(input: TokenStream) -> TokenStream {
    let ast = syn::parse_macro_input!(input as syn::DeriveInput);
    let syn::Data::Struct(_) = &ast.data
    else {
        return TokenStream::from(
            syn::Error::new(proc_macro2::Span::call_site(), "Struct is required").to_compile_error(),
        );
    };

    let name = &ast.ident;

    if name.clone().to_string().split("U").count() != 2 {
        return TokenStream::from(syn::Error::new(name.span(), "Size specifier required").to_compile_error());
    }

    match name.clone().to_string().split("U").collect::<Vec<_>>()[1].parse::<usize>() {
        Ok(size) => {
            let ident_u = format_ident!("u{}", size);
            let generics = &ast.generics;

            if generics.type_params().count() > 0 {
                return TokenStream::from(
                    syn::Error::new(proc_macro2::Span::call_site(), "Type generics are not allowed").to_compile_error(),
                );
            }

            if !generics
                .const_params()
                .all(|param| param.ident == "WIDTH" || param.ident == "ACC" || param.ident == "SIZE")
            {
                return TokenStream::from(
                    syn::Error::new(
                        proc_macro2::Span::call_site(),
                        "WIDTH, ACC, and SIZE const generics required",
                    )
                    .to_compile_error(),
                );
            }

            let (impl_generics, ty_generics, where_clause) = generics.split_for_impl();
            let mut range_alt = syn::Expr::from(
                syn::ExprRange::parse
                    .parse2(quote! {Self::RANGE_LHS..=Self::RANGE_RHS})
                    .unwrap(),
            );
            let mut set_across: syn::ExprBlock = parse_quote!(
                {
                    let p = &mut arr[Self::RANGE_ACROSS];
                    let num_old = ::core::primitive::#ident_u::from_ne_bytes(p.try_into().unwrap());
                    let num_new = num_old & !Self::LIMIT | (num << Self::OFFSET);
                    p.copy_from_slice(&num_new.to_ne_bytes());

                    let num_end = (num >> (#size - Self::OFFSET)) as u8;
                    Self::across_end(arr, num_end);
                }
            );

            let mut set_no_across: syn::ExprBlock = parse_quote!(
                {
                    let p = &mut arr[Self::RANGE];
                    let num_old = ::core::primitive::#ident_u::from_ne_bytes(p.try_into().expect(ERR));
                    let num_new = num_old & !Self::LIMIT | (num << Self::OFFSET);
                    p.copy_from_slice(&num_new.to_ne_bytes());
                }
            );

            let mut get_across: syn::ExprBlock = parse_quote!(
                {
                    let (num_start, num_end) = Self::get_across_helper(arr);
                    let num_start = (::core::primitive::#ident_u::from_ne_bytes(num_start) & Self::LIMIT) >> Self::OFFSET;
                    let num_end = (num_end as ::core::primitive::#ident_u & (::core::primitive::#ident_u::MAX >> (#size - Self::OFFSET_END_))) << Self::OFFSET_END;
                    num_start | num_end
                }
            );
            let mut get_no_across: syn::ExprBlock = parse_quote!(
                {
                    let num = ::core::primitive::#ident_u::from_ne_bytes(arr[Self::RANGE].try_into().expect(ERR));
                    (num & Self::LIMIT) >> Self::OFFSET
                }
            );

            if let Some(attr) = ast
                .attrs
                .iter()
                .find(|attr| attr.path().segments.iter().any(|segment| segment.ident == "set_get"))
            {
                for mut att in
                    attr.parse_args_with(syn::punctuated::Punctuated::<SetGetAttr, syn::Token![,]>::parse_terminated)?
                {
                    if att.name == "range_alt" {
                        match att.value {
                            AttrValue::Expr(expr) => {}
                            AttrValue::LitStr(lit_str) => {
                                if let Ok(rng) = syn::ExprRange::parse.parse_str(lit_str.value().as_str()) {
                                    range_alt = syn::Expr::from(rng);
                                }
                                else if let Ok(if_expr) = syn::ExprIf::parse.parse_str(lit_str.value().as_str()) {
                                    range_alt = syn::Expr::from(if_expr);
                                }
                                else {
                                    return TokenStream::from(
                                        syn::Error::new(attr.span(), "Expected expression").to_compile_error(),
                                    );
                                }
                            }
                        }
                    }
                }
                if let Err(e) = attr.parse_nested_meta(|meta| {
                    dbg!(&meta.path.segments);
                    Ok(())
                }) {
                    return TokenStream::from(e.to_compile_error());
                }
            }

            // for attr in &ast.attrs.clone() {
            //     if attr.path().is_ident("range_alt") {
            //         if let syn::Meta::NameValue(name_value) = &attr.meta {
            //             if let syn::Expr::Lit(expr) = &name_value.value {
            //                 if let syn::Lit::Str(lit_str) = &expr.lit {
            //                     if let Ok(rng) = syn::ExprRange::parse.parse_str(lit_str.value().as_str()) {
            //                         range_alt = syn::Expr::from(rng);
            //                     }
            //                     else if let Ok(if_expr) = syn::ExprIf::parse.parse_str(lit_str.value().as_str()) {
            //                         range_alt = syn::Expr::from(if_expr);
            //                     }
            //                     else {
            //                         return TokenStream::from(
            //                             syn::Error::new(expr.span(), "Expected expression").to_compile_error(),
            //                         );
            //                     }
            //                 }
            //                 else {
            //                     return TokenStream::from(
            //                         syn::Error::new(expr.span(), "Expected string literal").to_compile_error(),
            //                     );
            //                 }
            //             }
            //             else {
            //                 return TokenStream::from(
            //                     syn::Error::new(name_value.span(), "Expected literal").to_compile_error(),
            //                 );
            //             }
            //         }
            //         else {
            //             return TokenStream::from(
            //                 syn::Error::new(attr.meta.span(), "expected `range_alt = \"...\"`").to_compile_error(),
            //             );
            //         }
            //     }
            //     else if attr.path().is_ident("set_across") {
            //         if let syn::Meta::NameValue(name_value) = &attr.meta {
            //             if let syn::Expr::Lit(expr) = &name_value.value {
            //                 if let syn::Lit::Str(lit_str) = &expr.lit {
            //                     if let Ok(block) = syn::ExprBlock::parse.parse_str(lit_str.value().as_str()) {
            //                         set_across = block;
            //                     }
            //                     else {
            //                         return TokenStream::from(
            //                             syn::Error::new(expr.span(), "Expected block expression").to_compile_error(),
            //                         );
            //                     }
            //                 }
            //                 else {
            //                     return TokenStream::from(
            //                         syn::Error::new(expr.span(), "Expected string literal").to_compile_error(),
            //                     );
            //                 }
            //             }
            //             else {
            //                 return TokenStream::from(
            //                     syn::Error::new(name_value.span(), "Expected literal").to_compile_error(),
            //                 );
            //             }
            //         }
            //         else {
            //             return TokenStream::from(
            //                 syn::Error::new(attr.meta.span(), "expected `set_across = \"...\"`").to_compile_error(),
            //             );
            //         }
            //     }
            //     else if attr.path().is_ident("set_no_across") {
            //         if let syn::Meta::NameValue(name_value) = &attr.meta {
            //             if let syn::Expr::Lit(expr) = &name_value.value {
            //                 if let syn::Lit::Str(lit_str) = &expr.lit {
            //                     if let Ok(block) = syn::ExprBlock::parse.parse_str(lit_str.value().as_str()) {
            //                         set_no_across = block;
            //                     }
            //                     else {
            //                         return TokenStream::from(
            //                             syn::Error::new(expr.span(), "Expected block expression").to_compile_error(),
            //                         );
            //                     }
            //                 }
            //                 else {
            //                     return TokenStream::from(
            //                         syn::Error::new(expr.span(), "Expected string literal").to_compile_error(),
            //                     );
            //                 }
            //             }
            //             else {
            //                 return TokenStream::from(
            //                     syn::Error::new(name_value.span(), "Expected literal").to_compile_error(),
            //                 );
            //             }
            //         }
            //         else {
            //             return TokenStream::from(
            //                 syn::Error::new(attr.meta.span(), "expected `set_no_across =
            // \"...\"`").to_compile_error(),             );
            //         }
            //     }
            //     else if attr.path().is_ident("get_across") {
            //         if let syn::Meta::NameValue(name_value) = &attr.meta {
            //             if let syn::Expr::Lit(expr) = &name_value.value {
            //                 if let syn::Lit::Str(lit_str) = &expr.lit {
            //                     if let Ok(block) = syn::ExprBlock::parse.parse_str(lit_str.value().as_str()) {
            //                         get_across = block;
            //                     }
            //                     else {
            //                         return TokenStream::from(
            //                             syn::Error::new(expr.span(), "Expected block expression").to_compile_error(),
            //                         );
            //                     }
            //                 }
            //                 else {
            //                     return TokenStream::from(
            //                         syn::Error::new(expr.span(), "Expected string literal").to_compile_error(),
            //                     );
            //                 }
            //             }
            //             else {
            //                 return TokenStream::from(
            //                     syn::Error::new(name_value.span(), "Expected literal").to_compile_error(),
            //                 );
            //             }
            //         }
            //         else {
            //             return TokenStream::from(
            //                 syn::Error::new(attr.meta.span(), "expected `get_across = \"...\"`").to_compile_error(),
            //             );
            //         }
            //     }
            //     else if attr.path().is_ident("get_no_across") {
            //         if let syn::Meta::NameValue(name_value) = &attr.meta {
            //             if let syn::Expr::Lit(expr) = &name_value.value {
            //                 if let syn::Lit::Str(lit_str) = &expr.lit {
            //                     if let Ok(block) = syn::ExprBlock::parse.parse_str(lit_str.value().as_str()) {
            //                         get_no_across = block;
            //                     }
            //                     else {
            //                         return TokenStream::from(
            //                             syn::Error::new(expr.span(), "Expected block expression").to_compile_error(),
            //                         );
            //                     }
            //                 }
            //                 else {
            //                     return TokenStream::from(
            //                         syn::Error::new(expr.span(), "Expected string literal").to_compile_error(),
            //                     );
            //                 }
            //             }
            //             else {
            //                 return TokenStream::from(
            //                     syn::Error::new(name_value.span(), "Expected literal").to_compile_error(),
            //                 );
            //             }
            //         }
            //         else {
            //             return TokenStream::from(
            //                 syn::Error::new(attr.meta.span(), "expected `get_no_across =
            // \"...\"`").to_compile_error(),             );
            //         }
            //     }
            // }

            quote! {
                impl #impl_generics SetGet for #name #ty_generics #where_clause {
                    type Target = ::core::primitive::#ident_u;

                    const ACROSS: bool = Self::RANGE_BITS > ::core::primitive::#ident_u::BITS;
                    const GET: fn(&[u8]) -> ::core::primitive::#ident_u = if Self::ACROSS {
                        Self::get_across
                    }
                    else {
                        Self::get_no_across
                    };
                    const LIMIT: ::core::primitive::#ident_u = (::core::primitive::#ident_u::MAX >> (#size - WIDTH)) << Self::OFFSET;
                    const RANGE_ACROSS: std::ops::RangeInclusive<usize> = Self::RANGE_LHS..=(Self::RANGE_RHS - 1);
                    const RANGE_ALT: std::ops::RangeInclusive<usize> = #range_alt;
                    const RANGE_RHS2: std::ops::RangeInclusive<usize> = Self::RANGE_RHS..=Self::RANGE_RHS;

                    const SET: fn(&mut [u8], ::core::primitive::#ident_u) = if Self::ACROSS {
                        Self::set_across
                    }
                    else {
                        Self::set_no_across
                    };
                    const U8_MAX_OFFSET: u8 = !(u8::MAX >> (8 - Self::OFFSET_END_));

                    fn set_across(arr: &mut [u8], num: ::core::primitive::#ident_u) #set_across

                    fn set_no_across(arr: &mut [u8], num: ::core::primitive::#ident_u) #set_no_across

                    fn get_across(arr: &[u8]) -> ::core::primitive::#ident_u #get_across

                    fn get_no_across(arr: &[u8]) -> ::core::primitive::#ident_u #get_no_across
                }

            }.into()
        }
        Err(e) => TokenStream::from(syn::Error::new(name.span(), e).to_compile_error()),
    }
}
