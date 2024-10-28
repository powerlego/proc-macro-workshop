use proc_macro::TokenStream;
use quote::quote;
use syn::spanned::Spanned;
use syn::{parse_macro_input, parse_quote, DeriveInput};

#[proc_macro_derive(CustomDebug, attributes(debug))]
pub fn derive(input: TokenStream) -> TokenStream {
    let ast = parse_macro_input!(input as DeriveInput);
    if let syn::Data::Struct(syn::DataStruct {
        fields: syn::Fields::Named(syn::FieldsNamed { named, .. }),
        ..
    }) = ast.data
    {
        let ident = ast.ident.clone();

        let mut gen_idents = vec![];
        ast.generics.params.iter().for_each(|generic_param| {
            if let syn::GenericParam::Type(syn::TypeParam { ident, .. }) = generic_param {
                gen_idents.push(ident.clone());
            }
        });

        let mut ass_types = vec![];
        let mut field_formats: Vec<syn::ExprCall> = vec![];
        let mut phantom_data = vec![];
        let mut phantom_data_types: Vec<syn::TypePath> = vec![];
        for ty in named.iter().map(|field| &field.ty) {
            if let Some(tt) = get_assosiate_type(ty, &mut gen_idents.iter()) {
                ass_types.push(tt);
            }
            if let Some(syn::Type::Path(syn::TypePath {
                path: syn::Path { segments, .. },
                ..
            })) = get_inner(Some("PhantomData"), ty)
            {
                let seg_ident = &segments[0].ident;
                phantom_data.push(seg_ident);

                phantom_data_types.push(parse_quote! {
                    PhantomData<#seg_ident>
                });
            }
            else if let Some(syn::Type::Reference(syn::TypeReference {
                elem,
                lifetime,
                mutability,
                ..
            })) = get_inner(Some("PhantomData"), ty)
            {
                if let syn::Type::Path(ref type_path) = **elem {
                    let seg_ident = &type_path.path.segments[0].ident;
                    phantom_data.push(seg_ident);
                    phantom_data_types.push(parse_quote! {
                        PhantomData<&#lifetime #mutability #seg_ident>
                    });
                }
                else {
                    return TokenStream::from(
                        syn::Error::new(elem.span(), "Could not find type path.").to_compile_error(),
                    );
                }
            }
        }
        let mut has_bound_attr = false;
        let mut where_preds = vec![];
        if let Some(attr) = ast
            .attrs
            .iter()
            .find(|attr| attr.path().segments.iter().any(|segment| segment.ident == "debug"))
        {
            match attr.parse_nested_meta(|meta| {
                if meta.path.segments.len() > 1
                    || meta
                        .path
                        .segments
                        .iter()
                        .any(|path_segment| path_segment.ident != "bound")
                {
                    return Err(meta.error("expected `debug(bound = \"...\")`"));
                }
                let lit_str: syn::LitStr = meta.value()?.parse()?;
                has_bound_attr = true;
                where_preds.push(syn::parse_str(&lit_str.value())?);

                Ok(())
            }) {
                Ok(_) => (),
                Err(e) => return TokenStream::from(e.to_compile_error()),
            }
        }

        let generics = add_trait_bounds(ast.generics, &phantom_data, &ass_types, has_bound_attr);
        let (impl_generics, ty_generics, where_clause) = generics.split_for_impl();
        if !has_bound_attr {
            if let Some(w) = where_clause {
                w.predicates.iter().for_each(|pred| where_preds.push(pred.clone()));
            }
            else if !ass_types.is_empty() {
                ass_types.iter().for_each(|ass_type| {
                    where_preds.push(parse_quote! {
                        #ass_type: std::fmt::Debug
                    });
                });
            }
            else if !phantom_data.is_empty() {
                phantom_data_types.iter().for_each(|ty| {
                    where_preds.push(parse_quote! {
                        #ty: std::fmt::Debug
                    });
                });
            }
        }

        for field in named.clone().iter() {
            let field_ident = field.ident.as_ref().expect("Field name not found");
            if field.attrs.len() == 0 {
                field_formats.push(parse_quote! {
                    field(std::stringify!(#field_ident), &self.#field_ident)
                });
            }
            else {
                for attr in field.attrs.clone() {
                    if attr.path().is_ident("debug") {
                        if let syn::Meta::NameValue(name_value) = &attr.meta {
                            if let syn::Expr::Lit(expr) = &name_value.value {
                                if let syn::Lit::Str(lit_str) = &expr.lit {
                                    field_formats.push(parse_quote! {
                                    field(std::stringify!(#field_ident), &std::format_args!(#lit_str,self.#field_ident))
                                });
                                }
                                else {
                                    return TokenStream::from(
                                        syn::Error::new(expr.span(), "Expected string literal").to_compile_error(),
                                    );
                                }
                            }
                            else {
                                return TokenStream::from(
                                    syn::Error::new(name_value.span(), "Expected literal").to_compile_error(),
                                );
                            }
                        }
                        else {
                            return TokenStream::from(
                                syn::Error::new(attr.meta.span(), "expected `debug = \"...\"`").to_compile_error(),
                            );
                        }
                    }
                    else {
                        field_formats.push(parse_quote! {
                            field(std::stringify!(#field_ident), &self.#field_ident)
                        });
                    }
                }
            }
        }

        let new_where_clause: syn::WhereClause = parse_quote! {
            where #(#where_preds,)*
        };

        quote! {
            impl #impl_generics std::fmt::Debug for #ident #ty_generics #new_where_clause {
                fn fmt(&self, fmt: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                    fmt.debug_struct(stringify!(#ident))
                    #(.#field_formats)*
                    .finish()
                }
            }
        }
        .into()
    }
    else {
        TokenStream::from(syn::Error::new(ast.span(), "Named structs are required").to_compile_error())
    }
}

fn get_inner<'a>(wrapper: Option<impl AsRef<str>>, ty: &'a syn::Type) -> Option<&'a syn::Type> {
    if let syn::Type::Path(syn::TypePath {
        path: syn::Path { segments, .. },
        ..
    }) = ty
    {
        let v: Vec<_> = segments.iter().map(|e| e.ident.to_string()).collect();
        if wrapper.is_none() || v.last()? == wrapper.unwrap().as_ref() {
            if let syn::PathSegment {
                arguments: syn::PathArguments::AngleBracketed(syn::AngleBracketedGenericArguments { args, .. }),
                ..
            } = segments.last()?
            {
                if let syn::GenericArgument::Type(tp) = args.first()? {
                    return Some(tp);
                }
            }
        }
    }
    None
}

fn add_trait_bounds(
    mut generics: syn::Generics,
    phantom_types: &Vec<&syn::Ident>,
    ass_types: &Vec<&syn::Path>,
    has_bound_attr: bool,
) -> syn::Generics {
    let ass_idents: Vec<_> = ass_types.iter().map(|e| &e.segments[0].ident).collect();
    for param in &mut generics.params {
        if let syn::GenericParam::Type(tp) = param {
            if phantom_types.contains(&&tp.ident) || ass_idents.contains(&&tp.ident) || has_bound_attr {
                continue;
            }
        }
        if let syn::GenericParam::Type(ref mut type_param) = *param {
            type_param.bounds.push(parse_quote!(std::fmt::Debug));
        }
    }
    generics
}

fn get_assosiate_type<'a>(
    ty: &'a syn::Type,
    gen_idents: &mut impl Iterator<Item = &'a syn::Ident>,
) -> Option<&'a syn::Path> {
    if let Some(inner) = get_inner(None::<&str>, ty) {
        return get_assosiate_type(inner, gen_idents);
    };
    if let syn::Type::Path(syn::TypePath { path, .. }) = ty {
        if path.segments.len() > 1 && gen_idents.any(|ident| ident == &path.segments[0].ident) {
            return Some(path);
        }
    }
    None
}
