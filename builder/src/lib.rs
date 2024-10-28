use proc_macro::TokenStream;
use quote::{format_ident, quote};
use syn::parse::Parser;
use syn::spanned::Spanned;
use syn::{parse_macro_input, parse_quote, DeriveInput};

#[proc_macro_derive(Builder, attributes(builder))]
pub fn derive(input: TokenStream) -> TokenStream {
    let ast = parse_macro_input!(input as DeriveInput);
    if let syn::Data::Struct(syn::DataStruct {
        fields: syn::Fields::Named(syn::FieldsNamed { named, .. }),
        ..
    }) = ast.data
    {
        let (name, vis) = (ast.ident, ast.vis);
        let builder_name = format_ident!("{}Builder", name);

        let fields = named.iter().map(|field| {
            (
                field.ident.as_ref().expect("Field name not found"),
                &field.ty,
                &field.attrs,
            )
        });
        let builder_fields = fields.clone().map(|(field, ty, _)| {
            if get_inner("Option", ty).is_some() || get_inner("Vec", ty).is_some() {
                syn::Field::parse_named
                    .parse2(quote! {
                        #field: #ty
                    })
                    .unwrap()
            }
            else {
                syn::Field::parse_named
                    .parse2(quote! {
                        #field: std::option::Option<#ty>
                    })
                    .unwrap()
            }
        });
        let mut builder_new: Vec<syn::FieldValue> = vec![];

        for (ident, _, attrs) in fields.clone() {
            if attrs
                .iter()
                .find(|attr| attr.path().segments.iter().any(|segment| segment.ident == "builder"))
                .is_some()
            {
                builder_new.push(parse_quote! {
                    #ident: std::vec![]
                })
            }
            else {
                builder_new.push(parse_quote! {
                    #ident: std::option::Option::None
                });
            }
        }
        let build_values: Vec<syn::FieldValue> = fields
            .clone()
            .map(|(ident, ty, _)| {
                let inner_option = get_inner("Option", ty);
                let inner_vec = get_inner("Vec", ty);
                if inner_option.is_some() || inner_vec.is_some() {
                    parse_quote! {
                        #ident: self.#ident.clone()
                    }
                }
                else {
                    parse_quote! {
                        #ident: self.#ident.take().ok_or_else(|| format!("{} is not set", stringify!(#ident)))?
                    }
                }
            })
            .collect();
        let mut setters: Vec<syn::ImplItemFn> = vec![];
        for (ident, ty, attrs) in fields.clone() {
            if let Some(attr) = attrs
                .iter()
                .find(|attr| attr.path().segments.iter().any(|segment| segment.ident == "builder"))
            {
                match attr.parse_nested_meta(|meta| {
                    if meta.path.segments.len() > 1
                        || meta
                            .path
                            .segments
                            .iter()
                            .any(|path_segment| path_segment.ident != "each")
                    {
                        return Err(meta.error("expected `builder(each = \"...\")`"));
                    }
                    let lit_str: syn::LitStr = meta.value()?.parse()?;
                    let inner_ty = get_inner("Vec", ty).ok_or(meta.error("Field does not contain a Vec type"))?;
                    let lit_ident = format_ident!("{}", lit_str.value());
                    setters.push(parse_quote! {
                        #vis fn #lit_ident(&mut self, val: #inner_ty) -> &mut Self {
                            self.#ident.push(val);
                            self
                        }
                    });
                    if lit_ident != *ident {
                        setters.push(parse_quote! {
                            #vis fn #ident(&mut self, #ident:#ty) -> &mut Self {
                                self.#ident = #ident.clone();
                                self
                            }
                        });
                    }
                    Ok(())
                }) {
                    Ok(_) => (),
                    Err(e) => return TokenStream::from(e.to_compile_error()),
                }
            }
            else {
                let mut ty_option = ty;
                if let Some(inner) = get_inner("Option", ty) {
                    ty_option = inner;
                }
                setters.push(parse_quote! {
                    #vis fn #ident(&mut self, #ident:#ty_option) -> &mut Self {
                        self.#ident = std::option::Option::Some(#ident);
                        self
                    }
                });
            }
        }

        quote! {
            impl #name {
                #vis fn builder() -> #builder_name {
                    #builder_name {
                        #(#builder_new),*
                    }
                }
            }

            #vis struct #builder_name {
                #(#builder_fields),*
            }

            impl #builder_name {
                #(#setters)*
                #vis fn build(&mut self) -> std::result::Result<#name, std::boxed::Box<dyn std::error::Error>> {
                    std::result::Result::Ok(#name {
                        #(#build_values),*
                    })
                }
            }
        }
        .into()
    }
    else {
        TokenStream::from(syn::Error::new(ast.span(), "Named structs are required").to_compile_error())
    }
}

fn get_inner<'a>(wrapper: impl AsRef<str>, ty: &'a syn::Type) -> Option<&'a syn::Type> {
    if let syn::Type::Path(syn::TypePath {
        path: syn::Path { segments, .. },
        ..
    }) = ty
    {
        let segment_idents: Vec<_> = segments.iter().map(|segment| segment.ident.to_string()).collect();
        if segment_idents.last()? == wrapper.as_ref() {
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
