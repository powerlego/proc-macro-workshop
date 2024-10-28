use proc_macro::TokenStream;
use quote::quote;
use syn::spanned::Spanned;
use syn::visit_mut::{self, VisitMut};

struct MatchVisitor {
    error: Option<syn::Error>,
}

macro_rules! get_path_str {
    ($x:expr) => {
        $x.segments
            .pairs()
            .flat_map(|pair| {
                if pair.punct().is_some() {
                    format!("{}::", pair.value().ident.to_string())
                        .chars()
                        .collect::<Vec<_>>()
                }
                else {
                    pair.value().ident.to_string().chars().collect::<Vec<_>>()
                }
            })
            .collect::<String>()
    };
}

impl MatchVisitor {
    fn compare_pat(&self, a: &syn::Pat, b: &syn::Pat) -> Result<(), syn::Error> {
        match &a {
            syn::Pat::Ident(a_ident) => match &b {
                syn::Pat::Ident(b_ident) => {
                    if a_ident.ident > b_ident.ident {
                        Err(syn::Error::new(
                            b_ident.span(),
                            format!(
                                "{} should sort before {}",
                                b_ident.ident.to_string(),
                                a_ident.ident.to_string()
                            ),
                        ))
                    }
                    else {
                        Ok(())
                    }
                }
                syn::Pat::Or(b_or) => {
                    for b_case in &b_or.cases {
                        if let Err(e) = self.compare_pat(&syn::Pat::from(a_ident.clone()), b_case) {
                            return Err(e);
                        }
                    }
                    Ok(())
                }
                syn::Pat::Paren(b_paren) => self.compare_pat(&syn::Pat::from(a_ident.clone()), &b_paren.pat),
                syn::Pat::Path(b_path) => {
                    if a_ident.ident.to_string() > get_path_str!(b_path.path) {
                        Err(syn::Error::new_spanned(
                            &b_path.path,
                            format!(
                                "{} should sort before {}",
                                get_path_str!(b_path.path),
                                a_ident.ident.to_string()
                            ),
                        ))
                    }
                    else {
                        Ok(())
                    }
                }
                syn::Pat::Tuple(b_tuple) => {
                    for b_elem in b_tuple.elems.iter() {
                        if let Err(e) = self.compare_pat(&syn::Pat::from(a_ident.clone()), b_elem) {
                            return Err(e);
                        }
                    }
                    Ok(())
                }
                syn::Pat::Struct(b_struct) => {
                    if a_ident.ident.to_string() > get_path_str!(b_struct.path) {
                        Err(syn::Error::new_spanned(
                            &b_struct.path,
                            format!(
                                "{} should sort before {}",
                                get_path_str!(b_struct.path),
                                a_ident.ident.to_string()
                            ),
                        ))
                    }
                    else {
                        Ok(())
                    }
                }
                syn::Pat::TupleStruct(b_tuple_struct) => {
                    if a_ident.ident.to_string() > get_path_str!(b_tuple_struct.path) {
                        Err(syn::Error::new_spanned(
                            &b_tuple_struct.path,
                            format!(
                                "{} should sort before {}",
                                get_path_str!(b_tuple_struct.path),
                                a_ident.ident.to_string()
                            ),
                        ))
                    }
                    else {
                        Ok(())
                    }
                }
                syn::Pat::Wild(_) => Ok(()),
                _ => Err(syn::Error::new(b.span(), "unsupported by #[sorted]")),
            },
            syn::Pat::Or(a_or) => match &b {
                syn::Pat::Ident(b_ident) => {
                    for a_case in &a_or.cases {
                        if let Err(e) = self.compare_pat(a_case, &syn::Pat::from(b_ident.clone())) {
                            return Err(e);
                        }
                    }
                    Ok(())
                }
                syn::Pat::Or(b_or) => {
                    for a_case in &a_or.cases {
                        for b_case in &b_or.cases {
                            if let Err(e) = self.compare_pat(a_case, b_case) {
                                return Err(e);
                            }
                        }
                    }
                    Ok(())
                }
                syn::Pat::Paren(b_paren) => {
                    for a_case in &a_or.cases {
                        if let Err(e) = self.compare_pat(a_case, &b_paren.pat) {
                            return Err(e);
                        }
                    }
                    Ok(())
                }
                syn::Pat::Path(b_path) => {
                    for a_case in &a_or.cases {
                        if let Err(e) = self.compare_pat(a_case, &syn::Pat::from(b_path.clone())) {
                            return Err(e);
                        }
                    }
                    Ok(())
                }
                syn::Pat::Tuple(b_tuple) => {
                    for a_case in &a_or.cases {
                        for b_elem in b_tuple.elems.iter() {
                            if let Err(e) = self.compare_pat(a_case, b_elem) {
                                return Err(e);
                            }
                        }
                    }
                    Ok(())
                }
                syn::Pat::Struct(b_struct) => {
                    for a_case in &a_or.cases {
                        if let Err(e) = self.compare_pat(a_case, &syn::Pat::from(b_struct.clone())) {
                            return Err(e);
                        }
                    }
                    Ok(())
                }
                syn::Pat::TupleStruct(b_tuple_struct) => {
                    for a_case in &a_or.cases {
                        if let Err(e) = self.compare_pat(a_case, &syn::Pat::from(b_tuple_struct.clone())) {
                            return Err(e);
                        }
                    }
                    Ok(())
                }
                syn::Pat::Wild(_) => Ok(()),
                _ => Err(syn::Error::new(b.span(), "unsupported by #[sorted]")),
            },
            syn::Pat::Paren(a_paren) => match &b {
                syn::Pat::Ident(b_ident) => self.compare_pat(&a_paren.pat, &syn::Pat::from(b_ident.clone())),
                syn::Pat::Or(b_or) => {
                    for b_case in &b_or.cases {
                        if let Err(e) = self.compare_pat(&a_paren.pat, b_case) {
                            return Err(e);
                        }
                    }
                    Ok(())
                }
                syn::Pat::Paren(b_paren) => self.compare_pat(&a_paren.pat, &b_paren.pat),
                syn::Pat::Path(b_path) => self.compare_pat(&a_paren.pat, &syn::Pat::from(b_path.clone())),
                syn::Pat::Tuple(b_tuple) => {
                    for b_elem in b_tuple.elems.iter() {
                        if let Err(e) = self.compare_pat(&a_paren.pat, b_elem) {
                            return Err(e);
                        }
                    }
                    Ok(())
                }
                syn::Pat::Struct(b_struct) => self.compare_pat(&a_paren.pat, &syn::Pat::from(b_struct.clone())),
                syn::Pat::TupleStruct(b_tuple_struct) => {
                    self.compare_pat(&a_paren.pat, &syn::Pat::from(b_tuple_struct.clone()))
                }
                syn::Pat::Wild(_) => Ok(()),
                _ => Err(syn::Error::new(b.span(), "unsupported by #[sorted]")),
            },
            syn::Pat::Path(a_path) => match &b {
                syn::Pat::Ident(b_ident) => {
                    if get_path_str!(a_path.path) > b_ident.ident.to_string() {
                        Err(syn::Error::new(
                            b_ident.span(),
                            format!(
                                "{} should sort before {}",
                                b_ident.ident.to_string(),
                                get_path_str!(a_path.path).to_string()
                            ),
                        ))
                    }
                    else {
                        Ok(())
                    }
                }
                syn::Pat::Or(b_or) => {
                    for b_case in &b_or.cases {
                        if let Err(e) = self.compare_pat(&syn::Pat::from(a_path.clone()), b_case) {
                            return Err(e);
                        }
                    }
                    Ok(())
                }
                syn::Pat::Paren(b_paren) => self.compare_pat(&syn::Pat::from(a_path.clone()), &b_paren.pat),
                syn::Pat::Path(b_path) => {
                    if get_path_str!(a_path.path) > get_path_str!(b_path.path) {
                        Err(syn::Error::new_spanned(
                            &b_path.path,
                            format!(
                                "{} should sort before {}",
                                get_path_str!(b_path.path),
                                get_path_str!(a_path.path)
                            ),
                        ))
                    }
                    else {
                        Ok(())
                    }
                }
                syn::Pat::Tuple(b_tuple) => {
                    for b_elem in b_tuple.elems.iter() {
                        if let Err(e) = self.compare_pat(&syn::Pat::from(a_path.clone()), b_elem) {
                            return Err(e);
                        }
                    }
                    Ok(())
                }
                syn::Pat::Struct(b_struct) => {
                    if get_path_str!(a_path.path) > get_path_str!(b_struct.path) {
                        Err(syn::Error::new_spanned(
                            &b_struct.path,
                            format!(
                                "{} should sort before {}",
                                get_path_str!(b_struct.path),
                                get_path_str!(a_path.path)
                            ),
                        ))
                    }
                    else {
                        Ok(())
                    }
                }
                syn::Pat::TupleStruct(b_tuple_struct) => {
                    if get_path_str!(a_path.path) > get_path_str!(b_tuple_struct.path) {
                        Err(syn::Error::new_spanned(
                            &b_tuple_struct.path,
                            format!(
                                "{} should sort before {}",
                                get_path_str!(b_tuple_struct.path),
                                get_path_str!(a_path.path)
                            ),
                        ))
                    }
                    else {
                        Ok(())
                    }
                }
                syn::Pat::Wild(_) => Ok(()),
                _ => Err(syn::Error::new(b.span(), "unsupported by #[sorted]")),
            },
            syn::Pat::Tuple(a_tuple) => match &b {
                syn::Pat::Ident(b_ident) => {
                    for a_elem in a_tuple.elems.iter() {
                        if let Err(e) = self.compare_pat(a_elem, &syn::Pat::from(b_ident.clone())) {
                            return Err(e);
                        }
                    }
                    Ok(())
                }
                syn::Pat::Or(b_or) => {
                    for a_elem in a_tuple.elems.iter() {
                        for b_case in &b_or.cases {
                            if let Err(e) = self.compare_pat(a_elem, b_case) {
                                return Err(e);
                            }
                        }
                    }
                    Ok(())
                }
                syn::Pat::Paren(b_paren) => {
                    for a_elem in a_tuple.elems.iter() {
                        if let Err(e) = self.compare_pat(a_elem, &b_paren.pat) {
                            return Err(e);
                        }
                    }
                    Ok(())
                }
                syn::Pat::Path(b_path) => {
                    for a_elem in a_tuple.elems.iter() {
                        if let Err(e) = self.compare_pat(a_elem, &syn::Pat::from(b_path.clone())) {
                            return Err(e);
                        }
                    }
                    Ok(())
                }
                syn::Pat::Tuple(b_tuple) => {
                    for a_elem in a_tuple.elems.iter() {
                        for b_elem in b_tuple.elems.iter() {
                            if let Err(e) = self.compare_pat(a_elem, b_elem) {
                                return Err(e);
                            }
                        }
                    }
                    Ok(())
                }
                syn::Pat::Struct(b_struct) => {
                    for a_elem in a_tuple.elems.iter() {
                        if let Err(e) = self.compare_pat(a_elem, &syn::Pat::from(b_struct.clone())) {
                            return Err(e);
                        }
                    }
                    Ok(())
                }
                syn::Pat::TupleStruct(b_tuple_struct) => {
                    for a_elem in a_tuple.elems.iter() {
                        if let Err(e) = self.compare_pat(a_elem, &syn::Pat::from(b_tuple_struct.clone())) {
                            return Err(e);
                        }
                    }
                    Ok(())
                }
                syn::Pat::Wild(_) => Ok(()),
                _ => Err(syn::Error::new(b.span(), "unsupported by #[sorted]")),
            },
            syn::Pat::Struct(a_struct) => match &b {
                syn::Pat::Ident(b_ident) => {
                    if get_path_str!(a_struct.path) > b_ident.ident.to_string() {
                        Err(syn::Error::new(
                            b_ident.span(),
                            format!(
                                "{} should sort before {}",
                                b_ident.ident.to_string(),
                                get_path_str!(a_struct.path).to_string()
                            ),
                        ))
                    }
                    else {
                        Ok(())
                    }
                }
                syn::Pat::Or(b_or) => {
                    for b_case in &b_or.cases {
                        if let Err(e) = self.compare_pat(&syn::Pat::from(a_struct.clone()), b_case) {
                            return Err(e);
                        }
                    }
                    Ok(())
                }
                syn::Pat::Paren(b_paren) => self.compare_pat(&syn::Pat::from(a_struct.clone()), &b_paren.pat),
                syn::Pat::Path(b_path) => {
                    if get_path_str!(a_struct.path) > get_path_str!(b_path.path) {
                        Err(syn::Error::new_spanned(
                            &b_path.path,
                            format!(
                                "{} should sort before {}",
                                get_path_str!(b_path.path),
                                get_path_str!(a_struct.path)
                            ),
                        ))
                    }
                    else {
                        Ok(())
                    }
                }
                syn::Pat::Tuple(b_tuple) => {
                    for b_elem in b_tuple.elems.iter() {
                        if let Err(e) = self.compare_pat(&syn::Pat::from(a_struct.clone()), b_elem) {
                            return Err(e);
                        }
                    }
                    Ok(())
                }
                syn::Pat::Struct(b_struct) => {
                    if get_path_str!(a_struct.path) > get_path_str!(b_struct.path) {
                        Err(syn::Error::new_spanned(
                            &b_struct.path,
                            format!(
                                "{} should sort before {}",
                                get_path_str!(b_struct.path),
                                get_path_str!(a_struct.path)
                            ),
                        ))
                    }
                    else {
                        Ok(())
                    }
                }
                syn::Pat::TupleStruct(b_tuple_struct) => {
                    if get_path_str!(a_struct.path) > get_path_str!(b_tuple_struct.path) {
                        Err(syn::Error::new_spanned(
                            &b_tuple_struct.path,
                            format!(
                                "{} should sort before {}",
                                get_path_str!(b_tuple_struct.path),
                                get_path_str!(a_struct.path)
                            ),
                        ))
                    }
                    else {
                        Ok(())
                    }
                }
                syn::Pat::Wild(_) => Ok(()),
                _ => Err(syn::Error::new(b.span(), "unsupported by #[sorted]")),
            },
            syn::Pat::TupleStruct(a_tuple_struct) => match &b {
                syn::Pat::Ident(b_ident) => {
                    if get_path_str!(a_tuple_struct.path) > b_ident.ident.to_string() {
                        Err(syn::Error::new(
                            b_ident.span(),
                            format!(
                                "{} should sort before {}",
                                b_ident.ident.to_string(),
                                get_path_str!(a_tuple_struct.path).to_string()
                            ),
                        ))
                    }
                    else {
                        Ok(())
                    }
                }
                syn::Pat::Or(b_or) => {
                    for b_case in &b_or.cases {
                        if let Err(e) = self.compare_pat(&syn::Pat::from(a_tuple_struct.clone()), b_case) {
                            return Err(e);
                        }
                    }
                    Ok(())
                }
                syn::Pat::Paren(b_paren) => self.compare_pat(&syn::Pat::from(a_tuple_struct.clone()), &b_paren.pat),
                syn::Pat::Path(b_path) => {
                    if get_path_str!(a_tuple_struct.path) > get_path_str!(b_path.path) {
                        Err(syn::Error::new_spanned(
                            &b_path.path,
                            format!(
                                "{} should sort before {}",
                                get_path_str!(b_path.path),
                                get_path_str!(a_tuple_struct.path)
                            ),
                        ))
                    }
                    else {
                        Ok(())
                    }
                }
                syn::Pat::Tuple(b_tuple) => {
                    for b_elem in b_tuple.elems.iter() {
                        if let Err(e) = self.compare_pat(&syn::Pat::from(a_tuple_struct.clone()), b_elem) {
                            return Err(e);
                        }
                    }
                    Ok(())
                }
                syn::Pat::Struct(b_struct) => {
                    if get_path_str!(a_tuple_struct.path) > get_path_str!(b_struct.path) {
                        Err(syn::Error::new_spanned(
                            &b_struct.path,
                            format!(
                                "{} should sort before {}",
                                get_path_str!(b_struct.path),
                                get_path_str!(a_tuple_struct.path)
                            ),
                        ))
                    }
                    else {
                        Ok(())
                    }
                }
                syn::Pat::TupleStruct(b_tuple_struct) => {
                    if get_path_str!(a_tuple_struct.path) > get_path_str!(b_tuple_struct.path) {
                        Err(syn::Error::new_spanned(
                            &b_tuple_struct.path,
                            format!(
                                "{} should sort before {}",
                                get_path_str!(b_tuple_struct.path),
                                get_path_str!(a_tuple_struct.path)
                            ),
                        ))
                    }
                    else {
                        Ok(())
                    }
                }
                syn::Pat::Wild(_) => Ok(()),
                _ => Err(syn::Error::new(b.span(), "unsupported by #[sorted]")),
            },
            syn::Pat::Wild(_) => Ok(()),
            _ => Err(syn::Error::new(a.span(), "unsupported by #[sorted]")),
        }
    }
}

impl VisitMut for MatchVisitor {
    fn visit_expr_match_mut(&mut self, node: &mut syn::ExprMatch) {
        if let Some(attr_idx) = node
            .attrs
            .iter()
            .position(|attr| attr.path().segments[0].ident == "sorted")
        {
            node.attrs.swap_remove(attr_idx);

            for i in 0..node.arms.len() {
                for j in i..node.arms.len() {
                    if let Err(e) = self.compare_pat(&node.arms[i].pat, &node.arms[j].pat) {
                        self.error = Some(e);
                        return;
                    }
                }
            }
            if let Some(idx) = node.arms.iter().position(|arm| {
                if let syn::Pat::Wild(_) = arm.pat {
                    true
                }
                else {
                    false
                }
            }) {
                if idx != node.arms.len() - 1 {
                    self.error = Some(syn::Error::new(
                        node.arms[idx].span(),
                        "Wildcards should be placed last",
                    ));
                    return;
                }
            }
        }
        visit_mut::visit_expr_match_mut(self, node);
    }
}

#[proc_macro_attribute]
pub fn sorted(_args: TokenStream, input: TokenStream) -> TokenStream {
    let mut ast = syn::parse_macro_input!(input as syn::Item);

    match sorted_result(&mut ast) {
        Ok(tokens) => TokenStream::from(tokens),
        Err(e) => {
            let er = e.to_compile_error();
            quote! {
                #ast
                #er
            }
            .into()
        }
    }
}

fn sorted_result(ast: &mut syn::Item) -> Result<proc_macro2::TokenStream, syn::Error> {
    if let syn::Item::Enum(en) = ast {
        for i in 0..en.variants.len() {
            for j in i..en.variants.len() {
                if en.variants[i].ident > en.variants[j].ident {
                    return Err(syn::Error::new(
                        en.variants[j].span(),
                        format!(
                            "{} should sort before {}",
                            en.variants[j].ident.to_string(),
                            en.variants[i].ident.to_string()
                        ),
                    ));
                }
            }
        }

        Ok(quote! {
            #en
        })
    }
    else if let syn::Item::Fn(ref mut item_fn) = ast {
        let mut visitor = MatchVisitor { error: None };
        visitor.visit_item_fn_mut(item_fn);

        if let Some(e) = visitor.error {
            Err(e)
        }
        else {
            Ok(quote! {
                #item_fn
            })
        }
    }
    else {
        Err(syn::Error::new(
            proc_macro2::Span::call_site(),
            "expected enum or match expression",
        ))
    }
}

#[proc_macro_attribute]
pub fn check(_args: TokenStream, input: TokenStream) -> TokenStream {
    let mut ast = syn::parse_macro_input!(input as syn::Item);

    match sorted_result(&mut ast) {
        Ok(tokens) => TokenStream::from(tokens),
        Err(e) => {
            let er = e.to_compile_error();
            quote! {
                #ast
                #er
            }
            .into()
        }
    }
}
