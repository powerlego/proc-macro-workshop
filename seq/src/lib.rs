use proc_macro::TokenStream;
use quote::quote;
use syn;
use syn::parse::Parse;

struct Seq {
    ident: syn::Ident,
    start: isize,
    end:   isize,
    body:  proc_macro2::TokenStream,
}

impl Parse for Seq {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        let ident: syn::Ident = input.parse()?;
        input.parse::<syn::Token![in]>()?;
        let start: isize = input.parse::<syn::LitInt>()?.base10_parse()?;
        input.parse::<syn::Token![..]>()?;
        let end: isize = if let Ok(_) = input.parse::<syn::Token![=]>() {
            let num: isize = input.parse::<syn::LitInt>()?.base10_parse()?;
            num + 1
        }
        else {
            input.parse::<syn::LitInt>()?.base10_parse()?
        };

        let body;
        syn::braced!(body in input);
        Ok(Seq {
            ident,
            start,
            end,
            body: body.parse()?,
        })
    }
}

impl Seq {
    fn expand(&self, ts: &proc_macro2::TokenStream, n: isize) -> proc_macro2::TokenStream {
        let buf = ts.clone().into_iter().collect::<Vec<_>>();
        let mut ret = proc_macro2::TokenStream::new();
        let mut idx = 0;
        while idx < buf.len() {
            let node = &buf[idx];
            match node {
                proc_macro2::TokenTree::Group(grp) => {
                    let new_stream = self.expand(&grp.stream(), n);
                    let wrap_grp = proc_macro2::Group::new(grp.delimiter(), new_stream);
                    ret.extend(quote! {
                        #wrap_grp
                    });
                }
                proc_macro2::TokenTree::Ident(prefix) => {
                    if idx + 4 < buf.len() {
                        if let proc_macro2::TokenTree::Punct(p1) = &buf[idx + 1] {
                            if p1.as_char() == '~' {
                                if let proc_macro2::TokenTree::Ident(id) = &buf[idx + 2] {
                                    if id == &self.ident
                                        && prefix.span().end() == p1.span().start()
                                        && p1.span().end() == id.span().start()
                                    {
                                        if let proc_macro2::TokenTree::Punct(p2) = &buf[idx + 3] {
                                            if p2.as_char() == '~' {
                                                match &buf[idx + 4] {
                                                    proc_macro2::TokenTree::Group(grp) => {
                                                        let new_stream = self.expand(&grp.stream(), n);
                                                        let wrap_grp =
                                                            proc_macro2::Group::new(grp.delimiter(), new_stream);
                                                        let new_ident_lit = format!("{}{}", prefix.to_string(), n,);
                                                        let new_ident =
                                                            proc_macro2::Ident::new(&new_ident_lit, prefix.span());
                                                        ret.extend(quote! {
                                                            #new_ident #wrap_grp
                                                        });
                                                    }
                                                    proc_macro2::TokenTree::Ident(suffix) => {
                                                        let new_ident_lit = format!(
                                                            "{}{}{}",
                                                            prefix.to_string(),
                                                            n,
                                                            suffix.to_string()
                                                        );
                                                        let new_ident =
                                                            proc_macro2::Ident::new(&new_ident_lit, prefix.span());
                                                        ret.extend(quote! {
                                                            #new_ident
                                                        });
                                                    }
                                                    proc_macro2::TokenTree::Literal(lit) => {
                                                        let new_ident_lit =
                                                            format!("{}{}{}", prefix.to_string(), n, lit.to_string());
                                                        let new_ident =
                                                            proc_macro2::Ident::new(&new_ident_lit, prefix.span());
                                                        ret.extend(quote! {
                                                            #new_ident
                                                        });
                                                    }
                                                    proc_macro2::TokenTree::Punct(punct) => {
                                                        let new_ident_lit =
                                                            format!("{}{}{}", prefix.to_string(), n, punct.as_char());
                                                        let new_ident =
                                                            proc_macro2::Ident::new(&new_ident_lit, prefix.span());
                                                        ret.extend(quote! {
                                                            #new_ident
                                                        });
                                                    }
                                                }
                                                idx += 5;
                                                continue;
                                            }
                                        }
                                        else {
                                            let new_ident_lit = format!("{}{}", prefix.to_string(), n);
                                            let new_ident = proc_macro2::Ident::new(&new_ident_lit, prefix.span());
                                            ret.extend(quote! {
                                                #new_ident
                                            });
                                            idx += 3;
                                            continue;
                                        }
                                    }
                                }
                            }
                        }
                    }
                    else if idx + 2 < buf.len() {
                        if let proc_macro2::TokenTree::Punct(p1) = &buf[idx + 1] {
                            if p1.as_char() == '~' {
                                if let proc_macro2::TokenTree::Ident(id) = &buf[idx + 2] {
                                    if id == &self.ident
                                        && prefix.span().end() == p1.span().start()
                                        && p1.span().end() == id.span().start()
                                    {
                                        let new_ident_lit = format!("{}{}", prefix.to_string(), n);
                                        let new_ident = proc_macro2::Ident::new(&new_ident_lit, prefix.span());
                                        ret.extend(quote! {
                                            #new_ident
                                        });
                                        idx += 3;
                                        continue;
                                    }
                                }
                            }
                        }
                    }
                    if prefix == &self.ident {
                        let new_ident = proc_macro2::Literal::i64_unsuffixed(n as i64);
                        ret.extend(quote::quote! {#new_ident});
                        idx += 1;
                        continue;
                    }
                    ret.extend(quote! {#node});
                }
                _ => {
                    ret.extend(quote! {#node});
                }
            }
            idx += 1;
        }
        ret
    }

    fn expand_block(&self, cur: syn::buffer::Cursor) -> (proc_macro2::TokenStream, bool) {
        let mut expanded = false;
        let mut ret_stream = proc_macro2::TokenStream::new();

        let mut cursor = cur;
        while !cursor.eof() {
            if let Some((prefix_punct, prefix_end_cursor)) = cursor.punct() {
                if prefix_punct.as_char() == '#' {
                    if let Some((group_cur_start, _, group_cur_end)) =
                        prefix_end_cursor.group(proc_macro2::Delimiter::Parenthesis)
                    {
                        if let Some((suffix_punct, suffix_end_cursor)) = group_cur_end.punct() {
                            if suffix_punct.as_char() == '*' {
                                for i in self.start..self.end {
                                    ret_stream.extend(self.expand(&group_cur_start.token_stream(), i));
                                }
                                cursor = suffix_end_cursor;
                                expanded = true;
                                continue;
                            }
                        }
                    }
                }
            }

            if let Some((group_cur, _, next_cur)) = cursor.group(proc_macro2::Delimiter::Brace) {
                let (t, e) = self.expand_block(group_cur);
                expanded = e;
                ret_stream.extend(quote! {{#t}});
                cursor = next_cur;
            }
            else if let Some((group_cur, _, next_cur)) = cursor.group(proc_macro2::Delimiter::Bracket) {
                let (t, e) = self.expand_block(group_cur);
                expanded = e;
                ret_stream.extend(quote! {[#t]});
                cursor = next_cur;
            }
            else if let Some((group_cur, _, next_cur)) = cursor.group(proc_macro2::Delimiter::Parenthesis) {
                let (t, e) = self.expand_block(group_cur);
                expanded = e;
                ret_stream.extend(quote! {(#t)});
                cursor = next_cur;
            }
            else if let Some((punct, next_cur)) = cursor.punct() {
                ret_stream.extend(quote!(#punct));
                cursor = next_cur;
            }
            else if let Some((ident, next_cur)) = cursor.ident() {
                ret_stream.extend(quote!(#ident));
                cursor = next_cur;
            }
            else if let Some((literal, next_cur)) = cursor.literal() {
                ret_stream.extend(quote!(#literal));
                cursor = next_cur;
            }
            else if let Some((lifetime, next_cur)) = cursor.lifetime() {
                ret_stream.extend(quote!(#lifetime));
                cursor = next_cur;
            }
        }

        (ret_stream, expanded)
    }
}

#[proc_macro]
pub fn seq(input: TokenStream) -> TokenStream {
    let sq = syn::parse_macro_input!(input as Seq);

    let mut ret_stream = proc_macro2::TokenStream::new();

    let buf = syn::buffer::TokenBuffer::new2(sq.body.clone());

    let (block_stream, expanded) = sq.expand_block(buf.begin());
    if expanded {
        return block_stream.into();
    }

    for i in sq.start..sq.end {
        ret_stream.extend(sq.expand(&sq.body, i));
    }

    ret_stream.into()
}
