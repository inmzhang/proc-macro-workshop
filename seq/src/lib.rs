use proc_macro::TokenStream;
use syn::{
    parse::{Parse, ParseStream},
    parse_macro_input, Ident, LitInt, Result, Token, braced
};
use quote::quote;
use proc_macro2::{TokenTree, Group, Literal};

struct Seq {
    index: Ident,
    lower: LitInt,
    upper: LitInt,
    content: proc_macro2::TokenStream,
}

impl Parse for Seq {
    fn parse(input: ParseStream) -> Result<Self> {
        let index: Ident = input.parse()?;
        input.parse::<Token!(in)>()?;
        let lower: LitInt = input.parse()?;
        input.parse::<Token!(..)>()?;
        let upper: LitInt = input.parse()?;
        let content;
        braced!(content in input);
        let content = content.parse::<proc_macro2::TokenStream>()?;
        Ok(Seq {
            index,
            lower,
            upper,
            content,
        })
    }
}

#[proc_macro]
pub fn seq(input: TokenStream) -> TokenStream {
    let Seq {
        index,
        lower,
        upper,
        content,
    } = parse_macro_input!(input as Seq);
    let lower = lower.base10_parse::<usize>().unwrap();
    let upper = upper.base10_parse::<usize>().unwrap();
    let repeated_contents = (lower..upper).map(|n|{
        let content = replace_n_in_token_stream(content.clone(), &index, n);
        quote!{
            #content
        }
    });

    TokenStream::from(quote!(#(#repeated_contents)*))
}

fn replace_n_in_token_stream(
    token_stream: proc_macro2::TokenStream,
    ident: &Ident,
    n: usize,
) -> proc_macro2::TokenStream {
    let mut new_token_stream = proc_macro2::TokenStream::new();
    for tt in token_stream.into_iter() {
        let new_tt = match tt {
            TokenTree::Group(group) => {
                let delimiter = group.delimiter();
                let stream = group.stream();
                let new_stream = replace_n_in_token_stream(stream, ident, n);
                let mut new_group = Group::new(delimiter, new_stream);
                new_group.set_span(group.span());
                TokenTree::Group(new_group)
            }
            TokenTree::Ident(ident2) if ident2 == *ident => {
                let mut lit_int = Literal::usize_unsuffixed(n);
                lit_int.set_span(ident2.span());
                TokenTree::Literal(lit_int)
            }
            tt => tt,
        };
        new_token_stream.extend(std::iter::once(new_tt));
    }
    new_token_stream
}
