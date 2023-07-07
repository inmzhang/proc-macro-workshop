use proc_macro::TokenStream;
use syn::{
    braced,
    parse::{Parse, ParseStream},
    parse_macro_input, Ident, LitInt, Result, Token,
};

struct Seq {
    ident: Ident,
    lower: LitInt,
    upper: LitInt,
    inclusive: bool,
    content: proc_macro2::TokenStream,
}

impl Parse for Seq {
    fn parse(input: ParseStream) -> Result<Self> {
        let ident: Ident = input.parse()?;
        input.parse::<Token!(in)>()?;
        let lower: LitInt = input.parse()?;
        let inclusive = input.peek(Token![..=]);
        if inclusive {
            input.parse::<Token![..=]>()?;
        } else {
            input.parse::<Token!(..)>()?;
        }
        let upper: LitInt = input.parse()?;

        let content;
        braced!(content in input);
        let content = content.parse::<proc_macro2::TokenStream>()?;
        Ok(Seq {
            ident,
            lower,
            upper,
            inclusive,
            content,
        })
    }
}

#[derive(Copy, Clone, Debug)]
enum Mode {
    ReplaceIdent(usize),
    ReplaceSequence,
}

impl Seq {
    fn range(&self) -> impl Iterator<Item = usize> {
        let lower = self.lower.base10_parse::<usize>().unwrap();
        let upper = self.upper.base10_parse::<usize>().unwrap();
        if self.inclusive {
            lower..upper + 1
        } else {
            lower..upper
        }
    }

    fn expand2(
        &self,
        tt: proc_macro2::TokenTree,
        rest: &mut proc_macro2::token_stream::IntoIter,
        mutated: &mut bool,
        mode: Mode,
    ) -> proc_macro2::TokenStream {
        let tt = match tt {
            proc_macro2::TokenTree::Group(g) => {
                let (expanded, g_mutated) = self.expand_pass(g.stream(), mode);
                let mut expanded = proc_macro2::Group::new(g.delimiter(), expanded);
                *mutated |= g_mutated;
                expanded.set_span(g.span());
                proc_macro2::TokenTree::Group(expanded)
            }
            proc_macro2::TokenTree::Ident(ref ident) if ident == &self.ident => {
                if let Mode::ReplaceIdent(i) = mode {
                    let mut lit = proc_macro2::Literal::usize_unsuffixed(i);
                    lit.set_span(ident.span());
                    *mutated = true;
                    proc_macro2::TokenTree::Literal(lit)
                } else {
                    // not allowed to replace idents in first pass
                    proc_macro2::TokenTree::Ident(ident.clone())
                }
            }
            proc_macro2::TokenTree::Ident(mut ident) => {
                // search for ~ followed by self.ident at the end of an identifier
                let mut peek = rest.clone();
                match (mode, peek.next(), peek.next()) {
                    (
                        Mode::ReplaceIdent(i),
                        Some(proc_macro2::TokenTree::Punct(ref punct)),
                        Some(proc_macro2::TokenTree::Ident(ref ident2)),
                    ) if punct.as_char() == '~' && ident2 == &self.ident => {
                        // have seen ident ~ N
                        ident = proc_macro2::Ident::new(&format!("{}{}", ident, i), ident.span());
                        *rest = peek.clone();
                        *mutated = true;
                    }
                    _ => {}
                }

                proc_macro2::TokenTree::Ident(ident)
            }
            proc_macro2::TokenTree::Punct(ref p) if p.as_char() == '#' => {
                if let Mode::ReplaceSequence = mode {
                    // is this #(...)* ?
                    let mut peek = rest.clone();
                    match (peek.next(), peek.next()) {
                        (
                            Some(proc_macro2::TokenTree::Group(ref rep)),
                            Some(proc_macro2::TokenTree::Punct(ref star)),
                        ) if rep.delimiter() == proc_macro2::Delimiter::Parenthesis
                            && star.as_char() == '*' =>
                        {
                            // yes! expand ... for each sequence in the range
                            *mutated = true;
                            *rest = peek;
                            return self
                                .range()
                                .map(|i| self.expand_pass(rep.stream(), Mode::ReplaceIdent(i)))
                                .map(|(ts, _)| ts)
                                .collect();
                        }
                        _ => {}
                    }
                }
                proc_macro2::TokenTree::Punct(p.clone())
            }
            tt => tt,
        };
        std::iter::once(tt).collect()
    }

    fn expand_pass(
        &self,
        stream: proc_macro2::TokenStream,
        mode: Mode,
    ) -> (proc_macro2::TokenStream, bool) {
        let mut out = proc_macro2::TokenStream::new();
        let mut mutated = false;
        let mut tts = stream.into_iter();
        while let Some(tt) = tts.next() {
            out.extend(self.expand2(tt, &mut tts, &mut mutated, mode));
        }
        (out, mutated)
    }

    fn expand(&self, stream: proc_macro2::TokenStream) -> proc_macro2::TokenStream {
        let (out, mutated) = self.expand_pass(stream.clone(), Mode::ReplaceSequence);
        if mutated {
            return out;
        }

        self.range()
            .map(|i| self.expand_pass(stream.clone(), Mode::ReplaceIdent(i)))
            .map(|(ts, _)| ts)
            .collect()
    }
}

impl Into<proc_macro2::TokenStream> for Seq {
    fn into(self) -> proc_macro2::TokenStream {
        self.expand(self.content.clone())
    }
}

#[proc_macro]
pub fn seq(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as Seq);
    let output: proc_macro2::TokenStream = input.into();
    output.into()
}
