/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/. */

use computed_values::font_family::FontFamily;
use cssparser::{Token, Parser, DeclarationListParser, AtRuleParser, DeclarationParser};
use parser::{ParserContext, log_css_error};
use properties::longhands::font_family::parse_one_family;
use std::ascii::AsciiExt;
use string_cache::Atom;
use url::{Url, UrlParser};

#[derive(Clone, Debug, PartialEq, Eq, Deserialize, Serialize)]
pub enum Source {
    Url(UrlSource),
    Local(Atom),
}

#[derive(Clone, Debug, PartialEq, Eq, Deserialize, Serialize)]
pub struct UrlSource {
    pub url: Url,
    pub format_hints: Vec<String>,
}

#[derive(Debug, PartialEq, Eq)]
pub struct FontFaceRule {
    pub family: Atom,
    pub sources: Vec<Source>,
}

pub fn parse_font_face_block(context: &ParserContext, input: &mut Parser)
                             -> Result<FontFaceRule, ()> {
    let mut family = None;
    let mut src = None;
    let mut iter = DeclarationListParser::new(input, FontFaceRuleParser { context: context });
    while let Some(declaration) = iter.next() {
        match declaration {
            Err(range) => {
                let pos = range.start;
                let message = format!("Unsupported @font-face descriptor declaration: '{}'",
                                      iter.input.slice(range));
                log_css_error(iter.input, pos, &*message);
            }
            Ok(FontFaceDescriptorDeclaration::Family(value)) => {
                family = Some(value);
            }
            Ok(FontFaceDescriptorDeclaration::Src(value)) => {
                src = Some(value);
            }
        }
    }
    match (family, src) {
        (Some(family), Some(src)) => {
            Ok(FontFaceRule {
                family: family,
                sources: src,
            })
        }
        _ => Err(())
    }
}

enum FontFaceDescriptorDeclaration {
    Family(Atom),
    Src(Vec<Source>),
}


struct FontFaceRuleParser<'a, 'b: 'a> {
    context: &'a ParserContext<'b>,
}


/// Default methods reject all at rules.
impl<'a, 'b> AtRuleParser for FontFaceRuleParser<'a, 'b> {
    type Prelude = ();
    type AtRule = FontFaceDescriptorDeclaration;
}


impl<'a, 'b> DeclarationParser for FontFaceRuleParser<'a, 'b> {
    type Declaration = FontFaceDescriptorDeclaration;

    fn parse_value(&self, name: &str, input: &mut Parser) -> Result<FontFaceDescriptorDeclaration, ()> {
        match_ignore_ascii_case! { name,
            "font-family" => {
                Ok(FontFaceDescriptorDeclaration::Family(try!(
                            parse_one_non_generic_family_name(input))))
            },
            "src" => {
                Ok(FontFaceDescriptorDeclaration::Src(try!(input.parse_comma_separated(|input| {
                    parse_one_src(self.context, input)
                }))))
            }
            _ => Err(())
        }
    }
}

fn parse_one_non_generic_family_name(input: &mut Parser) -> Result<Atom, ()> {
    match parse_one_family(input) {
        Ok(FontFamily::FamilyName(name)) => Ok(name.clone()),
        _ => Err(())
    }
}


fn parse_one_src(context: &ParserContext, input: &mut Parser) -> Result<Source, ()> {
    let url = match input.next() {
        // Parsing url()
        Ok(Token::Url(url)) => {
            UrlParser::new().base_url(context.base_url).parse(&url).unwrap_or_else(
                |_error| Url::parse("about:invalid").unwrap())
        },
        // Parsing local() with early return
        Ok(Token::Function(name)) => {
            if name.eq_ignore_ascii_case("local") {
                return Ok(Source::Local(try!(input.parse_nested_block(|input| {
                    parse_one_non_generic_family_name(input)
                }))))
            }
            return Err(())
        },
        _ => return Err(())
    };

    // Parsing optional format()
    let format_hints = if input.try(|input| input.expect_function_matching("format")).is_ok() {
        try!(input.parse_nested_block(|input| {
            input.parse_comma_separated(|input| {
                Ok((try!(input.expect_string())).into_owned())
            })
        }))
    } else {
        vec![]
    };

    Ok(Source::Url(UrlSource {
        url: url,
        format_hints: format_hints,
    }))
}
