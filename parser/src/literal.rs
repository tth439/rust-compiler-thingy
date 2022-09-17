use super::ast::{Expr, Literal};
use nom::{
    branch::alt,
    bytes::complete::{escaped_transform, tag, take_while},
    character::complete::{char, digit1, line_ending, none_of},
    character::{is_alphabetic, is_alphanumeric},
    combinator::{fail, map, opt, recognize, value},
    error::{FromExternalError, ParseError},
    sequence::{delimited, tuple},
    IResult,
};

pub(crate) fn parse_bool<'i, E: ParseError<&'i str>>(
    input: &'i str,
) -> IResult<&'i str, Literal, E> {
    alt((
        value(Literal::BoolLiteral(true), tag("true")),
        value(Literal::BoolLiteral(false), tag("false")),
    ))(input)
}

pub(crate) fn parse_number<'i, E>(input: &'i str) -> IResult<&'i str, Literal, E>
where
    E: ParseError<&'i str> + FromExternalError<&'i str, std::num::ParseFloatError>,
{
    map(
        recognize(tuple((
            opt(char('-')),
            digit1,
            opt(tuple((char('.'), digit1))),
            opt(tuple((
                alt((char('E'), char('e'))),
                opt(alt((char('+'), char('-')))),
            ))),
        ))),
        |float_str: &'i str| Literal::NumLiteral(float_str.parse::<f32>().unwrap()),
    )(input)
}

pub(crate) fn parse_string<'i, E>(input: &'i str) -> IResult<&'i str, Expr, E>
where
    E: ParseError<&'i str>,
{
    use std::borrow::Cow;
    map(
        delimited(
            alt((char('"'), char('\''))),
            escaped_transform(
                none_of("\"\\"),
                '\\',
                alt((
                    value("", line_ending),
                    value("\t", char('t')),
                    value("\"", char('"')),
                    value("\n", char('n')),
                )),
            ),
            alt((char('"'), char('\''))),
        ),
        |parsed_str: String| {
            Expr::ExprLiteral(Literal::StringLiteral(Cow::Owned(parsed_str.to_string())))
        },
    )(input)
}

pub(crate) fn parse_identifier<'i, E>(input: &'i str) -> IResult<&'i str, &'i str, E>
where
    E: ParseError<&'i str>,
{
    if let Some(chr) = input.chars().next() {
        if !is_alphabetic(chr as u8) {
            return fail(input);
        }
    } else {
        return fail(input);
    }
    take_while(|c| is_alphanumeric(c as u8) || (c as char == '_'))(input)
}
