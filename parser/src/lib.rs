pub mod ast;
pub(crate) mod expression;
pub(crate) mod instruction;
pub(crate) mod literal;
use self::{ast::Stmt, instruction::program};

use nom::{
    combinator::map,
    error::{FromExternalError, ParseError},
    IResult,
};
use std::num::ParseFloatError;

pub fn parser_entrypoint<'i, E>(input: &'i str) -> IResult<&'i str, Stmt, E>
where
    E: ParseError<&'i str> + FromExternalError<&'i str, ParseFloatError>,
{
    map(program, move |list| Stmt::Block(list))(input)
}
