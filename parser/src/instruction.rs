use super::ast::{Declaration, Expr, Literal, OpType, OrderType, Stmt};
use super::expression::{expr, logic_or};
use super::literal::parse_identifier;
use super::literal::parse_string;
use nom::{
  branch::alt,
  bytes::complete::tag,
  character::complete::{char, multispace0, multispace1, newline, space0, space1},
  combinator::{eof, map, opt},
  error::{FromExternalError, ParseError},
  multi::{many0, separated_list0},
  sequence::{delimited, pair, preceded, separated_pair, terminated, tuple},
  IResult,
};
use std::borrow::Cow;
use std::num::ParseFloatError;
use std::ops::Deref;

pub fn program<'i, E>(input: &'i str) -> IResult<&'i str, Vec<Declaration>, E>
where
  E: ParseError<&'i str> + FromExternalError<&'i str, ParseFloatError>,
{
  delimited(multispace0, many0(declaration), eof)(input)
}

// Declaration parsers
pub(crate) fn declaration<'i, E>(input: &'i str) -> IResult<&'i str, Declaration, E>
where
  E: ParseError<&'i str> + FromExternalError<&'i str, ParseFloatError>,
{
  alt((var_decl, fun_decl, statement))(input)
}

pub(crate) fn fun_decl<'i, E>(input: &'i str) -> IResult<&'i str, Declaration, E>
where
  E: ParseError<&'i str> + FromExternalError<&'i str, ParseFloatError>,
{
  preceded(tag("fn"), function)(input)
}

pub(crate) fn var_decl<'i, E>(input: &'i str) -> IResult<&'i str, Declaration, E>
where
  E: ParseError<&'i str> + FromExternalError<&'i str, ParseFloatError>,
{
  map(
    preceded(
      tag("let"),
      terminated(
        preceded(
          space1,
          pair(
            parse_identifier,
            opt(pair(delimited(space0, char('='), space0), expr)),
          ),
        ),
        multispace1,
      ),
    ),
    |(id, exp)| {
      let parsed_id = Box::new(Expr::Id(id.into()));
      match &exp {
        Some((_, e)) => Declaration::VarDecl(parsed_id, Some(e.to_owned())),
        None => Declaration::VarDecl(parsed_id, None),
      }
    },
  )(input)
}

//statement parsers
pub(crate) fn statement<'i, E>(input: &'i str) -> IResult<&'i str, Declaration, E>
where
  E: ParseError<&'i str> + FromExternalError<&'i str, ParseFloatError>,
{
  map(alt((while_stmt, print_stmt, block, expr_stmt)), |stmt| {
    Declaration::Statement(Box::new(stmt))
  })(input)
}

pub(crate) fn expr_stmt<'i, E>(input: &'i str) -> IResult<&'i str, Stmt, E>
where
  E: ParseError<&'i str> + FromExternalError<&'i str, ParseFloatError>,
{
  map(terminated(expr, multispace1), |exp| Stmt::ExprStmt(exp))(input)
}

pub(crate) fn print_stmt<'i, E>(input: &'i str) -> IResult<&'i str, Stmt, E>
where
  E: ParseError<&'i str> + FromExternalError<&'i str, ParseFloatError>,
{
  map(
    terminated(
      preceded(
        tag("print"),
        delimited(
          char('('),
          delimited(space0, alt((expr, parse_string)), space0),
          char(')'),
        ),
      ),
      multispace1,
    ),
    |exp| Stmt::Print(exp),
  )(input)
}

pub(crate) fn while_stmt<'i, E>(input: &'i str) -> IResult<&'i str, Stmt, E>
where
  E: ParseError<&'i str> + FromExternalError<&'i str, ParseFloatError>,
{
  let (input, exp) = terminated(
    preceded(
      tag("while"),
      delimited(
        preceded(space1, char('(')),
        delimited(space0, expr, space0),
        char(')'),
      ),
    ),
    multispace1,
  )(input)?;
  let (input, list) = statement(input)?;
  let ret = match &list {
    Declaration::Statement(blk) => Stmt::While(exp, blk.to_owned()),
    _ => panic!("unable to parse while statement {}", input),
  };
  Ok((input, ret))
}

pub(crate) fn block<'i, E>(input: &'i str) -> IResult<&'i str, Stmt, E>
where
  E: ParseError<&'i str> + FromExternalError<&'i str, ParseFloatError>,
{
  map(
    tuple((
      terminated(tag("{"), multispace1),
      many0(declaration),
      terminated(tag("}"), multispace1),
    )),
    |(_, decl_list, _)| Stmt::Block(decl_list),
  )(input)
}

fn empty_params<'i, E>(input: &'i str) -> IResult<&'i str, Vec<&'i str>, E> {
  Ok((input, vec![]))
}

fn parse_params<'i, E>(input: &'i str) -> IResult<&'i str, Vec<&'i str>, E>
where
  E: ParseError<&'i str>,
{
  map(
    pair(
      parse_identifier,
      many0(preceded(tag(","), parse_identifier)),
    ),
    |(p, ps)| [&vec![p][..], &ps[..]].concat(),
  )(input)
}

pub(crate) fn function<'i, E>(input: &'i str) -> IResult<&'i str, Declaration, E>
where
  E: ParseError<&'i str> + FromExternalError<&'i str, ParseFloatError>,
{
  map(
    tuple((
      space1,
      parse_identifier,
      char('('),
      alt((parse_params, empty_params)),
      char(')'),
      block,
    )),
    move |(_, name, _, params, _, blck)| Declaration::FuncDecl(name.into(), params, Box::new(blck)),
  )(input)
}

mod test {
  use super::*;
  use nom::error::Error;

  #[test]
  fn test_print_stmt() {
    let test1 = print_stmt::<Error<&str>>("print(x)\n").unwrap();
    let test2 = print_stmt::<Error<&str>>("print(\"test\")\n").unwrap();
    let test3 = print_stmt::<Error<&str>>("print(x + y)\n").unwrap();
    assert_eq!(test1, ("", Stmt::Print(Expr::Id(Cow::Borrowed("x")))));
    assert_eq!(
      test2,
      (
        "",
        Stmt::Print(Expr::ExprLiteral(Literal::StringLiteral(Cow::Borrowed(
          "test"
        ))))
      )
    );
    assert_eq!(
      test3,
      (
        "",
        Stmt::Print(Expr::OpExpr(
          OpType::Add,
          Box::new(Expr::Id(Cow::Borrowed("x"))),
          Box::new(Expr::Id(Cow::Borrowed("y"))),
        ))
      )
    );
  }
}
