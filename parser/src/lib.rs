pub mod ast;
pub(crate) mod expression;
pub(crate) mod instruction;
pub(crate) mod literal;
use self::{ast::PrintVisitor, ast::Stmt, ast::VResult, ast::Visitable, instruction::program};
use ptree::print_tree;

use nom::{
  combinator::map,
  error::{FromExternalError, ParseError},
  IResult,
};
use std::num::ParseFloatError;

pub fn parser_entrypoint<'i, E>(input: &'i str) -> IResult<&'i str, Box<Stmt>, E>
where
  E: ParseError<&'i str> + FromExternalError<&'i str, ParseFloatError>,
{
  map(program, move |list| Box::new(Stmt::Block(list)))(input)
}

pub fn print_ast(input_file: String, tree: &mut Stmt) {
  let mut printer = PrintVisitor::new(input_file);
  tree.accept(&mut printer);
  let output_tree = printer.root.build();
  print_tree(&output_tree).expect("Error printing tree");
}
