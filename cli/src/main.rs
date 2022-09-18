extern crate ptree;
use clap::{App, Arg};
use parser::{parser_entrypoint, print_ast};
use nom::error::Error;
use ptree::{print_tree, TreeBuilder};
use std::fs;
use std::io::prelude::*;
use std::io::{self, BufRead, Write};

fn main() {
    let mut verbose = false;
    let matches = App::new("wasm-lang")
        .version("1.0")
        .about("a compiler for a made up language")
        .arg(
            Arg::with_name("INPUT")
                .help("Sets the input file to compile")
                .required(true)
                .index(1),
        )
        .arg(
            Arg::with_name("v")
                .short("v")
                .multiple(true)
                .help("verbose"),
        )
        .get_matches();

    match matches.occurrences_of("v") {
        1 => {
            println!("[!] Verbose");
            verbose = true;
        }
        _ => {}
    }

    if let Some(input_file) = matches.value_of("INPUT") {
        println!("input file provided: {}", input_file);
        let mut contents =
            fs::read_to_string(input_file).expect("Something went wrong reading the file");
        let mut res = parser_entrypoint::<Error<&str>>(contents.as_str());
        match res {
            Ok((_, ref mut node)) => {
                if verbose {
                    print_ast(input_file.to_string(), node.as_mut());
                }
            }
            Err(reason) => eprintln!("Compilation error: {}", reason),
        }
    }
}
