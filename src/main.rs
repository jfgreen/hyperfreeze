use std::fmt::Display;
use std::fs::File;
use std::io::prelude::*;

use crate::parse::{parse_str, ParseError};
use crate::render::render_html;

mod document;
mod parse;
mod render;
mod scan;
mod tokenise;

// Implementation ideas we could look into:
// - Stream processing (as opposed loading whole input into memory)
// - Pre allocate sensible vec capacities?
// - SIMD prefix matching - byte patterns - 'is_alphanumeric' could be range check?

// TODO:
// Features to build
// - References
// - Better error reporting -> what went wrong, where
// - Consider enforcing a system for IDs, e.g J Decimal
// - Consider enforcing basic metadata?
// - Strip leading/trailing whitespace from para
//
// Approaches to try out
// - Fuzz testing

#[derive(Debug)]
enum Error {
    ParseError(ParseError),
    IOError(std::io::Error),
}

impl From<ParseError> for Error {
    fn from(err: ParseError) -> Self {
        Self::ParseError(err)
    }
}

impl From<std::io::Error> for Error {
    fn from(err: std::io::Error) -> Self {
        Self::IOError(err)
    }
}

impl Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Error::ParseError(err) => write!(f, "Parse error: {}", err),
            Error::IOError(err) => write!(f, "IO error: {}", err),
        }
    }
}

fn main() {
    parse_doc().unwrap_or_else(|err| {
        eprint!("{err}");
        std::process::exit(1);
    });
}

fn parse_doc() -> Result<(), Error> {
    let (input_path, output_path) = parse_args(std::env::args());

    let mut input = String::new();
    let mut in_file = File::open(&input_path)?;
    in_file.read_to_string(&mut input)?;

    let doc = parse_str(&input)?;

    let mut out_file = File::create(&output_path)?;
    render_html(&doc, &mut out_file)?;

    Ok(())
}

fn parse_args(args: std::env::Args) -> (String, String) {
    let mut args = args.into_iter();
    let exec = args.next();
    match (args.next(), args.next()) {
        (Some(input_path), Some(output_path)) => (input_path, output_path),
        _ => {
            let exec = exec.as_deref().unwrap_or("hyperfreeze");
            println!("Usage: {exec} [input_path] [output_path]");
            std::process::exit(1);
        }
    }
}

//TODO: Helpful error printing
// writeln!(
//     f,
//     "\nEncountered at line {} column {}:",
//     self.input_line, self.input_column
// )?;
// writeln!(f)?;
// writeln!(f, "{}", failing_line)?;
// for _ in 1..self.input_column {
//     write!(f, " ")?;
// }
// writeln!(f, "^")?;
