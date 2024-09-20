use std::fmt::Display;
use std::fs::File;
use std::io::prelude::*;

use crate::parse::{parse_str, ParseError};
use crate::render::render_html;

mod parse;
mod render;
mod scan;

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

//TODO: Propper error messaging
fn main() -> Result<(), Error> {
    let (input_path, output_path) = parse_args(std::env::args());

    //TODO: Use buffered reader
    //TODO: Parse incrementally
    let mut input = String::new();
    let mut in_file = File::open(&input_path)?;
    in_file.read_to_string(&mut input)?;

    let doc = parse_str(&input)?;

    let mut out_file = File::create(&output_path)?;
    render_html(&doc, &mut out_file)?;

    println!("{}, {}", input_path, output_path);
    Ok(())
}

fn parse_args(args: std::env::Args) -> (String, String) {
    let mut args = args.into_iter();
    let exec = args.next();
    match (args.next(), args.next()) {
        (Some(input_path), Some(output_path)) => (input_path, output_path),
        _ => {
            let exec = exec.as_deref().unwrap_or("hyperfreeze");
            println!("Usage: {exec} [audio-file ...]");
            std::process::exit(1);
        }
    }
}
