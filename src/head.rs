// FIXME: Should position be in this module
use crate::{scan::Position, token::Token};

//TODO: Last token being in here is a bit of a smell

//TODO: Can we save some bytes by removing option
// e.g StartToken
#[derive(Debug, Copy, Clone)]
pub struct ReadHead<'a> {
    //FIX:ME make last_token not be pub
    pub last_token: Option<Token<'a>>,
    //TODO: Can we avoid both input and input_bytes?
    input: &'a str,
    input_bytes: &'a [u8],
    //TODO: direct usage of index, column is a bit meh
    index: usize,
    column: u32,
    row: u32,
    span_start: usize,
}

impl<'a> ReadHead<'a> {
    pub fn new(input: &'a str, last_token: Option<Token<'a>>, position: Position) -> Self {
        Self {
            last_token,
            input,
            input_bytes: input.as_bytes(),
            index: position.index,
            column: position.column,
            row: position.row,
            span_start: position.index,
        }
    }

    pub fn begin_span(&mut self) {
        self.span_start = self.index;
    }

    pub fn end_span(&mut self) -> Option<&'a str> {
        let start = self.span_start;
        let end = self.index;

        if start != end {
            Some(&self.input[start..end])
        } else {
            None
        }
    }

    pub fn position(&self) -> Position {
        Position {
            column: self.column,
            row: self.row,
            index: self.index,
        }
    }

    pub fn read_next_byte(&mut self) {
        let next_index = self.index + 1;
        if next_index < self.input_bytes.len() {
            if self.input_bytes[self.index] == b'\n' {
                self.column = 0;
                self.row += 1;
            } else {
                self.column += 1;
            }
            self.index = next_index;
        } else {
            self.index = self.input_bytes.len();
        }
    }

    pub fn is_on(&self, byte: u8) -> bool {
        self.input_bytes.get(self.index).copied() == Some(byte)
    }

    pub fn is_on_bytes(&self, pattern: &[u8]) -> bool {
        self.input_bytes[self.index..].starts_with(pattern)
    }

    pub fn is_on_one_of(&self, bytes: &[u8]) -> bool {
        bytes.iter().copied().any(|b| self.is_on(b))
    }

    pub fn is_end_of_input(&self) -> bool {
        self.index >= self.input_bytes.len()
    }

    pub fn is_on_ascii_alphanumeric(&self) -> bool {
        self.input_bytes
            .get(self.index)
            .is_some_and(u8::is_ascii_alphanumeric)
    }

    pub fn has_input_remaining(&self) -> bool {
        !self.is_end_of_input()
    }
}
