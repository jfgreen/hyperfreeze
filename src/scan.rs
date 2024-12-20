use std::fmt::Display;
use std::str::CharIndices;

#[derive(Debug)]
pub enum Peek {
    Char(char),
    Blockbreak,
    Linebreak,
    EndOfFile,
}

pub trait CharExt {
    fn usable_in_word(&self) -> bool;
    fn usable_in_raw(&self) -> bool;
    fn is_delimiter(&self) -> bool;
}

impl CharExt for char {
    fn usable_in_word(&self) -> bool {
        !(self.is_delimiter() || self.is_whitespace() || *self == BACKSLASH)
    }

    fn usable_in_raw(&self) -> bool {
        !(*self == NEW_LINE || *self == BACKTICK)
    }

    fn is_delimiter(&self) -> bool {
        *self == UNDERSCORE || *self == BACKTICK || *self == ASTERISK || *self == TILDE
    }
}

#[derive(PartialEq, Eq, Debug)]
pub enum ScannerError {
    UnexpectedEndOfFile,
    UnexpectedChar(char),
}

impl Display for ScannerError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ScannerError::UnexpectedChar(c) => write!(f, "unexpected char '{}'", c),
            ScannerError::UnexpectedEndOfFile => write!(f, "unexpected end of file"),
        }
    }
}

type ScannerResult<T> = Result<T, ScannerError>;

type CharPredicate = fn(char) -> bool;

pub mod chars {
    pub const NEW_LINE: char = '\n';
    pub const BACKTICK: char = '`';
    pub const ASTERISK: char = '*';
    pub const TILDE: char = '~';
    pub const UNDERSCORE: char = '_';
    pub const HASH: char = '#';
    pub const COLON: char = ':';
    pub const BACKSLASH: char = '\\';
}

use chars::*;

pub struct Scanner<'a> {
    input: &'a str,
    chars: CharIndices<'a>,
    current_char: Option<char>,
    current_index: usize,
    next_char: Option<char>,
    next_index: usize,
}

impl<'a> Scanner<'a> {
    pub fn new(input: &'a str) -> Self {
        let mut scanner = Self {
            input,
            chars: input.char_indices(),
            current_char: None,
            current_index: 0,
            next_char: None,
            next_index: 0,
        };

        // Place the first char of the input into `next_char`
        scanner.read_next_char();
        // Then propagate it into `current_char`
        scanner.read_next_char();

        scanner
    }

    pub fn peek(&self) -> Peek {
        match self.current_char {
            Some(NEW_LINE) if self.next_char == Some(NEW_LINE) => Peek::Blockbreak,
            Some(NEW_LINE) if self.next_char.is_none() => Peek::EndOfFile,
            Some(NEW_LINE) => Peek::Linebreak,
            Some(c) => Peek::Char(c),
            None => Peek::EndOfFile,
        }
    }

    pub fn eat_blockbreak(&mut self) -> ScannerResult<()> {
        self.eat_expected_char(NEW_LINE)?;
        self.eat_expected_char(NEW_LINE)?;
        self.skip_while_current(|c| c == NEW_LINE);
        Ok(())
    }

    pub fn eat_identifier(&mut self) -> ScannerResult<&'a str> {
        match self.current_char {
            Some(c) if c.is_alphabetic() => Ok(self.eat_while(char::is_alphanumeric)),
            Some(c) => Err(ScannerError::UnexpectedChar(c)),
            None => Err(ScannerError::UnexpectedEndOfFile),
        }
    }

    pub fn eat_text_fragment(&mut self) -> ScannerResult<&'a str> {
        match self.current_char {
            Some(c) if c.usable_in_word() => Ok(self.eat_while(|c| c.usable_in_word())),
            Some(c) => Err(ScannerError::UnexpectedChar(c)),
            None => Err(ScannerError::UnexpectedEndOfFile),
        }
    }

    pub fn eat_raw_fragment(&mut self) -> ScannerResult<&'a str> {
        match self.current_char {
            Some(c) if c.usable_in_raw() => Ok(self.eat_while(|c| c.usable_in_raw())),
            Some(c) => Err(ScannerError::UnexpectedChar(c)),
            None => Err(ScannerError::UnexpectedEndOfFile),
        }
    }

    pub fn eat_until_linebreak(&mut self) -> ScannerResult<&'a str> {
        match self.current_char {
            Some(_) => Ok(self.eat_while(|c| c != NEW_LINE)),
            None => Err(ScannerError::UnexpectedEndOfFile),
        }
    }

    pub fn eat_whitespace(&mut self) -> ScannerResult<()> {
        match self.current_char {
            Some(c) if c.is_whitespace() => {
                self.skip_while_current(char::is_whitespace);
                Ok(())
            }
            Some(c) => Err(ScannerError::UnexpectedChar(c)),
            None => Err(ScannerError::UnexpectedEndOfFile),
        }
    }

    pub fn eat_optional_whitespace(&mut self) {
        self.skip_while_current(char::is_whitespace)
    }

    pub fn eat_expected_char(&mut self, expected: char) -> ScannerResult<()> {
        match self.current_char {
            Some(c) if c == expected => {
                self.read_next_char();
                Ok(())
            }
            Some(c) => Err(ScannerError::UnexpectedChar(c)),
            None => Err(ScannerError::UnexpectedEndOfFile),
        }
    }

    pub fn eat_char(&mut self) -> ScannerResult<char> {
        match self.current_char {
            Some(c) => {
                self.read_next_char();
                Ok(c)
            }
            None => Err(ScannerError::UnexpectedEndOfFile),
        }
    }

    fn eat_while(&mut self, predicate: CharPredicate) -> &'a str {
        let i1 = self.current_index;
        self.skip_while_current(predicate);
        let i2 = self.current_index;
        &self.input[i1..i2]
    }

    fn skip_while_current(&mut self, predicate: CharPredicate) {
        while self.current_char.is_some_and(predicate) {
            self.read_next_char();
        }
    }

    fn read_next_char(&mut self) {
        self.current_char = self.next_char;
        self.current_index = self.next_index;

        if let Some((i, c)) = self.chars.next() {
            self.next_char = Some(c);
            self.next_index = i;
        } else {
            self.next_char = None;
            self.next_index = self.input.len();
        }
    }
}
