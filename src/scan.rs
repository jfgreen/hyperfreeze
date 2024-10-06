use std::fmt::Display;
use std::str::CharIndices;

//TODO: Support windows style newlines
//TODO: Support escaping special chars
//TODO: Better error reporting -> what went wrong, where

//TODO: Can we make peek more basic, therefore less mode or context dependant?
//TODO: Just chars?
//TODO: Or a bunch of context sensitive questions
#[derive(Debug)]
pub enum Peek {
    Hash,
    Text,
    Blockbreak,
    Linebreak,
    Whitespace,
    Delimiter(Delimiter),
    EndOfFile,
}

#[derive(PartialEq, Eq, Debug, Clone, Copy)]
pub enum Delimiter {
    Asterisk,
    Tilde,
    Underscore,
    Backtick,
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

fn usable_in_word(c: char) -> bool {
    !(c == '_' || c == '`' || c == '*' || c == '~' || c.is_whitespace())
}

fn usable_in_raw(c: char) -> bool {
    !(c == '\n' || c == '`')
}

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
            Some('\n') if self.next_char == Some('\n') => Peek::Blockbreak,
            Some('\n') if self.next_char.is_none() => Peek::EndOfFile,
            Some('\n') => Peek::Linebreak,
            Some(c) if c.is_whitespace() => Peek::Whitespace,
            Some('*') => Peek::Delimiter(Delimiter::Asterisk),
            Some('`') => Peek::Delimiter(Delimiter::Backtick),
            Some('~') => Peek::Delimiter(Delimiter::Tilde),
            Some('_') => Peek::Delimiter(Delimiter::Underscore),
            Some('#') => Peek::Hash,
            Some(_) => Peek::Text,
            None => Peek::EndOfFile,
        }
    }

    //TODO: Are these funcs eating single chars overkill?
    pub fn eat_hash(&mut self) -> ScannerResult<()> {
        self.eat_char('#')
    }

    pub fn eat_colon(&mut self) -> ScannerResult<()> {
        self.eat_char(':')
    }

    pub fn eat_delimiter(&mut self, delimiter: Delimiter) -> ScannerResult<char> {
        let character = match delimiter {
            Delimiter::Underscore => '_',
            Delimiter::Asterisk => '*',
            Delimiter::Tilde => '~',
            Delimiter::Backtick => '`',
        };
        self.eat_char(character)?;
        Ok(character)
    }

    pub fn eat_linebreak(&mut self) -> ScannerResult<()> {
        self.eat_char('\n')
    }

    pub fn eat_blockbreak(&mut self) -> ScannerResult<()> {
        self.eat_char('\n')?;
        self.eat_char('\n')?;
        self.skip_while_current(|c| c == '\n');
        Ok(())
    }

    //TODO: Clear up repetition of these funcs

    pub fn eat_identifier(&mut self) -> ScannerResult<&'a str> {
        match self.current_char {
            Some(c) if c.is_alphabetic() => Ok(self.eat_while(char::is_alphanumeric)),
            Some(c) => Err(ScannerError::UnexpectedChar(c)),
            None => Err(ScannerError::UnexpectedEndOfFile),
        }
    }

    pub fn eat_text_fragment(&mut self) -> ScannerResult<&'a str> {
        match self.current_char {
            Some(c) if usable_in_word(c) => Ok(self.eat_while(usable_in_word)),
            Some(c) => Err(ScannerError::UnexpectedChar(c)),
            None => Err(ScannerError::UnexpectedEndOfFile),
        }
    }

    pub fn eat_raw_fragment(&mut self) -> ScannerResult<&'a str> {
        match self.current_char {
            Some(c) if usable_in_raw(c) => Ok(self.eat_while(usable_in_raw)),
            Some(c) => Err(ScannerError::UnexpectedChar(c)),
            None => Err(ScannerError::UnexpectedEndOfFile),
        }
    }

    pub fn eat_until_linebreak(&mut self) -> ScannerResult<&'a str> {
        match self.current_char {
            Some(_) => Ok(self.eat_while(|c| c != '\n')),
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

    fn eat_char(&mut self, expected: char) -> ScannerResult<()> {
        match self.current_char {
            Some(c) if c == expected => {
                self.read_next_char();
                Ok(())
            }
            Some(c) => Err(ScannerError::UnexpectedChar(c)),
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
