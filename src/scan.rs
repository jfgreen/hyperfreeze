use std::fmt::Display;
use std::str::CharIndices;

//TODO: Are these semantic knowledge we dont want here...
const DELIM_BOLD: char = '*';
const DELIM_EMPH: char = '_';
const DELIM_STRIKE: char = '~';
const DELIM_RAW: char = '`';

//TODO: Support windows style newlines

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

// TODO: Peek only needs to look at a couple of chars to make a decision
// Therefore it does need to store a token

// TODO: The only thing that feels off about peek_
// is that it mixes in parsing level knowledge with lexing stuff
// (i.e its inherant int the name peek_markup, peek_inline_raw etc )

//TODO: Better error reporting -> what went wrong, where
//TODO: Review if we need an error here?
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

//TODO: Type alias for fn(char) -> bool

//TODO: Extension trait for these?j
fn usable_in_word(c: char) -> bool {
    // TODO: Is there a more efficent way to do this?
    // Maybe by treating char as an int
    !(c == DELIM_BOLD
        || c == DELIM_EMPH
        || c == DELIM_STRIKE
        || c == DELIM_RAW
        || c.is_whitespace())
}

//TODO: Cache the peek?
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
        // Then propigate it into `current_char`
        scanner.read_next_char();
        scanner.read_next_char();

        scanner
    }

    //TODO: Have one peek function with a hint param
    pub fn peek_start_of_block(&self) -> Peek {
        match self.current_char {
            Some('#') => Peek::Hash,
            _ => self.peek(),
        }
    }

    pub fn peek(&self) -> Peek {
        match self.current_char {
            Some('\n') if self.next_char == Some('\n') => Peek::Blockbreak,
            //FIXME: Bit of a hack?
            Some('\n') if self.next_char.is_none() => Peek::EndOfFile,
            Some('\n') => Peek::Linebreak,
            Some(c) if c.is_whitespace() => Peek::Whitespace,
            Some('*') => Peek::Delimiter(Delimiter::Asterisk),
            Some('`') => Peek::Delimiter(Delimiter::Backtick),
            Some('~') => Peek::Delimiter(Delimiter::Tilde),
            Some('_') => Peek::Delimiter(Delimiter::Underscore),
            Some(_) => Peek::Text,
            None => Peek::EndOfFile,
        }
    }

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
        self.skip_chars_while_current(|c| c == '\n');
        Ok(())
    }

    pub fn eat_identifier(&mut self) -> ScannerResult<&'a str> {
        match self.current_char {
            Some(c) if c.is_alphabetic() => Ok(self.eat_while(char::is_alphanumeric)),
            Some(c) => Err(ScannerError::UnexpectedChar(c)),
            None => Err(ScannerError::UnexpectedEndOfFile),
        }
    }

    pub fn eat_word(&mut self) -> ScannerResult<&'a str> {
        match self.current_char {
            Some(c) if usable_in_word(c) => Ok(self.eat_while(usable_in_word)),
            Some(c) => Err(ScannerError::UnexpectedChar(c)),
            None => Err(ScannerError::UnexpectedEndOfFile),
        }
    }

    pub fn eat_until_linebreak(&mut self) -> ScannerResult<&'a str> {
        //TODO: Should we throw an error if already on newline?
        // If so - drive with test for empty metadata value
        Ok(self.eat_while(|c| c != '\n'))
    }

    pub fn eat_whitespace(&mut self) -> ScannerResult<&'a str> {
        match self.current_char {
            Some(c) if c.is_whitespace() => Ok(self.eat_while(char::is_whitespace)),
            Some(c) => Err(ScannerError::UnexpectedChar(c)),
            None => Err(ScannerError::UnexpectedEndOfFile),
        }
    }

    pub fn eat_optional_whitespace(&mut self) {
        self.skip_chars_while_current(char::is_whitespace)
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

    //FIXME: Strictly we should return an error if we hit EOF
    // Otherwise if we call eat_mango, we dont know if we actually got a mango
    fn eat_while(&mut self, predicate: fn(char) -> bool) -> &'a str {
        let i1 = self.current_index;
        self.skip_chars_while_current(predicate);
        let i2 = self.current_index;
        &self.input[i1..i2]
    }

    fn skip_chars_while_current(&mut self, predicate: fn(char) -> bool) {
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
