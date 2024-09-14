use std::fmt::{Display, Write};
use std::str::CharIndices;

const DELIM_BOLD: char = '*';
const DELIM_EMPH: char = '_';
const DELIM_STRIKE: char = '~';
const DELIM_RAW: char = '`';

//TODO: Keep track of what line, row a token is on
//TODO: Seperate markup mode tokens from other modes?

//TODO: Are all these tokens needed?
#[derive(PartialEq, Eq, Debug)]
pub enum Token<'a> {
    BlockStart(BlockType),
    Blockbreak,
    Text(&'a str),
    Whitespace,
    RawDelimiter,
    FormatDelimiter(Format),
    Eof,
}

//TODO: Add paragraph (even though 99% of the time it gets sugared out)
#[derive(PartialEq, Eq, Debug)]
pub enum BlockType {
    Metadata,
}

#[derive(PartialEq, Eq, Debug, Clone, Copy)]
pub enum Format {
    Bold,
    Emphasis,
    Strikethrough,
}

pub struct Tokeniser<'a> {
    pub current_token: Token<'a>,
    input: &'a str,
    chars: CharIndices<'a>,
    current_char: Option<char>,
    current_index: usize,
}

#[derive(PartialEq, Eq, Debug)]
pub enum TokeniserError {
    UnexpectedChar,
    UnexpectedToken, //TODO: this will be removed once we loose current token in tokeniser
}

impl Display for TokeniserError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            &TokeniserError::UnexpectedChar => write!(f, "unexpected character"),
            &TokeniserError::UnexpectedToken => write!(f, "unexpected token"),
        }
    }
}

//TODO: Type alias for fn(char) -> bool

//TODO what about "contexts" that return a single value?
pub mod ctx {
    pub enum EndOfKV {
        Linebreak,
        Blockbreak,
        Eof,
    }
}

fn char_usable_in_text(c: char) -> bool {
    // TODO: Is there a more efficent way to do this?
    // Maybe by treating char as an int
    !(c == DELIM_BOLD
        || c == DELIM_EMPH
        || c == DELIM_STRIKE
        || c == DELIM_RAW
        || c.is_whitespace())
}

fn char_usable_in_raw(c: char) -> bool {
    // TODO: Is there a more efficent way to do this?
    // Maybe by treating char as an int
    !(c == DELIM_RAW || c == '\n')
}

//TODO: Tokeniser Result alias

//TODO: Remove "advance" funcs and move to expect based parseing where
// we can return an error if there are no valid ways to construct a token
// in a given context. Have an enum per token context?

impl<'a> Tokeniser<'a> {
    pub fn advance(&mut self) {
        self.current_token = match self.current_char {
            Some('#') => self.handle_block_start(),
            Some('\n') => self.handle_markup_newline(),
            Some(DELIM_BOLD) => self.handle_format_delimiter(Format::Bold),
            Some(DELIM_EMPH) => self.handle_format_delimiter(Format::Emphasis),
            Some(DELIM_STRIKE) => self.handle_format_delimiter(Format::Strikethrough),
            Some(DELIM_RAW) => self.handle_raw_delimiter(),
            Some(c) if c.is_whitespace() => self.handle_whitespace(),
            Some(_) => self.handle_text(),
            None => Token::Eof,
        }
    }

    pub fn advance_raw(&mut self) {
        self.current_token = match self.current_char {
            Some('\n') => self.handle_raw_newline(),
            Some(DELIM_RAW) => self.handle_raw_delimiter(),
            Some(_) => self.handle_text_raw(),
            None => Token::Eof,
        }
    }

    pub fn assert_at_meteadata_block_start(&self) -> Result<(), TokeniserError> {
        if self.current_token != Token::BlockStart(BlockType::Metadata) {
            Err(TokeniserError::UnexpectedToken)
        } else {
            Ok(())
        }
    }

    pub fn expect_linebreak(&mut self) -> Result<(), TokeniserError> {
        if self.current_char == Some('\n') {
            self.read_next_char();
            Ok(())
        } else {
            Err(TokeniserError::UnexpectedChar)
        }
    }

    pub fn expect_data_key(&mut self) -> Result<&'a str, TokeniserError> {
        //FIXME: If the following was the public interface... then we dont need the ctx module?
        self.assert_current_char_is(char::is_alphabetic)?;
        let text = self.eat_chars_while(char::is_alphabetic);
        self.eat_whitespace();
        self.eat_char(':')?;
        self.eat_whitespace();
        Ok(text)
    }

    pub fn expect_data_value(&mut self) -> Result<&'a str, TokeniserError> {
        //TODO: Normalise the value whitespace?
        self.assert_current_char_is(|c| c != '\n')?;
        let text = self.eat_chars_while(|c| c != '\n');
        Ok(text)
    }

    pub fn expect_end_of_key_value(&mut self) -> Result<ctx::EndOfKV, TokeniserError> {
        match self.current_char {
            Some('\n') => {
                self.read_next_char();
                if self.current_char == Some('\n') {
                    self.advance_while_current(|c| c == '\n');
                    Ok(ctx::EndOfKV::Blockbreak)
                } else if self.current_char == None {
                    Ok(ctx::EndOfKV::Eof)
                } else {
                    Ok(ctx::EndOfKV::Linebreak)
                }
            }
            None => Ok(ctx::EndOfKV::Eof),
            _ => Err(TokeniserError::UnexpectedChar),
        }
    }

    pub fn new(input: &'a str) -> Self {
        let mut tokeniser = Self {
            input,
            chars: input.char_indices(),
            current_char: None,
            current_index: 0,
            current_token: Token::Eof,
        };

        // Place the first char of the input into `current_char`
        tokeniser.read_next_char();

        // Then parse the first token into `current_token`
        tokeniser.advance();

        tokeniser
    }

    fn handle_block_start(&mut self) -> Token<'a> {
        self.read_next_char(); // Skip over #
        let text = self.eat_chars_while(|c| c.is_alphabetic());
        //TODO: Is doing the string match up front here overkill?
        match text {
            "metadata" => Token::BlockStart(BlockType::Metadata),
            _ => panic!(), //FIXME: remove panic
        }
    }

    fn handle_raw_newline(&mut self) -> Token<'a> {
        self.read_next_char();
        Token::Whitespace
    }

    fn handle_markup_newline(&mut self) -> Token<'a> {
        self.read_next_char();

        if self.current_char == Some('\n') {
            self.handle_block_break()
        } else {
            Token::Whitespace
        }
    }

    fn handle_block_break(&mut self) -> Token<'a> {
        // New lines are usually treated as whitespace. However, if there is
        // more than one in a row, they are all treated as a block break
        //TODO: add special case advance fun for current eq
        self.advance_while_current(|c| c == '\n');
        Token::Blockbreak
    }

    fn handle_whitespace(&mut self) -> Token<'a> {
        self.eat_whitespace();
        Token::Whitespace
    }

    fn handle_format_delimiter(&mut self, kind: Format) -> Token<'a> {
        self.read_next_char();
        Token::FormatDelimiter(kind)
    }

    fn handle_raw_delimiter(&mut self) -> Token<'a> {
        self.read_next_char();
        Token::RawDelimiter
    }

    // TODO: char_usable_in_text and char_usable_in_raw probably wont work
    // once we start allowing for escaped characters such as \*
    fn handle_text(&mut self) -> Token<'a> {
        let text = self.eat_chars_while(char_usable_in_text);
        Token::Text(text)
    }

    fn handle_text_raw(&mut self) -> Token<'a> {
        let text = self.eat_chars_while(char_usable_in_raw);
        Token::Text(text)
    }

    fn eat_char(&mut self, c: char) -> Result<(), TokeniserError> {
        if self.current_char == Some(c) {
            self.read_next_char();
            Ok(())
        } else {
            Err(TokeniserError::UnexpectedChar)
        }
    }

    fn eat_chars_while(&mut self, predicate: fn(char) -> bool) -> &'a str {
        let i1 = self.current_index;
        self.advance_while_current(predicate);
        let i2 = self.current_index;
        &self.input[i1..i2]
    }

    //TODO: make it clear the whitespace is optional?
    fn eat_whitespace(&mut self) {
        self.advance_while_current(char::is_whitespace)
    }

    fn assert_current_char_is(&self, predicate: fn(char) -> bool) -> Result<(), TokeniserError> {
        if self.current_char.is_some_and(predicate) {
            Ok(())
        } else {
            Err(TokeniserError::UnexpectedChar)
        }
    }

    fn read_next_char(&mut self) {
        if let Some((i, c)) = self.chars.next() {
            self.current_char = Some(c);
            self.current_index = i;
        } else {
            self.current_char = None;
            self.current_index = self.input.len();
        }
    }

    fn advance_while_current(&mut self, predicate: fn(char) -> bool) {
        while self.current_char.is_some_and(predicate) {
            self.read_next_char();
        }
    }
}
