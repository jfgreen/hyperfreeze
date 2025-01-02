use std::str::CharIndices;

#[derive(PartialEq, Eq, Debug, Clone, Copy)]
pub enum Token<'a> {
    EndOfFile,
    Blockbreak,
    Linebreak,
    BlockHeader(&'a str),
    Text(&'a str),
    Whitespace,
    Identifier(&'a str),
    Delimiter(Delimiter),
    Colon,
}

#[derive(PartialEq, Eq, Debug, Clone, Copy)]
pub enum Delimiter {
    Backtick,
    Asterisk,
    Tilde,
    Underscore,
}

const NEW_LINE: char = '\n';
const BACKTICK: char = '`';
const ASTERISK: char = '*';
const TILDE: char = '~';
const UNDERSCORE: char = '_';
const HASH: char = '#';
const COLON: char = ':';
const BACKSLASH: char = '\\';

trait CharExt {
    fn usable_in_word(&self) -> bool;
    fn usable_in_raw(&self) -> bool;
    fn usable_in_identifier(&self) -> bool;
    fn is_delimiter(&self) -> bool;
}

impl CharExt for char {
    fn usable_in_word(&self) -> bool {
        !(self.is_delimiter() || self.is_whitespace() || *self == BACKSLASH)
    }

    fn usable_in_raw(&self) -> bool {
        !(*self == NEW_LINE || *self == BACKTICK)
    }

    fn usable_in_identifier(&self) -> bool {
        self.is_alphanumeric()
    }

    fn is_delimiter(&self) -> bool {
        *self == UNDERSCORE || *self == BACKTICK || *self == ASTERISK || *self == TILDE
    }
}

#[derive(PartialEq, Eq, Clone, Copy, Debug)]
pub enum ScanContext {
    Base,
    Metadata,
    Paragraph,
    InlineRaw,
}

type CharPredicate = fn(char) -> bool;

pub struct Scanner<'a> {
    input: &'a str,
    chars: CharIndices<'a>,
    current_char: Option<char>,
    current_index: usize,
    context_stack: Vec<ScanContext>,
    peeked_token: Option<Token<'a>>,
    column: usize,
    row: usize,
}

impl<'a> Scanner<'a> {
    pub fn new(input: &'a str) -> Self {
        let mut scanner = Self {
            input,
            chars: input.char_indices(),
            current_char: None,
            current_index: 0,
            context_stack: vec![],
            peeked_token: None,
            column: 0,
            row: 0,
        };

        // Place the first char of the input into `next_char`
        scanner.read_next_char();

        scanner
    }

    pub fn peek(&mut self) -> Token<'a> {
        match self.peeked_token {
            Some(token) => token,
            None => {
                let token = self.read_token();
                self.peeked_token = Some(token);
                token
            }
        }
    }

    pub fn next(&mut self) -> Token<'a> {
        match self.peeked_token {
            Some(token) => {
                self.peeked_token = None;
                token
            }
            None => self.read_token(),
        }
    }

    fn read_token(&mut self) -> Token<'a> {
        let context = *self.context_stack.last().unwrap_or(&ScanContext::Base);

        use Delimiter::*;
        use ScanContext::*;

        //TODO: Try "borrowing" the right mode? Reset peek on return;
        //TODO: Track line and column in each token
        //TODO: Test we report correct line number
        //TODO: Pull different contexts out into seperate match statements
        //TODO: Modal Tokens! (i.e contrained set per mode)
        //TODO: Make the order less fragile

        let token = match self.current_char {
            Some(NEW_LINE) => {
                self.read_next_char();
                match self.current_char {
                    Some(NEW_LINE) => {
                        self.skip_while(|c| c == NEW_LINE);
                        Token::Blockbreak
                    }
                    None => Token::Blockbreak,
                    _ => Token::Linebreak,
                }
            }
            Some(HASH) if context == Base => {
                self.read_next_char();
                let name = self.eat_while(char::is_alphanumeric);
                Token::BlockHeader(name)
            }
            //TODO: Escaped chars in other modes
            Some(BACKSLASH) if context == Paragraph => {
                self.read_next_char();
                if self.current_char.is_some() {
                    Token::Text(self.eat_char())
                } else {
                    Token::EndOfFile
                }
            }
            Some(c) if c.usable_in_identifier() && context == Metadata && self.column == 1 => {
                let identifier = self.eat_while(|c| c.usable_in_identifier());
                Token::Identifier(identifier)
            }
            Some(COLON) if context == Metadata => {
                self.read_next_char();
                Token::Colon
            }
            Some(c) if c.usable_in_raw() && context == InlineRaw => {
                let fragment = self.eat_while(|c| c.usable_in_raw());
                //TODO: Ideally this should be a raw fragment token, exclusive to markup mode
                Token::Text(fragment)
            }
            Some(c) if c.is_whitespace() => {
                self.eat_while(char::is_whitespace);
                Token::Whitespace
            }
            Some(_) if context == Metadata => {
                let text = self.eat_while(|c| c != '\n');
                Token::Text(text)
            }
            Some(c) if c.usable_in_word() => {
                let word = self.eat_while(|c| c.usable_in_word());
                //TODO: Ideally this should be a word token, exclusive to markup mode
                Token::Text(word)
            }
            Some(c) if c.is_whitespace() => {
                self.eat_while(char::is_whitespace);
                Token::Whitespace
            }
            Some(BACKTICK) => {
                self.read_next_char();
                Token::Delimiter(Backtick)
            }
            Some(ASTERISK) => {
                self.read_next_char();
                Token::Delimiter(Asterisk)
            }
            Some(TILDE) => {
                self.read_next_char();
                Token::Delimiter(Tilde)
            }
            Some(UNDERSCORE) => {
                self.read_next_char();
                Token::Delimiter(Underscore)
            }
            Some(c) => {
                dbg!(c);
                todo!()
            }
            None => Token::EndOfFile,
        };

        dbg!(token);

        token
    }

    //TODO: More ergonomic to have a method per context (e.g push_context_paragraph())
    pub fn push_context(&mut self, context: ScanContext) {
        self.context_stack.push(context);
    }

    pub fn pop_context(&mut self) {
        self.context_stack.pop();
    }

    fn eat_while(&mut self, predicate: CharPredicate) -> &'a str {
        let i1 = self.current_index;
        self.skip_while(predicate);
        let i2 = self.current_index;
        &self.input[i1..i2]
    }

    fn eat_char(&mut self) -> &'a str {
        let i1 = self.current_index;
        self.read_next_char();
        let i2 = self.current_index;
        &self.input[i1..i2]
    }

    fn skip_while(&mut self, predicate: CharPredicate) {
        while self.current_char.is_some_and(predicate) {
            self.read_next_char();
        }
    }

    fn read_next_char(&mut self) {
        if let Some((i, c)) = self.chars.next() {
            if c == '\n' {
                self.column = 0;
                self.row += 1;
            } else {
                self.column += 1;
            }

            self.current_char = Some(c);
            self.current_index = i;
        } else {
            self.current_char = None;
            self.current_index = self.input.len();
        }
    }
}
