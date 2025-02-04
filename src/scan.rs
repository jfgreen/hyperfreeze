use std::str::CharIndices;

#[derive(PartialEq, Eq, Debug, Clone, Copy)]
pub enum Token<'a> {
    EndOfFile,
    Blockbreak,
    Linebreak,
    BlockHeader(&'a str),
    ContainerHeader(&'a str),
    ContainerFooter,
    Text(&'a str),
    Whitespace,
    Identifier(&'a str),
    Delimiter(Delimiter),
    MetaText(&'a str),
    Colon,
    RawFragment(&'a str),
    Unknown,
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
const EQUALS: char = '=';
const LEFT_SQUARE_BRACKET: char = '[';
const RIGHT_SQUARE_BRACKET: char = ']';

trait CharExt {
    fn usable_in_text(&self) -> bool;
    fn usable_in_raw(&self) -> bool;
    fn usable_in_identifier(&self) -> bool;
    fn is_delimiter(&self) -> bool;
}

impl CharExt for char {
    fn usable_in_text(&self) -> bool {
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
        let context = self.context_stack.last().unwrap_or(&ScanContext::Base);

        match self.current_char {
            None => Token::EndOfFile,
            Some(NEW_LINE) => {
                self.read_next_char();
                match self.current_char {
                    Some(NEW_LINE) => {
                        self.skip_while(|c| c == NEW_LINE);
                        Token::Blockbreak
                    }
                    _ => Token::Linebreak,
                }
            }
            Some(c) => match context {
                ScanContext::Base => self.read_token_base(c),
                ScanContext::Metadata => self.read_token_metadata(c),
                ScanContext::Paragraph => self.read_token_paragraph(c),
                ScanContext::InlineRaw => self.read_token_inline_raw(c),
            },
        }

        //TODO: Track line and column in each token
        //TODO: Test we report correct line number
    }

    fn read_token_base(&mut self, first_char: char) -> Token<'a> {
        if first_char == HASH && self.column == 1 {
            self.read_next_char();
            return match self.current_char {
                Some(LEFT_SQUARE_BRACKET) => {
                    self.read_next_char();
                    let name = self.eat_while(char::is_alphanumeric);
                    match self.current_char {
                        Some(RIGHT_SQUARE_BRACKET) => {
                            self.read_next_char();
                            Token::ContainerHeader(name)
                        }
                        _ => Token::Unknown,
                    }
                }
                _ => {
                    let name = self.eat_while(char::is_alphanumeric);
                    Token::BlockHeader(name)
                }
            };
        }

        if first_char.is_whitespace() {
            self.eat_while(char::is_whitespace);
            return Token::Whitespace;
        }

        // NOTE: Each mode needs to detect sentinal tokens that delimit
        // one context from another. But read_token_base also needs to
        // correctly infer the first token of a paragraph or list when
        // the block header is ommited (a valid syntactical sugar)
        self.read_token_paragraph(first_char)
    }

    fn read_token_metadata(&mut self, first_char: char) -> Token<'a> {
        //TODO: Pull out EOF stuff up also
        match first_char {
            c if c.usable_in_identifier() && self.column == 1 => {
                let identifier = self.eat_while(|c| c.usable_in_identifier());
                Token::Identifier(identifier)
            }
            COLON => {
                self.read_next_char();
                Token::Colon
            }
            c if c.is_whitespace() => {
                self.eat_while(char::is_whitespace);
                Token::Whitespace
            }
            _ => {
                let text = self.eat_while(|c| c != '\n');
                Token::MetaText(text)
            }
        }
    }

    fn read_token_paragraph(&mut self, first_char: char) -> Token<'a> {
        match first_char {
            //TODO: Bit of a hack?
            HASH if self.column == 1 => {
                self.read_next_char();
                //FIXME: Should enforce at least one =
                self.eat_while(|c| c == EQUALS);
                Token::ContainerFooter
            }
            BACKSLASH => {
                self.read_next_char();
                if self.current_char.is_some() {
                    Token::Text(self.eat_char())
                } else {
                    Token::EndOfFile
                }
            }
            c if c.is_whitespace() => {
                self.eat_while(char::is_whitespace);
                Token::Whitespace
            }
            c if c.usable_in_text() => {
                let text = self.eat_while(|c| c.usable_in_text());
                Token::Text(text)
            }
            BACKTICK => {
                self.read_next_char();
                Token::Delimiter(Delimiter::Backtick)
            }
            ASTERISK => {
                self.read_next_char();
                Token::Delimiter(Delimiter::Asterisk)
            }
            TILDE => {
                self.read_next_char();
                Token::Delimiter(Delimiter::Tilde)
            }
            UNDERSCORE => {
                self.read_next_char();
                Token::Delimiter(Delimiter::Underscore)
            }
            _ => Token::Unknown,
        }
    }

    fn read_token_inline_raw(&mut self, first_char: char) -> Token<'a> {
        match first_char {
            c if c.usable_in_raw() => {
                let fragment = self.eat_while(|c| c.usable_in_raw());
                Token::RawFragment(fragment)
            }
            BACKTICK => {
                self.read_next_char();
                Token::Delimiter(Delimiter::Backtick)
            }
            _ => Token::Unknown,
        }
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
