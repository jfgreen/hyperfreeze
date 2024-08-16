use std::str::CharIndices;

const DELIM_BOLD: char = '*';
const DELIM_EMPH: char = '_';
const DELIM_STRIKE: char = '~';
const DELIM_RAW: char = '`';

//TODO: Keep track of what line, row a token is on
//TODO: See if we actually need the lookahead? Escaping?

#[derive(PartialEq, Eq, Debug)]
pub enum Token<'a> {
    Linebreak,
    Text(&'a str),
    RawText(&'a str),
    Whitespace,
    FormatDelimiter(Format),
    Eof,
}

#[derive(PartialEq, Eq, Debug, Clone, Copy)]
pub enum Format {
    Bold,
    Emphasis,
    Strikethrough,
}

pub struct Tokeniser<'a> {
    input: &'a str,
    chars: CharIndices<'a>,
    current_char: Option<char>,
    current_index: usize,
    pub current_token: Token<'a>,
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

impl<'a> Tokeniser<'a> {
    pub fn advance(&mut self) {
        self.current_token = match self.current_char {
            Some('\n') => self.handle_new_line(),
            Some(DELIM_BOLD) => self.handle_delimiter(Format::Bold),
            Some(DELIM_EMPH) => self.handle_delimiter(Format::Emphasis),
            Some(DELIM_STRIKE) => self.handle_delimiter(Format::Strikethrough),
            Some(DELIM_RAW) => self.handle_raw_text(),
            Some(c) if c.is_whitespace() => self.handle_whitespace(),
            Some(_) => self.handle_text(),
            None => Token::Eof,
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

    fn handle_new_line(&mut self) -> Token<'a> {
        self.read_next_char();

        // New lines are usually treated as whitespace. However, if there is
        // more than one in a row, they are all treated as a single line break
        if self.current_char == Some('\n') {
            while self.current_char == Some('\n') {
                self.read_next_char();
            }
            Token::Linebreak
        } else {
            Token::Whitespace
        }
    }

    fn handle_whitespace(&mut self) -> Token<'a> {
        self.eat_whitespace();
        Token::Whitespace
    }

    fn handle_delimiter(&mut self, kind: Format) -> Token<'a> {
        self.read_next_char();
        Token::FormatDelimiter(kind)
    }

    fn handle_text(&mut self) -> Token<'a> {
        let i1 = self.current_index;
        self.advance_while_current(char_usable_in_text);
        let i2 = self.current_index;
        let text = &self.input[i1..i2];
        Token::Text(text)
    }

    fn handle_raw_text(&mut self) -> Token<'a> {
        self.read_next_char(); // over opening delimiter
        let i1 = self.current_index;
        self.advance_while_current(|c| c != DELIM_RAW);
        let i2 = self.current_index;
        let text = &self.input[i1..i2];
        self.read_next_char(); // over closing delimiter
        Token::RawText(text)
    }

    fn eat_whitespace(&mut self) {
        self.advance_while_current(char::is_whitespace)
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
