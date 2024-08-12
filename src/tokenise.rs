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
    Whitespace,
    Delimiter(Delimit),
    Eof,
}

//TODO: Delimiters...
// Does the tokeniser actually know its a delimiter... what if
// it is in raw text. Do we return "MaybeDelimit",
// or would that not work?w

// Options:
// Return a MaybeDelimit - meh
// Put statefullness into the tokeniser - meh
// Parser tells tokeniser the kind of tokenisation to do...?
// an "expect" based api?

#[derive(PartialEq, Eq, Debug, Clone, Copy)]
pub enum Delimit {
    Bold,
    Emphasis,
    Strikethrough,
    Raw,
}

pub struct Tokeniser<'a> {
    input: &'a str,
    chars: CharIndices<'a>,
    //TODO: Can we remove Option for current_char?
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
    //TODO: Rename to advance?
    pub fn next(&mut self) {
        self.current_token = match self.current_char {
            Some('\n') => self.handle_new_line(),
            Some(DELIM_BOLD) => self.handle_delimiter(Delimit::Bold),
            Some(DELIM_EMPH) => self.handle_delimiter(Delimit::Emphasis),
            Some(DELIM_STRIKE) => self.handle_delimiter(Delimit::Strikethrough),
            Some(DELIM_RAW) => self.handle_delimiter(Delimit::Raw),
            Some(c) if c.is_whitespace() => self.handle_whitespace(),
            //TODO: Do we really want any unicode char in a text?
            Some(_) => self.handle_text(),
            _ => Token::Eof,
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

        // Advance the tokeniser once to place the first char of the input into `current_char`
        tokeniser.advance();

        // Then parse the first token
        tokeniser.next();

        tokeniser
    }

    fn handle_new_line(&mut self) -> Token<'a> {
        self.advance();

        // New lines are usually treated as whitespace. However, if there is
        // more than one in a row, they are all treated as a single line break
        if self.current_char == Some('\n') {
            while self.current_char == Some('\n') {
                self.advance();
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

    fn handle_delimiter(&mut self, kind: Delimit) -> Token<'a> {
        self.advance();
        Token::Delimiter(kind)
    }

    fn handle_text(&mut self) -> Token<'a> {
        let text = self.eat_text();
        Token::Text(text)
    }

    fn eat_whitespace(&mut self) {
        self.advance_while_current(char::is_whitespace)
    }

    fn eat_text(&mut self) -> &'a str {
        let i1 = self.current_index;
        self.advance_while_current(char_usable_in_text);
        let i2 = self.current_index;
        &self.input[i1..i2]
    }

    fn advance(&mut self) {
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
            self.advance();
        }
    }
}
