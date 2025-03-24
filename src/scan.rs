use std::str::CharIndices;

#[derive(PartialEq, Eq, Debug, Clone, Copy)]
pub enum StyleDelimiter {
    Strong,
    Strikethrough,
    Emphasis,
}

#[derive(PartialEq, Eq, Debug)]
pub enum ScanError {
    UnexpectedInput,
}

type ScanResult<T> = Result<T, ScanError>;
type CharPredicate = fn(char) -> bool;

const NEW_LINE: char = '\n';
const SPACE: char = ' ';
const BACKTICK: char = '`';
const ASTERISK: char = '*';
const TILDE: char = '~';
const UNDERSCORE: char = '_';
const HASH: char = '#';
const COLON: char = ':';
const HYPHEN: char = '-';
const BACKSLASH: char = '\\';
const EQUALS: char = '=';
const LEFT_SQUARE_BRACKET: char = '[';
const RIGHT_SQUARE_BRACKET: char = ']';

const EXCLUDED_FROM_TEXT_FRAGMENT: [char; 8] = [
    UNDERSCORE, BACKTICK, ASTERISK, TILDE, SPACE, NEW_LINE, HASH, BACKSLASH,
];

const EXCLUDED_FROM_RAW_FRAGMENT: [char; 3] = [BACKTICK, SPACE, NEW_LINE];

#[derive(Default, Copy, Clone)]
struct Position {
    char: Option<char>,
    index: usize,
    col: usize,
    row: usize,
}

pub struct Scanner<'a> {
    input: &'a str,
    chars: CharIndices<'a>,
    current: Position,
    next: Position,
}

impl<'a> Scanner<'a> {
    pub fn new(input: &'a str) -> Self {
        let mut scanner = Self {
            input,
            chars: input.char_indices(),
            current: Position::default(),
            next: Position::default(),
        };

        // Place the first char of the input into `next`
        // Then propigate to `current`
        scanner.read_next_char();
        scanner.read_next_char();

        scanner
    }

    //TODO: a lot of the 'on_' methods are only true in context...
    // ...figure out if that matters, maybe just call them after actual chars instead?

    pub fn on_space(&self) -> bool {
        self.current_char_equals(SPACE)
    }

    pub fn on_escaped(&self) -> bool {
        self.current_char_equals(BACKSLASH)
    }

    pub fn on_linebreak(&self) -> bool {
        self.current_char_equals(NEW_LINE)
    }

    pub fn on_container_header(&self) -> bool {
        self.current_char_equals(HASH) && self.next_char_equals(LEFT_SQUARE_BRACKET)
    }

    pub fn on_container_footer(&self) -> bool {
        self.current_char_equals(HASH) && self.next_char_equals(EQUALS)
    }

    pub fn on_metadata_header(&self) -> bool {
        self.is_on("#metadata")
    }

    pub fn on_hash(&self) -> bool {
        self.current_char_equals(HASH)
    }

    pub fn on_style_delimiter(&self) -> bool {
        self.current_char_in(&[ASTERISK, TILDE, UNDERSCORE])
    }

    pub fn on_inline_raw_delimiter(&self) -> bool {
        self.current_char_equals(BACKTICK)
    }

    pub fn on_char_usable_in_text(&self) -> bool {
        self.current
            .char
            .is_some_and(|c| !EXCLUDED_FROM_TEXT_FRAGMENT.contains(&c))
    }

    pub fn on_char_usable_in_identifier(&self) -> bool {
        self.current.char.is_some_and(char::is_alphanumeric)
    }

    pub fn on_char_usable_in_raw(&self) -> bool {
        self.current
            .char
            .is_some_and(|c| !EXCLUDED_FROM_RAW_FRAGMENT.contains(&c))
    }

    pub fn has_input(&mut self) -> bool {
        self.current.char.is_some()
    }

    pub fn eat_block_header(&mut self) -> ScanResult<&'a str> {
        self.eat_char(HASH)?;
        self.eat_while(char::is_alphanumeric)
    }

    pub fn eat_container_header(&mut self) -> ScanResult<&'a str> {
        self.eat_char(HASH)?;
        self.eat_char(LEFT_SQUARE_BRACKET)?;
        let name = self.eat_while(char::is_alphanumeric)?;
        self.eat_char(RIGHT_SQUARE_BRACKET)?;
        Ok(name)
    }

    pub fn eat_container_footer(&mut self) -> ScanResult<()> {
        self.eat_char(HASH)?;
        self.eat_char(EQUALS)?;
        self.skip_while(|c| c == EQUALS);
        Ok(())
    }

    pub fn eat_linebreak(&mut self) -> ScanResult<()> {
        self.eat_char(NEW_LINE)
    }

    pub fn eat_space(&mut self) -> ScanResult<()> {
        self.eat_char(SPACE)?;
        self.skip_while(|c| c == SPACE);
        Ok(())
    }

    pub fn eat_style_delimiter(&mut self) -> ScanResult<StyleDelimiter> {
        let style = match self.current.char {
            Some(TILDE) => StyleDelimiter::Strikethrough,
            Some(UNDERSCORE) => StyleDelimiter::Emphasis,
            Some(ASTERISK) => StyleDelimiter::Strong,
            _ => return Err(ScanError::UnexpectedInput),
        };
        self.read_next_char();
        Ok(style)
    }

    pub fn eat_inline_raw_delimiter(&mut self) -> ScanResult<()> {
        self.eat_char(BACKTICK)
    }

    pub fn eat_escaped(&mut self) -> ScanResult<&'a str> {
        self.eat_char(BACKSLASH)?;
        let escaped = self.current_char_as_str()?;
        self.read_next_char();
        Ok(escaped)
    }

    pub fn eat_optional_space(&mut self) {
        self.skip_while(|c| c == SPACE);
    }

    pub fn eat_optional_linebreak(&mut self) {
        if self.current_char_equals(NEW_LINE) {
            self.read_next_char();
        }
    }

    pub fn eat_optional_whitespace(&mut self) {
        self.skip_while(|c| c == SPACE || c == NEW_LINE)
    }

    pub fn eat_identifier(&mut self) -> ScanResult<&'a str> {
        self.eat_while(char::is_alphanumeric)
    }

    pub fn eat_meta_text(&mut self) -> ScanResult<&'a str> {
        self.eat_while(|c| c != '\n')
    }

    pub fn eat_colon(&mut self) -> ScanResult<()> {
        self.eat_char(COLON)
    }

    pub fn eat_text(&mut self) -> ScanResult<&'a str> {
        self.eat_while(|c| !EXCLUDED_FROM_TEXT_FRAGMENT.contains(&c))
    }

    pub fn eat_raw_fragment(&mut self) -> ScanResult<&'a str> {
        self.eat_while(|c| !EXCLUDED_FROM_RAW_FRAGMENT.contains(&c))
    }

    pub fn eat_raw_space(&mut self) -> ScanResult<&'a str> {
        self.eat_while(|c| c == SPACE)
    }

    fn current_char_equals(&self, c: char) -> bool {
        self.current.char == Some(c)
    }

    fn current_char_in(&self, chars: &[char]) -> bool {
        self.current.char.is_some_and(|c| chars.contains(&c))
    }

    fn current_char_as_str(&self) -> ScanResult<&'a str> {
        if self.current.char.is_some() {
            Ok(&self.input[self.current.index..self.next.index])
        } else {
            Err(ScanError::UnexpectedInput)
        }
    }

    fn next_char_equals(&self, c: char) -> bool {
        self.next.char == Some(c)
    }

    fn eat_char(&mut self, c: char) -> ScanResult<()> {
        if self.current_char_equals(c) {
            self.read_next_char();
            Ok(())
        } else {
            Err(ScanError::UnexpectedInput)
        }
    }

    fn eat_while(&mut self, predicate: CharPredicate) -> ScanResult<&'a str> {
        let i1 = self.current.index;
        self.skip_while(predicate);
        let i2 = self.current.index;
        let string = &self.input[i1..i2];

        if string.is_empty() {
            Err(ScanError::UnexpectedInput)
        } else {
            Ok(string)
        }
    }

    fn skip_while(&mut self, predicate: CharPredicate) {
        while self.current.char.is_some_and(predicate) {
            self.read_next_char();
        }
    }

    //TODO: This could replace the lookahead?
    fn is_on(&self, s: &str) -> bool {
        self.input[self.current.index..].starts_with(s)
    }

    fn read_next_char(&mut self) {
        self.current = self.next;
        let last_col = self.current.col;
        let last_row = self.current.row;

        if let Some((index, c)) = self.chars.next() {
            let (col, row) = if c == '\n' {
                (0, last_row + 1)
            } else {
                (last_col + 1, last_row)
            };

            self.next = Position {
                char: Some(c),
                index,
                col,
                row,
            };
        } else {
            self.next = Position {
                char: None,
                index: self.input.len(),
                col: last_col,
                row: last_row,
            };
        }

        if let Some(c) = self.current.char {
            print!("{}", c);
        }
    }
}
