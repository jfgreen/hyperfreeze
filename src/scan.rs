use std::{iter::Peekable, str::CharIndices};

const NEW_LINE: char = '\n';
const SPACE: char = ' ';

#[derive(Clone, Copy, Debug)]
pub struct Position {
    pub column: u32,
    pub row: u32,
}

#[derive(Debug, Clone)]
pub struct Scanner<'a> {
    input: &'a str,
    // TODO: peekable iter  would mean we dont have to maintain index also
    // It would also make it easier to jump to matching expressions later on
    // as we could clone the char indicies and use a consistent index rather
    // than having it start from zero each time
    chars: CharIndices<'a>,
    index: usize,
    column: u32,
    row: u32,
}

impl<'a> Scanner<'a> {
    pub fn new(input: &'a str) -> Self {
        let mut char_iter = input.char_indices();
        char_iter.next();

        Self {
            input,
            chars: char_iter,
            index: 0,
            column: 0,
            row: 0,
        }
    }

    pub fn has_input(&self) -> bool {
        self.index < self.input.len()
    }

    pub fn position(&self) -> Position {
        Position {
            column: self.column,
            row: self.row,
        }
    }

    pub fn is_on(&self, predicate: impl Fn(char) -> bool) -> bool {
        self.input[self.index..].starts_with(predicate)
    }

    pub fn is_on_str(&self, s: &str) -> bool {
        self.input[self.index..].starts_with(s)
    }

    pub fn is_on_char(&self, c: char) -> bool {
        self.input[self.index..].starts_with(c)
    }

    pub fn is_on_one_of(&self, chars: &[char]) -> bool {
        self.input[self.index..].starts_with(chars)
    }

    pub fn is_on_empty_line(&self) -> bool {
        // TODO: if we could have an internal version of
        // this that returns the index of the new line
        // then we could be a lot more optimised
        self.input[self.index..]
            .trim_start_matches(SPACE)
            .starts_with(NEW_LINE)
    }

    pub fn skip_char(&mut self) {
        self.read_next_char();
    }

    pub fn skip_chars(&mut self, count: usize) {
        for _ in 0..count {
            self.read_next_char();
        }
    }

    pub fn skip_while_on(&mut self, c: char) -> usize {
        let mut i = 0;
        while self.input[self.index..].starts_with(c) {
            self.read_next_char();
            i += 1;
        }
        i
    }

    pub fn skip_while_on_whitespace(&mut self) -> usize {
        let mut i = 0;
        while self.input[self.index..]
            .chars()
            .next()
            .is_some_and(|c| c == SPACE || c == NEW_LINE)
        {
            self.read_next_char();
            i += 1;
        }
        i
    }

    pub fn skip_while_on_empty_line(&mut self) {
        // TODO: This is not that efficient...
        // once we have put in the work to look ahead,
        // can we use this to skip to new line
        while self.is_on_empty_line() {
            while self.is_on_one_of(&[SPACE, NEW_LINE]) {
                self.read_next_char();
            }
        }
    }

    pub fn eat_char(&mut self) -> &'a str {
        let i1 = self.index;
        self.read_next_char();
        let i2 = self.index;
        &self.input[i1..i2]
    }

    pub fn eat_while(&mut self, predicate: impl Fn(char) -> bool) -> &'a str {
        let i1 = self.index;

        while self.input[self.index..].starts_with(&predicate) {
            self.read_next_char();
        }

        let i2 = self.index;
        &self.input[i1..i2]
    }

    pub fn eat_until_line_starting_with(&mut self, prefix: &str) -> &'a str {
        let i1 = self.index;

        while !(self.column == 0 && self.input[self.index..].starts_with(prefix)) {
            self.read_next_char();
        }

        let i2 = self.index;
        &self.input[i1..i2]
    }

    fn read_next_char(&mut self) {
        if let Some((index, _)) = self.chars.next() {
            if self.is_on_char('\n') {
                self.column = 0;
                self.row += 1;
            } else {
                self.column += 1;
            }

            self.index = index;
        } else {
            self.index = self.input.len();
        }
    }

    // TODO: Rather than returning a tuple of two, try and instead
    // return a single enum?
    // TODO: This (and advance_to) indicate scanning should just be moved
    // into the lexer
    // TODO: Evolve from this into propper matching
    // that returns a new position at the end of match
    // We could do this by something like
    // [Space, Text(t), _, _] if t.is_alphanumeric => {
    // scanner.advance_to(t.end())
    pub fn peek(&self) -> (PeekItem<'a>, PeekItem<'a>) {
        //TODO: This is not at all efficent,
        // If this approach works out then we should compute a rolling window
        // up front

        let remaining = &self.input[self.index..];
        let mut iter = remaining.char_indices().peekable();

        let p1 = peek(&mut iter, remaining);
        let p2 = peek(&mut iter, remaining);

        (p1, p2)
    }

    // fn advance_to(&mut self) {}
}

#[derive(Debug, PartialEq)]
pub enum PeekItem<'a> {
    // Whitespace(SpaceInfo),
    // TODO: Maybe instead of the full str we just give a few chars
    // easier to match on prefix?
    // try matching the start of text with something like:
    // [b'a', b'b', ..]
    Space,
    Singlebreak,
    Multibreak,
    Text(&'a str),
    End,
}

//TODO: Maybe grouping Space and Singlebreak into a single enum
// and having multibreak be seperate is the way forward?
// #[derive(Debug, PartialEq)]
// pub enum SpaceInfo {
//     Space,
//     Singlebreak,
//     Multibreak,
// }

fn peek<'a>(iter: &mut Peekable<CharIndices>, input: &'a str) -> PeekItem<'a> {
    match iter.peek() {
        Some((_, SPACE | NEW_LINE)) => {
            let mut new_line_count = 0;
            loop {
                match iter.peek() {
                    Some((_, SPACE)) => {}
                    Some((_, NEW_LINE)) => {
                        new_line_count += 1;
                    }
                    _ => break,
                }
                iter.next();
            }

            match new_line_count {
                0 => PeekItem::Space,
                1 => PeekItem::Singlebreak,
                _ => PeekItem::Multibreak,
            }
        }
        Some(&(i1, _)) => {
            while iter
                .next_if(|&(_, c)| c != SPACE && c != NEW_LINE)
                .is_some()
            {}
            let i2 = iter.peek().map_or(input.len(), |&(i, _)| i);
            PeekItem::Text(&input[i1..i2])
        }
        None => PeekItem::End,
    }
}
