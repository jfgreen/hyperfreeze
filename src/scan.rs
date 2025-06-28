use std::str::CharIndices;

pub type ScanResult<'a, T> = Result<T, ScannerPosition<'a>>;

#[derive(Debug)]
pub struct ScannerPosition<'a> {
    pub input: &'a str,
    pub column: u32,
    pub row: u32,
}

impl<'a> ScannerPosition<'a> {
    pub fn line(&self) -> Option<&'a str> {
        self.input.lines().nth(self.row as usize)
    }
}

#[derive(Debug, Clone)]
pub struct Scanner<'a> {
    input: &'a str,
    chars: CharIndices<'a>,
    current_char: Option<char>,
    current_index: usize,
    column: u32,
    row: u32,
}

impl<'a> Scanner<'a> {
    pub fn new(input: &'a str) -> Self {
        let mut scanner = Self {
            input,
            chars: input.char_indices(),
            current_char: None,
            current_index: 0,
            column: 0,
            row: 0,
        };

        // Place the first char of the input into `next`
        scanner.read_next_char();

        scanner
    }

    pub fn has_input(&self) -> bool {
        self.current_char.is_some()
    }

    pub fn position(&self) -> ScannerPosition<'a> {
        ScannerPosition {
            input: self.input,
            column: self.column,
            row: self.row,
        }
    }

    pub fn is_on(&self, predicate: impl Fn(char) -> bool) -> bool {
        self.current_char.is_some_and(&predicate)
    }

    pub fn is_on_str(&self, s: &str) -> bool {
        self.input[self.current_index..].starts_with(s)
    }

    pub fn is_on_char(&self, c: char) -> bool {
        self.current_char == Some(c)
    }

    pub fn is_on_one_of(&self, chars: &[char]) -> bool {
        self.current_char.is_some_and(|c| chars.contains(&c))
    }

    pub fn peek(&self) -> Scanner<'a> {
        self.clone()
    }

    pub fn advance_to(&mut self, other: &Scanner<'a>) {
        *self = other.clone()
    }

    pub fn skip_char(&mut self) {
        self.read_next_char();
    }

    pub fn skip_chars(&mut self, count: usize) {
        for _ in 0..count {
            self.read_next_char();
        }
    }

    pub fn skip_while_on_char(&mut self, c1: char) {
        self.skip_while(|c2| c1 == c2)
    }

    pub fn skip_while_on_any(&mut self, chars: &[char]) {
        self.skip_while(|c| chars.contains(&c))
    }

    fn skip_while(&mut self, predicate: impl Fn(char) -> bool) {
        while self.current_char.is_some_and(&predicate) {
            self.read_next_char();
        }
    }

    pub fn expect_char(&mut self, c: char) -> ScanResult<()> {
        if self.current_char == Some(c) {
            self.read_next_char();
            Ok(())
        } else {
            Err(self.position())
        }
    }

    pub fn eat_char(&mut self) -> ScanResult<char> {
        match self.current_char {
            Some(c) => {
                self.read_next_char();
                Ok(c)
            }
            None => Err(self.position()),
        }
    }

    pub fn eat_while(&mut self, predicate: impl Fn(char) -> bool) -> ScanResult<&'a str> {
        let position = self.position();

        let i1 = self.current_index;

        self.skip_while(&predicate);

        let i2 = self.current_index;
        let string = &self.input[i1..i2];

        if string.is_empty() {
            Err(position)
        } else {
            Ok(string)
        }
    }

    pub fn eat_while_char(&mut self, c1: char) -> ScanResult<&'a str> {
        self.eat_while(|c| c == c1)
    }

    pub fn eat_until_one_of(&mut self, chars: &[char]) -> ScanResult<&'a str> {
        self.eat_while(|c| !chars.contains(&c))
    }

    fn read_next_char(&mut self) {
        if let Some((index, c)) = self.chars.next() {
            match self.current_char {
                Some('\n') => {
                    self.column = 0;
                    self.row += 1;
                }
                Some(_) => {
                    self.column += 1;
                }
                None => {
                    self.column = 0;
                    self.row = 0;
                }
            }

            self.current_char = Some(c);
            self.current_index = index;
        } else {
            self.current_char = None;
            self.current_index = self.input.len();
        }
    }
}
