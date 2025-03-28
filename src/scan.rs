use std::str::CharIndices;

#[derive(PartialEq, Eq, Debug)]
pub enum ScanError {
    UnexpectedInput,
}

type ScanResult<T> = Result<T, ScanError>;

pub struct Scanner<'a> {
    input: &'a str,
    chars: CharIndices<'a>,
    current_char: Option<char>,
    current_index: usize,
}

impl<'a> Scanner<'a> {
    pub fn new(input: &'a str) -> Self {
        let mut scanner = Self {
            input,
            chars: input.char_indices(),
            current_char: None,
            current_index: 0,
        };

        // Place the first char of the input into `next`
        scanner.read_next_char();

        scanner
    }

    pub fn has_input(&mut self) -> bool {
        self.current_char.is_some()
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

    pub fn is_on_any(&mut self, chars: &[char]) -> bool {
        self.current_char.is_some_and(|c| chars.contains(&c))
    }

    pub fn skip_char(&mut self) {
        self.read_next_char();
    }

    pub fn skip_if_on_char(&mut self, c: char) {
        if self.current_char == Some(c) {
            self.read_next_char();
        }
    }

    pub fn skip_while_on_char(&mut self, c1: char) {
        self.skip_while(|c2| c1 == c2)
    }

    pub fn skip_while_on_any(&mut self, chars: &[char]) {
        self.skip_while(|c| chars.contains(&c))
    }

    pub fn skip_while(&mut self, predicate: impl Fn(char) -> bool) {
        while self.current_char.is_some_and(&predicate) {
            self.read_next_char();
        }
    }

    pub fn expect_char(&mut self, c: char) -> ScanResult<()> {
        if self.current_char == Some(c) {
            self.read_next_char();
            Ok(())
        } else {
            Err(ScanError::UnexpectedInput)
        }
    }

    pub fn expect_str(&mut self, s: &str) -> ScanResult<()> {
        if self.is_on_str(s) {
            for _ in 0..s.chars().count() {
                // TODO: Can we jump to char instead?
                // especially if we dont have a look ahead
                self.read_next_char();
            }
            Ok(())
        } else {
            Err(ScanError::UnexpectedInput)
        }
    }

    pub fn eat_char(&mut self) -> ScanResult<char> {
        match self.current_char {
            Some(c) => {
                self.read_next_char();
                Ok(c)
            }
            None => Err(ScanError::UnexpectedInput),
        }
    }

    pub fn eat_while(&mut self, predicate: impl Fn(char) -> bool) -> ScanResult<&'a str> {
        let i1 = self.current_index;

        self.skip_while(&predicate);

        let i2 = self.current_index;
        let string = &self.input[i1..i2];

        if string.is_empty() {
            Err(ScanError::UnexpectedInput)
        } else {
            Ok(string)
        }
    }
    pub fn eat_while_char(&mut self, c1: char) -> ScanResult<&'a str> {
        self.eat_while(|c| c == c1)
    }

    pub fn eat_until_char(&mut self, c1: char) -> ScanResult<&'a str> {
        self.eat_while(|c| c != c1)
    }

    fn read_next_char(&mut self) {
        if let Some((index, c)) = self.chars.next() {
            self.current_char = Some(c);
            self.current_index = index;
        } else {
            self.current_char = None;
            self.current_index = self.input.len();
        }

        if let Some(c) = self.current_char {
            print!("{}", c);
        }
    }
}
