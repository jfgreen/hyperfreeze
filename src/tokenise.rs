use std::str::CharIndices;

const DELIM_BOLD: char = '*';
const DELIM_EMPH: char = '_';
const DELIM_STRIKE: char = '-';
const DELIM_RAW: char = '`';

//TODO: Keep track of what line, row a token is on

#[derive(PartialEq, Eq, Debug)]
pub enum Token<'a> {
    Linebreak,
    Word(&'a str),
    Whitespace,
    Delimiter(Delimit),
}

#[derive(PartialEq, Eq, Debug)]
pub enum Delimit {
    Bold,
    Emphasis,
    Strikethrough,
    Raw,
}

const BOLD_DELIMITER: Token = Token::Delimiter(Delimit::Bold);

pub struct Tokeniser<'a> {
    input: &'a str,
    chars: CharIndices<'a>,
    current_char: Option<char>,
    current_index: usize,
    next_char: Option<char>,
    next_index: usize,
}

fn char_usable_in_word(c: char) -> bool {
    // TODO: Is there a more efficent way to do this?
    // Maybe by treating char as an int
    !(c == DELIM_BOLD
        || c == DELIM_EMPH
        || c == DELIM_STRIKE
        || c == DELIM_RAW
        || c.is_whitespace())
}

impl<'a> Iterator for Tokeniser<'a> {
    type Item = Token<'a>;

    fn next(&mut self) -> Option<Token<'a>> {
        // The first time this function is called, `next_char` will hold the
        // start of the input string. Therefore the initial call to advance will
        // place the first char of the input into `current_char` (assuming that
        // the input is a non empty string).
        self.advance();

        match self.current_char {
            Some('\n') => Some(self.handle_new_line()),
            Some(DELIM_BOLD) => Some(Token::Delimiter(Delimit::Bold)),
            Some(DELIM_EMPH) => Some(Token::Delimiter(Delimit::Emphasis)),
            Some(DELIM_STRIKE) => Some(Token::Delimiter(Delimit::Strikethrough)),
            Some(DELIM_RAW) => Some(Token::Delimiter(Delimit::Raw)),
            Some(c) if c.is_whitespace() => Some(self.handle_whitespace()),
            //TODO: Do we really want any unicode char in a word?
            Some(_) => Some(self.handle_word()),
            _ => None,
        }
    }
}

impl<'a> Tokeniser<'a> {
    pub fn new(input: &'a str) -> Self {
        let mut tokeniser = Self {
            input,
            chars: input.char_indices(),
            current_char: None,
            current_index: 0,
            next_char: None,
            next_index: 0,
        };

        // Advance the tokeniser once to establish peek ahead, placing the start
        // of input string into 'next_char', assuming it has at least once char
        tokeniser.advance();

        tokeniser
    }

    fn handle_new_line(&mut self) -> Token<'a> {
        // New lines are usually treated as whitespace. However, if there is
        // more than one in a row, they are all treated as a single line break
        if self.next_char == Some('\n') {
            while self.next_char == Some('\n') {
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

    fn handle_word(&mut self) -> Token<'a> {
        let word = self.eat_word();
        Token::Word(word)
    }

    fn eat_whitespace(&mut self) {
        self.advance_while_next(char::is_whitespace)
    }

    fn eat_word(&mut self) -> &'a str {
        let i1 = self.current_index;
        self.advance_while_next(char_usable_in_word);
        let i2 = self.next_index;
        &self.input[i1..i2]
    }

    fn advance(&mut self) {
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

    fn advance_while_next(&mut self, predicate: fn(char) -> bool) {
        while self.next_char.is_some_and(predicate) {
            self.advance();
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;

    fn tokenise(input: &str) -> Vec<Token> {
        let tokeniser = Tokeniser::new(input);
        tokeniser.into_iter().collect()
    }

    #[test]
    fn simple_sentence() {
        let input = "The nice little cat pounced over the silly fox";

        let expected = vec![
            Token::Word("The"),
            Token::Whitespace,
            Token::Word("nice"),
            Token::Whitespace,
            Token::Word("little"),
            Token::Whitespace,
            Token::Word("cat"),
            Token::Whitespace,
            Token::Word("pounced"),
            Token::Whitespace,
            Token::Word("over"),
            Token::Whitespace,
            Token::Word("the"),
            Token::Whitespace,
            Token::Word("silly"),
            Token::Whitespace,
            Token::Word("fox"),
        ];

        let actual = tokenise(input);
        assert_eq!(actual, expected);
    }

    #[test]
    fn double_space() {
        let input = "Nice  kitty!";

        let expected = vec![
            Token::Word("Nice"),
            Token::Whitespace,
            Token::Word("kitty!"),
        ];

        let actual = tokenise(input);
        assert_eq!(actual, expected);
    }

    #[test]
    fn new_line_becomes_whitespace() {
        let input = "Cats\nwhiskers";

        let expected = vec![
            Token::Word("Cats"),
            Token::Whitespace,
            Token::Word("whiskers"),
        ];

        let actual = tokenise(input);
        assert_eq!(actual, expected);
    }

    #[test]
    fn two_new_lines_becomes_linebreak() {
        let input = "Cats\n\nwhiskers";

        let expected = vec![
            Token::Word("Cats"),
            Token::Linebreak,
            Token::Word("whiskers"),
        ];

        let actual = tokenise(input);
        assert_eq!(actual, expected);
    }

    #[test]
    fn three_new_lines_becomes_linebreak() {
        let input = "Cats\n\n\nwhiskers";

        let expected = vec![
            Token::Word("Cats"),
            Token::Linebreak,
            Token::Word("whiskers"),
        ];

        let actual = tokenise(input);
        assert_eq!(actual, expected);
    }

    #[test]
    fn bold_word() {
        let input = "Cats are *really* cute";

        let expected = vec![
            Token::Word("Cats"),
            Token::Whitespace,
            Token::Word("are"),
            Token::Whitespace,
            Token::Delimiter(Delimit::Bold),
            Token::Word("really"),
            Token::Delimiter(Delimit::Bold),
            Token::Whitespace,
            Token::Word("cute"),
        ];

        let actual = tokenise(input);
        assert_eq!(actual, expected);
    }

    #[test]
    fn emphasis_word() {
        let input = "Cats are _super_ smart";

        let expected = vec![
            Token::Word("Cats"),
            Token::Whitespace,
            Token::Word("are"),
            Token::Whitespace,
            Token::Delimiter(Delimit::Emphasis),
            Token::Word("super"),
            Token::Delimiter(Delimit::Emphasis),
            Token::Whitespace,
            Token::Word("smart"),
        ];

        let actual = tokenise(input);
        assert_eq!(actual, expected);
    }

    #[test]
    fn strike_world() {
        let input = "Learn -forbiden- cat secrets";

        let expected = vec![
            Token::Word("Learn"),
            Token::Whitespace,
            Token::Delimiter(Delimit::Strikethrough),
            Token::Word("forbiden"),
            Token::Delimiter(Delimit::Strikethrough),
            Token::Whitespace,
            Token::Word("cat"),
            Token::Whitespace,
            Token::Word("secrets"),
        ];

        let actual = tokenise(input);
        assert_eq!(actual, expected);
    }

    #[test]
    fn raw_word() {
        let input = "Cat `technology`!";

        let expected = vec![
            Token::Word("Cat"),
            Token::Whitespace,
            Token::Delimiter(Delimit::Raw),
            Token::Word("technology"),
            Token::Delimiter(Delimit::Raw),
            Token::Word("!"),
        ];

        let actual = tokenise(input);
        assert_eq!(actual, expected);
    }

    //TODO: References
}
