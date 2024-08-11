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
}

//TODO: Delimiters...
// Does the tokeniser actually know its a delimiter... what if
// it is in raw text. Do we return "MaybeDelimit",
// or would that not work?
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
    last_char: Option<char>,
    current_char: Option<char>,
    current_index: usize,
    next_char: Option<char>,
    next_index: usize,
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

impl<'a> Iterator for Tokeniser<'a> {
    type Item = Token<'a>;

    fn next(&mut self) -> Option<Token<'a>> {
        match self.current_char {
            Some('\n') => Some(self.handle_new_line()),
            Some(DELIM_BOLD) => Some(self.handle_delimiter(Delimit::Bold)),
            Some(DELIM_EMPH) => Some(self.handle_delimiter(Delimit::Emphasis)),
            Some(DELIM_STRIKE) => Some(self.handle_delimiter(Delimit::Strikethrough)),
            Some(DELIM_RAW) => Some(self.handle_delimiter(Delimit::Raw)),
            Some(c) if c.is_whitespace() => Some(self.handle_whitespace()),
            //TODO: Do we really want any unicode char in a text?
            Some(_) => Some(self.handle_text()),
            _ => None,
        }
    }
}

impl<'a> Tokeniser<'a> {
    pub fn new(input: &'a str) -> Self {
        let mut tokeniser = Self {
            input,
            chars: input.char_indices(),
            last_char: None,
            current_char: None,
            current_index: 0,
            next_char: None,
            next_index: 0,
        };

        // Advance the tokeniser once to establish peek ahead, placing the start
        // of input string into 'next_char', assuming it has at least once char
        tokeniser.advance();

        // Then again, to place the first char of the input into `current_char`
        tokeniser.advance();

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

    //TODO: Do we actually need the lookahead?
    fn advance(&mut self) {
        self.last_char = self.current_char;
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

    fn advance_while_current(&mut self, predicate: fn(char) -> bool) {
        while self.current_char.is_some_and(predicate) {
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
    fn three_new_lines_becomes_linebreak() {
        let input = "Cats\n\n\nwhiskers";

        let expected = vec![
            Token::Text("Cats"),
            Token::Linebreak,
            Token::Text("whiskers"),
        ];

        let actual = tokenise(input);
        assert_eq!(actual, expected);
    }

    #[test]
    fn bold_text() {
        let input = "Cats are *really* cute";

        let expected = vec![
            Token::Text("Cats"),
            Token::Whitespace,
            Token::Text("are"),
            Token::Whitespace,
            Token::Delimiter(Delimit::Bold),
            Token::Text("really"),
            Token::Delimiter(Delimit::Bold),
            Token::Whitespace,
            Token::Text("cute"),
        ];

        let actual = tokenise(input);
        assert_eq!(actual, expected);
    }

    #[test]
    fn emphasis_text() {
        let input = "Cats are _super_ smart";

        let expected = vec![
            Token::Text("Cats"),
            Token::Whitespace,
            Token::Text("are"),
            Token::Whitespace,
            Token::Delimiter(Delimit::Emphasis),
            Token::Text("super"),
            Token::Delimiter(Delimit::Emphasis),
            Token::Whitespace,
            Token::Text("smart"),
        ];

        let actual = tokenise(input);
        assert_eq!(actual, expected);
    }

    #[test]
    fn strike_world() {
        let input = "Learn ~forbiden~ cat secrets";

        let expected = vec![
            Token::Text("Learn"),
            Token::Whitespace,
            Token::Delimiter(Delimit::Strikethrough),
            Token::Text("forbiden"),
            Token::Delimiter(Delimit::Strikethrough),
            Token::Whitespace,
            Token::Text("cat"),
            Token::Whitespace,
            Token::Text("secrets"),
        ];

        let actual = tokenise(input);
        assert_eq!(actual, expected);
    }

    #[test]
    fn raw_text_with_punctuation() {
        let input = "Cat `technology`!";

        let expected = vec![
            Token::Text("Cat"),
            Token::Whitespace,
            Token::Delimiter(Delimit::Raw),
            Token::Text("technology"),
            Token::Delimiter(Delimit::Raw),
            Token::Text("!"),
        ];

        let actual = tokenise(input);
        assert_eq!(actual, expected);
    }
}
