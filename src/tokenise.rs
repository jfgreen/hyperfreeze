use std::iter::Peekable;
use std::str::CharIndices;

const NODE_START: char = '[';
const NODE_END: char = ']';

//TODO: Have an enum for accepted Node keywords?

#[derive(PartialEq, Eq, Debug)]
enum Token<'a> {
    NodeStart(&'a str),
    NodeEnd,
    Text(&'a str),
    Whitespace,
}

//TODO: Try a simple implementation without peekable, do the peek ourself
struct Tokeniser<'a> {
    input: &'a str,
    chars: Peekable<CharIndices<'a>>,
    current_char: Option<char>,
    current_index: usize,
    next_char: Option<char>,
    next_index: usize,
}

impl<'a> Iterator for Tokeniser<'a> {
    type Item = Token<'a>;

    fn next(&mut self) -> Option<Token<'a>> {
        self.advance();
        match self.current_char {
            Some(NODE_START) => {
                let node_name = self.eat_node_name();
                Some(Token::NodeStart(node_name))
            }
            Some(NODE_END) => Some(Token::NodeEnd),
            Some(c) if c.is_whitespace() => {
                self.eat_whitespace();
                Some(Token::Whitespace)
            }
            Some(_) => {
                let text = self.eat_text();
                Some(Token::Text(text))
            }
            None => None,
        }
    }
}
impl<'a> Tokeniser<'a> {
    fn new(input: &'a str) -> Self {
        Self {
            input,
            chars: input.char_indices().peekable(),
            current_char: None,
            current_index: 0,
            next_char: None,
            next_index: 0,
        }
    }

    // TODO: Maybe want to be less restrictive about what can be in a node name?
    fn eat_node_name(&mut self) -> &'a str {
        if !self.next_char.is_some_and(|c| c.is_ascii_alphabetic()) {
            return "";
        }
        self.advance();

        let i1 = self.current_index;
        self.advance_while(|c| c.is_ascii_alphabetic());
        let i2 = self.next_index;

        &self.input[i1..i2]
    }

    fn eat_whitespace(&mut self) {
        self.advance_while(|c| c.is_whitespace());
    }

    fn eat_text(&mut self) -> &'a str {
        let i1 = self.current_index;
        self.advance_while(|c| c != NODE_START && c != NODE_END && !c.is_whitespace());
        let i2 = self.next_index;
        &self.input[i1..i2]
    }

    fn advance(&mut self) {
        let next = self.chars.next();
        let peek = self.chars.peek();
        self.current_char = next.map(|(_, c)| c);
        self.current_index = next.map(|(i, _)| i).unwrap_or(self.input.len());
        self.next_char = peek.map(|(_, c)| *c);
        self.next_index = peek.map(|(i, _)| *i).unwrap_or(self.input.len());
    }

    fn advance_while(&mut self, predicate: fn(char) -> bool) {
        while self.next_char.is_some_and(predicate) {
            self.advance()
        }
    }
}

//TODO: Should we eat the whitespace after a [token" "<- ?

#[cfg(test)]
mod test {
    use super::*;

    fn tokenise(input: &str) -> Vec<Token> {
        let tokeniser = Tokeniser::new(input);
        tokeniser.into_iter().collect()
    }

    #[test]
    fn simple_document() {
        let input = "[title Enterprise Software Design For Cats]";

        let expected = vec![
            Token::NodeStart("title"),
            Token::Whitespace,
            Token::Text("Enterprise"),
            Token::Whitespace,
            Token::Text("Software"),
            Token::Whitespace,
            Token::Text("Design"),
            Token::Whitespace,
            Token::Text("For"),
            Token::Whitespace,
            Token::Text("Cats"),
            Token::NodeEnd,
        ];

        let actual = tokenise(input);

        assert_eq!(actual, expected);
    }

    #[test]
    fn double_space() {
        let input = "Feline  Engineering";

        let expected = vec![
            Token::Text("Feline"),
            Token::Whitespace,
            Token::Text("Engineering"),
        ];

        let tokeniser = Tokeniser::new(input);
        let actual: Vec<Token<'_>> = tokeniser.into_iter().collect();

        assert_eq!(actual, expected);
    }

    #[test]
    fn nested_nodes_without_spaces() {
        let input = "[bold[underline Cats]]";

        let expected = vec![
            Token::NodeStart("bold"),
            Token::NodeStart("underline"),
            Token::Whitespace,
            Token::Text("Cats"),
            Token::NodeEnd,
            Token::NodeEnd,
        ];

        let actual = tokenise(input);

        assert_eq!(actual, expected);
    }

    #[test]
    fn nodes_missing_name() {
        let input = "[ Fantastic";

        let expected = vec![
            Token::NodeStart(""),
            Token::Whitespace,
            Token::Text("Fantastic"),
        ];

        let actual = tokenise(input);

        assert_eq!(actual, expected);
    }
    #[test]
    fn nodes_missing_name_without_whitespace() {
        let input = "[[[";

        let expected = vec![
            Token::NodeStart(""),
            Token::NodeStart(""),
            Token::NodeStart(""),
        ];

        let tokeniser = Tokeniser::new(input);
        let actual: Vec<Token<'_>> = tokeniser.into_iter().collect();

        assert_eq!(actual, expected);
    }
}
