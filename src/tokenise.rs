use std::str::CharIndices;

const NODE_START: char = '[';
const NODE_END: char = ']';

trait CharExt {
    fn can_be_used_in_text(&self) -> bool;
}

impl CharExt for char {
    fn can_be_used_in_text(&self) -> bool {
        !(*self == NODE_START || *self == NODE_END || self.is_whitespace())
    }
}

#[derive(PartialEq, Eq, Debug)]
enum NodeType {
    Bold,
    Underline,
    Title,
    Unknown,
}

#[derive(PartialEq, Eq, Debug)]
enum Token<'a> {
    NodeStart(NodeType),
    NodeEnd,
    Linebreak,
    Text(&'a str),
    Whitespace,
}

struct Tokeniser<'a> {
    input: &'a str,
    chars: CharIndices<'a>,
    current_char: Option<char>,
    current_index: usize,
    next_char: Option<char>,
    next_index: usize,
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
            Some(NODE_START) => Some(self.handle_node_start()),
            Some(NODE_END) => Some(Token::NodeEnd),
            Some('\n') => Some(self.handle_new_line()),
            Some(c) if c.is_whitespace() => Some(self.handle_whitespace()),
            Some(_) => Some(self.handle_text()),
            None => None,
        }
    }
}

impl<'a> Tokeniser<'a> {
    fn new(input: &'a str) -> Self {
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

    fn handle_node_start(&mut self) -> Token<'a> {
        let node_name = self.eat_node_name();
        self.eat_following_whitespace();

        // Note that we fall back to an 'unknown' node type because in practice
        // this is simpler than the alterantive of returning an error. It also
        // makes it possible for the tokeniser to implement the Iterator trait.

        let node_type = match node_name {
            "bold" => NodeType::Bold,
            "underline" => NodeType::Underline,
            "title" => NodeType::Title,
            _ => NodeType::Unknown,
        };
        Token::NodeStart(node_type)
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
        self.eat_following_whitespace();
        Token::Whitespace
    }

    fn handle_text(&mut self) -> Token<'a> {
        let text = self.eat_text();
        Token::Text(text)
    }

    fn eat_node_name(&mut self) -> &'a str {
        // Only continue if the '[' is followed by usable characters
        if !self.next_char_is_text() {
            return "";
        }

        // Otherwise, eat the characters comprising the node name
        self.advance();
        self.eat_text()
    }

    fn eat_following_whitespace(&mut self) {
        self.advance_while_next_is(|c| c.is_whitespace());
    }

    fn eat_text(&mut self) -> &'a str {
        let i1 = self.current_index;
        self.advance_while_next_is(|c| c.can_be_used_in_text());
        let i2 = self.next_index;
        &self.input[i1..i2]
    }

    fn advance(&mut self) {
        self.current_char = self.next_char;
        self.current_index = self.next_index;

        if let Some((i, c)) = self.chars.next() {
            self.next_char = Some(c);
            self.next_index = i
        } else {
            self.next_char = None;
            self.next_index = self.input.len()
        }
    }

    fn next_char_is_text(&mut self) -> bool {
        self.next_char.is_some_and(|c| c.can_be_used_in_text())
    }

    fn advance_while_next_is(&mut self, predicate: fn(char) -> bool) {
        while self.next_char.is_some_and(predicate) {
            self.advance()
        }
    }
}

//TODO: Try and be more evil in tests
//TODO: At least test for unicode/emoji
//TODO: Node attributes
//TODO: Escaping, i.e \[, \], and \\

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
            Token::NodeStart(NodeType::Title),
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

        let actual = tokenise(input);

        assert_eq!(actual, expected);
    }

    #[test]
    fn nested_nodes_without_spaces() {
        let input = "[bold[underline Cats]]";

        let expected = vec![
            Token::NodeStart(NodeType::Bold),
            Token::NodeStart(NodeType::Underline),
            Token::Text("Cats"),
            Token::NodeEnd,
            Token::NodeEnd,
        ];

        let actual = tokenise(input);

        assert_eq!(actual, expected);
    }

    #[test]
    fn nodes_missing_name() {
        let input = "[ Fantastic creatures";

        let expected = vec![
            Token::NodeStart(NodeType::Unknown),
            Token::Text("Fantastic"),
            Token::Whitespace,
            Token::Text("creatures"),
        ];

        let actual = tokenise(input);

        assert_eq!(actual, expected);
    }

    #[test]
    fn nodes_missing_name_without_whitespace() {
        let input = "[[[";

        let expected = vec![
            Token::NodeStart(NodeType::Unknown),
            Token::NodeStart(NodeType::Unknown),
            Token::NodeStart(NodeType::Unknown),
        ];

        let actual = tokenise(input);

        assert_eq!(actual, expected);
    }

    #[test]
    fn empty_string() {
        let input = "";

        let expected = vec![];

        let actual = tokenise(input);

        assert_eq!(actual, expected);
    }

    #[test]
    fn new_line_becomes_whitespace() {
        let input = "Cats\nwhiskers";

        let expected = vec![
            Token::Text("Cats"),
            Token::Whitespace,
            Token::Text("whiskers"),
        ];

        let actual = tokenise(input);

        assert_eq!(actual, expected);
    }

    #[test]
    fn two_new_lines_becomes_linebreak() {
        let input = "Cats\n\nwhiskers";

        let expected = vec![
            Token::Text("Cats"),
            Token::Linebreak,
            Token::Text("whiskers"),
        ];

        let actual = tokenise(input);

        assert_eq!(actual, expected);
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
    fn node_types() {
        let input = concat!("[bold\n", "[underline\n", "[title\n", "[mango");

        let expected = vec![
            Token::NodeStart(NodeType::Bold),
            Token::NodeStart(NodeType::Underline),
            Token::NodeStart(NodeType::Title),
            Token::NodeStart(NodeType::Unknown),
        ];

        let actual = tokenise(input);

        assert_eq!(actual, expected);
    }

    #[test]
    fn empty_node() {
        let input = "[bold]";

        let expected = vec![Token::NodeStart(NodeType::Bold), Token::NodeEnd];

        let actual = tokenise(input);

        assert_eq!(actual, expected);
    }
}
