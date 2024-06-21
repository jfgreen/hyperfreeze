use crate::tokenise::{Token, Tokeniser};

#[derive(PartialEq, Eq, Debug)]
struct Document<'a> {
    nodes: Box<[Node<'a>]>,
}

#[derive(PartialEq, Eq, Debug)]
enum Node<'a> {
    Word(&'a str),
}

#[derive(Debug)]
enum ParseError {}

fn parse(input: &str) -> Result<Document, ParseError> {
    //TODO: Pre allocate a sensible vec capacity
    let mut nodes = Vec::new();

    let tokeniser = Tokeniser::new(input);

    for token in tokeniser {
        if let Token::Word(s) = token {
            nodes.push(Node::Word(s));
        }
    }

    Ok(Document {
        nodes: nodes.into_boxed_slice(),
    })
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn one_line_paragraph() {
        let input = "We like cats very much";

        let expected = Document {
            nodes: Box::new([
                Node::Word("We"),
                Node::Word("like"),
                Node::Word("cats"),
                Node::Word("very"),
                Node::Word("much"),
            ]),
        };

        let actual = parse(input).unwrap();

        assert_eq!(actual, expected);
    }
}
