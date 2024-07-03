use crate::tokenise::{Token, Tokeniser};

#[derive(PartialEq, Eq, Debug)]
struct Document<'a> {
    nodes: Box<[Node<'a>]>,
}

#[derive(PartialEq, Eq, Debug)]
enum Node<'a> {
    Word(&'a str),
    EmphasisedWords(Box<[&'a str]>),
}

#[derive(Debug)]
enum ParseError {}

fn parse(input: &str) -> Result<Document, ParseError> {
    //TODO: Pre allocate a sensible vec capacity
    let mut nodes = Vec::new();

    let tokeniser = Tokeniser::new(input);
    let mut tokens = tokeniser.into_iter();

    //TODO: Explictly look for an Eof token?

    while let Some(token) = tokens.next() {
        //TODO: Would lookahead be useful?

        match token {
            Token::Word(word) => nodes.push(Node::Word(word)),
            Token::EmphasisDelimiter => nodes.push(parse_emphasised_text(&mut tokens)),
            Token::Whitespace => (),
            _ => panic!("unexpected token"), // TODO: Propper error handling
        }
    }

    Ok(Document {
        nodes: nodes.into_boxed_slice(),
    })
}

fn parse_emphasised_text<'a, 'b>(tokens: &'a mut Tokeniser<'b>) -> Node<'b> {
    let mut words = Vec::new();

    loop {
        match tokens.next() {
            Some(Token::Word(word)) => words.push(word),
            Some(Token::Whitespace) => (),
            Some(Token::EmphasisDelimiter) => break,
            _ => panic!("unexpected token"), // TODO: Propper error handling
        }
    }

    Node::EmphasisedWords(words.into_boxed_slice())
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

    #[test]
    fn emphasised_words() {
        let input = "We _totally adore_ them";

        let expected = Document {
            nodes: Box::new([
                Node::Word("We"),
                Node::EmphasisedWords(Box::new(["totally", "adore"])),
                Node::Word("them"),
            ]),
        };

        let actual = parse(input).unwrap();

        assert_eq!(actual, expected);
    }

    /*
    #[test]
    fn emphasised_empty_string() {
        let input = "Rules cats must follow: __";

        let expected = Document {
            nodes: Box::new([
                Node::Word("Rules"),
                Node::Word("cats"),
                Node::Word("must"),
                Node::Word("follow:"),
            ]),
        };

        let actual = parse(input).unwrap();

        assert_eq!(actual, expected);
    }
    */
}
