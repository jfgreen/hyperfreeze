use crate::tokenise::{Token, Tokeniser};

#[derive(PartialEq, Eq, Debug)]
struct Document<'a> {
    nodes: Box<[Node<'a>]>,
}

//TODO: Is there a more efficent way of representing a document tree?
// Think: Flatter, all pre-allocated, contigious
#[derive(PartialEq, Eq, Debug)]
enum Node<'a> {
    Word(&'a str),
    EmphasisedWords(Box<[&'a str]>),
    BoldWords(Box<[&'a str]>),
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
            Token::BoldDelimiter => nodes.push(parse_bold_text(&mut tokens)),
            Token::Whitespace => (),
            _ => panic!("unexpected token"), // TODO: Propper error handling
        }
    }

    Ok(Document {
        nodes: nodes.into_boxed_slice(),
    })
}

fn parse_emphasised_text<'a, 'b>(tokens: &'a mut Tokeniser<'b>) -> Node<'b> {
    let mut words: Vec<&'b str> = Vec::new();

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

//FIXME: This is a bit repetative
fn parse_bold_text<'a, 'b>(tokens: &'a mut Tokeniser<'b>) -> Node<'b> {
    let mut words: Vec<&'b str> = Vec::new();

    loop {
        match tokens.next() {
            Some(Token::Word(word)) => words.push(word),
            Some(Token::Whitespace) => (),
            Some(Token::BoldDelimiter) => break,
            _ => panic!("unexpected token"), // TODO: Propper error handling
        }
    }

    Node::BoldWords(words.into_boxed_slice())
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

    #[test]
    fn bold_words() {
        let input = "I *need to pet that cat* right away.";

        let expected = Document {
            nodes: Box::new([
                Node::Word("I"),
                Node::BoldWords(Box::new(["need", "to", "pet", "that", "cat"])),
                Node::Word("right"),
                Node::Word("away."),
            ]),
        };

        let actual = parse(input).unwrap();

        assert_eq!(actual, expected);
    }

    //TODO: What do we do about 'Things like_this example'?
    //TODO: ALso:  "Rules cats must follow: __"
}
