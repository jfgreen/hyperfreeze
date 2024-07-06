use crate::tokenise::{Token, Tokeniser};

#[derive(PartialEq, Eq, Debug)]
struct Document<'a> {
    nodes: Box<[Node<'a>]>,
}

// We create a document tree, not a token stream, so that:
// - Its easy to generate a table of contents
// - Links dont have to be defined up front
// - The types model the overall semantic structure of a doc
// - It is clear as to which combinations of nodes can be nested
//
// If we didnt do these here, then HTML generation would have to build
// its own tree, or make more than one pass of the token stream.

//TODO: Is there a more efficent way of representing a document tree?
// Think: Flatter, all pre-allocated, contigious
#[derive(PartialEq, Eq, Debug)]
enum Node<'a> {
    Word(&'a str),
    EmphasisedWords(Box<[&'a str]>),
    BoldWords(Box<[&'a str]>),
    StrikethroughWords(Box<[&'a str]>),
    RawWords(Box<[&'a str]>),
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

        //FIXME: This is getting a bit unwieldy
        match token {
            Token::Word(word) => nodes.push(Node::Word(word)),
            Token::EmphasisDelimiter => nodes.push(Node::EmphasisedWords(parse_styled_text(
                &mut tokens,
                Token::EmphasisDelimiter,
            ))),
            Token::BoldDelimiter => nodes.push(Node::BoldWords(parse_styled_text(
                &mut tokens,
                Token::BoldDelimiter,
            ))),
            Token::StrikethroughDelimiter => nodes.push(Node::StrikethroughWords(
                parse_styled_text(&mut tokens, Token::StrikethroughDelimiter),
            )),
            Token::RawDelimiter => nodes.push(Node::RawWords(parse_styled_text(
                &mut tokens,
                Token::RawDelimiter,
            ))),
            Token::Whitespace => (),
            _ => panic!("unexpected token"), // TODO: Propper error handling
        }
    }

    Ok(Document {
        nodes: nodes.into_boxed_slice(),
    })
}

fn parse_styled_text<'a, 'b>(tokens: &'a mut Tokeniser<'b>, end_token: Token) -> Box<[&'b str]> {
    let mut words: Vec<&'b str> = Vec::new();

    loop {
        match tokens.next() {
            Some(Token::Word(word)) => words.push(word),
            Some(Token::Whitespace) => (),
            Some(token) if token == end_token => break,
            _ => panic!("unexpected token"), // TODO: Propper error handling
        }
    }

    words.into_boxed_slice()
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

    #[test]
    fn strikethrough_words() {
        let input = "Cats are -ok i guess- magnificant";

        let expected = Document {
            nodes: Box::new([
                Node::Word("Cats"),
                Node::Word("are"),
                Node::StrikethroughWords(Box::new(["ok", "i", "guess"])),
                Node::Word("magnificant"),
            ]),
        };

        let actual = parse(input).unwrap();

        assert_eq!(actual, expected);
    }

    #[test]
    fn raw_words() {
        let input = "Robot cat says `bleep bloop`!";

        let expected = Document {
            nodes: Box::new([
                Node::Word("Robot"),
                Node::Word("cat"),
                Node::Word("says"),
                Node::RawWords(Box::new(["bleep", "bloop"])),
                Node::Word("!"),
            ]),
        };

        let actual = parse(input).unwrap();

        assert_eq!(actual, expected);
    }

    //TODO: What do we do about 'Things like_this example'?
    //TODO: Also: "Rules cats must follow: __"
    //TODO: Also: "Or-this-example"
    //TODO: Also: "or this-"
    //TODO: Also: "-or this"
}
