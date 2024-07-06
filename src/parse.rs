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

//TODO: Replace 'Word' with words?

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
            Token::EmphasisDelimiter => {
                let text = parse_styled_text(&mut tokens, Token::EmphasisDelimiter);
                nodes.push(Node::EmphasisedWords(text));
            }
            Token::BoldDelimiter => {
                let text = parse_styled_text(&mut tokens, Token::BoldDelimiter);
                nodes.push(Node::BoldWords(text));
            }
            Token::StrikethroughDelimiter => {
                let text = parse_styled_text(&mut tokens, Token::StrikethroughDelimiter);
                nodes.push(Node::StrikethroughWords(text));
            }
            Token::RawDelimiter => {
                let text = parse_styled_text(&mut tokens, Token::RawDelimiter);
                nodes.push(Node::RawWords(text));
            }
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

    #[ignore]
    #[test]
    fn empty_emphasis() {
        let input = "Rules cats must follow: __.";

        let expected = Document {
            nodes: Box::new([
                Node::Word("Rules"),
                Node::Word("cats"),
                Node::Word("must"),
                Node::Word("follow:"),
                Node::Word("."),
            ]),
        };

        let actual = parse(input).unwrap();

        assert_eq!(actual, expected);
    }

    #[ignore]
    #[test]
    fn underscore_in_awkward_places() {
        let input = "Cat cat_cat cat_ cat.";

        let expected = Document {
            nodes: Box::new([
                Node::Word("Cat"),
                Node::Word("cat_cat"),
                Node::Word("cat_"),
                Node::Word("cat."),
            ]),
        };

        let actual = parse(input).unwrap();

        assert_eq!(actual, expected);
    }

    #[ignore]
    #[test]
    fn multi_dash_mid_word() {
        let input = "Visit Catville-on-sea today!";

        let expected = Document {
            nodes: Box::new([
                Node::Word("Visit"),
                Node::Word("Catville-on-sea"),
                Node::Word("today!"),
            ]),
        };

        let actual = parse(input).unwrap();

        assert_eq!(actual, expected);
    }

    #[ignore]
    #[test]
    fn invalid_strikethroughs() {
        let input = concat!(
            "Cat -cat cat- cat.\n",   // 1
            "Cat -cat cat - cat.\n",  // 2
            "Cat - cat cat- cat.\n",  // 3
            "Cat - cat cat - cat.\n", // 4
            "Cat- cat cat- cat.\n",   // 5
            "Cat -cat cat -cat.\n",   // 6
            "Cat- cat cat -cat.\n"    // 7
        );

        let expected = Document {
            nodes: Box::new([
                // Line 1
                Node::Word("Cat"),
                Node::StrikethroughWords(Box::new(["cat", "cat"])),
                Node::Word("cat."),
                // Line 2
                Node::Word("Cat"),
                Node::Word("-cat"),
                Node::Word("cat"),
                Node::Word("-"),
                Node::Word("cat."),
                // Line 3
                Node::Word("Cat"),
                Node::Word("-"),
                Node::Word("cat"),
                Node::Word("cat-"),
                Node::Word("cat."),
                // Line 4
                Node::Word("Cat"),
                Node::Word("-"),
                Node::Word("cat"),
                Node::Word("cat"),
                Node::Word("-"),
                Node::Word("cat."),
                // Line 5
                Node::Word("Cat-"),
                Node::Word("cat"),
                Node::Word("cat-"),
                Node::Word("cat."),
                // Line 6
                Node::Word("Cat"),
                Node::Word("-cat"),
                Node::Word("cat"),
                Node::Word("-cat."),
                // Line 7
                Node::Word("Cat-"),
                Node::Word("cat"),
                Node::Word("cat"),
                Node::Word("-cat."),
            ]),
        };
    }

    #[ignore]
    #[test]
    fn standalone_dash() {
        let input = "Felines - fantastic!";

        let expected = Document {
            nodes: Box::new([
                Node::Word("Felines"),
                Node::Word("-"),
                Node::Word("fantastic!"),
            ]),
        };

        let actual = parse(input).unwrap();

        assert_eq!(actual, expected);
    }
}
