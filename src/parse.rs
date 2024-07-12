use crate::tokenise::{Delimit, Token, Tokeniser};

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
// Think: Copy words runs into string fragments?

//TODO: Replace 'Word' with words?

#[derive(PartialEq, Eq, Debug)]
enum Node<'a> {
    Words(Box<[&'a str]>),
    EmphasisedWords(Box<[&'a str]>),
    BoldWords(Box<[&'a str]>),
    StrikethroughWords(Box<[&'a str]>),
    RawWords(Box<[&'a str]>),
}

#[derive(Debug)]
enum ParseError {}

// TODO: Propper error handling, replace calls to panic!

fn parse(input: &str) -> Result<Document, ParseError> {
    //TODO: Pre allocate a sensible vec capacity
    let mut nodes = Vec::new();

    let tokeniser = Tokeniser::new(input);
    let mut tokens = tokeniser.into_iter().peekable();

    //TODO: Explictly look for an Eof token?

    //TODO: Try a simmilar pattern to tokeniser?

    while let Some(token) = tokens.next() {
        match token {
            Token::Word(word) => {
                let mut words: Vec<&str> = Vec::new();
                words.push(word);
                loop {
                    match tokens.peek() {
                        Some(Token::Word(word)) => words.push(word),
                        Some(Token::Whitespace) => (),
                        _ => break,
                    }
                    tokens.next();
                }

                let text = words.into_boxed_slice();

                if let Some(Node::Words(existing_words)) = nodes.last() {
                    // Heal word runs split by bogus use of delimiters. e.g '__'

                    // FIXME: Either remove the need for this edge case,
                    // or keep the last node as a vec.

                    let mut joined_words: Vec<&str> = Vec::new();
                    joined_words.extend(existing_words.iter());
                    joined_words.extend(text.iter());
                    let joined_text = joined_words.into_boxed_slice();

                    // FIXME: This is a bit meh
                    let last_node = nodes.len();
                    nodes[last_node - 1] = Node::Words(joined_text);
                } else {
                    nodes.push(Node::Words(text));
                }
            }
            Token::Delimiter(d1) => {
                let mut words: Vec<&str> = Vec::new();

                loop {
                    match tokens.next() {
                        Some(Token::Word(word)) => words.push(word),
                        Some(Token::Whitespace) => (),
                        Some(Token::Delimiter(d2)) if d1 == d2 => break,
                        _ => panic!("unexpected token"),
                    }
                }

                let text = words.into_boxed_slice();

                if text.len() == 0 {
                    // Empty delimited text, e.g __ or **
                    continue;
                }

                let node = match d1 {
                    Delimit::Emphasis => Node::EmphasisedWords(text),
                    Delimit::Bold => Node::BoldWords(text),
                    Delimit::Strikethrough => Node::StrikethroughWords(text),
                    Delimit::Raw => Node::RawWords(text),
                };

                nodes.push(node);
            }
            Token::Whitespace => (),
            _ => panic!("unexpected token"),
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
            nodes: Box::new([Node::Words(Box::new([
                "We", "like", "cats", "very", "much",
            ]))]),
        };

        let actual = parse(input).unwrap();

        assert_eq!(actual, expected);
    }

    #[test]
    fn emphasised_words() {
        let input = "We _totally adore_ them";

        let expected = Document {
            nodes: Box::new([
                Node::Words(Box::new(["We"])),
                Node::EmphasisedWords(Box::new(["totally", "adore"])),
                Node::Words(Box::new(["them"])),
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
                Node::Words(Box::new(["I"])),
                Node::BoldWords(Box::new(["need", "to", "pet", "that", "cat"])),
                Node::Words(Box::new(["right", "away."])),
            ]),
        };

        let actual = parse(input).unwrap();

        assert_eq!(actual, expected);
    }

    #[test]
    fn strikethrough_words() {
        let input = "Cats are ~ok i guess~ magnificant";

        let expected = Document {
            nodes: Box::new([
                Node::Words(Box::new(["Cats", "are"])),
                Node::StrikethroughWords(Box::new(["ok", "i", "guess"])),
                Node::Words(Box::new(["magnificant"])),
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
                Node::Words(Box::new(["Robot", "cat", "says"])),
                Node::RawWords(Box::new(["bleep", "bloop"])),
                Node::Words(Box::new(["!"])),
            ]),
        };

        let actual = parse(input).unwrap();

        assert_eq!(actual, expected);
    }

    #[test]
    fn empty_emphasis() {
        let input = "Rules cats must follow: __.";

        let expected = Document {
            nodes: Box::new([Node::Words(Box::new([
                "Rules", "cats", "must", "follow:", ".",
            ]))]),
        };

        let actual = parse(input).unwrap();

        assert_eq!(actual, expected);
    }

    //TODO: Revisit the following cases, is this actually realistic?
    // Think: Cant enforce that formating needs whitespace due to punctuation.
    // E.g Cats are *great*. <- The '.' would get pulled into the word.

    // DJOT says: "A _ or * can open emphasis only if it is not directly
    // followed by whitespace. It can close emphasis only if it is not
    // directly preceded by whitespace, and only if there are some
    // characters besides the delimiter character between the opener and
    // the closer."

    #[ignore]
    #[test]
    fn underscore_in_awkward_places() {
        let input = "Cat cat_cat cat_ cat.";

        let expected = Document {
            nodes: Box::new([Node::Words(Box::new(["Cat", "cat_cat", "cat_", "cat."]))]),
        };

        let actual = parse(input).unwrap();

        assert_eq!(actual, expected);
    }

    #[ignore]
    #[test]
    fn multi_dash_mid_word() {
        let input = "Visit Catville-on-sea today!";

        let expected = Document {
            nodes: Box::new([Node::Words(Box::new([
                "Visit",
                "Catville-on-sea",
                "today!",
            ]))]),
        };

        let actual = parse(input).unwrap();

        assert_eq!(actual, expected);
    }

    // TODO: Maybe bring this back as individual tests?
    /*
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
                "Cat",
                Node::StrikethroughWords(Box::new(["cat", "cat"])),
                "cat.",
                // Line 2
                "Cat",
                "-cat",
                "cat",
                "-",
                "cat.",
                // Line 3
                "Cat",
                "-",
                "cat",
                "cat-",
                "cat.",
                // Line 4
                "Cat",
                "-",
                "cat",
                "cat",
                "-",
                "cat.",
                // Line 5
                "Cat-",
                "cat",
                "cat-",
                "cat.",
                // Line 6
                "Cat",
                "-cat",
                "cat",
                "-cat.",
                // Line 7
                "Cat-",
                "cat",
                "cat",
                "-cat.",
            ]),
        };
    }
    */

    #[ignore]
    #[test]
    fn standalone_dash() {
        let input = "Felines - fantastic!";

        let expected = Document {
            nodes: Box::new([Node::Words(Box::new(["Felines", "-", "fantastic!"]))]),
        };

        let actual = parse(input).unwrap();

        assert_eq!(actual, expected);
    }

    //TODO: Test: -foo\nbar- <- Valid?
    //TODO: Test: -foo\n\nbar- <- Invalid?
}
