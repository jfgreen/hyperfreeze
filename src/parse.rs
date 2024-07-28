use crate::tokenise::{Delimit, Token, Tokeniser};

//TODO: Reflect one if this document model is better than a higher level token stream?
// Lots of structure and indirection...
// So, is there a more efficent way of representing a document tree?
// Code up an alternative and compare?
// Token stream of text, space, start format, end format?
// Think about what will be easy to emit HTML for?
// Think: Flatter, all pre-allocated, contigious
// Think: Copy words runs into string fragments?
// Try the below struct, then...
// Mad idea, use unicode private use area and have whole doc a one string...
// But might not work nicely if we need numeric values?
// Could work just for paragraph / markup type?
//enum SemanticToken {
//    DelimiterStart(Delimit)
//    DelimiterEnd(Delimit)
//    Char(char),
//}

#[derive(PartialEq, Eq, Debug)]
struct Document {
    blocks: Box<[Block]>,
}

#[derive(PartialEq, Eq, Debug)]
enum Block {
    Paragraph(Box<[TextRun]>),
}

#[derive(PartialEq, Eq, Debug)]
struct TextRun {
    text: String,
    format: Format,
}

#[derive(PartialEq, Eq, Debug)]
enum Format {
    None,
    Bold,
    Emphasis,
    Strikethrough,
    Raw,
}

#[derive(Debug)]
enum ParseError {}

// TODO: Propper error handling, replace calls to panic!

fn parse(input: &str) -> Result<Document, ParseError> {
    //TODO: Pre allocate a sensible vec capacity?
    let mut text_runs = Vec::new();

    let tokeniser = Tokeniser::new(input);
    let mut tokens = tokeniser.into_iter().peekable();

    //TODO: Explictly look for an Eof token?
    //TODO: Try the same pattern to tokeniser?
    //TODO: Strip leading whitespace from para

    while let Some(token) = tokens.next() {
        match token {
            //TODO: Reduce duplication
            Token::Whitespace => {
                let mut run = String::new();
                run.push_str(" ");

                loop {
                    match tokens.peek() {
                        Some(Token::Text(word)) => run.push_str(word),
                        Some(Token::Whitespace) => run.push_str(" "),
                        _ => break,
                    }
                    tokens.next();
                }

                text_runs.push(TextRun {
                    text: run,
                    format: Format::None,
                });
            }
            Token::Text(word) => {
                let mut run = String::new();
                run.push_str(word);

                loop {
                    match tokens.peek() {
                        Some(Token::Text(word)) => run.push_str(word),
                        Some(Token::Whitespace) => run.push_str(" "),
                        _ => break,
                    }
                    tokens.next();
                }

                text_runs.push(TextRun {
                    text: run,
                    format: Format::None,
                });
            }
            Token::Delimiter(d1) => {
                let mut run = String::new();
                loop {
                    match tokens.next() {
                        Some(Token::Text(word)) => run.push_str(word),
                        Some(Token::Whitespace) => run.push_str(" "),
                        Some(Token::Delimiter(d2)) if d1 == d2 => break,
                        //TODO: We probably actualy want to handle EOF gracefully
                        Some(_) => panic!("unexpected token"),
                        None => panic!("unexpected eof"),
                    }
                }

                let format = match d1 {
                    Delimit::Emphasis => Format::Emphasis,
                    Delimit::Bold => Format::Bold,
                    Delimit::Strikethrough => Format::Strikethrough,
                    Delimit::Raw => Format::Raw,
                };

                text_runs.push(TextRun { text: run, format });
            }
            _ => panic!("unexpected token"),
        }
    }

    Ok(Document {
        blocks: Box::new([Block::Paragraph(text_runs.into_boxed_slice())]),
    })
}

#[cfg(test)]
mod test {
    use super::*;

    //TODO: Make unclosed delimiters illegal

    //TODO: More evils: _``_, `*`*
    //TODO: Foo_bar_baz vs foobar_baz
    //TODO: References

    //TODO: Test leading whitespace in a paragraph is ignored
    //TODO: Add test for bold mid word
    //TODO: Test newlines
    //TODO: Macros to make building test cases less painful

    #[test]
    fn one_line_paragraph() {
        let input = "We like cats very much";

        let run = TextRun {
            text: String::from("We like cats very much"),
            format: Format::None,
        };

        let text = Box::new([run]);

        let expected = Document {
            blocks: Box::new([Block::Paragraph(text)]),
        };

        let actual = parse(input).unwrap();

        assert_eq!(actual, expected);
    }

    #[test]
    fn emphasised_words() {
        let input = "We _totally adore_ them";

        let run1 = TextRun {
            text: String::from("We "),
            format: Format::None,
        };

        let run2 = TextRun {
            text: String::from("totally adore"),
            format: Format::Emphasis,
        };

        let run3 = TextRun {
            text: String::from(" them"),
            format: Format::None,
        };

        let text = Box::new([run1, run2, run3]);

        let expected = Document {
            blocks: Box::new([Block::Paragraph(text)]),
        };

        let actual = parse(input).unwrap();

        assert_eq!(actual, expected);
    }

    #[test]
    fn bold_words() {
        let input = "I *need to pet that cat* right away.";

        let run1 = TextRun {
            text: String::from("I "),
            format: Format::None,
        };

        let run2 = TextRun {
            text: String::from("need to pet that cat"),
            format: Format::Bold,
        };

        let run3 = TextRun {
            text: String::from(" right away."),
            format: Format::None,
        };

        let text = Box::new([run1, run2, run3]);

        let expected = Document {
            blocks: Box::new([Block::Paragraph(text)]),
        };

        let actual = parse(input).unwrap();

        assert_eq!(actual, expected);
    }

    #[test]
    fn strikethrough_words() {
        let input = "Cats are ~ok i guess~ magnificant";

        let run1 = TextRun {
            text: String::from("Cats are "),
            format: Format::None,
        };

        let run2 = TextRun {
            text: String::from("ok i guess"),
            format: Format::Strikethrough,
        };

        let run3 = TextRun {
            text: String::from(" magnificant"),
            format: Format::None,
        };

        let text = Box::new([run1, run2, run3]);

        let expected = Document {
            blocks: Box::new([Block::Paragraph(text)]),
        };

        let actual = parse(input).unwrap();

        assert_eq!(actual, expected);
    }

    #[test]
    fn raw_words() {
        let input = "Robot cat says `bleep bloop`!";

        let run1 = TextRun {
            text: String::from("Robot cat says "),
            format: Format::None,
        };

        let run2 = TextRun {
            text: String::from("bleep bloop"),
            format: Format::Raw,
        };

        let run3 = TextRun {
            text: String::from("!"),
            format: Format::None,
        };

        let text = Box::new([run1, run2, run3]);

        let expected = Document {
            blocks: Box::new([Block::Paragraph(text)]),
        };

        let actual = parse(input).unwrap();

        assert_eq!(actual, expected);
    }

    #[test]
    fn standalone_dash() {
        let input = "Felines - fantastic!";

        let run1 = TextRun {
            text: String::from("Felines - fantastic!"),
            format: Format::None,
        };

        let text = Box::new([run1]);

        let expected = Document {
            blocks: Box::new([Block::Paragraph(text)]),
        };

        let actual = parse(input).unwrap();

        assert_eq!(actual, expected);
    }

    #[test]
    #[ignore] // FIXME: we probably want to make this illegal
    fn empty_emphasis() {
        let input = "Rules cats must follow: __.";

        let run1 = TextRun {
            text: String::from("Rules cats must follow: __."),
            format: Format::None,
        };

        let text = Box::new([run1]);

        let expected = Document {
            blocks: Box::new([Block::Paragraph(text)]),
        };

        let actual = parse(input).unwrap();

        assert_eq!(actual, expected);
    }
    /*

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


    //TODO: Test: -foo\nbar- <- Valid?
    //TODO: Test: -foo\n\nbar- <- Invalid?
    */
}
