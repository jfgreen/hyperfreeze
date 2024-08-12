use crate::tokenise::{Delimit, Token, Tokeniser};
use std::iter::Peekable;

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

#[derive(PartialEq, Eq, Debug)]
enum ParseError {
    UnmatchedDelimiter,
    EmptyDelimitedText,
}

// TODO: Propper error handling, replace calls to panic!
// TODO: Pre allocate sensible vec capacities?
// TODO: This all gets easier if we fold tokensier into parser?

fn parse(input: &str) -> Result<Document, ParseError> {
    let tokeniser = Tokeniser::new(input);
    let mut tokens = tokeniser.into_iter().peekable();
    let mut blocks = Vec::new();

    //TODO: Explictly look for an Eof token?
    //TODO: Try the same pattern to tokeniser?
    //TODO: Strip leading whitespace from para

    while let Some(_) = tokens.peek() {
        let para = parse_paragraph(&mut tokens)?;
        blocks.push(para);
    }

    Ok(Document {
        blocks: blocks.into_boxed_slice(),
    })
}

fn parse_paragraph<'a>(tokens: &mut Peekable<Tokeniser<'a>>) -> Result<Block, ParseError> {
    let mut text_runs = Vec::new();

    //TODO; All a bit rough
    while let Some(token) = tokens.peek() {
        match token {
            Token::Whitespace | Token::Text(_) => text_runs.push(parse_plain_text(tokens)?),
            Token::Delimiter(d) => text_runs.push(parse_delimited_text(*d, tokens)?),
            Token::Linebreak => {
                tokens.next();
                break;
            }
            _ => panic!("unexpected token"),
        }
    }

    Ok(Block::Paragraph(text_runs.into_boxed_slice()))
}

fn parse_plain_text<'a>(tokens: &mut Peekable<Tokeniser<'a>>) -> Result<TextRun, ParseError> {
    let mut run = String::new();
    loop {
        //TODO: Is peek needed here?
        match tokens.peek() {
            Some(Token::Text(text)) => run.push_str(text),
            Some(Token::Whitespace) => run.push_str(" "),
            _ => break,
        }
        tokens.next();
    }

    Ok(TextRun {
        text: run,
        format: Format::None,
    })
}
fn parse_delimited_text<'a>(
    d1: Delimit,
    tokens: &mut Peekable<Tokeniser<'a>>,
) -> Result<TextRun, ParseError> {
    // Passing in d1 and calling this feels clumsy
    // TODO: Easier if we dont use token stream...
    // ...just direct access to our custom char iter.
    tokens.next();

    let mut run = String::new();
    //TODO: Something more elegant?
    if let Some(Token::Delimiter(d2)) = tokens.peek() {
        if d1 == *d2 {
            return Err(ParseError::EmptyDelimitedText);
        }
    }
    loop {
        match tokens.next() {
            Some(Token::Text(text)) => run.push_str(text),
            Some(Token::Whitespace) => run.push_str(" "),
            Some(Token::Delimiter(d2)) if d1 == d2 => break,
            _ => return Err(ParseError::UnmatchedDelimiter),
        }
    }

    let format = match d1 {
        Delimit::Emphasis => Format::Emphasis,
        Delimit::Bold => Format::Bold,
        Delimit::Strikethrough => Format::Strikethrough,
        Delimit::Raw => Format::Raw,
    };

    Ok(TextRun { text: run, format })
}

#[cfg(test)]
mod test {
    use super::*;

    //TODO: Make unclosed delimiters illegal

    //TODO: More evils: _``_, `*`*
    //TODO: Test: -foo\nbar- <- Valid?
    //TODO: Test: -foo\n\nbar- <- Invalid?
    //TODO: Foo_bar_baz vs foobar_baz
    //TODO: References
    //TODO: Decide on if we want to enforce "tight" delimiters
    //TODO: Bold and emph are ok mid word, but what about raw?

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
    fn double_space() {
        let input = "Nice  kitty!";

        let run = TextRun {
            text: String::from("Nice kitty!"),
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
    fn new_line_becomes_whitespace() {
        let input = "Cats\nwhiskers";

        let run = TextRun {
            text: String::from("Cats whiskers"),
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
    fn two_new_lines_becomes_whitespace() {
        let input = "Cats\n\nwhiskers";

        let run1 = TextRun {
            text: String::from("Cats"),
            format: Format::None,
        };

        let run2 = TextRun {
            text: String::from("whiskers"),
            format: Format::None,
        };

        let text1 = Box::new([run1]);
        let text2 = Box::new([run2]);

        let expected = Document {
            blocks: Box::new([Block::Paragraph(text1), Block::Paragraph(text2)]),
        };

        let actual = parse(input).unwrap();

        assert_eq!(actual, expected);
    }

    #[test]
    fn three_new_lines_becomes_whitespace() {
        let input = "Cats\n\n\nwhiskers";

        let run1 = TextRun {
            text: String::from("Cats"),
            format: Format::None,
        };

        let run2 = TextRun {
            text: String::from("whiskers"),
            format: Format::None,
        };

        let text1 = Box::new([run1]);
        let text2 = Box::new([run2]);

        let expected = Document {
            blocks: Box::new([Block::Paragraph(text1), Block::Paragraph(text2)]),
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
    #[ignore]
    fn underscore_in_raw() {
        let input = "Set `PURR_LOUDLY` to true";

        let run1 = TextRun {
            text: String::from("Set "),
            format: Format::None,
        };

        let run2 = TextRun {
            text: String::from("PURR_LOUDLY"),
            format: Format::Raw,
        };

        let run3 = TextRun {
            text: String::from(" to true"),
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
    #[ignore]
    fn extra_spaces_in_raw() {
        let input = "`Keep your       distance`";

        let run = TextRun {
            text: String::from("Keep your       distance"),
            format: Format::Raw,
        };

        let text = Box::new([run]);

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
    fn underscore_in_awkward_places() {
        let input = "Cat cat_cat cat_ cat.";

        let run1 = TextRun {
            text: String::from("Cat cat"),
            format: Format::None,
        };

        let run2 = TextRun {
            text: String::from("cat cat"),
            format: Format::Emphasis,
        };

        let run3 = TextRun {
            text: String::from(" cat."),
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
    fn empty_emphasis() {
        let input = "Rules cats must follow: __.";

        let expected = Err(ParseError::EmptyDelimitedText);

        let actual = parse(input);

        assert_eq!(actual, expected);
    }

    #[test]
    fn unmatched_emphasis_1() {
        let input = "_.";

        let expected = Err(ParseError::UnmatchedDelimiter);

        let actual = parse(input);

        assert_eq!(actual, expected);
    }

    #[test]
    fn unmatched_emphasis_2() {
        let input = "meow _meow.";

        let expected = Err(ParseError::UnmatchedDelimiter);

        let actual = parse(input);

        assert_eq!(actual, expected);
    }

    #[test]
    fn unmatched_emphasis_3() {
        let input = "meow meow_";

        let expected = Err(ParseError::UnmatchedDelimiter);

        let actual = parse(input);

        assert_eq!(actual, expected);
    }

    //TODO: Maybe mixture of bold/emph/strike is ok? Use bit mask?

    //TODO: Raw is a bit different as it can contain other delimiters...
    // but must treat them as text.
    //TODO: We probably want to have the tokeniser understand that a
    //'*' inside raw text is not a delimiter
    // But... it does not make sense that a tokeniser _and_ the parser keep track of state
    // Is the seperation of the two actually not that helpful?!
}
