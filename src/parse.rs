use std::fmt::Display;

use crate::scanner::{Scanner, ScannerError};

// TODO: Pick a system for IDs, e.g J Decimal, then use newtype or alias
// TODO: Enforce basic metadata?

// TODO: Reflect on if this document model is better than a token stream?
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
//
// We could start by having a parse state

#[derive(PartialEq, Eq, Debug)]
pub struct Document {
    pub metadata: Metadata,
    pub blocks: Box<[Block]>,
}

#[derive(PartialEq, Eq, Debug, Default)]
pub struct Metadata {
    pub id: String,
    pub title: String,
}

#[derive(PartialEq, Eq, Debug)]
pub enum Block {
    Paragraph(Box<[TextRun]>),
}

#[derive(PartialEq, Eq, Debug)]
pub struct TextRun {
    pub text: String,
    pub style: Style,
}

#[derive(PartialEq, Eq, Debug)]
//TODO 'Strong' instead of bold?
pub enum Style {
    None,
    Bold,
    Emphasis,
    Strikethrough,
    Raw,
}

#[derive(PartialEq, Eq, Debug)]
pub enum ParseError {
    ScannerError(ScannerError),
    //UnexpectedToken,
    //UnmatchedDelimiter,
    //LooseDelimiter,
    //EmptyDelimitedText,
    UnknownMetadata,
}

impl Display for ParseError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ParseError::ScannerError(e) => write!(f, "{}", e),
            //ParseError::UnexpectedToken => write!(f, "unexpected token"),
            //ParseError::UnmatchedDelimiter => write!(f, "unmatched delimiter"),
            //ParseError::LooseDelimiter => write!(f, "loose delimiter"),
            //ParseError::EmptyDelimitedText => write!(f, "empty delimited text"),
            ParseError::UnknownMetadata => write!(f, "unknown metadata"),
        }
    }
}

type ParseResult<T> = Result<T, ParseError>;

impl From<ScannerError> for ParseError {
    fn from(error: ScannerError) -> Self {
        ParseError::ScannerError(error)
    }
}

// TODO: Pre allocate sensible vec capacities?

// TODO: Helpful macros, expecially for checking current token is expected

// TODO: Allow parsing over buffered input stream
pub fn parse_str(input: &str) -> ParseResult<Document> {
    let mut scanner = Scanner::new(input);
    let mut blocks = Vec::new();

    let mut metadata = Metadata::default();

    //TODO: Strip leading whitespace from para

    // We either expect the start of a block or EOF

    while scanner.has_input_remaining() {
        if scanner.current_char_is("#") {
            scanner.eat_char('#')?;
            let block_name = scanner.eat_identifier()?;

            match block_name {
                "metadata" => {
                    parse_metadata(&mut scanner, &mut metadata)?;
                }
                "paragraph" => {
                    let para = parse_paragraph(&mut scanner)?;
                    blocks.push(para);
                }
            }
        } else {
            // As a syntactic sugar, the default block type is paragraph
            let para = parse_paragraph(&mut scanner)?;
            blocks.push(para);
        }
    }

    Ok(Document {
        blocks: blocks.into_boxed_slice(),
        metadata,
    })
}

fn parse_metadata<'a>(scanner: &mut Scanner, metadata: &mut Metadata) -> ParseResult<()> {
    scanner.eat_linebreak()?;

    loop {
        let key = scanner.eat_identifier()?;
        scanner.eat_char(":")?;
        scanner.eat_optional_whitespace()?;

        // For now, the value is just everything untill the end of line
        // This might get more complicated in the future
        // e.g treating value as int, bool, list, etc?

        let value = scanner.eat_untill_newline()?;

        match key {
            "id" => metadata.id.push_str(value),
            "title" => metadata.title.push_str(value),
            _ => return Err(ParseError::UnknownMetadata),
        };

        scanner.eat_newline()?;

        if scanner.has_no_input_remaining() {
            break;
        }

        if scanner.current_char_is('\n') {
            scanner.eat_newline()?;
            break;
        }
    }

    Ok(())
}

fn parse_paragraph<'a>(tokeniser: &mut Tokeniser) -> Result<Block, ParseError> {
    let mut text_runs = Vec::new();

    loop {
        let run = match tokeniser.current_token {
            Token::Whitespace | Token::Text(_) => parse_plain_text(tokeniser)?,
            Token::FormatDelimiter(d) => parse_delimited_text(tokeniser, d)?,
            Token::RawDelimiter => parse_raw_text(tokeniser)?,
            //TODO: Should this be pulled up as all blocks are seperated with a line break?
            Token::Blockbreak => {
                tokeniser.advance();
                break;
            }
            Token::Eof => {
                break;
            }
            _ => return Err(ParseError::UnexpectedToken),
        };
        text_runs.push(run);
    }

    Ok(Block::Paragraph(text_runs.into_boxed_slice()))
}

fn parse_plain_text<'a>(tokeniser: &mut Tokeniser) -> Result<TextRun, ParseError> {
    let mut run = String::new();
    loop {
        match tokeniser.current_token {
            Token::Text(text) => run.push_str(text),
            Token::Whitespace => run.push_str(" "),
            _ => break,
        }
        tokeniser.advance();
    }

    Ok(TextRun {
        text: run,
        style: Style::None,
    })
}

fn parse_raw_text<'a>(tokeniser: &mut Tokeniser) -> Result<TextRun, ParseError> {
    tokeniser.expect_raw_delimiter()?;

    // TODO: Add test for empty delimited text
    if tokeniser.current_token == Token::RawDelimiter {
        return Err(ParseError::EmptyDelimitedText);
    }

    let mut run = String::new();

    loop {
        match tokeniser.current_token {
            Token::Text(text) => run.push_str(text),
            //TODO: Is it clear where this whitespace comes from? (new lines?)
            Token::Whitespace => run.push_str(" "),
            Token::RawDelimiter => break,
            _ => return Err(ParseError::UnmatchedDelimiter),
        }
        tokeniser.advance_inline_raw();
    }

    tokeniser.advance();

    Ok(TextRun {
        text: run.to_string(),
        style: Style::Raw,
    })
}

fn parse_delimited_text<'a>(
    tokeniser: &mut Tokeniser,
    run_delimiter: Format,
) -> Result<TextRun, ParseError> {
    expect(tokeniser, Token::FormatDelimiter(run_delimiter))?;
    tokeniser.advance();

    if tokeniser.current_token == Token::FormatDelimiter(run_delimiter) {
        return Err(ParseError::EmptyDelimitedText);
    }

    let mut run = String::new();

    loop {
        match tokeniser.current_token {
            Token::Text(text) => run.push_str(text),
            Token::Whitespace => run.push_str(" "),
            Token::FormatDelimiter(d) if d == run_delimiter => break,
            _ => return Err(ParseError::UnmatchedDelimiter),
        }
        tokeniser.advance()
    }

    if run.starts_with(" ") || run.ends_with(" ") {
        return Err(ParseError::LooseDelimiter);
    }

    let style = match run_delimiter {
        Format::Emphasis => Style::Emphasis,
        Format::Bold => Style::Bold,
        Format::Strikethrough => Style::Strikethrough,
    };

    tokeniser.advance();

    Ok(TextRun { text: run, style })
}

#[cfg(test)]
mod test {
    use super::*;

    //TODO: Enforce that `foo\n\nbar` is invalid

    //TODO: Maybe mixture of bold/emph/strike is ok? Use bit mask?
    //TODO: More evils: _``_, `*`*
    //TODO: Test: -foo\nbar- <- Valid?
    //TODO: Test: -foo\n\nbar- <- Invalid?
    //TODO: Foo_bar_baz vs foobar_baz
    //TODO: References
    //TODO: Escaped chars

    //TODO: Test leading whitespace in a paragraph is ignored
    //TODO: Test newlines

    //TODO: Figure out what to do with newlines in raw text
    //      djot just uses same whitespace rules as normal text runs

    //TODO: Macros to make building test cases less painful?

    #[test]
    fn one_line_paragraph() {
        let input = "We like cats very much";

        let run = TextRun {
            text: String::from("We like cats very much"),
            style: Style::None,
        };

        let text = Box::new([run]);

        let expected = Document {
            metadata: Metadata::default(),
            blocks: Box::new([Block::Paragraph(text)]),
        };

        let actual = parse_str(input).unwrap();

        assert_eq!(actual, expected);
    }

    #[test]
    fn double_space() {
        let input = "Nice  kitty!";

        let run = TextRun {
            text: String::from("Nice kitty!"),
            style: Style::None,
        };

        let text = Box::new([run]);

        let expected = Document {
            metadata: Metadata::default(),
            blocks: Box::new([Block::Paragraph(text)]),
        };

        let actual = parse_str(input).unwrap();

        assert_eq!(actual, expected);
    }

    #[test]
    fn new_line_becomes_whitespace() {
        let input = "Cats\nwhiskers";

        let run = TextRun {
            text: String::from("Cats whiskers"),
            style: Style::None,
        };

        let text = Box::new([run]);

        let expected = Document {
            metadata: Metadata::default(),
            blocks: Box::new([Block::Paragraph(text)]),
        };

        let actual = parse_str(input).unwrap();

        assert_eq!(actual, expected);
    }

    #[test]
    fn two_new_lines_becomes_whitespace() {
        let input = "Cats\n\nwhiskers";

        let run1 = TextRun {
            text: String::from("Cats"),
            style: Style::None,
        };

        let run2 = TextRun {
            text: String::from("whiskers"),
            style: Style::None,
        };

        let text1 = Box::new([run1]);
        let text2 = Box::new([run2]);

        let expected = Document {
            metadata: Metadata::default(),
            blocks: Box::new([Block::Paragraph(text1), Block::Paragraph(text2)]),
        };

        let actual = parse_str(input).unwrap();

        assert_eq!(actual, expected);
    }

    #[test]
    fn three_new_lines_becomes_whitespace() {
        let input = "Cats\n\n\nwhiskers";

        let run1 = TextRun {
            text: String::from("Cats"),
            style: Style::None,
        };

        let run2 = TextRun {
            text: String::from("whiskers"),
            style: Style::None,
        };

        let text1 = Box::new([run1]);
        let text2 = Box::new([run2]);

        let expected = Document {
            metadata: Metadata::default(),
            blocks: Box::new([Block::Paragraph(text1), Block::Paragraph(text2)]),
        };

        let actual = parse_str(input).unwrap();

        assert_eq!(actual, expected);
    }

    #[test]
    fn emphasised_words() {
        let input = "We _totally adore_ them";

        let run1 = TextRun {
            text: String::from("We "),
            style: Style::None,
        };

        let run2 = TextRun {
            text: String::from("totally adore"),
            style: Style::Emphasis,
        };

        let run3 = TextRun {
            text: String::from(" them"),
            style: Style::None,
        };

        let text = Box::new([run1, run2, run3]);

        let expected = Document {
            metadata: Metadata::default(),
            blocks: Box::new([Block::Paragraph(text)]),
        };

        let actual = parse_str(input).unwrap();

        assert_eq!(actual, expected);
    }

    #[test]
    fn bold_words() {
        let input = "I *need to pet that cat* right away.";

        let run1 = TextRun {
            text: String::from("I "),
            style: Style::None,
        };

        let run2 = TextRun {
            text: String::from("need to pet that cat"),
            style: Style::Bold,
        };

        let run3 = TextRun {
            text: String::from(" right away."),
            style: Style::None,
        };

        let text = Box::new([run1, run2, run3]);

        let expected = Document {
            metadata: Metadata::default(),
            blocks: Box::new([Block::Paragraph(text)]),
        };

        let actual = parse_str(input).unwrap();

        assert_eq!(actual, expected);
    }

    #[test]
    fn bold_mid_word() {
        let input = "I said: mee*ooOOo*ww!";

        let run1 = TextRun {
            text: String::from("I said: mee"),
            style: Style::None,
        };

        let run2 = TextRun {
            text: String::from("ooOOo"),
            style: Style::Bold,
        };

        let run3 = TextRun {
            text: String::from("ww!"),
            style: Style::None,
        };

        let text = Box::new([run1, run2, run3]);

        let expected = Document {
            metadata: Metadata::default(),
            blocks: Box::new([Block::Paragraph(text)]),
        };

        let actual = parse_str(input).unwrap();

        assert_eq!(actual, expected);
    }

    #[test]
    fn strikethrough_words() {
        let input = "Cats are ~ok i guess~ magnificant";

        let run1 = TextRun {
            text: String::from("Cats are "),
            style: Style::None,
        };

        let run2 = TextRun {
            text: String::from("ok i guess"),
            style: Style::Strikethrough,
        };

        let run3 = TextRun {
            text: String::from(" magnificant"),
            style: Style::None,
        };

        let text = Box::new([run1, run2, run3]);

        let expected = Document {
            metadata: Metadata::default(),
            blocks: Box::new([Block::Paragraph(text)]),
        };

        let actual = parse_str(input).unwrap();

        assert_eq!(actual, expected);
    }

    #[test]
    fn raw_words() {
        let input = "Robot cat says `bleep bloop`!";

        let run1 = TextRun {
            text: String::from("Robot cat says "),
            style: Style::None,
        };

        let run2 = TextRun {
            text: String::from("bleep bloop"),
            style: Style::Raw,
        };

        let run3 = TextRun {
            text: String::from("!"),
            style: Style::None,
        };

        let text = Box::new([run1, run2, run3]);

        let expected = Document {
            metadata: Metadata::default(),
            blocks: Box::new([Block::Paragraph(text)]),
        };

        let actual = parse_str(input).unwrap();

        assert_eq!(actual, expected);
    }

    #[test]
    fn raw_mid_word() {
        let input = "Bl`ee`p!";

        let run1 = TextRun {
            text: String::from("Bl"),
            style: Style::None,
        };

        let run2 = TextRun {
            text: String::from("ee"),
            style: Style::Raw,
        };

        let run3 = TextRun {
            text: String::from("p!"),
            style: Style::None,
        };

        let text = Box::new([run1, run2, run3]);

        let expected = Document {
            metadata: Metadata::default(),
            blocks: Box::new([Block::Paragraph(text)]),
        };

        let actual = parse_str(input).unwrap();

        assert_eq!(actual, expected);
    }

    #[test]
    fn underscore_in_raw() {
        let input = "Set `PURR_LOUDLY` to true";

        let run1 = TextRun {
            text: String::from("Set "),
            style: Style::None,
        };

        let run2 = TextRun {
            text: String::from("PURR_LOUDLY"),
            style: Style::Raw,
        };

        let run3 = TextRun {
            text: String::from(" to true"),
            style: Style::None,
        };

        let text = Box::new([run1, run2, run3]);

        let expected = Document {
            metadata: Metadata::default(),
            blocks: Box::new([Block::Paragraph(text)]),
        };

        let actual = parse_str(input).unwrap();

        assert_eq!(actual, expected);
    }

    #[test]
    fn extra_spaces_in_raw() {
        let input = "`Keep your       distance`";

        let run = TextRun {
            text: String::from("Keep your       distance"),
            style: Style::Raw,
        };

        let text = Box::new([run]);

        let expected = Document {
            metadata: Metadata::default(),
            blocks: Box::new([Block::Paragraph(text)]),
        };

        let actual = parse_str(input).unwrap();

        assert_eq!(actual, expected);
    }

    #[test]
    fn raw_over_two_lines() {
        let input = "`Great\ncats`";

        let run = TextRun {
            text: String::from("Great cats"),
            style: Style::Raw,
        };

        let text = Box::new([run]);

        let expected = Document {
            metadata: Metadata::default(),
            blocks: Box::new([Block::Paragraph(text)]),
        };

        let actual = parse_str(input).unwrap();

        assert_eq!(actual, expected);
    }

    #[test]
    fn raw_over_three_lines() {
        let input = "`Great\ncats\nassemble!`";

        let run = TextRun {
            text: String::from("Great cats assemble!"),
            style: Style::Raw,
        };

        let text = Box::new([run]);

        let expected = Document {
            metadata: Metadata::default(),
            blocks: Box::new([Block::Paragraph(text)]),
        };

        let actual = parse_str(input).unwrap();

        assert_eq!(actual, expected);
    }

    #[test]
    fn standalone_dash() {
        let input = "Felines - fantastic!";

        let run1 = TextRun {
            text: String::from("Felines - fantastic!"),
            style: Style::None,
        };

        let text = Box::new([run1]);

        let expected = Document {
            metadata: Metadata::default(),
            blocks: Box::new([Block::Paragraph(text)]),
        };

        let actual = parse_str(input).unwrap();

        assert_eq!(actual, expected);
    }

    #[test]
    fn underscore_in_awkward_places() {
        let input = "Cat cat_cat cat_ cat.";

        let run1 = TextRun {
            text: String::from("Cat cat"),
            style: Style::None,
        };

        let run2 = TextRun {
            text: String::from("cat cat"),
            style: Style::Emphasis,
        };

        let run3 = TextRun {
            text: String::from(" cat."),
            style: Style::None,
        };

        let text = Box::new([run1, run2, run3]);

        let expected = Document {
            metadata: Metadata::default(),
            blocks: Box::new([Block::Paragraph(text)]),
        };

        let actual = parse_str(input).unwrap();

        assert_eq!(actual, expected);
    }

    #[test]
    fn empty_emphasis() {
        let input = "Rules cats must follow: __.";

        let expected = Err(ParseError::EmptyDelimitedText);

        let actual = parse_str(input);

        assert_eq!(actual, expected);
    }

    #[test]
    fn unmatched_emphasis_1() {
        let input = "_.";

        let expected = Err(ParseError::UnmatchedDelimiter);

        let actual = parse_str(input);

        assert_eq!(actual, expected);
    }

    #[test]
    fn unmatched_emphasis_2() {
        let input = "meow _meow.";

        let expected = Err(ParseError::UnmatchedDelimiter);

        let actual = parse_str(input);

        assert_eq!(actual, expected);
    }

    #[test]
    fn unmatched_emphasis_3() {
        let input = "meow meow_";

        let expected = Err(ParseError::UnmatchedDelimiter);

        let actual = parse_str(input);

        assert_eq!(actual, expected);
    }

    #[test]
    fn loose_bold_delimiter_start() {
        let input = "* meow meow*";

        let expected = Err(ParseError::LooseDelimiter);

        let actual = parse_str(input);

        assert_eq!(actual, expected);
    }

    #[test]
    fn loose_bold_delimiter_end() {
        let input = "*meow meow *";

        let expected = Err(ParseError::LooseDelimiter);

        let actual = parse_str(input);

        assert_eq!(actual, expected);
    }

    //TODO: Test for variable whitespace in key/value arg
    //TODO: Test doc with both metadata and paragraph
    #[test]
    fn doc_metadata() {
        let input = concat!(
            "#metadata\n",
            "id: 01.23\n",
            "title: Practical espionage for felines\n",
        );

        let expected = Metadata {
            id: String::from("01.23"),
            title: String::from("Practical espionage for felines"),
        };

        let parsed = parse_str(input).unwrap();
        let actual = parsed.metadata;

        assert_eq!(actual, expected);
    }
}
