use std::fmt::Display;

use crate::scan::{chars::*, CharExt, Peek, Scanner, ScannerError};

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
pub enum Style {
    None,
    Strong,
    Emphasis,
    Strikethrough,
    Raw,
}

#[derive(PartialEq, Eq, Debug)]
pub enum ParseError {
    ScannerError(ScannerError),
    UnexpectedInput,
    UnmatchedDelimiter,
    LooseDelimiter,
    EmptyDelimitedText,
    UnknownMetadata,
    UnknownBlock,
}

impl Display for ParseError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ParseError::ScannerError(e) => write!(f, "{}", e),
            ParseError::UnexpectedInput => write!(f, "unexpected input"),
            ParseError::UnmatchedDelimiter => write!(f, "unmatched delimiter"),
            ParseError::LooseDelimiter => write!(f, "loose delimiter"),
            ParseError::EmptyDelimitedText => write!(f, "empty delimited text"),
            ParseError::UnknownMetadata => write!(f, "unknown metadata"),
            ParseError::UnknownBlock => write!(f, "unknown block"),
        }
    }
}

type ParseResult<T> = Result<T, ParseError>;

impl From<ScannerError> for ParseError {
    fn from(error: ScannerError) -> Self {
        ParseError::ScannerError(error)
    }
}

const SPACE: char = ' ';

pub fn parse_str(input: &str) -> ParseResult<Document> {
    let mut scanner = Scanner::new(input);
    let mut blocks = Vec::new();

    let mut metadata = Metadata::default();

    loop {
        match scanner.peek() {
            Peek::Char(_) => {
                let block_name = parse_block_name(&mut scanner)?;

                match block_name {
                    "metadata" => {
                        parse_metadata(&mut scanner, &mut metadata)?;
                    }
                    "paragraph" => {
                        let para = parse_paragraph(&mut scanner)?;
                        blocks.push(para);
                    }
                    _ => return Err(ParseError::UnknownBlock),
                }
            }
            // Ignore leading newlines
            Peek::Linebreak => {
                scanner.eat_expected_char(NEW_LINE)?;
            }
            // Ignore block breaks
            Peek::Blockbreak => {
                scanner.eat_blockbreak()?;
            }
            Peek::EndOfFile => break,
        }
    }

    Ok(Document {
        blocks: blocks.into_boxed_slice(),
        metadata,
    })
}

fn parse_block_name<'a, 'b>(scanner: &'a mut Scanner<'b>) -> ParseResult<&'b str> {
    match scanner.peek() {
        Peek::Char(HASH) => {
            scanner.eat_expected_char(HASH)?;
            let block_name = scanner.eat_identifier()?;
            scanner.eat_expected_char(NEW_LINE)?;
            Ok(block_name)
        }
        Peek::Char(_) => Ok("paragraph"),
        _ => return Err(ParseError::UnexpectedInput),
    }
}

fn parse_metadata(scanner: &mut Scanner, metadata: &mut Metadata) -> ParseResult<()> {
    loop {
        let key = scanner.eat_identifier()?;
        scanner.eat_expected_char(COLON)?;
        scanner.eat_optional_whitespace();

        // For now, the value is just everything untill the end of line
        // This might get more complicated in the future
        // e.g treating value as int, bool, list, etc?

        let value = scanner.eat_until_linebreak()?;

        match key {
            "id" => metadata.id.push_str(value),
            "title" => metadata.title.push_str(value),
            _ => return Err(ParseError::UnknownMetadata),
        };

        match scanner.peek() {
            Peek::EndOfFile => break,
            Peek::Blockbreak => {
                scanner.eat_blockbreak()?;
                break;
            }
            Peek::Linebreak => {
                scanner.eat_expected_char(NEW_LINE)?;
                continue;
            }
            _ => return Err(ParseError::UnexpectedInput),
        }
    }

    Ok(())
}

fn parse_paragraph(scanner: &mut Scanner) -> ParseResult<Block> {
    let mut text_runs = Vec::new();

    loop {
        let run = match scanner.peek() {
            Peek::Char(c) if c.is_delimiter() => parse_delimited_text(scanner)?,
            Peek::Char(_) => parse_plain_text(scanner)?,
            Peek::EndOfFile => break,
            Peek::Blockbreak => {
                scanner.eat_blockbreak()?;
                break;
            }
            _ => return Err(ParseError::UnexpectedInput),
        };
        text_runs.push(run);
    }

    Ok(Block::Paragraph(text_runs.into_boxed_slice()))
}

fn parse_plain_text(scanner: &mut Scanner) -> ParseResult<TextRun> {
    let mut run = String::new();
    loop {
        match scanner.peek() {
            Peek::Char(BACKSLASH) => {
                scanner.eat_expected_char(BACKSLASH)?;
                let escaped = scanner.eat_char()?;
                run.push(escaped);
            }
            Peek::Char(c) if c.is_whitespace() => {
                let _ = scanner.eat_whitespace();
                run.push(SPACE);
            }
            Peek::Char(c) if c.usable_in_word() => {
                let text = scanner.eat_text_fragment()?;
                run.push_str(text);
            }
            Peek::Linebreak => {
                scanner.eat_expected_char(NEW_LINE)?;
                run.push(SPACE);
            }
            _ => break,
        }
    }

    Ok(TextRun {
        text: run,
        style: Style::None,
    })
}

fn parse_delimited_text(scanner: &mut Scanner) -> ParseResult<TextRun> {
    let delimiter = scanner.eat_char()?;

    let style = match delimiter {
        ASTERISK => Style::Strong,
        UNDERSCORE => Style::Emphasis,
        TILDE => Style::Strikethrough,
        BACKTICK => Style::Raw,
        _ => return Err(ParseError::UnexpectedInput),
    };

    let run = if delimiter == BACKTICK {
        parse_raw_text_run(scanner)?
    } else {
        parse_styled_text_run(scanner, delimiter)?
    };

    if run.is_empty() {
        return Err(ParseError::EmptyDelimitedText);
    }

    Ok(TextRun { text: run, style })
}

fn parse_styled_text_run(scanner: &mut Scanner, end: char) -> ParseResult<String> {
    let mut run = String::new();

    loop {
        match scanner.peek() {
            Peek::Char(BACKSLASH) => {
                scanner.eat_expected_char(BACKSLASH)?;
                let escaped = scanner.eat_char()?;
                run.push(escaped);
            }
            Peek::Char(c) if c.is_whitespace() => {
                scanner.eat_whitespace()?;
                run.push(SPACE);
            }
            Peek::Char(c) if c == end => {
                scanner.eat_expected_char(c)?;
                break;
            }
            Peek::Char(_) => {
                let text = scanner.eat_text_fragment()?;
                run.push_str(text)
            }
            Peek::Linebreak => {
                scanner.eat_expected_char(NEW_LINE)?;
                run.push(SPACE);
            }
            _ => return Err(ParseError::UnmatchedDelimiter),
        }
    }

    if run.starts_with(SPACE) || run.ends_with(SPACE) {
        return Err(ParseError::LooseDelimiter);
    }

    return Ok(run);
}

fn parse_raw_text_run(scanner: &mut Scanner) -> ParseResult<String> {
    let mut run = String::new();

    loop {
        match scanner.peek() {
            Peek::Char(BACKTICK) => {
                scanner.eat_expected_char(BACKTICK)?;
                return Ok(run);
            }
            Peek::Char(_) => {
                let text = scanner.eat_raw_fragment()?;
                run.push_str(text)
            }
            Peek::Linebreak => {
                scanner.eat_expected_char(NEW_LINE)?;
                run.push(SPACE);
            }
            _ => return Err(ParseError::UnmatchedDelimiter),
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;
    //TODO: Things to test
    // Enforce that `foo\n\nbar` is invalid
    // Strip leading whitespace from para
    // Foo_bar_baz vs foobar_baz
    // More evils: _``_, `*`*
    // Test: -foo\nbar- <- Valid?
    // Test: -foo\n\nbar- <- Invalid?

    // TODO: Macros to make building test cases less painful?

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
    fn two_new_lines_become_blocks() {
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
    fn three_new_lines_becomes_blocks() {
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
    fn hash_in_markup() {
        let input = "My cat does backflips #coolcat";

        let run = TextRun {
            text: String::from("My cat does backflips #coolcat"),
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
    fn escaped_underscore() {
        let input = "cat\\_case";

        let run = TextRun {
            text: String::from("cat_case"),
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
    fn escaped_underscore_in_emphasis() {
        let input = "_cat\\_case_";

        let run = TextRun {
            text: String::from("cat_case"),
            style: Style::Emphasis,
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
    fn escaped_ignored_in_raw() {
        let input = "`cat\\_case`";

        let run = TextRun {
            text: String::from("cat\\_case"),
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
    fn strong_words() {
        let input = "I *need to pet that cat* right away.";

        let run1 = TextRun {
            text: String::from("I "),
            style: Style::None,
        };

        let run2 = TextRun {
            text: String::from("need to pet that cat"),
            style: Style::Strong,
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
    fn strong_mid_word() {
        let input = "I said: mee*ooOOo*ww!";

        let run1 = TextRun {
            text: String::from("I said: mee"),
            style: Style::None,
        };

        let run2 = TextRun {
            text: String::from("ooOOo"),
            style: Style::Strong,
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
    fn strong_over_two_lines() {
        let input = "*me\now*";

        let run1 = TextRun {
            text: String::from("me ow"),
            style: Style::Strong,
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
    fn raw_leading_with_new_line() {
        let input = "`\nMeow`";

        let run = TextRun {
            text: String::from(" Meow"),
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
    fn raw_trailing_with_new_line() {
        let input = "`Meow\n`";

        let run = TextRun {
            text: String::from("Meow "),
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
    fn raw_leading_with_space() {
        let input = "` Meow`";

        let run = TextRun {
            text: String::from(" Meow"),
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
    fn raw_trailing_with_space() {
        let input = "`Meow `";

        let run = TextRun {
            text: String::from("Meow "),
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
    fn empty_raw() {
        let input = "Robot cat says: ``!.";

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
    fn loose_strong_delimiter_start() {
        let input = "* meow meow*";

        let expected = Err(ParseError::LooseDelimiter);

        let actual = parse_str(input);

        assert_eq!(actual, expected);
    }

    #[test]
    fn loose_strong_delimiter_end() {
        let input = "*meow meow *";

        let expected = Err(ParseError::LooseDelimiter);

        let actual = parse_str(input);

        assert_eq!(actual, expected);
    }

    #[test]
    fn doc_with_leading_new_line() {
        let input = "\nCats";

        let run = TextRun {
            text: String::from("Cats"),
            style: Style::None,
        };

        let para = Block::Paragraph(Box::new([run]));

        let expected = Document {
            metadata: Metadata::default(),
            blocks: Box::new([para]),
        };

        let actual = parse_str(input).unwrap();

        assert_eq!(actual, expected);
    }

    #[test]
    fn doc_with_leading_newlines() {
        let input = "\n\nCats";

        let run = TextRun {
            text: String::from("Cats"),
            style: Style::None,
        };

        let para = Block::Paragraph(Box::new([run]));

        let expected = Document {
            metadata: Metadata::default(),
            blocks: Box::new([para]),
        };

        let actual = parse_str(input).unwrap();

        assert_eq!(actual, expected);
    }

    #[test]
    fn doc_ending_with_new_line() {
        let input = "Cats are friends\n";

        let run = TextRun {
            text: String::from("Cats are friends"),
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
    fn doc_ending_with_new_lines() {
        let input = "Feline friends\n\n";

        let run = TextRun {
            text: String::from("Feline friends"),
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
