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
                scanner.eat_optional_whitespace();
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
            Peek::Char(c) if c.is_delimiter() => return Err(ParseError::UnexpectedInput),
            Peek::Char(_) => {
                let text = scanner.eat_text_fragment()?;
                run.push_str(text)
            }
            Peek::Linebreak => {
                scanner.eat_expected_char(NEW_LINE)?;
                scanner.eat_optional_whitespace();
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
    // More evils: _``_, `*`*
    // Test: -foo\nbar- <- Valid?
    // Test: -foo\n\nbar- <- Invalid?
    // Explicit #paragraph

    fn document() -> DocmentBuilder {
        DocmentBuilder::new()
    }

    fn paragraph() -> ParagraphBuilder {
        ParagraphBuilder::new()
    }

    struct DocmentBuilder {
        metadata: Metadata,
        blocks: Vec<Block>,
    }

    impl DocmentBuilder {
        fn new() -> Self {
            DocmentBuilder {
                metadata: Metadata::default(),
                blocks: Vec::new(),
            }
        }

        fn build(self) -> Document {
            Document {
                metadata: self.metadata,
                blocks: self.blocks.into_boxed_slice(),
            }
        }

        fn with_id(mut self, id: &str) -> Self {
            self.metadata.id = id.to_string();
            self
        }

        fn with_title(mut self, title: &str) -> Self {
            self.metadata.title = title.to_string();
            self
        }

        fn with_block<T: Into<Block>>(mut self, block: T) -> Self {
            self.blocks.push(block.into());
            self
        }
    }

    struct ParagraphBuilder {
        text_runs: Vec<TextRun>,
    }

    impl ParagraphBuilder {
        fn new() -> Self {
            ParagraphBuilder {
                text_runs: Vec::new(),
            }
        }

        fn push_run(&mut self, text: &str, style: Style) {
            let text = text.to_string();
            self.text_runs.push(TextRun { text, style });
        }

        fn with_run(mut self, text: &str) -> Self {
            self.push_run(text, Style::None);
            self
        }

        fn with_emphasised_run(mut self, text: &str) -> Self {
            self.push_run(text, Style::Emphasis);
            self
        }

        fn with_strong_run(mut self, text: &str) -> Self {
            self.push_run(text, Style::Strong);
            self
        }

        fn with_strikethrough_run(mut self, text: &str) -> Self {
            self.push_run(text, Style::Strikethrough);
            self
        }

        fn with_raw_run(mut self, text: &str) -> Self {
            self.push_run(text, Style::Raw);
            self
        }
    }

    impl Into<Block> for ParagraphBuilder {
        fn into(self) -> Block {
            Block::Paragraph(self.text_runs.into_boxed_slice())
        }
    }

    #[test]
    fn one_line_paragraph() {
        let input = "We like cats very much";

        let expected = document()
            .with_block(paragraph().with_run("We like cats very much"))
            .build();

        let actual = parse_str(input).unwrap();

        assert_eq!(actual, expected);
    }

    #[test]
    fn double_space() {
        let input = "Nice  kitty!";

        let expected = document()
            .with_block(paragraph().with_run("Nice kitty!"))
            .build();

        let actual = parse_str(input).unwrap();

        assert_eq!(actual, expected);
    }

    #[test]
    fn new_line_becomes_whitespace() {
        let input = "Cats\nwhiskers";

        let expected = document()
            .with_block(paragraph().with_run("Cats whiskers"))
            .build();

        let actual = parse_str(input).unwrap();

        assert_eq!(actual, expected);
    }

    #[test]
    fn two_new_lines_become_blocks() {
        let input = "Cats\n\nwhiskers";

        let expected = document()
            .with_block(paragraph().with_run("Cats"))
            .with_block(paragraph().with_run("whiskers"))
            .build();

        let actual = parse_str(input).unwrap();

        assert_eq!(actual, expected);
    }

    #[test]
    fn three_new_lines_becomes_blocks() {
        let input = "Cats\n\n\nwhiskers";

        let expected = document()
            .with_block(paragraph().with_run("Cats"))
            .with_block(paragraph().with_run("whiskers"))
            .build();

        let actual = parse_str(input).unwrap();

        assert_eq!(actual, expected);
    }

    #[test]
    fn hash_in_markup() {
        let input = "My cat does backflips #coolcat";

        let expected = document()
            .with_block(paragraph().with_run("My cat does backflips #coolcat"))
            .build();

        let actual = parse_str(input).unwrap();

        assert_eq!(actual, expected);
    }

    #[test]
    fn escaped_underscore() {
        let input = "cat\\_case";

        let expected = document()
            .with_block(paragraph().with_run("cat_case"))
            .build();

        let actual = parse_str(input).unwrap();

        assert_eq!(actual, expected);
    }

    #[test]
    fn escaped_underscore_in_emphasis() {
        let input = "_cat\\_case_";

        let expected = document()
            .with_block(paragraph().with_emphasised_run("cat_case"))
            .build();

        let actual = parse_str(input).unwrap();

        assert_eq!(actual, expected);
    }

    #[test]
    fn escaped_ignored_in_raw() {
        let input = "`cat\\_case`";

        let expected = document()
            .with_block(paragraph().with_raw_run("cat\\_case"))
            .build();

        let actual = parse_str(input).unwrap();

        assert_eq!(actual, expected);
    }

    #[test]
    fn emphasised_words() {
        let input = "We _totally adore_ them";

        let expected = document()
            .with_block(
                paragraph()
                    .with_run("We ")
                    .with_emphasised_run("totally adore")
                    .with_run(" them"),
            )
            .build();

        let actual = parse_str(input).unwrap();

        assert_eq!(actual, expected);
    }

    #[test]
    fn strong_words() {
        let input = "I *need to pet that cat* right away.";

        let expected = document()
            .with_block(
                paragraph()
                    .with_run("I ")
                    .with_strong_run("need to pet that cat")
                    .with_run(" right away."),
            )
            .build();

        let actual = parse_str(input).unwrap();

        assert_eq!(actual, expected);
    }

    #[test]
    fn strong_mid_word() {
        let input = "I said: mee*ooOOo*ww!";

        let expected = document()
            .with_block(
                paragraph()
                    .with_run("I said: mee")
                    .with_strong_run("ooOOo")
                    .with_run("ww!"),
            )
            .build();

        let actual = parse_str(input).unwrap();

        assert_eq!(actual, expected);
    }

    #[test]
    fn strong_over_two_lines() {
        let input = "*me\now*";

        let expected = document()
            .with_block(paragraph().with_strong_run("me ow"))
            .build();

        let actual = parse_str(input).unwrap();

        assert_eq!(actual, expected);
    }

    #[test]
    fn strikethrough_words() {
        let input = "Cats are ~ok i guess~ magnificant";

        let expected = document()
            .with_block(
                paragraph()
                    .with_run("Cats are ")
                    .with_strikethrough_run("ok i guess")
                    .with_run(" magnificant"),
            )
            .build();

        let actual = parse_str(input).unwrap();

        assert_eq!(actual, expected);
    }

    #[test]
    fn raw_words() {
        let input = "Robot cat says `bleep bloop`!";

        let expected = document()
            .with_block(
                paragraph()
                    .with_run("Robot cat says ")
                    .with_raw_run("bleep bloop")
                    .with_run("!"),
            )
            .build();

        let actual = parse_str(input).unwrap();

        assert_eq!(actual, expected);
    }

    #[test]
    fn raw_mid_word() {
        let input = "Bl`eeee`p!";

        let expected = document()
            .with_block(
                paragraph()
                    .with_run("Bl")
                    .with_raw_run("eeee")
                    .with_run("p!"),
            )
            .build();

        let actual = parse_str(input).unwrap();

        assert_eq!(actual, expected);
    }

    #[test]
    fn underscore_in_raw() {
        let input = "Set `PURR_LOUDLY` to true";

        let expected = document()
            .with_block(
                paragraph()
                    .with_run("Set ")
                    .with_raw_run("PURR_LOUDLY")
                    .with_run(" to true"),
            )
            .build();

        let actual = parse_str(input).unwrap();

        assert_eq!(actual, expected);
    }

    #[test]
    fn extra_spaces_in_raw() {
        let input = "`Keep your       distance`";

        let expected = document()
            .with_block(paragraph().with_raw_run("Keep your       distance"))
            .build();

        let actual = parse_str(input).unwrap();

        assert_eq!(actual, expected);
    }

    #[test]
    fn raw_over_two_lines() {
        let input = "`Great\ncats`";

        let expected = document()
            .with_block(paragraph().with_raw_run("Great cats"))
            .build();

        let actual = parse_str(input).unwrap();

        assert_eq!(actual, expected);
    }

    #[test]
    fn raw_leading_with_new_line() {
        let input = "`\nMeow?`";

        let expected = document()
            .with_block(paragraph().with_raw_run(" Meow?"))
            .build();

        let actual = parse_str(input).unwrap();

        assert_eq!(actual, expected);
    }

    #[test]
    fn raw_trailing_with_new_line() {
        let input = "`Meow\n`";

        let expected = document()
            .with_block(paragraph().with_raw_run("Meow "))
            .build();

        let actual = parse_str(input).unwrap();

        assert_eq!(actual, expected);
    }

    #[test]
    fn raw_leading_with_space() {
        let input = "` Meow`";

        let expected = document()
            .with_block(paragraph().with_raw_run(" Meow"))
            .build();

        let actual = parse_str(input).unwrap();

        assert_eq!(actual, expected);
    }

    #[test]
    fn raw_trailing_with_space() {
        let input = "`Meow `";

        let expected = document()
            .with_block(paragraph().with_raw_run("Meow "))
            .build();

        let actual = parse_str(input).unwrap();

        assert_eq!(actual, expected);
    }

    #[test]
    fn raw_over_three_lines() {
        let input = "`Great\ncats\nassemble!`";

        let expected = document()
            .with_block(paragraph().with_raw_run("Great cats assemble!"))
            .build();

        let actual = parse_str(input).unwrap();

        assert_eq!(actual, expected);
    }

    #[test]
    fn standalone_dash() {
        let input = "Felines - fantastic!";

        let expected = document()
            .with_block(paragraph().with_run("Felines - fantastic!"))
            .build();

        let actual = parse_str(input).unwrap();

        assert_eq!(actual, expected);
    }

    #[test]
    fn underscore_in_awkward_places() {
        let input = "Cat cat_cat cat_ cat.";

        let expected = document()
            .with_block(
                paragraph()
                    .with_run("Cat cat")
                    .with_emphasised_run("cat cat")
                    .with_run(" cat."),
            )
            .build();

        let actual = parse_str(input).unwrap();

        assert_eq!(actual, expected);
    }

    #[test]
    fn test_newline_then_multiple_spaces_in_plain_text() {
        let input = "Cat\n  cat";

        let expected = document()
            .with_block(paragraph().with_run("Cat cat"))
            .build();

        let actual = parse_str(input).unwrap();

        assert_eq!(actual, expected);
    }

    #[test]
    fn test_newline_then_multiple_spaces_in_styled() {
        let input = "*Cat\n  cat*";

        let expected = document()
            .with_block(paragraph().with_strong_run("Cat cat"))
            .build();

        let actual = parse_str(input).unwrap();

        assert_eq!(actual, expected);
    }

    #[test]
    fn test_newline_then_multiple_spaces_in_raw() {
        let input = "`Cat\n  cat`";

        let expected = document()
            .with_block(paragraph().with_raw_run("Cat   cat"))
            .build();

        let actual = parse_str(input).unwrap();

        assert_eq!(actual, expected);
    }

    #[test]
    fn test_multiple_spaces_then_newline_in_plain_text() {
        let input = "Cat  \ncat";

        let expected = document()
            .with_block(paragraph().with_run("Cat cat"))
            .build();

        let actual = parse_str(input).unwrap();

        assert_eq!(actual, expected);
    }

    #[test]
    fn test_multiple_spaces_then_newline_in_styled() {
        let input = "*Cat  \ncat*";

        let expected = document()
            .with_block(paragraph().with_strong_run("Cat cat"))
            .build();

        let actual = parse_str(input).unwrap();

        assert_eq!(actual, expected);
    }

    #[test]
    fn test_multiple_spaces_then_newline_in_raw() {
        let input = "`Cat  \ncat`";

        let expected = document()
            .with_block(paragraph().with_raw_run("Cat   cat"))
            .build();

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
    fn raw_with_double_linebreak() {
        let input = "`Erm...\n\nmeow?`";

        let expected = Err(ParseError::UnmatchedDelimiter);

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
    fn raw_immediately_in_emphasis() {
        let input = "_``_";

        let expected = Err(ParseError::UnexpectedInput);

        let actual = parse_str(input);

        assert_eq!(actual, expected);
    }

    #[test]
    fn raw_within_in_emphasis() {
        let input = "_a``a_";

        let expected = Err(ParseError::UnexpectedInput);

        let actual = parse_str(input);

        assert_eq!(actual, expected);
    }

    #[test]
    fn doc_with_leading_new_line() {
        let input = "\nCats cats cats";

        let expected = document()
            .with_block(paragraph().with_run("Cats cats cats"))
            .build();

        let actual = parse_str(input).unwrap();

        assert_eq!(actual, expected);
    }

    #[test]
    fn doc_with_leading_newlines() {
        let input = "\n\nCats cats cats";

        let expected = document()
            .with_block(paragraph().with_run("Cats cats cats"))
            .build();

        let actual = parse_str(input).unwrap();

        assert_eq!(actual, expected);
    }

    #[test]
    fn doc_ending_with_new_line() {
        let input = "Cats are friends\n";

        let expected = document()
            .with_block(paragraph().with_run("Cats are friends"))
            .build();

        let actual = parse_str(input).unwrap();

        assert_eq!(actual, expected);
    }

    #[test]
    fn doc_ending_with_new_lines() {
        let input = "Feline friends\n\n";

        let expected = document()
            .with_block(paragraph().with_run("Feline friends"))
            .build();

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

        let expected = document()
            .with_id("01.23")
            .with_title("Practical espionage for felines")
            .build();

        let actual = parse_str(input).unwrap();

        assert_eq!(actual, expected);
    }
}
