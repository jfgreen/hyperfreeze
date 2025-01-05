use std::fmt::Display;

use crate::scan::{Delimiter, ScanContext, Scanner, Token};

//TODO: Could be a type alias instead?
#[derive(PartialEq, Eq, Debug)]
pub struct Document {
    pub blocks: Box<[Block]>,
}

#[derive(PartialEq, Eq, Debug, Default)]
pub struct Metadata {
    pub id: String,
    pub title: String,
}

#[derive(PartialEq, Eq, Debug)]
pub enum Block {
    Metadata(Metadata),
    Paragraph(Paragraph),
    Alert(Alert),
}

#[derive(PartialEq, Eq, Debug)]
pub struct Paragraph(Box<[TextRun]>);

impl Paragraph {
    pub fn runs(&self) -> &[TextRun] {
        &self.0
    }
}

//TODO: What if we want to include things like lists in alerts?
//TODO: Generify the concept of a BlockContainer and limit what it can contain

#[derive(PartialEq, Eq, Debug)]
pub struct Alert {
    pub content: Box<[Paragraph]>,
    pub kind: AlertKind,
}

#[derive(PartialEq, Eq, Debug)]
pub enum AlertKind {
    Info,
    //TODO: Other kinds of alert
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

const SPACE: char = ' ';

//TODO: Make these be a macro?
fn eat(scanner: &mut Scanner, expected: Token) -> ParseResult<()> {
    if scanner.next() != expected {
        Err(ParseError::UnexpectedInput)
    } else {
        Ok(())
    }
}

fn eat_block_header<'a>(scanner: &mut Scanner<'a>) -> ParseResult<&'a str> {
    match scanner.next() {
        Token::BlockHeader(name) => Ok(name),
        _ => Err(ParseError::UnexpectedInput),
    }
}

fn eat_single_container_header<'a>(scanner: &mut Scanner<'a>) -> ParseResult<&'a str> {
    match scanner.next() {
        Token::SingleBlockContainerHeader(name) => Ok(name),
        _ => Err(ParseError::UnexpectedInput),
    }
}

fn eat_multi_container_header<'a>(scanner: &mut Scanner<'a>) -> ParseResult<&'a str> {
    match scanner.next() {
        Token::MultiBlockContainerHeader(name) => Ok(name),
        _ => Err(ParseError::UnexpectedInput),
    }
}

fn eat_identifier<'a>(scanner: &mut Scanner<'a>) -> ParseResult<&'a str> {
    match scanner.next() {
        Token::Identifier(name) => Ok(name),
        _ => Err(ParseError::UnexpectedInput),
    }
}

fn eat_text<'a>(scanner: &mut Scanner<'a>) -> ParseResult<&'a str> {
    match scanner.next() {
        Token::Text(text) => Ok(text),
        _ => Err(ParseError::UnexpectedInput),
    }
}

fn eat_delimiter(scanner: &mut Scanner) -> ParseResult<Delimiter> {
    match scanner.next() {
        Token::Delimiter(delimiter) => Ok(delimiter),
        _ => Err(ParseError::UnexpectedInput),
    }
}

fn eat_optional_whitespace(scanner: &mut Scanner) {
    //TODO: Does this need to be a loop...
    // would we ever get two consecutive whitespace tokens?
    while scanner.peek() == Token::Whitespace {
        scanner.next();
    }
}

// TODO: Maybe use asserts instead of eats for hand offs that should always be true
// Eg func A peeks a Text, calls func B. B should assert next() is text
// TODO: Handoff to and from various parse funcs feels a little adhoc
pub fn parse_str(input: &str) -> ParseResult<Document> {
    //TODO: Make metadata be a block?
    let mut scanner = Scanner::new(input);
    let mut blocks = Vec::new();

    //TODO: Think about using types to restrict set of tokens returned by each mode

    // Trim start of doc if it has some kind of whitespace
    if matches!(scanner.peek(), Token::Linebreak | Token::Blockbreak) {
        scanner.next();
    }

    loop {
        match scanner.peek() {
            Token::BlockHeader(_) => {
                let block = parse_named_block(&mut scanner)?;
                blocks.push(block);
            }
            Token::SingleBlockContainerHeader(_) => {
                //For now, alert is the only kind of container
                let block = parse_single_block_container(&mut scanner)?;
                blocks.push(block);
            }
            Token::MultiBlockContainerHeader(_) => {
                //For now, alert is the only kind of container
                let block = parse_multi_block_container(&mut scanner)?;
                blocks.push(block);
            }
            Token::EndOfFile => break,
            _ => {
                // Default for a block missing header is to assume paragraph
                // (this will likely change once we have lists)

                //TODO: Have parse_paragraph return block?
                let para = parse_paragraph(&mut scanner)?;
                let block = Block::Paragraph(para);
                blocks.push(block);
            }
        }
    }

    Ok(Document {
        blocks: blocks.into_boxed_slice(),
    })
}

fn parse_single_block_container(scanner: &mut Scanner) -> ParseResult<Block> {
    let container_name = eat_single_container_header(scanner)?;
    let container_kind = container_kind_from_name(container_name)?;
    eat(scanner, Token::Linebreak)?;

    let mut paragraphs = Vec::new();

    let para = parse_paragraph(scanner)?;
    paragraphs.push(para);

    //TODO: This should be of kind 'Container'
    let container = Alert {
        content: paragraphs.into_boxed_slice(),
        kind: container_kind,
    };
    let block = Block::Alert(container);
    Ok(block)
}

fn parse_multi_block_container(scanner: &mut Scanner) -> ParseResult<Block> {
    let container_name = eat_multi_container_header(scanner)?;
    let container_kind = container_kind_from_name(container_name)?;
    eat(scanner, Token::Linebreak)?;

    let mut paragraphs = Vec::new();

    loop {
        match scanner.peek() {
            Token::MultiBlockContainerFooter => {
                scanner.next();
                break;
            }
            //TODO: Helper func for tokens that could be para start
            Token::Text(_) | Token::Whitespace | Token::Delimiter(_) => {
                let para = parse_paragraph(scanner)?;
                paragraphs.push(para);
            }
            _ => return Err(ParseError::UnexpectedInput),
        }
    }

    //TODO: This should be of kind 'Container'
    let container = Alert {
        content: paragraphs.into_boxed_slice(),
        kind: container_kind,
    };
    let block = Block::Alert(container);
    Ok(block)
}

//TODO: This should return ContainerKind
fn container_kind_from_name(name: &str) -> ParseResult<AlertKind> {
    match name {
        "info" => Ok(AlertKind::Info),
        //TODO: Should be UnknownContainer?
        _ => Err(ParseError::UnknownBlock),
    }
}

fn parse_named_block(scanner: &mut Scanner) -> ParseResult<Block> {
    let block_name = eat_block_header(scanner)?;

    eat(scanner, Token::Linebreak)?;

    let block = match block_name {
        "metadata" => {
            //TODO: Validate that metadata is first block
            let meta = parse_metadata(scanner)?;
            Block::Metadata(meta)
        }
        "paragraph" => {
            let para = parse_paragraph(scanner)?;
            Block::Paragraph(para)
        }
        _ => return Err(ParseError::UnknownBlock),
    };

    Ok(block)
}

fn parse_metadata(scanner: &mut Scanner) -> ParseResult<Metadata> {
    let mut metadata = Metadata::default();
    scanner.push_context(ScanContext::Metadata);

    loop {
        let key = eat_identifier(scanner)?;
        eat_optional_whitespace(scanner);
        eat(scanner, Token::Colon)?;
        eat_optional_whitespace(scanner);

        // For now, the value is just everything untill the end of line
        // This might get more complicated in the future
        // e.g treating value as int, bool, list, etc?

        //TODO: Should we have a distinct token for 'metadata value'
        let value = eat_text(scanner)?;

        match key {
            "id" => metadata.id.push_str(value),
            "title" => metadata.title.push_str(value),
            _ => return Err(ParseError::UnknownMetadata),
        };

        match scanner.next() {
            Token::EndOfFile | Token::Blockbreak => break,
            Token::Linebreak => continue,
            _ => return Err(ParseError::UnexpectedInput),
        }
    }

    scanner.pop_context();
    Ok(metadata)
}

fn parse_paragraph(scanner: &mut Scanner) -> ParseResult<Paragraph> {
    let mut text_runs = Vec::new();
    scanner.push_context(ScanContext::Paragraph);

    loop {
        let run = match scanner.peek() {
            Token::Text(_) | Token::Whitespace => parse_plain_text(scanner)?,
            Token::Delimiter(_) => parse_delimited_text(scanner)?,
            Token::EndOfFile => break,
            Token::Blockbreak => {
                scanner.next();
                break;
            }
            //TODO: Should we instead give up and return on anything
            // we dont recognise? (i.e dont throw unexpected here?)
            Token::MultiBlockContainerFooter => break,
            _ => return Err(ParseError::UnexpectedInput),
        };
        text_runs.push(run);
    }

    scanner.pop_context();
    Ok(Paragraph(text_runs.into_boxed_slice()))
}

fn parse_plain_text(scanner: &mut Scanner) -> ParseResult<TextRun> {
    let mut run = String::new();

    let mut pending_linebreak = false;

    loop {
        match scanner.peek() {
            Token::Text(text) => {
                // Last token was linebreak,
                // and now we have more text.
                // So treat as a space.

                if pending_linebreak {
                    pending_linebreak = false;
                    run.push(SPACE);
                }
                scanner.next();
                run.push_str(text);
            }
            Token::Whitespace => {
                scanner.next();
                run.push(SPACE);
            }
            Token::Linebreak => {
                scanner.next();
                eat_optional_whitespace(scanner);
                // Wait to see if the next line is more text
                pending_linebreak = true;
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
    let delimiter = eat_delimiter(scanner)?;

    use Delimiter::*;

    let style = match delimiter {
        Asterisk => Style::Strong,
        Underscore => Style::Emphasis,
        Tilde => Style::Strikethrough,
        Backtick => Style::Raw,
    };

    let run = if delimiter == Backtick {
        parse_raw_text_run(scanner)?
    } else {
        parse_styled_text_run(scanner, delimiter)?
    };

    if run.is_empty() {
        return Err(ParseError::EmptyDelimitedText);
    }

    Ok(TextRun { text: run, style })
}

fn parse_styled_text_run(scanner: &mut Scanner, end: Delimiter) -> ParseResult<String> {
    let mut run = String::new();

    loop {
        match scanner.next() {
            Token::Text(text) => {
                run.push_str(text);
            }
            Token::Whitespace => {
                run.push(SPACE);
            }
            Token::Delimiter(d) if d == end => {
                break;
            }
            Token::Delimiter(_) => {
                return Err(ParseError::UnexpectedInput);
            }
            Token::Linebreak => {
                eat_optional_whitespace(scanner);
                run.push(SPACE);
            }
            _ => return Err(ParseError::UnmatchedDelimiter),
        }
    }

    if run.starts_with(SPACE) || run.ends_with(SPACE) {
        return Err(ParseError::LooseDelimiter);
    }

    Ok(run)
}

fn parse_raw_text_run(scanner: &mut Scanner) -> ParseResult<String> {
    scanner.push_context(ScanContext::InlineRaw);
    let mut run = String::new();

    loop {
        match scanner.next() {
            Token::Delimiter(Delimiter::Backtick) => {
                break;
            }
            //TODO: Should raw text fragment be its own type?
            Token::Text(text) => {
                run.push_str(text);
            }
            Token::Linebreak => {
                run.push(SPACE);
            }
            _ => return Err(ParseError::UnmatchedDelimiter),
        }
    }

    scanner.pop_context();
    Ok(run)
}

#[cfg(test)]
mod test {
    use super::*;
    //TODO: Things to test
    // More evils: _``_, `*`*
    // Test: -foo\nbar- <- Valid?
    // Test: -foo\n\nbar- <- Invalid?
    // Test: escaped chars in metadata
    // Test: just a '#'
    // Explicit #paragraph
    //
    // Very evil test for peek across context bounaries
    // (i.e token cached from peek in context A used context B)

    fn document() -> DocmentBuilder {
        DocmentBuilder::new()
    }

    fn metadata() -> MetadataBuilder {
        MetadataBuilder::new()
    }

    fn paragraph() -> ParagraphBuilder {
        ParagraphBuilder::new()
    }

    fn info() -> AlertBuilder {
        AlertBuilder::new(AlertKind::Info)
    }

    struct DocmentBuilder {
        blocks: Vec<Block>,
    }

    impl DocmentBuilder {
        fn new() -> Self {
            DocmentBuilder { blocks: Vec::new() }
        }

        fn build(self) -> Document {
            Document {
                blocks: self.blocks.into_boxed_slice(),
            }
        }

        fn with_block<T: Into<Block>>(mut self, block: T) -> Self {
            self.blocks.push(block.into());
            self
        }
    }

    struct MetadataBuilder {
        metadata: Metadata,
    }

    impl MetadataBuilder {
        fn new() -> Self {
            MetadataBuilder {
                metadata: Metadata::default(),
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

        fn build(self) -> Metadata {
            self.metadata
        }
    }

    impl Into<Block> for MetadataBuilder {
        fn into(self) -> Block {
            Block::Metadata(self.build())
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

        fn build(self) -> Paragraph {
            Paragraph(self.text_runs.into_boxed_slice())
        }
    }

    impl Into<Block> for ParagraphBuilder {
        fn into(self) -> Block {
            Block::Paragraph(self.build())
        }
    }

    struct AlertBuilder {
        content: Vec<Paragraph>,
        kind: AlertKind,
    }

    impl AlertBuilder {
        fn new(kind: AlertKind) -> Self {
            AlertBuilder {
                content: Vec::new(),
                kind,
            }
        }

        fn with(mut self, paragraph: ParagraphBuilder) -> Self {
            self.content.push(paragraph.build());
            self
        }
    }

    impl Into<Block> for AlertBuilder {
        fn into(self) -> Block {
            Block::Alert(Alert {
                content: self.content.into_boxed_slice(),
                kind: self.kind,
            })
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
    fn doc_with_leading_new_lines() {
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
            .with_block(
                metadata()
                    .with_id("01.23")
                    .with_title("Practical espionage for felines"),
            )
            .build();

        let actual = parse_str(input).unwrap();

        assert_eq!(actual, expected);
    }

    //TODO: Do we even want to support this syntax?
    // Maybe only for single anon blocks
    #[test]
    fn one_paragraph_info() {
        let input = concat!(
            "#=info\n",
            "Did you know that cats can reach speeds of up to *30mph*?"
        );

        let expected = document()
            .with_block(
                info().with(
                    paragraph()
                        .with_run("Did you know that cats can reach speeds of up to ")
                        .with_strong_run("30mph")
                        .with_run("?"),
                ),
            )
            .build();

        let actual = parse_str(input).unwrap();
        assert_eq!(actual, expected);
    }

    //TODO: Test for unterminated container
    //TODO: Test for container at EOF
    //TODO: Test for "pretty varients" of alert #=info=, #==info== and #=== INFO ===
    #[test]
    fn multi_paragraph_info() {
        let input = concat!(
            "#==info\n",
            "Here are some facts...\n",
            "\n",
            "...about the cats!\n",
            "#=="
        );

        let expected = document()
            .with_block(
                info()
                    .with(paragraph().with_run("Here are some facts..."))
                    .with(paragraph().with_run("...about the cats!")),
            )
            .build();

        let actual = parse_str(input).unwrap();
        assert_eq!(actual, expected);
    }
}
