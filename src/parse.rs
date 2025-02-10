use std::fmt::Display;

use crate::scan::{Delimiter, Scanner, Token};

//TODO: Can we simplify / flatten this at all?
//TODO: Container could just be a kind of block that has other blocks in it?
//TODO: Fuzz test?

#[derive(PartialEq, Eq, Debug)]
pub struct Document {
    pub metadata: Metadata,
    pub contents: Box<[Element]>,
}

#[derive(PartialEq, Eq, Debug, Default)]
pub struct Metadata {
    pub id: String,
    pub title: String,
}

#[derive(PartialEq, Eq, Debug)]
pub enum Element {
    Block(Block),
    Container(Container),
}

//TODO: Paragraph could just be Paragraph(Box<[TextRun])>?
#[derive(PartialEq, Eq, Debug)]
pub enum Block {
    Paragraph(Paragraph),
    List(Box<[ListItem]>),
}

#[derive(PartialEq, Eq, Debug)]
pub struct Container {
    pub content: Box<[ContainedBlock]>,
    pub kind: ContainerKind,
}

#[derive(PartialEq, Eq, Debug)]
pub enum ContainedBlock {
    Paragraph(Paragraph),
}

#[derive(PartialEq, Eq, Debug)]
pub enum ContainerKind {
    Info,
    //TODO: Other kinds of alert
}

#[derive(PartialEq, Eq, Debug)]
pub struct Paragraph(Box<[TextRun]>);

impl Paragraph {
    pub fn runs(&self) -> &[TextRun] {
        &self.0
    }
}

#[derive(PartialEq, Eq, Debug)]
pub struct ListItem {
    level: usize,
    runs: Box<[TextRun]>,
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

//TODO: Errors should describe where things went wrong, and what was expected
#[derive(PartialEq, Eq, Debug)]
pub enum ParseError {
    UnexpectedInput,
    UnmatchedDelimiter,
    LooseDelimiter,
    EmptyDelimitedText,
    UnknownMetadata,
    MetadataNotAtStart,
    UnknownBlock,
    UnknownContainer,
    UnterminatedContainer,
}

impl Display for ParseError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ParseError::UnexpectedInput => write!(f, "unexpected input"),
            ParseError::UnmatchedDelimiter => write!(f, "unmatched delimiter"),
            ParseError::LooseDelimiter => write!(f, "loose delimiter"),
            ParseError::EmptyDelimitedText => write!(f, "empty delimited text"),
            ParseError::UnknownMetadata => write!(f, "unknown metadata"),
            ParseError::MetadataNotAtStart => write!(f, "metadata not at start"),
            ParseError::UnknownBlock => write!(f, "unknown block"),
            ParseError::UnknownContainer => write!(f, "unknown container"),
            ParseError::UnterminatedContainer => write!(f, "unknown container"),
        }
    }
}

type ParseResult<T> = Result<T, ParseError>;

const SPACE: char = ' ';

fn eat_optional_whitespace(scanner: &mut Scanner) {
    if scanner.peek() == Token::Whitespace {
        scanner.next();
    }
}

macro_rules! token_eater {
    ($eater:ident, $token:ident, $return:ty) => {
        fn $eater<'a>(scanner: &mut Scanner<'a>) -> ParseResult<$return> {
            match scanner.next() {
                Token::$token(contents) => Ok(contents),
                _ => Err(ParseError::UnexpectedInput),
            }
        }
    };
    ($eater:ident, $token:ident) => {
        fn $eater(scanner: &mut Scanner) -> ParseResult<()> {
            match scanner.next() {
                Token::$token => Ok(()),
                _ => Err(ParseError::UnexpectedInput),
            }
        }
    };
}

token_eater!(eat_colon, Colon);
token_eater!(eat_linebreak, Linebreak);
token_eater!(eat_delimiter, Delimiter, Delimiter);
token_eater!(eat_block_header, BlockHeader, &'a str);
token_eater!(eat_container_header, ContainerHeader, &'a str);
token_eater!(eat_identifier, Identifier, &'a str);
token_eater!(eat_meta_text, MetaText, &'a str);

pub fn parse_str(input: &str) -> ParseResult<Document> {
    let scanner = &mut Scanner::new(input);
    let mut elements = Vec::new();

    let mut metadata = Metadata::default();

    // Trim start of doc if it has some kind of whitespace
    while matches!(
        scanner.peek(),
        Token::Linebreak | Token::Blockbreak | Token::Whitespace
    ) {
        scanner.next();
    }

    if matches!(scanner.peek(), Token::BlockHeader("metadata")) {
        metadata = parse_metadata_block(scanner)?;
    }

    loop {
        match scanner.peek() {
            Token::BlockHeader(_) => {
                let element = parse_named_block(scanner)?;
                elements.push(element);
            }
            Token::ContainerHeader(_) => {
                let element = parse_container(scanner)?;
                elements.push(element);
            }
            Token::EndOfFile => break,
            Token::Text(_) | Token::Delimiter(_) => {
                // Infer paragraph missing header (a valid sugar)
                let element = parse_paragraph_block(scanner)?;
                elements.push(element);
            }
            _ => return Err(ParseError::UnexpectedInput),
        }
    }

    Ok(Document {
        metadata,
        contents: elements.into_boxed_slice(),
    })
}

fn parse_container(scanner: &mut Scanner) -> ParseResult<Element> {
    let container_name = eat_container_header(scanner)?;
    let container_kind = container_kind_from_name(container_name)?;
    eat_linebreak(scanner)?;

    let mut blocks = Vec::new();

    loop {
        match scanner.peek() {
            Token::ContainerFooter => {
                scanner.next();
                break;
            }
            Token::Text(_) | Token::Whitespace | Token::Delimiter(_) => {
                let para = parse_paragraph(scanner)?;
                let block = ContainedBlock::Paragraph(para);
                blocks.push(block);
            }
            _ => return Err(ParseError::UnterminatedContainer),
        }
    }

    let peek = scanner.peek();
    if peek == Token::Blockbreak {
        scanner.next();
    } else if peek != Token::EndOfFile {
        return Err(ParseError::UnexpectedInput);
    }

    let container = Container {
        content: blocks.into_boxed_slice(),
        kind: container_kind,
    };
    let element = Element::Container(container);
    Ok(element)
}

fn container_kind_from_name(name: &str) -> ParseResult<ContainerKind> {
    match name {
        "info" => Ok(ContainerKind::Info),
        _ => Err(ParseError::UnknownContainer),
    }
}

fn parse_named_block(scanner: &mut Scanner) -> ParseResult<Element> {
    let block_name = eat_block_header(scanner)?;

    eat_linebreak(scanner)?;

    let element = match block_name {
        "metadata" => return Err(ParseError::MetadataNotAtStart),
        "paragraph" => parse_paragraph_block(scanner)?,
        _ => return Err(ParseError::UnknownBlock),
    };

    Ok(element)
}

fn parse_metadata_block(scanner: &mut Scanner) -> ParseResult<Metadata> {
    eat_block_header(scanner)?;
    eat_linebreak(scanner)?;

    let mut metadata = Metadata::default();
    scanner.push_context_metadata();

    loop {
        let key = eat_identifier(scanner)?;
        eat_optional_whitespace(scanner);
        eat_colon(scanner)?;
        eat_optional_whitespace(scanner);

        // For now, the value is just everything untill the end of line
        // This might get more complicated in the future
        // e.g treating value as int, bool, list, etc?

        let value = eat_meta_text(scanner)?;

        match key {
            "id" => metadata.id.push_str(value),
            "title" => metadata.title.push_str(value),
            _ => return Err(ParseError::UnknownMetadata),
        };

        let next_token = scanner.next();

        match next_token {
            Token::EndOfFile | Token::Blockbreak => break,
            Token::Linebreak => continue,
            _ => return Err(ParseError::UnexpectedInput),
        }
    }

    scanner.pop_context();
    Ok(metadata)
}

fn parse_paragraph_block(scanner: &mut Scanner) -> ParseResult<Element> {
    let paragraph = parse_paragraph(scanner)?;
    let block = Block::Paragraph(paragraph);
    let element = Element::Block(block);
    Ok(element)
}

fn parse_paragraph(scanner: &mut Scanner) -> ParseResult<Paragraph> {
    let mut text_runs = Vec::new();
    scanner.push_context_paragraph();

    loop {
        let run = match scanner.peek() {
            Token::Text(_) | Token::Whitespace | Token::Linebreak => parse_plain_text(scanner)?,
            Token::Delimiter(_) => parse_delimited_text(scanner)?,
            Token::EndOfFile => break,
            Token::Blockbreak => {
                scanner.next();
                break;
            }
            Token::ContainerFooter => break,
            _ => return Err(ParseError::UnexpectedInput),
        };
        text_runs.push(run);
    }

    scanner.pop_context();
    let para = Paragraph(text_runs.into_boxed_slice());
    Ok(para)
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
                if scanner.peek() != Token::Blockbreak {
                    run.push(SPACE);
                }
                if scanner.peek() == Token::Linebreak {
                    scanner.next();
                }
            }
            Token::Linebreak => {
                scanner.next();
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
                if scanner.peek() == Token::Linebreak {
                    scanner.next();
                }
            }
            Token::Delimiter(d) if d == end => {
                break;
            }
            Token::Delimiter(_) => {
                return Err(ParseError::UnexpectedInput);
            }
            Token::Linebreak => {
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
    scanner.push_context_inline_raw();
    let mut run = String::new();

    loop {
        match scanner.next() {
            Token::Delimiter(Delimiter::Backtick) => {
                break;
            }
            Token::RawFragment(text) => {
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

    fn document() -> DocmentBuilder {
        DocmentBuilder::new()
    }

    fn metadata() -> MetadataBuilder {
        MetadataBuilder::new()
    }

    fn paragraph() -> ParagraphBuilder {
        ParagraphBuilder::new()
    }

    fn list() -> ListBuilder {
        ListBuilder::new()
    }

    fn list_item() -> ListItemBuilder {
        ListItemBuilder::new()
    }

    fn info() -> ContainerBuilder {
        ContainerBuilder::new(ContainerKind::Info)
    }

    fn text(text: &str) -> TextRun {
        TextRun {
            text: text.to_string(),
            style: Style::None,
        }
    }

    fn emphasised_text(text: &str) -> TextRun {
        TextRun {
            text: text.to_string(),
            style: Style::Emphasis,
        }
    }

    fn strong_text(text: &str) -> TextRun {
        TextRun {
            text: text.to_string(),
            style: Style::Strong,
        }
    }

    fn strikethrough_text(text: &str) -> TextRun {
        TextRun {
            text: text.to_string(),
            style: Style::Strikethrough,
        }
    }

    fn raw_text(text: &str) -> TextRun {
        TextRun {
            text: text.to_string(),
            style: Style::Raw,
        }
    }

    struct DocmentBuilder {
        metadata: Metadata,
        contents: Vec<Element>,
    }

    impl DocmentBuilder {
        fn new() -> Self {
            DocmentBuilder {
                metadata: Metadata::default(),
                contents: Vec::new(),
            }
        }

        fn build(self) -> Document {
            Document {
                metadata: self.metadata,
                contents: self.contents.into_boxed_slice(),
            }
        }
        fn with_metadata(mut self, metadata: MetadataBuilder) -> Self {
            self.metadata = metadata.build();
            self
        }

        fn with_block<T: Into<Block>>(mut self, block: T) -> Self {
            self.contents.push(Element::Block(block.into()));
            self
        }

        fn with_container<T: Into<Container>>(mut self, container: T) -> Self {
            self.contents.push(Element::Container(container.into()));
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

        //TODO: Have `with_run` accept a run
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

    impl Into<ContainedBlock> for ParagraphBuilder {
        fn into(self) -> ContainedBlock {
            ContainedBlock::Paragraph(self.build())
        }
    }

    struct ListBuilder {
        items: Vec<ListItem>,
    }

    impl ListBuilder {
        fn new() -> Self {
            Self { items: Vec::new() }
        }

        fn with<T: Into<ListItem>>(mut self, item: T) -> Self {
            self.items.push(item.into());
            self
        }

        fn build(self) -> Box<[ListItem]> {
            self.items.into_boxed_slice()
        }
    }

    impl Into<Block> for ListBuilder {
        fn into(self) -> Block {
            Block::List(self.build())
        }
    }

    struct ListItemBuilder {
        level: usize,
        runs: Vec<TextRun>,
    }

    impl ListItemBuilder {
        fn new() -> Self {
            Self {
                level: 0,
                runs: Vec::new(),
            }
        }

        fn with(mut self, text: TextRun) -> Self {
            self.runs.push(text);
            self
        }
    }

    impl Into<ListItem> for ListItemBuilder {
        fn into(self) -> ListItem {
            ListItem {
                level: self.level,
                runs: self.runs.into_boxed_slice(),
            }
        }
    }

    struct ContainerBuilder {
        content: Vec<ContainedBlock>,
        kind: ContainerKind,
    }

    impl ContainerBuilder {
        fn new(kind: ContainerKind) -> Self {
            ContainerBuilder {
                content: Vec::new(),
                kind,
            }
        }

        fn with<T: Into<ContainedBlock>>(mut self, block: T) -> Self {
            self.content.push(block.into());
            self
        }
    }

    impl Into<Container> for ContainerBuilder {
        fn into(self) -> Container {
            Container {
                content: self.content.into_boxed_slice(),
                kind: self.kind,
            }
        }
    }

    //TODO: Reduce test verbosity some more, e.g assert_fails, given().expect().
    //TODO: See if we can group / order these ever growing tests...

    #[test]
    fn complete_doc_test() {
        let input = concat!(
            "#metadata\n",
            "id: 01.42\n",
            "title: Feline friendly flower arranging\n",
            "\n",
            "#[info]\n",
            "Did you know flower pots are for *more*\n",
            "than simply knocking on the floor?\n",
            "#=\n",
            "\n",
            "Opposable thumbs\n",
            "are useful?"
        );

        let expected = document()
            .with_metadata(
                metadata()
                    .with_id("01.42")
                    .with_title("Feline friendly flower arranging"),
            )
            .with_container(
                info().with(
                    paragraph()
                        .with_run("Did you know flower pots are for ")
                        .with_strong_run("more")
                        .with_run(" than simply knocking on the floor?"),
                ),
            )
            .with_block(paragraph().with_run("Opposable thumbs are useful?"))
            .build();

        let actual = parse_str(input).unwrap();

        assert_eq!(actual, expected);
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
    fn explicit_paragraph() {
        let input = "#paragraph\nCats go meeow!";

        let expected = document()
            .with_block(paragraph().with_run("Cats go meeow!"))
            .build();

        let actual = parse_str(input).unwrap();

        assert_eq!(actual, expected);
    }

    #[test]
    fn unknown_block_is_rejected() {
        let input = "#meowograph\nCats go meeow!";

        let expected = Err(ParseError::UnknownBlock);

        let actual = parse_str(input);

        assert_eq!(actual, expected);
    }

    #[test]
    fn empty_block_name_is_rejected() {
        let input = "#\nHi";

        let expected = Err(ParseError::UnknownBlock);

        let actual = parse_str(input);

        assert_eq!(actual, expected);
    }

    #[test]
    fn block_header_without_newline_is_rejected() {
        let input = "#paragraph";

        let expected = Err(ParseError::UnexpectedInput);

        let actual = parse_str(input);

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
    fn new_line_with_extra_whitespace_collapses() {
        let input = "Cats    \n    whiskers";
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
    fn two_new_lines_with_whitespace_is_treated_as_blockbreak() {
        let input = "Cats\n \nwhiskers";
        let expected = document()
            .with_block(paragraph().with_run("Cats"))
            .with_block(paragraph().with_run("whiskers"))
            .build();

        let actual = parse_str(input).unwrap();

        assert_eq!(actual, expected);
    }
    #[test]
    fn blockbreak_with_extra_whitespace() {
        let input = "Cats  \n    \n  whiskers";
        let expected = document()
            .with_block(paragraph().with_run("Cats"))
            .with_block(paragraph().with_run("whiskers"))
            .build();

        let actual = parse_str(input).unwrap();

        assert_eq!(actual, expected);
    }

    #[test]
    fn missing_blockbreak_is_rejected() {
        let input = concat!(
            "Cats can sometimes be\n",
            "#paragraph\n",
            "ever so surprising\n"
        );

        let expected = Err(ParseError::UnexpectedInput);

        let actual = parse_str(input);
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
    fn emphasis_at_end_of_line() {
        let input = "Cats like to _zoom_\naround";

        let expected = document()
            .with_block(
                paragraph()
                    .with_run("Cats like to ")
                    .with_emphasised_run("zoom")
                    .with_run(" around"),
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
    fn strikethrough_over_two_lines() {
        let input = "~Great\ndogs~";

        let expected = document()
            .with_block(paragraph().with_strikethrough_run("Great dogs"))
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
            .with_block(paragraph().with_raw_run("Cat cat"))
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
    fn strikethrough_with_double_linebreak() {
        let input = "~Erm...\n\nmeow?~";

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
    fn doc_with_leading_spaces_and_new_line() {
        let input = "   \nCats cats cats";

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

    #[test]
    fn doc_metadata() {
        let input = concat!(
            "#metadata\n",
            "id: 01.23\n",
            "title: Practical espionage for felines\n",
        );

        let expected = document()
            .with_metadata(
                metadata()
                    .with_id("01.23")
                    .with_title("Practical espionage for felines"),
            )
            .build();

        let actual = parse_str(input).unwrap();

        assert_eq!(actual, expected);
    }

    #[test]
    fn doc_metadata_with_alternate_spacing() {
        let input = concat!("#metadata\n", "id :01.23\n",);

        let expected = document()
            .with_metadata(metadata().with_id("01.23"))
            .build();

        let actual = parse_str(input).unwrap();

        assert_eq!(actual, expected);
    }

    #[test]
    fn metadata_not_at_start_is_rejected() {
        let input = concat!(
            "Hi!\n",
            "\n",
            "#metadata\n",
            "id: 01.23\n",
            "title: Some document\n",
        );

        let expected = Err(ParseError::MetadataNotAtStart);

        let actual = parse_str(input);

        assert_eq!(actual, expected);
    }

    //TODO: Pretty varient (e.g #== [ info ] ==)?
    #[test]
    fn multi_paragraph_info() {
        let input = concat!(
            "#[info]\n",
            "Here are some facts...\n",
            "\n",
            "...about the cats!\n",
            "#="
        );
        //TODO: Maybe just a lone '#' would be prettier?

        let expected = document()
            .with_container(
                info()
                    .with(paragraph().with_run("Here are some facts..."))
                    .with(paragraph().with_run("...about the cats!")),
            )
            .build();

        let actual = parse_str(input).unwrap();
        assert_eq!(actual, expected);
    }

    //TODO: We could allow this after all?
    #[test]
    fn unterminated_container_is_rejected() {
        let input = concat!(
            "#[info]\n",
            "Did you know that cats sometimes like to ...\n",
            "\n",
        );

        let expected = Err(ParseError::UnterminatedContainer);

        let actual = parse_str(input);
        assert_eq!(actual, expected);
    }

    #[test]
    fn container_missing_start_is_rejected() {
        let input = concat!("Silly cat\n", "#=");

        let expected = Err(ParseError::UnexpectedInput);

        let actual = parse_str(input);
        assert_eq!(actual, expected);
    }

    #[test]
    fn only_container_header_is_rejected() {
        let input = concat!("#[info]\n",);

        let expected = Err(ParseError::UnexpectedInput);

        let actual = parse_str(input);
        assert_eq!(actual, expected);
    }

    #[test]
    fn trailing_text_on_container_footer_is_rejected() {
        let input = concat!(
            "#[info]\n",
            "Let me know if you find where I left my\n",
            "#=toy"
        );

        let expected = Err(ParseError::UnexpectedInput);

        let actual = parse_str(input);
        assert_eq!(actual, expected);
    }

    #[test]
    #[ignore]
    fn simple_list() {
        let input = concat!(
            "- Dry food is ok\n",
            "- Wet food is much better\n",
            "- Water is important also\n"
        );

        let expected = document()
            .with_block(
                list()
                    .with(list_item().with(text("Dry food is ok")))
                    .with(list_item().with(text("Wet food is much better")))
                    .with(list_item().with(text("Water is important also"))),
            )
            .build();

        let actual = parse_str(input).unwrap();
        assert_eq!(actual, expected);
    }
}
