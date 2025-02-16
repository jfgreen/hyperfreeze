use std::fmt::Display;

use crate::scan::{Scanner, StyleDelimiter, Token};

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

#[derive(PartialEq, Eq, Debug)]
pub enum Block {
    Paragraph(Box<[TextRun]>),
    List(Box<[ListItem]>),
}

#[derive(PartialEq, Eq, Debug)]
pub struct Container {
    pub content: Box<[Block]>,
    pub kind: ContainerKind,
}

#[derive(PartialEq, Eq, Debug)]
pub enum ContainerKind {
    Info,
    //TODO: Other kinds of alert
}

//TODO: Is this the best way to represent a list, what would be easy to render?
//TODO: Could have a list item be an enum with sublist as a varient?
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

#[derive(PartialEq, Eq, Debug, Clone, Copy)]
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
            Token::ContainerHeader(_) => {
                let container = parse_container(scanner)?;
                let element = Element::Container(container);
                elements.push(element);
            }
            Token::EndOfFile => break,
            _ => {
                let block = parse_block(scanner)?;
                let element = Element::Block(block);
                elements.push(element);
            }
        }
    }

    Ok(Document {
        metadata,
        contents: elements.into_boxed_slice(),
    })
}

fn parse_container(scanner: &mut Scanner) -> ParseResult<Container> {
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
            Token::EndOfFile => {
                return Err(ParseError::UnterminatedContainer);
            }
            _ => {
                let block = parse_block(scanner)?;
                blocks.push(block);
            }
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

    Ok(container)
}

fn container_kind_from_name(name: &str) -> ParseResult<ContainerKind> {
    match name {
        "info" => Ok(ContainerKind::Info),
        _ => Err(ParseError::UnknownContainer),
    }
}

fn parse_block(scanner: &mut Scanner) -> ParseResult<Block> {
    match scanner.peek() {
        Token::BlockHeader(block_name) => {
            scanner.next();
            eat_linebreak(scanner)?;
            let block = match block_name {
                "metadata" => return Err(ParseError::MetadataNotAtStart),
                "paragraph" => parse_paragraph(scanner)?,
                _ => return Err(ParseError::UnknownBlock),
            };
            Ok(block)
        }
        Token::Text(_) | Token::StyleDelimiter(_) | Token::InlineRawDelimiter => {
            // Infer paragraph missing header (a valid sugar)
            let element = parse_paragraph(scanner)?;
            Ok(element)
        }
        _ => Err(ParseError::UnexpectedInput),
    }
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

//TODO: Is there a nicer abstraction for this? NewType pattern?
fn push_run(text_runs: &mut Vec<TextRun>, run: &mut String, style: Style) -> ParseResult<()> {
    // TODO: Not sure if this belongs here.. check for style is sus
    if run.is_empty() && style != Style::None {
        return Err(ParseError::EmptyDelimitedText);
    }

    // TODO: Empty check feels like a bit of a hack?
    if !run.is_empty() {
        let completed_run = std::mem::take(run);
        text_runs.push(TextRun {
            text: completed_run,
            style,
        });
    }

    Ok(())
}

fn style_from_delimiter(delimiter: StyleDelimiter) -> Style {
    match delimiter {
        StyleDelimiter::Strong => Style::Strong,
        StyleDelimiter::Emphasis => Style::Emphasis,
        StyleDelimiter::Strikethrough => Style::Strikethrough,
    }
}

fn parse_paragraph(scanner: &mut Scanner) -> ParseResult<Block> {
    let mut text_runs = Vec::new();
    scanner.push_context_paragraph();
    let mut run = String::new();
    let mut pending_linebreak = false;

    loop {
        match scanner.peek() {
            Token::StyleDelimiter(d1) => {
                push_run(&mut text_runs, &mut run, Style::None)?;
                let style = style_from_delimiter(d1);
                scanner.next();

                'styled_text: loop {
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
                        Token::StyleDelimiter(d2) if d1 == d2 => {
                            break 'styled_text;
                        }
                        Token::Linebreak => {
                            run.push(SPACE);
                        }
                        _ => return Err(ParseError::UnexpectedInput),
                    }
                }

                if run.starts_with(SPACE) || run.ends_with(SPACE) {
                    return Err(ParseError::LooseDelimiter);
                }

                push_run(&mut text_runs, &mut run, style)?;
            }
            Token::InlineRawDelimiter => {
                push_run(&mut text_runs, &mut run, Style::None)?;
                scanner.push_context_inline_raw();
                scanner.next();
                'inline_raw: loop {
                    match scanner.next() {
                        Token::InlineRawDelimiter => {
                            break 'inline_raw;
                        }
                        Token::RawFragment(text) => {
                            run.push_str(text);
                        }
                        Token::Linebreak => {
                            run.push(SPACE);
                        }
                        _ => return Err(ParseError::UnexpectedInput),
                    }
                }
                push_run(&mut text_runs, &mut run, Style::Raw)?;
                scanner.pop_context();
            }
            Token::Text(text) => {
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
                pending_linebreak = true;
            }
            Token::Blockbreak => {
                scanner.next();
                push_run(&mut text_runs, &mut run, Style::None)?;
                break;
            }
            Token::EndOfFile | Token::ContainerFooter => {
                push_run(&mut text_runs, &mut run, Style::None)?;
                break;
            }
            _ => return Err(ParseError::UnexpectedInput),
        }
    }

    scanner.pop_context();
    let para = text_runs.into_boxed_slice();
    Ok(Block::Paragraph(para))
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

        fn with<T: Into<TextRun>>(mut self, text: T) -> Self {
            self.text_runs.push(text.into());
            self
        }

        fn build(self) -> Box<[TextRun]> {
            self.text_runs.into_boxed_slice()
        }
    }

    impl Into<Block> for ParagraphBuilder {
        fn into(self) -> Block {
            Block::Paragraph(self.build())
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

        fn at_sub_level(mut self, level: usize) -> Self {
            self.level = level;
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
        content: Vec<Block>,
        kind: ContainerKind,
    }

    impl ContainerBuilder {
        fn new(kind: ContainerKind) -> Self {
            ContainerBuilder {
                content: Vec::new(),
                kind,
            }
        }

        fn with<T: Into<Block>>(mut self, block: T) -> Self {
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
                        .with(text("Did you know flower pots are for "))
                        .with(strong_text("more"))
                        .with(text(" than simply knocking on the floor?")),
                ),
            )
            .with_block(paragraph().with(text("Opposable thumbs are useful?")))
            .build();

        let actual = parse_str(input).unwrap();

        assert_eq!(actual, expected);
    }

    #[test]
    fn one_line_paragraph() {
        let input = "We like cats very much";

        let expected = document()
            .with_block(paragraph().with(text("We like cats very much")))
            .build();

        let actual = parse_str(input).unwrap();

        assert_eq!(actual, expected);
    }

    #[test]
    fn explicit_paragraph() {
        let input = "#paragraph\nCats go meeow!";

        let expected = document()
            .with_block(paragraph().with(text("Cats go meeow!")))
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
            .with_block(paragraph().with(text("Nice kitty!")))
            .build();

        let actual = parse_str(input).unwrap();

        assert_eq!(actual, expected);
    }

    #[test]
    fn new_line_becomes_whitespace() {
        let input = "Cats\nwhiskers";

        let expected = document()
            .with_block(paragraph().with(text("Cats whiskers")))
            .build();

        let actual = parse_str(input).unwrap();

        assert_eq!(actual, expected);
    }
    #[test]
    fn new_line_with_extra_whitespace_collapses() {
        let input = "Cats    \n    whiskers";
        let expected = document()
            .with_block(paragraph().with(text("Cats whiskers")))
            .build();

        let actual = parse_str(input).unwrap();

        assert_eq!(actual, expected);
    }

    #[test]
    fn two_new_lines_become_blocks() {
        let input = "Cats\n\nwhiskers";

        let expected = document()
            .with_block(paragraph().with(text("Cats")))
            .with_block(paragraph().with(text("whiskers")))
            .build();

        let actual = parse_str(input).unwrap();

        assert_eq!(actual, expected);
    }

    #[test]
    fn three_new_lines_becomes_blocks() {
        let input = "Cats\n\n\nwhiskers";

        let expected = document()
            .with_block(paragraph().with(text("Cats")))
            .with_block(paragraph().with(text("whiskers")))
            .build();

        let actual = parse_str(input).unwrap();

        assert_eq!(actual, expected);
    }

    #[test]
    fn two_new_lines_with_whitespace_is_treated_as_blockbreak() {
        let input = "Cats\n \nwhiskers";
        let expected = document()
            .with_block(paragraph().with(text("Cats")))
            .with_block(paragraph().with(text("whiskers")))
            .build();

        let actual = parse_str(input).unwrap();

        assert_eq!(actual, expected);
    }
    #[test]
    fn blockbreak_with_extra_whitespace() {
        let input = "Cats  \n    \n  whiskers";
        let expected = document()
            .with_block(paragraph().with(text("Cats")))
            .with_block(paragraph().with(text("whiskers")))
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
            .with_block(paragraph().with(text("My cat does backflips #coolcat")))
            .build();

        let actual = parse_str(input).unwrap();

        assert_eq!(actual, expected);
    }

    #[test]
    fn escaped_underscore() {
        let input = "cat\\_case";

        let expected = document()
            .with_block(paragraph().with(text("cat_case")))
            .build();

        let actual = parse_str(input).unwrap();

        assert_eq!(actual, expected);
    }

    #[test]
    fn escaped_underscore_in_emphasis() {
        let input = "_cat\\_case_";

        let expected = document()
            .with_block(paragraph().with(emphasised_text("cat_case")))
            .build();

        let actual = parse_str(input).unwrap();

        assert_eq!(actual, expected);
    }

    #[test]
    fn escaped_ignored_in_raw() {
        let input = "`cat\\_case`";

        let expected = document()
            .with_block(paragraph().with(raw_text("cat\\_case")))
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
                    .with(text("We "))
                    .with(emphasised_text("totally adore"))
                    .with(text(" them")),
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
                    .with(text("Cats like to "))
                    .with(emphasised_text("zoom"))
                    .with(text(" around")),
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
                    .with(text("I "))
                    .with(strong_text("need to pet that cat"))
                    .with(text(" right away.")),
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
                    .with(text("I said: mee"))
                    .with(strong_text("ooOOo"))
                    .with(text("ww!")),
            )
            .build();

        let actual = parse_str(input).unwrap();

        assert_eq!(actual, expected);
    }

    #[test]
    fn strong_over_two_lines() {
        let input = "*me\now*";

        let expected = document()
            .with_block(paragraph().with(strong_text("me ow")))
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
                    .with(text("Cats are "))
                    .with(strikethrough_text("ok i guess"))
                    .with(text(" magnificant")),
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
                    .with(text("Robot cat says "))
                    .with(raw_text("bleep bloop"))
                    .with(text("!")),
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
                    .with(text("Bl"))
                    .with(raw_text("eeee"))
                    .with(text("p!")),
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
                    .with(text("Set "))
                    .with(raw_text("PURR_LOUDLY"))
                    .with(text(" to true")),
            )
            .build();

        let actual = parse_str(input).unwrap();

        assert_eq!(actual, expected);
    }

    #[test]
    fn extra_spaces_in_raw() {
        let input = "`Keep your       distance`";

        let expected = document()
            .with_block(paragraph().with(raw_text("Keep your       distance")))
            .build();

        let actual = parse_str(input).unwrap();

        assert_eq!(actual, expected);
    }

    #[test]
    fn raw_over_two_lines() {
        let input = "`Great\ncats`";

        let expected = document()
            .with_block(paragraph().with(raw_text("Great cats")))
            .build();

        let actual = parse_str(input).unwrap();

        assert_eq!(actual, expected);
    }

    #[test]
    fn strikethrough_over_two_lines() {
        let input = "~Great\ndogs~";

        let expected = document()
            .with_block(paragraph().with(strikethrough_text("Great dogs")))
            .build();

        let actual = parse_str(input).unwrap();

        assert_eq!(actual, expected);
    }

    #[test]
    fn raw_leading_with_new_line() {
        let input = "`\nMeow?`";

        let expected = document()
            .with_block(paragraph().with(raw_text(" Meow?")))
            .build();

        let actual = parse_str(input).unwrap();

        assert_eq!(actual, expected);
    }

    #[test]
    fn raw_trailing_with_new_line() {
        let input = "`Meow\n`";

        let expected = document()
            .with_block(paragraph().with(raw_text("Meow ")))
            .build();

        let actual = parse_str(input).unwrap();

        assert_eq!(actual, expected);
    }

    #[test]
    fn raw_leading_with_space() {
        let input = "` Meow`";

        let expected = document()
            .with_block(paragraph().with(raw_text(" Meow")))
            .build();

        let actual = parse_str(input).unwrap();

        assert_eq!(actual, expected);
    }

    #[test]
    fn raw_trailing_with_space() {
        let input = "`Meow `";

        let expected = document()
            .with_block(paragraph().with(raw_text("Meow ")))
            .build();

        let actual = parse_str(input).unwrap();

        assert_eq!(actual, expected);
    }

    #[test]
    fn raw_over_three_lines() {
        let input = "`Great\ncats\nassemble!`";

        let expected = document()
            .with_block(paragraph().with(raw_text("Great cats assemble!")))
            .build();

        let actual = parse_str(input).unwrap();

        assert_eq!(actual, expected);
    }

    #[test]
    fn standalone_dash() {
        let input = "Felines - fantastic!";

        let expected = document()
            .with_block(paragraph().with(text("Felines - fantastic!")))
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
                    .with(text("Cat cat"))
                    .with(emphasised_text("cat cat"))
                    .with(text(" cat.")),
            )
            .build();

        let actual = parse_str(input).unwrap();

        assert_eq!(actual, expected);
    }

    #[test]
    fn newline_then_multiple_spaces_in_plain_text() {
        let input = "Cat\n  cat";

        let expected = document()
            .with_block(paragraph().with(text("Cat cat")))
            .build();

        let actual = parse_str(input).unwrap();

        assert_eq!(actual, expected);
    }

    #[test]
    fn newline_then_multiple_spaces_in_styled() {
        let input = "*Cat\n  cat*";

        let expected = document()
            .with_block(paragraph().with(strong_text("Cat cat")))
            .build();

        let actual = parse_str(input).unwrap();

        assert_eq!(actual, expected);
    }

    #[test]
    fn newline_then_multiple_spaces_in_raw() {
        let input = "`Cat\n  cat`";

        let expected = document()
            .with_block(paragraph().with(raw_text("Cat cat")))
            .build();

        let actual = parse_str(input).unwrap();

        assert_eq!(actual, expected);
    }

    #[test]
    fn multiple_spaces_then_newline_in_plain_text() {
        let input = "Cat  \ncat";

        let expected = document()
            .with_block(paragraph().with(text("Cat cat")))
            .build();

        let actual = parse_str(input).unwrap();

        assert_eq!(actual, expected);
    }

    #[test]
    fn multiple_spaces_then_newline_in_styled() {
        let input = "*Cat  \ncat*";

        let expected = document()
            .with_block(paragraph().with(strong_text("Cat cat")))
            .build();

        let actual = parse_str(input).unwrap();

        assert_eq!(actual, expected);
    }

    #[test]
    fn multiple_spaces_then_newline_in_raw() {
        let input = "`Cat  \ncat`";

        let expected = document()
            .with_block(paragraph().with(raw_text("Cat   cat")))
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

        let expected = Err(ParseError::UnexpectedInput);

        let actual = parse_str(input);

        assert_eq!(actual, expected);
    }

    #[test]
    fn strikethrough_with_double_linebreak() {
        let input = "~Erm...\n\nmeow?~";

        let expected = Err(ParseError::UnexpectedInput);

        let actual = parse_str(input);

        assert_eq!(actual, expected);
    }

    #[test]
    fn unmatched_emphasis_1() {
        let input = "_.";

        let expected = Err(ParseError::UnexpectedInput);

        let actual = parse_str(input);

        assert_eq!(actual, expected);
    }

    #[test]
    fn unmatched_emphasis_2() {
        let input = "meow _meow.";

        let expected = Err(ParseError::UnexpectedInput);

        let actual = parse_str(input);

        assert_eq!(actual, expected);
    }

    #[test]
    fn unmatched_emphasis_3() {
        let input = "meow meow_";

        let expected = Err(ParseError::UnexpectedInput);

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
            .with_block(paragraph().with(text("Cats cats cats")))
            .build();

        let actual = parse_str(input).unwrap();

        assert_eq!(actual, expected);
    }

    #[test]
    fn doc_with_leading_new_lines() {
        let input = "\n\nCats cats cats";

        let expected = document()
            .with_block(paragraph().with(text("Cats cats cats")))
            .build();

        let actual = parse_str(input).unwrap();

        assert_eq!(actual, expected);
    }

    #[test]
    fn doc_with_leading_spaces_and_new_line() {
        let input = "   \nCats cats cats";

        let expected = document()
            .with_block(paragraph().with(text("Cats cats cats")))
            .build();

        let actual = parse_str(input).unwrap();

        assert_eq!(actual, expected);
    }

    #[test]
    fn doc_ending_with_new_line() {
        let input = "Cats are friends\n";

        let expected = document()
            .with_block(paragraph().with(text("Cats are friends")))
            .build();

        let actual = parse_str(input).unwrap();

        assert_eq!(actual, expected);
    }

    #[test]
    fn doc_ending_with_new_lines() {
        let input = "Feline friends\n\n";

        let expected = document()
            .with_block(paragraph().with(text("Feline friends")))
            .build();

        let actual = parse_str(input).unwrap();

        assert_eq!(actual, expected);
    }

    #[test]
    fn new_line_and_space_between_styled_and_plain_text_runs() {
        let input = "*Cat*\n cat";

        let expected = document()
            .with_block(paragraph().with(strong_text("Cat")).with(text(" cat")))
            .build();

        let actual = parse_str(input).unwrap();

        assert_eq!(actual, expected);
    }

    #[test]
    fn leading_whitespace_on_paragraph_is_ignored() {
        let input = "Cat\n\n  cat";

        let expected = document()
            .with_block(paragraph().with(text("Cat")))
            .with_block(paragraph().with(text("cat")))
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
                    .with(paragraph().with(text("Here are some facts...")))
                    .with(paragraph().with(text("...about the cats!"))),
            )
            .build();

        let actual = parse_str(input).unwrap();
        assert_eq!(actual, expected);
    }

    //TODO: We could allow this after all?
    //Challenge: we would not know untill the end of the doc
    // (or start of next container) what is supposed to be inside
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

    //TODO: Would be cool to optionaly give lists a title

    #[test]
    #[ignore]
    fn list_with_sublist() {
        let input = concat!(
            "- Nice things to eat\n",
            "  - Tuna\n",
            "  - Chicken\n",
            "  - Beef\n",
        );

        let expected = document()
            .with_block(
                list()
                    .with(list_item().with(text("Nice things to eat")))
                    .with(list_item().at_sub_level(1).with(text("Tuna")))
                    .with(list_item().at_sub_level(1).with(text("Chicken")))
                    .with(list_item().at_sub_level(1).with(text("Beef"))),
            )
            .build();

        let actual = parse_str(input).unwrap();
        assert_eq!(actual, expected);
    }
}
