use std::fmt::Display;
use std::str::CharIndices;

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

#[derive(PartialEq, Eq, Debug)]
pub enum ListItem {
    Text(Box<[TextRun]>),
    SubList(Box<[ListItem]>),
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

//TODO: What if we leant more into using unicode chars like '•' or '¶'
// alt-gr < is •
// alt-gr r is ¶
// is there anything cool we could use for containers?

const SPACE: char = ' ';
const NEW_LINE: char = '\n';
const COLON: char = ':';
const METADATA_HEADER: &str = "#metadata";
const CONTAINER_HEADER_START: &str = "#[";
const CONTAINER_FOOTER: &str = "#="; //TODO: CONTAINER_FOOTER_START
const HASH: char = '#';
const LEFT_SQUARE_BRACKET: char = '[';
const RIGHT_SQUARE_BRACKET: char = ']';
const BACKTICK: char = '`';
const ASTERISK: char = '*';
const TILDE: char = '~';
const UNDERSCORE: char = '_';
const BACKSLASH: char = '\\';
const DASH: char = '-';
const WHITESPACE_CHARS: &[char; 2] = &[SPACE, NEW_LINE];
const STYLE_DELIMITER_CHARS: &[char; 3] = &[ASTERISK, TILDE, UNDERSCORE];

fn char_usable_in_identifier(c: char) -> bool {
    c.is_alphanumeric()
}

fn char_usable_in_text_frag(c: char) -> bool {
    ![
        UNDERSCORE, BACKTICK, ASTERISK, TILDE, SPACE, NEW_LINE, HASH, BACKSLASH,
    ]
    .contains(&c)
}

fn char_usable_in_para_frag(c: char) -> bool {
    ![NEW_LINE, HASH, SPACE].contains(&c)
}

fn char_usable_in_raw_frag(c: char) -> bool {
    ![BACKTICK, SPACE, NEW_LINE].contains(&c)
}

pub fn parse_str(input: &str) -> ParseResult<Document> {
    let scanner = &mut Scanner::new(input);
    let mut elements = Vec::new();

    let mut metadata = Metadata::default();

    scanner.skip_while_on_any(WHITESPACE_CHARS);

    if scanner.is_on_str(METADATA_HEADER) {
        metadata = parse_metadata_block(scanner)?;
    }

    while scanner.has_input() {
        if scanner.is_on_any(WHITESPACE_CHARS) {
            scanner.skip_char();
        } else if scanner.is_on_str(CONTAINER_HEADER_START) {
            let container = parse_container(scanner)?;
            let element = Element::Container(container);
            elements.push(element);
        } else {
            let block = parse_block(scanner)?;
            let element = Element::Block(block);
            elements.push(element);
        }
    }

    Ok(Document {
        metadata,
        contents: elements.into_boxed_slice(),
    })
}

fn parse_metadata_block(scanner: &mut Scanner) -> ParseResult<Metadata> {
    scanner.expect_str(METADATA_HEADER)?;
    scanner.expect_char(NEW_LINE)?;

    let mut metadata = Metadata::default();

    loop {
        let key = scanner.eat_while(char_usable_in_identifier)?;
        scanner.skip_while_on_char(SPACE);
        scanner.expect_char(COLON)?;
        scanner.skip_while_on_char(SPACE);

        // For now, the value is just everything untill the end of line
        // This might get more complicated in the future
        // e.g treating value as int, bool, list, etc?

        let value = scanner.eat_until_char(NEW_LINE)?;

        match key {
            "id" => metadata.id.push_str(value),
            "title" => metadata.title.push_str(value),
            _ => return Err(ParseError::UnknownMetadata),
        };

        if scanner.has_input() {
            let mut peek = scanner.peek();
            peek.expect_char(NEW_LINE)?;
            if peek.is_on(char_usable_in_identifier) {
                scanner.advance_to(&peek)
            } else {
                break;
            }
        }
    }

    Ok(metadata)
}

fn parse_container(scanner: &mut Scanner) -> ParseResult<Container> {
    scanner.expect_char(HASH)?;
    scanner.expect_char(LEFT_SQUARE_BRACKET)?;
    let container_name = scanner.eat_while(char::is_alphanumeric)?;
    scanner.expect_char(RIGHT_SQUARE_BRACKET)?;
    scanner.expect_char(NEW_LINE)?;

    let mut blocks = Vec::new();

    loop {
        if scanner.is_on_str(CONTAINER_FOOTER) {
            scanner.skip_char();
            scanner.skip_char(); //TODO: eat while '='
            break;
        } else if !scanner.has_input() {
            return Err(ParseError::UnterminatedContainer);
        } else if scanner.is_on_any(WHITESPACE_CHARS) {
            scanner.skip_char();
        } else {
            let block = parse_block(scanner)?;
            blocks.push(block);
        }
    }

    if scanner.has_input() {
        scanner.expect_char(NEW_LINE)?;
    }

    let container = Container {
        content: blocks.into_boxed_slice(),
        kind: container_kind_from_name(container_name)?,
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
    let block = if scanner.is_on_char(HASH) {
        scanner.skip_char();
        let block_name = scanner.eat_while(char_usable_in_identifier)?;
        scanner.expect_char(NEW_LINE)?;

        match block_name {
            "metadata" => Err(ParseError::MetadataNotAtStart),
            "paragraph" => parse_paragraph(scanner),
            _ => Err(ParseError::UnknownBlock),
        }
    } else if scanner.is_on_char(DASH) {
        parse_list(scanner)
    } else {
        parse_paragraph(scanner)
    }?;

    if scanner.has_input() {
        scanner.skip_while_on_char(SPACE);
        scanner.expect_char(NEW_LINE)?;
    }

    if scanner.has_input() && !scanner.is_on_str(CONTAINER_FOOTER) {
        scanner.skip_while_on_char(SPACE);
        scanner.expect_char(NEW_LINE)?;
    }

    Ok(block)
}

fn parse_paragraph(scanner: &mut Scanner) -> ParseResult<Block> {
    let mut text_runs = Vec::new();

    if scanner.is_on_any(WHITESPACE_CHARS) {
        return Err(ParseError::UnexpectedInput);
    }

    while on_text_run(scanner) {
        let run = parse_text_run(scanner)?;
        text_runs.push(run);
    }

    let para = text_runs.into_boxed_slice();
    Ok(Block::Paragraph(para))
}

fn parse_text_run(scanner: &mut Scanner) -> ParseResult<TextRun> {
    if scanner.is_on_any(STYLE_DELIMITER_CHARS) {
        parse_styled_text_run(scanner)
    } else if scanner.is_on_char(BACKTICK) {
        parse_inline_raw_text_run(scanner)
    } else {
        parse_plain_text_run(scanner)
    }
}

fn parse_list(scanner: &mut Scanner) -> ParseResult<Block> {
    let mut items = Vec::new();

    // IDEA: Parsing a list item is just like parsing a para
    // but instead of only stopping once we get to a block break
    // or container footer, we also stop when we get to a line that
    // starts with bullet (or space then bullet)

    //TODO: "while on_list_item(scanner)"
    loop {
        if scanner.is_on_char(DASH) {
            scanner.skip_char();
            scanner.skip_while_on_char(SPACE);
            let mut text_runs = Vec::new();

            while on_text_run(scanner) {
                let run = parse_text_run(scanner)?;
                text_runs.push(run);
            }

            let item = ListItem::Text(text_runs.into_boxed_slice());
            items.push(item);

            //TODO: should we only eat this if next line has a list item
            if scanner.is_on_char(NEW_LINE) {
                scanner.skip_char();
            }
        } else {
            break;
        }
    }

    let list = items.into_boxed_slice();
    Ok(Block::List(list))
}

fn parse_plain_text_run(scanner: &mut Scanner) -> ParseResult<TextRun> {
    let mut run = String::new();

    loop {
        if scanner.is_on_char(BACKSLASH) {
            scanner.skip_char();
            let escaped = scanner.eat_char()?;
            run.push(escaped);
        } else if scanner.is_on(char_usable_in_text_frag) {
            let text = scanner.eat_while(char_usable_in_text_frag)?;
            run.push_str(text);
        } else if let Some(position) = try_eat_text_space(scanner) {
            scanner.advance_to(&position);
            run.push(SPACE);
        } else {
            break;
        }
    }

    let run = TextRun {
        text: run,
        style: Style::None,
    };

    Ok(run)
}

fn parse_styled_text_run(scanner: &mut Scanner) -> ParseResult<TextRun> {
    let delimiter = scanner.eat_char()?;
    let style = style_from_delimiter(delimiter)?;
    let mut run = String::new();

    loop {
        if scanner.is_on_char(BACKSLASH) {
            scanner.skip_char();
            let escaped = scanner.eat_char()?;
            run.push(escaped);
        } else if scanner.is_on(char_usable_in_text_frag) {
            let text = scanner.eat_while(char_usable_in_text_frag)?;
            run.push_str(text);
        } else if scanner.is_on_char(delimiter) {
            scanner.skip_char();
            break;
        } else if let Some(position) = try_eat_text_space(scanner) {
            scanner.advance_to(&position);
            run.push(SPACE);
        } else {
            return Err(ParseError::UnexpectedInput);
        }
    }

    if run.starts_with(SPACE) || run.ends_with(SPACE) {
        return Err(ParseError::LooseDelimiter);
    }

    if run.is_empty() {
        return Err(ParseError::EmptyDelimitedText);
    }

    Ok(TextRun { text: run, style })
}

fn on_text_run(scanner: &Scanner) -> bool {
    scanner.is_on(char_usable_in_para_frag) || try_eat_text_space(scanner).is_some()
}

fn try_eat_text_space<'a>(scanner: &Scanner<'a>) -> Option<Scanner<'a>> {
    if scanner.is_on_any(WHITESPACE_CHARS) {
        let mut peek = scanner.peek();
        let mut has_new_line = false;

        //FIXME: this wont accept a dash after a new line in a regular para
        //... and it should. Is generally a bit of a mess also...

        peek.skip_while_on_char(SPACE);
        if peek.is_on_char(NEW_LINE) {
            has_new_line = true;
            peek.skip_char();
        }
        peek.skip_while_on_char(SPACE);

        if has_new_line && peek.is_on_char(DASH) {
            return None;
        }

        if peek.is_on(char_usable_in_para_frag) {
            return Some(peek);
        }
    }

    None
}

fn style_from_delimiter(delimiter: char) -> ParseResult<Style> {
    match delimiter {
        ASTERISK => Ok(Style::Strong),
        UNDERSCORE => Ok(Style::Emphasis),
        TILDE => Ok(Style::Strikethrough),
        _ => Err(ParseError::UnexpectedInput),
    }
}

fn parse_inline_raw_text_run(scanner: &mut Scanner) -> ParseResult<TextRun> {
    let mut run = String::new();
    scanner.expect_char(BACKTICK)?;

    loop {
        if scanner.is_on_char(BACKTICK) {
            scanner.skip_char();
            break;
        } else if scanner.is_on_char(NEW_LINE) {
            scanner.skip_char();

            if scanner.is_on_char(NEW_LINE) {
                return Err(ParseError::UnexpectedInput);
            } else {
                run.push(SPACE);
            }
        } else if scanner.is_on_char(SPACE) {
            let space = scanner.eat_while_char(SPACE)?;
            run.push_str(space);
        } else if scanner.is_on(char_usable_in_raw_frag) {
            let text = scanner.eat_while(char_usable_in_raw_frag)?;
            run.push_str(text);
        } else {
            return Err(ParseError::UnexpectedInput);
        }
    }

    if run.is_empty() {
        return Err(ParseError::EmptyDelimitedText);
    }

    Ok(TextRun {
        text: run,
        style: Style::Raw,
    })
}

#[derive(Debug)]
struct Scanner<'a> {
    input: &'a str,
    chars: CharIndices<'a>,
    current_char: Option<char>,
    current_index: usize,
}

impl<'a> Scanner<'a> {
    fn new(input: &'a str) -> Self {
        let mut scanner = Self {
            input,
            chars: input.char_indices(),
            current_char: None,
            current_index: 0,
        };

        // Place the first char of the input into `next`
        scanner.read_next_char();

        scanner
    }

    fn has_input(&mut self) -> bool {
        self.current_char.is_some()
    }

    fn is_on(&self, predicate: impl Fn(char) -> bool) -> bool {
        self.current_char.is_some_and(&predicate)
    }

    fn is_on_str(&self, s: &str) -> bool {
        self.input[self.current_index..].starts_with(s)
    }

    fn is_on_char(&self, c: char) -> bool {
        self.current_char == Some(c)
    }

    //TODO: rename to is_on_one_of? see how used...
    fn is_on_any(&self, chars: &[char]) -> bool {
        self.current_char.is_some_and(|c| chars.contains(&c))
    }

    fn peek(&self) -> Scanner<'a> {
        Scanner::new(&self.input[self.current_index..])
    }

    fn advance_to(&mut self, other: &Scanner<'a>) {
        *self = Scanner::new(&other.input[other.current_index..]);
    }

    fn skip_char(&mut self) {
        self.read_next_char();
    }

    fn skip_if_on_char(&mut self, c: char) {
        if self.current_char == Some(c) {
            self.read_next_char();
        }
    }

    fn skip_while_on_char(&mut self, c1: char) {
        self.skip_while(|c2| c1 == c2)
    }

    fn skip_while_on_any(&mut self, chars: &[char]) {
        self.skip_while(|c| chars.contains(&c))
    }

    fn skip_while(&mut self, predicate: impl Fn(char) -> bool) {
        while self.current_char.is_some_and(&predicate) {
            self.read_next_char();
        }
    }

    fn expect_char(&mut self, c: char) -> ParseResult<()> {
        if self.current_char == Some(c) {
            self.read_next_char();
            Ok(())
        } else {
            Err(ParseError::UnexpectedInput)
        }
    }

    fn expect_str(&mut self, s: &str) -> ParseResult<()> {
        if self.is_on_str(s) {
            for _ in 0..s.chars().count() {
                // TODO: Can we jump to char instead?
                // especially if we dont have a look ahead
                self.read_next_char();
            }
            Ok(())
        } else {
            Err(ParseError::UnexpectedInput)
        }
    }

    fn eat_char(&mut self) -> ParseResult<char> {
        match self.current_char {
            Some(c) => {
                self.read_next_char();
                Ok(c)
            }
            None => Err(ParseError::UnexpectedInput),
        }
    }

    fn eat_while(&mut self, predicate: impl Fn(char) -> bool) -> ParseResult<&'a str> {
        let i1 = self.current_index;

        self.skip_while(&predicate);

        let i2 = self.current_index;
        let string = &self.input[i1..i2];

        if string.is_empty() {
            Err(ParseError::UnexpectedInput)
        } else {
            Ok(string)
        }
    }

    fn eat_while_char(&mut self, c1: char) -> ParseResult<&'a str> {
        self.eat_while(|c| c == c1)
    }

    fn eat_until_char(&mut self, c1: char) -> ParseResult<&'a str> {
        self.eat_while(|c| c != c1)
    }

    fn read_next_char(&mut self) {
        if let Some((index, c)) = self.chars.next() {
            self.current_char = Some(c);
            self.current_index = index;
        } else {
            self.current_char = None;
            self.current_index = self.input.len();
        }
    }
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

    impl Into<ListItem> for ParagraphBuilder {
        fn into(self) -> ListItem {
            ListItem::Text(self.build())
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

    impl Into<ListItem> for ListBuilder {
        fn into(self) -> ListItem {
            ListItem::SubList(self.build())
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
    fn explicit_paragraph_with_block_break_before_text_is_rejected() {
        let input = "#paragraph\n\nCats go meeow!";

        let expected = Err(ParseError::UnexpectedInput);

        let actual = parse_str(input);

        assert_eq!(actual, expected);
    }

    #[test]
    fn unknown_block_is_rejected() {
        let input = "#feline\nMeow?";

        let expected = Err(ParseError::UnknownBlock);

        let actual = parse_str(input);

        assert_eq!(actual, expected);
    }

    #[test]
    fn unknown_block_starting_with_m_is_rejected() {
        let input = "#meowograph\nCats go meeow!";

        let expected = Err(ParseError::UnknownBlock);

        let actual = parse_str(input);

        assert_eq!(actual, expected);
    }

    #[test]
    fn empty_block_name_is_rejected() {
        let input = "#\nHi";

        let expected = Err(ParseError::UnexpectedInput);

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
    fn trailing_newline_is_ignored() {
        let input = "Cats\n";

        let expected = document()
            .with_block(paragraph().with(text("Cats")))
            .build();

        let actual = parse_str(input).unwrap();

        assert_eq!(actual, expected);
    }

    #[test]
    fn space_then_trailing_newline_is_ignored() {
        let input = "Cats \n";

        let expected = document()
            .with_block(paragraph().with(text("Cats")))
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
    fn new_line_becomes_whitespace_given_plain_then_styled() {
        let input = "Cats\n*whiskers*";

        let expected = document()
            .with_block(
                paragraph()
                    .with(text("Cats "))
                    .with(strong_text("whiskers")),
            )
            .build();

        let actual = parse_str(input).unwrap();

        assert_eq!(actual, expected);
    }

    #[test]
    fn new_line_becomes_whitespace_given_plain_then_raw() {
        let input = "Cats\n`nice whiskers`";

        let expected = document()
            .with_block(
                paragraph()
                    .with(text("Cats "))
                    .with(raw_text("nice whiskers")),
            )
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
    fn escaped_char() {
        let input = "\\A";

        let expected = document().with_block(paragraph().with(text("A"))).build();

        let actual = parse_str(input).unwrap();

        assert_eq!(actual, expected);
    }

    #[test]
    fn escaped_hash_in_markup() {
        let input = "My cat does backflips \\#coolcat";

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
            .with_block(paragraph().with(raw_text("Cat   cat")))
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

    //TODO: We should test an extended block break between meta and para

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

    //TODO: We should probably allow this after all.
    // Challenge: we would not know until the end of the doc
    // (or start of next container) what is supposed to be inside
    // Unless we reserved the #=[info]= form for multiblock?
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

        let expected = Err(ParseError::UnterminatedContainer);

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
    fn simple_list() {
        let input = concat!(
            "- Dry food is ok\n",
            "- Wet food is much better\n",
            "- Water is important also\n"
        );

        let expected = document()
            .with_block(
                list()
                    .with(paragraph().with(text("Dry food is ok")))
                    .with(paragraph().with(text("Wet food is much better")))
                    .with(paragraph().with(text("Water is important also"))),
            )
            .build();

        let actual = parse_str(input).unwrap();
        assert_eq!(actual, expected);
    }

    #[test]
    fn simple_list_with_continuations() {
        let input = concat!(
            "- Dry food\n",
            "is ok\n",
            "- Wet food\n",
            "  is much better\n",
            "- Water is\n",
            "    important also\n"
        );

        let expected = document()
            .with_block(
                list()
                    .with(paragraph().with(text("Dry food is ok")))
                    .with(paragraph().with(text("Wet food is much better")))
                    .with(paragraph().with(text("Water is important also"))),
            )
            .build();

        let actual = parse_str(input).unwrap();
        assert_eq!(actual, expected);
    }

    #[test]
    fn list_with_styled_text() {
        let input = concat!(
            "- Dry food is *ok*\n",
            "- Wet food is _much better_\n",
            "- Water is `important  also`\n"
        );

        let expected = document()
            .with_block(
                list()
                    .with(
                        paragraph()
                            .with(text("Dry food is "))
                            .with(strong_text("ok")),
                    )
                    .with(
                        paragraph()
                            .with(text("Wet food is "))
                            .with(emphasised_text("much better")),
                    )
                    .with(
                        paragraph()
                            .with(text("Water is "))
                            .with(raw_text("important  also")),
                    ),
            )
            .build();

        let actual = parse_str(input).unwrap();
        assert_eq!(actual, expected);
    }

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
                    .with(paragraph().with(text("Nice things to eat")))
                    .with(
                        list()
                            .with(paragraph().with(text("Tuna")))
                            .with(paragraph().with(text("Chicken")))
                            .with(paragraph().with(text("Beef"))),
                    ),
            )
            .build();

        let actual = parse_str(input).unwrap();
        assert_eq!(actual, expected);
    }

    //TODO: Would be cool to optionaly give lists a title

    //TODO: Realy good test:
    //- f`oo
    //  ba`r
    //  - baz

    //TODO: reject this
    //Foo `mango
    //' '<- space
    //banana` bar

    //TODO: ensure newline is treated as space in
    //Foo `mango
    //      banana` bar

    //TODO: test explicit list
    //TODO: lists with different styles
}
