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

#[derive(Debug)]
pub struct ParseError {
    kind: ErrorKind,
    column: usize,
    row: usize,
    line: String,
}

#[derive(PartialEq, Eq, Debug)]
enum ErrorKind {
    UnexpectedInput, //TODO: Should describe what was expected
    LooseDelimiter,
    EmptyDelimitedText,
    UnknownMetadata, //TODO: Should describe the unknown metadata
    MetadataNotAtStart,
    UnknownBlock,     //TODO: Should describe the unknown block
    UnknownContainer, //TODO: Should describe the unknown container
    UnterminatedContainer,
}

impl Display for ParseError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self.kind {
            ErrorKind::UnexpectedInput => write!(f, "unexpected input"),
            ErrorKind::LooseDelimiter => write!(f, "loose delimiter"),
            ErrorKind::EmptyDelimitedText => write!(f, "empty delimited text"),
            ErrorKind::UnknownMetadata => write!(f, "unknown metadata"),
            ErrorKind::MetadataNotAtStart => write!(f, "metadata not at start"),
            ErrorKind::UnknownBlock => write!(f, "unknown block"),
            ErrorKind::UnknownContainer => write!(f, "unknown container"),
            ErrorKind::UnterminatedContainer => write!(f, "unknown container"),
        }?;

        write!(f, " at line {} column {}:\n", self.column, self.row)?;
        write!(f, "{}\n", self.line)?;
        for _ in 0..self.column {
            write!(f, " ")?;
        }
        write!(f, "^")?;

        Ok(())
    }
}

type ParseResult<T> = Result<T, ErrorKind>;

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

pub fn parse_str(input: &str) -> Result<Document, ParseError> {
    let scanner = &mut Scanner::new(input);
    let result = parse_document(scanner);

    result.map_err(|kind| ParseError {
        kind,
        column: scanner.column,
        row: scanner.row,
        line: scanner.current_row(),
    })
}

fn parse_document(scanner: &mut Scanner) -> ParseResult<Document> {
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
            _ => return Err(ErrorKind::UnknownMetadata),
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
            return Err(ErrorKind::UnterminatedContainer);
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
        _ => Err(ErrorKind::UnknownContainer),
    }
}

fn parse_block(scanner: &mut Scanner) -> ParseResult<Block> {
    let block = if scanner.is_on_char(HASH) {
        scanner.skip_char();
        let block_name = scanner.eat_while(char_usable_in_identifier)?;
        scanner.expect_char(NEW_LINE)?;

        match block_name {
            "metadata" => Err(ErrorKind::MetadataNotAtStart),
            "paragraph" => parse_paragraph(scanner),
            _ => Err(ErrorKind::UnknownBlock),
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
        return Err(ErrorKind::UnexpectedInput);
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
            return Err(ErrorKind::UnexpectedInput);
        }
    }

    if run.starts_with(SPACE) || run.ends_with(SPACE) {
        return Err(ErrorKind::LooseDelimiter);
    }

    if run.is_empty() {
        return Err(ErrorKind::EmptyDelimitedText);
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
        _ => Err(ErrorKind::UnexpectedInput),
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
                return Err(ErrorKind::UnexpectedInput);
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
            return Err(ErrorKind::UnexpectedInput);
        }
    }

    if run.is_empty() {
        return Err(ErrorKind::EmptyDelimitedText);
    }

    Ok(TextRun {
        text: run,
        style: Style::Raw,
    })
}

#[derive(Debug, Clone)]
struct Scanner<'a> {
    input: &'a str,
    chars: CharIndices<'a>,
    current_char: Option<char>,
    current_index: usize,
    column: usize,
    row: usize,
}

impl<'a> Scanner<'a> {
    fn new(input: &'a str) -> Self {
        let mut scanner = Self {
            input,
            chars: input.char_indices(),
            current_char: None,
            current_index: 0,
            column: 0,
            row: 1,
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
        self.clone()
    }

    fn advance_to(&mut self, other: &Scanner<'a>) {
        *self = other.clone()
    }

    fn skip_char(&mut self) {
        self.read_next_char();
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
            Err(ErrorKind::UnexpectedInput)
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
            Err(ErrorKind::UnexpectedInput)
        }
    }

    fn eat_char(&mut self) -> ParseResult<char> {
        match self.current_char {
            Some(c) => {
                self.read_next_char();
                Ok(c)
            }
            None => Err(ErrorKind::UnexpectedInput),
        }
    }

    fn eat_while(&mut self, predicate: impl Fn(char) -> bool) -> ParseResult<&'a str> {
        let i1 = self.current_index;

        self.skip_while(&predicate);

        let i2 = self.current_index;
        let string = &self.input[i1..i2];

        if string.is_empty() {
            Err(ErrorKind::UnexpectedInput)
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
            if c == '\n' {
                self.column = 0;
                self.row += 1;
            } else {
                self.column += 1;
            }

            self.current_char = Some(c);
            self.current_index = index;
        } else {
            self.current_char = None;
            self.current_index = self.input.len();
        }
    }

    fn current_row(&self) -> String {
        self.input
            .lines()
            .nth(self.row - 1)
            .unwrap_or("")
            .to_owned()
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

    fn assert_parses_succeeds<T: Into<Document>>(input: &'static str, expected: T) {
        let expected = expected.into();
        let result = parse_str(input);

        if let Err(ref e) = result {
            eprintln!("{}", e);
        }

        let actual = result.expect("parse should succeed");

        assert_eq!(actual, expected);
    }

    fn assert_parse_fails(input: &'static str, expected: ErrorKind) {
        let expected = expected.into();
        let result = parse_str(input);

        let err = result.expect_err("parse should fail");
        let actual = err.kind;

        assert_eq!(actual, expected);
    }

    //TODO: Fix naming of Docment builder
    impl Into<Document> for DocmentBuilder {
        fn into(self) -> Document {
            self.build()
        }
    }

    impl Into<Document> for ContainerBuilder {
        fn into(self) -> Document {
            document().with_container(self).build()
        }
    }

    impl Into<Document> for ParagraphBuilder {
        fn into(self) -> Document {
            document().with_block(self).build()
        }
    }

    impl Into<Document> for ListBuilder {
        fn into(self) -> Document {
            document().with_block(self).build()
        }
    }

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

        assert_parses_succeeds(input, expected);
    }

    #[test]
    fn one_line_paragraph() {
        let input = "We like cats very much";

        let expected = paragraph().with(text("We like cats very much"));

        assert_parses_succeeds(input, expected);
    }

    #[test]
    fn explicit_paragraph() {
        let input = "#paragraph\nCats go meeow!";

        let expected = paragraph().with(text("Cats go meeow!"));

        assert_parses_succeeds(input, expected);
    }

    #[test]
    fn explicit_paragraph_with_block_break_before_text_is_rejected() {
        let input = "#paragraph\n\nCats go meeow!";

        let expected = ErrorKind::UnexpectedInput;

        assert_parse_fails(input, expected);
    }

    #[test]
    fn unknown_block_is_rejected() {
        let input = "#feline\nMeow?";

        let expected = ErrorKind::UnknownBlock;

        assert_parse_fails(input, expected);
    }

    #[test]
    fn unknown_block_starting_with_m_is_rejected() {
        let input = "#meowograph\nCats go meeow!";

        let expected = ErrorKind::UnknownBlock;

        assert_parse_fails(input, expected);
    }

    #[test]
    fn empty_block_name_is_rejected() {
        let input = "#\nHi";

        let expected = ErrorKind::UnexpectedInput;

        assert_parse_fails(input, expected);
    }

    #[test]
    fn block_header_without_newline_is_rejected() {
        let input = "#paragraph";

        let expected = ErrorKind::UnexpectedInput;

        assert_parse_fails(input, expected);
    }

    #[test]
    fn double_space() {
        let input = "Nice  kitty!";

        let expected = paragraph().with(text("Nice kitty!"));

        assert_parses_succeeds(input, expected);
    }

    #[test]
    fn trailing_newline_is_ignored() {
        let input = "Cats\n";

        let expected = paragraph().with(text("Cats"));

        assert_parses_succeeds(input, expected);
    }

    #[test]
    fn space_then_trailing_newline_is_ignored() {
        let input = "Cats \n";

        let expected = paragraph().with(text("Cats"));

        assert_parses_succeeds(input, expected);
    }

    #[test]
    fn new_line_becomes_whitespace() {
        let input = "Cats\nwhiskers";

        let expected = paragraph().with(text("Cats whiskers"));

        assert_parses_succeeds(input, expected);
    }

    #[test]
    fn new_line_becomes_whitespace_given_plain_then_styled() {
        let input = "Cats\n*whiskers*";

        let expected = paragraph()
            .with(text("Cats "))
            .with(strong_text("whiskers"));

        assert_parses_succeeds(input, expected);
    }

    #[test]
    fn new_line_becomes_whitespace_given_plain_then_raw() {
        let input = "Cats\n`nice whiskers`";

        let expected = paragraph()
            .with(text("Cats "))
            .with(raw_text("nice whiskers"));

        assert_parses_succeeds(input, expected);
    }

    #[test]
    fn new_line_with_extra_whitespace_collapses() {
        let input = "Cats    \n    whiskers";
        let expected = paragraph().with(text("Cats whiskers"));

        assert_parses_succeeds(input, expected);
    }

    #[test]
    fn two_new_lines_become_blocks() {
        let input = "Cats\n\nwhiskers";

        let expected = document()
            .with_block(paragraph().with(text("Cats")))
            .with_block(paragraph().with(text("whiskers")));

        assert_parses_succeeds(input, expected);
    }

    #[test]
    fn three_new_lines_becomes_blocks() {
        let input = "Cats\n\n\nwhiskers";

        let expected = document()
            .with_block(paragraph().with(text("Cats")))
            .with_block(paragraph().with(text("whiskers")));

        assert_parses_succeeds(input, expected);
    }

    #[test]
    fn two_new_lines_with_whitespace_is_treated_as_blockbreak() {
        let input = "Cats\n \nwhiskers";
        let expected = document()
            .with_block(paragraph().with(text("Cats")))
            .with_block(paragraph().with(text("whiskers")));

        assert_parses_succeeds(input, expected);
    }

    #[test]
    fn blockbreak_with_extra_whitespace() {
        let input = "Cats  \n    \n  whiskers";
        let expected = document()
            .with_block(paragraph().with(text("Cats")))
            .with_block(paragraph().with(text("whiskers")));

        assert_parses_succeeds(input, expected);
    }

    #[test]
    fn missing_blockbreak_is_rejected() {
        let input = concat!(
            "Cats can sometimes be\n",
            "#paragraph\n",
            "ever so surprising\n"
        );

        let expected = ErrorKind::UnexpectedInput;

        assert_parse_fails(input, expected);
    }
    #[test]
    fn escaped_char() {
        let input = "\\A";

        let expected = paragraph().with(text("A"));

        assert_parses_succeeds(input, expected);
    }

    #[test]
    fn escaped_hash_in_markup() {
        let input = "My cat does backflips \\#coolcat";

        let expected = paragraph().with(text("My cat does backflips #coolcat"));

        assert_parses_succeeds(input, expected);
    }

    #[test]
    fn escaped_underscore() {
        let input = "cat\\_case";

        let expected = paragraph().with(text("cat_case"));

        assert_parses_succeeds(input, expected);
    }

    #[test]
    fn escaped_underscore_in_emphasis() {
        let input = "_cat\\_case_";

        let expected = paragraph().with(emphasised_text("cat_case"));

        assert_parses_succeeds(input, expected);
    }

    #[test]
    fn escaped_ignored_in_raw() {
        let input = "`cat\\_case`";

        let expected = paragraph().with(raw_text("cat\\_case"));

        assert_parses_succeeds(input, expected);
    }

    #[test]
    fn emphasised_words() {
        let input = "We _totally adore_ them";

        let expected = paragraph()
            .with(text("We "))
            .with(emphasised_text("totally adore"))
            .with(text(" them"));

        assert_parses_succeeds(input, expected);
    }

    #[test]
    fn emphasis_at_end_of_line() {
        let input = "Cats like to _zoom_\naround";

        let expected = paragraph()
            .with(text("Cats like to "))
            .with(emphasised_text("zoom"))
            .with(text(" around"));

        assert_parses_succeeds(input, expected);
    }

    #[test]
    fn strong_words() {
        let input = "I *need to pet that cat* right away.";

        let expected = paragraph()
            .with(text("I "))
            .with(strong_text("need to pet that cat"))
            .with(text(" right away."));

        assert_parses_succeeds(input, expected);
    }

    #[test]
    fn strong_mid_word() {
        let input = "I said: mee*ooOOo*ww!";

        let expected = paragraph()
            .with(text("I said: mee"))
            .with(strong_text("ooOOo"))
            .with(text("ww!"));

        assert_parses_succeeds(input, expected);
    }

    #[test]
    fn strong_over_two_lines() {
        let input = "*me\now*";

        let expected = paragraph().with(strong_text("me ow"));

        assert_parses_succeeds(input, expected);
    }

    #[test]
    fn strikethrough_words() {
        let input = "Cats are ~ok i guess~ magnificant";

        let expected = paragraph()
            .with(text("Cats are "))
            .with(strikethrough_text("ok i guess"))
            .with(text(" magnificant"));

        assert_parses_succeeds(input, expected);
    }

    #[test]
    fn raw_words() {
        let input = "Robot cat says `bleep bloop`!";

        let expected = paragraph()
            .with(text("Robot cat says "))
            .with(raw_text("bleep bloop"))
            .with(text("!"));

        assert_parses_succeeds(input, expected);
    }

    #[test]
    fn raw_mid_word() {
        let input = "Bl`eeee`p!";

        let expected = paragraph()
            .with(text("Bl"))
            .with(raw_text("eeee"))
            .with(text("p!"));

        assert_parses_succeeds(input, expected);
    }

    #[test]
    fn underscore_in_raw() {
        let input = "Set `PURR_LOUDLY` to true";

        let expected = paragraph()
            .with(text("Set "))
            .with(raw_text("PURR_LOUDLY"))
            .with(text(" to true"));

        assert_parses_succeeds(input, expected);
    }

    #[test]
    fn extra_spaces_in_raw() {
        let input = "`Keep your       distance`";

        let expected = paragraph().with(raw_text("Keep your       distance"));

        assert_parses_succeeds(input, expected);
    }

    #[test]
    fn raw_over_two_lines() {
        let input = "`Great\ncats`";

        let expected = paragraph().with(raw_text("Great cats"));

        assert_parses_succeeds(input, expected);
    }

    #[test]
    fn strikethrough_over_two_lines() {
        let input = "~Great\ndogs~";

        let expected = paragraph().with(strikethrough_text("Great dogs"));

        assert_parses_succeeds(input, expected);
    }

    #[test]
    fn raw_leading_with_new_line() {
        let input = "`\nMeow?`";

        let expected = paragraph().with(raw_text(" Meow?"));

        assert_parses_succeeds(input, expected);
    }

    #[test]
    fn raw_trailing_with_new_line() {
        let input = "`Meow\n`";

        let expected = paragraph().with(raw_text("Meow "));

        assert_parses_succeeds(input, expected);
    }

    #[test]
    fn raw_leading_with_space() {
        let input = "` Meow`";

        let expected = paragraph().with(raw_text(" Meow"));

        assert_parses_succeeds(input, expected);
    }

    #[test]
    fn raw_trailing_with_space() {
        let input = "`Meow `";

        let expected = paragraph().with(raw_text("Meow "));

        assert_parses_succeeds(input, expected);
    }

    #[test]
    fn raw_over_three_lines() {
        let input = "`Great\ncats\nassemble!`";

        let expected = paragraph().with(raw_text("Great cats assemble!"));

        assert_parses_succeeds(input, expected);
    }

    #[test]
    fn standalone_dash() {
        let input = "Felines - fantastic!";

        let expected = paragraph().with(text("Felines - fantastic!"));

        assert_parses_succeeds(input, expected);
    }

    #[test]
    #[ignore]
    fn paragraph_with_trailing_whitespace() {
        let input = "Cool kitty   ";

        let expected = paragraph().with(text("Cool kitty"));

        assert_parses_succeeds(input, expected);
    }

    #[test]
    fn underscore_in_awkward_places() {
        let input = "Cat cat_cat cat_ cat.";

        let expected = paragraph()
            .with(text("Cat cat"))
            .with(emphasised_text("cat cat"))
            .with(text(" cat."));

        assert_parses_succeeds(input, expected);
    }

    #[test]
    fn newline_then_multiple_spaces_in_plain_text() {
        let input = "Cat\n  cat";

        let expected = paragraph().with(text("Cat cat"));

        assert_parses_succeeds(input, expected);
    }

    #[test]
    fn newline_then_multiple_spaces_in_styled() {
        let input = "*Cat\n  cat*";

        let expected = paragraph().with(strong_text("Cat cat"));

        assert_parses_succeeds(input, expected);
    }

    #[test]
    fn newline_then_multiple_spaces_in_raw() {
        let input = "`Cat\n  cat`";

        let expected = paragraph().with(raw_text("Cat   cat"));

        assert_parses_succeeds(input, expected);
    }

    #[test]
    fn multiple_spaces_then_newline_in_plain_text() {
        let input = "Cat  \ncat";

        let expected = paragraph().with(text("Cat cat"));

        assert_parses_succeeds(input, expected);
    }

    #[test]
    fn multiple_spaces_then_newline_in_styled() {
        let input = "*Cat  \ncat*";

        let expected = paragraph().with(strong_text("Cat cat"));

        assert_parses_succeeds(input, expected);
    }

    #[test]
    fn multiple_spaces_then_newline_in_raw() {
        let input = "`Cat  \ncat`";

        let expected = paragraph().with(raw_text("Cat   cat"));

        assert_parses_succeeds(input, expected);
    }

    #[test]
    fn empty_emphasis() {
        let input = "Rules cats must follow: __.";

        let expected = ErrorKind::EmptyDelimitedText;

        assert_parse_fails(input, expected);
    }

    #[test]
    fn empty_raw() {
        let input = "Robot cat says: ``!.";

        let expected = ErrorKind::EmptyDelimitedText;

        assert_parse_fails(input, expected);
    }

    #[test]
    fn raw_with_double_linebreak() {
        let input = "`Erm...\n\nmeow?`";

        let expected = ErrorKind::UnexpectedInput;

        assert_parse_fails(input, expected);
    }

    #[test]
    fn strikethrough_with_double_linebreak() {
        let input = "~Erm...\n\nmeow?~";

        let expected = ErrorKind::UnexpectedInput;

        assert_parse_fails(input, expected);
    }

    #[test]
    fn unmatched_emphasis_1() {
        let input = "_.";

        let expected = ErrorKind::UnexpectedInput;

        assert_parse_fails(input, expected);
    }

    #[test]
    fn unmatched_emphasis_2() {
        let input = "meow _meow.";

        let expected = ErrorKind::UnexpectedInput;

        assert_parse_fails(input, expected);
    }

    #[test]
    fn unmatched_emphasis_3() {
        let input = "meow meow_";

        let expected = ErrorKind::UnexpectedInput;

        assert_parse_fails(input, expected);
    }

    #[test]
    fn loose_strong_delimiter_start() {
        let input = "* meow meow*";

        let expected = ErrorKind::LooseDelimiter;

        assert_parse_fails(input, expected);
    }

    #[test]
    fn loose_strong_delimiter_end() {
        let input = "*meow meow *";

        let expected = ErrorKind::LooseDelimiter;

        assert_parse_fails(input, expected);
    }

    #[test]
    fn raw_immediately_in_emphasis() {
        let input = "_``_";

        let expected = ErrorKind::UnexpectedInput;

        assert_parse_fails(input, expected);
    }

    #[test]
    fn raw_within_in_emphasis() {
        let input = "_a``a_";

        let expected = ErrorKind::UnexpectedInput;

        assert_parse_fails(input, expected);
    }

    #[test]
    fn doc_with_leading_new_line() {
        let input = "\nCats cats cats";

        let expected = paragraph().with(text("Cats cats cats"));

        assert_parses_succeeds(input, expected);
    }

    #[test]
    fn doc_with_leading_new_lines() {
        let input = "\n\nCats cats cats";

        let expected = paragraph().with(text("Cats cats cats"));

        assert_parses_succeeds(input, expected);
    }

    #[test]
    fn doc_with_leading_spaces_and_new_line() {
        let input = "   \nCats cats cats";

        let expected = paragraph().with(text("Cats cats cats"));

        assert_parses_succeeds(input, expected);
    }

    #[test]
    fn doc_ending_with_new_line() {
        let input = "Cats are friends\n";

        let expected = paragraph().with(text("Cats are friends"));

        assert_parses_succeeds(input, expected);
    }

    #[test]
    fn doc_ending_with_new_lines() {
        let input = "Feline friends\n\n";

        let expected = paragraph().with(text("Feline friends"));

        assert_parses_succeeds(input, expected);
    }

    #[test]
    fn new_line_and_space_between_styled_and_plain_text_runs() {
        let input = "*Cat*\n cat";

        let expected = paragraph().with(strong_text("Cat")).with(text(" cat"));

        assert_parses_succeeds(input, expected);
    }

    #[test]
    fn leading_whitespace_on_paragraph_is_ignored() {
        let input = "Cat\n\n  cat";

        let expected = document()
            .with_block(paragraph().with(text("Cat")))
            .with_block(paragraph().with(text("cat")));

        assert_parses_succeeds(input, expected);
    }

    //TODO: We should test an extended block break between meta and para

    #[test]
    fn doc_metadata() {
        let input = concat!(
            "#metadata\n",
            "id: 01.23\n",
            "title: Practical espionage for felines\n",
        );

        let expected = document().with_metadata(
            metadata()
                .with_id("01.23")
                .with_title("Practical espionage for felines"),
        );

        assert_parses_succeeds(input, expected);
    }

    #[test]
    fn doc_metadata_with_alternate_spacing() {
        let input = concat!("#metadata\n", "id :01.23\n",);

        let expected = document().with_metadata(metadata().with_id("01.23"));

        assert_parses_succeeds(input, expected);
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

        let expected = ErrorKind::MetadataNotAtStart;

        assert_parse_fails(input, expected);
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

        let expected = info()
            .with(paragraph().with(text("Here are some facts...")))
            .with(paragraph().with(text("...about the cats!")));

        assert_parses_succeeds(input, expected);
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

        let expected = ErrorKind::UnterminatedContainer;

        assert_parse_fails(input, expected);
    }

    #[test]
    fn container_missing_start_is_rejected() {
        let input = concat!("Silly cat\n", "#=");

        let expected = ErrorKind::UnexpectedInput;

        assert_parse_fails(input, expected);
    }

    #[test]
    fn only_container_header_is_rejected() {
        let input = concat!("#[info]\n",);

        let expected = ErrorKind::UnterminatedContainer;

        assert_parse_fails(input, expected);
    }

    #[test]
    fn trailing_text_on_container_footer_is_rejected() {
        let input = concat!(
            "#[info]\n",
            "Let me know if you find where I left my\n",
            "#=toy"
        );

        let expected = ErrorKind::UnexpectedInput;

        assert_parse_fails(input, expected);
    }

    #[test]
    #[ignore]
    fn dash_in_paragraph_is_treated_as_part_of_text() {
        let input = "Ripley\n- Cat";

        let expected = paragraph().with(text("Ripley - Cat"));

        assert_parses_succeeds(input, expected);
    }

    #[test]
    fn simple_list() {
        let input = concat!(
            "- Dry food is ok\n",
            "- Wet food is much better\n",
            "- Water is important also\n"
        );

        let expected = list()
            .with(paragraph().with(text("Dry food is ok")))
            .with(paragraph().with(text("Wet food is much better")))
            .with(paragraph().with(text("Water is important also")));

        assert_parses_succeeds(input, expected);
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

        let expected = list()
            .with(paragraph().with(text("Dry food is ok")))
            .with(paragraph().with(text("Wet food is much better")))
            .with(paragraph().with(text("Water is important also")));

        assert_parses_succeeds(input, expected);
    }

    #[test]
    fn list_with_styled_text() {
        let input = concat!(
            "- Dry food is *ok*\n",
            "- Wet food is _much better_\n",
            "- Water is `important  also`\n"
        );

        let expected = list()
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
            );

        assert_parses_succeeds(input, expected);
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

        let expected = list()
            .with(paragraph().with(text("Nice things to eat")))
            .with(
                list()
                    .with(paragraph().with(text("Tuna")))
                    .with(paragraph().with(text("Chicken")))
                    .with(paragraph().with(text("Beef"))),
            );

        assert_parses_succeeds(input, expected);
    }

    #[test]
    fn error_specifies_correct_row_and_column() {
        let input = "Silly cat\ngoes *_*";

        let expected = (7, 2);

        let error = parse_str(input).unwrap_err();
        let actual = (error.column, error.row);

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
