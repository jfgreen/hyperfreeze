use std::backtrace::{Backtrace, BacktraceStatus};
use std::fmt::Display;

use crate::document::*;
use crate::scan::*;

#[derive(Debug)]
pub struct ParseError {
    kind: ErrorKind,
    input_column: u32,
    input_line: u32,
    failing_input_line: Option<String>,
    backtrace: Backtrace,
}

macro_rules! parse_err {
    ($error:expr, $position:expr) => {
        Err(build_parse_error!($error, $position))
    };
}

macro_rules! try_scan {
    ($scan: expr, $error:expr) => {
        $scan.map_err(|position| build_parse_error!($error, position))
    };
}

macro_rules! build_parse_error {
    ($error:expr, $position:expr) => {
        ParseError {
            kind: $error,
            input_column: $position.column + 1,
            input_line: $position.row + 1,
            failing_input_line: $position.line().map(str::to_string),
            backtrace: Backtrace::capture(),
        }
    };
}

#[derive(PartialEq, Eq, Debug)]
enum ErrorKind {
    LooseDelimiter,
    EmptyDelimitedText,
    DocumentHeaderNotAtStart,
    UnknownMetadata(String),
    UnknownBlock(String),
    UnknownContainer(String),
    EmptyContainer,
    ContainerMissingStart,
    WhitespaceAtParagraphStart,
    InvalidStyleDelimiter(char),
    InlineRawHasBlockBreak,
    ExpectedChar(char),
    ExpectedIdentifier,
    ExpectedRawFragment,
    ExpectedSpace,
    ExpectedLink,
    ExpectedSectionHeader,
    ExpectedMetadataId,
    ExpectedMetadataTag,
    ExpectedReferencesBlock,
    ReferencesOutOfPlace,
    UnevenListIndent(usize),
    MissingListLevel((usize, usize)),
    UnexpectedEndOfInput,
}

impl Display for ParseError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match &self.kind {
            LooseDelimiter => write!(f, "delimited text cant have leading/trailing whitespace"),
            EmptyDelimitedText => write!(f, "delimited text cant be empty"),
            DocumentHeaderNotAtStart => write!(f, "document header is not at start of document"),
            UnknownMetadata(key) => write!(f, "unknown metadata '{}", key),
            UnknownBlock(name) => write!(f, "unknown block '{}'", name),
            UnknownContainer(name) => write!(f, "unknown container '{}'", name),
            EmptyContainer => write!(f, "empty container,"),
            ContainerMissingStart => write!(f, "delimited container end with no preceeding start"),
            WhitespaceAtParagraphStart => write!(f, "paragraph cant start with whitespace"),
            InvalidStyleDelimiter(c) => write!(f, "'{}' is  not a valid style delimiter", c),
            InlineRawHasBlockBreak => write!(f, "inline raw text cant have a block break"),
            ExpectedChar(c) => write!(f, "expected char '{}'", c.escape_default()),
            ExpectedIdentifier => write!(f, "expected identifier"),
            ExpectedRawFragment => write!(f, "expected raw fragment"),
            ExpectedSpace => write!(f, "expected one or more spaces"),
            ExpectedLink => write!(f, "expected link"),
            ExpectedSectionHeader => write!(f, "expected start of section header"),
            ExpectedMetadataId => write!(f, "expected id"),
            ExpectedMetadataTag => write!(f, "expected tag"),
            ExpectedReferencesBlock => write!(f, "expected 'references' header"),
            ReferencesOutOfPlace => write!(f, "references not in correct part of document"),
            UnevenListIndent(spaces) => write!(f, "list indent of {} is not even", spaces),
            MissingListLevel((from, to)) => {
                write!(f, "list indent skipped from {} to {}", from, to)
            }
            UnexpectedEndOfInput => write!(f, "unexpected end of input"),
        }?;

        if let Some(failing_line) = &self.failing_input_line {
            writeln!(
                f,
                "\nEncountered at line {} column {}:",
                self.input_line, self.input_column
            )?;
            writeln!(f)?;
            writeln!(f, "{}", failing_line)?;
            for _ in 2..self.input_column {
                write!(f, " ")?;
            }
            writeln!(f, "^")?;
        }

        if self.backtrace.status() == BacktraceStatus::Captured {
            writeln!(f)?;
            writeln!(f, "Parse backtrace:")?;
            writeln!(f, "{}", self.backtrace)?;
        }

        Ok(())
    }
}

type ParseResult<T> = Result<T, ParseError>;

use ErrorKind::*;

const SPACE: char = ' ';
const NEW_LINE: char = '\n';
const COLON: char = ':';
const DELIMITED_CONATINER_START: &str = ">>>";
const DELIMITED_CONTAINER_END: &str = "<<<";
const HASH: char = '#';
const LEFT_SQUARE_BRACKET: char = '[';
const RIGHT_SQUARE_BRACKET: char = ']';
const BACKTICK: char = '`';
const ASTERISK: char = '*';
const TILDE: char = '~';
const UNDERSCORE: char = '_';
const SLASH: char = '/';
const TRIPPLE_SLASH: &str = "///";
const BACKSLASH: char = '\\';
const DASH: char = '-';
const AT_SIGN: char = '@';
const EXCLAMATION_MARK: char = '!';
const GREATER_THAN: char = '>';
const VERTICAL_BAR: char = '|';
const WHITESPACE_CHARS: &[char; 2] = &[SPACE, NEW_LINE];
const STYLE_DELIMITER_CHARS: &[char; 3] = &[ASTERISK, TILDE, UNDERSCORE];

const MARKUP_CHARS: &[char; 10] = &[
    UNDERSCORE,
    BACKTICK,
    ASTERISK,
    TILDE,
    SPACE,
    NEW_LINE,
    HASH,
    BACKSLASH,
    LEFT_SQUARE_BRACKET,
    RIGHT_SQUARE_BRACKET,
];

fn char_usable_in_identifier(c: char) -> bool {
    c.is_alphanumeric()
}

fn char_usable_in_id(c: char) -> bool {
    c.is_alphanumeric() || c == '_' || c == '-' || c == '.'
}

fn char_usable_in_tag(c: char) -> bool {
    c.is_alphanumeric() || c == '_' || c == '-'
}

fn char_usable_in_text_frag(c: char) -> bool {
    !MARKUP_CHARS.contains(&c)
}

fn char_usable_in_raw_frag(c: char) -> bool {
    ![BACKTICK, SPACE, NEW_LINE].contains(&c)
}

fn expect_char(scanner: &mut Scanner, expected: char) -> ParseResult<()> {
    try_scan!(scanner.expect_char(expected), ExpectedChar(expected))
}

fn eat_identifier<'a>(scanner: &mut Scanner<'a>) -> ParseResult<&'a str> {
    try_scan!(
        scanner.eat_while(char_usable_in_identifier),
        ExpectedIdentifier
    )
}

fn eat_metadata_id<'a>(scanner: &mut Scanner<'a>) -> ParseResult<&'a str> {
    try_scan!(scanner.eat_while(char_usable_in_id), ExpectedMetadataId)
}

fn eat_metadata_tag<'a>(scanner: &mut Scanner<'a>) -> ParseResult<&'a str> {
    try_scan!(scanner.eat_while(char_usable_in_tag), ExpectedMetadataTag)
}

fn eat_link<'a>(scanner: &mut Scanner<'a>) -> ParseResult<&'a str> {
    try_scan!(scanner.eat_until_one_of(WHITESPACE_CHARS), ExpectedLink)
}

fn eat_space<'a>(scanner: &mut Scanner<'a>) -> ParseResult<&'a str> {
    try_scan!(scanner.eat_while_char(SPACE), ExpectedSpace)
}

fn eat_raw_fragment<'a>(scanner: &mut Scanner<'a>) -> ParseResult<&'a str> {
    try_scan!(
        scanner.eat_while(char_usable_in_raw_frag),
        ExpectedRawFragment
    )
}

fn eat_text_fragment<'a>(scanner: &mut Scanner<'a>) -> ParseResult<&'a str> {
    try_scan!(
        scanner.eat_while(char_usable_in_text_frag),
        ExpectedRawFragment
    )
}

fn eat_char(scanner: &mut Scanner) -> ParseResult<char> {
    try_scan!(scanner.eat_char(), UnexpectedEndOfInput)
}

fn eat_section_slashes<'a>(scanner: &mut Scanner<'a>) -> ParseResult<&'a str> {
    try_scan!(scanner.eat_while_char(SLASH), ExpectedSectionHeader)
}

#[derive(Clone, Copy, PartialEq)]
enum TextMode {
    Paragraph,
    List,
    Title,
}

pub fn parse_str(input: &str) -> Result<Document, ParseError> {
    let scanner = &mut Scanner::new(input);
    parse_document(scanner)
}

fn parse_document(scanner: &mut Scanner) -> ParseResult<Document> {
    let mut elements = Vec::new();

    let mut metadata = Metadata::default();
    let mut references = Vec::new();

    scanner.skip_while_on_any(WHITESPACE_CHARS);

    if scanner.is_on_char(SLASH) {
        metadata = parse_document_header(scanner)?;
    }

    scanner.skip_while_on_any(WHITESPACE_CHARS);

    if scanner.is_on_char(AT_SIGN) {
        let refs = parse_references(scanner)?;
        references.extend(refs);
    }

    while scanner.has_input() {
        if scanner.is_on_one_of(WHITESPACE_CHARS) {
            scanner.skip_char();
        } else {
            let element = parse_element(scanner)?;
            elements.push(element);
        }
    }

    Ok(Document {
        metadata,
        contents: elements.into_boxed_slice(),
        references: references.into_boxed_slice(),
    })
}

fn parse_element(scanner: &mut Scanner) -> Result<Element, ParseError> {
    if scanner.is_on_char(EXCLAMATION_MARK) {
        let container = parse_container(scanner)?;
        Ok(Element::Container(container))
    } else if scanner.is_on_char(AT_SIGN) {
        parse_err!(ReferencesOutOfPlace, scanner.position())
    } else if scanner.is_on_char(SLASH) {
        let section = parse_section(scanner)?;
        Ok(Element::Section(section))
    } else {
        let block = parse_block(scanner)?;
        Ok(Element::Block(block))
    }
}

fn parse_document_header(scanner: &mut Scanner) -> ParseResult<Metadata> {
    expect_char(scanner, SLASH)?;
    scanner.skip_while_on_char(SPACE);

    if scanner.is_on_char(SLASH) {
        todo!("not implemented - should reject")
    }

    let title = parse_markup_text(scanner, TextMode::Title)?;

    scanner.skip_while_on_char(SPACE);

    if scanner.has_input() {
        expect_char(scanner, NEW_LINE)?;
    }

    let mut metadata = Metadata {
        title: Some(title),
        ..Default::default()
    };

    while scanner.is_on(char_usable_in_identifier) {
        let key_position = scanner.position();
        let key = eat_identifier(scanner)?;
        scanner.skip_while_on_char(SPACE);
        expect_char(scanner, COLON)?;
        scanner.skip_while_on_char(SPACE);

        match key {
            "id" => {
                let id = eat_metadata_id(scanner)?;
                metadata.id = Some(id.to_string());
            }
            "tags" => {
                let tags = parse_tag_list(scanner)?;
                metadata.tags = Some(tags);
            }
            _ => return parse_err!(UnknownMetadata(key.into()), key_position),
        };

        if scanner.has_input() {
            expect_char(scanner, NEW_LINE)?;
        }
    }

    Ok(metadata)
}

fn parse_tag_list(scanner: &mut Scanner) -> ParseResult<Box<[String]>> {
    let mut tags = Vec::new();

    loop {
        let tag = eat_metadata_tag(scanner)?;
        tags.push(tag.to_string());
        scanner.skip_while_on_char(SPACE);
        if scanner.is_on_char(VERTICAL_BAR) {
            scanner.skip_char();
            scanner.skip_while_on_char(SPACE);
        } else {
            break;
        }
    }

    let tags = tags.into_boxed_slice();
    Ok(tags)
}

fn parse_container(scanner: &mut Scanner) -> ParseResult<Container> {
    expect_char(scanner, EXCLAMATION_MARK)?;
    let name_pos = scanner.position();
    let name = eat_identifier(scanner)?;
    expect_char(scanner, NEW_LINE)?;

    let container_kind = match name {
        "info" => ContainerKind::Info,
        _ => return parse_err!(UnknownContainer(name.into()), name_pos),
    };

    let mut blocks = Vec::new();

    //TODO: Ensure containers can not hold sections
    if scanner.is_on_str(DELIMITED_CONATINER_START) {
        scanner.skip_chars(3);
        expect_char(scanner, NEW_LINE)?;
        loop {
            if scanner.is_on_str(DELIMITED_CONTAINER_END) {
                scanner.skip_chars(3);

                if scanner.has_input() {
                    expect_char(scanner, NEW_LINE)?;
                }

                scanner.skip_while_on_char(SPACE);

                if scanner.has_input() {
                    expect_char(scanner, NEW_LINE)?;
                }

                break;
            } else if scanner.is_on_one_of(WHITESPACE_CHARS) {
                scanner.skip_char();
            } else {
                let block = parse_block(scanner)?;
                blocks.push(block);
            }
        }
    } else {
        if scanner.is_on_one_of(WHITESPACE_CHARS) || !scanner.has_input() {
            return parse_err!(EmptyContainer, name_pos);
        }
        let block = parse_block(scanner)?;
        blocks.push(block);
    }

    let container = Container {
        content: blocks.into_boxed_slice(),
        kind: container_kind,
    };

    Ok(container)
}

fn parse_section(scanner: &mut Scanner) -> ParseResult<Section> {
    let section_start = scanner.position();
    let slashes = eat_section_slashes(scanner)?;
    let section_level = slashes.len() - 1;
    if section_level == 0 {
        return parse_err!(DocumentHeaderNotAtStart, section_start);
    }

    //TODO: reject level 3 and beyond

    scanner.skip_while_on_char(SPACE);

    let name = parse_markup_text(scanner, TextMode::Title)?;

    scanner.skip_while_on_char(SPACE);

    expect_char(scanner, NEW_LINE)?;
    scanner.skip_while_on_char(SPACE);
    expect_char(scanner, NEW_LINE)?;

    let mut elements = Vec::new();

    while scanner.has_input() {
        if scanner.is_on_one_of(WHITESPACE_CHARS) {
            scanner.skip_char();
        } else if scanner.is_on_char(SLASH)
            && !(section_level == 1 && scanner.is_on_str(TRIPPLE_SLASH))
        {
            break;
        } else {
            let element = parse_element(scanner)?;
            elements.push(element);
        }
    }

    let section = Section {
        content: elements.into_boxed_slice(),
        title: name.to_string(),
        level: section_level,
    };

    Ok(section)
}

fn parse_block(scanner: &mut Scanner) -> ParseResult<Block> {
    let block = if scanner.is_on_char(HASH) {
        let block_position = scanner.position();
        scanner.skip_char();
        let block_name = eat_identifier(scanner)?;
        expect_char(scanner, NEW_LINE)?;

        match block_name {
            "paragraph" => parse_paragraph(scanner),
            "list" => parse_list(scanner),
            _ => parse_err!(UnknownBlock(block_name.into()), block_position),
        }
    } else if scanner.is_on_char(DASH) {
        parse_list(scanner)
    } else if scanner.is_on_str(DELIMITED_CONTAINER_END) {
        parse_err!(ContainerMissingStart, scanner.position())
    } else if !scanner.has_input() {
        parse_err!(UnexpectedEndOfInput, scanner.position())
    } else {
        parse_paragraph(scanner)
    }?;

    scanner.skip_while_on_char(SPACE);

    if scanner.has_input() {
        expect_char(scanner, NEW_LINE)?;
    }

    if scanner.has_input() && !scanner.is_on_str(DELIMITED_CONTAINER_END) {
        scanner.skip_while_on_char(SPACE);
        expect_char(scanner, NEW_LINE)?;
    }

    Ok(block)
}

fn parse_paragraph(scanner: &mut Scanner) -> ParseResult<Block> {
    if scanner.is_on_one_of(WHITESPACE_CHARS) {
        return parse_err!(WhitespaceAtParagraphStart, scanner.position());
    }

    let text_runs = parse_text_runs(scanner, TextMode::Paragraph)?;
    Ok(Block::Paragraph(text_runs))
}

fn parse_text_runs(scanner: &mut Scanner, mode: TextMode) -> ParseResult<Box<[TextRun]>> {
    let mut text_runs = Vec::new();

    while on_text_run(scanner, mode) {
        let run = if scanner.is_on_one_of(STYLE_DELIMITER_CHARS) {
            parse_styled_text_run(scanner, mode)
        } else if scanner.is_on_char(BACKTICK) {
            parse_inline_raw_text_run(scanner)
        } else if scanner.is_on_char(LEFT_SQUARE_BRACKET) {
            parse_linked_text_run(scanner, mode)
        } else {
            parse_plain_text_run(scanner, mode)
        }?;

        text_runs.push(run);
    }

    Ok(text_runs.into_boxed_slice())
}

fn parse_list(scanner: &mut Scanner) -> ParseResult<Block> {
    //TODO: is this stack based solution slightly incongruous with
    // the rest of the parser that just uses recursive functions?
    let mut stack = ListStack::new();

    while on_list_item(scanner) {
        let mut space_count = 0;
        let start_of_line = scanner.position();
        while scanner.is_on_char(SPACE) {
            space_count += 1;
            scanner.skip_char();
        }

        if space_count % 2 != 0 {
            return parse_err!(UnevenListIndent(space_count), start_of_line);
        }

        expect_char(scanner, DASH)?;
        scanner.skip_while_on_char(SPACE);

        let indent = space_count / 2;

        if indent == stack.indent + 1 {
            stack.push();
        } else if indent < stack.indent {
            for _ in 0..(stack.indent - indent) {
                stack.pop();
            }
        } else if indent != stack.indent {
            return parse_err!(MissingListLevel((stack.indent, indent)), start_of_line);
        }

        let text = parse_text_runs(scanner, TextMode::List)?;
        stack.add_text(text);

        scanner.skip_while_on_char(SPACE);

        if scanner.is_on_char(NEW_LINE) {
            scanner.skip_char();
        }
    }

    let list = stack.collect();
    Ok(Block::List(list))
}

fn on_list_item(scanner: &Scanner) -> bool {
    let mut peek = scanner.peek();
    peek.skip_while_on_char(SPACE);
    peek.is_on_char(DASH)
}

fn parse_plain_text_run(scanner: &mut Scanner, mode: TextMode) -> ParseResult<TextRun> {
    let run = parse_markup_text(scanner, mode)?;

    let run = TextRun {
        text: run,
        style: Style::None,
    };

    Ok(run)
}

fn parse_markup_text(scanner: &mut Scanner, mode: TextMode) -> ParseResult<String> {
    let mut run = String::new();

    loop {
        if scanner.is_on_char(BACKSLASH) {
            scanner.skip_char();
            let escaped = eat_char(scanner)?;
            run.push(escaped);
        } else if scanner.is_on(char_usable_in_text_frag) {
            let text = eat_text_fragment(scanner)?;
            run.push_str(text);
        } else if let Some(position) = try_eat_text_space(scanner, mode) {
            scanner.advance_to(&position);
            run.push(SPACE);
        } else {
            break;
        }
    }

    Ok(run)
}

fn parse_styled_text_run(scanner: &mut Scanner, mode: TextMode) -> ParseResult<TextRun> {
    let delimiter_pos = scanner.position();
    let delimiter = eat_char(scanner)?;

    let style = match delimiter {
        ASTERISK => Style::Strong,
        UNDERSCORE => Style::Emphasis,
        TILDE => Style::Strikethrough,
        _ => return parse_err!(InvalidStyleDelimiter(delimiter), delimiter_pos),
    };

    let run = parse_markup_text(scanner, mode)?;

    expect_char(scanner, delimiter)?;

    if run.starts_with(SPACE) || run.ends_with(SPACE) {
        return parse_err!(LooseDelimiter, delimiter_pos);
    }

    if run.is_empty() {
        return parse_err!(EmptyDelimitedText, delimiter_pos);
    }

    Ok(TextRun { text: run, style })
}

fn parse_linked_text_run(scanner: &mut Scanner, mode: TextMode) -> ParseResult<TextRun> {
    let delimiter_pos = scanner.position();

    expect_char(scanner, LEFT_SQUARE_BRACKET)?;

    let run = parse_markup_text(scanner, mode)?;
    if run.starts_with(SPACE) || run.ends_with(SPACE) {
        return parse_err!(LooseDelimiter, delimiter_pos);
    }

    expect_char(scanner, RIGHT_SQUARE_BRACKET)?;
    expect_char(scanner, AT_SIGN)?;
    let id = eat_identifier(scanner)?;
    Ok(TextRun {
        text: run,
        style: Style::Link(id.into()),
    })
}

//IDEA: Maybe this would all be easier if we "compressed" the input stream
// by run length encoding whitespace...
// e.g current_char could be a Trinary of Char/Whitespace/None
// then both interogating the composition of the whitespace and
// peeking past it are both trivial
// Doesn't even have to be full RLE, could just store if SPACE | SINGLE BREAK | BLOCK
// Equally it might be worth storing a str slice to the underlying whitespace string
//
// SO... we need to be able to look through whitespace if we are on it
// and do any of the normal things we are used to doing on the sub string on the other side
// but we also need to know about the composition of the whitespace

// enum Blah {
//     Char(&'a str)
//     Whitespace(&'a str, WhiteSpaceKind)
// }

// or
// enum Blah {
//     Char(usize)
//     Whitespace(usize, WhiteSpaceKind)
// }
// enum WhitespaceKind {
//     Spaces
//     SingleBreak
//     MultiBreak
// }
//
// Maybe a better would be to use the concept of read heads
// Maintain:
// - A current head
// - start of next whitespace head
// - After next whitespace head
//
// Main challenge is to figure out how to cascade reads from the first head
// to the last, given that the space between the heads might be arbitrary
//
// Unless... we just dont do that? Ok to re tread old ground as the whole
// string will be in memory anyway?
//
// Another aproach would be to only position the forward heads on demand
//
// Otherwise we have to figure out when to advance them.

fn on_text_run(scanner: &Scanner, mode: TextMode) -> bool {
    let on_space = scanner.is_on_one_of(WHITESPACE_CHARS);
    let on_block_start = scanner.is_on_char(HASH);
    let on_text_fragment = scanner.has_input() && !on_space && !on_block_start;
    let on_leading_whitespace = try_eat_text_space(scanner, mode).is_some();

    on_text_fragment || on_leading_whitespace
    //TODO: ideal would be as follows
    // (maybe leveraging RLE of whitespace)
    // (then maybe dont need our own peek API, just peekable iter on CharIndices)
    // (to work this needs whitespace be the only thing we peek over)
    // scanner.is_on(char_usable_in_text_run) || scanner.is_on_text_space_leading_to(char_usable_in_text_run)
}

//TODO: This is getting silly - passing a function would be easier to follow?
fn try_eat_text_space<'a>(scanner: &Scanner<'a>, mode: TextMode) -> Option<Scanner<'a>> {
    if !scanner.is_on_one_of(WHITESPACE_CHARS) {
        return None;
    }

    let mut peek = scanner.peek();
    let mut has_new_line = false;

    peek.skip_while_on_char(SPACE);

    if mode == TextMode::Title {
        if peek.is_on(char_usable_in_text_frag) {
            return Some(peek);
        } else {
            return None;
        }
    }

    if peek.is_on_char(NEW_LINE) {
        has_new_line = true;
        peek.skip_char();
    }

    peek.skip_while_on_char(SPACE);

    if mode == TextMode::List && has_new_line && peek.is_on_char(DASH) {
        return None;
    }

    let on_blockbreak = peek.is_on_char(NEW_LINE);
    let on_container_end = peek.is_on_str(DELIMITED_CONTAINER_END);
    let more_text_ahead = peek.has_input() && !on_blockbreak && !on_container_end;

    if more_text_ahead {
        return Some(peek);
    }

    None
}

fn parse_inline_raw_text_run(scanner: &mut Scanner) -> ParseResult<TextRun> {
    let mut run = String::new();
    let run_start = scanner.position();
    expect_char(scanner, BACKTICK)?;

    loop {
        if scanner.is_on_char(BACKTICK) {
            scanner.skip_char();
            break;
        } else if scanner.is_on_char(NEW_LINE) {
            let mut space_count = 1;
            scanner.skip_char();

            while scanner.is_on_char(SPACE) {
                space_count += 1;
                scanner.skip_char();
            }

            if scanner.is_on_char(NEW_LINE) {
                return parse_err!(InlineRawHasBlockBreak, scanner.position());
            } else {
                for _ in 0..space_count {
                    run.push(SPACE);
                }
            }
        } else if scanner.is_on_char(SPACE) {
            let space = eat_space(scanner)?;
            run.push_str(space);
        } else {
            let text = eat_raw_fragment(scanner)?;
            run.push_str(text);
        }
    }

    if run.is_empty() {
        return parse_err!(EmptyDelimitedText, run_start);
    }

    Ok(TextRun {
        text: run,
        style: Style::Raw,
    })
}

fn parse_references(scanner: &mut Scanner) -> ParseResult<Vec<Reference>> {
    expect_char(scanner, AT_SIGN)?;
    let ident_position = scanner.position();
    let ident = eat_identifier(scanner)?;
    if ident != "references" {
        return parse_err!(ExpectedReferencesBlock, ident_position);
    }

    expect_char(scanner, NEW_LINE)?;

    let mut references = Vec::new();

    loop {
        let id = eat_identifier(scanner)?;
        scanner.skip_while_on_char(SPACE);
        expect_char(scanner, DASH)?;
        expect_char(scanner, GREATER_THAN)?;
        scanner.skip_while_on_char(SPACE);

        let link = eat_link(scanner)?;

        references.push(Reference {
            id: id.into(),
            link: link.into(),
        });

        if scanner.has_input() {
            expect_char(scanner, NEW_LINE)?;
        }

        if scanner.is_on(char_usable_in_identifier) {
            continue;
        } else {
            break;
        }
    }

    Ok(references)
}

struct ListStack {
    stack: Vec<Vec<ListItem>>,
    items: Vec<ListItem>,
    indent: usize,
}

impl ListStack {
    fn new() -> Self {
        ListStack {
            stack: Vec::new(),
            items: Vec::new(),
            indent: 0,
        }
    }

    fn add_text(&mut self, text: Box<[TextRun]>) {
        let text = ListItem::Text(text);
        self.items.push(text);
    }

    fn push(&mut self) {
        let parent = std::mem::take(&mut self.items);
        self.stack.push(parent);
        self.indent += 1;
    }

    fn pop(&mut self) {
        let parent = self.stack.pop().expect("parent list");
        let sub_list = std::mem::replace(&mut self.items, parent);
        let sub_list = sub_list.into_boxed_slice();
        let sub_list = ListItem::SubList(sub_list);
        self.items.push(sub_list);
        self.indent -= 1;
    }

    fn collect(mut self) -> Box<[ListItem]> {
        while !self.stack.is_empty() {
            self.pop();
        }
        self.items.into_boxed_slice()
    }
}

#[cfg(test)]
mod test {
    use super::*;

    macro_rules! document {
        ($($token:tt)+) => {
            build_document!({} $($token)+)
        };
    }

    macro_rules! build_document {
        ({$($fields:tt)*}) => {
            Document {
                $($fields)*
              ..Default::default()
            }
        };

        ({$($fields:tt)*} $field:ident : { $($token:tt)+ } $(, $($tail:tt)+)?) => {
            build_document!(
                {
                    //TODO: can we return the field from document_field (foo: bar)
                    $field: document_field!($field $($token)+),
                    $($fields)*
                }
                $($($tail)+)?
            )

        };
    }

    macro_rules! document_field {
        (metadata $($token:tt)+) => {
            build_metadata!({} $($token)+)
        };
        (contents  $($token:tt)+) => {
            build_contents!($($token)+)
        };
        (references $($token:tt)+) => {
            build_references!($($token)+)
        };
    }

    macro_rules! build_metadata {
        ( {$($fields:tt)*} $field:ident : $value:expr $(, $($tail:tt)*)?) => {
            build_metadata!(
                {
                    $field: metadata_arg!($field $value),
                    $($fields)*
                }
                $($($tail)*)?
            )
        };

        ( {$($fields:tt)*}) => {
            Metadata{
                $($fields)*
              ..Default::default()
            }
        };
    }

    macro_rules! metadata_arg {
        (tags $tags:expr) => {
            Some(Box::new($tags.map(|t| t.into())))
        };

        ($field:ident $value:expr) => {
            Some($value.into())
        };
    }

    macro_rules! build_contents {
        (
            $(
              $element_type:ident
              $(($element_name:expr))?
              { $($element_content:tt)* }
            ),*

        ) => {
            Box::new(
                [$(element!(
                    $element_type
                    $(($element_name))?
                    $($element_content)*
                ),)*]
            )
        };
    }

    macro_rules! build_references {
        (
            $( ($ref_id:expr, $ref_link:expr) ),*
            $(,)?
        ) => {
            Box::new(
                [$(Reference {
                    id: $ref_id.to_string(),
                    link: $ref_link.to_string(),
                },)*]
            )
        };
    }

    macro_rules! element {
        (info $( $block:ident { $($content:tt)* } $(,)? )*) => {
            Element::Container(Container{
                content: Box::new([
                    $(
                        block!($block $($content)*),
                    )*
                ]),
                kind: ContainerKind::Info,
            })
        };

        (section ($name:expr) $( $element:ident $(($element_name:expr))? { $($content:tt)* } $(,)? )*) => {
            Element::Section(Section{
                content: Box::new([
                    $(
                        element!($element $(($element_name))? $($content)*),
                    )*
                ]),
                level: 1,
                title: String::from($name)
            })
        };

        (subsection ($name:expr) $( $element:ident $(($element_name:expr))? { $($content:tt)* } $(,)? )*) => {
            Element::Section(Section{
                content: Box::new([
                    $(
                        element!($element $(($element_name))? $($content)*),
                    )*
                ]),
                level: 2,
                title: String::from($name)
            })
        };

        ($block:ident $($content:tt)*) => {
            Element::Block(block!($block $($content)*))
        };


    }

    macro_rules! block {
        (paragraph $($text:expr),* $(,)?) => {
            Block::Paragraph(Box::new([
                $(
                    $text,
                )*
            ]))
        };

        (list $($item:ident { $($content:tt)* } $(,)?)*) => {
            Block::List(Box::new([
                $(
                    list_item!($item $($content)*),
                )*
            ]))
        };
    }

    macro_rules! list_item {
        (paragraph $($text:expr),* $(,)?) => {
            ListItem::Text(Box::new([
                $(
                    $text,
                )*
            ]))
        };

        (list $($item:ident { $($content:tt)* } $(,)?)*) => {
            ListItem::SubList(Box::new([
                $(
                    list_item!($item $($content)*),
                )*
            ]))
        };
    }

    macro_rules! info {
        ($($content:tt)*) => {
            Document {
                contents: Box::new([element!(info $($content)*)]),
                ..Default::default()
            }
        }
    }

    macro_rules! list {
        ($($content:tt)*) => {
            Document {
                contents: Box::new([element!(list $($content)*)]),
                ..Default::default()
            }
        }
    }

    macro_rules! paragraph {
        ($($content:tt)*) => {
            Document {
                contents: Box::new([element!(paragraph $($content)*)]),
                ..Default::default()
            }
        }
    }

    macro_rules! elements {
        (
            $(
              $element_type:ident
              $(($element_name:expr))?
              { $($element_content:tt)* }
            ),*
            $(,)?
        ) => {
            Document {
                contents: Box::new(
                    [$(element!(
                        $element_type
                        $(($element_name))?
                        $($element_content)*
                    ),)*]
                ),
                ..Default::default()
            }
        }
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

    fn linked_text(text: &str, reference: &str) -> TextRun {
        TextRun {
            text: text.to_string(),
            style: Style::Link(reference.to_string()),
        }
    }

    fn assert_parse_succeeds<T: Into<Document>>(input: &'static str, expected: T) {
        let expected = expected.into();
        let result = parse_str(input);

        match result {
            Ok(doc) => {
                if doc != expected {
                    eprintln!("Actual:\n{:#?}", doc);
                    eprintln!("Expected:\n{:#?}", expected);
                    panic!("Parsed document not what was expected")
                }
            }
            Err(error) => {
                eprintln!("{}", error);
                panic!("parse unexpectedly failed")
            }
        }
    }

    fn assert_parse_fails(input: &'static str, expected: ErrorKind) {
        let result = parse_str(input);

        match result {
            Ok(doc) => {
                eprintln!("Expected parse to fail, but got doc:");
                eprintln!("{:?}\n", doc);
                panic!("parse unexpectedly succeeded")
            }
            Err(err) => {
                if err.kind != expected {
                    eprintln!("Expected error: {:?}", expected);
                    eprintln!("Actual error: {:?}", err.kind);

                    eprintln!("Full failure detail:\n{}", err);

                    panic!("Failed with wrong kind of error")
                }
            }
        }
    }

    //TODO: See if we can group / order these ever growing tests...

    #[test]
    fn complete_doc_test() {
        let input = concat!(
            "/ Feline friendly flower arranging\n",
            "id: 01.42\n",
            "\n",
            "!info\n",
            "Did you know flower pots are for *more*\n",
            "than simply knocking on the floor?\n",
            "\n",
            "Opposable thumbs\n",
            "are useful?\n",
            "\n",
            "- Nose\n",
            "- Toes\n",
            "  - Big one\n",
            "  - Little one\n",
            "  - _Other_\n",
            "     one\n"
        );

        let expected = document!(
            metadata: {
                id: "01.42",
                title: "Feline friendly flower arranging"
            },
            contents: {
                info {
                    paragraph {
                        text("Did you know flower pots are for "),
                        strong_text("more"),
                        text(" than simply knocking on the floor?")
                    }
                },
                paragraph {
                    text("Opposable thumbs are useful?")
                },
                list {
                    paragraph { text("Nose") },
                    paragraph { text("Toes") },
                    list {
                        paragraph { text("Big one") },
                        paragraph { text("Little one") },
                        paragraph {
                            emphasised_text("Other"),
                            text(" one")
                        }
                    }
                }
            }
        );

        assert_parse_succeeds(input, expected);
    }

    #[test]
    fn one_line_paragraph() {
        let input = "We like cats very much";

        let expected = paragraph! { text("We like cats very much") };

        assert_parse_succeeds(input, expected);
    }

    #[test]
    fn explicit_paragraph() {
        let input = "#paragraph\nCats go meeow!";

        let expected = paragraph! { text("Cats go meeow!") };

        assert_parse_succeeds(input, expected);
    }

    #[test]
    fn explicit_paragraph_with_block_break_before_text_is_rejected() {
        let input = "#paragraph\n\nCats go meeow!";

        let expected = WhitespaceAtParagraphStart;

        assert_parse_fails(input, expected);
    }

    #[test]
    fn unknown_block_is_rejected() {
        let input = "#feline\nMeow?";

        let expected = UnknownBlock("feline".into());

        assert_parse_fails(input, expected);
    }

    #[test]
    fn unknown_block_starting_with_m_is_rejected() {
        let input = "#meowograph\nCats go meeow!";

        let expected = UnknownBlock("meowograph".into());

        assert_parse_fails(input, expected);
    }

    #[test]
    fn empty_block_name_is_rejected() {
        let input = "#\nHi";

        let expected = ExpectedIdentifier;

        assert_parse_fails(input, expected);
    }

    #[test]
    fn block_without_new_line_is_rejected() {
        let input = "#paragraph";

        let expected = ExpectedChar('\n');

        assert_parse_fails(input, expected);
    }

    #[test]
    fn double_space() {
        let input = "Nice  kitty!";

        let expected = paragraph! { text("Nice kitty!") };

        assert_parse_succeeds(input, expected);
    }

    #[test]
    fn trailing_new_line_is_ignored() {
        let input = "Cats\n";

        let expected = paragraph! { text("Cats") };

        assert_parse_succeeds(input, expected);
    }

    #[test]
    fn space_then_trailing_new_line_is_ignored() {
        let input = "Cats \n";

        let expected = paragraph! { text("Cats") };

        assert_parse_succeeds(input, expected);
    }

    #[test]
    fn new_line_becomes_whitespace() {
        let input = "Cats\nwhiskers";

        let expected = paragraph! { text("Cats whiskers") };

        assert_parse_succeeds(input, expected);
    }

    #[test]
    fn new_line_becomes_whitespace_given_plain_then_styled() {
        let input = "Cats\n*whiskers*";

        let expected = paragraph! {
            text("Cats "),
            strong_text("whiskers"),
        };

        assert_parse_succeeds(input, expected);
    }

    #[test]
    fn new_line_becomes_whitespace_given_plain_then_raw() {
        let input = "Cats\n`nice whiskers`";

        let expected = paragraph! {
            text("Cats "),
            raw_text("nice whiskers"),
        };

        assert_parse_succeeds(input, expected);
    }

    #[test]
    fn new_line_with_extra_whitespace_collapses() {
        let input = "Cats    \n    whiskers";
        let expected = paragraph! { text("Cats whiskers") };

        assert_parse_succeeds(input, expected);
    }

    #[test]
    fn two_new_lines_become_blocks() {
        let input = "Cats\n\nwhiskers";

        let expected = elements! {
            paragraph { text("Cats") },
            paragraph { text("whiskers") }
        };

        assert_parse_succeeds(input, expected);
    }

    #[test]
    fn three_new_lines_becomes_blocks() {
        let input = "Cats\n\n\nwhiskers";

        let expected = elements! {
            paragraph { text("Cats") },
            paragraph { text("whiskers") }
        };

        assert_parse_succeeds(input, expected);
    }

    #[test]
    fn two_new_lines_with_whitespace_is_treated_as_blockbreak() {
        let input = "Cats\n \nwhiskers";

        let expected = elements! {
            paragraph { text("Cats") },
            paragraph { text("whiskers") }
        };

        assert_parse_succeeds(input, expected);
    }

    #[test]
    fn blockbreak_with_extra_whitespace() {
        let input = "Cats  \n    \n  whiskers";
        let expected = elements!(
            paragraph { text("Cats") },
            paragraph { text("whiskers") }
        );

        assert_parse_succeeds(input, expected);
    }

    #[test]
    fn missing_blockbreak_is_rejected() {
        let input = concat!(
            "Cats can sometimes be\n",
            "#paragraph\n",
            "ever so surprising\n"
        );

        let expected = ExpectedChar('\n');

        assert_parse_fails(input, expected);
    }

    #[test]
    fn escaped_char() {
        let input = "\\A";

        let expected = paragraph! { text("A") };

        assert_parse_succeeds(input, expected);
    }

    #[test]
    fn escaped_hash_in_markup() {
        let input = "My cat does backflips \\#coolcat";

        let expected = paragraph! { text("My cat does backflips #coolcat") };

        assert_parse_succeeds(input, expected);
    }

    #[test]
    fn escaped_underscore() {
        let input = "cat\\_case";

        let expected = paragraph! { text("cat_case") };

        assert_parse_succeeds(input, expected);
    }

    #[test]
    fn escaped_underscore_in_emphasis() {
        let input = "_cat\\_case_";

        let expected = paragraph! { emphasised_text("cat_case") };

        assert_parse_succeeds(input, expected);
    }

    #[test]
    fn escaped_ignored_in_raw() {
        let input = "`cat\\_case`";

        let expected = paragraph! { raw_text("cat\\_case") };

        assert_parse_succeeds(input, expected);
    }

    #[test]
    fn emphasised_words() {
        let input = "We _totally adore_ them";

        let expected = paragraph! {
            text("We "),
            emphasised_text("totally adore"),
            text(" them"),
        };

        assert_parse_succeeds(input, expected);
    }

    #[test]
    fn emphasis_at_end_of_line() {
        let input = "Cats like to _zoom_\naround";

        let expected = paragraph! {
            text("Cats like to "),
            emphasised_text("zoom"),
            text(" around"),
        };

        assert_parse_succeeds(input, expected);
    }

    #[test]
    fn strong_words() {
        let input = "I *need to pet that cat* right away.";

        let expected = paragraph! {
            text("I "),
            strong_text("need to pet that cat"),
            text(" right away."),
        };

        assert_parse_succeeds(input, expected);
    }

    #[test]
    fn strong_mid_word() {
        let input = "I said: mee*ooOOo*ww!";

        let expected = paragraph! {
            text("I said: mee"),
            strong_text("ooOOo"),
            text("ww!"),
        };

        assert_parse_succeeds(input, expected);
    }

    #[test]
    fn strong_over_two_lines() {
        let input = "*me\now*";

        let expected = paragraph! { strong_text("me ow") };

        assert_parse_succeeds(input, expected);
    }

    #[test]
    fn strikethrough_words() {
        let input = "Cats are ~ok i guess~ magnificant";

        let expected = paragraph! {
            text("Cats are "),
            strikethrough_text("ok i guess"),
            text(" magnificant"),
        };

        assert_parse_succeeds(input, expected);
    }

    #[test]
    fn raw_words() {
        let input = "Robot cat says `bleep bloop`!";

        let expected = paragraph! {
            text("Robot cat says "),
            raw_text("bleep bloop"),
            text("!"),
        };

        assert_parse_succeeds(input, expected);
    }

    #[test]
    fn raw_mid_word() {
        let input = "Bl`eeee`p!";

        let expected = paragraph! {
            text("Bl"),
            raw_text("eeee"),
            text("p!"),
        };

        assert_parse_succeeds(input, expected);
    }

    #[test]
    fn underscore_in_raw() {
        let input = "Set `PURR_LOUDLY` to true";

        let expected = paragraph! {
            text("Set "),
            raw_text("PURR_LOUDLY"),
            text(" to true"),
        };

        assert_parse_succeeds(input, expected);
    }

    #[test]
    fn extra_spaces_in_raw() {
        let input = "`Keep your       distance`";

        let expected = paragraph! { raw_text("Keep your       distance") };

        assert_parse_succeeds(input, expected);
    }

    #[test]
    fn raw_over_two_lines() {
        let input = "`Great\ncats`";

        let expected = paragraph! { raw_text("Great cats") };

        assert_parse_succeeds(input, expected);
    }

    #[test]
    fn strikethrough_over_two_lines() {
        let input = "~Great\ndogs~";

        let expected = paragraph! { strikethrough_text("Great dogs") };

        assert_parse_succeeds(input, expected);
    }

    #[test]
    fn raw_leading_with_new_line() {
        let input = "`\nMeow?`";

        let expected = paragraph! { raw_text(" Meow?") };

        assert_parse_succeeds(input, expected);
    }

    #[test]
    fn raw_trailing_with_new_line() {
        let input = "`Meow\n`";

        let expected = paragraph! { raw_text("Meow ") };

        assert_parse_succeeds(input, expected);
    }

    #[test]
    fn raw_leading_with_space() {
        let input = "` Meow`";

        let expected = paragraph! { raw_text(" Meow") };

        assert_parse_succeeds(input, expected);
    }

    #[test]
    fn raw_trailing_with_space() {
        let input = "`Meow `";

        let expected = paragraph! { raw_text("Meow ") };

        assert_parse_succeeds(input, expected);
    }

    #[test]
    fn raw_over_three_lines() {
        let input = "`Great\ncats\nassemble!`";

        let expected = paragraph! { raw_text("Great cats assemble!") };

        assert_parse_succeeds(input, expected);
    }

    #[test]
    fn standalone_dash() {
        let input = "Felines - fantastic!";

        let expected = paragraph! { text("Felines - fantastic!") };

        assert_parse_succeeds(input, expected);
    }

    #[test]
    fn paragraph_with_trailing_whitespace() {
        let input = "Cool kitty   ";

        let expected = paragraph! { text("Cool kitty") };

        assert_parse_succeeds(input, expected);
    }

    #[test]
    fn underscore_in_awkward_places() {
        let input = "Cat cat_cat cat_ cat.";

        let expected = paragraph! {
            text("Cat cat"),
            emphasised_text("cat cat"),
            text(" cat.")
        };

        assert_parse_succeeds(input, expected);
    }

    #[test]
    fn new_line_then_multiple_spaces_in_plain_text() {
        let input = "Cat\n  cat";

        let expected = paragraph! { text("Cat cat") };

        assert_parse_succeeds(input, expected);
    }

    #[test]
    fn new_line_then_multiple_spaces_in_styled() {
        let input = "*Cat\n  cat*";

        let expected = paragraph! { strong_text("Cat cat") };

        assert_parse_succeeds(input, expected);
    }

    #[test]
    fn new_line_then_multiple_spaces_in_raw() {
        let input = "`Cat\n  cat`";

        let expected = paragraph! { raw_text("Cat   cat") };

        assert_parse_succeeds(input, expected);
    }

    #[test]
    fn multiple_spaces_then_new_line_in_plain_text() {
        let input = "Cat  \ncat";

        let expected = paragraph! { text("Cat cat") };

        assert_parse_succeeds(input, expected);
    }

    #[test]
    fn multiple_spaces_then_new_line_in_styled() {
        let input = "*Cat  \ncat*";

        let expected = paragraph! { strong_text("Cat cat") };

        assert_parse_succeeds(input, expected);
    }

    #[test]
    fn multiple_spaces_then_new_line_in_raw() {
        let input = "`Cat  \ncat`";

        let expected = paragraph! { raw_text("Cat   cat") };

        assert_parse_succeeds(input, expected);
    }

    #[test]
    fn empty_emphasis() {
        let input = "Rules cats must follow: __.";

        let expected = EmptyDelimitedText;

        assert_parse_fails(input, expected);
    }

    #[test]
    fn empty_raw() {
        let input = "Robot cat says: ``!.";

        let expected = EmptyDelimitedText;

        assert_parse_fails(input, expected);
    }

    #[test]
    fn raw_with_double_linebreak() {
        let input = "`Erm...\n\nmeow?`";

        let expected = InlineRawHasBlockBreak;

        assert_parse_fails(input, expected);
    }

    #[test]
    fn raw_with_double_linebreak_containing_whitespace() {
        let input = "`Erm...\n \nmeow?`";

        let expected = InlineRawHasBlockBreak;

        assert_parse_fails(input, expected);
    }

    #[test]
    fn strikethrough_with_double_linebreak() {
        let input = "~Erm...\n\nmeow?~";

        let expected = ExpectedChar('~');

        assert_parse_fails(input, expected);
    }

    #[test]
    fn unmatched_emphasis_1() {
        let input = "_.";

        let expected = ExpectedChar('_');

        assert_parse_fails(input, expected);
    }

    #[test]
    fn unmatched_emphasis_2() {
        let input = "meow _meow.";

        let expected = ExpectedChar('_');

        assert_parse_fails(input, expected);
    }

    #[test]
    fn unmatched_emphasis_3() {
        let input = "meow meow_";

        let expected = ExpectedChar('_');

        assert_parse_fails(input, expected);
    }

    #[test]
    fn nested_styled_text() {
        let input = "_*meow!*_";

        let expected = ExpectedChar('_');

        assert_parse_fails(input, expected);
    }

    #[test]
    fn loose_strong_delimiter_start() {
        let input = "* meow meow*";

        let expected = LooseDelimiter;

        assert_parse_fails(input, expected);
    }

    #[test]
    fn loose_strong_delimiter_end() {
        let input = "*meow meow *";

        let expected = LooseDelimiter;

        assert_parse_fails(input, expected);
    }

    #[test]
    fn raw_immediately_in_emphasis() {
        let input = "_``_";

        let expected = ExpectedChar('_');

        assert_parse_fails(input, expected);
    }

    #[test]
    fn raw_within_in_emphasis() {
        let input = "_a``a_";

        let expected = ExpectedChar('_');

        assert_parse_fails(input, expected);
    }

    #[test]
    fn doc_with_leading_new_line() {
        let input = "\nCats cats cats";

        let expected = paragraph! { text("Cats cats cats") };

        assert_parse_succeeds(input, expected);
    }

    #[test]
    fn doc_with_leading_new_lines() {
        let input = "\n\nCats cats cats";

        let expected = paragraph! { text("Cats cats cats") };

        assert_parse_succeeds(input, expected);
    }

    #[test]
    fn doc_with_leading_spaces_and_new_line() {
        let input = "   \nCats cats cats";

        let expected = paragraph! { text("Cats cats cats") };

        assert_parse_succeeds(input, expected);
    }

    #[test]
    fn doc_ending_with_new_line() {
        let input = "Cats are friends\n";

        let expected = paragraph! { text("Cats are friends") };

        assert_parse_succeeds(input, expected);
    }

    #[test]
    fn doc_ending_with_new_lines() {
        let input = "Feline friends\n\n";

        let expected = paragraph! { text("Feline friends") };

        assert_parse_succeeds(input, expected);
    }

    #[test]
    fn new_line_and_space_between_styled_and_plain_text_runs() {
        let input = "*Cat*\n cat";

        let expected = paragraph! {
            strong_text("Cat"),
            text(" cat"),
        };

        assert_parse_succeeds(input, expected);
    }

    #[test]
    fn leading_whitespace_on_paragraph_is_ignored() {
        let input = "Cat\n\n  cat";

        let expected = elements!(
            paragraph { text("Cat") },
            paragraph { text("cat") }
        );

        assert_parse_succeeds(input, expected);
    }

    #[test]
    fn doc_header() {
        let input = concat!(
            "/ Practical espionage for felines in urban settings\n",
            "id: 01.23\n",
        );

        let expected = document!(
            metadata: {
                id: "01.23",
                title: "Practical espionage for felines in urban settings",
            }
        );

        assert_parse_succeeds(input, expected);
    }

    #[test]
    fn doc_header_with_folowing_para() {
        let input = concat!(
            "/ Some Doc\n",
            "id: 01.23\n",
            "\n",
            "\n",
            "\n",
            "Hello cats and kittens"
        );

        let expected = document!(
            metadata: {
                title:"Some Doc",
                id: "01.23",
            },
            contents: {
                paragraph { text("Hello cats and kittens") }
            }
        );

        assert_parse_succeeds(input, expected);
    }

    #[test]
    fn doc_header_with_tags() {
        let input = concat!(
            "/ Feasts for felines\n",
            "tags: cooking | eating | nice-smells\n",
        );

        let expected = document!(
            metadata: {
                title:"Feasts for felines",
                tags: ["cooking", "eating", "nice-smells"],
            }
        );

        assert_parse_succeeds(input, expected);
    }

    #[test]
    fn doc_header_with_worky_spacing() {
        let input = "/My Very   Cool Document   \nid :01.23\n";

        let expected = document!(
            metadata: {
                id:"01.23",
                title:"My Very Cool Document",
            }
        );

        assert_parse_succeeds(input, expected);
    }

    #[test]
    fn doc_header_with_no_trailing_newline() {
        let input = "/Doc";

        let expected = document!(
            metadata: {
                title: "Doc"
            }
        );

        assert_parse_succeeds(input, expected);
    }

    #[test]
    fn doc_header_not_at_start_is_rejected() {
        let input = concat!(
            "Hi!\n",
            "\n",
            "/ My Really Very Cool Document \n",
            "id: 01.23\n",
        );

        let expected = DocumentHeaderNotAtStart;

        assert_parse_fails(input, expected);
    }

    #[test]
    fn doc_header_with_unknown_metadata_is_rejected() {
        let input = "/Doc\nkibble: yes please\n";

        let expected = UnknownMetadata(String::from("kibble"));

        assert_parse_fails(input, expected);
    }

    #[test]
    fn multi_paragraph_info() {
        let input = concat!(
            "!info\n",
            ">>>\n",
            "Here are some facts...\n",
            "\n",
            "...about the cats!\n",
            "<<<"
        );

        let expected = info! [
            paragraph { text("Here are some facts...") },
            paragraph { text("...about the cats!") }
        ];

        assert_parse_succeeds(input, expected);
    }

    #[test]
    fn single_paragraph_info() {
        let input = concat!(
            "!info\n",
            "Did you know that cats sometimes like a nice long massage\n",
            "\n",
        );

        let expected = info! [
            paragraph {
                text(
                    "Did you know that cats sometimes like a nice long massage"
                )
            }
        ];

        assert_parse_succeeds(input, expected);
    }

    #[test]
    fn container_missing_start_is_rejected() {
        let input = "Silly cat\n<<<";

        let expected = ContainerMissingStart;

        assert_parse_fails(input, expected);
    }

    #[test]
    fn empty_container_is_rejected() {
        let input = "!info\n";

        let expected = EmptyContainer;

        assert_parse_fails(input, expected);
    }

    #[test]
    fn detactched_container_is_rejected() {
        let input = "!info\n\ncats!";

        let expected = EmptyContainer;

        assert_parse_fails(input, expected);
    }

    #[test]
    fn trailing_text_on_delimited_start_is_rejected() {
        let input = concat!(
            "!info\n",
            ">>>squeek\n",
            "Let me know if you find where I left my\n",
            "<<<"
        );

        let expected = ExpectedChar('\n');

        assert_parse_fails(input, expected);
    }

    #[test]
    fn trailing_text_on_delimited_end_is_rejected() {
        let input = concat!(
            "!info\n",
            ">>>\n",
            "Let me know if you find where I left my\n",
            "<<<toy"
        );

        let expected = ExpectedChar('\n');

        assert_parse_fails(input, expected);
    }

    #[test]
    fn missing_blockbreak_after_container_is_rejected() {
        let input = concat!(
            "!info\n",
            ">>>\n",
            "Let me know if you find where I left my\n",
            "<<<\n",
            "toy"
        );

        let expected = ExpectedChar('\n');

        assert_parse_fails(input, expected);
    }

    #[test]
    fn dash_in_paragraph_is_treated_as_part_of_text() {
        let input = "Ripley\n- Cat";

        let expected = paragraph! { text("Ripley - Cat") };

        assert_parse_succeeds(input, expected);
    }

    #[test]
    fn simple_list() {
        let input = concat!(
            "- Dry food is ok\n",
            "- Wet food is much better\n",
            "- Water is important also\n"
        );

        let expected = list! {
            paragraph { text("Dry food is ok")},
            paragraph { text("Wet food is much better")},
            paragraph { text("Water is important also")}
        };
        assert_parse_succeeds(input, expected);
    }

    #[test]
    fn explicit_list() {
        let input = concat!(
            "#list\n",
            "- Dry food is ok\n",
            "- Wet food is much better\n",
            "- Water is important also"
        );

        let expected = list! {
            paragraph { text("Dry food is ok") },
            paragraph { text("Wet food is much better") },
            paragraph { text("Water is important also") }
        };

        assert_parse_succeeds(input, expected);
    }

    #[test]
    fn dash_in_list_text_is_not_treated_as_bullet() {
        let input = concat!("- Meow - meow\n",);

        let expected = list! {paragraph { text("Meow - meow") }};

        assert_parse_succeeds(input, expected);
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

        let expected = list! {
            paragraph { text("Dry food is ok") },
            paragraph { text("Wet food is much better") },
            paragraph { text("Water is important also") },
        };

        assert_parse_succeeds(input, expected);
    }

    #[test]
    fn list_with_styled_text() {
        let input = concat!(
            "- Dry food is *ok*\n",
            "- Wet food is _much better_\n",
            "- Water is `important  also`\n"
        );

        let expected = list! {
            paragraph {
                text("Dry food is "),
                strong_text("ok"),
            }
            paragraph {
                text("Wet food is "),
                emphasised_text("much better"),
            }
            paragraph {
                text("Water is "),
                raw_text("important  also"),
            }
        };

        assert_parse_succeeds(input, expected);
    }

    #[test]
    fn list_with_sublist() {
        let input = concat!(
            "- Nice things to eat\n",
            "  - Tuna\n",
            "  - Chicken\n",
            "  - Beef\n",
        );

        let expected = list! {
            paragraph { text("Nice things to eat") }
            list {
                paragraph { text("Tuna") },
                paragraph { text("Chicken") },
                paragraph { text("Beef") },
            }
        };

        assert_parse_succeeds(input, expected);
    }

    #[test]
    fn list_with_subsublist() {
        let input = concat!(
            "- Nice things to eat\n",
            "  - Beef\n",
            "    - Hereford\n",
            "    - Wagyu\n",
        );

        let expected = list! {
            paragraph { text("Nice things to eat") },
            list {
                paragraph { text("Beef") },
                list {
                    paragraph { text("Hereford") },
                    paragraph { text("Wagyu") },
                }
            }
        };

        assert_parse_succeeds(input, expected);
    }

    #[test]
    fn list_with_raw_over_newline() {
        let input = "- f`oo\n  ba`r\n  - baz";

        let expected = list! {
            paragraph {
                text("f"),
                raw_text("oo   ba"),
                text("r"),
            },
            list { paragraph { text("baz") }}
        };

        assert_parse_succeeds(input, expected);
    }

    #[test]
    fn list_item_with_trailing_whitespace() {
        let input = "- Foo    \n- Bar";

        let expected = list! {
            paragraph { text("Foo")},
            paragraph { text("Bar")},
        };

        assert_parse_succeeds(input, expected);
    }

    #[test]
    fn list_with_raw_over_multiple_points() {
        let input = "- f`oo\n  -ba`r";

        let expected = list! {
            paragraph {
                text("f"),
                raw_text("oo   -ba"),
                text("r"),
            }
        };

        assert_parse_succeeds(input, expected);
    }

    #[test]
    fn list_with_emphasis_over_multiple_points() {
        let input = "- f_oo\n  -ba_r";

        let expected = ExpectedChar('_');

        assert_parse_fails(input, expected);
    }

    #[test]
    fn list_with_uneven_spaces() {
        let input = "-foo\n -bar";

        let expected = UnevenListIndent(1);

        assert_parse_fails(input, expected);
    }

    #[test]
    fn list_that_skips_ascending_indent_level() {
        let input = concat!(
            "- Nice things to eat\n",
            "    - Wagyu beef because it is oh so tender\n",
        );

        let expected = MissingListLevel((0, 2));

        assert_parse_fails(input, expected);
    }

    #[test]
    fn list_that_skips_decending_indent_level() {
        let input = concat!(
            "- Nice things to eat\n",
            "  - Beef\n",
            "    - Wagyu\n",
            "- Nice things to drink\n",
        );

        let expected = list! {
            paragraph { text("Nice things to eat") },
            list {
                paragraph { text("Beef") },
                list {
                    paragraph { text("Wagyu") }
                }
            }
            paragraph { text("Nice things to drink") }
        };

        assert_parse_succeeds(input, expected);
    }

    #[test]
    fn error_specifies_correct_row_and_column() {
        let input = "Silly cat\ngoes *_*";

        let expected = (7, 2);

        let error = parse_str(input).unwrap_err();
        let actual = (error.input_column, error.input_line);

        assert_eq!(actual, expected);
    }

    #[test]
    fn basic_link_with_reference() {
        let input = concat!(
            "@references\n",
            "Ripley2020 -> https://example.com\n",
            "\n",
            "For more info, consult [our guide on petting cats]@Ripley2020,\n",
            "created by our own in house experts.\n",
        );

        let expected = document!(
            contents: {
                paragraph {
                    text("For more info, consult "),
                    linked_text("our guide on petting cats", "Ripley2020"),
                    text(", created by our own in house experts.")
                }
            },
            references: {
                ("Ripley2020", "https://example.com")
            }
        );

        assert_parse_succeeds(input, expected);
    }

    #[test]
    fn whitespace_around_linked_text_is_rejected() {
        let input = concat!(
            "@references\n",
            "Ripley2020 -> https://example.com\n",
            "\n",
            "We like [ petting cats ]@Ripley2020 a lot.\n",
        );

        let expected = LooseDelimiter;
        assert_parse_fails(input, expected);
    }

    #[test]
    fn references_after_content_rejected() {
        let input = concat!(
            "For more info, consult [our guide on petting cats]@Ripley2020,\n",
            "created by our own in house experts.\n",
            "\n",
            "@references\n",
            "Ripley2020 -> https://example.com"
        );

        let expected = ReferencesOutOfPlace;

        assert_parse_fails(input, expected);
    }

    #[test]
    fn document_with_title_and_references() {
        let input = concat!(
            "/ Boots and\n",
            "\n",
            "@references\n",
            "Cats -> https://example.com\n",
        );

        let expected = document!(
            metadata: {
                title: "Boots and"
            },
            references: {
                ("Cats", "https://example.com")
            }
        );

        assert_parse_succeeds(input, expected);
    }

    #[test]
    fn document_with_sections() {
        let input = concat!(
            "/Speed running the kitchen at 4am\n",
            "\n",
            "This is a comprehensive guide.\n",
            "\n",
            "// Motivation\n",
            "\n",
            "Set a personal best,\n",
            "while others rest!\n",
            "\n",
            "// Planning the perfect lap\n",
            "\n",
            "This requires care.\n",
            "\n",
            "/// Selecting a route\n",
            "\n",
            "Avoid the toaster.\n",
            "\n",
            "/// Choosing a victory scream\n",
            "\n",
            "\n",
            "\n",
            "Meeaaahhh?\n",
            "\n",
            "// Conclusion\n",
            "\n",
            "Go go go!"
        );

        let expected = document! {
            metadata: {
                title: "Speed running the kitchen at 4am"
            },
            contents: {
                paragraph { text("This is a comprehensive guide.") },

                section("Motivation") {
                    paragraph { text("Set a personal best, while others rest!") },
                },
                section("Planning the perfect lap") {
                    paragraph { text("This requires care.") },
                    subsection("Selecting a route") {
                        paragraph { text("Avoid the toaster.") },
                    },
                    subsection("Choosing a victory scream") {
                        paragraph { text("Meeaaahhh?") },
                    }
                },
                section("Conclusion") {
                    paragraph { text("Go go go!") },
                }
            }
        };

        assert_parse_succeeds(input, expected);
    }

    // TODO: test missing reference
}
