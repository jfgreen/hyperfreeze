use crate::scan::*;
use std::fmt::Display;

//TODO: Some kind of annotated example that describes the terminology
//TODO: Make terminology less confusing

const SPACE: char = ' ';
const NEW_LINE: char = '\n';
const COLON: char = ':';
const DELIMITED_CONTAINER_START: &str = ">>>";
const DELIMITED_CONTAINER_END: &str = "<<<";
const CODE_DELIMITER: &str = "---";
const HASH: char = '#';
const LEFT_SQUARE_BRACKET: char = '[';
const RIGHT_SQUARE_BRACKET: char = ']';
const LEFT_BRACKET: char = '(';
const RIGHT_BRACKET: char = ')';
const EQUALS: char = '=';
const BACKTICK: char = '`';
const ASTERISK: char = '*';
const TILDE: char = '~';
const UNDERSCORE: char = '_';
const SLASH: char = '/';
const BACKSLASH: char = '\\';
const DASH: char = '-';
const AT_SIGN: char = '@';
const EXCLAMATION_MARK: char = '!';
const VERTICAL_BAR: char = '|';

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

type Indent = usize;

// TODO: Maybe we are tring to be too abstracted / high level in these tokens?
// TODO: Would having _every_ token have a value (aka literial) be simpler?
#[derive(Clone, Copy, Debug, PartialEq)]
pub enum Token<'a> {
    StartOfInput,
    EndOfInput,
    Unknown(&'a str),
    StructuredDataDirective(&'a str),
    TitleDirective,
    SectionDirective,
    SubSectionDirective,
    ContainerDirective(&'a str),
    BlockDirective(&'a str),
    BlockParametersStart,
    BlockParametersEnd,
    BlockParameterName(&'a str),
    BlockParameterValue(&'a str),
    //TODO: This name is meh, just call it what it is: equals sign?
    BlockParameterNameValueSeperator,
    BlockBreak,
    DataListSeperator,
    DataKeyValueSeperator,
    DataIdentifier(&'a str),
    DataValue(&'a str),
    TitleText(&'a str),
    TitleTextSpace,
    LineBreak,
    StrongDelimiter,
    EmphasisDelimiter,
    StrikethroughDelimiter,
    RawDelimiter,
    RawFragment(&'a str),
    MarkupText(&'a str),
    MarkupTextSpace,
    Code(&'a str),
    LinkOpeningDelimiter,
    LinkClosingDelimiter,
    LinkToReferenceJoiner,
    ListBullet(Indent),
    CodeDelimiter,
    //TODO: More rubbish naming
    DelimitedContainerStart,
    DelimitedContainerEnd,
}

impl Display for Token<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            StartOfInput => write!(f, "start of input"),
            EndOfInput => write!(f, "end of input"),
            Unknown(value) => write!(f, "unknown '{value}'"),
            StructuredDataDirective(title) => write!(f, "structured data directive '{title}'"),
            TitleDirective => write!(f, "document directive"),
            SectionDirective => write!(f, "section directive"),
            SubSectionDirective => write!(f, "subsection directive"),
            ContainerDirective(title) => write!(f, "container directive '{title}'"),
            BlockDirective(title) => write!(f, "block directive {title}"),
            BlockParametersStart => write!(f, "block parameters start '('"),
            BlockParametersEnd => write!(f, "block parameters end ')'"),
            BlockParameterName(name) => write!(f, "block parameter name '{name}'"),
            BlockParameterValue(value) => write!(f, "block parameter value '{value}'"),
            BlockParameterNameValueSeperator => write!(f, "block parameter name value seperator"),
            BlockBreak => write!(f, "block break"),
            DataListSeperator => write!(f, "metadata list seperator"),
            DataKeyValueSeperator => write!(f, "metadata key value seperator"),
            DataIdentifier(identifier) => write!(f, "metadata identifier'{identifier}'"),
            DataValue(value) => write!(f, "metadata value '{value}'"),
            TitleText(text) => write!(f, "title text '{text}'"),
            TitleTextSpace => write!(f, "title text space"),
            LineBreak => write!(f, "linebreak"),
            StrongDelimiter => write!(f, "strong delimiter"),
            EmphasisDelimiter => write!(f, "emphasis delimiter"),
            StrikethroughDelimiter => write!(f, "strikethrough delimiter"),
            RawDelimiter => write!(f, "raw delimiter"),
            RawFragment(fragment) => write!(f, "raw fragment '{fragment}'"),
            MarkupText(text) => write!(f, "markup text '{text}'"),
            MarkupTextSpace => write!(f, "markup text space"),
            LinkOpeningDelimiter => write!(f, "link opening delimiter"),
            LinkClosingDelimiter => write!(f, "link closing delimiter"),
            LinkToReferenceJoiner => write!(f, "link to reference joiner '@'"),
            ListBullet(indent) => write!(f, "list bullet (indent {indent})"),
            CodeDelimiter => write!(f, "delimited block delimiter"),
            DelimitedContainerStart => write!(f, "delimited container start"),
            DelimitedContainerEnd => write!(f, "delimited container end"),
            Code(code) => write!(f, "code '{code}'"),
        }
    }
}

// TODO: Sub modes might be easier to reason about
// Markup(List)
// Markup(ListRaw)
// Markup(Paragraph)
// Markup(ParagraphRaw)
#[derive(Clone, Copy, Debug, PartialEq)]
enum ContentMode {
    Title,
    Header,
    StructuredData,
    Paragraph,
    List,
    CodeBlock,
}

use Token::*;

use ContentMode::*;

pub struct Tokeniser<'a> {
    scanner: Scanner<'a>,
    position: Position,
    current: Token<'a>,
    token_count: usize,
    max_tokens: usize,
    mode: ContentMode,
    upcoming_mode: Option<ContentMode>,
    mode_inference_needed: bool,
    in_raw: bool,
}

impl<'a> Tokeniser<'a> {
    pub fn new(input: &'a str) -> Self {
        let mut scanner = Scanner::new(input);
        scanner.skip_while_on_empty_line();

        Tokeniser {
            scanner,
            position: Position { column: 0, row: 0 },
            current: StartOfInput,
            token_count: 0,
            max_tokens: input.len(),
            mode: ContentMode::Paragraph,
            upcoming_mode: None,
            mode_inference_needed: true,
            in_raw: false,
        }
    }

    pub fn current(&self) -> Token<'a> {
        self.current
    }

    pub fn position(&self) -> Position {
        self.position
    }

    pub fn advance(&mut self) {
        if self.token_count > self.max_tokens {
            panic!("Posible infinite loop detected")
        }

        //TODO: could we get position from the matches instead?
        let position = self.scanner.position();

        if self.mode_inference_needed {
            self.mode = self.infer_mode();
            self.mode_inference_needed = false;
        }

        let next_token = self.read_next_token();
        dbg!(next_token);

        self.set_forward_flags(next_token);

        self.token_count += 1;
        self.position = position;
        self.current = next_token;
    }

    // TODO: Maybe now we are in a place to consider if we want to try and have
    // the parser determine the lex mode again?
    fn infer_mode(&self) -> ContentMode {
        let scanner = &self.scanner;
        if scanner.is_on_char(SLASH) {
            Title
        } else if scanner.is_on_one_of(&[HASH, EXCLAMATION_MARK, AT_SIGN]) {
            Header
        } else if scanner.is_on_char(DASH) {
            List
        } else {
            Paragraph
        }
    }

    fn read_next_token(&mut self) -> Token<'a> {
        self.read_container_delimiter_token()
            .or_else(|| self.read_modal_token())
            .unwrap_or_else(|| self.read_generic_token())
    }

    fn read_modal_token(&mut self) -> Option<Token<'a>> {
        match self.mode {
            Title => self.read_title_token(),
            Header => self.read_header_token(),
            StructuredData => self.read_structured_data_token(),
            Paragraph | List => self.read_markup_token(),
            CodeBlock => self.read_code_block_token(),
        }
    }

    fn set_forward_flags(&mut self, next_token: Token<'a>) {
        match next_token {
            LineBreak => {
                if matches!(self.current, ContainerDirective(_)) {
                    self.mode_inference_needed = true;
                }
                if let Some(next_mode) = self.upcoming_mode {
                    self.mode = next_mode;
                    self.upcoming_mode = None;
                }
                if matches!(self.current, CodeDelimiter) {
                    self.in_raw = true;
                }
            }
            BlockBreak => {
                self.mode_inference_needed = true;
            }
            BlockDirective(name) => {
                match name {
                    "paragraph" => {
                        self.upcoming_mode = Some(Paragraph);
                    }
                    "list" => {
                        self.upcoming_mode = Some(List);
                    }
                    "code" => {
                        self.upcoming_mode = Some(CodeBlock);
                    }
                    _ => {}
                };
            }
            StructuredDataDirective(_) => {
                self.upcoming_mode = Some(StructuredData);
            }
            CodeDelimiter if self.in_raw => {
                self.in_raw = false;
            }
            RawDelimiter => {
                self.in_raw = !self.in_raw;
            }
            _ => {}
        };
    }

    fn read_container_delimiter_token(&mut self) -> Option<Token<'a>> {
        let scanner = &mut self.scanner;

        if let Some(delimiter) = scanner.match_delimited_container_start() {
            scanner.advance_past(&delimiter);
            Some(DelimitedContainerStart)
        } else if let Some(delimiter) = scanner.match_delimited_container_end() {
            scanner.advance_past(&delimiter);
            Some(DelimitedContainerEnd)
        } else {
            None
        }
    }

    fn read_title_token(&mut self) -> Option<Token<'a>> {
        let scanner = &mut self.scanner;

        if let Some(directive) = scanner.match_subsection_directive() {
            scanner.advance_past(&directive);
            Some(SubSectionDirective)
        } else if let Some(directive) = scanner.match_section_directive() {
            scanner.advance_past(&directive);
            Some(SectionDirective)
        } else if let Some(directive) = scanner.match_title_directive() {
            scanner.advance_past(&directive);
            Some(TitleDirective)
        } else if let Some(space) = scanner.match_title_text_space() {
            scanner.advance_past(&space);
            Some(TitleTextSpace)
        } else if let Some(text) = scanner.match_markup_text() {
            scanner.advance_past(&text);
            Some(TitleText(text.text))
        } else {
            None
        }
    }

    fn read_header_token(&mut self) -> Option<Token<'a>> {
        let scanner = &mut self.scanner;

        if let Some(data_name) = scanner.match_structured_data_directive() {
            scanner.advance_past(&data_name);
            Some(StructuredDataDirective(data_name.text))
        } else if let Some(container_name) = scanner.match_container_directive() {
            scanner.advance_past(&container_name);
            Some(ContainerDirective(container_name.text))
        } else if let Some(block_name) = scanner.match_block_directive() {
            scanner.advance_past(&block_name);
            Some(BlockDirective(block_name.text))
        } else if let Some(bracket) = scanner.match_char(LEFT_BRACKET) {
            scanner.advance_past(&bracket);
            Some(BlockParametersStart)
        } else if let Some(bracket) = scanner.match_char(RIGHT_BRACKET) {
            scanner.advance_past(&bracket);
            Some(BlockParametersEnd)
        } else if let Some(equals) = scanner.match_char(EQUALS) {
            scanner.advance_past(&equals);
            Some(BlockParameterNameValueSeperator)
        } else if let Some(value) = scanner.match_identifier()
            && self.current == BlockParameterNameValueSeperator
        {
            scanner.advance_past(&value);
            Some(BlockParameterValue(value.text))
        } else if let Some(value) = scanner.match_identifier() {
            scanner.advance_past(&value);
            Some(BlockParameterName(value.text))
        } else {
            None
        }
    }

    fn read_structured_data_token(&mut self) -> Option<Token<'a>> {
        let scanner = &mut self.scanner;
        let position = scanner.position();
        let column = position.column;

        if let Some(identifier) = scanner.match_identifier()
            && column == 0
        {
            scanner.advance_past(&identifier);
            // scanner.skip_while_on(SPACE);
            Some(DataIdentifier(identifier.text))
        } else if let Some(colon) = scanner.match_char(COLON) {
            scanner.advance_past(&colon);
            scanner.skip_while_on(SPACE);
            Some(DataKeyValueSeperator)
        } else if let Some(bar) = scanner.match_char(VERTICAL_BAR) {
            scanner.advance_past(&bar);
            scanner.skip_while_on(SPACE);
            Some(DataListSeperator)
        } else if let Some(value) = scanner.match_data_value() {
            scanner.advance_past(&value);
            scanner.skip_while_on(SPACE);
            Some(DataValue(value.text))
        } else {
            None
        }
    }

    fn read_markup_token(&mut self) -> Option<Token<'a>> {
        let scanner = &mut self.scanner;
        let position = scanner.position();
        let column = position.column;

        let markup_space_allowed = !self.in_raw
            && !matches!(
                self.current,
                DelimitedContainerStart | DelimitedContainerEnd
            );

        let in_list = self.mode == List;
        let list_indent_allowed = in_list && !self.in_raw && column == 0;

        if let Some(raw_delimiter) = scanner.match_char(BACKTICK) {
            scanner.advance_past(&raw_delimiter);
            Some(RawDelimiter)
        } else if let Some((list_bullet, space_count)) = scanner.match_list_bullet()
            && list_indent_allowed
        {
            scanner.advance_past(&list_bullet);
            Some(ListBullet(space_count))
        } else if let Some(markup_space) = scanner.match_markup_text_space(in_list)
            && markup_space_allowed
        {
            scanner.advance_past(&markup_space);
            Some(MarkupTextSpace)
        } else if let Some(fragment) = scanner.match_raw_fragment()
            && self.in_raw
        {
            scanner.advance_past(&fragment);
            Some(RawFragment(fragment.text))
        } else if let Some(delimiter) = scanner.match_char(LEFT_SQUARE_BRACKET) {
            scanner.advance_past(&delimiter);
            Some(LinkOpeningDelimiter)
        } else if let Some(delimiter) = scanner.match_char(RIGHT_SQUARE_BRACKET) {
            scanner.advance_past(&delimiter);
            Some(LinkClosingDelimiter)
        } else if let Some(at_sign) = scanner.match_char(AT_SIGN)
            && self.current == LinkClosingDelimiter
        {
            scanner.advance_past(&at_sign);
            Some(LinkToReferenceJoiner)
        } else if let Some(identifier) = scanner.match_identifier()
            && self.current == LinkToReferenceJoiner
        {
            scanner.advance_past(&identifier);
            Some(DataIdentifier(identifier.text))
        } else if let Some(strong_delimiter) = scanner.match_char(ASTERISK) {
            scanner.advance_past(&strong_delimiter);
            Some(StrongDelimiter)
        } else if let Some(emphasis_delimiter) = scanner.match_char(UNDERSCORE) {
            scanner.advance_past(&emphasis_delimiter);
            Some(EmphasisDelimiter)
        } else if let Some(strikethrough_delimiter) = scanner.match_char(TILDE) {
            scanner.advance_past(&strikethrough_delimiter);
            Some(StrikethroughDelimiter)
        } else if let Some(escaped) = scanner.match_escaped() {
            scanner.advance_past(&escaped);
            Some(MarkupText(escaped.text))
        } else if let Some(markup) = scanner.match_markup_text() {
            scanner.advance_past(&markup);
            Some(MarkupText(markup.text))
        } else {
            None
        }
    }

    fn read_code_block_token(&mut self) -> Option<Token<'a>> {
        let scanner = &mut self.scanner;

        if let Some(delimiter) = scanner.match_str(CODE_DELIMITER) {
            //TODO: could the 'advance' past actually be done by the match?
            scanner.advance_past(&delimiter);
            Some(CodeDelimiter)
        } else if self.in_raw
            && let Some(code) = scanner.match_until_line_starting_with(CODE_DELIMITER)
        {
            // TODO: Consider line by line instead
            // Think: how to give a helpful error like 'unterminated code block'
            scanner.advance_past(&code);
            Some(Code(code.text))
        } else {
            None
        }
    }

    fn read_generic_token(&mut self) -> Token<'a> {
        let scanner = &mut self.scanner;

        // TODO: this if/let strucutre is probable not the final form..
        // If we return the match and the new position, let code
        // higher up apply the advance?
        //
        // Would it be nice if this could be inlinable
        // i.e probably dont want a seperate scanner?
        //
        // Once we do have the final form, maybe start playing
        // with some macros?

        if let Some(blockbreak) = scanner.match_blockbreak() {
            scanner.advance_past(&blockbreak);
            BlockBreak
        } else if let Some(linebreak) = scanner.match_linebreak() {
            scanner.advance_past(&linebreak);
            LineBreak
        } else if let Some(end_of_input) = scanner.match_end_of_input() {
            scanner.advance_past(&end_of_input);
            EndOfInput
        } else {
            let unknown = scanner.match_unknown();
            scanner.advance_past(&unknown);
            Unknown(unknown.text)
        }
    }
}
