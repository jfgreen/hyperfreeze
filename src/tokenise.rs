use std::fmt::Display;

use crate::scan::*;

use PeekItem::*;
use SpaceInfo::*;

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

fn char_usable_in_block_name(c: char) -> bool {
    c.is_alphanumeric()
}

fn char_usable_in_identifier(c: char) -> bool {
    c.is_alphanumeric() || c == '_' || c == '-' || c == '.'
}

fn char_usable_in_data_value(c: char) -> bool {
    !(c.is_whitespace() || c == '|')
}

fn char_usable_in_parameter_name(c: char) -> bool {
    c.is_alphanumeric()
}

fn char_usable_in_parameter_value(c: char) -> bool {
    c.is_alphanumeric()
}

fn char_usable_in_title_text(c: char) -> bool {
    c != SPACE && c != NEW_LINE
}

fn char_usable_in_text_frag(c: char) -> bool {
    !MARKUP_CHARS.contains(&c)
}

fn char_usable_in_raw_frag(c: char) -> bool {
    ![BACKTICK, NEW_LINE].contains(&c)
}

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

        let position = self.scanner.position();

        if self.mode_inference_needed {
            self.mode = self.infer_mode();
            self.mode_inference_needed = false;
        }

        let next_token = self.read_next_token();

        self.set_forward_flags(next_token);

        self.token_count += 1;
        self.position = position;
        self.current = next_token;
    }

    fn infer_mode(&self) -> ContentMode {
        //TODO: Just use peek API? (maybe peek api needs to be more ergonomic?)
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
            _ => {}
        };
    }

    fn read_container_delimiter_token(&mut self) -> Option<Token<'a>> {
        let scanner = &mut self.scanner;

        match scanner.peek() {
            (Text(t), _) if t.starts_with(DELIMITED_CONTAINER_START) => {
                scanner.skip_chars(3);
                scanner.skip_while_on(SPACE);
                Some(DelimitedContainerStart)
            }
            (Text(t), _) if t.starts_with(DELIMITED_CONTAINER_END) => {
                scanner.skip_chars(3);
                scanner.skip_while_on(SPACE);
                Some(DelimitedContainerEnd)
            }
            _ => None,
        }
    }

    fn read_title_token(&mut self) -> Option<Token<'a>> {
        let scanner = &mut self.scanner;

        match scanner.peek() {
            (Text(t), _) if t.starts_with(SLASH) => {
                scanner.skip_char();
                if scanner.is_on_char(SLASH) {
                    scanner.skip_char();
                    if scanner.is_on_char(SLASH) {
                        scanner.skip_char();
                        scanner.skip_while_on(SPACE);
                        Some(SubSectionDirective)
                    } else {
                        scanner.skip_while_on(SPACE);
                        Some(SectionDirective)
                    }
                } else {
                    scanner.skip_while_on(SPACE);
                    Some(TitleDirective)
                }
            }

            (Whitespace(Space), Text(_)) => {
                scanner.skip_while_on(SPACE);
                Some(TitleTextSpace)
            }

            (Text(t), _) if t.starts_with(char_usable_in_title_text) => {
                let text = scanner.eat_while(char_usable_in_title_text);
                Some(TitleText(text))
            }
            _ => None,
        }
    }

    fn read_header_token(&mut self) -> Option<Token<'a>> {
        let scanner = &mut self.scanner;

        match scanner.peek() {
            // text!(AT_SIGN) =>
            (Text(t), _) if t.starts_with(AT_SIGN) => {
                scanner.skip_char();
                let name = scanner.eat_while(char_usable_in_block_name);
                Some(StructuredDataDirective(name))
            }
            (Text(t), _) if t.starts_with(EXCLAMATION_MARK) => {
                scanner.skip_char();
                let name = scanner.eat_while(char_usable_in_block_name);
                Some(ContainerDirective(name))
            }
            (Text(t), _) if t.starts_with(HASH) => {
                scanner.skip_char();
                let name = scanner.eat_while(char_usable_in_block_name);
                Some(BlockDirective(name))
            }

            (Text(t), _) if t.starts_with(LEFT_BRACKET) => {
                scanner.skip_char();
                Some(BlockParametersStart)
            }
            (Text(t), _) if t.starts_with(RIGHT_BRACKET) => {
                scanner.skip_char();
                Some(BlockParametersEnd)
            }
            (Text(t), _) if t.starts_with(EQUALS) => {
                scanner.skip_char();
                Some(BlockParameterNameValueSeperator)
            }
            (Text(t), _)
                if t.starts_with(char_usable_in_parameter_value)
                    && self.current == BlockParameterNameValueSeperator =>
            {
                let value = scanner.eat_while(char_usable_in_parameter_value);
                Some(BlockParameterValue(value))
            }
            (Text(t), _) if t.starts_with(char_usable_in_parameter_name) => {
                let name = scanner.eat_while(char_usable_in_parameter_name);
                Some(BlockParameterName(name))
            }
            _ => None,
        }
    }

    fn read_structured_data_token(&mut self) -> Option<Token<'a>> {
        let scanner = &mut self.scanner;
        let position = scanner.position();
        let column = position.column;

        match scanner.peek() {
            (Text(t), _) if t.starts_with(char_usable_in_identifier) && column == 0 => {
                let key = scanner.eat_while(char_usable_in_identifier);
                scanner.skip_while_on(SPACE);
                Some(DataIdentifier(key))
            }
            (Text(t), _) if t.starts_with(COLON) => {
                scanner.skip_char();
                scanner.skip_while_on(SPACE);
                Some(DataKeyValueSeperator)
            }
            (Text(t), _) if t.starts_with(VERTICAL_BAR) => {
                scanner.skip_char();
                scanner.skip_while_on(SPACE);
                Some(DataListSeperator)
            }
            (Text(t), _) if t.starts_with(char_usable_in_data_value) => {
                let value = scanner.eat_while(char_usable_in_data_value);
                scanner.skip_while_on(SPACE);
                Some(DataValue(value))
            }

            _ => None,
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

        let list_indent_allowed = self.mode == List && !self.in_raw && column == 0;

        match scanner.peek() {
            (Text(t), _) if t.starts_with(BACKTICK) => {
                self.in_raw = !self.in_raw;
                scanner.skip_char();
                Some(RawDelimiter)
            }

            // TODO: nasty - find a way to match either space or text
            // both are valid for raw
            (Text(t), _) | (Whitespace(Space), Text(t))
                if t.starts_with(DASH) && list_indent_allowed =>
            {
                let space_count = scanner.skip_while_on(SPACE);
                scanner.skip_char();
                scanner.skip_while_on(SPACE);
                Some(ListBullet(space_count))
            }
            (Whitespace(s), Text(t))
                if markup_space_allowed
                    && (s == Space
                        || (s == Singlebreak
                            && !t.starts_with(DELIMITED_CONTAINER_END)
                            && !(self.mode == List && t.starts_with(DASH)))) =>
            {
                scanner.skip_while_on(SPACE);
                if scanner.is_on_char(NEW_LINE) {
                    scanner.skip_char();
                }
                scanner.skip_while_on(SPACE);
                Some(MarkupTextSpace)
            }
            (_, _) if self.in_raw && scanner.is_on(char_usable_in_raw_frag) => {
                let fragment = scanner.eat_while(char_usable_in_raw_frag);
                Some(RawFragment(fragment))
            }

            (Text(t), _) if t.starts_with(LEFT_SQUARE_BRACKET) => {
                scanner.skip_char();
                Some(LinkOpeningDelimiter)
            }

            (Text(t), _) if t.starts_with(RIGHT_SQUARE_BRACKET) => {
                scanner.skip_char();
                Some(LinkClosingDelimiter)
            }

            (Text(t), _) if self.current == LinkClosingDelimiter && t.starts_with(AT_SIGN) => {
                scanner.skip_char();
                Some(LinkToReferenceJoiner)
            }

            (Text(t), _)
                if self.current == LinkToReferenceJoiner
                    && t.starts_with(char_usable_in_identifier) =>
            {
                let identifier = scanner.eat_while(char_usable_in_identifier);
                Some(DataIdentifier(identifier))
            }

            (Text(t), _) if t.starts_with(ASTERISK) => {
                scanner.skip_char();
                Some(StrongDelimiter)
            }

            (Text(t), _) if t.starts_with(UNDERSCORE) => {
                scanner.skip_char();
                Some(EmphasisDelimiter)
            }

            (Text(t), _) if t.starts_with(TILDE) => {
                scanner.skip_char();
                Some(StrikethroughDelimiter)
            }

            (Text(t), _) if t.starts_with(BACKSLASH) => {
                scanner.skip_char();
                let text = scanner.eat_char();
                Some(MarkupText(text))
            }

            (Text(t), _) if t.starts_with(char_usable_in_text_frag) => {
                let text = scanner.eat_while(char_usable_in_text_frag);
                Some(MarkupText(text))
            }

            _ => None,
        }
    }

    fn read_code_block_token(&mut self) -> Option<Token<'a>> {
        let scanner = &mut self.scanner;

        match scanner.peek() {
            (Text(t), _) if t.starts_with(CODE_DELIMITER) => {
                self.in_raw = !self.in_raw;
                scanner.skip_chars(3);
                Some(CodeDelimiter)
            }

            (_, _) if self.in_raw && self.current != CodeDelimiter => {
                let code = scanner.eat_until_line_starting_with(CODE_DELIMITER);
                Some(Code(code))
            }
            _ => None,
        }
    }

    fn read_generic_token(&mut self) -> Token<'a> {
        let scanner = &mut self.scanner;
        match scanner.peek() {
            (Whitespace(Multibreak), _) => {
                scanner.skip_char();
                scanner.skip_while_on_empty_line();
                BlockBreak
            }
            (Whitespace(Singlebreak), _) => {
                scanner.skip_while_on(SPACE);
                scanner.skip_char();
                LineBreak
            }
            (Whitespace(_), End) => {
                scanner.skip_while_on_whitespace();
                EndOfInput
            }

            (End, _) => EndOfInput,

            // TODO: If we do have to have an unknown, maybe take larger chunks?
            // (e.g until next space)
            _ => {
                let c = scanner.eat_char();
                Unknown(c)
            }
        }
    }
}
