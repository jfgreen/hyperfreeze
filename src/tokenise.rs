use std::fmt::Display;

use crate::scan::*;

//TODO: Some kind of annotated example that describes the terminology
//TODO: Make terminology less confusing

const SPACE: char = ' ';
const NEW_LINE: char = '\n';
const COLON: char = ':';
const DELIMITED_CONTAINER_START: &str = ">>>";
const DELIMITED_CONTAINER_END: &str = "<<<";
const DELIMITED_BLOCK_DELIMITER: &str = "---\n";
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
// Revisit once parsing in a byte oriented way
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
    LinkOpeningDelimiter,
    LinkClosingDelimiter,
    LinkToReferenceJoiner,
    ListBullet(Indent),
    //TODO: More rubbish naming
    DelimitedBlockDelimiter,
    DelimitedContainerStart,
    DelimitedContainerEnd,
    Raw(&'a str),
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
            DelimitedBlockDelimiter => write!(f, "delimited block delimiter"),
            DelimitedContainerStart => write!(f, "delimited container start"),
            DelimitedContainerEnd => write!(f, "delimited container end"),
            Raw(text) => write!(f, "raw text '{text}'"),
        }
    }
}

// TODO: Gradually move away from block based states to mode based states
// e.g
// Properties
// Markup
// MarkupList
// ???
//
// Or maybe just flags
#[derive(Clone, Copy, Debug, PartialEq)]
enum State {
    Start,
    ContainerHeader,
    HeaderText,
    StructuredData,
    Paragraph,
    ParagraphRaw,
    List,
    ListRaw,
    Code,
    CodeDelimited,
    Finished,
}

use Token::*;

use State::*;

pub struct Tokeniser<'a> {
    scanner: Scanner<'a>,
    state: State,
    position: Position,
    current: Token<'a>,
    token_count: usize,
    max_tokens: usize,
    on_header_line: bool,
}

impl<'a> Tokeniser<'a> {
    pub fn new(input: &'a str) -> Self {
        let mut scanner = Scanner::new(input);
        scanner.skip_while_on_empty_line();

        Tokeniser {
            scanner,
            state: State::Start,
            position: Position { column: 0, row: 0 },
            current: StartOfInput,
            token_count: 0,
            max_tokens: input.len(),
            on_header_line: false,
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

        dbg!(self.state);
        dbg!(self.on_header_line);

        let value = self.read_next_token();

        dbg!(value);
        self.token_count += 1;
        self.position = position;
        self.current = value;
    }

    // TODO: If we do have to have an unknown, maybe take larger chunks?
    // (e.g until next space)
    fn read_next_token(&mut self) -> Token<'a> {
        //TODO: Try and gradually simplify this mess
        let scanner = &mut self.scanner;
        let position = scanner.position();
        let column = position.column;
        let on_start_of_block = matches!(self.current, BlockBreak | StartOfInput)
            || (self.state == ContainerHeader && self.current == LineBreak);

        if on_start_of_block && scanner.is_on_char(DASH) {
            self.state = List
        } else if on_start_of_block
            && !scanner.is_on_one_of(&[SLASH, HASH, EXCLAMATION_MARK, AT_SIGN])
        {
            self.state = Paragraph
        }

        if !scanner.has_input() {
            self.state = State::Finished;
            EndOfInput
        } else if matches!(self.current, TitleText(_)) && scanner.is_on_char(SPACE) {
            scanner.skip_while_on(SPACE);
            if scanner.is_on_char(NEW_LINE) {
                self.on_header_line = false;
                scanner.skip_char();
                if scanner.is_on_empty_line() {
                    scanner.skip_while_on_empty_line();
                    BlockBreak
                } else {
                    LineBreak
                }
            } else {
                TitleTextSpace
            }
        } else if matches!(
            self.current,
            DelimitedContainerStart | DelimitedContainerEnd
        ) && scanner.is_on_char(NEW_LINE)
        {
            scanner.skip_char();
            LineBreak
        } else if matches!(self.state, Paragraph | List)
            && !self.on_header_line
            && scanner.is_on_one_of(&[SPACE, NEW_LINE])
        {
            scanner.skip_while_on(SPACE);
            if scanner.is_on_char(NEW_LINE) {
                scanner.skip_char();
                let space_count = scanner.skip_while_on(SPACE);
                if scanner.is_on_char(NEW_LINE) {
                    scanner.skip_char();
                    scanner.skip_while_on_empty_line();
                    BlockBreak
                } else if scanner.is_on_char(DASH) && space_count > 0 && self.state == List {
                    scanner.skip_char();
                    scanner.skip_while_on(SPACE);
                    ListBullet(space_count)
                } else if scanner.has_input()
                    && !(scanner.is_on_char(DASH) && self.state == List)
                    && !scanner.is_on_str(DELIMITED_CONTAINER_END)
                {
                    MarkupTextSpace
                } else if !scanner.has_input() {
                    EndOfInput
                } else {
                    LineBreak
                }
            } else if !scanner.has_input() {
                EndOfInput
            } else {
                MarkupTextSpace
            }
        } else if scanner.is_on_char(NEW_LINE) {
            scanner.skip_char();
            self.on_header_line = false;
            if scanner.is_on_empty_line() {
                scanner.skip_while_on_empty_line();
                BlockBreak
            } else {
                LineBreak
            }
        } else if on_start_of_block && scanner.is_on_char(SLASH) {
            self.on_header_line = true;
            scanner.skip_char();
            self.state = HeaderText;
            if scanner.is_on_char(SLASH) {
                scanner.skip_char();
                if scanner.is_on_char(SLASH) {
                    scanner.skip_char();
                    scanner.skip_while_on(SPACE);
                    SubSectionDirective
                } else {
                    scanner.skip_while_on(SPACE);
                    SectionDirective
                }
            } else {
                scanner.skip_while_on(SPACE);
                TitleDirective
            }
        } else if on_start_of_block && scanner.is_on_char(AT_SIGN) {
            scanner.skip_char();
            self.on_header_line = true;
            let name = scanner.eat_while(char_usable_in_block_name);
            self.state = StructuredData;
            StructuredDataDirective(name)
        } else if on_start_of_block && scanner.is_on_char(EXCLAMATION_MARK) {
            scanner.skip_char();
            self.on_header_line = true;
            let name = scanner.eat_while(char_usable_in_block_name);
            self.state = ContainerHeader;
            ContainerDirective(name)
        } else if on_start_of_block && scanner.is_on_char(HASH) {
            scanner.skip_char();
            self.on_header_line = true;
            let name = scanner.eat_while(char_usable_in_block_name);
            match name {
                "paragraph" => {
                    self.state = Paragraph;
                }
                "list" => {
                    self.state = List;
                }
                "code" => {
                    self.state = Code;
                }
                _ => {}
            };
            BlockDirective(name)
        } else if scanner.is_on_str(DELIMITED_CONTAINER_START) {
            scanner.skip_chars(3);
            scanner.skip_while_on(SPACE);
            DelimitedContainerStart
        } else if scanner.is_on_str(DELIMITED_CONTAINER_END) {
            scanner.skip_chars(3);
            scanner.skip_while_on(SPACE);
            DelimitedContainerEnd
        } else if self.state == HeaderText {
            let text = scanner.eat_while(char_usable_in_title_text);
            TitleText(text)
        } else if self.state == StructuredData {
            if scanner.is_on(char_usable_in_identifier) && column == 0 {
                let key = scanner.eat_while(char_usable_in_identifier);
                scanner.skip_while_on(SPACE);
                DataIdentifier(key)
            } else if scanner.is_on_char(COLON) {
                scanner.skip_char();
                scanner.skip_while_on(SPACE);
                DataKeyValueSeperator
            } else if scanner.is_on_char(VERTICAL_BAR) {
                scanner.skip_char();
                scanner.skip_while_on(SPACE);
                DataListSeperator
            } else if scanner.is_on(char_usable_in_data_value) {
                let value = scanner.eat_while(char_usable_in_data_value);
                scanner.skip_while_on(SPACE);
                DataValue(value)
            } else {
                let c = scanner.eat_char();
                Unknown(c)
            }
        } else if self.on_header_line {
            let scanner = &mut self.scanner;
            if scanner.is_on_char(LEFT_BRACKET) {
                scanner.skip_char();
                BlockParametersStart
            } else if scanner.is_on_char(RIGHT_BRACKET) {
                scanner.skip_char();
                BlockParametersEnd
            } else if scanner.is_on_char(EQUALS) {
                scanner.skip_char();
                BlockParameterNameValueSeperator
            } else if scanner.is_on(char_usable_in_parameter_value)
                && self.current == BlockParameterNameValueSeperator
            {
                let value = scanner.eat_while(char_usable_in_parameter_value);
                BlockParameterValue(value)
            } else if scanner.is_on(char_usable_in_parameter_name) {
                let name = scanner.eat_while(char_usable_in_parameter_name);
                BlockParameterName(name)
            } else {
                let c = scanner.eat_char();
                Unknown(c)
            }
        } else if self.state == Paragraph {
            if scanner.is_on_char(LEFT_SQUARE_BRACKET) {
                scanner.skip_char();
                LinkOpeningDelimiter
            } else if scanner.is_on_char(RIGHT_SQUARE_BRACKET) {
                scanner.skip_char();
                LinkClosingDelimiter
            } else if self.current == LinkClosingDelimiter && scanner.is_on_char(AT_SIGN) {
                scanner.skip_char();
                LinkToReferenceJoiner
            } else if self.current == LinkToReferenceJoiner
                && scanner.is_on(char_usable_in_identifier)
            {
                let identifier = scanner.eat_while(char_usable_in_identifier);
                DataIdentifier(identifier)
            } else if scanner.is_on_char(ASTERISK) {
                scanner.skip_char();
                StrongDelimiter
            } else if scanner.is_on_char(UNDERSCORE) {
                scanner.skip_char();
                EmphasisDelimiter
            } else if scanner.is_on_char(TILDE) {
                scanner.skip_char();
                StrikethroughDelimiter
            } else if scanner.is_on(char_usable_in_text_frag) {
                let text = scanner.eat_while(char_usable_in_text_frag);
                MarkupText(text)
            } else if scanner.is_on_char(BACKTICK) {
                scanner.skip_char();
                self.state = ParagraphRaw;
                RawDelimiter
            } else if scanner.is_on_char(BACKSLASH) {
                scanner.skip_char();
                let text = scanner.eat_char();
                MarkupText(text)
            } else {
                let c = scanner.eat_char();
                Unknown(c)
            }
        } else if self.state == List {
            //TODO: Instead of column set a flag earlier?
            if column == 0 && scanner.is_on_char(DASH) {
                scanner.skip_char();
                scanner.skip_while_on(SPACE);
                ListBullet(0)
            } else if scanner.is_on_char(LEFT_SQUARE_BRACKET) {
                scanner.skip_char();
                LinkOpeningDelimiter
            } else if scanner.is_on_char(RIGHT_SQUARE_BRACKET) {
                scanner.skip_char();
                LinkClosingDelimiter
            } else if self.current == LinkClosingDelimiter && scanner.is_on_char(AT_SIGN) {
                scanner.skip_char();
                LinkToReferenceJoiner
            } else if self.current == LinkToReferenceJoiner
                && scanner.is_on(char_usable_in_identifier)
            {
                let identifier = scanner.eat_while(char_usable_in_identifier);
                DataIdentifier(identifier)
            } else if scanner.is_on_char(ASTERISK) {
                scanner.skip_char();
                StrongDelimiter
            } else if scanner.is_on_char(UNDERSCORE) {
                scanner.skip_char();
                EmphasisDelimiter
            } else if scanner.is_on_char(TILDE) {
                scanner.skip_char();
                StrikethroughDelimiter
            } else if scanner.is_on(char_usable_in_text_frag) {
                let text = scanner.eat_while(char_usable_in_text_frag);
                MarkupText(text)
            } else if scanner.is_on_char(BACKTICK) {
                scanner.skip_char();
                self.state = ListRaw;
                RawDelimiter
            } else if scanner.is_on_char(BACKSLASH) {
                scanner.skip_char();
                let text = scanner.eat_char();
                MarkupText(text)
            } else {
                let c = scanner.eat_char();
                Unknown(c)
            }
        } else if matches!(self.state, ListRaw | ParagraphRaw) {
            if scanner.is_on_char(BACKTICK) {
                scanner.skip_char();
                if self.state == ListRaw {
                    self.state = List
                } else {
                    self.state = Paragraph
                }
                RawDelimiter
            } else if scanner.is_on(char_usable_in_raw_frag) {
                let fragment = scanner.eat_while(char_usable_in_raw_frag);
                RawFragment(fragment)
            } else {
                let c = scanner.eat_char();
                Unknown(c)
            }
        } else if matches!(self.state, Code | CodeDelimited) {
            if column == 0 && scanner.is_on_str(DELIMITED_BLOCK_DELIMITER) {
                if self.state == Code {
                    self.state = CodeDelimited;
                } else if self.state == CodeDelimited {
                    self.state = Code;
                }
                scanner.skip_chars(4);
                DelimitedBlockDelimiter
            } else if self.state == CodeDelimited {
                let raw = scanner.eat_until_line_starting_with(DELIMITED_BLOCK_DELIMITER);
                Raw(raw)
            } else {
                let c = scanner.eat_char();
                Unknown(c)
            }
        } else if self.state == Finished {
            EndOfInput
        } else {
            let c = scanner.eat_char();
            Unknown(c)
        }
    }
}
