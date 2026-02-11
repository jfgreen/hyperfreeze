use std::fmt::Display;
use std::str::CharIndices;

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
const FULL_STOP: char = '.';

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

        let scan_match = self.read_next_token();
        let next_token = scan_match.token;
        dbg!(next_token);

        self.set_forward_flags(next_token);

        self.scanner.advance_past(&scan_match);

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

    fn read_next_token(&self) -> ScanMatch<'a> {
        self.read_container_delimiter_token()
            .or_else(|| self.read_modal_token())
            .unwrap_or_else(|| self.read_generic_token())
    }

    fn read_modal_token(&self) -> Option<ScanMatch<'a>> {
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

    fn read_container_delimiter_token(&self) -> Option<ScanMatch<'a>> {
        let scanner = &self.scanner;

        if let Some(delimiter) = scanner.match_delimited_container_start() {
            Some(delimiter)
        } else if let Some(delimiter) = scanner.match_delimited_container_end() {
            Some(delimiter)
        } else {
            None
        }
    }

    fn read_title_token(&self) -> Option<ScanMatch<'a>> {
        let scanner = &self.scanner;

        if let Some(directive) = scanner.match_subsection_directive() {
            Some(directive)
        } else if let Some(directive) = scanner.match_section_directive() {
            Some(directive)
        } else if let Some(directive) = scanner.match_title_directive() {
            Some(directive)
        } else if let Some(space) = scanner.match_title_text_space() {
            Some(space)
        } else if let Some(text) = scanner.match_title_text() {
            Some(text)
        } else {
            None
        }
    }

    fn read_header_token(&self) -> Option<ScanMatch<'a>> {
        let scanner = &self.scanner;

        if let Some(data_name) = scanner.match_structured_data_directive() {
            Some(data_name)
        } else if let Some(container_name) = scanner.match_container_directive() {
            Some(container_name)
        } else if let Some(block_name) = scanner.match_block_directive() {
            Some(block_name)
        } else if let Some(start) = scanner.match_block_parameters_start() {
            Some(start)
        } else if let Some(end) = scanner.match_block_parameters_end() {
            Some(end)
        } else if let Some(seperator) = scanner.match_block_parameter_name_value_seperator() {
            Some(seperator)
        } else if let Some(value) = scanner.match_block_parameter_value()
            && self.current == BlockParameterNameValueSeperator
        {
            Some(value)
        } else if let Some(value) = scanner.match_block_parameter_name() {
            Some(value)
        } else {
            None
        }
    }

    //TODO: remove &mut, do mutation in calling function
    fn read_structured_data_token(&self) -> Option<ScanMatch<'a>> {
        let scanner = &self.scanner;
        let position = scanner.position();
        let column = position.column;

        if let Some(identifier) = scanner.match_data_identifier()
            && column == 0
        {
            Some(identifier)
        } else if let Some(seperator) = scanner.match_data_key_value_seperator() {
            Some(seperator)
        } else if let Some(seperator) = scanner.match_data_list_seperator() {
            Some(seperator)
        } else if let Some(value) = scanner.match_data_value() {
            Some(value)
        } else {
            None
        }
    }

    fn read_markup_token(&self) -> Option<ScanMatch<'a>> {
        let scanner = &self.scanner;
        let position = scanner.position();
        let column = position.column;

        let markup_space_allowed = !self.in_raw
            && !matches!(
                self.current,
                DelimitedContainerStart | DelimitedContainerEnd
            );

        let in_list = self.mode == List;
        let list_indent_allowed = in_list && !self.in_raw && column == 0;

        if let Some(raw_delimiter) = scanner.match_raw_delimiter() {
            Some(raw_delimiter)
        } else if let Some(list_bullet) = scanner.match_list_bullet()
            && list_indent_allowed
        {
            Some(list_bullet)
        } else if let Some(markup_space) = scanner.match_markup_text_space()
            && !in_list
            && markup_space_allowed
        {
            Some(markup_space)
        } else if let Some(markup_space) = scanner.match_list_markup_text_space()
            && in_list
            && markup_space_allowed
        {
            Some(markup_space)
        } else if let Some(fragment) = scanner.match_raw_fragment()
            && self.in_raw
        {
            Some(fragment)
        } else if let Some(delimiter) = scanner.match_link_opening_delimiter() {
            Some(delimiter)
        } else if let Some(delimiter) = scanner.match_link_closing_delimiter() {
            Some(delimiter)
        } else if let Some(joiner) = scanner.match_link_to_reference_joiner()
            && self.current == LinkClosingDelimiter
        {
            Some(joiner)
        } else if let Some(identifier) = scanner.match_data_identifier()
            && self.current == LinkToReferenceJoiner
        {
            Some(identifier)
        } else if let Some(strong_delimiter) = scanner.match_strong_delimiter() {
            Some(strong_delimiter)
        } else if let Some(emphasis_delimiter) = scanner.match_emphasis_delimiter() {
            Some(emphasis_delimiter)
        } else if let Some(strikethrough_delimiter) = scanner.match_strikethrough_delimiter() {
            Some(strikethrough_delimiter)
        } else if let Some(escaped) = scanner.match_escaped_markup_text() {
            Some(escaped)
        } else if let Some(markup) = scanner.match_markup_text() {
            Some(markup)
        } else {
            None
        }
    }

    fn read_code_block_token(&self) -> Option<ScanMatch<'a>> {
        let scanner = &self.scanner;

        if let Some(delimiter) = scanner.match_code_delimiter() {
            Some(delimiter)
        } else if self.in_raw
            && let Some(code) = scanner.match_code_block()
        {
            // TODO: Consider line by line instead
            // Think: how to give a helpful error like 'unterminated code block'
            Some(code)
        } else {
            None
        }
    }

    fn read_generic_token(&self) -> ScanMatch<'a> {
        let scanner = &self.scanner;

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
            blockbreak
        } else if let Some(linebreak) = scanner.match_linebreak() {
            linebreak
        } else if let Some(end_of_input) = scanner.match_end_of_input() {
            end_of_input
        } else {
            let unknown = scanner.match_unknown();
            unknown
        }
    }
}
#[derive(Clone, Copy, Debug)]
pub struct Position {
    pub column: u32,
    pub row: u32,
}

//TODO: Ideally we wouldn't need a copyable read head
#[derive(Debug, Clone)]
struct ReadHead<'a> {
    //TODO: all a bit much?
    chars: CharIndices<'a>,
    current: Option<char>,
    index: usize,
    column: u32,
    row: u32,
    input_len: usize,
}

impl<'a> ReadHead<'a> {
    fn new(input: &'a str) -> Self {
        let mut char_iter = input.char_indices();
        //TODO: meh
        let (i, c) = char_iter.next().unwrap();

        Self {
            chars: char_iter,
            current: Some(c),
            index: i,
            column: 0,
            row: 0,
            input_len: input.len(),
        }
    }

    fn position(&self) -> Position {
        Position {
            column: self.column,
            row: self.row,
        }
    }

    fn read_next_char(&mut self) {
        if let Some((index, c)) = self.chars.next() {
            if self.current == Some('\n') {
                self.column = 0;
                self.row += 1;
            } else {
                self.column += 1;
            }

            self.current = Some(c);
            self.index = index;
        } else {
            self.current = None;
            self.index = self.input_len;
        }
    }
}

#[derive(Debug)]
struct Scanner<'a> {
    input: &'a str,
    read_head: ReadHead<'a>,
}

impl<'a> Scanner<'a> {
    fn new(input: &'a str) -> Self {
        Self {
            input,
            read_head: ReadHead::new(input),
        }
    }

    fn position(&self) -> Position {
        self.read_head.position()
    }

    //TODO: Meh, remove
    fn index(&self) -> usize {
        self.read_head.index
    }

    fn is_on_char(&self, c: char) -> bool {
        self.input[self.index()..].starts_with(c)
    }

    fn is_on_one_of(&self, chars: &[char]) -> bool {
        self.input[self.index()..].starts_with(chars)
    }

    fn is_on_empty_line(&self) -> bool {
        // TODO: if we could have an internal version of
        // this that returns the index of the new line
        // then we could be a lot more optimised
        self.input[self.index()..]
            .trim_start_matches(SPACE)
            .starts_with(NEW_LINE)
    }

    fn skip_while_on_empty_line(&mut self) {
        // TODO: This is not that efficient...
        // once we have put in the work to look ahead,
        // can we use this to skip to new line
        while self.is_on_empty_line() {
            while self.is_on_one_of(&[SPACE, NEW_LINE]) {
                self.read_head.read_next_char();
            }
        }
    }

    // TODO: Get this working using any old technique,
    // then rework to front load most of the effort
    // in a way that can be used in all match functions
    //
    // What if we had a forward buffer that holds char info
    // the instead of passing around read heads we just pass
    // around an index
    //
    // Life would probably be eaiser if this buffer stored
    // compressed whitespace
    //
    // TODO: We could even end up with a small DSL for matching?
    //
    // From there it would be a small hop to replacing each function with a macro
    // so wouldn't even need to worry about inlining
    //
    //
    // e.g
    //
    // if let Some(blockbreak) = match_blockbreak!(scanner) { ... }

    fn match_list_bullet(&self) -> Option<ScanMatch<'a>> {
        let mut head = self.read_head.clone();

        let mut space_count = 0;

        while head.current == Some(SPACE) {
            space_count += 1;
            head.read_next_char();
        }

        if head.current == Some(DASH) {
            head.read_next_char();
        } else {
            return None;
        }

        while head.current == Some(SPACE) {
            head.read_next_char();
        }

        Some(ScanMatch {
            token: ListBullet(space_count),
            end: head,
        })
    }

    fn match_markup_text_space(&self) -> Option<ScanMatch<'a>> {
        let mut head = self.read_head.clone();
        let i1 = head.index;

        while head.current == Some(SPACE) {
            head.read_next_char();
        }

        let mut has_new_line = false;
        if head.current == Some(NEW_LINE) {
            head.read_next_char();
            has_new_line = true;
        }

        if has_new_line && self.input[head.index..].starts_with(DELIMITED_CONTAINER_END) {
            return None;
        }

        while head.current == Some(SPACE) {
            head.read_next_char();
        }

        if head.current == Some(NEW_LINE) {
            return None;
        }

        let i2 = head.index;

        if i1 == i2 {
            return None;
        }

        if head.current == None {
            return None;
        }

        Some(ScanMatch {
            token: MarkupTextSpace,
            end: head,
        })
    }

    fn match_list_markup_text_space(&self) -> Option<ScanMatch<'a>> {
        let mut head = self.read_head.clone();
        let i1 = head.index;

        while head.current == Some(SPACE) {
            head.read_next_char();
        }

        let mut has_new_line = false;
        if head.current == Some(NEW_LINE) {
            head.read_next_char();
            has_new_line = true;
        }

        if has_new_line && self.input[head.index..].starts_with(DELIMITED_CONTAINER_END) {
            return None;
        }

        while head.current == Some(SPACE) {
            head.read_next_char();
        }

        if head.current == Some(NEW_LINE) {
            return None;
        }

        let i2 = head.index;

        if i1 == i2 {
            return None;
        }

        if has_new_line && head.current == Some(DASH) {
            return None;
        }

        if head.current == None {
            return None;
        }

        Some(ScanMatch {
            token: MarkupTextSpace,
            end: head,
        })
    }

    fn match_title_text_space(&self) -> Option<ScanMatch<'a>> {
        let mut head = self.read_head.clone();

        if head.current == Some(SPACE) {
            head.read_next_char();
        } else {
            return None;
        }

        while head.current == Some(SPACE) {
            head.read_next_char();
        }

        let has_text_next = head.current.is_some_and(|c| c != SPACE && c != NEW_LINE);

        if !has_text_next {
            return None;
        }

        Some(ScanMatch {
            token: TitleTextSpace,
            end: head,
        })
    }

    fn match_block_parameters_start(&self) -> Option<ScanMatch<'a>> {
        {
            let this = &self;
            let mut head = this.read_head.clone();

            if head.current == Some(LEFT_BRACKET) {
                head.read_next_char();
            } else {
                return None;
            }

            Some(ScanMatch {
                token: BlockParametersStart,
                end: head,
            })
        }
    }

    fn match_block_parameters_end(&self) -> Option<ScanMatch<'a>> {
        {
            let this = &self;
            let mut head = this.read_head.clone();

            if head.current == Some(RIGHT_BRACKET) {
                head.read_next_char();
            } else {
                return None;
            }

            Some(ScanMatch {
                token: BlockParametersEnd,
                end: head,
            })
        }
    }

    fn match_block_parameter_name_value_seperator(&self) -> Option<ScanMatch<'a>> {
        {
            let this = &self;
            let mut head = this.read_head.clone();

            if head.current == Some(EQUALS) {
                head.read_next_char();
            } else {
                return None;
            }

            Some(ScanMatch {
                token: BlockParameterNameValueSeperator,
                end: head,
            })
        }
    }

    fn match_raw_delimiter(&self) -> Option<ScanMatch<'a>> {
        {
            let this = &self;
            let mut head = this.read_head.clone();

            if head.current == Some(BACKTICK) {
                head.read_next_char();
            } else {
                return None;
            }

            Some(ScanMatch {
                token: RawDelimiter,
                end: head,
            })
        }
    }

    fn match_link_opening_delimiter(&self) -> Option<ScanMatch<'a>> {
        {
            let this = &self;
            let mut head = this.read_head.clone();

            if head.current == Some(LEFT_SQUARE_BRACKET) {
                head.read_next_char();
            } else {
                return None;
            }

            Some(ScanMatch {
                token: LinkOpeningDelimiter,
                end: head,
            })
        }
    }

    fn match_link_closing_delimiter(&self) -> Option<ScanMatch<'a>> {
        {
            let this = &self;
            let mut head = this.read_head.clone();

            if head.current == Some(RIGHT_SQUARE_BRACKET) {
                head.read_next_char();
            } else {
                return None;
            }

            Some(ScanMatch {
                token: LinkClosingDelimiter,
                end: head,
            })
        }
    }

    fn match_link_to_reference_joiner(&self) -> Option<ScanMatch<'a>> {
        {
            let this = &self;
            let mut head = this.read_head.clone();

            if head.current == Some(AT_SIGN) {
                head.read_next_char();
            } else {
                return None;
            }

            Some(ScanMatch {
                token: LinkToReferenceJoiner,
                end: head,
            })
        }
    }

    fn match_strong_delimiter(&self) -> Option<ScanMatch<'a>> {
        {
            let this = &self;
            let mut head = this.read_head.clone();

            if head.current == Some(ASTERISK) {
                head.read_next_char();
            } else {
                return None;
            }

            Some(ScanMatch {
                token: StrongDelimiter,
                end: head,
            })
        }
    }

    fn match_emphasis_delimiter(&self) -> Option<ScanMatch<'a>> {
        {
            let this = &self;
            let mut head = this.read_head.clone();

            if head.current == Some(UNDERSCORE) {
                head.read_next_char();
            } else {
                return None;
            }

            Some(ScanMatch {
                token: EmphasisDelimiter,
                end: head,
            })
        }
    }

    fn match_strikethrough_delimiter(&self) -> Option<ScanMatch<'a>> {
        {
            let this = &self;
            let mut head = this.read_head.clone();

            if head.current == Some(TILDE) {
                head.read_next_char();
            } else {
                return None;
            }

            Some(ScanMatch {
                token: StrikethroughDelimiter,
                end: head,
            })
        }
    }

    fn match_code_delimiter(&self) -> Option<ScanMatch<'a>> {
        let mut head = self.read_head.clone();

        for char in CODE_DELIMITER.chars() {
            if head.current == Some(char) {
                head.read_next_char();
                continue;
            } else {
                return None;
            }
        }

        Some(ScanMatch {
            token: CodeDelimiter,
            end: head,
        })
    }

    fn match_code_block(&self) -> Option<ScanMatch<'a>> {
        let mut head = self.read_head.clone();

        let i1 = self.index();

        loop {
            let on_start_of_line = head.column == 0;
            let prefix_matches = self.input[head.index..].starts_with(CODE_DELIMITER);
            if on_start_of_line && prefix_matches {
                let i2 = head.index;
                let text = &self.input[i1..i2];
                return Some(ScanMatch {
                    token: Code(text),
                    end: head,
                });
            } else if head.current == None {
                return None;
            } else {
                head.read_next_char();
            }
        }
    }

    fn match_blockbreak(&self) -> Option<ScanMatch<'a>> {
        //TODO: Maintain peek/lookahead on advance
        let mut head = self.read_head.clone();

        let mut new_line_count = 0;
        loop {
            match head.current {
                Some(SPACE) => {}
                Some(NEW_LINE) => {
                    new_line_count += 1;
                }
                _ => break,
            }
            head.read_next_char();
        }

        if new_line_count > 1 {
            Some(ScanMatch {
                token: BlockBreak,
                end: head,
            })
        } else {
            None
        }
    }

    fn match_linebreak(&self) -> Option<ScanMatch<'a>> {
        let mut head = self.read_head.clone();

        while let Some(SPACE) = head.current {
            head.read_next_char();
        }

        if head.current == Some(NEW_LINE) {
            head.read_next_char();
            Some(ScanMatch {
                token: LineBreak,
                end: head,
            })
        } else {
            None
        }
    }

    fn match_end_of_input(&self) -> Option<ScanMatch<'a>> {
        let mut head = self.read_head.clone();

        while let Some(SPACE) = head.current {
            head.read_next_char();
        }

        if head.current == None {
            Some(ScanMatch {
                token: EndOfInput,
                end: head,
            })
        } else {
            None
        }
    }

    fn match_escaped_markup_text(&self) -> Option<ScanMatch<'a>> {
        let mut head = self.read_head.clone();

        if head.current == Some(BACKSLASH) {
            head.read_next_char();
        } else {
            return None;
        }

        let i1 = head.index;

        if head.current.is_some() {
            head.read_next_char();
        } else {
            return None;
        }

        let i2 = head.index;
        let text = &self.input[i1..i2];

        Some(ScanMatch {
            token: MarkupText(text),
            end: head,
        })
    }

    fn match_raw_fragment(&self) -> Option<ScanMatch<'a>> {
        let mut head = self.read_head.clone();

        let i1 = head.index;

        while head
            .current
            .is_some_and(|c| !(c == BACKTICK || c == NEW_LINE))
        {
            head.read_next_char();
        }

        let i2 = head.index;
        let text = &self.input[i1..i2];

        if i1 == i2 {
            None
        } else {
            Some(ScanMatch {
                token: RawFragment(text),
                end: head,
            })
        }
    }

    fn match_data_value(&self) -> Option<ScanMatch<'a>> {
        let mut head = self.read_head.clone();

        let i1 = head.index;

        // TODO: There are probably other things we want to reject from
        // data value right?
        while head
            .current
            .is_some_and(|c| !(c == SPACE || c == NEW_LINE || c == VERTICAL_BAR))
        {
            head.read_next_char();
        }

        let i2 = head.index;
        let text = &self.input[i1..i2];

        while head.current.is_some_and(|c| c == SPACE) {
            head.read_next_char();
        }

        if i1 == i2 {
            None
        } else {
            Some(ScanMatch {
                token: DataValue(text),
                end: head,
            })
        }
    }

    fn match_markup_text(&self) -> Option<ScanMatch<'a>> {
        let mut head = self.read_head.clone();

        let i1 = head.index;

        while head.current.is_some_and(|c| !MARKUP_CHARS.contains(&c)) {
            head.read_next_char();
        }

        let i2 = head.index;
        let text = &self.input[i1..i2];

        if i1 == i2 {
            None
        } else {
            Some(ScanMatch {
                token: MarkupText(text),
                end: head,
            })
        }
    }

    //TODO: This is the same as match_markup_text
    fn match_title_text(&self) -> Option<ScanMatch<'a>> {
        let mut head = self.read_head.clone();

        let i1 = head.index;

        while head.current.is_some_and(|c| !MARKUP_CHARS.contains(&c)) {
            head.read_next_char();
        }

        let i2 = head.index;
        let text = &self.input[i1..i2];

        if i1 == i2 {
            None
        } else {
            Some(ScanMatch {
                token: TitleText(text),
                end: head,
            })
        }
    }

    //TODO:
    //
    // These are all the same logic
    //
    // match_data_identifier
    // match_block_parameter_value
    // match_block_parameter_name
    //
    // Can we collapse the tokens into one?

    fn match_block_parameter_value(&self) -> Option<ScanMatch<'a>> {
        {
            let this = &self;
            let mut head = this.read_head.clone();

            let i1 = head.index;

            while head.current.is_some_and(|c| {
                c.is_alphanumeric() || c == UNDERSCORE || c == DASH || c == FULL_STOP
            }) {
                head.read_next_char();
            }

            let i2 = head.index;
            let text = &this.input[i1..i2];

            if i1 == i2 {
                return None;
            }

            while head.current.is_some_and(|c| c == SPACE) {
                head.read_next_char();
            }

            Some(ScanMatch {
                token: BlockParameterValue(text),
                end: head,
            })
        }
    }

    fn match_block_parameter_name(&self) -> Option<ScanMatch<'a>> {
        {
            let this = &self;
            let mut head = this.read_head.clone();

            let i1 = head.index;

            while head.current.is_some_and(|c| {
                c.is_alphanumeric() || c == UNDERSCORE || c == DASH || c == FULL_STOP
            }) {
                head.read_next_char();
            }

            let i2 = head.index;
            let text = &this.input[i1..i2];

            if i1 == i2 {
                return None;
            }

            while head.current.is_some_and(|c| c == SPACE) {
                head.read_next_char();
            }

            Some(ScanMatch {
                token: BlockParameterName(text),
                end: head,
            })
        }
    }

    fn match_data_identifier(&self) -> Option<ScanMatch<'a>> {
        {
            let this = &self;
            let mut head = this.read_head.clone();

            let i1 = head.index;

            while head.current.is_some_and(|c| {
                c.is_alphanumeric() || c == UNDERSCORE || c == DASH || c == FULL_STOP
            }) {
                head.read_next_char();
            }

            let i2 = head.index;
            let text = &this.input[i1..i2];

            if i1 == i2 {
                return None;
            }

            while head.current.is_some_and(|c| c == SPACE) {
                head.read_next_char();
            }

            Some(ScanMatch {
                token: DataIdentifier(text),
                end: head,
            })
        }
    }

    fn match_data_key_value_seperator(&self) -> Option<ScanMatch<'a>> {
        let mut head = self.read_head.clone();

        if head.current == Some(COLON) {
            head.read_next_char()
        } else {
            return None;
        }

        while head.current.is_some_and(|c| c == SPACE) {
            head.read_next_char();
        }

        Some(ScanMatch {
            token: DataKeyValueSeperator,
            end: head,
        })
    }

    fn match_data_list_seperator(&self) -> Option<ScanMatch<'a>> {
        let mut head = self.read_head.clone();

        if head.current == Some(VERTICAL_BAR) {
            head.read_next_char()
        } else {
            return None;
        }

        while head.current.is_some_and(|c| c == SPACE) {
            head.read_next_char();
        }

        Some(ScanMatch {
            token: DataListSeperator,
            end: head,
        })
    }

    fn match_structured_data_directive(&self) -> Option<ScanMatch<'a>> {
        let mut head = self.read_head.clone();

        if head.current == Some(AT_SIGN) {
            head.read_next_char()
        } else {
            return None;
        }

        let i1 = head.index;
        while head.current.is_some_and(|c| c.is_alphanumeric()) {
            head.read_next_char();
        }

        let i2 = head.index;
        let text = &self.input[i1..i2];

        // TODO: Strictly speaking we should not allow a match with an
        // empty name, e.g just an '@'
        Some(ScanMatch {
            token: StructuredDataDirective(text),
            end: head,
        })
    }

    fn match_container_directive(&self) -> Option<ScanMatch<'a>> {
        let mut head = self.read_head.clone();

        if head.current == Some(EXCLAMATION_MARK) {
            head.read_next_char()
        } else {
            return None;
        }

        let i1 = head.index;
        while head.current.is_some_and(|c| c.is_alphanumeric()) {
            head.read_next_char();
        }

        let i2 = head.index;
        let text = &self.input[i1..i2];

        Some(ScanMatch {
            token: ContainerDirective(text),
            end: head,
        })
    }

    fn match_block_directive(&self) -> Option<ScanMatch<'a>> {
        let mut head = self.read_head.clone();

        if head.current == Some(HASH) {
            head.read_next_char()
        } else {
            return None;
        }

        let i1 = head.index;
        while head.current.is_some_and(|c| c.is_alphanumeric()) {
            head.read_next_char();
        }

        let i2 = head.index;
        let text = &self.input[i1..i2];

        Some(ScanMatch {
            token: BlockDirective(text),
            end: head,
        })
    }

    fn match_subsection_directive(&self) -> Option<ScanMatch<'a>> {
        let mut head = self.read_head.clone();

        if head.current == Some(SLASH) {
            head.read_next_char()
        } else {
            return None;
        }

        if head.current == Some(SLASH) {
            head.read_next_char()
        } else {
            return None;
        }

        if head.current == Some(SLASH) {
            head.read_next_char()
        } else {
            return None;
        }

        while head.current.is_some_and(|c| c == SPACE) {
            head.read_next_char();
        }

        Some(ScanMatch {
            token: SubSectionDirective,
            end: head,
        })
    }

    fn match_section_directive(&self) -> Option<ScanMatch<'a>> {
        let mut head = self.read_head.clone();

        if head.current == Some(SLASH) {
            head.read_next_char()
        } else {
            return None;
        }

        if head.current == Some(SLASH) {
            head.read_next_char()
        } else {
            return None;
        }

        while head.current.is_some_and(|c| c == SPACE) {
            head.read_next_char();
        }

        Some(ScanMatch {
            token: SectionDirective,
            end: head,
        })
    }

    fn match_title_directive(&self) -> Option<ScanMatch<'a>> {
        let mut head = self.read_head.clone();

        if head.current == Some(SLASH) {
            head.read_next_char()
        } else {
            return None;
        }

        while head.current.is_some_and(|c| c == SPACE) {
            head.read_next_char();
        }

        Some(ScanMatch {
            token: TitleDirective,
            end: head,
        })
    }

    fn match_delimited_container_start(&self) -> Option<ScanMatch<'a>> {
        let mut head = self.read_head.clone();

        for char in DELIMITED_CONTAINER_START.chars() {
            if head.current == Some(char) {
                head.read_next_char();
                continue;
            } else {
                return None;
            }
        }

        while head.current.is_some_and(|c| c == SPACE) {
            head.read_next_char();
        }

        Some(ScanMatch {
            token: DelimitedContainerStart,
            end: head,
        })
    }

    fn match_delimited_container_end(&self) -> Option<ScanMatch<'a>> {
        let mut head = self.read_head.clone();

        for char in DELIMITED_CONTAINER_END.chars() {
            if head.current == Some(char) {
                head.read_next_char();
                continue;
            } else {
                return None;
            }
        }

        while head.current.is_some_and(|c| c == SPACE) {
            head.read_next_char();
        }

        Some(ScanMatch {
            token: DelimitedContainerEnd,
            end: head,
        })
    }

    fn match_unknown(&self) -> ScanMatch<'a> {
        let mut head = self.read_head.clone();

        let i1 = head.index;

        while head.current.is_some_and(|c| !(c == SPACE || c == NEW_LINE)) {
            head.read_next_char();
        }

        let i2 = head.index;
        let text = &self.input[i1..i2];

        ScanMatch {
            token: Unknown(text),
            end: head,
        }
    }

    fn advance_past(&mut self, scan_match: &ScanMatch<'a>) {
        //TODO: read head is a bit chunky to clone about the place no?
        self.read_head = scan_match.end.clone();
    }
}

// TODO: Could hold different positions for full extent of
// matching text vs the sub text we are interested in
// e.g escaped chars
#[derive(Debug)]
struct ScanMatch<'a> {
    token: Token<'a>,
    //TODO: Store a position instead of a head
    end: ReadHead<'a>,
}
