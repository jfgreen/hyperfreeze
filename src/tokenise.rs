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
const LESS_THAN: char = '<';

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
        } else if let Some(bracket) = scanner.match_left_bracket() {
            scanner.advance_past(&bracket);
            Some(BlockParametersStart)
        } else if let Some(bracket) = scanner.match_right_bracket() {
            scanner.advance_past(&bracket);
            Some(BlockParametersEnd)
        } else if let Some(equals) = scanner.match_equals() {
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

    //TODO: remove &mut, do mutation in calling function
    fn read_structured_data_token(&mut self) -> Option<Token<'a>> {
        let scanner = &mut self.scanner;
        let position = scanner.position();
        let column = position.column;

        if let Some(identifier) = scanner.match_identifier()
            && column == 0
        {
            scanner.advance_past(&identifier);
            Some(DataIdentifier(identifier.text))
        } else if let Some(colon) = scanner.match_data_key_value_seperator() {
            scanner.advance_past(&colon);
            Some(DataKeyValueSeperator)
        } else if let Some(bar) = scanner.match_data_list_seperator() {
            scanner.advance_past(&bar);
            Some(DataListSeperator)
        } else if let Some(value) = scanner.match_data_value() {
            scanner.advance_past(&value);
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

        if let Some(raw_delimiter) = scanner.match_raw_delimiter() {
            scanner.advance_past(&raw_delimiter);
            Some(RawDelimiter)
        } else if let Some((list_bullet, space_count)) = scanner.match_list_bullet()
            && list_indent_allowed
        {
            scanner.advance_past(&list_bullet);
            Some(ListBullet(space_count))
        } else if let Some(markup_space) = scanner.match_markup_text_space()
            && !in_list
            && markup_space_allowed
        {
            scanner.advance_past(&markup_space);
            Some(MarkupTextSpace)
        } else if let Some(markup_space) = scanner.match_list_markup_text_space()
            && in_list
            && markup_space_allowed
        {
            scanner.advance_past(&markup_space);
            Some(MarkupTextSpace)
        } else if let Some(fragment) = scanner.match_raw_fragment()
            && self.in_raw
        {
            scanner.advance_past(&fragment);
            Some(RawFragment(fragment.text))
        } else if let Some(delimiter) = scanner.match_left_square_bracket() {
            scanner.advance_past(&delimiter);
            Some(LinkOpeningDelimiter)
        } else if let Some(delimiter) = scanner.match_right_square_bracket() {
            scanner.advance_past(&delimiter);
            Some(LinkClosingDelimiter)
        } else if let Some(at_sign) = scanner.match_at_sign()
            && self.current == LinkClosingDelimiter
        {
            scanner.advance_past(&at_sign);
            Some(LinkToReferenceJoiner)
        } else if let Some(identifier) = scanner.match_identifier()
            && self.current == LinkToReferenceJoiner
        {
            scanner.advance_past(&identifier);
            Some(DataIdentifier(identifier.text))
        } else if let Some(strong_delimiter) = scanner.match_strong_delimiter() {
            scanner.advance_past(&strong_delimiter);
            Some(StrongDelimiter)
        } else if let Some(emphasis_delimiter) = scanner.match_emphasis_delimiter() {
            scanner.advance_past(&emphasis_delimiter);
            Some(EmphasisDelimiter)
        } else if let Some(strikethrough_delimiter) = scanner.match_strikethrough_delimiter() {
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

        if let Some(delimiter) = scanner.match_code_delimiter() {
            //TODO: could the 'advance' past actually be done by the match?
            scanner.advance_past(&delimiter);
            Some(CodeDelimiter)
        } else if self.in_raw
            && let Some(code) = scanner.match_code_block()
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
pub struct Scanner<'a> {
    input: &'a str,
    read_head: ReadHead<'a>,
}

impl<'a> Scanner<'a> {
    pub fn new(input: &'a str) -> Self {
        Self {
            input,
            read_head: ReadHead::new(input),
        }
    }

    pub fn position(&self) -> Position {
        self.read_head.position()
    }

    //TODO: Meh, remove
    fn index(&self) -> usize {
        self.read_head.index
    }

    pub fn is_on_char(&self, c: char) -> bool {
        self.input[self.index()..].starts_with(c)
    }

    pub fn is_on_one_of(&self, chars: &[char]) -> bool {
        self.input[self.index()..].starts_with(chars)
    }

    pub fn is_on_empty_line(&self) -> bool {
        // TODO: if we could have an internal version of
        // this that returns the index of the new line
        // then we could be a lot more optimised
        self.input[self.index()..]
            .trim_start_matches(SPACE)
            .starts_with(NEW_LINE)
    }

    pub fn skip_while_on_empty_line(&mut self) {
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

    pub fn match_list_bullet(&self) -> Option<(ScanMatch<'a>, usize)> {
        let mut head = self.read_head.clone();
        let i1 = head.index;

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

        let i2 = head.index;

        Some((
            ScanMatch {
                text: &self.input[i1..i2],
                end: head,
            },
            space_count,
        ))
    }

    pub fn match_markup_text_space(&self) -> Option<ScanMatch<'a>> {
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
            text: &self.input[i1..i2],
            end: head,
        })
    }

    pub fn match_list_markup_text_space(&self) -> Option<ScanMatch<'a>> {
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
            text: &self.input[i1..i2],
            end: head,
        })
    }

    pub fn match_title_text_space(&self) -> Option<ScanMatch<'a>> {
        let mut head = self.read_head.clone();
        let i1 = head.index;

        if head.current == Some(SPACE) {
            head.read_next_char();
        } else {
            return None;
        }

        while head.current == Some(SPACE) {
            head.read_next_char();
        }

        let i2 = head.index;

        let has_text_next = head.current.is_some_and(|c| c != SPACE && c != NEW_LINE);

        if !has_text_next {
            return None;
        }

        Some(ScanMatch {
            text: &self.input[i1..i2],
            end: head,
        })
    }

    //TODO: Rename these to be closer to semantic token names

    pub fn match_left_bracket(&self) -> Option<ScanMatch<'a>> {
        self.match_char(LEFT_BRACKET)
    }

    pub fn match_right_bracket(&self) -> Option<ScanMatch<'a>> {
        self.match_char(RIGHT_BRACKET)
    }

    pub fn match_equals(&self) -> Option<ScanMatch<'a>> {
        self.match_char(EQUALS)
    }

    pub fn match_raw_delimiter(&self) -> Option<ScanMatch<'a>> {
        self.match_char(BACKTICK)
    }

    pub fn match_left_square_bracket(&self) -> Option<ScanMatch<'a>> {
        self.match_char(LEFT_SQUARE_BRACKET)
    }

    pub fn match_right_square_bracket(&self) -> Option<ScanMatch<'a>> {
        self.match_char(RIGHT_SQUARE_BRACKET)
    }

    pub fn match_at_sign(&self) -> Option<ScanMatch<'a>> {
        self.match_char(AT_SIGN)
    }

    pub fn match_strong_delimiter(&self) -> Option<ScanMatch<'a>> {
        self.match_char(ASTERISK)
    }

    pub fn match_emphasis_delimiter(&self) -> Option<ScanMatch<'a>> {
        self.match_char(UNDERSCORE)
    }

    pub fn match_strikethrough_delimiter(&self) -> Option<ScanMatch<'a>> {
        self.match_char(TILDE)
    }

    fn match_char(&self, c: char) -> Option<ScanMatch<'a>> {
        let mut head = self.read_head.clone();
        let i1 = head.index;

        if head.current == Some(c) {
            head.read_next_char();
        } else {
            return None;
        }

        let i2 = head.index;

        Some(ScanMatch {
            text: &self.input[i1..i2],
            end: head,
        })
    }

    pub fn match_code_delimiter(&self) -> Option<ScanMatch<'a>> {
        let mut head = self.read_head.clone();
        let i1 = head.index;

        for char in CODE_DELIMITER.chars() {
            if head.current == Some(char) {
                head.read_next_char();
                continue;
            } else {
                return None;
            }
        }

        let i2 = head.index;
        Some(ScanMatch {
            text: &self.input[i1..i2],
            end: head,
        })
    }

    pub fn match_code_block(&self) -> Option<ScanMatch<'a>> {
        let mut head = self.read_head.clone();

        let i1 = self.index();

        loop {
            let on_start_of_line = head.column == 0;
            let prefix_matches = self.input[head.index..].starts_with(CODE_DELIMITER);
            if on_start_of_line && prefix_matches {
                let i2 = head.index;
                return Some(ScanMatch {
                    text: &self.input[i1..i2],
                    end: head,
                });
            } else if head.current == None {
                return None;
            } else {
                head.read_next_char();
            }
        }
    }

    pub fn match_blockbreak(&self) -> Option<ScanMatch<'a>> {
        //TODO: Maintain peek/lookahead on advance
        let mut head = self.read_head.clone();

        let i1 = head.index;
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
            let i2 = head.index;
            Some(ScanMatch {
                text: &self.input[i1..i2],
                end: head,
            })
        } else {
            None
        }
    }

    pub fn match_linebreak(&self) -> Option<ScanMatch<'a>> {
        let mut head = self.read_head.clone();

        let i1 = head.index;

        while let Some(SPACE) = head.current {
            head.read_next_char();
        }

        if head.current == Some(NEW_LINE) {
            head.read_next_char();
            let i2 = head.index;
            Some(ScanMatch {
                text: &self.input[i1..i2],
                end: head,
            })
        } else {
            None
        }
    }

    pub fn match_end_of_input(&self) -> Option<ScanMatch<'a>> {
        let mut head = self.read_head.clone();

        let i1 = head.index;

        while let Some(SPACE) = head.current {
            head.read_next_char();
        }

        if head.current == None {
            let i2 = head.index;
            Some(ScanMatch {
                text: &self.input[i1..i2],
                end: head,
            })
        } else {
            None
        }
    }

    pub fn match_escaped(&self) -> Option<ScanMatch<'a>> {
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

        Some(ScanMatch {
            text: &self.input[i1..i2],
            end: head,
        })
    }

    pub fn match_raw_fragment(&self) -> Option<ScanMatch<'a>> {
        let mut head = self.read_head.clone();

        let i1 = head.index;

        while head
            .current
            .is_some_and(|c| !(c == BACKTICK || c == NEW_LINE))
        {
            head.read_next_char();
        }

        let i2 = head.index;

        if i1 == i2 {
            None
        } else {
            Some(ScanMatch {
                text: &self.input[i1..i2],
                end: head,
            })
        }
    }

    pub fn match_data_value(&self) -> Option<ScanMatch<'a>> {
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

        while head.current.is_some_and(|c| c == SPACE) {
            head.read_next_char();
        }

        if i1 == i2 {
            None
        } else {
            Some(ScanMatch {
                text: &self.input[i1..i2],
                end: head,
            })
        }
    }

    pub fn match_markup_text(&self) -> Option<ScanMatch<'a>> {
        let mut head = self.read_head.clone();

        let i1 = head.index;

        while head.current.is_some_and(|c| !MARKUP_CHARS.contains(&c)) {
            head.read_next_char();
        }

        let i2 = head.index;

        if i1 == i2 {
            None
        } else {
            Some(ScanMatch {
                text: &self.input[i1..i2],
                end: head,
            })
        }
    }

    pub fn match_identifier(&self) -> Option<ScanMatch<'a>> {
        let mut head = self.read_head.clone();

        let i1 = head.index;

        while head
            .current
            .is_some_and(|c| c.is_alphanumeric() || c == UNDERSCORE || c == DASH || c == FULL_STOP)
        {
            head.read_next_char();
        }

        let i2 = head.index;

        if i1 == i2 {
            return None;
        }

        while head.current.is_some_and(|c| c == SPACE) {
            head.read_next_char();
        }

        Some(ScanMatch {
            text: &self.input[i1..i2],
            end: head,
        })
    }

    pub fn match_data_key_value_seperator(&self) -> Option<ScanMatch<'a>> {
        let mut head = self.read_head.clone();

        let i1 = head.index;

        if head.current == Some(COLON) {
            head.read_next_char()
        } else {
            return None;
        }

        let i2 = head.index;

        while head.current.is_some_and(|c| c == SPACE) {
            head.read_next_char();
        }

        Some(ScanMatch {
            text: &self.input[i1..i2],
            end: head,
        })
    }

    pub fn match_data_list_seperator(&self) -> Option<ScanMatch<'a>> {
        let mut head = self.read_head.clone();

        let i1 = head.index;

        if head.current == Some(VERTICAL_BAR) {
            head.read_next_char()
        } else {
            return None;
        }

        let i2 = head.index;

        while head.current.is_some_and(|c| c == SPACE) {
            head.read_next_char();
        }

        Some(ScanMatch {
            text: &self.input[i1..i2],
            end: head,
        })
    }

    pub fn match_structured_data_directive(&self) -> Option<ScanMatch<'a>> {
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

        // TODO: Strictly speaking we should not allow a match with an
        // empty name, e.g just an '@'
        Some(ScanMatch {
            text: &self.input[i1..i2],
            end: head,
        })
    }

    pub fn match_container_directive(&self) -> Option<ScanMatch<'a>> {
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

        Some(ScanMatch {
            text: &self.input[i1..i2],
            end: head,
        })
    }

    pub fn match_block_directive(&self) -> Option<ScanMatch<'a>> {
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

        Some(ScanMatch {
            text: &self.input[i1..i2],
            end: head,
        })
    }

    pub fn match_subsection_directive(&self) -> Option<ScanMatch<'a>> {
        let mut head = self.read_head.clone();

        let i1 = head.index;
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

        let i2 = head.index;

        while head.current.is_some_and(|c| c == SPACE) {
            head.read_next_char();
        }

        Some(ScanMatch {
            text: &self.input[i1..i2],
            end: head,
        })
    }

    pub fn match_section_directive(&self) -> Option<ScanMatch<'a>> {
        let mut head = self.read_head.clone();

        let i1 = head.index;

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

        let i2 = head.index;

        while head.current.is_some_and(|c| c == SPACE) {
            head.read_next_char();
        }

        Some(ScanMatch {
            text: &self.input[i1..i2],
            end: head,
        })
    }

    pub fn match_title_directive(&self) -> Option<ScanMatch<'a>> {
        let mut head = self.read_head.clone();

        let i1 = head.index;

        if head.current == Some(SLASH) {
            head.read_next_char()
        } else {
            return None;
        }

        let i2 = head.index;

        while head.current.is_some_and(|c| c == SPACE) {
            head.read_next_char();
        }

        Some(ScanMatch {
            text: &self.input[i1..i2],
            end: head,
        })
    }

    pub fn match_delimited_container_start(&self) -> Option<ScanMatch<'a>> {
        let mut head = self.read_head.clone();

        let i1 = head.index;

        for char in DELIMITED_CONTAINER_START.chars() {
            if head.current == Some(char) {
                head.read_next_char();
                continue;
            } else {
                return None;
            }
        }

        let i2 = head.index;

        while head.current.is_some_and(|c| c == SPACE) {
            head.read_next_char();
        }

        Some(ScanMatch {
            text: &self.input[i1..i2],
            end: head,
        })
    }

    pub fn match_delimited_container_end(&self) -> Option<ScanMatch<'a>> {
        let mut head = self.read_head.clone();

        let i1 = head.index;

        if head.current == Some(LESS_THAN) {
            head.read_next_char()
        } else {
            return None;
        }

        if head.current == Some(LESS_THAN) {
            head.read_next_char()
        } else {
            return None;
        }

        if head.current == Some(LESS_THAN) {
            head.read_next_char()
        } else {
            return None;
        }

        let i2 = head.index;

        while head.current.is_some_and(|c| c == SPACE) {
            head.read_next_char();
        }

        Some(ScanMatch {
            text: &self.input[i1..i2],
            end: head,
        })
    }

    pub fn match_unknown(&self) -> ScanMatch<'a> {
        let mut head = self.read_head.clone();

        let i1 = head.index;

        while head.current.is_some_and(|c| !(c == SPACE || c == NEW_LINE)) {
            head.read_next_char();
        }

        let i2 = head.index;

        ScanMatch {
            text: &self.input[i1..i2],
            end: head,
        }
    }

    pub fn advance_past(&mut self, scan_match: &ScanMatch<'a>) {
        //TODO: read head is a bit chunky to clone about the place no?
        self.read_head = scan_match.end.clone();
    }
}

// TODO: Could hold different positions for full extent of
// matching text vs the sub text we are interested in
// e.g escaped chars
#[derive(Debug)]
pub struct ScanMatch<'a> {
    pub text: &'a str,
    //TODO: Store a position instead of a head
    end: ReadHead<'a>,
}
