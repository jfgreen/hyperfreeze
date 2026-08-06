use std::fmt::{self, Display, Formatter, Write};

use crate::token::{Indent, Token, TokenName, TokenSpec};

// TODO: Some kind of annotated example that describes the terminology
// Could even be a doc test?
// TODO: Make terminology less confusing

const SPACE: u8 = b' ';
const NEW_LINE: u8 = b'\n';
const COLON: u8 = b':';
const HASH: u8 = b'#';
const LEFT_SQUARE_BRACKET: u8 = b'[';
const RIGHT_SQUARE_BRACKET: u8 = b']';
const LEFT_BRACKET: u8 = b'(';
const RIGHT_BRACKET: u8 = b')';
const EQUALS: u8 = b'=';
const BACKTICK: u8 = b'`';
const ASTERISK: u8 = b'*';
const TILDE: u8 = b'~';
const UNDERSCORE: u8 = b'_';
const SLASH: u8 = b'/';
const BACKSLASH: u8 = b'\\';
const DASH: u8 = b'-';
const AT_SIGN: u8 = b'@';
const EXCLAMATION_MARK: u8 = b'!';
const VERTICAL_BAR: u8 = b'|';
const FULL_STOP: u8 = b'.';

const CODE_DELIMITER_PATTERN: [u8; 3] = [b'-', b'-', b'-'];
const CONTAINER_START_PATTERN: [u8; 3] = [b'>', b'>', b'>'];
const CONTAINER_END_PATTERN: [u8; 3] = [b'<', b'<', b'<'];

const MARKUP_CHARS: &[u8; 10] = &[
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

#[derive(PartialEq, Eq, Debug)]
pub struct LexemeString(Box<str>);

impl<'a> From<&'a str> for LexemeString {
    fn from(value: &'a str) -> Self {
        LexemeString(value.into())
    }
}

impl Display for LexemeString {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        let LexemeString(str) = self;
        for c in str.escape_default() {
            f.write_char(c)?;
        }
        Ok(())
    }
}

impl Display for TokenName {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        let TokenName(name) = self;
        f.write_str(name)
    }
}

#[derive(PartialEq, Eq, Debug)]
pub struct TokenDescription {
    pub name: TokenName,
    pub lexeme: LexemeString,
    pub position: Position,
}

type Matcher = for<'a> fn(&Scanner<'a>) -> Option<ScanMatch<'a>>;

#[derive(PartialEq, Eq, Debug)]
pub struct UnexpectedTokenError {
    pub expected: TokenName,
    pub actual: TokenDescription,
}

pub struct Spanned<'a, T>
where
    T: TokenSpec<'a>,
{
    pub value: T,
    pub span: Span,
    pub lexeme: &'a str,
}

impl<'a, T> Spanned<'a, T>
where
    T: TokenSpec<'a>,
{
    pub fn lexeme_to_owned(&self) -> LexemeString {
        LexemeString::from(self.lexeme)
    }

    pub fn description(&self) -> TokenDescription {
        TokenDescription {
            name: T::NAME,
            lexeme: self.lexeme_to_owned(),
            position: self.span.start,
        }
    }
}

#[derive(Debug, Copy, Clone)]
pub struct SpannedToken<'a> {
    pub value: Token<'a>,
    pub span: Span,
    pub lexeme: &'a str,
}

impl<'a> SpannedToken<'a> {
    pub fn require<T>(&self) -> Result<Spanned<'a, T>, UnexpectedTokenError>
    where
        T: TokenSpec<'a>,
    {
        self.try_consume().ok_or(UnexpectedTokenError {
            expected: T::NAME,
            actual: self.description(),
        })
    }

    pub fn try_consume<T>(&self) -> Option<Spanned<'a, T>>
    where
        T: TokenSpec<'a>,
    {
        T::try_from(self.value).ok().map(|token| Spanned {
            value: token,
            span: self.span,
            lexeme: self.lexeme,
        })
    }

    pub fn is<T>(&self) -> bool
    where
        T: TokenSpec<'a>,
    {
        T::try_from(self.value).is_ok()
    }

    pub fn description(&self) -> TokenDescription {
        TokenDescription {
            name: self.value.name(),
            lexeme: LexemeString::from(self.lexeme),
            position: self.span.start,
        }
    }

    pub fn lexeme_to_owned(&self) -> LexemeString {
        LexemeString::from(self.lexeme)
    }
}

//TODO: Could we make the state push/pop transitions
// a) Be data driven
// b) Live inside the tokeniser?

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum ScanMode {
    ElementStart,
    Markup,
    ListMarkup,
    Raw,
    Title,
    Header,
    StructuredData,
    Code,
}

impl ScanMode {
    fn try_match<'a>(self, scanner: &Scanner<'a>) -> Option<ScanMatch<'a>> {
        let matchers = match self {
            ScanMode::ElementStart => SCAN_ELEMENT_START,
            ScanMode::Markup => SCAN_MARKUP,
            ScanMode::ListMarkup => SCAN_LIST_MARKUP,
            ScanMode::Raw => SCAN_RAW,
            ScanMode::Title => SCAN_TITLE,
            ScanMode::Header => SCAN_HEADER,
            ScanMode::StructuredData => SCAN_STRUCTURED_DATA,
            ScanMode::Code => SCAN_CODE,
        };

        matchers.iter().find_map(|m| (m)(scanner))
    }
}

pub struct Tokeniser<'a> {
    scanner: Scanner<'a>,
    peeked: Option<SpannedToken<'a>>,
    token_count: usize,
    max_tokens: usize,
}

impl<'a> Tokeniser<'a> {
    pub fn new(input: &'a str) -> Self {
        let mut scanner = Scanner::new(input);
        scanner.skip_while_on_empty_line();

        Tokeniser {
            scanner,
            peeked: None,
            token_count: 0,
            max_tokens: input.len(),
        }
    }

    pub fn push_mode(&mut self, mode: ScanMode) {
        self.scanner.push_mode(mode);
    }

    pub fn pop_mode(&mut self) {
        self.scanner.pop_mode();
    }

    pub fn peek(&mut self) -> SpannedToken<'a> {
        *self.peeked.get_or_insert_with(|| self.scanner.scan())
    }

    pub fn advance(&mut self) -> SpannedToken<'a> {
        let next = if let Some(peeked_token) = self.peeked.take() {
            peeked_token
        } else {
            self.scanner.scan()
        };

        assert!(
            self.token_count <= self.max_tokens,
            "Posible infinite loop detected"
        );

        // if !matches!(next.value, Token::EndOfInput) {
        self.scanner.advance_to(next.span.end);
        // }

        next
    }
}

#[derive(Eq, PartialEq, Clone, Copy, Debug)]
pub struct Span {
    pub start: Position,
    pub end: Position,
}

#[derive(Eq, PartialEq, Clone, Copy, Debug)]
pub struct Position {
    pub column: u32,
    pub row: u32,
    pub index: usize,
}

//TODO: Ideally we wouldn't need a copyable read head
#[derive(Debug, Clone)]
struct ReadHead<'a> {
    input_bytes: &'a [u8],
    //TODO: direct usage of index, column is a bit meh
    index: usize,
    column: u32,
    row: u32,
}

impl<'a> ReadHead<'a> {
    fn new(input: &'a str) -> Self {
        Self {
            input_bytes: input.as_bytes(),
            index: 0,
            column: 0,
            row: 0,
        }
    }

    fn position(&self) -> Position {
        Position {
            column: self.column,
            row: self.row,
            index: self.index,
        }
    }

    fn move_to(&mut self, position: Position) {
        self.column = position.column;
        self.row = position.row;
        self.index = position.index;
    }

    fn read_next_byte(&mut self) {
        let next_index = self.index + 1;
        if next_index < self.input_bytes.len() {
            if self.input_bytes[self.index] == b'\n' {
                self.column = 0;
                self.row += 1;
            } else {
                self.column += 1;
            }
            self.index = next_index;
        } else {
            self.index = self.input_bytes.len();
        }
    }

    fn is_on(&self, byte: u8) -> bool {
        self.input_bytes.get(self.index).copied() == Some(byte)
    }

    fn is_on_bytes(&self, pattern: &[u8]) -> bool {
        self.input_bytes[self.index..].starts_with(pattern)
    }

    fn is_on_one_of(&self, bytes: &[u8]) -> bool {
        bytes.iter().copied().any(|b| self.is_on(b))
    }

    fn is_end_of_input(&self) -> bool {
        self.index >= self.input_bytes.len()
    }

    fn is_on_ascii_alphanumeric(&self) -> bool {
        self.input_bytes
            .get(self.index)
            .is_some_and(u8::is_ascii_alphanumeric)
    }

    fn has_input_remaining(&self) -> bool {
        !self.is_end_of_input()
    }
}

// TODO: having stuff split across scanner and head is meh
// the way this should work is
// we feed a pattern into the scanner
// and it returns a match or not
//
// Except: we want this to be expressive... macros?
#[derive(Debug)]
struct Scanner<'a> {
    //TODO: Actually store a peek
    input: &'a str,
    mode_stack: Vec<ScanMode>,
    last_token: Option<Token<'a>>,
    read_head: ReadHead<'a>,
}

impl<'a> Scanner<'a> {
    fn new(input: &'a str) -> Self {
        Self {
            input,
            mode_stack: vec![],
            last_token: None,
            read_head: ReadHead::new(input),
        }
    }

    fn push_mode(&mut self, mode: ScanMode) {
        self.mode_stack.push(mode);
    }

    fn pop_mode(&mut self) {
        self.mode_stack.pop();
    }

    fn scan(&mut self) -> SpannedToken<'a> {
        let scan_match = self
            .mode_stack
            .last()
            //TODO: passing self is a bit hmm
            .and_then(|mode| mode.try_match(self))
            .unwrap_or(match_generic(self));

        let start = self.read_head.position();
        let end = scan_match.end;
        let lexeme = &self.input[start.index..end.index];
        let span = Span { start, end };

        self.last_token = Some(scan_match.token);

        SpannedToken {
            value: scan_match.token,
            lexeme,
            span,
        }
    }

    //TODO: can we avoid skipping on empty line being special handling?

    fn skip_while_on_empty_line(&mut self) {
        let mut start_of_line = self.read_head.clone();
        let mut head = start_of_line.clone();

        loop {
            while head.is_on(SPACE) {
                head.read_next_byte();
            }

            if head.is_on(NEW_LINE) {
                head.read_next_byte();
                start_of_line = head.clone();
            } else {
                break;
            }
        }

        self.read_head = start_of_line.clone();

        // while self.is_on_empty_line() {
        //     while self.input[self.read_head.index..].starts_with([SPACE, NEW_LINE]) {
        //         self.read_head.read_next_byte();
        //     }
        // }
    }

    pub fn advance_to(&mut self, position: Position) {
        self.read_head.move_to(position);
    }
}

#[derive(Debug)]
pub struct ScanMatch<'a> {
    token: Token<'a>,
    end: Position,
}

fn match_list_bullet<'a>(scanner: &Scanner<'a>) -> Option<ScanMatch<'a>> {
    let mut head = scanner.read_head.clone();

    if !matches!(
        scanner.last_token,
        Some(Token::LineBreak | Token::BlockBreak) | None
    ) {
        return None;
    }

    let mut space_count = 0;

    while head.is_on(SPACE) {
        space_count += 1;
        head.read_next_byte();
    }

    if head.is_on(DASH) {
        head.read_next_byte();
    } else {
        return None;
    }

    while head.is_on(SPACE) {
        head.read_next_byte();
    }

    Some(ScanMatch {
        token: Token::ListBullet(Indent { space_count }),
        end: head.position(),
    })
}

fn match_markup_text_space<'a>(scanner: &Scanner<'a>) -> Option<ScanMatch<'a>> {
    let mut head = scanner.read_head.clone();
    let i1 = head.index;

    while head.is_on(SPACE) {
        head.read_next_byte();
    }

    let has_new_line = head.is_on(NEW_LINE);

    if has_new_line {
        head.read_next_byte();
    }

    if has_new_line && head.is_on_bytes(&CONTAINER_END_PATTERN) {
        return None;
    }

    while head.is_on(SPACE) {
        head.read_next_byte();
    }

    if head.is_on(NEW_LINE) {
        return None;
    }

    let i2 = head.index;

    if i1 == i2 {
        return None;
    }

    if head.is_end_of_input() {
        return None;
    }

    Some(ScanMatch {
        token: Token::MarkupTextSpace,
        end: head.position(),
    })
}

fn match_list_markup_text_space<'a>(scanner: &Scanner<'a>) -> Option<ScanMatch<'a>> {
    let mut head = scanner.read_head.clone();
    let i1 = head.index;

    while head.is_on(SPACE) {
        head.read_next_byte();
    }

    let has_new_line = head.is_on(NEW_LINE);

    if has_new_line {
        head.read_next_byte();
    }

    if has_new_line && head.is_on_bytes(&CONTAINER_END_PATTERN) {
        return None;
    }

    while head.is_on(SPACE) {
        head.read_next_byte();
    }

    if head.is_on(NEW_LINE) {
        return None;
    }

    let i2 = head.index;

    if i1 == i2 {
        return None;
    }

    if has_new_line && head.is_on(DASH) {
        return None;
    }

    if head.is_end_of_input() {
        return None;
    }

    Some(ScanMatch {
        token: Token::MarkupTextSpace,
        end: head.position(),
    })
}

fn match_title_text_space<'a>(scanner: &Scanner<'a>) -> Option<ScanMatch<'a>> {
    let mut head = scanner.read_head.clone();

    if head.is_on(SPACE) {
        head.read_next_byte();
    } else {
        return None;
    }

    while head.is_on(SPACE) {
        head.read_next_byte();
    }

    let has_text_next = head.has_input_remaining() && !head.is_on_one_of(&[SPACE, NEW_LINE]);

    if !has_text_next {
        return None;
    }

    Some(ScanMatch {
        token: Token::TitleTextSpace,
        end: head.position(),
    })
}

fn match_parameters_start<'a>(scanner: &Scanner<'a>) -> Option<ScanMatch<'a>> {
    let mut head = scanner.read_head.clone();

    if head.is_on(LEFT_BRACKET) {
        head.read_next_byte();
    } else {
        return None;
    }

    Some(ScanMatch {
        token: Token::BlockParametersStart,
        end: head.position(),
    })
}

fn match_parameters_end<'a>(scanner: &Scanner<'a>) -> Option<ScanMatch<'a>> {
    let mut head = scanner.read_head.clone();

    if head.is_on(RIGHT_BRACKET) {
        head.read_next_byte();
    } else {
        return None;
    }

    Some(ScanMatch {
        token: Token::BlockParametersEnd,
        end: head.position(),
    })
}

fn match_parameter_name_value_seperator<'a>(scanner: &Scanner<'a>) -> Option<ScanMatch<'a>> {
    let mut head = scanner.read_head.clone();

    if head.is_on(EQUALS) {
        head.read_next_byte();
    } else {
        return None;
    }

    Some(ScanMatch {
        token: Token::BlockParameterNameValueSeperator,
        end: head.position(),
    })
}

fn match_raw_delimiter<'a>(scanner: &Scanner<'a>) -> Option<ScanMatch<'a>> {
    let mut head = scanner.read_head.clone();

    if head.is_on(BACKTICK) {
        head.read_next_byte();
    } else {
        return None;
    }

    Some(ScanMatch {
        token: Token::RawDelimiter,
        end: head.position(),
    })
}

fn match_link_opening_delimiter<'a>(scanner: &Scanner<'a>) -> Option<ScanMatch<'a>> {
    let mut head = scanner.read_head.clone();

    if head.is_on(LEFT_SQUARE_BRACKET) {
        head.read_next_byte();
    } else {
        return None;
    }

    Some(ScanMatch {
        token: Token::LinkOpeningDelimiter,
        end: head.position(),
    })
}

fn match_link_closing_delimiter<'a>(scanner: &Scanner<'a>) -> Option<ScanMatch<'a>> {
    let mut head = scanner.read_head.clone();

    if head.is_on(RIGHT_SQUARE_BRACKET) {
        head.read_next_byte();
    } else {
        return None;
    }

    Some(ScanMatch {
        token: Token::LinkClosingDelimiter,
        end: head.position(),
    })
}

fn match_link_to_reference<'a>(scanner: &Scanner<'a>) -> Option<ScanMatch<'a>> {
    let mut head = scanner.read_head.clone();

    if !matches!(scanner.last_token, Some(Token::LinkClosingDelimiter)) {
        return None;
    }

    if head.is_on(AT_SIGN) {
        head.read_next_byte();
    } else {
        return None;
    }

    let i1 = head.index;

    while head.is_on_ascii_alphanumeric() || head.is_on_one_of(&[UNDERSCORE, DASH, FULL_STOP]) {
        head.read_next_byte();
    }

    let i2 = head.index;
    let text = &scanner.input[i1..i2];

    if i1 == i2 {
        return None;
    }

    Some(ScanMatch {
        token: Token::LinkToReference(text),
        end: head.position(),
    })
}

fn match_strong_delimiter<'a>(scanner: &Scanner<'a>) -> Option<ScanMatch<'a>> {
    let mut head = scanner.read_head.clone();

    if head.is_on(ASTERISK) {
        head.read_next_byte();
    } else {
        return None;
    }

    Some(ScanMatch {
        token: Token::StrongDelimiter,
        end: head.position(),
    })
}

fn match_emphasis_delimiter<'a>(scanner: &Scanner<'a>) -> Option<ScanMatch<'a>> {
    let mut head = scanner.read_head.clone();

    if head.is_on(UNDERSCORE) {
        head.read_next_byte();
    } else {
        return None;
    }

    Some(ScanMatch {
        token: Token::EmphasisDelimiter,
        end: head.position(),
    })
}

fn match_strikethrough_delimiter<'a>(scanner: &Scanner<'a>) -> Option<ScanMatch<'a>> {
    let mut head = scanner.read_head.clone();

    if head.is_on(TILDE) {
        head.read_next_byte();
    } else {
        return None;
    }

    Some(ScanMatch {
        token: Token::StrikethroughDelimiter,
        end: head.position(),
    })
}

fn match_code_delimiter<'a>(scanner: &Scanner<'a>) -> Option<ScanMatch<'a>> {
    let mut head = scanner.read_head.clone();

    for byte in CODE_DELIMITER_PATTERN {
        if head.is_on(byte) {
            head.read_next_byte();
        } else {
            return None;
        }
    }

    Some(ScanMatch {
        token: Token::CodeDelimiter,
        end: head.position(),
    })
}

fn match_code_block<'a>(scanner: &Scanner<'a>) -> Option<ScanMatch<'a>> {
    let mut head = scanner.read_head.clone();

    let i1 = head.index;

    loop {
        let on_start_of_line = head.column == 0;
        let prefix_matches = head.is_on_bytes(&CODE_DELIMITER_PATTERN);
        if on_start_of_line && prefix_matches {
            let i2 = head.index;
            let text = &scanner.input[i1..i2];
            return Some(ScanMatch {
                token: Token::Code(text),
                end: head.position(),
            });
        } else if head.is_end_of_input() {
            return None;
        }

        head.read_next_byte();
    }
}

fn match_blockbreak<'a>(scanner: &Scanner<'a>) -> Option<ScanMatch<'a>> {
    let mut head = scanner.read_head.clone();

    let mut new_line_count = 0;
    loop {
        if head.is_on(SPACE) {
            // TODO: meh
        } else if head.is_on(NEW_LINE) {
            new_line_count += 1;
        } else {
            break;
        }
        head.read_next_byte();
    }

    if new_line_count > 1 {
        Some(ScanMatch {
            token: Token::BlockBreak,
            end: head.position(),
        })
    } else {
        None
    }
}

fn match_linebreak<'a>(scanner: &Scanner<'a>) -> Option<ScanMatch<'a>> {
    let mut head = scanner.read_head.clone();

    while head.is_on(SPACE) {
        head.read_next_byte();
    }

    if head.is_on(NEW_LINE) {
        head.read_next_byte();
        Some(ScanMatch {
            token: Token::LineBreak,
            end: head.position(),
        })
    } else {
        None
    }
}

fn match_end_of_input<'a>(scanner: &Scanner<'a>) -> Option<ScanMatch<'a>> {
    let mut head = scanner.read_head.clone();

    if head.is_on(NEW_LINE) {
        head.read_next_byte();
    }

    while head.is_on(SPACE) {
        head.read_next_byte();
    }

    if head.is_end_of_input() {
        Some(ScanMatch {
            token: Token::EndOfInput,
            end: head.position(),
        })
    } else {
        None
    }
}

fn match_escaped_markup_text<'a>(scanner: &Scanner<'a>) -> Option<ScanMatch<'a>> {
    let mut head = scanner.read_head.clone();

    if head.is_on(BACKSLASH) {
        head.read_next_byte();
    } else {
        return None;
    }

    let i1 = head.index;

    if head.has_input_remaining() {
        head.read_next_byte();
    } else {
        return None;
    }

    let i2 = head.index;
    let text = &scanner.input[i1..i2];

    Some(ScanMatch {
        token: Token::MarkupText(text),
        end: head.position(),
    })
}

fn match_raw_fragment<'a>(scanner: &Scanner<'a>) -> Option<ScanMatch<'a>> {
    let mut head = scanner.read_head.clone();

    let i1 = head.index;

    while head.has_input_remaining() && !head.is_on_one_of(&[BACKTICK, NEW_LINE]) {
        head.read_next_byte();
    }

    let i2 = head.index;
    let text = &scanner.input[i1..i2];

    if i1 == i2 {
        None
    } else {
        Some(ScanMatch {
            token: Token::RawFragment(text),
            end: head.position(),
        })
    }
}

fn match_data_value<'a>(scanner: &Scanner<'a>) -> Option<ScanMatch<'a>> {
    let mut head = scanner.read_head.clone();

    let i1 = head.index;

    while head.has_input_remaining() && !head.is_on_one_of(&[SPACE, NEW_LINE, VERTICAL_BAR]) {
        head.read_next_byte();
    }

    let i2 = head.index;
    let text = &scanner.input[i1..i2];

    while head.is_on(SPACE) {
        head.read_next_byte();
    }

    if i1 == i2 {
        None
    } else {
        Some(ScanMatch {
            //TODO: meh amounts of ceremony here
            token: Token::DataValue(text),
            end: head.position(),
        })
    }
}

fn match_markup_text<'a>(scanner: &Scanner<'a>) -> Option<ScanMatch<'a>> {
    let mut head = scanner.read_head.clone();

    let i1 = head.index;

    while head.has_input_remaining() && !head.is_on_one_of(MARKUP_CHARS) {
        head.read_next_byte();
    }

    let i2 = head.index;
    let text = &scanner.input[i1..i2];

    if i1 == i2 {
        None
    } else {
        Some(ScanMatch {
            token: Token::MarkupText(text),
            end: head.position(),
        })
    }
}

fn match_title_text<'a>(scanner: &Scanner<'a>) -> Option<ScanMatch<'a>> {
    let mut head = scanner.read_head.clone();

    let i1 = head.index;

    while head.has_input_remaining() && !head.is_on_one_of(MARKUP_CHARS) {
        head.read_next_byte();
    }

    let i2 = head.index;
    let text = &scanner.input[i1..i2];

    if i1 == i2 {
        None
    } else {
        Some(ScanMatch {
            token: Token::TitleText(text),
            end: head.position(),
        })
    }
}

fn match_parameter_value<'a>(scanner: &Scanner<'a>) -> Option<ScanMatch<'a>> {
    let mut head = scanner.read_head.clone();

    if !matches!(
        scanner.last_token,
        Some(Token::BlockParameterNameValueSeperator)
    ) {
        return None;
    }

    let i1 = head.index;

    while head.is_on_one_of(&[UNDERSCORE, HASH, FULL_STOP]) || head.is_on_ascii_alphanumeric() {
        head.read_next_byte();
    }

    let i2 = head.index;
    let text = &scanner.input[i1..i2];

    if i1 == i2 {
        return None;
    }

    while head.is_on(SPACE) {
        head.read_next_byte();
    }

    Some(ScanMatch {
        token: Token::BlockParameterValue(text),
        end: head.position(),
    })
}

fn match_parameter_name<'a>(scanner: &Scanner<'a>) -> Option<ScanMatch<'a>> {
    let mut head = scanner.read_head.clone();

    let i1 = head.index;

    while head.is_on_ascii_alphanumeric() || head.is_on_one_of(&[UNDERSCORE, DASH, FULL_STOP]) {
        head.read_next_byte();
    }

    let i2 = head.index;
    let text = &scanner.input[i1..i2];

    if i1 == i2 {
        return None;
    }

    while head.is_on(SPACE) {
        head.read_next_byte();
    }

    Some(ScanMatch {
        token: Token::BlockParameterName(text),
        end: head.position(),
    })
}

fn match_data_identifier<'a>(scanner: &Scanner<'a>) -> Option<ScanMatch<'a>> {
    let mut head = scanner.read_head.clone();

    let i1 = head.index;

    if !matches!(scanner.last_token, Some(Token::LineBreak)) {
        return None;
    }

    while head.is_on_ascii_alphanumeric() || head.is_on_one_of(&[UNDERSCORE, DASH, FULL_STOP]) {
        head.read_next_byte();
    }

    let i2 = head.index;
    let text = &scanner.input[i1..i2];

    if i1 == i2 {
        return None;
    }

    Some(ScanMatch {
        token: Token::DataIdentifier(text),
        end: head.position(),
    })
}

fn match_data_key_value_seperator<'a>(scanner: &Scanner<'a>) -> Option<ScanMatch<'a>> {
    let mut head = scanner.read_head.clone();

    while head.is_on(SPACE) {
        head.read_next_byte();
    }

    if head.is_on(COLON) {
        head.read_next_byte();
    } else {
        return None;
    }

    while head.is_on(SPACE) {
        head.read_next_byte();
    }

    Some(ScanMatch {
        token: Token::DataKeyValueSeperator,
        end: head.position(),
    })
}

fn match_data_list_seperator<'a>(scanner: &Scanner<'a>) -> Option<ScanMatch<'a>> {
    let mut head = scanner.read_head.clone();

    if head.is_on(VERTICAL_BAR) {
        head.read_next_byte();
    } else {
        return None;
    }

    while head.is_on(SPACE) {
        head.read_next_byte();
    }

    Some(ScanMatch {
        token: Token::DataListSeperator,
        end: head.position(),
    })
}

//TODO: This should probably be seperate functions for references, metadata
fn match_data_directive<'a>(scanner: &Scanner<'a>) -> Option<ScanMatch<'a>> {
    let mut head = scanner.read_head.clone();

    let i1 = head.index;

    if head.is_on(AT_SIGN) {
        head.read_next_byte();
    } else {
        return None;
    }

    while head.is_on_ascii_alphanumeric() {
        head.read_next_byte();
    }

    let i2 = head.index;
    let text = &scanner.input[i1..i2];

    let token = match text {
        "@metadata" => Token::MetadataDirective,
        "@references" => Token::ReferencesDirective,
        _ => Token::UnknownDirective(text),
    };

    Some(ScanMatch {
        token,
        end: head.position(),
    })
}

fn match_container_directive<'a>(scanner: &Scanner<'a>) -> Option<ScanMatch<'a>> {
    let mut head = scanner.read_head.clone();

    let i1 = head.index;

    if head.is_on(EXCLAMATION_MARK) {
        head.read_next_byte();
    } else {
        return None;
    }

    while head.is_on_ascii_alphanumeric() {
        head.read_next_byte();
    }

    let i2 = head.index;
    let text = &scanner.input[i1..i2];

    let token = match text {
        "!info" => Token::InfoContainerDirective,
        _ => Token::UnknownDirective(text),
    };

    Some(ScanMatch {
        token,
        end: head.position(),
    })
}

//TODO: should be seperate match functions?
fn match_block_directive<'a>(scanner: &Scanner<'a>) -> Option<ScanMatch<'a>> {
    let mut head = scanner.read_head.clone();

    let i1 = head.index;

    if head.is_on(HASH) {
        head.read_next_byte();
    } else {
        return None;
    }

    while head.is_on_ascii_alphanumeric() {
        head.read_next_byte();
    }

    let i2 = head.index;
    let text = &scanner.input[i1..i2];

    let token = match text {
        "#paragraph" => Token::ParagraphDirective,
        "#list" => Token::ListDirective,
        "#code" => Token::CodeDirective,
        _ => Token::UnknownDirective(text),
    };

    Some(ScanMatch {
        token,
        end: head.position(),
    })
}

fn match_subsection_directive<'a>(scanner: &Scanner<'a>) -> Option<ScanMatch<'a>> {
    let mut head = scanner.read_head.clone();

    if head.is_on(SLASH) {
        head.read_next_byte();
    } else {
        return None;
    }

    if head.is_on(SLASH) {
        head.read_next_byte();
    } else {
        return None;
    }

    if head.is_on(SLASH) {
        head.read_next_byte();
    } else {
        return None;
    }

    while head.is_on(SPACE) {
        head.read_next_byte();
    }

    Some(ScanMatch {
        token: Token::SubSectionDirective,
        end: head.position(),
    })
}

fn match_section_directive<'a>(scanner: &Scanner<'a>) -> Option<ScanMatch<'a>> {
    let mut head = scanner.read_head.clone();

    if head.is_on(SLASH) {
        head.read_next_byte();
    } else {
        return None;
    }

    if head.is_on(SLASH) {
        head.read_next_byte();
    } else {
        return None;
    }

    while head.is_on(SPACE) {
        head.read_next_byte();
    }

    Some(ScanMatch {
        token: Token::SectionDirective,
        end: head.position(),
    })
}

fn match_title_directive<'a>(scanner: &Scanner<'a>) -> Option<ScanMatch<'a>> {
    let mut head = scanner.read_head.clone();

    if head.is_on(SLASH) {
        head.read_next_byte();
    } else {
        return None;
    }

    while head.is_on(SPACE) {
        head.read_next_byte();
    }

    Some(ScanMatch {
        token: Token::TitleDirective,
        end: head.position(),
    })
}

fn match_container_start<'a>(scanner: &Scanner<'a>) -> Option<ScanMatch<'a>> {
    let mut head = scanner.read_head.clone();

    for char in CONTAINER_START_PATTERN {
        if head.is_on(char) {
            head.read_next_byte();
        } else {
            return None;
        }
    }

    while head.is_on(SPACE) {
        head.read_next_byte();
    }

    Some(ScanMatch {
        token: Token::DelimitedContainerStart,
        end: head.position(),
    })
}

fn match_container_end<'a>(scanner: &Scanner<'a>) -> Option<ScanMatch<'a>> {
    let mut head = scanner.read_head.clone();

    for char in CONTAINER_END_PATTERN {
        if head.is_on(char) {
            head.read_next_byte();
        } else {
            return None;
        }
    }

    while head.is_on(SPACE) {
        head.read_next_byte();
    }

    Some(ScanMatch {
        token: Token::DelimitedContainerEnd,
        end: head.position(),
    })
}

fn match_unknown<'a>(scanner: &Scanner<'a>) -> ScanMatch<'a> {
    let mut head = scanner.read_head.clone();

    let i1 = head.index;

    while head.has_input_remaining() && !head.is_on_one_of(&[SPACE, NEW_LINE]) {
        head.read_next_byte();
    }

    let i2 = head.index;
    let text = &scanner.input[i1..i2];

    ScanMatch {
        token: Token::Unknown(text),
        end: head.position(),
    }
}

fn match_generic<'a>(scanner: &Scanner<'a>) -> ScanMatch<'a> {
    if let Some(end_of_input) = match_end_of_input(scanner) {
        end_of_input
    } else if let Some(blockbreak) = match_blockbreak(scanner) {
        blockbreak
    } else if let Some(linebreak) = match_linebreak(scanner) {
        linebreak
    } else {
        match_unknown(scanner)
    }
}

const CONTAINER_START: Matcher = match_container_start;
const CONTAINER_END: Matcher = match_container_end;
const SUBSECTION_DIRECTIVE: Matcher = match_subsection_directive;
const SECTION_DIRECTIVE: Matcher = match_section_directive;
const TITLE_DIRECTIVE: Matcher = match_title_directive;
const DATA_DIRECTIVE: Matcher = match_data_directive;
const CONTAINER_DIRECTIVE: Matcher = match_container_directive;
const BLOCK_DIRECTIVE: Matcher = match_block_directive;
const LIST_BULLET: Matcher = match_list_bullet;
const RAW_DELIMITER: Matcher = match_raw_delimiter;
const RAW_FRAGMENT: Matcher = match_raw_fragment;
const LINK_OPENING: Matcher = match_link_opening_delimiter;
const LINK_CLOSING: Matcher = match_link_closing_delimiter;
const LINK_TO_REFERENCE: Matcher = match_link_to_reference;
const STRONG_DELIMITER: Matcher = match_strong_delimiter;
const EMPHASIS_DELIMITER: Matcher = match_emphasis_delimiter;
const STRIKETHROUGH_DELIMITER: Matcher = match_strikethrough_delimiter;
const ESCAPED_TEXT: Matcher = match_escaped_markup_text;
const MARKUP_TEXT: Matcher = match_markup_text;
const MARKUP_TEXT_SPACE: Matcher = match_markup_text_space;
const LIST_MARKUP_TEXT_SPACE: Matcher = match_list_markup_text_space;
const TITLE_TEXT: Matcher = match_title_text;
const TITLE_TEXT_SPACE: Matcher = match_title_text_space;
const PARAMETERS_START: Matcher = match_parameters_start;
const PARAMETERS_END: Matcher = match_parameters_end;
const PARAMETER_NAME_VALUE_SEP: Matcher = match_parameter_name_value_seperator;
const PARAMETER_NAME: Matcher = match_parameter_name;
const PARAMETER_VALUE: Matcher = match_parameter_value;
const DATA_IDENTIFIER: Matcher = match_data_identifier;
const DATA_KEY_VALUE_SEP: Matcher = match_data_key_value_seperator;
const DATA_LIST_SEP: Matcher = match_data_list_seperator;
const DATA_VALUE: Matcher = match_data_value;
const CODE_DELIMITER: Matcher = match_code_delimiter;
const CODE_BLOCK: Matcher = match_code_block;

const SCAN_ELEMENT_START: &[Matcher] = &[
    CONTAINER_START,
    CONTAINER_END,
    SUBSECTION_DIRECTIVE,
    SECTION_DIRECTIVE,
    TITLE_DIRECTIVE,
    DATA_DIRECTIVE,
    CONTAINER_DIRECTIVE,
    BLOCK_DIRECTIVE,
    LIST_BULLET,
    RAW_DELIMITER,
    LINK_OPENING,
    STRONG_DELIMITER,
    EMPHASIS_DELIMITER,
    STRIKETHROUGH_DELIMITER,
    ESCAPED_TEXT,
    MARKUP_TEXT,
];

const SCAN_MARKUP: &[Matcher] = &[
    RAW_DELIMITER,
    LINK_OPENING,
    LINK_CLOSING,
    LINK_TO_REFERENCE,
    STRONG_DELIMITER,
    EMPHASIS_DELIMITER,
    STRIKETHROUGH_DELIMITER,
    ESCAPED_TEXT,
    MARKUP_TEXT_SPACE,
    MARKUP_TEXT,
];

const SCAN_LIST_MARKUP: &[Matcher] = &[
    LIST_BULLET,
    RAW_DELIMITER,
    LINK_OPENING,
    LINK_CLOSING,
    LINK_TO_REFERENCE,
    STRONG_DELIMITER,
    EMPHASIS_DELIMITER,
    STRIKETHROUGH_DELIMITER,
    ESCAPED_TEXT,
    LIST_MARKUP_TEXT_SPACE,
    MARKUP_TEXT,
];

const SCAN_RAW: &[Matcher] = &[RAW_DELIMITER, RAW_FRAGMENT];

const SCAN_TITLE: &[Matcher] = &[
    SUBSECTION_DIRECTIVE,
    SECTION_DIRECTIVE,
    TITLE_DIRECTIVE,
    TITLE_TEXT_SPACE,
    TITLE_TEXT,
];

const SCAN_HEADER: &[Matcher] = &[
    DATA_DIRECTIVE,
    CONTAINER_DIRECTIVE,
    BLOCK_DIRECTIVE,
    PARAMETERS_START,
    PARAMETERS_END,
    PARAMETER_NAME_VALUE_SEP,
    PARAMETER_VALUE,
    PARAMETER_NAME,
];

const SCAN_STRUCTURED_DATA: &[Matcher] = &[
    DATA_IDENTIFIER,
    DATA_KEY_VALUE_SEP,
    DATA_LIST_SEP,
    DATA_VALUE,
];

const SCAN_CODE: &[Matcher] = &[CODE_DELIMITER, CODE_BLOCK];
