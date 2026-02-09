use std::str::CharIndices;

const SPACE: char = ' ';
const NEW_LINE: char = '\n';
const COLON: char = ':';
const FULL_STOP: char = '.';
const DELIMITED_CONTAINER_START: &str = ">>>";
const DELIMITED_CONTAINER_END: &str = "<<<";
const GREATER_THAN: char = '>';
const LESS_THAN: char = '<';
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

#[derive(Clone, Copy, Debug)]
pub struct Position {
    pub column: u32,
    pub row: u32,
}

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

    //TODO: Bool flag is kinda smelly
    // Maybe if we get the parser telling us what tokens to expect it will
    // be somewhat easier
    //
    // Could just have to matching funcs, one for list and one for markup
    //
    // Could add an intermediate layer that just handles the mode switching?
    pub fn match_markup_text_space(&self, in_list: bool) -> Option<ScanMatch<'a>> {
        // Whatever refactor we do here, needs to be about as simple as:
        // (s @ Space | s @ Singlebreak, Text(t))
        //     if markup_space_allowed
        //         && (s == Space
        //             || (s == Singlebreak
        //                 && !t.starts_with(DELIMITED_CONTAINER_END)
        //                 && !(self.mode == List && t.starts_with(DASH)))) =>
        // {
        //     scanner.skip_while_on(SPACE);
        //     if scanner.is_on_char(NEW_LINE) {
        //         scanner.skip_char();
        //     }
        //     scanner.skip_while_on(SPACE);
        //     Some(MarkupTextSpace)
        // }
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

        if has_new_line && in_list && head.current == Some(DASH) {
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

    pub fn match_char(&self, c: char) -> Option<ScanMatch<'a>> {
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

        if head.current == Some(GREATER_THAN) {
            head.read_next_char()
        } else {
            return None;
        }

        if head.current == Some(GREATER_THAN) {
            head.read_next_char()
        } else {
            return None;
        }

        if head.current == Some(GREATER_THAN) {
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
