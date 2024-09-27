use std::fmt::Display;
use std::str::CharIndices;

//TODO: Are these semantic knowledge we dont want here...
const DELIM_BOLD: char = '*';
const DELIM_EMPH: char = '_';
const DELIM_STRIKE: char = '~';
const DELIM_RAW: char = '`';

//TODO: Support windows style newlines

#[derive(Debug)]
pub enum Peek {
    Hash,
    Text,
    Blockbreak,
    Linebreak,
    Whitespace,
    Delimiter(Delimiter),
    EndOfFile,
}

#[derive(PartialEq, Eq, Debug, Clone, Copy)]
pub enum Delimiter {
    Asterisk,
    Tilde,
    Underscore,
    Backtick,
}

// TODO: Peek only needs to look at a couple of chars to make a decision
// Therefore it does need to store a token

// TODO: The only thing that feels off about peek_
// is that it mixes in parsing level knowledge with lexing stuff
// (i.e its inherant int the name peek_markup, peek_inline_raw etc )

//TODO: Better error reporting -> what went wrong, where
//TODO: Review if we need an error here?
#[derive(PartialEq, Eq, Debug)]
pub enum ScannerError {
    UnexpectedEndOfFile,
    UnexpectedChar(char),
}

impl Display for ScannerError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ScannerError::UnexpectedChar(c) => write!(f, "unexpected char '{}'", c),
            ScannerError::UnexpectedEndOfFile => write!(f, "unexpected end of file"),
        }
    }
}

type ScannerResult<T> = Result<T, ScannerError>;

//TODO: Type alias for fn(char) -> bool

fn usable_in_word(c: char) -> bool {
    // TODO: Is there a more efficent way to do this?
    // Maybe by treating char as an int
    !(c == DELIM_BOLD
        || c == DELIM_EMPH
        || c == DELIM_STRIKE
        || c == DELIM_RAW
        || c.is_whitespace())
}

fn char_usable_in_raw(c: char) -> bool {
    // TODO: Is there a more efficent way to do this?
    // Maybe by treating char as an int
    !(c == DELIM_RAW || c == '\n')
}

//TODO: Cache the peek?
pub struct Scanner<'a> {
    input: &'a str,
    chars: CharIndices<'a>,
    current_char: Option<char>,
    current_index: usize,
    next_char: Option<char>,
    next_index: usize,
}

impl<'a> Scanner<'a> {
    pub fn new(input: &'a str) -> Self {
        let mut scanner = Self {
            input,
            chars: input.char_indices(),
            current_char: None,
            current_index: 0,
            next_char: None,
            next_index: 0,
        };

        // Place the first char of the input into `next_char`
        // Then propigate it into `current_char`
        scanner.read_next_char();
        scanner.read_next_char();

        scanner
    }

    //TODO: Have one peek function with a hint param

    pub fn peek_start_of_block(&self) -> Peek {
        match self.current_char {
            Some('#') => Peek::Hash,
            _ => self.peek(),
        }
    }

    pub fn peek_end_of_meta_line(&self) -> Peek {
        self.peek()
    }

    pub fn peek_markup(&self) -> Peek {
        self.peek()
    }

    pub fn peek_inline_raw(&self) -> Peek {
        match self.current_char {
            Some('\n') if self.next_char == Some('\n') => Peek::Blockbreak,
            //FIXME: Bit of a hack?
            Some('\n') if self.next_char.is_none() => Peek::EndOfFile,
            Some('\n') => Peek::Linebreak,
            Some(c) if c.is_whitespace() => Peek::Whitespace,
            Some('`') => Peek::Delimiter(Delimiter::Backtick),
            Some(_) => Peek::Text,
            None => Peek::EndOfFile,
        }
    }

    pub fn peek(&self) -> Peek {
        match self.current_char {
            Some('\n') if self.next_char == Some('\n') => Peek::Blockbreak,
            //FIXME: Bit of a hack?
            Some('\n') if self.next_char.is_none() => Peek::EndOfFile,
            Some('\n') => Peek::Linebreak,
            Some(c) if c.is_whitespace() => Peek::Whitespace,
            Some('*') => Peek::Delimiter(Delimiter::Asterisk),
            Some('`') => Peek::Delimiter(Delimiter::Backtick),
            Some('~') => Peek::Delimiter(Delimiter::Tilde),
            Some('_') => Peek::Delimiter(Delimiter::Underscore),
            Some(_) => Peek::Text,
            None => Peek::EndOfFile,
        }
    }

    pub fn eat_hash(&mut self) -> ScannerResult<()> {
        self.eat_char('#')
    }

    pub fn eat_colon(&mut self) -> ScannerResult<()> {
        self.eat_char(':')
    }

    pub fn eat_delimiter(&mut self, delimiter: Delimiter) -> ScannerResult<()> {
        let character = match delimiter {
            Delimiter::Underscore => '_',
            Delimiter::Asterisk => '*',
            Delimiter::Tilde => '~',
            Delimiter::Backtick => '`',
        };
        self.eat_char(character)
    }

    //pub fn has_input_remaining(&self) -> bool {
    //    return self.current_char.is_some();
    //}

    //pub fn has_no_input_remaining(&self) -> bool {
    //    return self.current_char.is_none();
    //}

    //pub fn current_char(&self) -> ScannerResult<char> {
    //    self.current_char.ok_or(ScannerError::UnexpectedEndOfFile)
    //}

    pub fn eat_linebreak(&mut self) -> ScannerResult<()> {
        self.eat_char('\n')
    }

    pub fn eat_blockbreak(&mut self) -> ScannerResult<()> {
        self.eat_char('\n')?;
        self.eat_char('\n')?;
        self.skip_chars_while_current(|c| c == '\n');
        Ok(())
    }

    /*
    pub fn eat_newlines(&mut self) -> ScannerResult<u32> {
        if let Some(c) = self.current_char {
            if c != '\n' {
                return Err(ScannerError::UnexpectedChar(c));
            }
        }

        let count = 0;
        while self.current_char.is_some_and(|c| c == '\n') {
            self.read_next_char();
            count += 1;
        }
        Ok(count);
    }
    */

    pub fn eat_identifier(&mut self) -> ScannerResult<&'a str> {
        match self.current_char {
            Some(c) if c.is_alphabetic() => Ok(self.eat_while(char::is_alphanumeric)),
            Some(c) => Err(ScannerError::UnexpectedChar(c)),
            None => Err(ScannerError::UnexpectedEndOfFile),
        }
    }

    pub fn eat_word(&mut self) -> ScannerResult<&'a str> {
        match self.current_char {
            Some(c) if usable_in_word(c) => Ok(self.eat_while(usable_in_word)),
            Some(c) => Err(ScannerError::UnexpectedChar(c)),
            None => Err(ScannerError::UnexpectedEndOfFile),
        }
    }

    //TODO: Is this too much semantic knowledge?
    pub fn eat_raw_fragment(&mut self) -> ScannerResult<&'a str> {
        Ok(self.eat_while(|c| c != '\n' && c != '`'))
    }

    pub fn eat_until_linebreak(&mut self) -> ScannerResult<&'a str> {
        //TODO: Should we throw an error if already on newline?
        // If so - drive with test for empty metadata value
        Ok(self.eat_while(|c| c != '\n'))
    }

    pub fn eat_whitespace(&mut self) {
        //FIXME: Asser there is some whitespace to start with
        self.skip_chars_while_current(char::is_whitespace)
    }

    pub fn eat_optional_whitespace(&mut self) {
        self.skip_chars_while_current(char::is_whitespace)
    }

    fn eat_char(&mut self, expected: char) -> ScannerResult<()> {
        match self.current_char {
            Some(c) if c == expected => {
                self.read_next_char();
                Ok(())
            }
            Some(c) => Err(ScannerError::UnexpectedChar(c)),
            None => Err(ScannerError::UnexpectedEndOfFile),
        }
    }

    //FIXME: Strictly we should return an error if we hit EOF
    // Otherwise if we call eat_mango, we dont know if we actually got a mango
    fn eat_while(&mut self, predicate: fn(char) -> bool) -> &'a str {
        let i1 = self.current_index;
        self.skip_chars_while_current(predicate);
        let i2 = self.current_index;
        &self.input[i1..i2]
    }

    fn skip_chars_while_current(&mut self, predicate: fn(char) -> bool) {
        while self.current_char.is_some_and(predicate) {
            self.read_next_char();
        }
    }

    fn read_next_char(&mut self) {
        self.current_char = self.next_char;
        self.current_index = self.next_index;

        if let Some((i, c)) = self.chars.next() {
            self.next_char = Some(c);
            self.next_index = i;
        } else {
            self.next_char = None;
            self.next_index = self.input.len();
        }
    }

    /*
       pub fn advance_block_start(&mut self) {
           self.current_token = match self.current_char {
               Some('#') => self.handle_block_type(),
               // Implicit default pseudo-token of paragraph
               // TODO: Do we actually like pseudo tokens?
               Some(_) => Token::BlockStartParagraph,
               None => Token::Eof,
           }
       }

       pub fn advance_metadata(&mut self, hint) {
           self.current_token = match self.current_char {
               Some('\n') => self.handle_newline(),
               Some(c) if c.is_whitespace() => self.handle_whitespace(),
               Some(_) => self.handle_data(),
               None => Token::Eof,
           }
       }
       /*
       pub fn expect_data_key(&mut self) -> TokeniserResult<&'a str> {
           //FIXME: If the following was the public interface... then we dont need the ctx module?
           self.assert_current_char_is(char::is_alphabetic)?;
           let text = self.eat_chars_while(char::is_alphabetic);
           self.eat_whitespace();
           self.eat_char(':')?;
           self.eat_whitespace();
           Ok(text)
       }
       */

       //TODO: If we lean into 'hints' then could use for raw also?
       pub fn advance_paragraph(&mut self) {
           self.current_token = match self.current_char {
               Some('\n') => self.handle_newline(),
               Some(DELIM_BOLD) => self.handle_format_delimiter(Format::Bold),
               Some(DELIM_EMPH) => self.handle_format_delimiter(Format::Emphasis),
               Some(DELIM_STRIKE) => self.handle_format_delimiter(Format::Strikethrough),
               Some(DELIM_RAW) => self.handle_inline_raw_delimiter(),
               Some(c) if c.is_whitespace() => self.handle_whitespace(),
               Some(_) => self.handle_text(),
               None => Token::Eof,
           }
       }

       pub fn advance_inline_raw(&mut self) {
           //TODO: Raw should probably have its own token set
           self.current_token = match self.current_char {
               Some('\n') => self.handle_raw_newline(),
               Some(DELIM_RAW) => self.handle_inline_raw_delimiter(),
               Some(_) => self.handle_text_raw(),
               None => Token::Eof,
           }
       }

       /*
       pub fn assert_at_meteadata_block_start(&self) -> TokeniserResult<()> {
           if self.current_token != Token::BlockStart(BlockType::Metadata) {
               Err(TokeniserError::UnexpectedToken)
           } else {
               Ok(())
           }
       }

       pub fn expect_linebreak(&mut self) -> TokeniserResult<()> {
           if self.current_char == Some('\n') {
               self.read_next_char();
               Ok(())
           } else {
               Err(TokeniserError::UnexpectedChar)
           }
       }


       pub fn expect_data_value(&mut self) -> TokeniserResult<&'a str> {
           //TODO: Normalise the value whitespace?
           self.assert_current_char_is(|c| c != '\n')?;
           let text = self.eat_chars_while(|c| c != '\n');
           Ok(text)
       }

       pub fn expect_end_of_key_value(&mut self) -> TokeniserResult<ctx::EndOfKV> {
           match self.current_char {
               Some('\n') => {
                   self.read_next_char();
                   if self.current_char == Some('\n') {
                       self.advance_while_current(|c| c == '\n');
                       Ok(ctx::EndOfKV::Blockbreak)
                   } else if self.current_char == None {
                       Ok(ctx::EndOfKV::Eof)
                   } else {
                       Ok(ctx::EndOfKV::Linebreak)
                   }
               }
               None => Ok(ctx::EndOfKV::Eof),
               _ => Err(TokeniserError::UnexpectedChar),
           }
       }
       */


       fn handle_block_type(&mut self) -> Token<'a> {
           self.read_next_char(); // Skip over #
           let text = self.eat_chars_while(|c| c.is_alphabetic());
           match text {
               "metadata" => Token::BlockStart(BlockType::Metadata),
               "paragraph" => Token::BlockStart(BlockType::Paragraph),
               _ => panic!(), //FIXME: remove panic
           }
       }

       fn handle_raw_newline(&mut self) -> Token<'a> {
           self.read_next_char();
           Token::Whitespace
           // FIXME: A second newline should not be valid inside a raw string
       }

       fn handle_paragraph_newline(&mut self) -> Token<'a> {
           self.read_next_char();

           if self.current_char == Some('\n') {
               self.handle_blockbreak()
           } else {
               Token::Whitespace
           }
       }

       fn handle_metadata_newline(&mut self) -> Token<'a> {
           self.read_next_char();

           if self.current_char == Some('\n') {
               self.handle_blockbreak()
           } else {
    let Token::MetadataValue(value) = tokeniser.current_token {
               value
           } else {
               return Err(ParseError::UnexpectedToken);
           };

           match key {
               "id" => metadata.id.push_str(value),
               "title" => metadata.title.push_str(value),
               _ => return Err(ParseError::UnknownMetadata),
           };

           tokeniser.advance_metadata();

    let Token::MetadataValue(value) = tokeniser.current_token {
               value
           } else {
               return Err(ParseError::UnexpectedToken);
           };

           match key {
               "id" => metadata.id.push_str(value),
               "title" => metadata.title.push_str(value),
               _ => return Err(ParseError::UnknownMetadata),
           };

           tokeniser.advance_metadata();

               Token::MetadataLinebreak
           }
       }

       fn handle_blockbreak(&mut self) -> Token<'a> {
           // New lines are usually treated as whitespace. However, if there is
           // more than one in a row, they are all treated as a block break
           //TODO: add special case advance fun for current eq
           self.skip_chars_while_current(|c| c == '\n');
           Token::Blockbreak
       }

       fn handle_whitespace(&mut self) -> Token<'a> {
           self.skip_whitespace();
           Token::Whitespace
       }

       fn handle_format_delimiter(&mut self, kind: Format) -> Token<'a> {
           self.read_next_char();
           Token::FormatDelimiter(kind)
       }

       fn handle_inline_raw_delimiter(&mut self) -> Token<'a> {
           self.read_next_char();
           Token::RawDelimiter
       }

       // TODO: char_usable_in_text and char_usable_in_raw probably wont work
       // once we start allowing for escaped characters such as \*
       fn handle_text(&mut self) -> Token<'a> {
           let text = self.eat_chars_while(char_usable_in_text);value),

           Token::Text(text)
       }

       fn handle_text_raw(&mut self) -> Token<'a> {
           let text = self.eat_chars_while(char_usable_in_raw);
           Token::Text(text)
       }

       fn assert_current_char_is(&self, predicate: fn(char) -> bool) -> TokeniserResult<()> {
           if self.current_char.is_some_and(predicate) {
               Ok(())
           } else {
               Err(TokeniserError::UnexpectedChar)
           }
       }
       */
}
