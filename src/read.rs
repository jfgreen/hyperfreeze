//TODO: Do we really need this module?
use std::{collections::VecDeque, iter::Peekable, str::CharIndices};

#[derive(PartialEq, Eq, Debug, Clone, Copy)]
struct ReadPosition {
    char: char,
    index: usize,
    column: usize,
    row: usize,
}

#[derive(Debug)]
enum ReaderState {
    StartOfInput,
    Replaying(usize),
    Reading,
    EndOfInput,
}

struct Reader<'a> {
    input: &'a str,
    chars: Peekable<CharIndices<'a>>,
    buffer: VecDeque<ReadPosition>,
    index: usize,
}

impl<'a> Reader<'a> {
    fn new(input: &'a str) -> Self {
        Self {
            input,
            chars: input.char_indices().peekable(),
            buffer: VecDeque::new(),
            index: 0,
        }
    }

    fn advance(&mut self) {
        let last_index = self.buffer.len() - 1;
        let is_more_input = self.chars.peek().is_some();

        if self.buffer.is_empty() && is_more_input {
            // At the start
            self.read_next();
        } else if self.index == last_index && is_more_input {
            // At the end of the buffer, more input
            self.read_next();
            self.index += 1;
        } else if self.index < last_index {
            // Replaying
            self.index += 1;
        } else if !is_more_input {
            // At end of buffer, no more input
            self.index = self.buffer.len();
        }
    }

    fn commit(&mut self) {
        self.buffer.drain(0..self.index);
        self.index = 0;
    }

    fn rewind(&mut self) {
        self.index = 0;
    }

    fn current_char(&self) -> Option<char> {
        if self.index < self.buffer.len() {
            Some(self.buffer[self.index].char)
        } else {
            None
        }
    }

    fn read_next(&mut self) {
        let (cur_row, cur_col) = self
            .buffer
            .back()
            .map_or((0, 0), |pos| (pos.row, pos.column));

        if let Some((i, c)) = self.chars.next() {
            let (next_col, next_row) = match c {
                '\n' => (0, cur_row + 1),
                _ => (cur_row, cur_col + 1),
            };

            let next_pos = ReadPosition {
                char: c,
                index: i,
                column: next_col,
                row: next_row,
            };
            self.buffer.push_back(next_pos);
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;

    struct TestHelper<'a> {
        reader: Reader<'a>,
        actual_chars: Vec<Option<char>>,
    }

    impl<'a> TestHelper<'a> {
        fn advancing(mut self, count: usize) -> Self {
            for _ in 0..count {
                self.reader.advance();
                self.actual_chars.push(self.reader.current_char());
            }
            self
        }

        fn commiting(mut self) -> Self {
            self.reader.commit();
            self
        }

        fn rewinding(mut self) -> Self {
            self.reader.rewind();
            self.actual_chars.push(self.reader.current_char());
            self
        }

        fn should_return_chars(self, expected_chars: Vec<Option<char>>) {
            assert_eq!(self.actual_chars, expected_chars);
        }
    }

    fn given(input: &str) -> TestHelper {
        TestHelper {
            reader: Reader::new(input),
            actual_chars: Vec::new(),
        }
    }

    #[test]
    fn simple_advance_returns_chars() {
        given("ab")
            .advancing(3)
            .should_return_chars(vec![Some('a'), Some('b'), None]);
    }

    #[test]
    fn commit_and_rewind() {
        given("abcd")
            .advancing(2)
            .commiting()
            .advancing(2)
            .rewinding()
            .advancing(3)
            .should_return_chars(vec![
                Some('a'),
                Some('b'),
                Some('c'),
                Some('d'),
                Some('b'),
                Some('c'),
                Some('d'),
                None,
            ]);
    }

    #[test]
    fn successive_commit_and_rewind() {
        given("abcd")
            .advancing(1)
            .commiting()
            .advancing(1)
            .rewinding()
            .advancing(2)
            .commiting()
            .advancing(1)
            .rewinding()
            .advancing(2)
            .should_return_chars(vec![
                Some('a'),
                Some('b'),
                Some('a'),
                Some('b'),
                Some('c'),
                Some('d'),
                Some('c'),
                Some('d'),
                None,
            ]);
    }

    #[test]
    fn rewind_without_commit() {
        given("abc")
            .advancing(2)
            .rewinding()
            .advancing(3)
            .should_return_chars(vec![
                Some('a'),
                Some('b'),
                Some('a'),
                Some('b'),
                Some('c'),
                None,
            ]);
    }

    #[test]
    fn rewind_without_advancing() {
        given("a")
            .rewinding()
            .advancing(2)
            .should_return_chars(vec![None, Some('a'), None]);
    }

    #[test]
    fn rewind_after_commit() {
        given("ab")
            .advancing(1)
            .commiting()
            .rewinding()
            .advancing(2)
            .should_return_chars(vec![Some('a'), Some('a'), Some('b'), None]);
    }

    #[test]
    fn commit_and_rewind_end_of_input() {
        given("ab")
            .advancing(3)
            .commiting()
            .rewinding()
            .should_return_chars(vec![Some('a'), Some('b'), None, None]);
    }

    #[test]
    fn commiting_mid_replay() {
        given("abcde")
            .advancing(2)
            .commiting()
            .advancing(2)
            .rewinding()
            .advancing(1)
            .commiting()
            .advancing(3)
            .should_return_chars(vec![
                Some('a'),
                Some('b'),
                Some('c'),
                Some('d'),
                Some('b'),
                Some('c'),
                Some('d'),
                Some('e'),
                None,
            ])
    }

    #[test]
    fn rewind_to_same_commit_twice() {
        given("abc")
            .advancing(2)
            .commiting()
            .advancing(1)
            .rewinding()
            .advancing(1)
            .rewinding()
            .advancing(2)
            .should_return_chars(vec![
                Some('a'),
                Some('b'),
                Some('c'),
                Some('b'),
                Some('c'),
                Some('b'),
                Some('c'),
                None,
            ])
    }

    #[test]
    fn rewind_after_end() {
        given("ab")
            .advancing(1)
            .commiting()
            .advancing(2)
            .rewinding()
            .advancing(2)
            .rewinding()
            .should_return_chars(vec![
                Some('a'),
                Some('b'),
                None,
                Some('a'),
                Some('b'),
                None,
                Some('a'),
            ])
    }

    #[test]
    fn commiting_and_rewinding_mid_replay() {
        given("abcde")
            .advancing(1)
            .commiting()
            .advancing(3)
            .rewinding()
            .advancing(2)
            .commiting()
            .advancing(2)
            .rewinding()
            .advancing(3)
            .should_return_chars(vec![
                Some('a'),
                Some('b'),
                Some('c'),
                Some('d'),
                Some('a'),
                Some('b'),
                Some('c'),
                Some('d'),
                Some('e'),
                Some('c'),
                Some('d'),
                Some('e'),
                None,
            ]);
    }
}
