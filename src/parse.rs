use std::backtrace::{Backtrace, BacktraceStatus};
use std::fmt::{self, Display};

use crate::document::{self as doc, Document};

// TODO: Can we solve this lint with a prelude?
#[allow(clippy::wildcard_imports)]
use crate::tokenise::*;

#[derive(Debug)]
pub struct ParseError {
    kind: ErrorKind,
    token: TokenDescription,
    backtrace: Backtrace,
}

#[derive(PartialEq, Eq, Debug)]
enum ErrorKind {
    LooseDelimiter,
    MissingListLevel { from: usize, to: usize },
    MetadataNotAtStart,
    ReferencesOutOfPlace,
    ExpectedToken(TokenName),
    UnknownMetadata(LexemeString),
    InvalidListStyle(LexemeString),
    InvalidListParameter(LexemeString),
    UnevenListIndent(Indent),
    UnknownDirective(LexemeString),
    ContainerMissingStart,
    EmptyContainer,
    UnexpectedBlockStart,
    UnexpectedTextRunStart,
    UnexpectedRawTextRunStart,
    UnexpectedHeaderTextStart,
    SubSectionNotNested,
}

impl Display for ParseError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.write_token(f)?;
        self.write_message(f)?;
        self.write_backtrace(f)?;
        Ok(())
    }
}

impl ParseError {
    fn write_token(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        let position = self.token.position;
        writeln!(
            f,
            "parsing error on line {} column {} at {} token '{}'",
            position.row + 1,
            position.column + 1,
            self.token.name,
            self.token.lexeme,
        )?;

        Ok(())
    }

    fn write_message(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        match &self.kind {
            ErrorKind::LooseDelimiter => {
                write!(f, "delimited text cant have leading/trailing whitespace")
            }

            ErrorKind::UnknownMetadata(key) => {
                write!(f, "unknown metadata '{key}'")
            }

            ErrorKind::MissingListLevel { from, to } => {
                write!(f, "list indent skipped from {from} to {to}")
            }

            ErrorKind::MetadataNotAtStart => {
                write!(f, "document metadata is not at start of document")
            }

            ErrorKind::ReferencesOutOfPlace => {
                write!(f, "references not at start of document")
            }

            ErrorKind::ExpectedToken(expected) => {
                write!(f, "unexpected token, expected: {expected}")
            }

            ErrorKind::UnevenListIndent(indent) => {
                write!(
                    f,
                    "list indent of {} spaces is not even",
                    indent.space_count
                )
            }

            ErrorKind::UnknownDirective(name) => {
                write!(f, "unknown directive '{name}'")
            }

            ErrorKind::InvalidListStyle(style) => {
                write!(f, "invalid list style '{style}'")
            }

            ErrorKind::InvalidListParameter(parameter) => {
                write!(f, "invalid list style '{parameter}'")
            }

            ErrorKind::ContainerMissingStart => {
                write!(f, "delimited container end with no preceeding start")
            }

            ErrorKind::EmptyContainer => {
                write!(f, "empty container")
            }

            ErrorKind::UnexpectedBlockStart => {
                write!(f, "expected start of block")
            }

            ErrorKind::UnexpectedTextRunStart => {
                write!(f, "expected start of text")
            }

            ErrorKind::UnexpectedRawTextRunStart => {
                write!(f, "expected start of raw")
            }

            ErrorKind::UnexpectedHeaderTextStart => {
                write!(f, "expected start of header")
            }

            ErrorKind::SubSectionNotNested => {
                write!(f, "subsection not inside an enclosing section")
            }
        }
    }

    fn write_backtrace(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        if self.backtrace.status() == BacktraceStatus::Captured {
            writeln!(f)?;
            writeln!(f, "Parse backtrace:")?;
            writeln!(f, "{}", self.backtrace)?;
        }
        Ok(())
    }
}

impl From<UnexpectedTokenError> for ParseError {
    fn from(err: UnexpectedTokenError) -> Self {
        ParseError {
            kind: ErrorKind::ExpectedToken(err.expected),
            token: err.actual,
            backtrace: Backtrace::capture(),
        }
    }
}

macro_rules! parse_err {
    ($error:expr, $token:expr) => {{
        Err(ParseError {
            kind: $error,
            backtrace: Backtrace::capture(),
            token: $token.description(),
        })
    }};
}

type ParseResult<T> = Result<T, ParseError>;

const SPACE: char = ' ';

fn is_markup(token: Token) -> bool {
    matches!(
        token,
        Token::MarkupTextSpace
            | Token::LinkOpeningDelimiter
            | Token::RawDelimiter
            | Token::EmphasisDelimiter
            | Token::StrongDelimiter
            | Token::StrikethroughDelimiter
            | Token::MarkupText(_)
    )
}

pub fn parse_str(input: &str) -> Result<Document, ParseError> {
    let tokeniser = &mut Tokeniser::new(input);
    parse_document(tokeniser)
}

fn parse_document(tokeniser: &mut Tokeniser) -> ParseResult<Document> {
    let mut metadata = doc::Metadata::default();
    let mut references = Vec::new();
    let mut elements = Vec::new();

    tokeniser.push_mode(ScanMode::BlockStart);

    if tokeniser.peek().is::<MetadataDirective>() {
        metadata = parse_metadata(tokeniser)?;
    }

    if tokeniser.peek().is::<ReferencesDirective>() {
        let refs = parse_references(tokeniser)?;
        references.extend(refs);
    }

    let title = parse_document_title(tokeniser)?;

    while !tokeniser.peek().is::<EndOfInput>() {
        let element = parse_element(tokeniser)?;
        elements.push(element);
    }

    tokeniser.pop_mode();

    Ok(Document {
        title,
        metadata,
        references: references.into_boxed_slice(),
        contents: elements.into_boxed_slice(),
    })
}

fn parse_document_title(tokeniser: &mut Tokeniser) -> ParseResult<String> {
    tokeniser.advance().require::<TitleDirective>()?;

    let title = parse_header_text(tokeniser)?;

    if !tokeniser.peek().is::<EndOfInput>() {
        tokeniser.advance().require::<BlockBreak>()?;
    }

    Ok(title)
}

fn parse_metadata(tokeniser: &mut Tokeniser) -> ParseResult<doc::Metadata> {
    let mut metadata = doc::Metadata::default();

    tokeniser.advance().require::<MetadataDirective>()?;
    tokeniser.advance().require::<LineBreak>()?;

    tokeniser.push_mode(ScanMode::StructuredData);

    while let Some(identifier_token) = tokeniser.peek().try_consume() {
        let DataIdentifier(key) = identifier_token.value;
        tokeniser.advance();
        tokeniser.advance().require::<DataKeyValueSeperator>()?;

        match key {
            "id" => {
                let id_token = tokeniser.advance().require()?;
                let DataValue(id) = id_token.value;

                metadata.id = Some(id.to_string());
            }
            "tags" => {
                let tags = parse_metadata_list(tokeniser)?;
                metadata.tags = Some(tags);
            }
            _ => {
                return parse_err!(
                    ErrorKind::UnknownMetadata(identifier_token.lexeme_to_owned()),
                    identifier_token
                );
            }
        }

        if tokeniser.peek().is::<LineBreak>() {
            tokeniser.advance();
        }
    }

    tokeniser.pop_mode();

    if !tokeniser.peek().is::<EndOfInput>() {
        tokeniser.advance().require::<BlockBreak>()?;
    }

    Ok(metadata)
}

fn parse_references(tokeniser: &mut Tokeniser) -> ParseResult<Box<[doc::Reference]>> {
    tokeniser.advance().require::<ReferencesDirective>()?;
    tokeniser.advance().require::<LineBreak>()?;

    tokeniser.push_mode(ScanMode::StructuredData);

    let mut references = Vec::new();

    while let Some(id_token) = tokeniser.peek().try_consume() {
        let DataIdentifier(id) = id_token.value;

        tokeniser.advance();
        tokeniser.advance().require::<DataKeyValueSeperator>()?;

        let link_token = tokeniser.advance().require()?;
        let DataValue(link) = link_token.value;

        if tokeniser.peek().is::<LineBreak>() {
            tokeniser.advance();
        }

        let reference = doc::Reference {
            id: id.into(),
            link: link.into(),
        };

        references.push(reference);
    }

    tokeniser.pop_mode();

    if !tokeniser.peek().is::<EndOfInput>() {
        tokeniser.advance().require::<BlockBreak>()?;
    }

    let references = references.into_boxed_slice();
    Ok(references)
}

fn parse_header_text(tokeniser: &mut Tokeniser) -> ParseResult<String> {
    let mut title = String::new();

    tokeniser.push_mode(ScanMode::Title);

    let next = tokeniser.peek();

    if !matches!(next.value, Token::TitleTextSpace | Token::TitleText(_)) {
        return parse_err!(ErrorKind::UnexpectedHeaderTextStart, next);
    }

    loop {
        let next = tokeniser.peek();
        match next.value {
            Token::TitleText(text) => {
                title.push_str(text);
                tokeniser.advance();
            }
            Token::TitleTextSpace => {
                title.push(SPACE);
                tokeniser.advance();
            }
            _ => break,
        }
    }

    tokeniser.pop_mode();

    Ok(title)
}

fn parse_metadata_list(tokeniser: &mut Tokeniser) -> ParseResult<Box<[String]>> {
    let mut tags = Vec::new();

    let first_item_token = tokeniser.advance().require()?;
    let DataValue(first_item) = first_item_token.value;

    tags.push(first_item.to_string());

    while tokeniser.peek().is::<DataListSeperator>() {
        tokeniser.advance();
        let item_token = tokeniser.advance().require()?;
        let DataValue(item) = item_token.value;
        tags.push(item.to_string());
    }

    let tags = tags.into_boxed_slice();
    Ok(tags)
}

fn parse_element(tokeniser: &mut Tokeniser) -> ParseResult<doc::Element> {
    let next = tokeniser.peek();
    match next.value {
        Token::SectionDirective => {
            let section = parse_section(tokeniser)?;
            Ok(doc::Element::Section(section))
        }
        Token::InfoContainerDirective => {
            let container = parse_container(tokeniser)?;
            Ok(doc::Element::Container(container))
        }
        Token::SubSectionDirective => {
            parse_err!(ErrorKind::SubSectionNotNested, next)
        }
        _ => {
            let block = parse_block(tokeniser)?;
            Ok(doc::Element::Block(block))
        }
    }
}

fn parse_container(tokeniser: &mut Tokeniser) -> ParseResult<doc::Container> {
    tokeniser.advance().require::<InfoContainerDirective>()?;

    let next = tokeniser.peek();
    if next.is::<BlockBreak>() {
        return parse_err!(ErrorKind::EmptyContainer, next);
    }

    tokeniser.advance().require::<LineBreak>()?;

    let next = tokeniser.peek();
    if next.is::<EndOfInput>() {
        return parse_err!(ErrorKind::EmptyContainer, next);
    }

    let container_kind = doc::ContainerKind::Info;

    let mut blocks = Vec::new();

    if tokeniser.peek().is::<DelimitedContainerStart>() {
        tokeniser.advance();

        tokeniser.advance().require::<LineBreak>()?;

        let next = tokeniser.peek();
        if next.is::<DelimitedContainerEnd>() {
            return parse_err!(ErrorKind::EmptyContainer, next);
        }

        while !tokeniser.peek().is::<DelimitedContainerEnd>() {
            let block = parse_block(tokeniser)?;
            blocks.push(block);
        }

        tokeniser.advance().require::<DelimitedContainerEnd>()?;

        if !tokeniser.peek().is::<EndOfInput>() {
            tokeniser.advance().require::<BlockBreak>()?;
        }
    } else {
        let block = parse_block(tokeniser)?;
        blocks.push(block);
    }

    let container = doc::Container {
        content: blocks.into_boxed_slice(),
        kind: container_kind,
    };

    Ok(container)
}

fn parse_section(tokeniser: &mut Tokeniser) -> ParseResult<doc::Section> {
    tokeniser.advance().require::<SectionDirective>()?;

    let name = parse_header_text(tokeniser)?;

    tokeniser.advance().require::<BlockBreak>()?;

    let mut elements = Vec::new();

    loop {
        let next = tokeniser.peek();
        if next.is::<EndOfInput>() || next.is::<SectionDirective>() {
            break;
        }
        let element = parse_section_element(tokeniser)?;
        elements.push(element);
    }

    let section = doc::Section {
        content: elements.into_boxed_slice(),
        heading: name,
    };

    Ok(section)
}

fn parse_section_element(tokeniser: &mut Tokeniser) -> ParseResult<doc::SectionElement> {
    let peeked = tokeniser.peek();
    match peeked.value {
        Token::SubSectionDirective => {
            let subsection = parse_subsection(tokeniser)?;
            Ok(doc::SectionElement::SubSection(subsection))
        }
        Token::InfoContainerDirective => {
            let container = parse_container(tokeniser)?;
            Ok(doc::SectionElement::Container(container))
        }
        _ => {
            let block = parse_block(tokeniser)?;
            Ok(doc::SectionElement::Block(block))
        }
    }
}

fn parse_subsection(tokeniser: &mut Tokeniser) -> ParseResult<doc::SubSection> {
    tokeniser.advance().require::<SubSectionDirective>()?;

    let name = parse_header_text(tokeniser)?;

    tokeniser.advance().require::<BlockBreak>()?;

    let mut elements = Vec::new();

    loop {
        let next = tokeniser.peek();
        if next.is::<EndOfInput>()
            || next.is::<SectionDirective>()
            || next.is::<SubSectionDirective>()
        {
            break;
        }
        let element = parse_subsection_element(tokeniser)?;
        elements.push(element);
    }

    let subsection = doc::SubSection {
        content: elements.into_boxed_slice(),
        heading: name,
    };

    Ok(subsection)
}

fn parse_subsection_element(tokeniser: &mut Tokeniser) -> ParseResult<doc::SubSectionElement> {
    let next = tokeniser.peek();
    if let Token::InfoContainerDirective = next.value {
        let container = parse_container(tokeniser)?;
        Ok(doc::SubSectionElement::Container(container))
    } else {
        let block = parse_block(tokeniser)?;
        Ok(doc::SubSectionElement::Block(block))
    }
}

fn parse_block(tokeniser: &mut Tokeniser) -> ParseResult<doc::Block> {
    let next = tokeniser.peek();
    let block = match next.value {
        Token::ListBullet(_) | Token::ListDirective => parse_list(tokeniser)?,
        Token::ParagraphDirective => parse_paragraph(tokeniser)?,
        Token::CodeDirective => parse_code(tokeniser)?,
        Token::MetadataDirective => {
            return parse_err!(ErrorKind::MetadataNotAtStart, next);
        }
        Token::ReferencesDirective => {
            return parse_err!(ErrorKind::ReferencesOutOfPlace, next);
        }
        Token::UnknownDirective(_) => {
            let err = ErrorKind::UnknownDirective(next.lexeme_to_owned());
            return parse_err!(err, next);
        }
        Token::DelimitedContainerEnd => {
            return parse_err!(ErrorKind::ContainerMissingStart, next);
        }
        t if is_markup(t) => parse_paragraph(tokeniser)?,
        _ => {
            return parse_err!(ErrorKind::UnexpectedBlockStart, next);
        }
    };

    let next = tokeniser.peek();
    if next.is::<LineBreak>() {
        tokeniser.advance();
    } else if !(next.is::<EndOfInput>() || next.is::<DelimitedContainerEnd>()) {
        tokeniser.advance().require::<BlockBreak>()?;
    }

    Ok(block)
}

fn parse_paragraph(tokeniser: &mut Tokeniser) -> ParseResult<doc::Block> {
    if tokeniser.peek().is::<ParagraphDirective>() {
        tokeniser.advance();
        tokeniser.advance().require::<LineBreak>()?;
    }

    tokeniser.push_mode(ScanMode::Markup);

    if tokeniser.peek().is::<MarkupTextSpace>() {
        tokeniser.advance();
    }

    let text_runs = parse_text_runs(tokeniser)?;
    tokeniser.pop_mode();
    Ok(doc::Block::Paragraph(text_runs))
}

fn parse_list_level(
    tokeniser: &mut Tokeniser,
    current_depth: usize,
) -> ParseResult<Box<[doc::ListItem]>> {
    let mut items = Vec::new();

    while let Some(bullet_token) = tokeniser.peek().try_consume() {
        let ListBullet(indent) = bullet_token.value;
        let space_count = indent.space_count;

        if space_count % 2 != 0 {
            return parse_err!(ErrorKind::UnevenListIndent(indent), bullet_token);
        }

        let depth = space_count / 2;

        let item = if depth == current_depth {
            tokeniser.advance();
            let text = parse_text_runs(tokeniser)?;
            if tokeniser.peek().is::<LineBreak>() {
                tokeniser.advance();
            }
            doc::ListItem::Text(text)
        } else if depth == current_depth + 1 {
            let sub_items = parse_list_level(tokeniser, depth)?;
            doc::ListItem::SubList(sub_items)
        } else if depth < current_depth {
            break;
        } else {
            return parse_err!(
                ErrorKind::MissingListLevel {
                    from: current_depth,
                    to: depth
                },
                bullet_token
            );
        };

        items.push(item);
    }

    let items = items.into_boxed_slice();
    Ok(items)
}

//TODO: At some point we need common mechanics for iterating block parameters
fn parse_list(tokeniser: &mut Tokeniser) -> ParseResult<doc::Block> {
    let mut style = doc::ListStyle::Unordered;

    if tokeniser.peek().is::<ListDirective>() {
        tokeniser.advance();

        tokeniser.push_mode(ScanMode::Header);

        if tokeniser.peek().is::<BlockParametersStart>() {
            tokeniser.advance();

            if let Some(name_token) = tokeniser.peek().try_consume() {
                let BlockParameterName(name) = name_token.value;
                tokeniser.advance();

                tokeniser
                    .advance()
                    .require::<BlockParameterNameValueSeperator>()?;

                tokeniser.push_mode(ScanMode::HeaderValue);
                let param_value_token = tokeniser.advance().require()?;
                let BlockParameterValue(value) = param_value_token.value;

                tokeniser.pop_mode();

                match name {
                    "style" => {
                        style = match value {
                            "ordered" => doc::ListStyle::Ordered,
                            "unordered" => doc::ListStyle::Unordered,
                            _ => {
                                return parse_err!(
                                    ErrorKind::InvalidListStyle(
                                        param_value_token.lexeme_to_owned()
                                    ),
                                    param_value_token
                                );
                            }
                        }
                    }
                    _ => {
                        return parse_err!(
                            ErrorKind::InvalidListParameter(name_token.lexeme_to_owned()),
                            name_token
                        );
                    }
                }
            }

            tokeniser.advance().require::<BlockParametersEnd>()?;
        }

        tokeniser.pop_mode();

        tokeniser.advance().require::<LineBreak>()?;
    }

    tokeniser.push_mode(ScanMode::ListMarkup);
    let base_depth = 0;
    let items = parse_list_level(tokeniser, base_depth)?;
    tokeniser.pop_mode();

    let list = doc::List { items, style };
    let block = doc::Block::List(list);
    Ok(block)
}

fn parse_text_runs(tokeniser: &mut Tokeniser) -> ParseResult<Box<[doc::TextRun]>> {
    let mut text_runs = Vec::new();

    loop {
        let next = tokeniser.peek();
        let run = match next.value {
            Token::MarkupText(_) | Token::MarkupTextSpace => parse_plain_text_run(tokeniser)?,
            Token::StrikethroughDelimiter => parse_strikethrough_text_run(tokeniser)?,
            Token::EmphasisDelimiter => parse_emphasised_text_run(tokeniser)?,
            Token::StrongDelimiter => parse_strong_text_run(tokeniser)?,
            Token::RawDelimiter => parse_raw_text_run(tokeniser)?,
            Token::LinkOpeningDelimiter => parse_linked_text_run(tokeniser)?,
            _ => break,
        };
        text_runs.push(run);
    }

    let text_runs = text_runs.into_boxed_slice();
    Ok(text_runs)
}

fn parse_plain_text_run(tokeniser: &mut Tokeniser) -> ParseResult<doc::TextRun> {
    let run = parse_markup_text(tokeniser)?;

    let run = doc::TextRun {
        text: run,
        style: doc::Style::None,
    };

    Ok(run)
}

fn parse_raw_text_run(tokeniser: &mut Tokeniser) -> ParseResult<doc::TextRun> {
    tokeniser.advance().require::<RawDelimiter>()?;

    tokeniser.push_mode(ScanMode::Raw);
    let next = tokeniser.peek();

    if !matches!(next.value, Token::RawFragment(_) | Token::LineBreak) {
        return parse_err!(ErrorKind::UnexpectedRawTextRunStart, next);
    }

    let mut run = String::new();

    loop {
        let next = tokeniser.peek();
        if let Some(fragment_token) = next.try_consume() {
            let RawFragment(fragment) = fragment_token.value;
            run.push_str(fragment);
            tokeniser.advance();
        } else if next.is::<LineBreak>() {
            tokeniser.advance();
            run.push(SPACE);
        } else {
            break;
        }
    }

    tokeniser.advance().require::<RawDelimiter>()?;

    tokeniser.pop_mode();

    let run = doc::TextRun {
        text: run,
        style: doc::Style::Raw,
    };

    Ok(run)
}

fn parse_linked_text_run(tokeniser: &mut Tokeniser) -> ParseResult<doc::TextRun> {
    let next = tokeniser.advance().require::<LinkOpeningDelimiter>()?;

    let run = parse_markup_text(tokeniser)?;

    if run.starts_with(SPACE) || run.ends_with(SPACE) {
        return parse_err!(ErrorKind::LooseDelimiter, next);
    }

    tokeniser.advance().require::<LinkClosingDelimiter>()?;

    tokeniser.push_mode(ScanMode::LinkReference);

    tokeniser.advance().require::<LinkToReferenceJoiner>()?;

    let identifier_token = tokeniser.advance().require()?;
    let DataIdentifier(identifier) = identifier_token.value;

    tokeniser.pop_mode();

    Ok(doc::TextRun {
        text: run,
        style: doc::Style::Link(identifier.into()),
    })
}

fn parse_strong_text_run(tokeniser: &mut Tokeniser) -> ParseResult<doc::TextRun> {
    let run_start = tokeniser.peek();

    tokeniser.advance().require::<StrongDelimiter>()?;

    let run = parse_markup_text(tokeniser)?;

    tokeniser.advance().require::<StrongDelimiter>()?;

    if let Err(error) = validate_styled_text_run(&run) {
        return parse_err!(error, run_start);
    }

    let run = doc::TextRun {
        text: run,
        style: doc::Style::Strong,
    };

    Ok(run)
}

fn parse_emphasised_text_run(tokeniser: &mut Tokeniser) -> ParseResult<doc::TextRun> {
    let run_start = tokeniser.peek();

    tokeniser.advance().require::<EmphasisDelimiter>()?;

    let run = parse_markup_text(tokeniser)?;

    tokeniser.advance().require::<EmphasisDelimiter>()?;

    if let Err(error) = validate_styled_text_run(&run) {
        return parse_err!(error, run_start);
    }

    let run = doc::TextRun {
        text: run,
        style: doc::Style::Emphasis,
    };

    Ok(run)
}

fn parse_strikethrough_text_run(tokeniser: &mut Tokeniser) -> ParseResult<doc::TextRun> {
    let run_start = tokeniser.peek();

    tokeniser.advance().require::<StrikethroughDelimiter>()?;

    let run = parse_markup_text(tokeniser)?;

    tokeniser.advance().require::<StrikethroughDelimiter>()?;

    if let Err(error) = validate_styled_text_run(&run) {
        return parse_err!(error, run_start);
    }

    let run = doc::TextRun {
        text: run,
        style: doc::Style::Strikethrough,
    };

    Ok(run)
}

fn validate_styled_text_run(run: &str) -> Result<(), ErrorKind> {
    if run.starts_with(SPACE) || run.ends_with(SPACE) {
        return Err(ErrorKind::LooseDelimiter);
    }

    Ok(())
}

fn parse_markup_text(tokeniser: &mut Tokeniser) -> ParseResult<String> {
    let mut run = String::new();

    let next = tokeniser.peek();

    if !matches!(next.value, Token::MarkupTextSpace | Token::MarkupText(_)) {
        return parse_err!(ErrorKind::UnexpectedTextRunStart, next);
    }

    loop {
        let next = tokeniser.peek();
        if let Some(text_token) = next.try_consume() {
            let MarkupText(text) = text_token.value;
            run.push_str(text);
            tokeniser.advance();
        } else if next.is::<MarkupTextSpace>() {
            tokeniser.advance();
            run.push(SPACE);
        } else {
            break;
        }
    }

    Ok(run)
}

fn parse_code(tokeniser: &mut Tokeniser) -> ParseResult<doc::Block> {
    tokeniser.advance().require::<CodeDirective>()?;

    tokeniser.advance().require::<LineBreak>()?;

    tokeniser.push_mode(ScanMode::Code);

    tokeniser.advance().require::<CodeDelimiter>()?;

    tokeniser.pop_mode();

    tokeniser.advance().require::<LineBreak>()?;

    tokeniser.push_mode(ScanMode::Code);

    let code_token = tokeniser.advance().require()?;
    let Code(code) = code_token.value;

    tokeniser.advance().require::<CodeDelimiter>()?;

    tokeniser.pop_mode();

    let block = doc::Block::Code(String::from(code));
    Ok(block)
}

//TODO: Implement a concat with auto newlines macro?
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

        ({$($fields:tt)*} $field:ident : $value:expr $(, $($tail:tt)+)?) => {
            build_document!(
                {
                    $field: document_field!($field $value),
                    $($fields)*
                }
                $($($tail)+)?
            )

        };
    }

    macro_rules! document_field {
        (title $value:expr) => {
            String::from($value)
        };
        (metadata $($token:tt)+) => {
            build_metadata!({} $($token)+)
        };
        (contents $($token:tt)+) => {
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
                    $field: metadata_field!($field $value),
                    $($fields)*
                }
                $($($tail)*)?
            )
        };

        ( {$($fields:tt)*}) => {
            doc::Metadata{
                $($fields)*
              ..Default::default()
            }
        };
    }

    macro_rules! metadata_field {
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
                [$(doc::Reference {
                    id: $ref_id.to_string(),
                    link: $ref_link.to_string(),
                },)*]
            )
        };
    }

    //TODO: Less confusing for macros to be more like (info: {...}) => {...}
    macro_rules! element {
        (info $( $block:ident { $($content:tt)* } $(,)? )*) => {
            doc::Element::Container(doc::Container{
                content: Box::new([
                    $(
                        block!($block $($content)*),
                    )*
                ]),
                kind: doc::ContainerKind::Info,
            })
        };

        (section ($name:expr) $( $element:ident $(($element_name:expr))? { $($content:tt)* } $(,)? )*) => {
            doc::Element::Section(doc::Section{
                content: Box::new([
                    $(
                        section_element!($element $(($element_name))? $($content)*),
                    )*
                ]),
                heading: String::from($name)
            })
        };

        ($block:ident $($content:tt)*) => {
            doc::Element::Block(block!($block $($content)*))
        };


    }

    macro_rules! section_element {
        (info $( $block:ident { $($content:tt)* } $(,)? )*) => {
            SectionElement::Container(Container{
                content: Box::new([
                    $(
                        block!($block $($content)*),
                    )*
                ]),
                kind: ContainerKind::Info,
            })
        };

        (subsection ($name:expr) $( $element:ident $(($element_name:expr))? { $($content:tt)* } $(,)? )*) => {
            doc::SectionElement::SubSection(doc::SubSection{
                content: Box::new([
                    $(
                        subsection_element!($element $(($element_name))? $($content)*),
                    )*
                ]),
                heading: String::from($name)
            })
        };

        ($block:ident $($content:tt)*) => {
            doc::SectionElement::Block(block!($block $($content)*))
        };
    }

    macro_rules! subsection_element {
        (info $( $block:ident { $($content:tt)* } $(,)? )*) => {
            doc::SubSectionElement::Container(doc::Container{
                content: Box::new([
                    $(
                        block!($block $($content)*),
                    )*
                ]),
                kind: ContainerKind::Info,
            })
        };

        ($block:ident $($content:tt)*) => {
            doc::SubSectionElement::Block(block!($block $($content)*))
        };

    }

    macro_rules! block {
        (paragraph $($text:expr),* $(,)?) => {
            doc::Block::Paragraph(Box::new([
                $(
                    $text,
                )*
            ]))
        };

        (list $($item:ident { $($content:tt)* } $(,)?)*) => {
            doc::Block::List(
                doc::List {
                    items: Box::new([
                    $(
                        list_item!($item $($content)*),
                    )*
                    ]),
                    style: doc::ListStyle::Unordered,
                }
            )
        };

        (ordered_list $($item:ident { $($content:tt)* } $(,)?)*) => {
            doc::Block::List(
                doc::List {
                    items: Box::new([
                    $(
                        list_item!($item $($content)*),
                    )*
                    ]),
                    style: doc::ListStyle::Ordered,
                }
            )
        };

        (code $($text:expr),+ $(,)?) => {
            doc::Block::Code(concat!($($text,)+).to_string())
        };
    }

    macro_rules! list_item {
        (paragraph $($text:expr),* $(,)?) => {
            doc::ListItem::Text(Box::new([
                $(
                    $text,
                )*
            ]))
        };

        (list $($item:ident { $($content:tt)* } $(,)?)*) => {
            doc::ListItem::SubList(Box::new([
                $(
                    list_item!($item $($content)*),
                )*
            ]))
        };
    }

    macro_rules! info {
        ($($content:tt)*) => {
            Box::new([element!(info $($content)*)])
        }
    }

    macro_rules! list {
        ($($content:tt)*) => {
            Box::new([element!(list $($content)*)])
        }
    }

    macro_rules! ordered_list {
        ($($content:tt)*) => {
            Box::new([element!(ordered_list $($content)*)])
        }
    }

    macro_rules! paragraph {
        ($($content:tt)*) => {
            Box::new([element!(paragraph $($content)*)])
        }
    }

    macro_rules! code {
        ($($content:tt)*) => {
            Box::new([element!(code $($content)*)])
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
            Box::new(
                [$(element!(
                    $element_type
                    $(($element_name))?
                    $($element_content)*
                ),)*]
            )
        }
    }

    fn text(text: &str) -> doc::TextRun {
        doc::TextRun {
            text: text.to_string(),
            style: doc::Style::None,
        }
    }

    fn emphasised_text(text: &str) -> doc::TextRun {
        doc::TextRun {
            text: text.to_string(),
            style: doc::Style::Emphasis,
        }
    }

    fn strong_text(text: &str) -> doc::TextRun {
        doc::TextRun {
            text: text.to_string(),
            style: doc::Style::Strong,
        }
    }

    fn strikethrough_text(text: &str) -> doc::TextRun {
        doc::TextRun {
            text: text.to_string(),
            style: doc::Style::Strikethrough,
        }
    }

    fn raw_text(text: &str) -> doc::TextRun {
        doc::TextRun {
            text: text.to_string(),
            style: doc::Style::Raw,
        }
    }

    fn linked_text(text: &str, reference: &str) -> doc::TextRun {
        doc::TextRun {
            text: text.to_string(),
            style: doc::Style::Link(reference.to_string()),
        }
    }

    fn parse_document_str(document: &'static str) -> ParseResult<Document> {
        parse_str(&document)
    }

    fn parse_content_str(content: &'static str) -> ParseResult<Document> {
        let content_with_title = "/ Some Document\n\n".to_string() + content;
        parse_str(&content_with_title)
    }

    fn assert_document_eq(result: ParseResult<Document>, expected: Document) {
        let doc = expect_successful_parse(result);
        if doc != expected {
            eprintln!("Actual:\n{:#?}", doc);
            eprintln!("Expected:\n{:#?}", expected);
            panic!("Parsed content not what was expected")
        }
    }

    fn assert_content_eq(result: ParseResult<Document>, expected: Box<[doc::Element]>) {
        let doc = expect_successful_parse(result);
        if doc.contents != expected {
            eprintln!("Actual:\n{:#?}", doc.contents);
            eprintln!("Expected:\n{:#?}", expected);
            panic!("Parsed content not what was expected")
        }
    }

    fn assert_parse_fails(result: ParseResult<Document>, expected: ErrorKind) {
        match result {
            Ok(doc) => {
                eprintln!("Expected parse to fail, but got doc:");
                eprintln!("{:#?}\n", doc);
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

    fn expect_successful_parse(result: ParseResult<Document>) -> Document {
        match result {
            Ok(doc) => doc,
            Err(error) => {
                eprintln!("{}", error);
                panic!("parse unexpectedly failed")
            }
        }
    }

    //TODO: See if we can group / order these ever growing tests...

    #[test]
    fn complete_doc_test() {
        let input = concat!(
            "@metadata\n",
            "id: 01.42\n",
            "\n",
            "/ Feline friendly flower arranging\n",
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
            "     one\n",
            "\n",
            "Yay!"
        );

        //TODO: Simplify macro - dont need explicit nesting of elements ?
        // document!(
        // metadata: {...}
        // references: { ... },
        // title {},
        // info {}
        // )
        //

        let expected = document!(
            title: "Feline friendly flower arranging",
            metadata: {
                id: "01.42",
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
                },
                paragraph {
                    text("Yay!")
                }
            }
        );

        let result = parse_document_str(input);
        assert_document_eq(result, expected);
    }

    #[test]
    fn one_line_paragraph() {
        let input = "We like cats very much";

        let expected = paragraph! { text("We like cats very much") };

        let result = parse_content_str(input);
        assert_content_eq(result, expected);
    }

    #[test]
    fn explicit_paragraph() {
        let input = "#paragraph\nCats go meeow!";

        let expected = paragraph! { text("Cats go meeow!") };

        let result = parse_content_str(input);
        assert_content_eq(result, expected);
    }

    #[test]
    fn explicit_paragraph_with_block_break_before_text_is_rejected() {
        let input = "#paragraph\n\nCats go meeow!";

        let expected = ErrorKind::ExpectedToken(LineBreak::NAME);

        let result = parse_content_str(input);
        assert_parse_fails(result, expected);
    }

    #[test]
    fn unknown_block_directive_is_rejected() {
        let input = "#meowograph\nMeow?";

        let expected = ErrorKind::UnknownDirective("#meowograph".into());

        let result = parse_content_str(input);
        assert_parse_fails(result, expected);
    }

    #[test]
    fn unknown_data_directive_is_rejected() {
        let input = "@mrerps\nPurrRR!";

        let expected = ErrorKind::UnknownDirective("@mrerps".into());

        let result = parse_content_str(input);
        assert_parse_fails(result, expected);
    }

    #[test]
    fn unknown_container_directive_is_rejected() {
        let input = "!meeps\nMorps!";

        let expected = ErrorKind::UnknownDirective("!meeps".into());

        let result = parse_content_str(input);
        assert_parse_fails(result, expected);
    }

    #[test]
    fn empty_block_name_is_rejected() {
        let input = "#\nHi";

        let expected = ErrorKind::UnknownDirective("#".into());

        let result = parse_content_str(input);
        assert_parse_fails(result, expected);
    }

    #[test]
    fn block_without_new_line_is_rejected() {
        let input = "#paragraph";

        let expected = ErrorKind::ExpectedToken(LineBreak::NAME);

        let result = parse_content_str(input);
        assert_parse_fails(result, expected);
    }

    #[test]
    fn double_space() {
        let input = "Nice  kitty!";

        let expected = paragraph! { text("Nice kitty!") };

        let result = parse_content_str(input);
        assert_content_eq(result, expected);
    }

    #[test]
    fn trailing_new_line_is_ignored() {
        let input = "Cats\n";

        let expected = paragraph! { text("Cats") };

        let result = parse_content_str(input);
        assert_content_eq(result, expected);
    }

    #[test]
    fn space_then_trailing_new_line_is_ignored() {
        let input = "Cats \n";

        let expected = paragraph! { text("Cats") };

        let result = parse_content_str(input);
        assert_content_eq(result, expected);
    }

    #[test]
    fn new_line_becomes_whitespace() {
        let input = "Cats\nwhiskers";

        let expected = paragraph! { text("Cats whiskers") };

        let result = parse_content_str(input);
        assert_content_eq(result, expected);
    }

    #[test]
    fn new_line_becomes_whitespace_given_plain_then_styled() {
        let input = "Cats\n*whiskers*";

        let expected = paragraph! {
            text("Cats "),
            strong_text("whiskers"),
        };

        let result = parse_content_str(input);
        assert_content_eq(result, expected);
    }

    #[test]
    fn new_line_becomes_whitespace_given_plain_then_raw() {
        let input = "Cats\n`nice whiskers`";

        let expected = paragraph! {
            text("Cats "),
            raw_text("nice whiskers"),
        };

        let result = parse_content_str(input);
        assert_content_eq(result, expected);
    }

    #[test]
    fn new_line_with_extra_whitespace_collapses() {
        let input = "Cats    \n    whiskers";
        let expected = paragraph! { text("Cats whiskers") };

        let result = parse_content_str(input);
        assert_content_eq(result, expected);
    }

    #[test]
    fn two_new_lines_become_blocks() {
        let input = "Cats\n\nwhiskers";

        let expected = elements! {
            paragraph { text("Cats") },
            paragraph { text("whiskers") }
        };

        let result = parse_content_str(input);
        assert_content_eq(result, expected);
    }

    #[test]
    fn three_new_lines_becomes_blocks() {
        let input = "Cats\n\n\nwhiskers";

        let expected = elements! {
            paragraph { text("Cats") },
            paragraph { text("whiskers") }
        };

        let result = parse_content_str(input);
        assert_content_eq(result, expected);
    }

    #[test]
    fn two_new_lines_with_whitespace_is_treated_as_blockbreak() {
        let input = "Cats\n \nwhiskers";

        let expected = elements! {
            paragraph { text("Cats") },
            paragraph { text("whiskers") }
        };

        let result = parse_content_str(input);
        assert_content_eq(result, expected);
    }

    #[test]
    fn blockbreak_with_extra_whitespace() {
        let input = "Cats  \n    \n  whiskers";
        let expected = elements!(
            paragraph { text("Cats") },
            paragraph { text("whiskers") }
        );

        let result = parse_content_str(input);
        assert_content_eq(result, expected);
    }

    #[test]
    fn missing_blockbreak_is_rejected() {
        let input = concat!(
            "Cats can sometimes be\n",
            "#paragraph\n",
            "ever so surprising\n"
        );

        let expected = ErrorKind::ExpectedToken(BlockBreak::NAME);

        let result = parse_content_str(input);
        assert_parse_fails(result, expected);
    }

    #[test]
    fn escaped_char() {
        let input = "\\A";

        let expected = paragraph! { text("A") };

        let result = parse_content_str(input);
        assert_content_eq(result, expected);
    }

    #[test]
    fn escaped_underscore_in_markup() {
        let input = "My cat does backflips \\_coolcat";

        let expected = paragraph! { text("My cat does backflips _coolcat") };

        let result = parse_content_str(input);
        assert_content_eq(result, expected);
    }

    #[test]
    fn escaped_underscore() {
        let input = "cat\\_case";

        let expected = paragraph! { text("cat_case") };

        let result = parse_content_str(input);
        assert_content_eq(result, expected);
    }

    #[test]
    fn escaped_underscore_in_emphasis() {
        let input = "_cat\\_case_";

        let expected = paragraph! { emphasised_text("cat_case") };

        let result = parse_content_str(input);
        assert_content_eq(result, expected);
    }

    #[test]
    fn escaped_ignored_in_raw() {
        let input = "`cat\\_case`";

        let expected = paragraph! { raw_text("cat\\_case") };

        let result = parse_content_str(input);
        assert_content_eq(result, expected);
    }

    #[test]
    fn emphasised_words() {
        let input = "We _totally adore_ them";

        let expected = paragraph! {
            text("We "),
            emphasised_text("totally adore"),
            text(" them"),
        };

        let result = parse_content_str(input);
        assert_content_eq(result, expected);
    }

    #[test]
    fn emphasis_at_end_of_line() {
        let input = "Cats like to _zoom_\naround";

        let expected = paragraph! {
            text("Cats like to "),
            emphasised_text("zoom"),
            text(" around"),
        };

        let result = parse_content_str(input);
        assert_content_eq(result, expected);
    }

    #[test]
    fn strong_words() {
        let input = "I *need to pet that cat* right away.";

        let expected = paragraph! {
            text("I "),
            strong_text("need to pet that cat"),
            text(" right away."),
        };

        let result = parse_content_str(input);
        assert_content_eq(result, expected);
    }

    #[test]
    fn strong_mid_word() {
        let input = "I said: mee*ooOOo*ww!";

        let expected = paragraph! {
            text("I said: mee"),
            strong_text("ooOOo"),
            text("ww!"),
        };

        let result = parse_content_str(input);
        assert_content_eq(result, expected);
    }

    #[test]
    fn strong_over_two_lines() {
        let input = "*me\now*";

        let expected = paragraph! { strong_text("me ow") };

        let result = parse_content_str(input);
        assert_content_eq(result, expected);
    }

    #[test]
    fn strikethrough_words() {
        let input = "Cats are ~ok i guess~ magnificant";

        let expected = paragraph! {
            text("Cats are "),
            strikethrough_text("ok i guess"),
            text(" magnificant"),
        };

        let result = parse_content_str(input);
        assert_content_eq(result, expected);
    }

    #[test]
    fn raw_words() {
        let input = "Robot cat says `bleep bloop`!";

        let expected = paragraph! {
            text("Robot cat says "),
            raw_text("bleep bloop"),
            text("!"),
        };

        let result = parse_content_str(input);
        assert_content_eq(result, expected);
    }

    #[test]
    fn raw_mid_word() {
        let input = "Bl`eeee`p!";

        let expected = paragraph! {
            text("Bl"),
            raw_text("eeee"),
            text("p!"),
        };

        let result = parse_content_str(input);
        assert_content_eq(result, expected);
    }

    #[test]
    fn underscore_in_raw() {
        let input = "Set `PURR_LOUDLY` to true";

        let expected = paragraph! {
            text("Set "),
            raw_text("PURR_LOUDLY"),
            text(" to true"),
        };

        let result = parse_content_str(input);
        assert_content_eq(result, expected);
    }

    #[test]
    fn extra_spaces_in_raw() {
        let input = "`Keep your       distance`";

        let expected = paragraph! { raw_text("Keep your       distance") };

        let result = parse_content_str(input);
        assert_content_eq(result, expected);
    }

    #[test]
    fn raw_over_two_lines() {
        let input = "`Great\ncats`";

        let expected = paragraph! { raw_text("Great cats") };

        let result = parse_content_str(input);
        assert_content_eq(result, expected);
    }

    #[test]
    fn strikethrough_over_two_lines() {
        let input = "~Great\ndogs~";

        let expected = paragraph! { strikethrough_text("Great dogs") };

        let result = parse_content_str(input);
        assert_content_eq(result, expected);
    }

    #[test]
    fn raw_leading_with_new_line() {
        let input = "`\nMeow?`";

        let expected = paragraph! { raw_text(" Meow?") };

        let result = parse_content_str(input);
        assert_content_eq(result, expected);
    }

    #[test]
    fn raw_trailing_with_new_line() {
        let input = "`Meow\n`";

        let expected = paragraph! { raw_text("Meow ") };

        let result = parse_content_str(input);
        assert_content_eq(result, expected);
    }

    #[test]
    fn raw_leading_with_space() {
        let input = "` Meow`";

        let expected = paragraph! { raw_text(" Meow") };

        let result = parse_content_str(input);
        assert_content_eq(result, expected);
    }

    #[test]
    fn raw_trailing_with_space() {
        let input = "`Meow `";

        let expected = paragraph! { raw_text("Meow ") };

        let result = parse_content_str(input);
        assert_content_eq(result, expected);
    }

    #[test]
    fn raw_over_three_lines() {
        let input = "`Great\ncats\nassemble!`";

        let expected = paragraph! { raw_text("Great cats assemble!") };

        let result = parse_content_str(input);
        assert_content_eq(result, expected);
    }

    #[test]
    fn standalone_dash() {
        let input = "Felines - fantastic!";

        let expected = paragraph! { text("Felines - fantastic!") };

        let result = parse_content_str(input);
        assert_content_eq(result, expected);
    }

    #[test]
    fn paragraph_with_trailing_whitespace() {
        let input = "Cool kitty   ";

        let expected = paragraph! { text("Cool kitty") };

        let result = parse_content_str(input);
        assert_content_eq(result, expected);
    }

    #[test]
    fn underscore_in_awkward_places() {
        let input = "Cat cat_cat cat_ cat.";

        let expected = paragraph! {
            text("Cat cat"),
            emphasised_text("cat cat"),
            text(" cat.")
        };

        let result = parse_content_str(input);
        assert_content_eq(result, expected);
    }

    #[test]
    fn new_line_then_multiple_spaces_in_plain_text() {
        let input = "Cat\n  cat";

        let expected = paragraph! { text("Cat cat") };

        let result = parse_content_str(input);
        assert_content_eq(result, expected);
    }

    #[test]
    fn new_line_then_multiple_spaces_in_styled() {
        let input = "*Cat\n  cat*";

        let expected = paragraph! { strong_text("Cat cat") };

        let result = parse_content_str(input);
        assert_content_eq(result, expected);
    }

    #[test]
    fn new_line_then_multiple_spaces_in_raw() {
        let input = "`Cat\n  cat`";

        let expected = paragraph! { raw_text("Cat   cat") };

        let result = parse_content_str(input);
        assert_content_eq(result, expected);
    }

    #[test]
    fn multiple_spaces_then_new_line_in_plain_text() {
        let input = "Cat  \ncat";

        let expected = paragraph! { text("Cat cat") };

        let result = parse_content_str(input);
        assert_content_eq(result, expected);
    }

    #[test]
    fn multiple_spaces_then_new_line_in_styled() {
        let input = "*Cat  \ncat*";

        let expected = paragraph! { strong_text("Cat cat") };

        let result = parse_content_str(input);
        assert_content_eq(result, expected);
    }

    #[test]
    fn multiple_spaces_then_new_line_in_raw() {
        let input = "`Cat  \ncat`";

        let expected = paragraph! { raw_text("Cat   cat") };

        let result = parse_content_str(input);
        assert_content_eq(result, expected);
    }

    #[test]
    fn empty_emphasis() {
        let input = "Rules cats must follow: __.";

        let expected = ErrorKind::UnexpectedTextRunStart;

        let result = parse_content_str(input);
        assert_parse_fails(result, expected);
    }

    #[test]
    fn empty_raw() {
        let input = "Robot cat says: ``!.";

        let expected = ErrorKind::UnexpectedRawTextRunStart;

        let result = parse_content_str(input);
        assert_parse_fails(result, expected);
    }

    #[test]
    fn raw_with_double_linebreak() {
        let input = "`Erm...\n\nmeow?`";

        let expected = ErrorKind::ExpectedToken(RawDelimiter::NAME);

        let result = parse_content_str(input);
        assert_parse_fails(result, expected);
    }

    #[test]
    fn raw_with_double_linebreak_containing_whitespace() {
        let input = "`Erm...\n \nmeow?`";

        let expected = ErrorKind::ExpectedToken(RawDelimiter::NAME);

        let result = parse_content_str(input);
        assert_parse_fails(result, expected);
    }

    #[test]
    fn strikethrough_with_double_linebreak() {
        let input = "~Erm...\n\nmeow?~";

        let expected = ErrorKind::ExpectedToken(StrikethroughDelimiter::NAME);

        let result = parse_content_str(input);
        assert_parse_fails(result, expected);
    }

    #[test]
    fn unmatched_emphasis_1() {
        let input = "_.";

        let expected = ErrorKind::ExpectedToken(EmphasisDelimiter::NAME);

        let result = parse_content_str(input);
        assert_parse_fails(result, expected);
    }

    #[test]
    fn unmatched_emphasis_2() {
        let input = "meow _meow.";

        let expected = ErrorKind::ExpectedToken(EmphasisDelimiter::NAME);

        let result = parse_content_str(input);
        assert_parse_fails(result, expected);
    }

    #[test]
    fn unmatched_emphasis_3() {
        let input = "meow meow_";

        let expected = ErrorKind::UnexpectedTextRunStart;

        let result = parse_content_str(input);
        assert_parse_fails(result, expected);
    }

    #[test]
    fn nested_styled_text() {
        let input = "_*meow!*_";

        let expected = ErrorKind::UnexpectedTextRunStart;

        let result = parse_content_str(input);
        assert_parse_fails(result, expected);
    }

    #[test]
    fn loose_strong_delimiter_start() {
        let input = "* meow meow*";

        let expected = ErrorKind::LooseDelimiter;

        let result = parse_content_str(input);
        assert_parse_fails(result, expected);
    }

    #[test]
    fn loose_strong_delimiter_end() {
        let input = "*meow meow *";

        let expected = ErrorKind::LooseDelimiter;

        let result = parse_content_str(input);
        assert_parse_fails(result, expected);
    }

    #[test]
    fn raw_immediately_in_emphasis() {
        let input = "_``_";

        let expected = ErrorKind::UnexpectedTextRunStart;

        let result = parse_content_str(input);
        assert_parse_fails(result, expected);
    }

    #[test]
    fn raw_within_in_emphasis() {
        let input = "_a``a_";

        let expected = ErrorKind::ExpectedToken(EmphasisDelimiter::NAME);

        let result = parse_content_str(input);
        assert_parse_fails(result, expected);
    }

    #[test]
    fn doc_with_leading_new_line() {
        let input = "\nCats cats cats";

        let expected = paragraph!(text("Cats cats cats"));

        let result = parse_content_str(input);
        assert_content_eq(result, expected);
    }

    #[test]
    fn doc_with_leading_new_lines() {
        let input = "\n\nCats cats cats";

        let expected = paragraph!(text("Cats cats cats"));

        let result = parse_content_str(input);
        assert_content_eq(result, expected);
    }

    #[test]
    fn doc_with_leading_spaces_and_new_line() {
        let input = "   \nCats cats cats";

        let expected = paragraph!(text("Cats cats cats"));

        let result = parse_content_str(input);
        assert_content_eq(result, expected);
    }

    #[test]
    fn doc_ending_with_new_line() {
        let input = "Cats are friends\n";

        let expected = paragraph! { text("Cats are friends") };

        let result = parse_content_str(input);
        assert_content_eq(result, expected);
    }

    #[test]
    fn doc_ending_with_new_lines() {
        let input = "Feline friends\n\n";

        let expected = paragraph! { text("Feline friends") };

        let result = parse_content_str(input);
        assert_content_eq(result, expected);
    }

    #[test]
    fn new_line_and_space_between_styled_and_plain_text_runs() {
        let input = "*Cat*\n cat";

        let expected = paragraph! {
            strong_text("Cat"),
            text(" cat"),
        };

        let result = parse_content_str(input);
        assert_content_eq(result, expected);
    }

    #[test]
    fn leading_whitespace_on_paragraph_is_ignored() {
        let input = "Cat\n\n  cat";

        let expected = elements!(
            paragraph { text("Cat") },
            paragraph { text("cat") }
        );

        let result = parse_content_str(input);
        assert_content_eq(result, expected);
    }

    #[test]
    fn doc_metadata() {
        let input = concat!(
            "@metadata\n",
            "id: 12.03\n",
            "\n",
            "/ Document with metadata",
        );

        let expected = document!(
            title: "Document with metadata",
            metadata: {
                id: "12.03"
            }
        );

        let result = parse_document_str(input);
        assert_document_eq(result, expected);
    }

    #[test]
    fn doc_metadata_with_tags() {
        let input = concat!(
            "@metadata\n",
            "id: feline.feasts.25\n",
            "tags: cooking | eating | nice-smells\n",
            "\n",
            "/ Document with metadata",
        );

        let expected = document!(
            title: "Document with metadata",
            metadata: {
                id:"feline.feasts.25",
                tags: ["cooking", "eating", "nice-smells"],
            }
        );

        let result = parse_document_str(input);
        assert_document_eq(result, expected);
    }

    #[test]
    fn doc_metadata_with_unknown_identifier_is_rejected() {
        let input = "@metadata\nkibble: yes please\n";

        let expected = ErrorKind::UnknownMetadata("kibble".into());

        let result = parse_document_str(input);
        assert_parse_fails(result, expected);
    }

    #[test]
    fn doc_metadata_not_at_start_is_rejected() {
        let input = concat!(
            "/ Some title\n",
            "\n",
            "Helloo there. Metadata should not follow this.!\n",
            "\n",
            "@metadata\n",
            "id: 01.23\n"
        );

        let expected = ErrorKind::MetadataNotAtStart;

        let result = parse_document_str(input);
        assert_parse_fails(result, expected);
    }

    #[test]
    fn doc_title() {
        let input = "/ Practical espionage for felines in urban settings";

        let expected = document!(
            title: "Practical espionage for felines in urban settings"
        );

        let result = parse_document_str(input);
        assert_document_eq(result, expected);
    }

    #[test]
    fn doc_title_with_folowing_para() {
        let input = concat!(
            "/ Some Doc\n",
            "\n",
            "\n",
            "\n",
            "\n",
            "Hello cats and kittens"
        );

        let expected = document!(
            title: "Some Doc",
            contents: {
                paragraph { text("Hello cats and kittens") }
            }
        );

        let result = parse_document_str(input);
        assert_document_eq(result, expected);
    }

    #[test]
    fn doc_title_with_wonky_spacing() {
        let input = "/My Very   Cool Document   \n\n";

        let expected = document!(
            title: "My Very Cool Document"
        );

        let result = parse_document_str(input);
        assert_document_eq(result, expected);
    }

    #[test]
    fn doc_title_with_no_trailing_newline() {
        let input = "/Some Doc";

        let expected = document!(
            title: "Some Doc"
        );

        let result = parse_document_str(input);
        assert_document_eq(result, expected);
    }

    #[test]
    fn doc_title_in_not_at_start() {
        let input = concat!(
            "Document should not be after this!\n",
            "\n",
            "/Some Document Title"
        );

        //TODO: double expected is a bit meh
        let expected = ErrorKind::ExpectedToken(TitleDirective::NAME);

        let result = parse_document_str(input);
        assert_parse_fails(result, expected);
    }

    #[test]
    fn doc_title_in_section() {
        let input = concat!(
            "/ Some Document Title\n",
            "\n",
            "// Some important document section\n",
            "\n",
            "/ Other Document Title"
        );

        let expected = ErrorKind::UnexpectedBlockStart;

        let result = parse_document_str(input);
        assert_parse_fails(result, expected);
    }

    #[test]
    fn doc_title_in_sub_section() {
        let input = concat!(
            "/ Some Document Title\n",
            "\n",
            "// Some important document section\n",
            "\n",
            "/// Some important document sub section\n",
            "\n",
            "/Some Document Title"
        );

        let expected = ErrorKind::UnexpectedBlockStart;

        let result = parse_document_str(input);
        assert_parse_fails(result, expected);
    }

    #[test]
    fn empty_doc_title() {
        let input = "/";

        let result = parse_document_str(input);

        let expected = ErrorKind::UnexpectedHeaderTextStart;

        assert_parse_fails(result, expected);
    }

    #[test]
    fn subsection_missing_parent() {
        let input = concat!(
            "/Some Document Title\n",
            "\n",
            "/// Some important document sub section\n",
        );

        let expected = ErrorKind::SubSectionNotNested;

        let result = parse_document_str(input);
        assert_parse_fails(result, expected);
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

        let result = parse_content_str(input);
        assert_content_eq(result, expected);
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

        let result = parse_content_str(input);
        assert_content_eq(result, expected);
    }

    #[test]
    fn container_missing_start_is_rejected() {
        let input = "Silly cat\n<<<";

        let expected = ErrorKind::ContainerMissingStart;

        let result = parse_content_str(input);
        assert_parse_fails(result, expected);
    }

    #[test]
    fn empty_container_is_rejected() {
        let input = "!info\n";

        let expected = ErrorKind::EmptyContainer;

        let result = parse_content_str(input);
        assert_parse_fails(result, expected);
    }

    #[test]
    fn detactched_container_is_rejected() {
        let input = "!info\n\ncats!";

        let expected = ErrorKind::EmptyContainer;

        let result = parse_content_str(input);
        assert_parse_fails(result, expected);
    }

    #[test]
    fn trailing_text_on_delimited_start_is_rejected() {
        let input = concat!(
            "!info\n",
            ">>>squeek\n",
            "Let me know if you find where I left my\n",
            "<<<"
        );

        let expected = ErrorKind::ExpectedToken(LineBreak::NAME);

        let result = parse_content_str(input);
        assert_parse_fails(result, expected);
    }

    #[test]
    fn trailing_text_on_delimited_end_is_rejected() {
        let input = concat!(
            "!info\n",
            ">>>\n",
            "Let me know if you find where I left my\n",
            "<<<toy"
        );

        let expected = ErrorKind::ExpectedToken(BlockBreak::NAME);

        let result = parse_content_str(input);
        assert_parse_fails(result, expected);
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

        let expected = ErrorKind::ExpectedToken(BlockBreak::NAME);

        let result = parse_content_str(input);
        assert_parse_fails(result, expected);
    }

    #[test]
    fn dash_in_paragraph_is_treated_as_part_of_text() {
        let input = "Ripley\n- Cat";

        let expected = paragraph! { text("Ripley - Cat") };

        let result = parse_content_str(input);
        assert_content_eq(result, expected);
    }

    #[test]
    fn indented_dash_in_paragraph_is_treated_as_part_of_text() {
        let input = "Ripley\n  - Cat";

        let expected = paragraph! { text("Ripley - Cat") };

        let result = parse_content_str(input);
        assert_content_eq(result, expected);
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
        let result = parse_content_str(input);
        assert_content_eq(result, expected);
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

        let result = parse_content_str(input);
        assert_content_eq(result, expected);
    }

    #[test]
    fn ordered_list() {
        let input = concat!(
            "#list(style=ordered)\n",
            "- Dry food is ok\n",
            "- Wet food is much better\n",
            "- Water is important also\n"
        );

        let expected = ordered_list! {
            paragraph { text("Dry food is ok")},
            paragraph { text("Wet food is much better")},
            paragraph { text("Water is important also")}
        };
        let result = parse_content_str(input);
        assert_content_eq(result, expected);
    }

    #[test]
    fn list_with_invalid_style() {
        let input = concat!(
            "#list(style=cool)\n",
            "- Dont you think this list is really rather neat?\n",
        );

        let expected = ErrorKind::InvalidListStyle("cool".into());

        let result = parse_content_str(input);
        assert_parse_fails(result, expected);
    }

    #[test]
    fn list_with_invalid_parameter() {
        let input = concat!(
            "#list(up=down)\n",
            "- Dont you think this list is really rather odd?\n",
        );

        let expected = ErrorKind::InvalidListParameter("up".into());

        let result = parse_content_str(input);
        assert_parse_fails(result, expected);
    }

    #[test]
    fn dash_in_list_text_is_not_treated_as_bullet() {
        let input = concat!("- Meow - meow\n",);

        let expected = list! {paragraph { text("Meow - meow") }};

        let result = parse_content_str(input);
        assert_content_eq(result, expected);
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

        let result = parse_content_str(input);
        assert_content_eq(result, expected);
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

        let result = parse_content_str(input);
        assert_content_eq(result, expected);
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

        let result = parse_content_str(input);
        assert_content_eq(result, expected);
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

        let result = parse_content_str(input);
        assert_content_eq(result, expected);
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

        let result = parse_content_str(input);
        assert_content_eq(result, expected);
    }

    #[test]
    fn list_item_with_trailing_whitespace() {
        let input = "- Foo    \n- Bar";

        let expected = list! {
            paragraph { text("Foo")},
            paragraph { text("Bar")},
        };

        let result = parse_content_str(input);
        assert_content_eq(result, expected);
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

        let result = parse_content_str(input);
        assert_content_eq(result, expected);
    }

    #[test]
    fn list_with_emphasis_over_multiple_points() {
        let input = "- f_oo\n  -ba_r";

        let expected = ErrorKind::ExpectedToken(EmphasisDelimiter::NAME);

        let result = parse_content_str(input);
        assert_parse_fails(result, expected);
    }

    #[test]
    fn list_with_uneven_spaces() {
        let input = "-foo\n -bar";

        let expected = ErrorKind::UnevenListIndent(Indent { space_count: 1 });

        let result = parse_content_str(input);
        assert_parse_fails(result, expected);
    }

    #[test]
    fn list_that_skips_ascending_indent_level() {
        let input = concat!(
            "- Nice things to eat\n",
            "    - Wagyu beef because it is oh so tender\n",
        );

        let expected = ErrorKind::MissingListLevel { from: 0, to: 2 };

        let result = parse_content_str(input);
        assert_parse_fails(result, expected);
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

        let result = parse_content_str(input);
        assert_content_eq(result, expected);
    }

    #[test]
    fn error_specifies_correct_row_and_column() {
        let input = concat!(
            "/ Document with an silly cat error\n",
            "\n",
            "Silly cat\n",
            "goes *_*"
        );

        let expected = (6, 3);

        let error = parse_document_str(input).unwrap_err();
        let position = error.token.position;
        let actual = (position.column, position.row);

        assert_eq!(actual, expected);
    }

    #[test]
    fn error_includes_token_name() {
        let input = concat!(
            "/ Document with an silly cat error\n",
            "\n",
            "Silly cat\n",
            "goes *_*"
        );

        let expected = EmphasisDelimiter::NAME;

        let error = parse_document_str(input).unwrap_err();
        let actual = error.token.name;

        assert_eq!(actual, expected);
    }

    #[test]
    fn error_includes_token_lexeme() {
        let input = concat!(
            "/ Document with an silly cat error\n",
            "\n",
            "Silly cat\n",
            "goes *_*"
        );

        let expected = "_".into();

        let error = parse_document_str(input).unwrap_err();
        let actual = error.token.lexeme;

        assert_eq!(actual, expected);
    }

    #[test]
    fn link_with_reference() {
        let input = concat!(
            "@references\n",
            "ripley_2020: https://example.com\n",
            "\n",
            "/ Cat petting tips\n",
            "\n",
            "For more info, consult [our guide on petting cats]@ripley_2020,\n",
            "created by our own in house experts.\n",
        );

        let expected = document!(
            title: "Cat petting tips",
            contents: {
                paragraph {
                    text("For more info, consult "),
                    linked_text("our guide on petting cats", "ripley_2020"),
                    text(", created by our own in house experts.")
                }
            },
            references: {
                ("ripley_2020", "https://example.com")
            }
        );

        let result = parse_document_str(input);
        assert_document_eq(result, expected);
    }

    #[test]
    fn link_with_following_space() {
        let input = concat!(
            "@references\n",
            "some_ref: https://example.com\n",
            "\n",
            "/ Some doc\n",
            "\n",
            "See [our guide]@some_ref for more\n",
        );

        let expected = document!(
            title: "Some doc",
            contents: {
                paragraph {
                    text("See "),
                    linked_text("our guide", "some_ref"),
                    text(" for more")
                }
            },
            references: {
                ("some_ref", "https://example.com")
            }
        );

        let result = parse_document_str(input);
        assert_document_eq(result, expected);
    }

    #[test]
    fn references_with_odd_spacing() {
        let input = concat!(
            "@references\n",
            "ripley_2020: https://example.com/a\n",
            "ripley_2021:https://example.com/b\n",
            "ripley_2022 :https://example.com/c\n",
            "ripley_2023 : https://example.com/d\n",
            "ripley_2024: https://example.com/e  \n",
            "\n",
            "/ Doc with lots of references",
        );

        let expected = document!(
            title: "Doc with lots of references",
            references: {
                ("ripley_2020", "https://example.com/a"),
                ("ripley_2021", "https://example.com/b"),
                ("ripley_2022", "https://example.com/c"),
                ("ripley_2023", "https://example.com/d"),
                ("ripley_2024", "https://example.com/e"),
            }
        );

        let result = parse_document_str(input);
        assert_document_eq(result, expected);

        //TODO: I guess we should have assert for references and metadata also?
        // e.g
        // let result = parse_references_str(input);
        // assert_references_eq(result, expected)
    }

    #[test]
    fn at_sign_can_be_used_normally() {
        let input = "C@ts are great @ that";

        let expected = paragraph! {
            text("C@ts are great @ that")
        };

        let result = parse_content_str(input);
        assert_content_eq(result, expected);
    }

    #[test]
    fn whitespace_around_linked_text_is_rejected() {
        let input = concat!(
            "@references\n",
            "ripley_2020: https://example.com\n",
            "\n",
            "/ Some Title\n",
            "\n",
            "We like [ petting cats ]@ripley_2020 a lot.\n",
        );

        let expected = ErrorKind::LooseDelimiter;

        let result = parse_document_str(input);
        assert_parse_fails(result, expected);
    }

    #[test]
    fn references_after_content_rejected() {
        let input = concat!(
            "/ Some Title\n",
            "\n",
            "For more info, consult [our guide on petting cats]@ripley_2020,\n",
            "created by our own in house experts.\n",
            "\n",
            "@references\n",
            "ripley_2020: https://example.com"
        );

        let expected = ErrorKind::ReferencesOutOfPlace;

        let result = parse_document_str(input);
        assert_parse_fails(result, expected);
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
            title: "Speed running the kitchen at 4am",
            contents: {
                paragraph { text("This is a comprehensive guide.") },

                section("Motivation") {
                    paragraph { text("Set a personal best, while others rest!") },
                },
                section("Planning the perfect lap") {
                    paragraph { text("This requires care.") },
                    subsection("Selecting a route") {
                        paragraph { text("Avoid the toaster.") },
                    }
                    subsection("Choosing a victory scream") {
                        paragraph { text("Meeaaahhh?") },
                    },
                },
                section("Conclusion") {
                    paragraph { text("Go go go!") },
                }
            }
        };

        let result = parse_document_str(input);
        assert_document_eq(result, expected);
    }

    #[test]
    fn section_with_trailing_space_in_title() {
        let input = concat!(
            "/Speed running the kitchen at 4am \n",
            "\n",
            "This is a comprehensive guide.\n",
        );

        let expected = document! {
            title: "Speed running the kitchen at 4am",
            contents: {
                paragraph { text("This is a comprehensive guide.") }
            }
        };

        let result = parse_document_str(input);
        assert_document_eq(result, expected);
    }

    #[test]
    fn code_block() {
        let input = concat!(
            "#code\n",
            "---\n",
            "Meow?\n",
            "\n",
            "Meow.\n",
            "Me...           ...ow.\n",
            "Meow!\n",
            "---\n"
        );

        let expected = code! {
            "Meow?\n",
            "\n",
            "Meow.\n",
            "Me...           ...ow.\n",
            "Meow!\n",
        };

        let result = parse_content_str(input);
        assert_content_eq(result, expected);
    }

    #[test]
    fn code_block_then_paragraph() {
        let input = concat!(
            "#code\n",
            "---\n",
            "Meow?\n",
            "---\n",
            "\n",
            "Hey, whats up?"
        );

        let expected = elements! (
            code {
                "Meow?\n",
            },
            paragraph {
                text("Hey, whats up?")
            }
        );

        let result = parse_content_str(input);
        assert_content_eq(result, expected);
    }
}
