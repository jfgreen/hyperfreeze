use std::backtrace::{Backtrace, BacktraceStatus};
use std::fmt::{self, Display};

use crate::document::{self as doc, Document};

// TODO: Can we solve these lint with a prelude?
#[allow(clippy::wildcard_imports)]
use crate::scan::*;

#[allow(clippy::wildcard_imports)]
use crate::token::*;

#[derive(Debug)]
pub struct ParseError {
    kind: ErrorKind,
    token: TokenDescription,
    backtrace: Backtrace,
}

#[derive(PartialEq, Eq, Debug)]
enum ErrorKind {
    LooseDelimiter,
    MissingListLevel { from: u8, to: u8 },
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

    tokeniser.push_mode(ScanMode::ElementStart);

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
    current_depth: u8,
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

                let param_value_token = tokeniser.advance().require()?;
                let BlockParameterValue(value) = param_value_token.value;

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
        style: doc::TextStyle::None,
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
        style: doc::TextStyle::Raw,
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

    let reference_token = tokeniser.advance().require()?;
    let LinkToReference(identifier) = reference_token.value;

    Ok(doc::TextRun {
        text: run,
        style: doc::TextStyle::Link(identifier.into()),
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
        style: doc::TextStyle::Strong,
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
        style: doc::TextStyle::Emphasis,
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
        style: doc::TextStyle::Strikethrough,
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

//TODO: See if we can group / order these ever growing tests...

#[cfg(test)]
mod test {
    use super::*;

    macro_rules! document_lines {
        ($($line:literal),+ $(,)?) => {
            concat!(
                $($line, "\n",)+
            )
        };
    }

    macro_rules! content_lines {
        ($($line:literal),+ $(,)?) => {
            document_lines!(
                "/ Some doc",
                "",
                $($line,)+
            )
        };
    }

    fn text(text: &str) -> doc::TextRun {
        doc::TextRun {
            text: text.to_string(),
            style: doc::TextStyle::None,
        }
    }

    fn emphasised_text(text: &str) -> doc::TextRun {
        doc::TextRun {
            text: text.to_string(),
            style: doc::TextStyle::Emphasis,
        }
    }

    fn strong_text(text: &str) -> doc::TextRun {
        doc::TextRun {
            text: text.to_string(),
            style: doc::TextStyle::Strong,
        }
    }

    fn strikethrough_text(text: &str) -> doc::TextRun {
        doc::TextRun {
            text: text.to_string(),
            style: doc::TextStyle::Strikethrough,
        }
    }

    fn raw_text(text: &str) -> doc::TextRun {
        doc::TextRun {
            text: text.to_string(),
            style: doc::TextStyle::Raw,
        }
    }

    fn linked_text(text: &str, reference: &str) -> doc::TextRun {
        doc::TextRun {
            text: text.to_string(),
            style: doc::TextStyle::Link(reference.to_string()),
        }
    }

    impl From<doc::Container> for doc::Element {
        fn from(container: doc::Container) -> Self {
            doc::Element::Container(container)
        }
    }

    impl From<doc::Section> for doc::Element {
        fn from(section: doc::Section) -> Self {
            doc::Element::Section(section)
        }
    }

    impl From<doc::Block> for doc::Element {
        fn from(block: doc::Block) -> Self {
            doc::Element::Block(block)
        }
    }

    impl From<doc::Block> for doc::SectionElement {
        fn from(block: doc::Block) -> Self {
            doc::SectionElement::Block(block)
        }
    }

    impl From<doc::SubSection> for doc::SectionElement {
        fn from(sub_section: doc::SubSection) -> Self {
            doc::SectionElement::SubSection(sub_section)
        }
    }

    impl From<doc::Block> for doc::SubSectionElement {
        fn from(block: doc::Block) -> Self {
            doc::SubSectionElement::Block(block)
        }
    }

    impl From<doc::Block> for Box<[doc::Element]> {
        fn from(block: doc::Block) -> Box<[doc::Element]> {
            Box::new([block.into()])
        }
    }

    impl From<doc::Container> for Box<[doc::Element]> {
        fn from(block: doc::Container) -> Box<[doc::Element]> {
            Box::new([block.into()])
        }
    }

    macro_rules! document {
        (
            $(title: $title:literal,)?
            $(id: $id:literal,)?
            $(tags: [$($tag:literal),+ $(,)?],)?
            $(references: [$(($ref_id:literal, $ref_link:literal)),+ $(,)?],)?
            $(contents: $contents:expr,)?
        ) => {
            {

            let mut doc = Document::default();

            $(
                doc.title = $title.to_string();
            )?

            $(
                doc.metadata.id = Some($id.to_string());
            )?

            $(
                doc.metadata.tags = Some([
                    $($tag.to_string() ,)+
                ].into());
            )?

            $(
                doc.references = [
                    $(doc::Reference {
                        id: $ref_id.to_string(),
                        link: $ref_link.to_string(),
                    },)+
                ].into();
            )?

            $(
                doc.contents = $contents.into();
            )?

            doc

            }

        };
    }

    macro_rules! contents {
        ($($item:expr),+ $(,)?) => {
            Box::new([$($item, )+])
        };
    }

    macro_rules! paragraph {
        ($($text_run:expr),+ $(,)?) => {
            doc::Block::Paragraph(
                Box::new([$($text_run, )+])
            ).into()
        };
    }

    macro_rules! list {
        ($($item:expr),+ $(,)?) => {
            doc::Block::List(doc::List {
                items: Box::new([$($item, )+]),
                style: doc::ListStyle::Unordered,
            }).into()
        };
    }

    macro_rules! ordered_list {
        ($($item:expr),+ $(,)?) => {
            doc::Block::List(doc::List {
                items: Box::new([$($item, )+]),
                style: doc::ListStyle::Ordered,
            }).into()
        };
    }

    macro_rules! info {
        ($($item:expr),+ $(,)?) => {
            doc::Container {
                content: Box::new([$($item, )+]),
                kind: doc::ContainerKind::Info,
            }.into()
        };
    }

    macro_rules! section {
        (heading= $heading:literal, $($item:expr),+ $(,)?) => {
            doc::Section {
                heading: $heading.to_string(),
                content: Box::new([
                    $($item, )+
                ]),
            }
            .into()
        };
    }

    macro_rules! subsection {
        (heading= $heading:literal, $($item:expr),+ $(,)?) => {
            doc::SubSection {
                heading: $heading.to_string(),
                content: Box::new([
                    $($item, )+
                ]),
            }
            .into()
        };
    }

    macro_rules! code {
        ($($line:literal),+ $(,)?) => {
            doc::Block::Code(
                concat!($($line, )+).to_string()
            ).into()
        };
    }

    macro_rules! sub_list {
        ($($item:expr),+ $(,)?) => {
            doc::ListItem::SubList(
                Box::new([$($item, )+])
            )
        }
    }

    macro_rules! list_text {
        ($($text_run:expr),+ $(,)?) => {
            doc::ListItem::Text(
                Box::new([$($text_run, )+])
            )
        };
    }

    trait ParseResultTestHelpers {
        fn expect_successful(self) -> Document;
        fn expect_failure(self) -> ParseError;
    }

    impl ParseResultTestHelpers for ParseResult<Document> {
        fn expect_successful(self) -> Document {
            match self {
                Ok(doc) => doc,
                Err(error) => {
                    eprintln!("{}", error);
                    panic!("parse unexpectedly failed")
                }
            }
        }

        fn expect_failure(self) -> ParseError {
            match self {
                Ok(doc) => {
                    eprintln!("{:#?}", doc);
                    panic!("parse unexpectedly succeeded");
                }
                Err(error) => error,
            }
        }
    }

    trait DocumentTestHelpers {
        fn assert_document_eq(self, expected: impl Into<Document>);
        fn assert_contents_eq(self, expected: Box<[doc::Element]>);
    }

    impl DocumentTestHelpers for Document {
        fn assert_document_eq(self, expected: impl Into<Document>) {
            let expected = expected.into();
            if self != expected {
                eprintln!("Actual:\n{:#?}", self);
                eprintln!("Expected:\n{:#?}", expected);
                panic!("Parsed document not what was expected")
            }
        }

        fn assert_contents_eq(self, expected: Box<[doc::Element]>) {
            if self.contents != expected {
                eprintln!("Actual:\n{:#?}", self.contents);
                eprintln!("Expected:\n{:#?}", expected);
                panic!("Parsed document contents not what was expected")
            }
        }
    }

    trait ParseErrorTestHelpers {
        fn assert_error_kind_eq(self, expected: ErrorKind);
        fn assert_token_position_eq(self, expected: Position);
        fn assert_token_name_eq(self, expected: TokenName);
        fn assert_token_lexeme_eq(self, expected: LexemeString);
    }

    impl ParseErrorTestHelpers for ParseError {
        fn assert_error_kind_eq(self, expected: ErrorKind) {
            let actual = self.kind;
            if actual != expected {
                eprintln!("Actual:\n{:#?}", actual);
                eprintln!("Expected:\n{:#?}", expected);
                panic!("Failure reason not what was expected")
            }
        }

        fn assert_token_position_eq(self, expected: Position) {
            let actual = self.token.position;
            if actual != expected {
                eprintln!("Actual:\n{:#?}", actual);
                eprintln!("Expected:\n{:#?}", expected);
                panic!("Failure position was not expected")
            }
        }

        fn assert_token_name_eq(self, expected: TokenName) {
            let actual = self.token.name;
            if actual != expected {
                eprintln!("Actual:\n{:#?}", actual);
                eprintln!("Expected:\n{:#?}", expected);
                panic!("Failed token name was not expected")
            }
        }

        fn assert_token_lexeme_eq(self, expected: LexemeString) {
            let actual = self.token.lexeme;
            if actual != expected {
                eprintln!("Actual:\n{:#?}", self.kind);
                eprintln!("Expected:\n{:#?}", expected);
                panic!("Failed token lexeme was not expected")
            }
        }
    }

    #[test]
    fn complete_doc_test() {
        let input = document_lines!(
            "@metadata",
            "id: 01.42",
            "",
            "/ Feline friendly flower arranging",
            "",
            "!info",
            "Did you know flower pots are for *more*",
            "than simply knocking on the floor?",
            "",
            "Opposable thumbs",
            "are useful?",
            "",
            "- Nose",
            "- Toes",
            "  - Big one",
            "  - Little one",
            "  - _Other_",
            "     one",
            "",
            "Yay!"
        );

        let expected = document! {
            title: "Feline friendly flower arranging",
            id: "01.42",
            contents: [
                info![paragraph![
                    text("Did you know flower pots are for "),
                    strong_text("more"),
                    text(" than simply knocking on the floor?"),
                ]],
                paragraph![text("Opposable thumbs are useful?")],
                list![
                    list_text![text("Nose")],
                    list_text![text("Toes")],
                    sub_list![
                        list_text![text("Big one")],
                        list_text![text("Little one")],
                        list_text![emphasised_text("Other"), text(" one")],
                    ]
                ],
                paragraph![text("Yay!")],
            ],
        };

        parse_str(input)
            .expect_successful()
            .assert_document_eq(expected);
    }

    #[test]
    fn one_line_paragraph() {
        let input = content_lines!("We like cats very much");

        let expected = paragraph![text("We like cats very much")];

        parse_str(input)
            .expect_successful()
            .assert_contents_eq(expected);
    }

    #[test]
    fn explicit_paragraph() {
        let input = content_lines!("#paragraph", "Cats go meeow!");

        let expected = paragraph![text("Cats go meeow!")];

        parse_str(input)
            .expect_successful()
            .assert_contents_eq(expected);
    }

    #[test]
    fn explicit_paragraph_with_block_break_before_text_is_rejected() {
        let input = content_lines!("#paragraph", "", "Cats go meeow!");

        let expected = ErrorKind::ExpectedToken(LineBreak::NAME);

        parse_str(input)
            .expect_failure()
            .assert_error_kind_eq(expected);
    }

    #[test]
    fn unknown_block_directive_is_rejected() {
        let input = content_lines!("#meowograph", "Meow?");

        let expected = ErrorKind::UnknownDirective("#meowograph".into());

        parse_str(input)
            .expect_failure()
            .assert_error_kind_eq(expected);
    }

    #[test]
    fn unknown_data_directive_is_rejected() {
        let input = content_lines!("@mrerps", "PurrRR!");

        let expected = ErrorKind::UnknownDirective("@mrerps".into());

        parse_str(input)
            .expect_failure()
            .assert_error_kind_eq(expected);
    }

    #[test]
    fn unknown_container_directive_is_rejected() {
        let input = content_lines!("!meeps", "Morps!");

        let expected = ErrorKind::UnknownDirective("!meeps".into());

        parse_str(input)
            .expect_failure()
            .assert_error_kind_eq(expected);
    }

    #[test]
    fn empty_block_name_is_rejected() {
        let input = content_lines!("#", "Hi");

        let expected = ErrorKind::UnknownDirective("#".into());

        parse_str(input)
            .expect_failure()
            .assert_error_kind_eq(expected);
    }

    #[test]
    fn block_without_new_line_is_rejected() {
        let input = "/Some doc\n\n#paragraph";

        let expected = ErrorKind::ExpectedToken(LineBreak::NAME);

        parse_str(input)
            .expect_failure()
            .assert_error_kind_eq(expected);
    }

    #[test]
    fn double_space() {
        let input = content_lines!("Nice  kitty!");

        let expected = paragraph![text("Nice kitty!")];

        parse_str(input)
            .expect_successful()
            .assert_contents_eq(expected);
    }

    #[test]
    fn trailing_new_line_is_ignored() {
        let input = content_lines!("Cats", "");

        let expected = paragraph![text("Cats")];

        parse_str(input)
            .expect_successful()
            .assert_contents_eq(expected);
    }

    #[test]
    fn space_then_trailing_new_line_is_ignored() {
        let input = content_lines!("Cats ", "");

        let expected = paragraph![text("Cats")];

        parse_str(input)
            .expect_successful()
            .assert_contents_eq(expected);
    }

    #[test]
    fn new_line_becomes_whitespace() {
        let input = content_lines!("Cats", "whiskers");

        let expected = paragraph![text("Cats whiskers")];

        parse_str(input)
            .expect_successful()
            .assert_contents_eq(expected);
    }

    #[test]
    fn new_line_becomes_whitespace_given_plain_then_styled() {
        let input = content_lines!("Cats", "*whiskers*");

        let expected = paragraph![text("Cats "), strong_text("whiskers"),];

        parse_str(input)
            .expect_successful()
            .assert_contents_eq(expected);
    }

    #[test]
    fn new_line_becomes_whitespace_given_plain_then_raw() {
        let input = content_lines!("Cats", "`nice whiskers`");

        let expected = paragraph![text("Cats "), raw_text("nice whiskers"),];

        parse_str(input)
            .expect_successful()
            .assert_contents_eq(expected);
    }

    #[test]
    fn new_line_with_extra_whitespace_collapses() {
        let input = content_lines!("Cats    ", "    whiskers");
        let expected = paragraph![text("Cats whiskers")];

        parse_str(input)
            .expect_successful()
            .assert_contents_eq(expected);
    }

    #[test]
    fn two_new_lines_become_blocks() {
        let input = content_lines!("Cats", "", "whiskers");

        let expected = contents![paragraph![text("Cats")], paragraph![text("whiskers")]];

        parse_str(input)
            .expect_successful()
            .assert_contents_eq(expected);
    }

    #[test]
    fn three_new_lines_becomes_blocks() {
        let input = content_lines!("Cats", "", "", "whiskers");

        let expected = contents![paragraph![text("Cats")], paragraph![text("whiskers")]];

        parse_str(input)
            .expect_successful()
            .assert_contents_eq(expected);
    }

    #[test]
    fn two_new_lines_with_whitespace_is_treated_as_blockbreak() {
        let input = content_lines!("Cats", "  ", "whiskers");

        let expected = contents![paragraph![text("Cats")], paragraph![text("whiskers")]];

        parse_str(input)
            .expect_successful()
            .assert_contents_eq(expected);
    }

    #[test]
    fn blockbreak_with_extra_whitespace() {
        let input = content_lines!(
            "The increadible and mostly unbelieveable case of the Cats  ",
            "    ",
            "  whiskers",
        );

        let expected = ErrorKind::UnexpectedBlockStart;

        parse_str(input)
            .expect_failure()
            .assert_error_kind_eq(expected);
    }

    #[test]
    fn missing_blockbreak_is_rejected() {
        let input = content_lines!("Cats can sometimes be", "#paragraph", "ever so surprising");

        let expected = ErrorKind::ExpectedToken(BlockBreak::NAME);

        parse_str(input)
            .expect_failure()
            .assert_error_kind_eq(expected);
    }

    #[test]
    fn escaped_char() {
        let input = content_lines!("\\A");

        let expected = paragraph![text("A")];

        parse_str(input)
            .expect_successful()
            .assert_contents_eq(expected);
    }

    #[test]
    fn escaped_underscore_in_markup() {
        let input = content_lines!("My cat does backflips \\_coolcat");

        let expected = paragraph![text("My cat does backflips _coolcat")];

        parse_str(input)
            .expect_successful()
            .assert_contents_eq(expected);
    }

    #[test]
    fn escaped_underscore() {
        let input = content_lines!("cat\\_case");

        let expected = paragraph![text("cat_case")];

        parse_str(input)
            .expect_successful()
            .assert_contents_eq(expected);
    }

    #[test]
    fn escaped_underscore_in_emphasis() {
        let input = content_lines!("_cat\\_case_");

        let expected = paragraph![emphasised_text("cat_case")];

        parse_str(input)
            .expect_successful()
            .assert_contents_eq(expected);
    }

    #[test]
    fn escaped_ignored_in_raw() {
        let input = content_lines!("`cat\\_case`");

        let expected = paragraph![raw_text("cat\\_case")];

        parse_str(input)
            .expect_successful()
            .assert_contents_eq(expected);
    }

    #[test]
    fn emphasised_words() {
        let input = content_lines!("We _totally adore_ them");

        let expected = paragraph![text("We "), emphasised_text("totally adore"), text(" them"),];

        parse_str(input)
            .expect_successful()
            .assert_contents_eq(expected);
    }

    #[test]
    fn emphasis_at_end_of_line() {
        let input = content_lines!("Cats like to _zoom_", "around");

        let expected = paragraph![
            text("Cats like to "),
            emphasised_text("zoom"),
            text(" around"),
        ];

        parse_str(input)
            .expect_successful()
            .assert_contents_eq(expected);
    }

    #[test]
    fn strong_words() {
        let input = content_lines!("I *need to pet that cat* right away.");

        let expected = paragraph![
            text("I "),
            strong_text("need to pet that cat"),
            text(" right away."),
        ];

        parse_str(input)
            .expect_successful()
            .assert_contents_eq(expected);
    }

    #[test]
    fn strong_mid_word() {
        let input = content_lines!("I said: mee*ooOOo*ww!");

        let expected = paragraph![text("I said: mee"), strong_text("ooOOo"), text("ww!"),];

        parse_str(input)
            .expect_successful()
            .assert_contents_eq(expected);
    }

    #[test]
    fn strong_over_two_lines() {
        let input = content_lines!("*me", "ow*");

        let expected = paragraph![strong_text("me ow")];

        parse_str(input)
            .expect_successful()
            .assert_contents_eq(expected);
    }

    #[test]
    fn strikethrough_words() {
        let input = content_lines!("Cats are ~ok i guess~ magnificant");

        let expected = paragraph![
            text("Cats are "),
            strikethrough_text("ok i guess"),
            text(" magnificant"),
        ];

        parse_str(input)
            .expect_successful()
            .assert_contents_eq(expected);
    }

    #[test]
    fn raw_words() {
        let input = content_lines!("Robot cat says `bleep bloop`!");

        let expected = paragraph![text("Robot cat says "), raw_text("bleep bloop"), text("!"),];

        parse_str(input)
            .expect_successful()
            .assert_contents_eq(expected);
    }

    #[test]
    fn raw_mid_word() {
        let input = content_lines!("Bl`eeee`p!");

        let expected = paragraph![text("Bl"), raw_text("eeee"), text("p!"),];

        parse_str(input)
            .expect_successful()
            .assert_contents_eq(expected);
    }

    #[test]
    fn underscore_in_raw() {
        let input = content_lines!("Set `PURR_LOUDLY` to true");

        let expected = paragraph![text("Set "), raw_text("PURR_LOUDLY"), text(" to true"),];

        parse_str(input)
            .expect_successful()
            .assert_contents_eq(expected);
    }

    #[test]
    fn extra_spaces_in_raw() {
        let input = content_lines!("`Keep your       distance`");

        let expected = paragraph![raw_text("Keep your       distance")];

        parse_str(input)
            .expect_successful()
            .assert_contents_eq(expected);
    }

    #[test]
    fn raw_over_two_lines() {
        let input = content_lines!("`Great", "cats`");

        let expected = paragraph![raw_text("Great cats")];

        parse_str(input)
            .expect_successful()
            .assert_contents_eq(expected);
    }

    #[test]
    fn strikethrough_over_two_lines() {
        let input = content_lines!("~Great", "dogs~");

        let expected = paragraph![strikethrough_text("Great dogs")];

        parse_str(input)
            .expect_successful()
            .assert_contents_eq(expected);
    }

    #[test]
    fn raw_leading_with_new_line() {
        let input = content_lines!("`", "Meow?`");

        let expected = paragraph![raw_text(" Meow?")];

        parse_str(input)
            .expect_successful()
            .assert_contents_eq(expected);
    }

    #[test]
    fn raw_trailing_with_new_line() {
        let input = content_lines!("`Meow", "`");

        let expected = paragraph![raw_text("Meow ")];

        parse_str(input)
            .expect_successful()
            .assert_contents_eq(expected);
    }

    #[test]
    fn raw_leading_with_space() {
        let input = content_lines!("` Meow`");

        let expected = paragraph![raw_text(" Meow")];

        parse_str(input)
            .expect_successful()
            .assert_contents_eq(expected);
    }

    #[test]
    fn raw_trailing_with_space() {
        let input = content_lines!("`Meow `");

        let expected = paragraph![raw_text("Meow ")];

        parse_str(input)
            .expect_successful()
            .assert_contents_eq(expected);
    }

    #[test]
    fn raw_over_three_lines() {
        let input = content_lines!("`Great", "cats", "assemble!`");

        let expected = paragraph![raw_text("Great cats assemble!")];

        parse_str(input)
            .expect_successful()
            .assert_contents_eq(expected);
    }

    #[test]
    fn standalone_dash() {
        let input = content_lines!("Felines - fantastic!");

        let expected = paragraph![text("Felines - fantastic!")];

        parse_str(input)
            .expect_successful()
            .assert_contents_eq(expected);
    }

    #[test]
    fn paragraph_with_trailing_whitespace() {
        let input = content_lines!("Cool kitty   ");

        let expected = paragraph![text("Cool kitty")];

        parse_str(input)
            .expect_successful()
            .assert_contents_eq(expected);
    }

    #[test]
    fn underscore_in_awkward_places() {
        let input = content_lines!("Cat cat_cat cat_ cat.");

        let expected = paragraph![text("Cat cat"), emphasised_text("cat cat"), text(" cat.")];

        parse_str(input)
            .expect_successful()
            .assert_contents_eq(expected);
    }

    #[test]
    fn new_line_then_multiple_spaces_in_plain_text() {
        let input = content_lines!("Cat", "  cat");

        let expected = paragraph![text("Cat cat")];

        parse_str(input)
            .expect_successful()
            .assert_contents_eq(expected);
    }

    #[test]
    fn new_line_then_multiple_spaces_in_styled() {
        let input = content_lines!("*Cat", "  cat*");

        let expected = paragraph![strong_text("Cat cat")];

        parse_str(input)
            .expect_successful()
            .assert_contents_eq(expected);
    }

    #[test]
    fn new_line_then_multiple_spaces_in_raw() {
        let input = content_lines!("`Cat", "  cat`");

        let expected = paragraph![raw_text("Cat   cat")];

        parse_str(input)
            .expect_successful()
            .assert_contents_eq(expected);
    }

    #[test]
    fn multiple_spaces_then_new_line_in_plain_text() {
        let input = content_lines!("Cat  ", "cat");

        let expected = paragraph![text("Cat cat")];

        parse_str(input)
            .expect_successful()
            .assert_contents_eq(expected);
    }

    #[test]
    fn multiple_spaces_then_new_line_in_styled() {
        let input = content_lines!("*Cat  ", "cat*");

        let expected = paragraph![strong_text("Cat cat")];

        parse_str(input)
            .expect_successful()
            .assert_contents_eq(expected);
    }

    #[test]
    fn multiple_spaces_then_new_line_in_raw() {
        let input = content_lines!("`Cat  ", "cat`");

        let expected = paragraph![raw_text("Cat   cat")];

        parse_str(input)
            .expect_successful()
            .assert_contents_eq(expected);
    }

    #[test]
    fn empty_emphasis() {
        let input = content_lines!("Rules cats must follow: __.");

        let expected = ErrorKind::UnexpectedTextRunStart;

        parse_str(input)
            .expect_failure()
            .assert_error_kind_eq(expected);
    }

    #[test]
    fn empty_raw() {
        let input = content_lines!("Robot cat says: ``!.");

        let expected = ErrorKind::UnexpectedRawTextRunStart;

        parse_str(input)
            .expect_failure()
            .assert_error_kind_eq(expected);
    }

    #[test]
    fn raw_with_double_linebreak() {
        let input = content_lines!("`Erm...", "", "meow?`");

        let expected = ErrorKind::ExpectedToken(RawDelimiter::NAME);

        parse_str(input)
            .expect_failure()
            .assert_error_kind_eq(expected);
    }

    #[test]
    fn raw_with_double_linebreak_containing_whitespace() {
        let input = content_lines!("`Erm...", "  ", "meow?`");

        let expected = ErrorKind::ExpectedToken(RawDelimiter::NAME);

        parse_str(input)
            .expect_failure()
            .assert_error_kind_eq(expected);
    }

    #[test]
    fn strikethrough_with_double_linebreak() {
        let input = content_lines!("~Erm...", "", "meow?~");

        let expected = ErrorKind::ExpectedToken(StrikethroughDelimiter::NAME);

        parse_str(input)
            .expect_failure()
            .assert_error_kind_eq(expected);
    }

    #[test]
    fn unmatched_emphasis_1() {
        let input = content_lines!("_.");

        let expected = ErrorKind::ExpectedToken(EmphasisDelimiter::NAME);

        parse_str(input)
            .expect_failure()
            .assert_error_kind_eq(expected);
    }

    #[test]
    fn unmatched_emphasis_2() {
        let input = content_lines!("meow _meow.");

        let expected = ErrorKind::ExpectedToken(EmphasisDelimiter::NAME);

        parse_str(input)
            .expect_failure()
            .assert_error_kind_eq(expected);
    }

    #[test]
    fn unmatched_emphasis_3() {
        let input = content_lines!("meow meow_");

        let expected = ErrorKind::UnexpectedTextRunStart;

        parse_str(input)
            .expect_failure()
            .assert_error_kind_eq(expected);
    }

    #[test]
    fn nested_styled_text() {
        let input = content_lines!("_*meow!*_");

        let expected = ErrorKind::UnexpectedTextRunStart;

        parse_str(input)
            .expect_failure()
            .assert_error_kind_eq(expected);
    }

    #[test]
    fn loose_strong_delimiter_start() {
        let input = content_lines!("* meow meow*");

        let expected = ErrorKind::LooseDelimiter;

        parse_str(input)
            .expect_failure()
            .assert_error_kind_eq(expected);
    }

    #[test]
    fn loose_strong_delimiter_end() {
        let input = content_lines!("*meow meow *");

        let expected = ErrorKind::LooseDelimiter;

        parse_str(input)
            .expect_failure()
            .assert_error_kind_eq(expected);
    }

    #[test]
    fn raw_immediately_in_emphasis() {
        let input = content_lines!("_``_");

        let expected = ErrorKind::UnexpectedTextRunStart;

        parse_str(input)
            .expect_failure()
            .assert_error_kind_eq(expected);
    }

    #[test]
    fn raw_within_in_emphasis() {
        let input = content_lines!("_a``a_");

        let expected = ErrorKind::ExpectedToken(EmphasisDelimiter::NAME);

        parse_str(input)
            .expect_failure()
            .assert_error_kind_eq(expected);
    }

    #[test]
    fn doc_with_leading_new_line() {
        let input = content_lines!("", "Cats cats cats");

        let expected = paragraph![text("Cats cats cats")];

        parse_str(input)
            .expect_successful()
            .assert_contents_eq(expected);
    }

    #[test]
    fn doc_with_leading_new_lines() {
        let input = content_lines!("", "", "Cats cats cats");

        let expected = paragraph![text("Cats cats cats")];

        parse_str(input)
            .expect_successful()
            .assert_contents_eq(expected);
    }

    #[test]
    fn doc_with_leading_spaces_and_new_line() {
        let input = content_lines!("   ", "Cats cats cats");

        let expected = paragraph![text("Cats cats cats")];

        parse_str(input)
            .expect_successful()
            .assert_contents_eq(expected);
    }

    #[test]
    fn doc_ending_with_new_line() {
        let input = content_lines!("Cats are friends");

        let expected = paragraph![text("Cats are friends")];

        parse_str(input)
            .expect_successful()
            .assert_contents_eq(expected);
    }

    #[test]
    fn doc_ending_with_new_lines() {
        let input = content_lines!("Feline friends", "");

        let expected = paragraph![text("Feline friends")];

        parse_str(input)
            .expect_successful()
            .assert_contents_eq(expected);
    }

    #[test]
    fn new_line_and_space_between_styled_and_plain_text_runs() {
        let input = content_lines!("*Cat*", " cat");

        let expected = paragraph![strong_text("Cat"), text(" cat"),];

        parse_str(input)
            .expect_successful()
            .assert_contents_eq(expected);
    }

    #[test]
    fn leading_whitespace_on_paragraph_is_ignored() {
        let input = content_lines!("Cat", "", "  cat");

        let expected = ErrorKind::UnexpectedBlockStart;

        parse_str(input)
            .expect_failure()
            .assert_error_kind_eq(expected);
    }

    #[test]
    fn doc_metadata() {
        let input = document_lines!(
            "@metadata",
            "id: 12.03",
            "",
            "/ Some document with metadata",
            "",
        );

        let expected = document! {
            title: "Some document with metadata",
            id: "12.03",
        };

        parse_str(input)
            .expect_successful()
            .assert_document_eq(expected);
    }

    #[test]
    fn doc_metadata_with_tags() {
        let input = document_lines!(
            "@metadata",
            "id: feline.feasts.25",
            "tags: cooking | eating | nice-smells",
            "",
            "/ Document with metadata",
            "",
        );

        let expected = document! {
            title: "Document with metadata",
            id: "feline.feasts.25",
            tags: ["cooking", "eating", "nice-smells"],
        };

        parse_str(input)
            .expect_successful()
            .assert_document_eq(expected);
    }

    #[test]
    fn doc_metadata_with_unknown_identifier_is_rejected() {
        let input = document_lines!(
            "@metadata",
            "kibble: yes please",
            "",
            "/ Doc with weird metadata"
        );

        let expected = ErrorKind::UnknownMetadata("kibble".into());

        parse_str(input)
            .expect_failure()
            .assert_error_kind_eq(expected);
    }

    #[test]
    fn doc_metadata_not_at_start_is_rejected() {
        let input = document_lines!(
            "/ Some title",
            "",
            "Helloo there. Metadata should not follow this.!",
            "",
            "@metadata",
            "id: 01.23"
        );

        let expected = ErrorKind::MetadataNotAtStart;

        parse_str(input)
            .expect_failure()
            .assert_error_kind_eq(expected);
    }

    #[test]
    fn doc_title() {
        let input = "/ Practical espionage for felines in urban settings";

        let expected = document! {
            title: "Practical espionage for felines in urban settings",
        };

        parse_str(input)
            .expect_successful()
            .assert_document_eq(expected);
    }

    #[test]
    fn doc_title_with_folowing_para() {
        let input = document_lines!(
            "/ Some Doc",
            "",
            "",
            "",
            "",
            "Why hello there cats and kittens"
        );

        let expected = document! {
            title: "Some Doc",
            contents: [
                paragraph![text("Why hello there cats and kittens")]
            ],
        };

        parse_str(input)
            .expect_successful()
            .assert_document_eq(expected);
    }

    #[test]
    fn doc_title_with_wonky_spacing() {
        let input = document_lines!("/My Very   Cool Document   ", "");

        let expected = document! {
            title: "My Very Cool Document",
        };

        parse_str(input)
            .expect_successful()
            .assert_document_eq(expected);
    }

    #[test]
    fn doc_title_with_no_trailing_newline() {
        let input = "/Some Doc";

        let expected = document! {
            title: "Some Doc",
        };

        parse_str(input)
            .expect_successful()
            .assert_document_eq(expected);
    }

    #[test]
    fn doc_title_in_not_at_start() {
        let input = document_lines!(
            "Document should not be after this!",
            "",
            "/Some Document Title"
        );

        let expected = ErrorKind::ExpectedToken(TitleDirective::NAME);

        parse_str(input)
            .expect_failure()
            .assert_error_kind_eq(expected);
    }

    #[test]
    fn doc_title_in_section() {
        let input = document_lines!(
            "/ Some Document Title",
            "",
            "// Some important document section",
            "",
            "/ Other Document Title"
        );

        let expected = ErrorKind::UnexpectedBlockStart;

        parse_str(input)
            .expect_failure()
            .assert_error_kind_eq(expected);
    }

    #[test]
    fn doc_title_in_sub_section() {
        let input = document_lines!(
            "/ Some Document Title",
            "",
            "// Some important document section",
            "",
            "/// Some important document sub section",
            "",
            "/Some Document Title",
            "",
            "Sup",
        );

        let expected = ErrorKind::UnexpectedBlockStart;

        parse_str(input)
            .expect_failure()
            .assert_error_kind_eq(expected);
    }

    #[test]
    fn empty_doc_title() {
        let input = document_lines!("/");

        let expected = ErrorKind::UnexpectedHeaderTextStart;

        parse_str(input)
            .expect_failure()
            .assert_error_kind_eq(expected);
    }

    #[test]
    fn subsection_missing_parent() {
        let input = document_lines!(
            "/Some Document Title",
            "",
            "/// Some important document sub section",
        );

        let expected = ErrorKind::SubSectionNotNested;

        parse_str(input)
            .expect_failure()
            .assert_error_kind_eq(expected);
    }

    #[test]
    fn multi_paragraph_info() {
        let input = content_lines!(
            "!info",
            ">>>",
            "Here are some facts...",
            "",
            "...about the cats!",
            "<<<",
        );

        let expected = info![
            paragraph![text("Here are some facts...")],
            paragraph![text("...about the cats!")]
        ];

        parse_str(input)
            .expect_successful()
            .assert_contents_eq(expected);
    }

    #[test]
    fn single_paragraph_info() {
        let input = content_lines!(
            "!info",
            "Did you know that cats sometimes like a nice long massage",
            "",
        );

        let expected = info![paragraph![text(
            "Did you know that cats sometimes like a nice long massage"
        )]];

        parse_str(input)
            .expect_successful()
            .assert_contents_eq(expected);
    }

    #[test]
    fn container_missing_start_is_rejected() {
        let input = content_lines!("Silly cat", "<<<");

        let expected = ErrorKind::ContainerMissingStart;

        parse_str(input)
            .expect_failure()
            .assert_error_kind_eq(expected);
    }

    #[test]
    fn empty_container_is_rejected() {
        let input = content_lines!("!info", "");

        let expected = ErrorKind::EmptyContainer;

        parse_str(input)
            .expect_failure()
            .assert_error_kind_eq(expected);
    }

    #[test]
    fn detactched_container_is_rejected() {
        let input = content_lines!("!info", "", "cats!");

        let expected = ErrorKind::EmptyContainer;

        parse_str(input)
            .expect_failure()
            .assert_error_kind_eq(expected);
    }

    #[test]
    fn trailing_text_on_delimited_start_is_rejected() {
        let input = content_lines!(
            "!info",
            ">>>squeek",
            "Let me know if you find where I left my",
            "<<<",
            ""
        );

        let expected = ErrorKind::ExpectedToken(LineBreak::NAME);

        parse_str(input)
            .expect_failure()
            .assert_error_kind_eq(expected);
    }

    #[test]
    fn trailing_text_on_delimited_end_is_rejected() {
        let input = content_lines!(
            "!info",
            ">>>",
            "Let me know if you find where I left my",
            "<<<toy",
            ""
        );

        let expected = ErrorKind::ExpectedToken(BlockBreak::NAME);

        parse_str(input)
            .expect_failure()
            .assert_error_kind_eq(expected);
    }

    #[test]
    fn missing_blockbreak_after_container_is_rejected() {
        let input = content_lines!(
            "!info",
            ">>>",
            "Let me know if you find where I left my",
            "<<<",
            "toy"
        );

        let expected = ErrorKind::ExpectedToken(BlockBreak::NAME);

        parse_str(input)
            .expect_failure()
            .assert_error_kind_eq(expected);
    }

    #[test]
    fn dash_in_paragraph_is_treated_as_part_of_text() {
        let input = content_lines!("Ripley\n- Cat");

        let expected = paragraph![text("Ripley - Cat")];

        parse_str(input)
            .expect_successful()
            .assert_contents_eq(expected);
    }

    #[test]
    fn indented_dash_in_paragraph_is_treated_as_part_of_text() {
        let input = content_lines!("Ripley\n  - Cat");

        let expected = paragraph![text("Ripley - Cat")];

        parse_str(input)
            .expect_successful()
            .assert_contents_eq(expected);
    }

    #[test]
    fn simple_list() {
        let input = content_lines!(
            "- Dry food is ok",
            "- Wet food is much better",
            "- Water is important also"
        );

        let expected = list![
            list_text![text("Dry food is ok")],
            list_text![text("Wet food is much better")],
            list_text![text("Water is important also")]
        ];

        parse_str(input)
            .expect_successful()
            .assert_contents_eq(expected);
    }

    #[test]
    fn explicit_list() {
        let input = content_lines!(
            "#list",
            "- Dry food is ok",
            "- Wet food is much better",
            "- Water is important also"
        );

        let expected = list![
            list_text![text("Dry food is ok")],
            list_text![text("Wet food is much better")],
            list_text![text("Water is important also")]
        ];

        parse_str(input)
            .expect_successful()
            .assert_contents_eq(expected);
    }

    #[test]
    fn ordered_list() {
        let input = content_lines!(
            "#list(style=ordered)",
            "- Dry food is ok",
            "- Wet food is much better",
            "- Water is important also"
        );

        let expected = ordered_list![
            list_text![text("Dry food is ok")],
            list_text![text("Wet food is much better")],
            list_text![text("Water is important also")]
        ];

        parse_str(input)
            .expect_successful()
            .assert_contents_eq(expected);
    }

    #[test]
    fn list_with_invalid_style() {
        let input = content_lines!(
            "#list(style=cool)\n",
            "- Dont you think this list is really rather neat?\n",
        );

        let expected = ErrorKind::InvalidListStyle("cool".into());

        parse_str(input)
            .expect_failure()
            .assert_error_kind_eq(expected);
    }

    #[test]
    fn list_with_invalid_parameter() {
        let input = content_lines!(
            "#list(up=down)\n",
            "- Dont you think this list is really rather odd?\n",
        );

        let expected = ErrorKind::InvalidListParameter("up".into());

        parse_str(input)
            .expect_failure()
            .assert_error_kind_eq(expected);
    }

    #[test]
    fn dash_in_list_text_is_not_treated_as_bullet() {
        let input = content_lines!("- Meow - meow\n",);

        let expected = list![list_text![text("Meow - meow")]];

        parse_str(input)
            .expect_successful()
            .assert_contents_eq(expected);
    }

    #[test]
    fn simple_list_with_continuations() {
        let input = content_lines!(
            "- Dry food",
            "is ok",
            "- Wet food",
            "  is much better",
            "- Water is",
            "    important also"
        );

        let expected = list![
            list_text![text("Dry food is ok")],
            list_text![text("Wet food is much better")],
            list_text![text("Water is important also")],
        ];

        parse_str(input)
            .expect_successful()
            .assert_contents_eq(expected);
    }

    #[test]
    fn list_with_styled_text() {
        let input = content_lines!(
            "- Dry food is *ok*",
            "- Wet food is _much better_",
            "- Water is `important  also`"
        );

        let expected = list![
            list_text![text("Dry food is "), strong_text("ok")],
            list_text![text("Wet food is "), emphasised_text("much better")],
            list_text![text("Water is "), raw_text("important  also")]
        ];

        parse_str(input)
            .expect_successful()
            .assert_contents_eq(expected);
    }

    #[test]
    fn list_with_sublist() {
        let input = content_lines!(
            "- Nice things to eat",
            "  - Tuna",
            "  - Chicken",
            "  - Beef",
        );

        let expected = list![
            list_text![text("Nice things to eat")],
            sub_list![
                list_text![text("Tuna")],
                list_text![text("Chicken")],
                list_text![text("Beef")],
            ]
        ];

        parse_str(input)
            .expect_successful()
            .assert_contents_eq(expected);
    }

    #[test]
    fn list_with_subsublist() {
        let input = content_lines!(
            "- Nice things to eat",
            "  - Beef",
            "    - Hereford",
            "    - Wagyu",
        );

        let expected = list![
            list_text![text("Nice things to eat")],
            sub_list![
                list_text![text("Beef")],
                sub_list![list_text![text("Hereford")], list_text![text("Wagyu")],]
            ]
        ];

        parse_str(input)
            .expect_successful()
            .assert_contents_eq(expected);
    }

    #[test]
    fn list_with_raw_over_newline() {
        let input = content_lines!("- f`oo", "  ba`r", "  - baz");

        let expected = list![
            list_text![text("f"), raw_text("oo   ba"), text("r"),],
            sub_list![list_text![text("baz")]]
        ];

        parse_str(input)
            .expect_successful()
            .assert_contents_eq(expected);
    }

    #[test]
    fn list_item_with_trailing_whitespace() {
        let input = content_lines!("- Foo    ", "- Bar");

        let expected = list![list_text![text("Foo")], list_text![text("Bar")],];

        parse_str(input)
            .expect_successful()
            .assert_contents_eq(expected);
    }

    #[test]
    fn list_with_raw_over_multiple_points() {
        let input = content_lines!("- f`oo", "  -ba`r");

        let expected = list![list_text![text("f"), raw_text("oo   -ba"), text("r"),]];

        parse_str(input)
            .expect_successful()
            .assert_contents_eq(expected);
    }

    #[test]
    fn list_with_emphasis_over_multiple_points() {
        let input = content_lines!("- f_oo", "  -ba_r");

        let expected = ErrorKind::ExpectedToken(EmphasisDelimiter::NAME);

        parse_str(input)
            .expect_failure()
            .assert_error_kind_eq(expected);
    }

    #[test]
    fn list_with_uneven_spaces() {
        let input = content_lines!("-foo", " -bar");

        let expected = ErrorKind::UnevenListIndent(Indent { space_count: 1 });

        parse_str(input)
            .expect_failure()
            .assert_error_kind_eq(expected);
    }

    #[test]
    fn list_that_skips_ascending_indent_level() {
        let input = content_lines!(
            "- Nice things to eat",
            "    - Wagyu beef because it is oh so tender",
        );

        let expected = ErrorKind::MissingListLevel { from: 0, to: 2 };

        parse_str(input)
            .expect_failure()
            .assert_error_kind_eq(expected);
    }

    #[test]
    fn list_that_skips_decending_indent_level() {
        let input = content_lines!(
            "- Nice things to eat",
            "  - Beef",
            "    - Wagyu",
            "- Nice things to drink",
        );

        let expected = list![
            list_text![text("Nice things to eat")],
            sub_list![
                list_text![text("Beef")],
                sub_list![list_text![text("Wagyu")]]
            ],
            list_text![text("Nice things to drink")]
        ];

        parse_str(input)
            .expect_successful()
            .assert_contents_eq(expected);
    }

    #[test]
    fn error_specifies_correct_span() {
        let input = document_lines!(
            "/ Document with an silly cat error",
            "",
            "Silly cat",
            "goes *_*"
        );

        let expected = Position {
            column: 6,
            row: 3,
            index: 52,
        };

        parse_str(input)
            .expect_failure()
            .assert_token_position_eq(expected);
    }

    #[test]
    fn error_includes_token_name() {
        let input = document_lines!(
            "/ Document with an silly cat error",
            "",
            "Silly cat",
            "goes *_*"
        );

        let expected = EmphasisDelimiter::NAME;

        parse_str(input)
            .expect_failure()
            .assert_token_name_eq(expected);
    }

    #[test]
    fn error_includes_token_lexeme() {
        let input = document_lines!(
            "/ Document with an silly cat error\n",
            "\n",
            "Silly cat\n",
            "goes *_*"
        );

        let expected = "_".into();

        parse_str(input)
            .expect_failure()
            .assert_token_lexeme_eq(expected);
    }

    #[test]
    fn link_with_reference() {
        let input = document_lines!(
            "@references",
            "ripley_2020: https://example.com",
            "",
            "/ Cat petting tips",
            "",
            "For more info, consult [our guide on petting cats]@ripley_2020,",
            "created by our own in house experts.",
        );

        let expected = document! {
            title: "Cat petting tips",
            references: [("ripley_2020", "https://example.com")],
            contents: [
                paragraph![
                    text("For more info, consult "),
                    linked_text("our guide on petting cats", "ripley_2020"),
                    text(", created by our own in house experts.")
                ]
            ],
        };

        parse_str(input)
            .expect_successful()
            .assert_document_eq(expected);
    }

    #[test]
    fn link_with_following_space() {
        let input = document_lines!(
            "@references",
            "some_ref: https://example.com",
            "",
            "/ Some doc",
            "",
            "See [our guide]@some_ref for more",
        );

        let expected = document! {
            title: "Some doc",
            references: [("some_ref", "https://example.com")],
            contents: [
                paragraph![
                    text("See "),
                    linked_text("our guide", "some_ref"),
                    text(" for more")
                ]
            ],
        };

        parse_str(input)
            .expect_successful()
            .assert_document_eq(expected);
    }

    #[test]
    fn references_with_odd_spacing() {
        let input = document_lines!(
            "@references",
            "ripley_2020: https://example.com/a",
            "ripley_2021:https://example.com/b",
            "ripley_2022 :https://example.com/c",
            "ripley_2023 : https://example.com/d",
            "ripley_2024: https://example.com/e  ",
            "",
            "/ Doc with lots of references",
        );

        let expected = document! {
            title: "Doc with lots of references",
            references: [
                ("ripley_2020", "https://example.com/a"),
                ("ripley_2021", "https://example.com/b"),
                ("ripley_2022", "https://example.com/c"),
                ("ripley_2023", "https://example.com/d"),
                ("ripley_2024", "https://example.com/e"),
            ],
        };

        parse_str(input)
            .expect_successful()
            .assert_document_eq(expected);
    }

    #[test]
    fn at_sign_can_be_used_normally() {
        let input = content_lines!("C@ts are great @ that");

        let expected = paragraph![text("C@ts are great @ that")];

        parse_str(input)
            .expect_successful()
            .assert_contents_eq(expected);
    }

    #[test]
    fn whitespace_around_linked_text_is_rejected() {
        let input = document_lines!(
            "@references",
            "ripley_2020: https://example.com",
            "",
            "/ Some Title",
            "",
            "We like [ petting cats ]@ripley_2020 a lot.",
        );

        let expected = ErrorKind::LooseDelimiter;

        parse_str(input)
            .expect_failure()
            .assert_error_kind_eq(expected);
    }

    #[test]
    fn references_after_content_rejected() {
        let input = document_lines!(
            "/ Some Title\n",
            "\n",
            "For more info, consult [our guide on petting cats]@ripley_2020,\n",
            "created by our own in house experts.\n",
            "\n",
            "@references\n",
            "ripley_2020: https://example.com"
        );

        let expected = ErrorKind::ReferencesOutOfPlace;

        parse_str(input)
            .expect_failure()
            .assert_error_kind_eq(expected);
    }

    #[test]
    fn document_with_sections() {
        let input = document_lines!(
            "/Speed running the kitchen at 4am",
            "",
            "This is a comprehensive guide.",
            "",
            "// Motivation",
            "",
            "Set a personal best,",
            "while others rest!",
            "",
            "// Planning the perfect lap",
            "",
            "This requires care.",
            "",
            "/// Selecting a route",
            "",
            "Avoid the toaster.",
            "",
            "/// Choosing a victory scream",
            "",
            "",
            "",
            "Meeaaahhh?",
            "",
            "// Conclusion and reflections",
            "",
            "Go go go!"
        );

        let expected = document! {
            title: "Speed running the kitchen at 4am",
            contents: [
                paragraph![text("This is a comprehensive guide.")],
                section![
                    heading = "Motivation",
                    paragraph![text("Set a personal best, while others rest!")],
                ],
                section![
                    heading = "Planning the perfect lap",
                    paragraph![text("This requires care.")],
                    subsection![
                        heading = "Selecting a route",
                        paragraph![text("Avoid the toaster.")],
                    ],
                    subsection![
                        heading = "Choosing a victory scream",
                        paragraph![text("Meeaaahhh?")],
                    ],
                ],
                section![
                    heading = "Conclusion and reflections",
                    paragraph![text("Go go go!")],
                ],
            ],
        };

        parse_str(input)
            .expect_successful()
            .assert_document_eq(expected);
    }

    #[test]
    fn section_with_trailing_space_in_title() {
        let input = document_lines!(
            "/Speed running the kitchen at 4am \n",
            "\n",
            "This is a comprehensive guide.\n",
        );

        let expected = document! {
            title: "Speed running the kitchen at 4am",
            contents: [
                paragraph![
                    text("This is a comprehensive guide.")
                ]
            ],
        };

        parse_str(input)
            .expect_successful()
            .assert_document_eq(expected);
    }

    #[test]
    fn code_block() {
        let input = content_lines!(
            "#code",
            "---",
            "Meow?",
            "",
            "Meow.",
            "Me...           ...ow.",
            "Meow!",
            "---"
        );

        let expected = code![
            "Meow?\n",
            "\n",
            "Meow.\n",
            "Me...           ...ow.\n",
            "Meow!\n",
        ];

        parse_str(input)
            .expect_successful()
            .assert_contents_eq(expected);
    }

    #[test]
    fn code_block_then_paragraph() {
        let input = content_lines!(
            "#code",
            "---",
            "Meow? Purrr purrr purrr!",
            "---",
            "",
            "Hey, whats up?"
        );

        let expected = contents![
            code!["Meow? Purrr purrr purrr!\n",],
            paragraph![text("Hey, whats up?")],
        ];

        parse_str(input)
            .expect_successful()
            .assert_contents_eq(expected);
    }
}
