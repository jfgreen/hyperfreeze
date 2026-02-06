use std::backtrace::{Backtrace, BacktraceStatus};
use std::fmt::Display;

use crate::document::*;
use crate::tokenise::*;

use Token::*;

#[derive(Debug)]
pub struct ParseError {
    kind: ErrorKind,
    input_column: u32,
    input_line: u32,
    backtrace: Backtrace,
}

macro_rules! parse_err {
    //TODO: Try track_caller stuff
    ($error:expr, $position:expr) => {{
        Err(ParseError {
            kind: $error,
            //TODO: can we bake the +1 into token position?
            input_column: $position.column + 1,
            input_line: $position.row + 1,
            backtrace: Backtrace::capture(),
        })
    }};
}

//TODO: Replace UnexpectedToken with more specific errors
#[derive(PartialEq, Eq, Debug)]
enum ErrorKind {
    LooseDelimiter,
    EmptyDelimitedText,
    MissingListLevel((usize, usize)),
    TitleNotAtStart,
    MetadataNotAtStart,
    ReferencesOutOfPlace,
    UnexpectedToken(String),
    UnknownMetadata(String),
    UnevenListIndent(usize),
    UnknownBlock(String),
    ContainerMissingStart,
    EmptyContainer,
}

impl Display for ParseError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        writeln!(
            f,
            "parsing error on line {} column {}",
            self.input_line, self.input_column
        )?;

        match &self.kind {
            LooseDelimiter => write!(f, "delimited text cant have leading/trailing whitespace"),
            EmptyDelimitedText => write!(f, "delimited text cant be empty"),
            UnknownMetadata(key) => write!(f, "unknown metadata '{}'", key),
            MissingListLevel((from, to)) => {
                write!(f, "list indent skipped from {} to {}", from, to)
            }
            TitleNotAtStart => write!(f, "document title is not at start of document"),
            MetadataNotAtStart => write!(f, "document metadata is not at start of document"),
            ReferencesOutOfPlace => write!(f, "references not in at start of document"),
            UnexpectedToken(token) => write!(f, "unexpected token, {}", token),
            UnevenListIndent(count) => write!(f, "list indent of {} is not even", count),
            UnknownBlock(name) => write!(f, "unknown block '{}'", name),
            ContainerMissingStart => write!(f, "delimited container end with no preceeding start"),
            EmptyContainer => write!(f, "empty container,"),
        }?;

        if self.backtrace.status() == BacktraceStatus::Captured {
            writeln!(f)?;
            writeln!(f, "Parse backtrace:")?;
            writeln!(f, "{}", self.backtrace)?;
        }

        Ok(())
    }
}

type ParseResult<T> = Result<T, ParseError>;

use ErrorKind::*;

const SPACE: char = ' ';

//TODO: Think, what is the minimal, cohesive set of macros we can use to parse?

// TODO: Repetition here could be cleared up?
macro_rules! consume {
    ($tokeniser:expr, $variant:pat) => {{
        let token = $tokeniser.current();
        match token {
            $variant => {
                $tokeniser.advance();
                Ok(())
            }
            _ => {
                let expected = stringify!($variant);
                let message = format!("expected: {expected}, got: {token}");
                parse_err!(UnexpectedToken(message), $tokeniser.position())
            }
        }
    }};
}

macro_rules! consume_value {
    ($tokeniser:expr, $variant:ident) => {{
        let token = $tokeniser.current();
        match token {
            $variant(value) => {
                $tokeniser.advance();
                Ok(value)
            }
            _ => {
                let expected = stringify!($variant);
                let message = format!("expected: {expected}, got: {token}");
                parse_err!(UnexpectedToken(message), $tokeniser.position())
            }
        }
    }};
}

// TODO: Do we need this?
// Will position even be reliably correct?
macro_rules! unexpected_token_err {
    ($tokeniser: expr) => {{
        let value = $tokeniser.current();
        let message = format!("{} was not expected here", value);
        parse_err!(UnexpectedToken(message), $tokeniser.position())
    }};

    ($tokeniser: expr, $expected: expr) => {{
        let value = $tokeniser.current();
        let expected = format!($expected);
        let message = format!("{expected}, got: {value}");
        parse_err!(UnexpectedToken(message), $tokeniser.position())
    }};
}

// TODO: Ensure containers can not hold sections
// TODO: Test for doc that ends with escape '\'

pub fn parse_str(input: &str) -> Result<Document, ParseError> {
    let tokeniser = &mut Tokeniser::new(input);
    tokeniser.advance();
    parse_document(tokeniser)
}

fn parse_document(tokeniser: &mut Tokeniser) -> ParseResult<Document> {
    let mut elements = Vec::new();

    let mut title = None;
    let mut metadata = Metadata::default();
    let mut references = Vec::new();

    if tokeniser.current() == StructuredDataDirective("metadata") {
        parse_metadata(tokeniser, &mut metadata)?;
    }

    if tokeniser.current() == StructuredDataDirective("references") {
        let refs = parse_references(tokeniser)?;
        references.extend(refs);
    }

    if tokeniser.current() == TitleDirective {
        let heading = parse_document_title(tokeniser)?;
        title = Some(heading);
    }

    while tokeniser.current() != EndOfInput {
        let element = parse_element(tokeniser)?;
        elements.push(element);
    }

    Ok(Document {
        title,
        metadata,
        contents: elements.into_boxed_slice(),
        references: references.into_boxed_slice(),
    })
}

fn parse_document_title(tokeniser: &mut Tokeniser) -> ParseResult<String> {
    consume!(tokeniser, TitleDirective)?;
    let title = parse_header_text(tokeniser)?;

    if tokeniser.current() != EndOfInput {
        consume!(tokeniser, BlockBreak)?;
    }

    Ok(title)
}

fn parse_metadata(tokeniser: &mut Tokeniser, metadata: &mut Metadata) -> ParseResult<()> {
    consume!(tokeniser, StructuredDataDirective("metadata"))?;
    consume!(tokeniser, LineBreak)?;

    //TODO: this could be a while let? Would need a new macro?
    while matches!(tokeniser.current(), DataIdentifier(_)) {
        let key_position = tokeniser.position();
        let key = consume_value!(tokeniser, DataIdentifier)?;

        consume!(tokeniser, DataKeyValueSeperator)?;

        match key {
            "id" => {
                let id = consume_value!(tokeniser, DataValue)?;
                metadata.id = Some(id.to_string());
            }
            "tags" => {
                let tags = parse_metadata_list(tokeniser)?;
                metadata.tags = Some(tags);
            }
            _ => return parse_err!(UnknownMetadata(key.to_string()), key_position),
        };

        if !matches!(tokeniser.current(), EndOfInput | BlockBreak) {
            consume!(tokeniser, LineBreak)?;
        }
    }

    if tokeniser.current() != EndOfInput {
        consume!(tokeniser, BlockBreak)?;
    }

    Ok(())
}

//TODO: parse_references and parse_metadata _should_ share mechanics
fn parse_references(tokeniser: &mut Tokeniser) -> ParseResult<Box<[Reference]>> {
    consume!(tokeniser, StructuredDataDirective("references"))?;
    consume!(tokeniser, LineBreak)?;

    let mut references = Vec::new();

    while let DataIdentifier(id) = tokeniser.current() {
        tokeniser.advance();
        consume!(tokeniser, DataKeyValueSeperator)?;
        let link = consume_value!(tokeniser, DataValue)?;

        // TODO: This is a common pattern - macro? Or refactor to not
        // need these delimiter tokens?
        if !matches!(tokeniser.current(), EndOfInput | BlockBreak) {
            consume!(tokeniser, LineBreak)?;
        }

        references.push(Reference {
            id: id.into(),
            link: link.into(),
        });
    }

    if tokeniser.current() != EndOfInput {
        consume!(tokeniser, BlockBreak)?;
    }

    let references = references.into_boxed_slice();
    Ok(references)
}

fn parse_header_text(tokeniser: &mut Tokeniser) -> ParseResult<String> {
    let mut title = String::new();

    loop {
        match tokeniser.current() {
            TitleText(word) => {
                title.push_str(word);
            }
            TitleTextSpace => {
                title.push(SPACE);
            }
            _ => break,
        }
        tokeniser.advance();
    }

    Ok(title)
}

fn parse_metadata_list(tokeniser: &mut Tokeniser) -> ParseResult<Box<[String]>> {
    let mut tags = Vec::new();

    let first_tag = consume_value!(tokeniser, DataValue)?;
    tags.push(first_tag.to_string());

    while matches!(tokeniser.current(), DataListSeperator) {
        tokeniser.advance();
        let tag = consume_value!(tokeniser, DataValue)?;
        tags.push(tag.to_string());
    }

    let tags = tags.into_boxed_slice();
    Ok(tags)
}

fn parse_element(tokeniser: &mut Tokeniser) -> ParseResult<Element> {
    match tokeniser.current() {
        TitleDirective => {
            parse_err!(TitleNotAtStart, tokeniser.position())
        }
        SectionDirective => {
            let section = parse_section(tokeniser)?;
            Ok(Element::Section(section))
        }
        ContainerDirective(_) => {
            let container = parse_container(tokeniser)?;
            Ok(Element::Container(container))
        }
        SubSectionDirective => {
            todo!("reject subsection for not being inside a section");
        }
        _ => {
            let block = parse_block(tokeniser)?;
            Ok(Element::Block(block))
        }
    }
}

// TODO: Ensure containers can not hold sections
fn parse_container(tokeniser: &mut Tokeniser) -> ParseResult<Container> {
    consume!(tokeniser, ContainerDirective("info"))?;

    if tokeniser.current() == BlockBreak {
        return parse_err!(EmptyContainer, tokeniser.position());
    }

    consume!(tokeniser, LineBreak)?;

    if tokeniser.current() == EndOfInput {
        return parse_err!(EmptyContainer, tokeniser.position());
    }

    //TODO: Support other kinds of container
    let container_kind = ContainerKind::Info;

    let mut blocks = Vec::new();

    if tokeniser.current() == DelimitedContainerStart {
        tokeniser.advance();

        consume!(tokeniser, LineBreak)?;

        if tokeniser.current() == DelimitedContainerEnd {
            return parse_err!(EmptyContainer, tokeniser.position());
        }

        while tokeniser.current() != DelimitedContainerEnd {
            let block = parse_block(tokeniser)?;
            blocks.push(block);
        }

        consume!(tokeniser, DelimitedContainerEnd)?;
        if tokeniser.current() != EndOfInput {
            consume!(tokeniser, BlockBreak)?;
        }
    } else {
        let block = parse_block(tokeniser)?;
        blocks.push(block);
    }

    let container = Container {
        content: blocks.into_boxed_slice(),
        kind: container_kind,
    };

    Ok(container)
}

fn parse_section(tokeniser: &mut Tokeniser) -> ParseResult<Section> {
    consume!(tokeniser, SectionDirective)?;
    let name = parse_header_text(tokeniser)?;
    consume!(tokeniser, BlockBreak)?;

    let mut elements = Vec::new();

    while !matches!(tokeniser.current(), EndOfInput | SectionDirective) {
        let element = parse_section_element(tokeniser)?;
        elements.push(element);
    }

    let section = Section {
        content: elements.into_boxed_slice(),
        heading: name.to_string(),
    };

    Ok(section)
}

fn parse_section_element(tokeniser: &mut Tokeniser) -> ParseResult<SectionElement> {
    //TODO: Very simmilar to parse_element, possible re-use?
    match tokeniser.current() {
        TitleDirective => {
            parse_err!(TitleNotAtStart, tokeniser.position())
        }
        SubSectionDirective => {
            let subsection = parse_subsection(tokeniser)?;
            Ok(SectionElement::SubSection(subsection))
        }
        ContainerDirective(_) => {
            let container = parse_container(tokeniser)?;
            Ok(SectionElement::Container(container))
        }
        _ => {
            let block = parse_block(tokeniser)?;
            Ok(SectionElement::Block(block))
        }
    }
}

fn parse_subsection(tokeniser: &mut Tokeniser) -> ParseResult<SubSection> {
    consume!(tokeniser, SubSectionDirective)?;
    let name = parse_header_text(tokeniser)?;
    consume!(tokeniser, BlockBreak)?;

    let mut elements = Vec::new();

    while !matches!(
        tokeniser.current(),
        EndOfInput | SectionDirective | SubSectionDirective
    ) {
        let element = parse_subsection_element(tokeniser)?;
        elements.push(element);
    }

    let subsection = SubSection {
        content: elements.into_boxed_slice(),
        heading: name.to_string(),
    };

    Ok(subsection)
}

fn parse_subsection_element(tokeniser: &mut Tokeniser) -> ParseResult<SubSectionElement> {
    match tokeniser.current() {
        // TODO: Test for this?
        // TitleDirective => {
        //     parse_err!(TitleNotAtStart, tokeniser.position())
        // }
        SubSectionDirective => {
            todo!("not allow")
        }
        ContainerDirective(_) => {
            let container = parse_container(tokeniser)?;
            Ok(SubSectionElement::Container(container))
        }
        _ => {
            let block = parse_block(tokeniser)?;
            Ok(SubSectionElement::Block(block))
        }
    }
}

fn parse_block(tokeniser: &mut Tokeniser) -> ParseResult<Block> {
    let block = match tokeniser.current() {
        ListBullet(_) => parse_list(tokeniser)?,
        BlockDirective("paragraph") => parse_paragraph(tokeniser)?,
        BlockDirective("list") => parse_list(tokeniser)?,
        BlockDirective("code") => parse_code(tokeniser)?,
        StructuredDataDirective("metadata") => {
            return parse_err!(MetadataNotAtStart, tokeniser.position());
        }
        StructuredDataDirective("references") => {
            return parse_err!(ReferencesOutOfPlace, tokeniser.position());
        }
        BlockDirective(unknown) => {
            return parse_err!(UnknownBlock(unknown.into()), tokeniser.position());
        }
        //TODO: Easier way to have 'is markup'
        MarkupText(_)
        | MarkupTextSpace
        | LinkOpeningDelimiter
        | RawDelimiter
        | EmphasisDelimiter
        | StrongDelimiter
        | StrikethroughDelimiter => parse_paragraph(tokeniser)?,
        DelimitedContainerEnd => return parse_err!(ContainerMissingStart, tokeniser.position()),
        _ => return unexpected_token_err!(tokeniser),
    };

    if tokeniser.current() == LineBreak {
        // Trailing linebreak before EOF
        tokeniser.advance();
    } else if tokeniser.current() != EndOfInput && tokeniser.current() != DelimitedContainerEnd {
        consume!(tokeniser, BlockBreak)?;
    }

    Ok(block)
}

fn parse_paragraph(tokeniser: &mut Tokeniser) -> ParseResult<Block> {
    if tokeniser.current() == BlockDirective("paragraph") {
        tokeniser.advance();
        consume!(tokeniser, LineBreak)?;
    }

    if tokeniser.current() == MarkupTextSpace {
        tokeniser.advance();
    }

    let text_runs = parse_text_runs(tokeniser)?;
    Ok(Block::Paragraph(text_runs))
}

fn parse_list_level(
    tokeniser: &mut Tokeniser,
    current_depth: usize,
) -> ParseResult<Box<[ListItem]>> {
    let mut items = Vec::new();

    while let ListBullet(indent_count) = tokeniser.current() {
        if indent_count % 2 != 0 {
            return parse_err!(UnevenListIndent(indent_count), tokeniser.position());
        }

        let depth = indent_count / 2;

        let item = if depth == current_depth {
            tokeniser.advance();
            let text = parse_text_runs(tokeniser)?;
            if tokeniser.current() == LineBreak {
                tokeniser.advance();
            }
            ListItem::Text(text)
        } else if depth == current_depth + 1 {
            let sub_items = parse_list_level(tokeniser, depth)?;
            ListItem::SubList(sub_items)
        } else if depth < current_depth {
            break;
        } else {
            return parse_err!(
                MissingListLevel((current_depth, depth)),
                tokeniser.position()
            );
        };

        items.push(item);
    }

    let items = items.into_boxed_slice();
    Ok(items)
}

//TODO: support some kind of term (aka definiton) list
fn parse_list(tokeniser: &mut Tokeniser) -> ParseResult<Block> {
    let mut style = ListStyle::Unordered;

    if tokeniser.current() == BlockDirective("list") {
        tokeniser.advance();
        if tokeniser.current() == BlockParametersStart {
            tokeniser.advance();
            while matches!(tokeniser.current(), BlockParameterName(_)) {
                let name = consume_value!(tokeniser, BlockParameterName)?;
                consume!(tokeniser, BlockParameterNameValueSeperator)?;
                let value = consume_value!(tokeniser, BlockParameterValue)?;
                match name {
                    "style" => {
                        style = match value {
                            "ordered" => ListStyle::Ordered,
                            "unordered" => ListStyle::Unordered,
                            _ => todo!(),
                        }
                    }
                    _ => todo!(),
                }
            }
            consume!(tokeniser, BlockParametersEnd)?;
        }
        consume!(tokeniser, LineBreak)?;
    }

    let base_depth = 0;
    let items = parse_list_level(tokeniser, base_depth)?;
    let list = List { items, style };
    Ok(Block::List(list))
}

fn parse_text_runs(tokeniser: &mut Tokeniser) -> ParseResult<Box<[TextRun]>> {
    let mut text_runs = Vec::new();

    while !matches!(
        tokeniser.current(),
        BlockBreak | EndOfInput | ListBullet(_) | DelimitedContainerEnd | LineBreak
    ) {
        let run = match tokeniser.current() {
            MarkupText(_) | MarkupTextSpace => parse_plain_text_run(tokeniser)?,
            //TODO: Could group style delimiters into sub enum
            StrongDelimiter | EmphasisDelimiter | StrikethroughDelimiter => {
                parse_styled_text_run(tokeniser)?
            }
            RawDelimiter => parse_raw_text_run(tokeniser)?,
            LinkOpeningDelimiter => parse_linked_text_run(tokeniser)?,

            _ => return unexpected_token_err!(tokeniser),
        };
        text_runs.push(run);
    }

    let text_runs = text_runs.into_boxed_slice();
    Ok(text_runs)
}

fn parse_plain_text_run(tokeniser: &mut Tokeniser) -> ParseResult<TextRun> {
    let run = parse_markup_text(tokeniser)?;

    let run = TextRun {
        text: run,
        style: Style::None,
    };

    Ok(run)
}

fn parse_styled_text_run(tokeniser: &mut Tokeniser) -> ParseResult<TextRun> {
    let opening_delimiter = tokeniser.current();
    let run_start = tokeniser.position();
    tokeniser.advance();

    let style = match opening_delimiter {
        StrongDelimiter => Style::Strong,
        EmphasisDelimiter => Style::Emphasis,
        StrikethroughDelimiter => Style::Strikethrough,
        _ => return unexpected_token_err!(tokeniser),
    };

    let run = parse_markup_text(tokeniser)?;

    if tokeniser.current() != opening_delimiter {
        return unexpected_token_err!(tokeniser, "expected: {opening_delimiter}");
    }

    tokeniser.advance();

    if run.starts_with(SPACE) || run.ends_with(SPACE) {
        return parse_err!(LooseDelimiter, run_start);
    }

    if run.is_empty() {
        return parse_err!(EmptyDelimitedText, run_start);
    }

    let run = TextRun { text: run, style };

    Ok(run)
}

fn parse_raw_text_run(tokeniser: &mut Tokeniser) -> ParseResult<TextRun> {
    let run_start = tokeniser.position();
    consume!(tokeniser, RawDelimiter)?;

    let mut run = String::new();

    while tokeniser.current() != RawDelimiter {
        match tokeniser.current() {
            RawFragment(fragment) => {
                run.push_str(fragment);
            }
            LineBreak => {
                run.push(SPACE);
            }
            RawDelimiter => break,
            _ => return unexpected_token_err!(tokeniser),
        }
        tokeniser.advance();
    }

    tokeniser.advance();

    if run.is_empty() {
        return parse_err!(EmptyDelimitedText, run_start);
    }

    let run = TextRun {
        text: run,
        style: Style::Raw,
    };

    Ok(run)
}

fn parse_linked_text_run(tokeniser: &mut Tokeniser) -> ParseResult<TextRun> {
    let start_of_run = tokeniser.position();
    consume!(tokeniser, LinkOpeningDelimiter)?;
    let run = parse_markup_text(tokeniser)?;

    if run.starts_with(SPACE) || run.ends_with(SPACE) {
        return parse_err!(LooseDelimiter, start_of_run);
    }

    consume!(tokeniser, LinkClosingDelimiter)?;
    consume!(tokeniser, LinkToReferenceJoiner)?;
    let identifier = consume_value!(tokeniser, DataIdentifier)?;

    Ok(TextRun {
        text: run,
        style: Style::Link(identifier.into()),
    })
}

fn parse_markup_text(tokeniser: &mut Tokeniser) -> ParseResult<String> {
    let mut run = String::new();

    // TODO: Sometimes we loop untill we get an explicit end token
    //   while !matches(tokensier, Foo | Bar)
    // Other times we loop while we have a token we like
    //   loop { match ..., _ => break }
    // We should be consistent
    // and... if we like the loop break pattern, macro-ify it
    loop {
        match tokeniser.current() {
            MarkupText(text) => {
                run.push_str(text);
            }
            MarkupTextSpace => {
                run.push(SPACE);
            }
            _ => break,
        }
        tokeniser.advance();
    }

    Ok(run)
}

fn parse_code(tokeniser: &mut Tokeniser) -> ParseResult<Block> {
    consume!(tokeniser, BlockDirective("code"))?;
    consume!(tokeniser, LineBreak)?;
    consume!(tokeniser, CodeDelimiter)?;
    consume!(tokeniser, LineBreak)?;
    let code = consume_value!(tokeniser, Code)?;
    consume!(tokeniser, CodeDelimiter)?;

    let block = Block::Code(String::from(code));
    Ok(block)
}

//TODO: Allow ignorable indenting on delimited content
// e.g
// !info
// >>>
//   Some cats are actually quite naughty.
//
//   Yes.
//
//   #code
//   ---
//   def meow():
//     print("MEooowww!")
//   ---
// <<<

//TODO: Implement a concat with auto newlines macro?
//TODO: test for leading whitespace like '    / Hello'
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
            Some(String::from($value))
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
            Metadata{
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
                [$(Reference {
                    id: $ref_id.to_string(),
                    link: $ref_link.to_string(),
                },)*]
            )
        };
    }

    //TODO: Less confusing for macros to be more like (info: {...}) => {...}
    macro_rules! element {
        (info $( $block:ident { $($content:tt)* } $(,)? )*) => {
            Element::Container(Container{
                content: Box::new([
                    $(
                        block!($block $($content)*),
                    )*
                ]),
                kind: ContainerKind::Info,
            })
        };

        (section ($name:expr) $( $element:ident $(($element_name:expr))? { $($content:tt)* } $(,)? )*) => {
            Element::Section(Section{
                content: Box::new([
                    $(
                        section_element!($element $(($element_name))? $($content)*),
                    )*
                ]),
                heading: String::from($name)
            })
        };

        ($block:ident $($content:tt)*) => {
            Element::Block(block!($block $($content)*))
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
            SectionElement::SubSection(SubSection{
                content: Box::new([
                    $(
                        subsection_element!($element $(($element_name))? $($content)*),
                    )*
                ]),
                heading: String::from($name)
            })
        };

        ($block:ident $($content:tt)*) => {
            SectionElement::Block(block!($block $($content)*))
        };
    }

    macro_rules! subsection_element {
        (info $( $block:ident { $($content:tt)* } $(,)? )*) => {
            SubSectionElement::Container(Container{
                content: Box::new([
                    $(
                        block!($block $($content)*),
                    )*
                ]),
                kind: ContainerKind::Info,
            })
        };

        ($block:ident $($content:tt)*) => {
            SubSectionElement::Block(block!($block $($content)*))
        };

    }

    macro_rules! block {
        (paragraph $($text:expr),* $(,)?) => {
            Block::Paragraph(Box::new([
                $(
                    $text,
                )*
            ]))
        };

        (list $($item:ident { $($content:tt)* } $(,)?)*) => {
            Block::List(
                List {
                    items: Box::new([
                    $(
                        list_item!($item $($content)*),
                    )*
                    ]),
                    style: ListStyle::Unordered,
                }
            )
        };

        (ordered_list $($item:ident { $($content:tt)* } $(,)?)*) => {
            Block::List(
                List {
                    items: Box::new([
                    $(
                        list_item!($item $($content)*),
                    )*
                    ]),
                    style: ListStyle::Ordered,
                }
            )
        };

        (code $($text:expr),+ $(,)?) => {
            Block::Code(concat!($($text,)+).to_string())
        };
    }

    macro_rules! list_item {
        (paragraph $($text:expr),* $(,)?) => {
            ListItem::Text(Box::new([
                $(
                    $text,
                )*
            ]))
        };

        (list $($item:ident { $($content:tt)* } $(,)?)*) => {
            ListItem::SubList(Box::new([
                $(
                    list_item!($item $($content)*),
                )*
            ]))
        };
    }

    macro_rules! info {
        ($($content:tt)*) => {
            Document {
                contents: Box::new([element!(info $($content)*)]),
                ..Default::default()
            }
        }
    }

    macro_rules! list {
        ($($content:tt)*) => {
            Document {
                contents: Box::new([element!(list $($content)*)]),
                ..Default::default()
            }
        }
    }

    macro_rules! ordered_list {
        ($($content:tt)*) => {
            Document {
                contents: Box::new([element!(ordered_list $($content)*)]),
                ..Default::default()
            }
        }
    }

    macro_rules! paragraph {
        ($($content:tt)*) => {
            Document {
                contents: Box::new([element!(paragraph $($content)*)]),
                ..Default::default()
            }
        }
    }

    macro_rules! code {
        ($($content:tt)*) => {
            Document {
                contents: Box::new([element!(code $($content)*)]),
                ..Default::default()
            }
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
            Document {
                contents: Box::new(
                    [$(element!(
                        $element_type
                        $(($element_name))?
                        $($element_content)*
                    ),)*]
                ),
                ..Default::default()
            }
        }
    }

    fn text(text: &str) -> TextRun {
        TextRun {
            text: text.to_string(),
            style: Style::None,
        }
    }

    fn emphasised_text(text: &str) -> TextRun {
        TextRun {
            text: text.to_string(),
            style: Style::Emphasis,
        }
    }

    fn strong_text(text: &str) -> TextRun {
        TextRun {
            text: text.to_string(),
            style: Style::Strong,
        }
    }

    fn strikethrough_text(text: &str) -> TextRun {
        TextRun {
            text: text.to_string(),
            style: Style::Strikethrough,
        }
    }

    fn raw_text(text: &str) -> TextRun {
        TextRun {
            text: text.to_string(),
            style: Style::Raw,
        }
    }

    fn linked_text(text: &str, reference: &str) -> TextRun {
        TextRun {
            text: text.to_string(),
            style: Style::Link(reference.to_string()),
        }
    }

    fn assert_parse_succeeds<T: Into<Document>>(input: &'static str, expected: T) {
        let expected = expected.into();
        let result = parse_str(input);

        match result {
            Ok(doc) => {
                if doc != expected {
                    eprintln!("Actual:\n{:#?}", doc);
                    eprintln!("Expected:\n{:#?}", expected);
                    panic!("Parsed document not what was expected")
                }
            }
            Err(error) => {
                eprintln!("{}", error);
                panic!("parse unexpectedly failed")
            }
        }
    }

    fn assert_parse_fails(input: &'static str, expected: ErrorKind) {
        let result = parse_str(input);

        match result {
            Ok(doc) => {
                eprintln!("Expected parse to fail, but got doc:");
                eprintln!("{:?}\n", doc);
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

        assert_parse_succeeds(input, expected);
    }

    #[test]
    fn one_line_paragraph() {
        let input = "We like cats very much";

        let expected = paragraph! { text("We like cats very much") };

        assert_parse_succeeds(input, expected);
    }

    #[test]
    fn explicit_paragraph() {
        let input = "#paragraph\nCats go meeow!";

        let expected = paragraph! { text("Cats go meeow!") };

        assert_parse_succeeds(input, expected);
    }

    #[test]
    fn explicit_paragraph_with_block_break_before_text_is_rejected() {
        let input = "#paragraph\n\nCats go meeow!";

        let expected = UnexpectedToken("expected: LineBreak, got: block break".into());

        assert_parse_fails(input, expected);
    }

    #[test]
    fn unknown_block_is_rejected() {
        let input = "#meowograph\nMeow?";

        let expected = UnknownBlock("meowograph".into());

        assert_parse_fails(input, expected);
    }

    #[test]
    fn empty_block_name_is_rejected() {
        let input = "#\nHi";

        //TODO: Maybe a specific error for empty block name would be better?
        let expected = UnknownBlock("".into());

        assert_parse_fails(input, expected);
    }

    #[test]
    fn block_without_new_line_is_rejected() {
        let input = "#paragraph";

        let expected = UnexpectedToken("expected: LineBreak, got: end of input".into());

        assert_parse_fails(input, expected);
    }

    #[test]
    fn double_space() {
        let input = "Nice  kitty!";

        let expected = paragraph! { text("Nice kitty!") };

        assert_parse_succeeds(input, expected);
    }

    #[test]
    fn trailing_new_line_is_ignored() {
        let input = "Cats\n";

        let expected = paragraph! { text("Cats") };

        assert_parse_succeeds(input, expected);
    }

    #[test]
    fn space_then_trailing_new_line_is_ignored() {
        let input = "Cats \n";

        let expected = paragraph! { text("Cats") };

        assert_parse_succeeds(input, expected);
    }

    #[test]
    fn new_line_becomes_whitespace() {
        let input = "Cats\nwhiskers";

        let expected = paragraph! { text("Cats whiskers") };

        assert_parse_succeeds(input, expected);
    }

    #[test]
    fn new_line_becomes_whitespace_given_plain_then_styled() {
        let input = "Cats\n*whiskers*";

        let expected = paragraph! {
            text("Cats "),
            strong_text("whiskers"),
        };

        assert_parse_succeeds(input, expected);
    }

    #[test]
    fn new_line_becomes_whitespace_given_plain_then_raw() {
        let input = "Cats\n`nice whiskers`";

        let expected = paragraph! {
            text("Cats "),
            raw_text("nice whiskers"),
        };

        assert_parse_succeeds(input, expected);
    }

    #[test]
    fn new_line_with_extra_whitespace_collapses() {
        let input = "Cats    \n    whiskers";
        let expected = paragraph! { text("Cats whiskers") };

        assert_parse_succeeds(input, expected);
    }

    #[test]
    fn two_new_lines_become_blocks() {
        let input = "Cats\n\nwhiskers";

        let expected = elements! {
            paragraph { text("Cats") },
            paragraph { text("whiskers") }
        };

        assert_parse_succeeds(input, expected);
    }

    #[test]
    fn three_new_lines_becomes_blocks() {
        let input = "Cats\n\n\nwhiskers";

        let expected = elements! {
            paragraph { text("Cats") },
            paragraph { text("whiskers") }
        };

        assert_parse_succeeds(input, expected);
    }

    #[test]
    fn two_new_lines_with_whitespace_is_treated_as_blockbreak() {
        let input = "Cats\n \nwhiskers";

        let expected = elements! {
            paragraph { text("Cats") },
            paragraph { text("whiskers") }
        };

        assert_parse_succeeds(input, expected);
    }

    #[test]
    fn blockbreak_with_extra_whitespace() {
        let input = "Cats  \n    \n  whiskers";
        let expected = elements!(
            paragraph { text("Cats") },
            paragraph { text("whiskers") }
        );

        assert_parse_succeeds(input, expected);
    }

    #[test]
    fn missing_blockbreak_is_rejected() {
        let input = concat!(
            "Cats can sometimes be\n",
            "#paragraph\n",
            "ever so surprising\n"
        );

        // TODO: This is a bit meh
        // Would be better to have a missing blockbreak error or simmilar
        // Tokeniser should also not treat the # as completely unknown
        // Or... we allow this and have it treated as text?
        let expected = UnexpectedToken("unknown '#paragraph' was not expected here".into());

        assert_parse_fails(input, expected);
    }

    #[test]
    fn escaped_char() {
        let input = "\\A";

        let expected = paragraph! { text("A") };

        assert_parse_succeeds(input, expected);
    }

    #[test]
    fn escaped_underscore_in_markup() {
        let input = "My cat does backflips \\_coolcat";

        let expected = paragraph! { text("My cat does backflips _coolcat") };

        assert_parse_succeeds(input, expected);
    }

    #[test]
    fn escaped_underscore() {
        let input = "cat\\_case";

        let expected = paragraph! { text("cat_case") };

        assert_parse_succeeds(input, expected);
    }

    #[test]
    fn escaped_underscore_in_emphasis() {
        let input = "_cat\\_case_";

        let expected = paragraph! { emphasised_text("cat_case") };

        assert_parse_succeeds(input, expected);
    }

    #[test]
    fn escaped_ignored_in_raw() {
        let input = "`cat\\_case`";

        let expected = paragraph! { raw_text("cat\\_case") };

        assert_parse_succeeds(input, expected);
    }

    #[test]
    fn emphasised_words() {
        let input = "We _totally adore_ them";

        let expected = paragraph! {
            text("We "),
            emphasised_text("totally adore"),
            text(" them"),
        };

        assert_parse_succeeds(input, expected);
    }

    #[test]
    fn emphasis_at_end_of_line() {
        let input = "Cats like to _zoom_\naround";

        let expected = paragraph! {
            text("Cats like to "),
            emphasised_text("zoom"),
            text(" around"),
        };

        assert_parse_succeeds(input, expected);
    }

    #[test]
    fn strong_words() {
        let input = "I *need to pet that cat* right away.";

        let expected = paragraph! {
            text("I "),
            strong_text("need to pet that cat"),
            text(" right away."),
        };

        assert_parse_succeeds(input, expected);
    }

    #[test]
    fn strong_mid_word() {
        let input = "I said: mee*ooOOo*ww!";

        let expected = paragraph! {
            text("I said: mee"),
            strong_text("ooOOo"),
            text("ww!"),
        };

        assert_parse_succeeds(input, expected);
    }

    #[test]
    fn strong_over_two_lines() {
        let input = "*me\now*";

        let expected = paragraph! { strong_text("me ow") };

        assert_parse_succeeds(input, expected);
    }

    #[test]
    fn strikethrough_words() {
        let input = "Cats are ~ok i guess~ magnificant";

        let expected = paragraph! {
            text("Cats are "),
            strikethrough_text("ok i guess"),
            text(" magnificant"),
        };

        assert_parse_succeeds(input, expected);
    }

    #[test]
    fn raw_words() {
        let input = "Robot cat says `bleep bloop`!";

        let expected = paragraph! {
            text("Robot cat says "),
            raw_text("bleep bloop"),
            text("!"),
        };

        assert_parse_succeeds(input, expected);
    }

    #[test]
    fn raw_mid_word() {
        let input = "Bl`eeee`p!";

        let expected = paragraph! {
            text("Bl"),
            raw_text("eeee"),
            text("p!"),
        };

        assert_parse_succeeds(input, expected);
    }

    #[test]
    fn underscore_in_raw() {
        let input = "Set `PURR_LOUDLY` to true";

        let expected = paragraph! {
            text("Set "),
            raw_text("PURR_LOUDLY"),
            text(" to true"),
        };

        assert_parse_succeeds(input, expected);
    }

    #[test]
    fn extra_spaces_in_raw() {
        let input = "`Keep your       distance`";

        let expected = paragraph! { raw_text("Keep your       distance") };

        assert_parse_succeeds(input, expected);
    }

    #[test]
    fn raw_over_two_lines() {
        let input = "`Great\ncats`";

        let expected = paragraph! { raw_text("Great cats") };

        assert_parse_succeeds(input, expected);
    }

    #[test]
    fn strikethrough_over_two_lines() {
        let input = "~Great\ndogs~";

        let expected = paragraph! { strikethrough_text("Great dogs") };

        assert_parse_succeeds(input, expected);
    }

    #[test]
    fn raw_leading_with_new_line() {
        let input = "`\nMeow?`";

        let expected = paragraph! { raw_text(" Meow?") };

        assert_parse_succeeds(input, expected);
    }

    #[test]
    fn raw_trailing_with_new_line() {
        let input = "`Meow\n`";

        let expected = paragraph! { raw_text("Meow ") };

        assert_parse_succeeds(input, expected);
    }

    #[test]
    fn raw_leading_with_space() {
        let input = "` Meow`";

        let expected = paragraph! { raw_text(" Meow") };

        assert_parse_succeeds(input, expected);
    }

    #[test]
    fn raw_trailing_with_space() {
        let input = "`Meow `";

        let expected = paragraph! { raw_text("Meow ") };

        assert_parse_succeeds(input, expected);
    }

    #[test]
    fn raw_over_three_lines() {
        let input = "`Great\ncats\nassemble!`";

        let expected = paragraph! { raw_text("Great cats assemble!") };

        assert_parse_succeeds(input, expected);
    }

    #[test]
    fn standalone_dash() {
        let input = "Felines - fantastic!";

        let expected = paragraph! { text("Felines - fantastic!") };

        assert_parse_succeeds(input, expected);
    }

    #[test]
    fn paragraph_with_trailing_whitespace() {
        let input = "Cool kitty   ";

        let expected = paragraph! { text("Cool kitty") };

        assert_parse_succeeds(input, expected);
    }

    #[test]
    fn underscore_in_awkward_places() {
        let input = "Cat cat_cat cat_ cat.";

        let expected = paragraph! {
            text("Cat cat"),
            emphasised_text("cat cat"),
            text(" cat.")
        };

        assert_parse_succeeds(input, expected);
    }

    #[test]
    fn new_line_then_multiple_spaces_in_plain_text() {
        let input = "Cat\n  cat";

        let expected = paragraph! { text("Cat cat") };

        assert_parse_succeeds(input, expected);
    }

    #[test]
    fn new_line_then_multiple_spaces_in_styled() {
        let input = "*Cat\n  cat*";

        let expected = paragraph! { strong_text("Cat cat") };

        assert_parse_succeeds(input, expected);
    }

    #[test]
    fn new_line_then_multiple_spaces_in_raw() {
        let input = "`Cat\n  cat`";

        let expected = paragraph! { raw_text("Cat   cat") };

        assert_parse_succeeds(input, expected);
    }

    #[test]
    fn multiple_spaces_then_new_line_in_plain_text() {
        let input = "Cat  \ncat";

        let expected = paragraph! { text("Cat cat") };

        assert_parse_succeeds(input, expected);
    }

    #[test]
    fn multiple_spaces_then_new_line_in_styled() {
        let input = "*Cat  \ncat*";

        let expected = paragraph! { strong_text("Cat cat") };

        assert_parse_succeeds(input, expected);
    }

    #[test]
    fn multiple_spaces_then_new_line_in_raw() {
        let input = "`Cat  \ncat`";

        let expected = paragraph! { raw_text("Cat   cat") };

        assert_parse_succeeds(input, expected);
    }

    #[test]
    fn empty_emphasis() {
        let input = "Rules cats must follow: __.";

        let expected = EmptyDelimitedText;

        assert_parse_fails(input, expected);
    }

    #[test]
    fn empty_raw() {
        let input = "Robot cat says: ``!.";

        let expected = EmptyDelimitedText;

        assert_parse_fails(input, expected);
    }

    #[test]
    fn raw_with_double_linebreak() {
        let input = "`Erm...\n\nmeow?`";

        let expected = UnexpectedToken("block break was not expected here".to_string());

        assert_parse_fails(input, expected);
    }

    #[test]
    fn raw_with_double_linebreak_containing_whitespace() {
        let input = "`Erm...\n \nmeow?`";

        let expected = UnexpectedToken("block break was not expected here".to_string());

        assert_parse_fails(input, expected);
    }

    #[test]
    fn strikethrough_with_double_linebreak() {
        let input = "~Erm...\n\nmeow?~";

        //TODO: Would be much better to have a specific error for unterminated style delimiter
        let expected =
            UnexpectedToken("expected: strikethrough delimiter, got: block break".into());

        assert_parse_fails(input, expected);
    }

    #[test]
    fn unmatched_emphasis_1() {
        let input = "_.";

        let expected =
            UnexpectedToken("expected: emphasis delimiter, got: end of input".to_string());

        assert_parse_fails(input, expected);
    }

    #[test]
    fn unmatched_emphasis_2() {
        let input = "meow _meow.";

        let expected =
            UnexpectedToken("expected: emphasis delimiter, got: end of input".to_string());

        assert_parse_fails(input, expected);
    }

    #[test]
    fn unmatched_emphasis_3() {
        let input = "meow meow_";

        let expected =
            UnexpectedToken("expected: emphasis delimiter, got: end of input".to_string());

        assert_parse_fails(input, expected);
    }

    #[test]
    fn nested_styled_text() {
        let input = "_*meow!*_";

        //TODO: We should have an explicit error for nested styled text
        let expected =
            UnexpectedToken("expected: emphasis delimiter, got: strong delimiter".into());

        assert_parse_fails(input, expected);
    }

    #[test]
    fn loose_strong_delimiter_start() {
        let input = "* meow meow*";

        let expected = LooseDelimiter;

        assert_parse_fails(input, expected);
    }

    #[test]
    fn loose_strong_delimiter_end() {
        let input = "*meow meow *";

        let expected = LooseDelimiter;

        assert_parse_fails(input, expected);
    }

    #[test]
    fn raw_immediately_in_emphasis() {
        let input = "_``_";

        let expected =
            UnexpectedToken("expected: emphasis delimiter, got: raw delimiter".to_string());

        assert_parse_fails(input, expected);
    }

    #[test]
    fn raw_within_in_emphasis() {
        let input = "_a``a_";

        let expected =
            UnexpectedToken("expected: emphasis delimiter, got: raw delimiter".to_string());

        assert_parse_fails(input, expected);
    }

    #[test]
    fn doc_with_leading_new_line() {
        let input = "\nCats cats cats";

        let expected = paragraph!(text("Cats cats cats"));

        assert_parse_succeeds(input, expected);
    }

    #[test]
    fn doc_with_leading_new_lines() {
        let input = "\n\nCats cats cats";

        let expected = paragraph!(text("Cats cats cats"));

        assert_parse_succeeds(input, expected);
    }

    #[test]
    fn doc_with_leading_spaces_and_new_line() {
        let input = "   \nCats cats cats";

        let expected = paragraph!(text("Cats cats cats"));

        assert_parse_succeeds(input, expected);
    }

    #[test]
    fn doc_ending_with_new_line() {
        let input = "Cats are friends\n";

        let expected = paragraph! { text("Cats are friends") };

        assert_parse_succeeds(input, expected);
    }

    #[test]
    fn doc_ending_with_new_lines() {
        let input = "Feline friends\n\n";

        let expected = paragraph! { text("Feline friends") };

        assert_parse_succeeds(input, expected);
    }

    #[test]
    fn new_line_and_space_between_styled_and_plain_text_runs() {
        let input = "*Cat*\n cat";

        let expected = paragraph! {
            strong_text("Cat"),
            text(" cat"),
        };

        assert_parse_succeeds(input, expected);
    }

    #[test]
    fn leading_whitespace_on_paragraph_is_ignored() {
        let input = "Cat\n\n  cat";

        let expected = elements!(
            paragraph { text("Cat") },
            paragraph { text("cat") }
        );

        assert_parse_succeeds(input, expected);
    }

    #[test]
    fn doc_metadata() {
        let input = concat!("@metadata\n", "id: 12.03\n");

        let expected = document!(
            metadata: {
                id: "12.03"
            }
        );

        assert_parse_succeeds(input, expected);
    }

    #[test]
    fn doc_metadata_with_tags() {
        let input = concat!(
            "@metadata\n",
            "id: feline.feasts.25\n",
            "tags: cooking | eating | nice-smells\n",
        );

        let expected = document!(
            metadata: {
                id:"feline.feasts.25",
                tags: ["cooking", "eating", "nice-smells"],
            }
        );

        assert_parse_succeeds(input, expected);
    }

    #[test]
    fn doc_metadata_with_unknown_identifier_is_rejected() {
        let input = "@metadata\nkibble: yes please\n";

        let expected = UnknownMetadata(String::from("kibble"));

        assert_parse_fails(input, expected);
    }

    #[test]
    fn doc_metadata_not_at_start_is_rejected() {
        let input = concat!(
            "Helloo there. Metadata should not follow this.!\n",
            "\n",
            "@metadata\n",
            "id: 01.23\n"
        );

        let expected = MetadataNotAtStart;

        assert_parse_fails(input, expected);
    }

    #[test]
    fn doc_title() {
        let input = "/ Practical espionage for felines in urban settings";

        let expected = document!(
            title: "Practical espionage for felines in urban settings"
        );

        assert_parse_succeeds(input, expected);
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

        assert_parse_succeeds(input, expected);
    }

    #[test]
    fn doc_title_with_wonky_spacing() {
        let input = "/My Very   Cool Document   \n\n";

        let expected = document!(
            title: "My Very Cool Document"
        );

        assert_parse_succeeds(input, expected);
    }

    #[test]
    fn doc_title_with_no_trailing_newline() {
        let input = "/Some Doc";

        let expected = document!(
            title: "Some Doc"
        );

        assert_parse_succeeds(input, expected);
    }

    #[test]
    fn doc_title_in_not_at_start() {
        let input = concat!(
            "Document should not be after this!\n",
            "\n",
            "/Some Document Title"
        );

        let expected = TitleNotAtStart;

        assert_parse_fails(input, expected);
    }

    #[test]
    fn doc_title_in_section() {
        let input = concat!(
            "// Some important document section\n",
            "\n",
            "/Some Document Title"
        );

        let expected = TitleNotAtStart;

        assert_parse_fails(input, expected);
    }

    #[test]
    #[ignore]
    fn doc_title_in_subsection() {
        let input = concat!(
            "/// Some important document section\n",
            "\n",
            "/Some Document Title"
        );

        let expected = TitleNotAtStart;

        assert_parse_fails(input, expected);
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

        assert_parse_succeeds(input, expected);
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

        assert_parse_succeeds(input, expected);
    }

    #[test]
    fn container_missing_start_is_rejected() {
        let input = "Silly cat\n<<<";

        let expected = ContainerMissingStart;

        assert_parse_fails(input, expected);
    }

    #[test]
    fn empty_container_is_rejected() {
        let input = "!info\n";

        let expected = EmptyContainer;

        assert_parse_fails(input, expected);
    }

    #[test]
    fn detactched_container_is_rejected() {
        let input = "!info\n\ncats!";

        let expected = EmptyContainer;

        assert_parse_fails(input, expected);
    }

    #[test]
    fn trailing_text_on_delimited_start_is_rejected() {
        let input = concat!(
            "!info\n",
            ">>>squeek\n",
            "Let me know if you find where I left my\n",
            "<<<"
        );

        let expected = UnexpectedToken("expected: LineBreak, got: markup text 'squeek'".into());

        assert_parse_fails(input, expected);
    }

    #[test]
    fn trailing_text_on_delimited_end_is_rejected() {
        let input = concat!(
            "!info\n",
            ">>>\n",
            "Let me know if you find where I left my\n",
            "<<<toy"
        );

        let expected = UnexpectedToken("expected: BlockBreak, got: markup text 'toy'".into());

        assert_parse_fails(input, expected);
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

        //TODO: Mismatch casing on expected vs got is a bit meh
        let expected = UnexpectedToken("expected: BlockBreak, got: linebreak".into());

        assert_parse_fails(input, expected);
    }

    #[test]
    fn dash_in_paragraph_is_treated_as_part_of_text() {
        let input = "Ripley\n- Cat";

        let expected = paragraph! { text("Ripley - Cat") };

        assert_parse_succeeds(input, expected);
    }

    #[test]
    fn indented_dash_in_paragraph_is_treated_as_part_of_text() {
        let input = "Ripley\n  - Cat";

        let expected = paragraph! { text("Ripley - Cat") };

        assert_parse_succeeds(input, expected);
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
        assert_parse_succeeds(input, expected);
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

        assert_parse_succeeds(input, expected);
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
        assert_parse_succeeds(input, expected);
    }

    #[test]
    fn dash_in_list_text_is_not_treated_as_bullet() {
        let input = concat!("- Meow - meow\n",);

        let expected = list! {paragraph { text("Meow - meow") }};

        assert_parse_succeeds(input, expected);
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

        assert_parse_succeeds(input, expected);
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

        assert_parse_succeeds(input, expected);
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

        assert_parse_succeeds(input, expected);
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

        assert_parse_succeeds(input, expected);
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

        assert_parse_succeeds(input, expected);
    }

    #[test]
    fn list_item_with_trailing_whitespace() {
        let input = "- Foo    \n- Bar";

        let expected = list! {
            paragraph { text("Foo")},
            paragraph { text("Bar")},
        };

        assert_parse_succeeds(input, expected);
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

        assert_parse_succeeds(input, expected);
    }

    #[test]
    fn list_with_emphasis_over_multiple_points() {
        let input = "- f_oo\n  -ba_r";

        //TODO: Could we have more helpful, more specific error messages
        // e.g. UnterminatedEmphasis
        let expected = UnexpectedToken("expected: emphasis delimiter, got: linebreak".into());

        assert_parse_fails(input, expected);
    }

    #[test]
    fn list_with_uneven_spaces() {
        let input = "-foo\n -bar";

        let expected = UnevenListIndent(1);

        assert_parse_fails(input, expected);
    }

    #[test]
    fn list_that_skips_ascending_indent_level() {
        let input = concat!(
            "- Nice things to eat\n",
            "    - Wagyu beef because it is oh so tender\n",
        );

        let expected = MissingListLevel((0, 2));

        assert_parse_fails(input, expected);
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

        assert_parse_succeeds(input, expected);
    }

    #[test]
    fn error_specifies_correct_row_and_column() {
        let input = "Silly cat\ngoes *_*";

        let expected = (7, 2);

        let error = parse_str(input).unwrap_err();
        let actual = (error.input_column, error.input_line);

        assert_eq!(actual, expected);
    }

    #[test]
    fn basic_link_with_reference() {
        let input = concat!(
            "@references\n",
            "ripley_2020: https://example.com\n",
            "\n",
            "For more info, consult [our guide on petting cats]@ripley_2020,\n",
            "created by our own in house experts.\n",
        );

        let expected = document!(
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

        assert_parse_succeeds(input, expected);
    }

    #[test]
    fn at_sign_can_be_used_normally() {
        let input = "C@ts are great @ that";

        let expected = paragraph! {
            text("C@ts are great @ that")
        };

        assert_parse_succeeds(input, expected);
    }

    #[test]
    fn whitespace_around_linked_text_is_rejected() {
        let input = concat!(
            "@references\n",
            "ripley_2020: https://example.com\n",
            "\n",
            "We like [ petting cats ]@ripley_2020 a lot.\n",
        );

        let expected = LooseDelimiter;
        assert_parse_fails(input, expected);
    }

    #[test]
    fn references_after_content_rejected() {
        let input = concat!(
            "For more info, consult [our guide on petting cats]@ripley_2020,\n",
            "created by our own in house experts.\n",
            "\n",
            "@references\n",
            "ripley_2020: https://example.com"
        );

        let expected = ReferencesOutOfPlace;

        assert_parse_fails(input, expected);
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

        assert_parse_succeeds(input, expected);
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

        assert_parse_succeeds(input, expected);
    }

    // TODO: test missing reference
    // TODO: Allow un-delimited code blocks
    // TODO: Test we cant have sections or sub sections in containers
    // TODO: Test code block missing ending delimiter has an informative error

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

        assert_parse_succeeds(input, expected);
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

        let expected = document! (
            contents: {
                code {
                    "Meow?\n",
                },
                paragraph {
                    text("Hey, whats up?")
                }
            }
        );

        assert_parse_succeeds(input, expected);
    }
}
