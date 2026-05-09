use std::fmt::{self, Display, Formatter};
use std::str::CharIndices;

//TODO: Some kind of annotated example that describes the terminology
//TODO: Make terminology less confusing

const SPACE: char = ' ';
const NEW_LINE: char = '\n';
const COLON: char = ':';
const CONTAINER_START_PATTERN: &str = ">>>";
const CONTAINER_END_PATTERN: &str = "<<<";
const CODE_DELIMITER_PATTERN: &str = "---";
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

#[derive(Clone, Copy, Debug)]
pub struct Indent {
    pub space_count: usize,
}

#[derive(PartialEq, Eq, Debug)]
pub struct TokenName(pub &'static str);

impl Display for TokenName {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        let TokenName(name) = self;
        f.write_str(name)
    }
}

#[derive(PartialEq, Eq, Debug)]
pub struct TokenDescription {
    pub name: TokenName,
    pub escaped_lexeme: String,
}

// TODO: Use a macro to clear up token building
// A macro where we can define for each token
// name
// structure
// pattern matcher
// ???

#[derive(Clone, Copy, Debug)]
pub enum Token<'a> {
    EndOfInput,
    TitleDirective,
    SectionDirective,
    SubSectionDirective,
    MetadataDirective,
    ReferencesDirective,
    ParagraphDirective,
    ListDirective,
    CodeDirective,
    InfoContainerDirective,
    BlockParametersStart,
    BlockParametersEnd,
    //TODO: This name is meh - just call it what it is: equals sign?
    BlockParameterNameValueSeperator,
    BlockBreak,
    DataListSeperator,
    DataKeyValueSeperator,
    TitleTextSpace,
    LineBreak,
    StrongDelimiter,
    EmphasisDelimiter,
    StrikethroughDelimiter,
    RawDelimiter,
    MarkupTextSpace,
    LinkOpeningDelimiter,
    LinkClosingDelimiter,
    LinkToReferenceJoiner,
    CodeDelimiter,
    //TODO: More rubbish naming
    DelimitedContainerStart,
    DelimitedContainerEnd,
    // TODO: dont need the indirection here?
    // just have values in place?
    UnknownDirective(&'a str),
    Unknown(&'a str),
    BlockParameterName(&'a str),
    BlockParameterValue(&'a str),
    DataIdentifier(&'a str),
    DataValue(&'a str),
    TitleText(&'a str),
    MarkupText(&'a str),
    RawFragment(&'a str),
    Code(&'a str),
    ListBullet(Indent),
}

impl<'a> Token<'a> {
    fn name(&self) -> TokenName {
        match self {
            Token::EndOfInput => EndOfInput::NAME,
            Token::TitleDirective => TitleDirective::NAME,
            Token::SectionDirective => SectionDirective::NAME,
            Token::SubSectionDirective => SubSectionDirective::NAME,
            Token::MetadataDirective => MetadataDirective::NAME,
            Token::ReferencesDirective => ReferencesDirective::NAME,
            Token::ParagraphDirective => ParagraphDirective::NAME,
            Token::ListDirective => ListDirective::NAME,
            Token::CodeDirective => CodeDirective::NAME,
            Token::InfoContainerDirective => InfoContainerDirective::NAME,
            Token::BlockParametersStart => BlockParametersStart::NAME,
            Token::BlockParametersEnd => BlockParametersEnd::NAME,
            Token::BlockParameterNameValueSeperator => BlockParameterNameValueSeperator::NAME,
            Token::BlockBreak => BlockBreak::NAME,
            Token::DataListSeperator => DataListSeperator::NAME,
            Token::DataKeyValueSeperator => DataKeyValueSeperator::NAME,
            Token::TitleTextSpace => TitleTextSpace::NAME,
            Token::LineBreak => LineBreak::NAME,
            Token::StrongDelimiter => StrongDelimiter::NAME,
            Token::EmphasisDelimiter => EmphasisDelimiter::NAME,
            Token::StrikethroughDelimiter => StrikethroughDelimiter::NAME,
            Token::RawDelimiter => RawDelimiter::NAME,
            Token::MarkupTextSpace => MarkupTextSpace::NAME,
            Token::LinkOpeningDelimiter => LinkOpeningDelimiter::NAME,
            Token::LinkClosingDelimiter => LinkClosingDelimiter::NAME,
            Token::LinkToReferenceJoiner => LinkToReferenceJoiner::NAME,
            Token::CodeDelimiter => CodeDelimiter::NAME,
            Token::DelimitedContainerStart => DelimitedContainerStart::NAME,
            Token::DelimitedContainerEnd => DelimitedContainerEnd::NAME,
            Token::UnknownDirective(_) => UnknownDirective::NAME,
            Token::Unknown(_) => Unknown::NAME,
            Token::BlockParameterName(_) => BlockParameterName::NAME,
            Token::BlockParameterValue(_) => BlockParameterValue::NAME,
            Token::DataIdentifier(_) => DataIdentifier::NAME,
            Token::DataValue(_) => DataValue::NAME,
            Token::TitleText(_) => TitleText::NAME,
            Token::MarkupText(_) => MarkupText::NAME,
            Token::RawFragment(_) => RawFragment::NAME,
            Token::Code(_) => Code::NAME,
            Token::ListBullet(_) => ListBullet::NAME,
        }
    }
}

type Matcher = for<'a> fn(&Scanner<'a>) -> Option<ScanMatch<'a>>;

#[derive(PartialEq, Eq, Debug)]
pub struct UnexpectedTokenError {
    pub position: Position,
    pub expected: TokenName,
    pub actual: TokenDescription,
}

// TODO: store span, not position
pub struct Spanned<T> {
    pub value: T,
    pub position: Position,
}

// TODO: What if we leant into exposing the dynamic type?
pub struct SpannedToken<'a> {
    pub value: Token<'a>,
    pub position: Position,
    pub lexeme: &'a str,
}

impl<'a> SpannedToken<'a> {
    pub fn expect<T>(&self) -> Result<T, UnexpectedTokenError>
    where
        T: TokenSpec<'a>,
    {
        T::try_from(self.value).map_err(|_| UnexpectedTokenError {
            expected: T::NAME,
            actual: self.description(),
            position: self.position,
        })
    }

    // TODO: naming of these next two funcs is still a bit off...
    // TODO: Do we need both?

    pub fn try_value<T>(&self) -> Option<Spanned<T>>
    where
        T: TokenSpec<'a>,
    {
        T::try_from(self.value).ok().map(|token| Spanned {
            value: token,
            position: self.position,
        })
    }

    pub fn value_into<T>(&self) -> Option<T>
    where
        T: TokenSpec<'a>,
    {
        T::try_from(self.value).ok()
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
            escaped_lexeme: self.lexeme.escape_default().to_string(),
        }
    }
}

//TODO: put matcher func as an associated type?
// TODO: do we really need this? Its just the TryFrom thats
// essential right?
pub trait TokenSpec<'a>: TryFrom<Token<'a>> {
    const NAME: TokenName;
}

#[derive(Clone, Copy, Debug)]
pub struct MetadataDirective;

#[derive(Clone, Copy, Debug)]
pub struct ReferencesDirective;

#[derive(Clone, Copy, Debug)]
pub struct ParagraphDirective;

#[derive(Clone, Copy, Debug)]
pub struct ListDirective;

#[derive(Clone, Copy, Debug)]
pub struct CodeDirective;

#[derive(Clone, Copy, Debug)]
pub struct InfoContainerDirective;

#[derive(Clone, Copy, Debug)]
pub struct UnknownDirective<'a>(pub &'a str);

#[derive(Clone, Copy, Debug)]
pub struct EndOfInput;

#[derive(Clone, Copy, Debug)]
pub struct TitleDirective;

#[derive(Clone, Copy, Debug)]
pub struct SectionDirective;

#[derive(Clone, Copy, Debug)]
pub struct SubSectionDirective;

#[derive(Clone, Copy, Debug)]
pub struct BlockParametersStart;

#[derive(Clone, Copy, Debug)]
pub struct BlockParametersEnd;

#[derive(Clone, Copy, Debug)]
pub struct BlockParameterNameValueSeperator;

#[derive(Clone, Copy, Debug)]
pub struct BlockBreak;

#[derive(Clone, Copy, Debug)]
pub struct DataListSeperator;

#[derive(Clone, Copy, Debug)]
pub struct DataKeyValueSeperator;

#[derive(Clone, Copy, Debug)]
pub struct TitleTextSpace;

#[derive(Clone, Copy, Debug)]
pub struct LineBreak;

#[derive(Clone, Copy, Debug)]
pub struct StrongDelimiter;

#[derive(Clone, Copy, Debug)]
pub struct EmphasisDelimiter;

#[derive(Clone, Copy, Debug)]
pub struct StrikethroughDelimiter;

#[derive(Clone, Copy, Debug)]
pub struct RawDelimiter;

#[derive(Clone, Copy, Debug)]
pub struct MarkupTextSpace;

#[derive(Clone, Copy, Debug)]
pub struct LinkOpeningDelimiter;

#[derive(Clone, Copy, Debug)]
pub struct LinkClosingDelimiter;

#[derive(Clone, Copy, Debug)]
pub struct LinkToReferenceJoiner;

#[derive(Clone, Copy, Debug)]
pub struct CodeDelimiter;

#[derive(Clone, Copy, Debug)]
pub struct DelimitedContainerStart;

#[derive(Clone, Copy, Debug)]
pub struct DelimitedContainerEnd;

#[derive(Clone, Copy, Debug)]
pub struct BlockParameterName<'a>(pub &'a str);

#[derive(Clone, Copy, Debug)]
pub struct BlockParameterValue<'a>(pub &'a str);

#[derive(Clone, Copy, Debug)]
pub struct DataIdentifier<'a>(pub &'a str);

#[derive(Clone, Copy, Debug)]
pub struct DataValue<'a>(pub &'a str);

#[derive(Clone, Copy, Debug)]
pub struct TitleText<'a>(pub &'a str);

#[derive(Clone, Copy, Debug)]
pub struct MarkupText<'a>(pub &'a str);

#[derive(Clone, Copy, Debug)]
pub struct RawFragment<'a>(pub &'a str);

#[derive(Clone, Copy, Debug)]
pub struct Code<'a>(pub &'a str);

#[derive(Clone, Copy, Debug)]
pub struct Unknown<'a>(pub &'a str);

#[derive(Clone, Copy, Debug)]
pub struct ListBullet(pub Indent);

//TODO: Use macros to clear up repetition

impl<'a> TryFrom<Token<'a>> for DataValue<'a> {
    type Error = ();

    fn try_from(value: Token<'a>) -> Result<Self, Self::Error> {
        match value {
            Token::DataValue(token) => Ok(Self(token)),
            _ => Err(()),
        }
    }
}

impl<'a> TryFrom<Token<'a>> for DataIdentifier<'a> {
    type Error = ();

    fn try_from(value: Token<'a>) -> Result<Self, Self::Error> {
        match value {
            Token::DataIdentifier(token) => Ok(Self(token)),
            _ => Err(()),
        }
    }
}

impl<'a> TryFrom<Token<'a>> for TitleText<'a> {
    type Error = ();

    fn try_from(value: Token<'a>) -> Result<Self, Self::Error> {
        match value {
            Token::TitleText(token) => Ok(Self(token)),
            _ => Err(()),
        }
    }
}

impl<'a> TryFrom<Token<'a>> for BlockParameterName<'a> {
    type Error = ();

    fn try_from(value: Token<'a>) -> Result<Self, Self::Error> {
        match value {
            Token::BlockParameterName(token) => Ok(Self(token)),
            _ => Err(()),
        }
    }
}

impl<'a> TryFrom<Token<'a>> for BlockParameterValue<'a> {
    type Error = ();

    fn try_from(value: Token<'a>) -> Result<Self, Self::Error> {
        match value {
            Token::BlockParameterValue(token) => Ok(Self(token)),
            _ => Err(()),
        }
    }
}

impl<'a> TryFrom<Token<'a>> for RawFragment<'a> {
    type Error = ();

    fn try_from(value: Token<'a>) -> Result<Self, Self::Error> {
        match value {
            Token::RawFragment(token) => Ok(Self(token)),
            _ => Err(()),
        }
    }
}

impl<'a> TryFrom<Token<'a>> for MarkupText<'a> {
    type Error = ();

    fn try_from(value: Token<'a>) -> Result<Self, Self::Error> {
        match value {
            Token::MarkupText(token) => Ok(Self(token)),
            _ => Err(()),
        }
    }
}

impl<'a> TryFrom<Token<'a>> for Code<'a> {
    type Error = ();

    fn try_from(value: Token<'a>) -> Result<Self, Self::Error> {
        match value {
            Token::Code(token) => Ok(Self(token)),
            _ => Err(()),
        }
    }
}

impl<'a> TryFrom<Token<'a>> for ListBullet {
    type Error = ();

    fn try_from(value: Token<'a>) -> Result<Self, Self::Error> {
        match value {
            Token::ListBullet(token) => Ok(Self(token)),
            _ => Err(()),
        }
    }
}

impl<'a> TryFrom<Token<'a>> for LineBreak {
    type Error = ();

    fn try_from(value: Token<'a>) -> Result<Self, Self::Error> {
        match value {
            Token::LineBreak => Ok(Self),
            _ => Err(()),
        }
    }
}

impl<'a> TryFrom<Token<'a>> for TitleDirective {
    type Error = ();

    fn try_from(value: Token<'a>) -> Result<Self, Self::Error> {
        match value {
            Token::TitleDirective => Ok(Self),
            _ => Err(()),
        }
    }
}

impl<'a> TryFrom<Token<'a>> for BlockBreak {
    type Error = ();

    fn try_from(value: Token<'a>) -> Result<Self, Self::Error> {
        match value {
            Token::BlockBreak => Ok(Self),
            _ => Err(()),
        }
    }
}

impl<'a> TryFrom<Token<'a>> for DataKeyValueSeperator {
    type Error = ();

    fn try_from(value: Token<'a>) -> Result<Self, Self::Error> {
        match value {
            Token::DataKeyValueSeperator => Ok(Self),
            _ => Err(()),
        }
    }
}

impl<'a> TryFrom<Token<'a>> for DelimitedContainerEnd {
    type Error = ();

    fn try_from(value: Token<'a>) -> Result<Self, Self::Error> {
        match value {
            Token::DelimitedContainerEnd => Ok(Self),
            _ => Err(()),
        }
    }
}

impl<'a> TryFrom<Token<'a>> for SectionDirective {
    type Error = ();

    fn try_from(value: Token<'a>) -> Result<Self, Self::Error> {
        match value {
            Token::SectionDirective => Ok(Self),
            _ => Err(()),
        }
    }
}

impl<'a> TryFrom<Token<'a>> for SubSectionDirective {
    type Error = ();

    fn try_from(value: Token<'a>) -> Result<Self, Self::Error> {
        match value {
            Token::SubSectionDirective => Ok(Self),
            _ => Err(()),
        }
    }
}

impl<'a> TryFrom<Token<'a>> for BlockParameterNameValueSeperator {
    type Error = ();

    fn try_from(value: Token<'a>) -> Result<Self, Self::Error> {
        match value {
            Token::BlockParameterNameValueSeperator => Ok(Self),
            _ => Err(()),
        }
    }
}

impl<'a> TryFrom<Token<'a>> for BlockParametersEnd {
    type Error = ();

    fn try_from(value: Token<'a>) -> Result<Self, Self::Error> {
        match value {
            Token::BlockParametersEnd => Ok(Self),
            _ => Err(()),
        }
    }
}

impl<'a> TryFrom<Token<'a>> for RawDelimiter {
    type Error = ();

    fn try_from(value: Token<'a>) -> Result<Self, Self::Error> {
        match value {
            Token::RawDelimiter => Ok(Self),
            _ => Err(()),
        }
    }
}

impl<'a> TryFrom<Token<'a>> for LinkOpeningDelimiter {
    type Error = ();

    fn try_from(value: Token<'a>) -> Result<Self, Self::Error> {
        match value {
            Token::LinkOpeningDelimiter => Ok(Self),
            _ => Err(()),
        }
    }
}

impl<'a> TryFrom<Token<'a>> for LinkClosingDelimiter {
    type Error = ();

    fn try_from(value: Token<'a>) -> Result<Self, Self::Error> {
        match value {
            Token::LinkClosingDelimiter => Ok(Self),
            _ => Err(()),
        }
    }
}

impl<'a> TryFrom<Token<'a>> for LinkToReferenceJoiner {
    type Error = ();

    fn try_from(value: Token<'a>) -> Result<Self, Self::Error> {
        match value {
            Token::LinkToReferenceJoiner => Ok(Self),
            _ => Err(()),
        }
    }
}

impl<'a> TryFrom<Token<'a>> for CodeDelimiter {
    type Error = ();

    fn try_from(value: Token<'a>) -> Result<Self, Self::Error> {
        match value {
            Token::CodeDelimiter => Ok(Self),
            _ => Err(()),
        }
    }
}

impl<'a> TryFrom<Token<'a>> for StrongDelimiter {
    type Error = ();

    fn try_from(value: Token<'a>) -> Result<Self, Self::Error> {
        match value {
            Token::StrongDelimiter => Ok(Self),
            _ => Err(()),
        }
    }
}

impl<'a> TryFrom<Token<'a>> for EmphasisDelimiter {
    type Error = ();

    fn try_from(value: Token<'a>) -> Result<Self, Self::Error> {
        match value {
            Token::EmphasisDelimiter => Ok(Self),
            _ => Err(()),
        }
    }
}

impl<'a> TryFrom<Token<'a>> for StrikethroughDelimiter {
    type Error = ();

    fn try_from(value: Token<'a>) -> Result<Self, Self::Error> {
        match value {
            Token::StrikethroughDelimiter => Ok(Self),
            _ => Err(()),
        }
    }
}

impl<'a> TryFrom<Token<'a>> for EndOfInput {
    type Error = ();

    fn try_from(value: Token<'a>) -> Result<Self, Self::Error> {
        match value {
            Token::EndOfInput => Ok(Self),
            _ => Err(()),
        }
    }
}

impl<'a> TryFrom<Token<'a>> for BlockParametersStart {
    type Error = ();

    fn try_from(value: Token<'a>) -> Result<Self, Self::Error> {
        match value {
            Token::BlockParametersStart => Ok(Self),
            _ => Err(()),
        }
    }
}

impl<'a> TryFrom<Token<'a>> for DataListSeperator {
    type Error = ();

    fn try_from(value: Token<'a>) -> Result<Self, Self::Error> {
        match value {
            Token::DataListSeperator => Ok(Self),
            _ => Err(()),
        }
    }
}

impl<'a> TryFrom<Token<'a>> for TitleTextSpace {
    type Error = ();

    fn try_from(value: Token<'a>) -> Result<Self, Self::Error> {
        match value {
            Token::TitleTextSpace => Ok(Self),
            _ => Err(()),
        }
    }
}
impl<'a> TryFrom<Token<'a>> for MarkupTextSpace {
    type Error = ();

    fn try_from(value: Token<'a>) -> Result<Self, Self::Error> {
        match value {
            Token::MarkupTextSpace => Ok(Self),
            _ => Err(()),
        }
    }
}
impl<'a> TryFrom<Token<'a>> for DelimitedContainerStart {
    type Error = ();

    fn try_from(value: Token<'a>) -> Result<Self, Self::Error> {
        match value {
            Token::DelimitedContainerStart => Ok(Self),
            _ => Err(()),
        }
    }
}

impl<'a> TryFrom<Token<'a>> for Unknown<'a> {
    type Error = ();

    fn try_from(value: Token<'a>) -> Result<Self, Self::Error> {
        match value {
            Token::Unknown(token) => Ok(Self(token)),
            _ => Err(()),
        }
    }
}

impl<'a> TryFrom<Token<'a>> for MetadataDirective {
    type Error = ();

    fn try_from(value: Token<'a>) -> Result<Self, Self::Error> {
        match value {
            Token::MetadataDirective => Ok(Self),
            _ => Err(()),
        }
    }
}

impl<'a> TryFrom<Token<'a>> for ReferencesDirective {
    type Error = ();

    fn try_from(value: Token<'a>) -> Result<Self, Self::Error> {
        match value {
            Token::ReferencesDirective => Ok(Self),
            _ => Err(()),
        }
    }
}

impl<'a> TryFrom<Token<'a>> for ParagraphDirective {
    type Error = ();

    fn try_from(value: Token<'a>) -> Result<Self, Self::Error> {
        match value {
            Token::ParagraphDirective => Ok(Self),
            _ => Err(()),
        }
    }
}

impl<'a> TryFrom<Token<'a>> for ListDirective {
    type Error = ();

    fn try_from(value: Token<'a>) -> Result<Self, Self::Error> {
        match value {
            Token::ListDirective => Ok(Self),
            _ => Err(()),
        }
    }
}

impl<'a> TryFrom<Token<'a>> for CodeDirective {
    type Error = ();

    fn try_from(value: Token<'a>) -> Result<Self, Self::Error> {
        match value {
            Token::CodeDirective => Ok(Self),
            _ => Err(()),
        }
    }
}

impl<'a> TryFrom<Token<'a>> for InfoContainerDirective {
    type Error = ();

    fn try_from(value: Token<'a>) -> Result<Self, Self::Error> {
        match value {
            Token::InfoContainerDirective => Ok(Self),
            _ => Err(()),
        }
    }
}

impl<'a> TryFrom<Token<'a>> for UnknownDirective<'a> {
    type Error = ();

    fn try_from(value: Token<'a>) -> Result<Self, Self::Error> {
        match value {
            Token::UnknownDirective(token) => Ok(Self(token)),
            _ => Err(()),
        }
    }
}

impl<'a> TokenSpec<'a> for EndOfInput {
    const NAME: TokenName = TokenName("end of input");
}

impl<'a> TokenSpec<'a> for TitleDirective {
    const NAME: TokenName = TokenName("title directive");
}

impl<'a> TokenSpec<'a> for SectionDirective {
    const NAME: TokenName = TokenName("section directive");
}

impl<'a> TokenSpec<'a> for SubSectionDirective {
    const NAME: TokenName = TokenName("subsection directive");
}

impl<'a> TokenSpec<'a> for BlockParametersStart {
    const NAME: TokenName = TokenName("block parameters start '('");
}

impl<'a> TokenSpec<'a> for BlockParametersEnd {
    const NAME: TokenName = TokenName("block parameters end ')'");
}

impl<'a> TokenSpec<'a> for BlockParameterNameValueSeperator {
    const NAME: TokenName = TokenName("block parameter name value seperator");
}

impl<'a> TokenSpec<'a> for BlockBreak {
    const NAME: TokenName = TokenName("block break");
}

impl<'a> TokenSpec<'a> for DataListSeperator {
    const NAME: TokenName = TokenName("metadata list seperator");
}

impl<'a> TokenSpec<'a> for DataKeyValueSeperator {
    const NAME: TokenName = TokenName("metadata key value seperator");
}

impl<'a> TokenSpec<'a> for TitleTextSpace {
    const NAME: TokenName = TokenName("title text space");
}

impl<'a> TokenSpec<'a> for LineBreak {
    const NAME: TokenName = TokenName("linebreak");
}

impl<'a> TokenSpec<'a> for StrongDelimiter {
    const NAME: TokenName = TokenName("strong delimiter");
}

impl<'a> TokenSpec<'a> for EmphasisDelimiter {
    const NAME: TokenName = TokenName("emphasis delimiter");
}

impl<'a> TokenSpec<'a> for StrikethroughDelimiter {
    const NAME: TokenName = TokenName("strikethrough delimiter");
}

impl<'a> TokenSpec<'a> for RawDelimiter {
    const NAME: TokenName = TokenName("raw delimiter");
}

impl<'a> TokenSpec<'a> for MarkupTextSpace {
    const NAME: TokenName = TokenName("markup text space");
}

impl<'a> TokenSpec<'a> for LinkOpeningDelimiter {
    const NAME: TokenName = TokenName("link opening delimiter");
}

impl<'a> TokenSpec<'a> for LinkClosingDelimiter {
    const NAME: TokenName = TokenName("link closing delimiter");
}

impl<'a> TokenSpec<'a> for LinkToReferenceJoiner {
    const NAME: TokenName = TokenName("link to reference joiner '@'");
}

impl<'a> TokenSpec<'a> for CodeDelimiter {
    const NAME: TokenName = TokenName("delimited block delimiter");
}

impl<'a> TokenSpec<'a> for DelimitedContainerStart {
    const NAME: TokenName = TokenName("delimited container start");
}

impl<'a> TokenSpec<'a> for DelimitedContainerEnd {
    const NAME: TokenName = TokenName("delimited container end");
}

impl<'a> TokenSpec<'a> for Unknown<'a> {
    const NAME: TokenName = TokenName("unknown");
}

impl<'a> TokenSpec<'a> for BlockParameterName<'a> {
    const NAME: TokenName = TokenName("block parameter name");
}

impl<'a> TokenSpec<'a> for BlockParameterValue<'a> {
    const NAME: TokenName = TokenName("block parameter value");
}

impl<'a> TokenSpec<'a> for DataIdentifier<'a> {
    const NAME: TokenName = TokenName("metadata identifier");
}

impl<'a> TokenSpec<'a> for DataValue<'a> {
    const NAME: TokenName = TokenName("metadata value");
}

impl<'a> TokenSpec<'a> for TitleText<'a> {
    const NAME: TokenName = TokenName("title text");
}

impl<'a> TokenSpec<'a> for MarkupText<'a> {
    const NAME: TokenName = TokenName("markup text");
}

impl<'a> TokenSpec<'a> for RawFragment<'a> {
    const NAME: TokenName = TokenName("raw fragment");
}

impl<'a> TokenSpec<'a> for Code<'a> {
    const NAME: TokenName = TokenName("code");
}

impl<'a> TokenSpec<'a> for ListBullet {
    const NAME: TokenName = TokenName("list bullet");
}

impl<'a> TokenSpec<'a> for MetadataDirective {
    const NAME: TokenName = TokenName("metadata directive");
}

impl<'a> TokenSpec<'a> for ReferencesDirective {
    const NAME: TokenName = TokenName("references directive");
}

impl<'a> TokenSpec<'a> for ParagraphDirective {
    const NAME: TokenName = TokenName("paragraph directive");
}

impl<'a> TokenSpec<'a> for ListDirective {
    const NAME: TokenName = TokenName("list directive");
}

impl<'a> TokenSpec<'a> for CodeDirective {
    const NAME: TokenName = TokenName("code directive");
}

impl<'a> TokenSpec<'a> for InfoContainerDirective {
    const NAME: TokenName = TokenName("code directive");
}

impl<'a> TokenSpec<'a> for UnknownDirective<'a> {
    const NAME: TokenName = TokenName("unknown block directive");
}

//TODO: we should remove display from TokenSpec
//TODO: these should contain value for value tokens
impl Display for EndOfInput {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        f.write_str(Self::NAME.0)
    }
}

impl Display for TitleDirective {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        f.write_str(Self::NAME.0)
    }
}

impl Display for SectionDirective {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        f.write_str(Self::NAME.0)
    }
}

impl Display for SubSectionDirective {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        f.write_str(Self::NAME.0)
    }
}

impl Display for BlockParametersStart {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        f.write_str(Self::NAME.0)
    }
}

impl Display for BlockParametersEnd {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        f.write_str(Self::NAME.0)
    }
}

impl Display for BlockParameterNameValueSeperator {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        f.write_str(Self::NAME.0)
    }
}

impl Display for BlockBreak {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        f.write_str(Self::NAME.0)
    }
}

impl Display for DataListSeperator {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        f.write_str(Self::NAME.0)
    }
}

impl Display for DataKeyValueSeperator {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        f.write_str(Self::NAME.0)
    }
}

impl Display for TitleTextSpace {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        f.write_str(Self::NAME.0)
    }
}

impl Display for LineBreak {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        f.write_str(Self::NAME.0)
    }
}

impl Display for StrongDelimiter {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        f.write_str(Self::NAME.0)
    }
}

impl Display for EmphasisDelimiter {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        f.write_str(Self::NAME.0)
    }
}

impl Display for StrikethroughDelimiter {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        f.write_str(Self::NAME.0)
    }
}

impl Display for RawDelimiter {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        f.write_str(Self::NAME.0)
    }
}

impl Display for MarkupTextSpace {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        f.write_str(Self::NAME.0)
    }
}

impl Display for LinkOpeningDelimiter {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        f.write_str(Self::NAME.0)
    }
}

impl Display for LinkClosingDelimiter {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        f.write_str(Self::NAME.0)
    }
}

impl Display for LinkToReferenceJoiner {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        f.write_str(Self::NAME.0)
    }
}

impl Display for CodeDelimiter {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        f.write_str(Self::NAME.0)
    }
}

impl Display for DelimitedContainerStart {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        f.write_str(Self::NAME.0)
    }
}

impl Display for DelimitedContainerEnd {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        f.write_str(Self::NAME.0)
    }
}

impl Display for ListBullet {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        f.write_str(Self::NAME.0)
    }
}

impl<'a> Display for Unknown<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        f.write_str(Self::NAME.0)
    }
}

impl Display for MetadataDirective {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        f.write_str(Self::NAME.0)
    }
}

impl Display for ReferencesDirective {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        f.write_str(Self::NAME.0)
    }
}

impl Display for ParagraphDirective {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        f.write_str(Self::NAME.0)
    }
}

impl Display for ListDirective {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        f.write_str(Self::NAME.0)
    }
}

impl Display for CodeDirective {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        f.write_str(Self::NAME.0)
    }
}

impl<'a> Display for BlockParameterName<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        f.write_str(Self::NAME.0)
    }
}

impl<'a> Display for BlockParameterValue<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        f.write_str(Self::NAME.0)
    }
}

impl<'a> Display for DataIdentifier<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        f.write_str(Self::NAME.0)
    }
}

impl<'a> Display for DataValue<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        f.write_str(Self::NAME.0)
    }
}

impl<'a> Display for TitleText<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        f.write_str(Self::NAME.0)
    }
}

impl<'a> Display for MarkupText<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        f.write_str(Self::NAME.0)
    }
}

impl<'a> Display for RawFragment<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        f.write_str(Self::NAME.0)
    }
}

impl<'a> Display for Code<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        f.write_str(Self::NAME.0)
    }
}

impl<'a> Display for UnknownDirective<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        f.write_str(Self::NAME.0)
    }
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum ScanMode {
    //TODO: Should BlockStart be element start?
    BlockStart,
    Markup,
    ListMarkup,
    Raw,
    Title,
    Header,
    HeaderValue,
    StructuredData,
    LinkReference,
    Code,
    Generic,
}

pub struct Tokeniser<'a> {
    scanner: Scanner<'a>,
    token_count: usize,
    max_tokens: usize,
    mode_stack: Vec<ScanMode>,
}

impl<'a> Tokeniser<'a> {
    pub fn new(input: &'a str) -> Self {
        let mut scanner = Scanner::new(input);
        scanner.skip_while_on_empty_line();

        Tokeniser {
            scanner,
            token_count: 0,
            max_tokens: input.len(),
            mode_stack: vec![ScanMode::Generic],
        }
    }

    pub fn push_mode(&mut self, mode: ScanMode) {
        self.mode_stack.push(mode);
    }

    pub fn pop_mode(&mut self) -> ScanMode {
        //TODO: Instead of stack underflow, default to generic?
        debug_assert!(self.mode_stack.len() > 1, "Cannot pop the base mode");
        self.mode_stack.pop().expect("mode stack underflow")
    }

    fn current_matchers(&self) -> &'static [Matcher] {
        //TODO: meh expect
        match self.mode_stack.last().expect("empty mode stack") {
            ScanMode::BlockStart => SCAN_BLOCK_START,
            ScanMode::Markup => SCAN_MARKUP,
            ScanMode::ListMarkup => SCAN_LIST_MARKUP,
            ScanMode::Raw => SCAN_RAW,
            ScanMode::Title => SCAN_TITLE,
            ScanMode::Header => SCAN_HEADER,
            ScanMode::HeaderValue => SCAN_HEADER_VALUE,
            ScanMode::StructuredData => SCAN_STRUCTURED_DATA,
            ScanMode::LinkReference => SCAN_LINK_REFERENCE,
            ScanMode::Code => SCAN_CODE,
            ScanMode::Generic => SCAN_GENERIC,
        }
    }

    pub fn peek(&self) -> SpannedToken<'a> {
        //TODO: re-use with advance
        let matchers = self.current_matchers();
        let position = self.scanner.position();
        let start = self.scanner.read_head.index;
        let scan_match = self.scan(matchers);
        let end = scan_match.end.index;
        let lexeme = &self.scanner.input[start..end];

        SpannedToken {
            value: scan_match.token,
            lexeme,
            position,
        }
    }

    pub fn advance(&mut self) -> SpannedToken<'a> {
        if self.token_count > self.max_tokens {
            panic!("Posible infinite loop detected")
        }

        let matchers = self.current_matchers();
        let position = self.scanner.position();

        let start = self.scanner.read_head.index;
        let scan_match = self.scan(matchers);
        let end = scan_match.end.index;
        let lexeme = &self.scanner.input[start..end];

        self.scanner.advance_past(&scan_match);
        self.token_count += 1;

        SpannedToken {
            value: scan_match.token,
            lexeme,
            position,
        }
    }

    fn scan(&self, matchers: &[Matcher]) -> ScanMatch<'a> {
        matchers
            .iter()
            .find_map(|m| (m)(&self.scanner))
            // TODO: This is a bit clumsy, why not have match generic always be
            // at the base of the stack?
            .unwrap_or_else(|| match_generic(&self.scanner))
    }
}

#[derive(Eq, PartialEq, Clone, Copy, Debug)]
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
    //TODO: Actually store a peek
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

    fn is_on_empty_line(&self) -> bool {
        self.input[self.read_head.index..]
            .trim_start_matches(SPACE)
            .starts_with(NEW_LINE)
    }

    fn skip_while_on_empty_line(&mut self) {
        while self.is_on_empty_line() {
            while self.input[self.read_head.index..].starts_with([SPACE, NEW_LINE]) {
                self.read_head.read_next_char();
            }
        }
    }

    pub(crate) fn advance_past(&mut self, scan_match: &ScanMatch<'a>) {
        //TODO: read head is a bit chunky to clone about the place no?
        self.read_head = scan_match.end.clone();
    }
}

// TODO: Could hold different positions for full extent of
// matching text vs the sub text we are interested in
// e.g escaped chars
pub struct ScanMatch<'a> {
    token: Token<'a>,
    // TODO: Store a position instead of a head
    end: ReadHead<'a>,
}

fn match_list_bullet<'a>(scanner: &Scanner<'a>) -> Option<ScanMatch<'a>> {
    let mut head = scanner.read_head.clone();

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
        token: Token::ListBullet(Indent { space_count }),
        end: head,
    })
}

fn match_markup_text_space<'a>(scanner: &Scanner<'a>) -> Option<ScanMatch<'a>> {
    let mut head = scanner.read_head.clone();
    let i1 = head.index;

    while head.current == Some(SPACE) {
        head.read_next_char();
    }

    let mut has_new_line = false;
    if head.current == Some(NEW_LINE) {
        head.read_next_char();
        has_new_line = true;
    }

    if has_new_line && scanner.input[head.index..].starts_with(CONTAINER_END_PATTERN) {
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

    head.current?;

    Some(ScanMatch {
        token: Token::MarkupTextSpace,
        end: head,
    })
}

fn match_list_markup_text_space<'a>(scanner: &Scanner<'a>) -> Option<ScanMatch<'a>> {
    let mut head = scanner.read_head.clone();
    let i1 = head.index;

    while head.current == Some(SPACE) {
        head.read_next_char();
    }

    let mut has_new_line = false;
    if head.current == Some(NEW_LINE) {
        head.read_next_char();
        has_new_line = true;
    }

    if has_new_line && scanner.input[head.index..].starts_with(CONTAINER_END_PATTERN) {
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

    head.current?;

    Some(ScanMatch {
        token: Token::MarkupTextSpace,
        end: head,
    })
}

fn match_title_text_space<'a>(scanner: &Scanner<'a>) -> Option<ScanMatch<'a>> {
    let mut head = scanner.read_head.clone();

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
        token: Token::TitleTextSpace,
        end: head,
    })
}

fn match_parameters_start<'a>(scanner: &Scanner<'a>) -> Option<ScanMatch<'a>> {
    let mut head = scanner.read_head.clone();

    if head.current == Some(LEFT_BRACKET) {
        head.read_next_char();
    } else {
        return None;
    }

    Some(ScanMatch {
        token: Token::BlockParametersStart,
        end: head,
    })
}

fn match_parameters_end<'a>(scanner: &Scanner<'a>) -> Option<ScanMatch<'a>> {
    let mut head = scanner.read_head.clone();

    if head.current == Some(RIGHT_BRACKET) {
        head.read_next_char();
    } else {
        return None;
    }

    Some(ScanMatch {
        token: Token::BlockParametersEnd,
        end: head,
    })
}

fn match_parameter_name_value_seperator<'a>(scanner: &Scanner<'a>) -> Option<ScanMatch<'a>> {
    let mut head = scanner.read_head.clone();

    if head.current == Some(EQUALS) {
        head.read_next_char();
    } else {
        return None;
    }

    Some(ScanMatch {
        token: Token::BlockParameterNameValueSeperator,
        end: head,
    })
}

fn match_raw_delimiter<'a>(scanner: &Scanner<'a>) -> Option<ScanMatch<'a>> {
    let mut head = scanner.read_head.clone();

    if head.current == Some(BACKTICK) {
        head.read_next_char();
    } else {
        return None;
    }

    Some(ScanMatch {
        token: Token::RawDelimiter,
        end: head,
    })
}

fn match_link_opening_delimiter<'a>(scanner: &Scanner<'a>) -> Option<ScanMatch<'a>> {
    let mut head = scanner.read_head.clone();

    if head.current == Some(LEFT_SQUARE_BRACKET) {
        head.read_next_char();
    } else {
        return None;
    }

    Some(ScanMatch {
        token: Token::LinkOpeningDelimiter,
        end: head,
    })
}

fn match_link_closing_delimiter<'a>(scanner: &Scanner<'a>) -> Option<ScanMatch<'a>> {
    let mut head = scanner.read_head.clone();

    if head.current == Some(RIGHT_SQUARE_BRACKET) {
        head.read_next_char();
    } else {
        return None;
    }

    Some(ScanMatch {
        token: Token::LinkClosingDelimiter,
        end: head,
    })
}

fn match_link_to_reference_joiner<'a>(scanner: &Scanner<'a>) -> Option<ScanMatch<'a>> {
    let mut head = scanner.read_head.clone();

    if head.current == Some(AT_SIGN) {
        head.read_next_char();
    } else {
        return None;
    }

    Some(ScanMatch {
        token: Token::LinkToReferenceJoiner,
        end: head,
    })
}

fn match_strong_delimiter<'a>(scanner: &Scanner<'a>) -> Option<ScanMatch<'a>> {
    let mut head = scanner.read_head.clone();

    if head.current == Some(ASTERISK) {
        head.read_next_char();
    } else {
        return None;
    }

    Some(ScanMatch {
        token: Token::StrongDelimiter,
        end: head,
    })
}

fn match_emphasis_delimiter<'a>(scanner: &Scanner<'a>) -> Option<ScanMatch<'a>> {
    let mut head = scanner.read_head.clone();

    if head.current == Some(UNDERSCORE) {
        head.read_next_char();
    } else {
        return None;
    }

    Some(ScanMatch {
        token: Token::EmphasisDelimiter,
        end: head,
    })
}

fn match_strikethrough_delimiter<'a>(scanner: &Scanner<'a>) -> Option<ScanMatch<'a>> {
    let mut head = scanner.read_head.clone();

    if head.current == Some(TILDE) {
        head.read_next_char();
    } else {
        return None;
    }

    Some(ScanMatch {
        token: Token::StrikethroughDelimiter,
        end: head,
    })
}

fn match_code_delimiter<'a>(scanner: &Scanner<'a>) -> Option<ScanMatch<'a>> {
    let mut head = scanner.read_head.clone();

    for char in CODE_DELIMITER_PATTERN.chars() {
        if head.current == Some(char) {
            head.read_next_char();
            continue;
        } else {
            return None;
        }
    }

    Some(ScanMatch {
        token: Token::CodeDelimiter,
        end: head,
    })
}

fn match_code_block<'a>(scanner: &Scanner<'a>) -> Option<ScanMatch<'a>> {
    let mut head = scanner.read_head.clone();

    let i1 = head.index;

    loop {
        let on_start_of_line = head.column == 0;
        let prefix_matches = scanner.input[head.index..].starts_with(CODE_DELIMITER_PATTERN);
        if on_start_of_line && prefix_matches {
            let i2 = head.index;
            let text = &scanner.input[i1..i2];
            return Some(ScanMatch {
                token: Token::Code(text),
                end: head,
            });
        } else if head.current.is_none() {
            return None;
        } else {
            head.read_next_char();
        }
    }
}

fn match_blockbreak<'a>(scanner: &Scanner<'a>) -> Option<ScanMatch<'a>> {
    let mut head = scanner.read_head.clone();

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
            token: Token::BlockBreak,
            end: head,
        })
    } else {
        None
    }
}

fn match_linebreak<'a>(scanner: &Scanner<'a>) -> Option<ScanMatch<'a>> {
    let mut head = scanner.read_head.clone();

    while let Some(SPACE) = head.current {
        head.read_next_char();
    }

    if head.current == Some(NEW_LINE) {
        head.read_next_char();
        Some(ScanMatch {
            token: Token::LineBreak,
            end: head,
        })
    } else {
        None
    }
}

fn match_end_of_input<'a>(scanner: &Scanner<'a>) -> Option<ScanMatch<'a>> {
    let mut head = scanner.read_head.clone();

    while let Some(SPACE) = head.current {
        head.read_next_char();
    }

    if head.current.is_none() {
        Some(ScanMatch {
            token: Token::EndOfInput,
            end: head,
        })
    } else {
        None
    }
}

fn match_escaped_markup_text<'a>(scanner: &Scanner<'a>) -> Option<ScanMatch<'a>> {
    let mut head = scanner.read_head.clone();

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
    let text = &scanner.input[i1..i2];

    Some(ScanMatch {
        token: Token::MarkupText(text),
        end: head,
    })
}

fn match_raw_fragment<'a>(scanner: &Scanner<'a>) -> Option<ScanMatch<'a>> {
    let mut head = scanner.read_head.clone();

    let i1 = head.index;

    while head
        .current
        .is_some_and(|c| !(c == BACKTICK || c == NEW_LINE))
    {
        head.read_next_char();
    }

    let i2 = head.index;
    let text = &scanner.input[i1..i2];

    if i1 == i2 {
        None
    } else {
        Some(ScanMatch {
            token: Token::RawFragment(text),
            end: head,
        })
    }
}

fn match_data_value<'a>(scanner: &Scanner<'a>) -> Option<ScanMatch<'a>> {
    let mut head = scanner.read_head.clone();

    let i1 = head.index;

    while head
        .current
        .is_some_and(|c| !(c == SPACE || c == NEW_LINE || c == VERTICAL_BAR))
    {
        head.read_next_char();
    }

    let i2 = head.index;
    let text = &scanner.input[i1..i2];

    while head.current.is_some_and(|c| c == SPACE) {
        head.read_next_char();
    }

    if i1 == i2 {
        None
    } else {
        Some(ScanMatch {
            //TODO: meh amounts of ceremony here
            token: Token::DataValue(text),
            end: head,
        })
    }
}

fn match_markup_text<'a>(scanner: &Scanner<'a>) -> Option<ScanMatch<'a>> {
    let mut head = scanner.read_head.clone();

    let i1 = head.index;

    while head.current.is_some_and(|c| !MARKUP_CHARS.contains(&c)) {
        head.read_next_char();
    }

    let i2 = head.index;
    let text = &scanner.input[i1..i2];

    if i1 == i2 {
        None
    } else {
        Some(ScanMatch {
            token: Token::MarkupText(text),
            end: head,
        })
    }
}

fn match_title_text<'a>(scanner: &Scanner<'a>) -> Option<ScanMatch<'a>> {
    let mut head = scanner.read_head.clone();

    let i1 = head.index;

    while head.current.is_some_and(|c| !MARKUP_CHARS.contains(&c)) {
        head.read_next_char();
    }

    let i2 = head.index;
    let text = &scanner.input[i1..i2];

    if i1 == i2 {
        None
    } else {
        Some(ScanMatch {
            token: Token::TitleText(text),
            end: head,
        })
    }
}

fn match_parameter_value<'a>(scanner: &Scanner<'a>) -> Option<ScanMatch<'a>> {
    let mut head = scanner.read_head.clone();

    let i1 = head.index;

    while head
        .current
        .is_some_and(|c| c.is_alphanumeric() || c == UNDERSCORE || c == DASH || c == FULL_STOP)
    {
        head.read_next_char();
    }

    let i2 = head.index;
    let text = &scanner.input[i1..i2];

    if i1 == i2 {
        return None;
    }

    while head.current.is_some_and(|c| c == SPACE) {
        head.read_next_char();
    }

    Some(ScanMatch {
        token: Token::BlockParameterValue(text),
        end: head,
    })
}

fn match_parameter_name<'a>(scanner: &Scanner<'a>) -> Option<ScanMatch<'a>> {
    let mut head = scanner.read_head.clone();

    let i1 = head.index;

    while head
        .current
        .is_some_and(|c| c.is_alphanumeric() || c == UNDERSCORE || c == DASH || c == FULL_STOP)
    {
        head.read_next_char();
    }

    let i2 = head.index;
    let text = &scanner.input[i1..i2];

    if i1 == i2 {
        return None;
    }

    while head.current.is_some_and(|c| c == SPACE) {
        head.read_next_char();
    }

    Some(ScanMatch {
        token: Token::BlockParameterName(text),
        end: head,
    })
}

fn match_data_identifier<'a>(scanner: &Scanner<'a>) -> Option<ScanMatch<'a>> {
    let mut head = scanner.read_head.clone();

    let i1 = head.index;

    while head
        .current
        .is_some_and(|c| c.is_alphanumeric() || c == UNDERSCORE || c == DASH || c == FULL_STOP)
    {
        head.read_next_char();
    }

    let i2 = head.index;
    let text = &scanner.input[i1..i2];

    if i1 == i2 {
        return None;
    }

    while head.current.is_some_and(|c| c == SPACE) {
        head.read_next_char();
    }

    Some(ScanMatch {
        token: Token::DataIdentifier(text),
        end: head,
    })
}

fn match_data_key_value_seperator<'a>(scanner: &Scanner<'a>) -> Option<ScanMatch<'a>> {
    let mut head = scanner.read_head.clone();

    if head.current == Some(COLON) {
        head.read_next_char()
    } else {
        return None;
    }

    while head.current.is_some_and(|c| c == SPACE) {
        head.read_next_char();
    }

    Some(ScanMatch {
        token: Token::DataKeyValueSeperator,
        end: head,
    })
}

fn match_data_list_seperator<'a>(scanner: &Scanner<'a>) -> Option<ScanMatch<'a>> {
    let mut head = scanner.read_head.clone();

    if head.current == Some(VERTICAL_BAR) {
        head.read_next_char()
    } else {
        return None;
    }

    while head.current.is_some_and(|c| c == SPACE) {
        head.read_next_char();
    }

    Some(ScanMatch {
        token: Token::DataListSeperator,
        end: head,
    })
}

//TODO: This should probably be seperate functions for references, metadata
fn match_data_directive<'a>(scanner: &Scanner<'a>) -> Option<ScanMatch<'a>> {
    let mut head = scanner.read_head.clone();

    let i1 = head.index;

    if head.current == Some(AT_SIGN) {
        head.read_next_char()
    } else {
        return None;
    }

    while head.current.is_some_and(|c| c.is_alphanumeric()) {
        head.read_next_char();
    }

    let i2 = head.index;
    let text = &scanner.input[i1..i2];

    let token = match text {
        "@metadata" => Token::MetadataDirective,
        "@references" => Token::ReferencesDirective,
        _ => Token::UnknownDirective(text),
    };

    Some(ScanMatch { token, end: head })
}

fn match_container_directive<'a>(scanner: &Scanner<'a>) -> Option<ScanMatch<'a>> {
    let mut head = scanner.read_head.clone();

    let i1 = head.index;

    if head.current == Some(EXCLAMATION_MARK) {
        head.read_next_char()
    } else {
        return None;
    }

    while head.current.is_some_and(|c| c.is_alphanumeric()) {
        head.read_next_char();
    }

    let i2 = head.index;
    let text = &scanner.input[i1..i2];

    let token = match text {
        "!info" => Token::InfoContainerDirective,
        _ => Token::UnknownDirective(text),
    };

    Some(ScanMatch { token, end: head })
}

//TODO: should be seperate match functions?
fn match_block_directive<'a>(scanner: &Scanner<'a>) -> Option<ScanMatch<'a>> {
    let mut head = scanner.read_head.clone();

    let i1 = head.index;

    if head.current == Some(HASH) {
        head.read_next_char()
    } else {
        return None;
    }

    while head.current.is_some_and(|c| c.is_alphanumeric()) {
        head.read_next_char();
    }

    let i2 = head.index;
    let text = &scanner.input[i1..i2];

    let token = match text {
        "#paragraph" => Token::ParagraphDirective,
        "#list" => Token::ListDirective,
        "#code" => Token::CodeDirective,
        _ => Token::UnknownDirective(text),
    };

    Some(ScanMatch { token, end: head })
}

fn match_subsection_directive<'a>(scanner: &Scanner<'a>) -> Option<ScanMatch<'a>> {
    let mut head = scanner.read_head.clone();

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
        token: Token::SubSectionDirective,
        end: head,
    })
}

fn match_section_directive<'a>(scanner: &Scanner<'a>) -> Option<ScanMatch<'a>> {
    let mut head = scanner.read_head.clone();

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
        token: Token::SectionDirective,
        end: head,
    })
}

fn match_title_directive<'a>(scanner: &Scanner<'a>) -> Option<ScanMatch<'a>> {
    let mut head = scanner.read_head.clone();

    if head.current == Some(SLASH) {
        head.read_next_char()
    } else {
        return None;
    }

    while head.current.is_some_and(|c| c == SPACE) {
        head.read_next_char();
    }

    Some(ScanMatch {
        token: Token::TitleDirective,
        end: head,
    })
}

fn match_container_start<'a>(scanner: &Scanner<'a>) -> Option<ScanMatch<'a>> {
    let mut head = scanner.read_head.clone();

    for char in CONTAINER_START_PATTERN.chars() {
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
        token: Token::DelimitedContainerStart,
        end: head,
    })
}

fn match_container_end<'a>(scanner: &Scanner<'a>) -> Option<ScanMatch<'a>> {
    let mut head = scanner.read_head.clone();

    for char in CONTAINER_END_PATTERN.chars() {
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
        token: Token::DelimitedContainerEnd,
        end: head,
    })
}

fn match_unknown<'a>(scanner: &Scanner<'a>) -> ScanMatch<'a> {
    let mut head = scanner.read_head.clone();

    let i1 = head.index;

    while head.current.is_some_and(|c| !(c == SPACE || c == NEW_LINE)) {
        head.read_next_char();
    }

    let i2 = head.index;
    let text = &scanner.input[i1..i2];

    ScanMatch {
        token: Token::Unknown(text),
        end: head,
    }
}

fn match_generic<'a>(scanner: &Scanner<'a>) -> ScanMatch<'a> {
    if let Some(blockbreak) = match_blockbreak(scanner) {
        blockbreak
    } else if let Some(linebreak) = match_linebreak(scanner) {
        linebreak
    } else if let Some(end_of_input) = match_end_of_input(scanner) {
        end_of_input
    } else {
        match_unknown(scanner)
    }
}

//TODO: do we really need this?
fn match_data_identifier_at_start<'a>(scanner: &Scanner<'a>) -> Option<ScanMatch<'a>> {
    if scanner.position().column != 0 {
        return None;
    }
    match_data_identifier(scanner)
}

//TODO: do we really need this?
fn match_list_bullet_at_start<'a>(scanner: &Scanner<'a>) -> Option<ScanMatch<'a>> {
    if scanner.position().column != 0 {
        return None;
    }
    match_list_bullet(scanner)
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
const LIST_BULLET_AT_START: Matcher = match_list_bullet_at_start;
const RAW_DELIMITER: Matcher = match_raw_delimiter;
const RAW_FRAGMENT: Matcher = match_raw_fragment;
const LINK_OPENING: Matcher = match_link_opening_delimiter;
const LINK_CLOSING: Matcher = match_link_closing_delimiter;
const LINK_TO_REFERENCE_JOINER: Matcher = match_link_to_reference_joiner;
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
const DATA_IDENTIFIER_AT_START: Matcher = match_data_identifier_at_start;
const DATA_IDENTIFIER: Matcher = match_data_identifier;
const DATA_KEY_VALUE_SEP: Matcher = match_data_key_value_seperator;
const DATA_LIST_SEP: Matcher = match_data_list_seperator;
const DATA_VALUE: Matcher = match_data_value;
const CODE_DELIMITER: Matcher = match_code_delimiter;
const CODE_BLOCK: Matcher = match_code_block;

const SCAN_BLOCK_START: &[Matcher] = &[
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
    STRONG_DELIMITER,
    EMPHASIS_DELIMITER,
    STRIKETHROUGH_DELIMITER,
    ESCAPED_TEXT,
    MARKUP_TEXT_SPACE,
    MARKUP_TEXT,
];

const SCAN_LIST_MARKUP: &[Matcher] = &[
    RAW_DELIMITER,
    LIST_BULLET_AT_START,
    LINK_OPENING,
    LINK_CLOSING,
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
    PARAMETER_NAME,
];

const SCAN_HEADER_VALUE: &[Matcher] = &[PARAMETER_VALUE];

const SCAN_STRUCTURED_DATA: &[Matcher] = &[
    DATA_IDENTIFIER_AT_START,
    DATA_KEY_VALUE_SEP,
    DATA_LIST_SEP,
    DATA_VALUE,
];

const SCAN_LINK_REFERENCE: &[Matcher] = &[LINK_TO_REFERENCE_JOINER, DATA_IDENTIFIER];

const SCAN_CODE: &[Matcher] = &[CODE_DELIMITER, CODE_BLOCK];

const SCAN_GENERIC: &[Matcher] = &[];
