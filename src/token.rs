pub trait TokenSpec<'a>: TryFrom<Token<'a>> {
    const NAME: TokenName;
}

#[derive(PartialEq, Eq, Debug)]
pub struct TokenName(pub &'static str);

#[derive(PartialEq, Eq, Clone, Copy, Debug)]
pub struct Indent {
    pub space_count: u8,
}

macro_rules! token {

    ($name:ident) => {
        #[derive(Clone, Copy, Debug)]
        pub struct $name;

        impl<'t> TokenSpec<'t> for $name {
            const NAME: TokenName = TokenName(stringify!($name));
        }

        impl<'t> TryFrom<Token<'t>> for $name {
            type Error = ();

            fn try_from(token: Token<'t>) -> Result<Self, Self::Error> {
                match token {
                    Token::$name => Ok(Self),
                    _ => Err(()),
                }
            }
        }
    };

    ($name:ident $(<$lifetime:lifetime>)? ($value:ty)) => {
        #[allow(dead_code)]
        #[derive(Clone, Copy, Debug)]
        pub struct $name$(<$lifetime>)?(pub $value);

        impl<'t$(, $lifetime)?> TokenSpec<'t> for $name$(<$lifetime>)?
        $(where 't: $lifetime)?
        {
            const NAME: TokenName = TokenName(stringify!($name));
        }

        impl<'t$(, $lifetime)?> TryFrom<Token<'t>> for $name$(<$lifetime>)?
        $(where 't: $lifetime)?
        {
            type Error = ();

            fn try_from(token: Token<'t>) -> Result<Self, Self::Error> {
                match token {
                    Token::$name(value) => Ok(Self(value)),
                    _ => Err(()),
                }
            }
        }

    };
}

macro_rules! token_name_pattern {
    ($name:ident) => {
        Token::$name
    };

    ($name:ident($value:ty)) => {
        Token::$name(_)
    };
}

macro_rules! tokens {
    ($($name:ident $(<$lifetime:lifetime>)? $(($value:ty))?),+ $(,)?) => {
        $(token!($name $(<$lifetime>)?$(($value))?);)+

        #[derive(Clone, Copy, Debug)]
        //TODO: 'a just happening to match here is meh
        pub enum Token<'a> {
            $($name$(($value))?,)+
        }

        impl Token<'_> {
            pub fn name(&self) -> TokenName {
                match self {
                    $(token_name_pattern!($name$(($value))?) => $name::NAME,)+
                }
            }
        }
    };
}

tokens!(
    MetadataDirective,
    ReferencesDirective,
    ParagraphDirective,
    ListDirective,
    CodeDirective,
    InfoContainerDirective,
    EndOfInput,
    TitleDirective,
    SectionDirective,
    SubSectionDirective,
    BlockParametersStart,
    BlockParametersEnd,
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
    CodeDelimiter,
    DelimitedContainerStart,
    DelimitedContainerEnd,
    UnknownDirective<'a>(&'a str),
    BlockParameterName<'a>(&'a str),
    BlockParameterValue<'a>(&'a str),
    DataIdentifier<'a>(&'a str),
    DataValue<'a>(&'a str),
    LinkToReference<'a>(&'a str),
    TitleText<'a>(&'a str),
    MarkupText<'a>(&'a str),
    RawFragment<'a>(&'a str),
    Code<'a>(&'a str),
    Unknown<'a>(&'a str),
    ListBullet(Indent),
);
