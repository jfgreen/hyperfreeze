#[derive(PartialEq, Eq, Debug, Default)]
pub struct Document {
    pub metadata: Metadata,
    pub contents: Box<[Element]>,
    pub references: Box<[Reference]>,
}

#[derive(PartialEq, Eq, Debug, Default)]
pub struct Metadata {
    pub id: Option<String>,
    pub title: Option<String>,
    pub tags: Option<Box<[String]>>,
}

//TODO: Might be nice to also have an optional title and/or description
#[derive(PartialEq, Eq, Debug)]
pub struct Reference {
    pub id: String,
    pub link: String,
}

#[derive(PartialEq, Eq, Debug)]
pub enum Element {
    Block(Block),
    Container(Container),
    Section(Section),
}

//TODO: Footnotes
#[derive(PartialEq, Eq, Debug)]
pub enum Block {
    Paragraph(Box<[TextRun]>),
    List(List),
}

#[derive(PartialEq, Eq, Debug)]
pub struct Container {
    pub content: Box<[Block]>,
    pub kind: ContainerKind,
}

//TODO: Consider enforcing only two levels of section
#[derive(PartialEq, Eq, Debug)]
pub struct Section {
    pub content: Box<[Element]>,
    pub title: String,
    pub level: usize,
}

#[derive(PartialEq, Eq, Debug)]
pub enum ContainerKind {
    Info,
    //TODO: Other kinds of alert
}

#[derive(PartialEq, Eq, Debug)]
pub struct List {
    pub items: Box<[ListItem]>,
    pub kind: ListKind,
}

#[derive(PartialEq, Eq, Debug)]
pub enum ListItem {
    Text(Box<[TextRun]>),
    SubList(Box<[ListItem]>),
}

#[derive(PartialEq, Eq, Debug)]
pub enum ListKind {
    Unordered,
    Ordered,
}

#[derive(PartialEq, Eq, Debug)]
pub struct TextRun {
    pub text: String,
    pub style: Style,
}

#[derive(PartialEq, Eq, Debug)]
pub enum Style {
    None,
    Strong,
    Emphasis,
    Strikethrough,
    Raw,
    Link(String),
}
