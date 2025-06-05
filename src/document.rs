#[derive(PartialEq, Eq, Debug)]
pub struct Document {
    pub metadata: Metadata,
    pub contents: Box<[Element]>,
    pub references: Box<[Reference]>,
}

impl Default for Document {
    fn default() -> Self {
        Document {
            metadata: Metadata::default(),
            contents: Box::new([]),
            references: Box::new([]),
        }
    }
}

// TODO: Make id, title be explicty optional
#[derive(PartialEq, Eq, Debug, Default)]
pub struct Metadata {
    pub id: String,
    pub title: String,
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
    //Section(Section),
}

#[derive(PartialEq, Eq, Debug)]
pub enum Block {
    Paragraph(Box<[TextRun]>),
    List(Box<[ListItem]>),
}

#[derive(PartialEq, Eq, Debug)]
pub struct Container {
    pub content: Box<[Block]>,
    pub kind: ContainerKind,
}

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
pub enum ListItem {
    Text(Box<[TextRun]>),
    SubList(Box<[ListItem]>),
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
