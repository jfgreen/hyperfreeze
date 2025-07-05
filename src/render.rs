use std::io;

use crate::document::*;

//TODO: Have some kind of helper for writing HTML, with indent
pub fn render_html(document: &Document, out: &mut impl io::Write) -> io::Result<()> {
    //FIXME: Decide on if to enforce this, or to have a fallback?
    let title = &document.metadata.title;
    let title = title.as_ref().expect("document should have title");

    writeln!(out, "<!DOCTYPE html>")?;
    writeln!(out, "<html>")?;
    writeln!(out, "  <head>")?;
    writeln!(out, "    <meta charset=\"UTF-8\">")?;
    writeln!(out, "    <title>{}</title>", title)?;
    writeln!(out, "  </head>")?;
    writeln!(out, "  <body>")?;
    writeln!(out, "    <article>")?;
    writeln!(out, "      <h1>{}</h1>", title)?;

    for element in document.contents.iter() {
        render_element(element, out)?;
    }

    writeln!(out, "    </article>")?;
    writeln!(out, "  </body>")?;
    write!(out, "</html>")?;

    Ok(())
}

fn render_element(element: &Element, out: &mut impl io::Write) -> io::Result<()> {
    match element {
        Element::Block(block) => render_block(block, out)?,
        Element::Container(container) => render_container(container, out)?,
        Element::Section(section) => render_section(section, out)?,
    }

    Ok(())
}

fn render_block(block: &Block, out: &mut impl io::Write) -> io::Result<()> {
    match block {
        Block::Paragraph(text) => render_text(text, out)?,
        Block::List(list) => render_list(list, out)?,
        Block::Code(code) => render_code(code, out)?,
    };
    Ok(())
}

fn render_container(container: &Container, out: &mut impl io::Write) -> io::Result<()> {
    //TODO: Actually display the container somehow
    for block in container.content.iter() {
        render_block(block, out)?;
    }
    Ok(())
}

fn render_section(section: &Section, out: &mut impl io::Write) -> io::Result<()> {
    writeln!(out, "<section>")?;
    //TODO: This is a bit meh
    match section.level {
        1 => write!(out, "<h2>")?,
        2 => write!(out, "<h3>")?,
        _ => panic!(),
    }

    out.write_all(section.title.as_bytes())?;

    match section.level {
        1 => write!(out, "</h2>")?,
        2 => write!(out, "</h3>")?,
        _ => panic!(),
    }

    writeln!(out)?;

    for element in &section.content {
        render_element(element, out)?;
    }

    writeln!(out, "</section>")?;
    Ok(())
}

fn render_text(text_runs: &[TextRun], out: &mut impl io::Write) -> io::Result<()> {
    writeln!(out, "    <p>")?;
    writeln!(out, "      ")?;
    for run in text_runs.iter() {
        render_text_run(run, out)?;
    }
    writeln!(out, "    </p>")?;
    Ok(())
}

fn render_text_run(run: &TextRun, out: &mut impl io::Write) -> io::Result<()> {
    match &run.style {
        Style::None => (),
        Style::Strong => write!(out, "<strong>")?,
        Style::Emphasis => write!(out, "<em>")?,
        Style::Strikethrough => write!(out, "<s>")?,
        Style::Raw => write!(out, "<code>")?,
        //FIXME: This is wrong, we need to resolve the link
        Style::Link(link) => write!(out, "<a href={}>", link)?,
    };

    out.write_all(run.text.as_bytes())?;

    match &run.style {
        Style::None => (),
        Style::Strong => write!(out, "</strong>")?,
        Style::Emphasis => write!(out, "</em>")?,
        Style::Strikethrough => write!(out, "</s>")?,
        Style::Raw => write!(out, "</code>")?,
        Style::Link(_) => write!(out, "</a>")?,
    };
    Ok(())
}

fn render_list(list: &List, out: &mut impl io::Write) -> io::Result<()> {
    //TODO: Handle list style
    writeln!(out, "<ul>")?;
    for item in list.items.iter() {
        render_list_item(item, out)?;
    }
    writeln!(out, "</ul>")?;
    Ok(())
}

fn render_list_item(item: &ListItem, out: &mut impl io::Write) -> io::Result<()> {
    write!(out, "<li>")?;
    match item {
        ListItem::Text(text) => render_text(text, out)?,
        ListItem::SubList(sub_list) => {
            writeln!(out, "<ul>")?;
            for item in sub_list.iter() {
                render_list_item(item, out)?;
            }
            writeln!(out, "</ul>")?;
        }
    }
    write!(out, "</li>")?;
    Ok(())
}

fn render_code(code: &String, out: &mut impl io::Write) -> io::Result<()> {
    //TODO: Use code inside pre?
    writeln!(out, "<pre>")?;
    out.write_all(code.as_bytes())?;
    writeln!(out, "</pre>")?;
    Ok(())
}
