use std::io;

use crate::document as doc;

//TODO: Have some kind of helper for writing HTML, with indent
pub fn render_html(document: &doc::Document, out: &mut impl io::Write) -> io::Result<()> {
    let title = &document.title;

    writeln!(out, "<!DOCTYPE html>")?;
    writeln!(out, "<html>")?;
    writeln!(out, "  <head>")?;
    writeln!(out, "    <meta charset=\"UTF-8\">")?;
    writeln!(out, "    <title>{title}</title>")?;
    writeln!(out, "  </head>")?;
    writeln!(out, "  <body>")?;
    writeln!(out, "    <main>")?;
    writeln!(out, "      <h1>{title}</h1>")?;

    for element in &document.contents {
        render_element(element, out)?;
    }

    writeln!(out, "    </main>")?;
    writeln!(out, "  </body>")?;
    write!(out, "</html>")?;

    Ok(())
}

fn render_element(element: &doc::Element, out: &mut impl io::Write) -> io::Result<()> {
    match element {
        doc::Element::Block(block) => render_block(block, out)?,
        doc::Element::Container(container) => render_container(container, out)?,
        doc::Element::Section(section) => render_section(section, out)?,
    }

    Ok(())
}

fn render_section_element(
    element: &doc::SectionElement,
    out: &mut impl io::Write,
) -> io::Result<()> {
    match element {
        doc::SectionElement::Block(block) => render_block(block, out)?,
        doc::SectionElement::Container(container) => render_container(container, out)?,
        doc::SectionElement::SubSection(subsection) => render_subsection(subsection, out)?,
    }

    Ok(())
}

fn render_subsection_element(
    element: &doc::SubSectionElement,
    out: &mut impl io::Write,
) -> io::Result<()> {
    match element {
        doc::SubSectionElement::Block(block) => render_block(block, out)?,
        doc::SubSectionElement::Container(container) => render_container(container, out)?,
    }

    Ok(())
}

fn render_block(block: &doc::Block, out: &mut impl io::Write) -> io::Result<()> {
    match block {
        doc::Block::Paragraph(text) => render_text(text, out)?,
        doc::Block::List(list) => render_list(list, out)?,
        doc::Block::Code(code) => render_code(code, out)?,
    }
    Ok(())
}

fn render_container(container: &doc::Container, out: &mut impl io::Write) -> io::Result<()> {
    //TODO: Actually display the container somehow
    for block in &container.content {
        render_block(block, out)?;
    }
    Ok(())
}

fn render_section(section: &doc::Section, out: &mut impl io::Write) -> io::Result<()> {
    writeln!(out, "<section>")?;
    writeln!(out, "<h2>{}</h2>", section.heading)?;

    for element in &section.content {
        render_section_element(element, out)?;
    }

    writeln!(out, "</section>")?;
    Ok(())
}

fn render_subsection(subsection: &doc::SubSection, out: &mut impl io::Write) -> io::Result<()> {
    writeln!(out, "<section>")?;
    writeln!(out, "<h3>{}</h3>", subsection.heading)?;

    for element in &subsection.content {
        render_subsection_element(element, out)?;
    }

    writeln!(out, "</section>")?;
    Ok(())
}

fn render_text(text_runs: &[doc::TextRun], out: &mut impl io::Write) -> io::Result<()> {
    writeln!(out, "    <p>")?;
    writeln!(out, "      ")?;
    for run in text_runs {
        render_text_run(run, out)?;
    }
    writeln!(out, "    </p>")?;
    Ok(())
}

fn render_text_run(run: &doc::TextRun, out: &mut impl io::Write) -> io::Result<()> {
    use doc::Style;

    match &run.style {
        Style::None => (),
        Style::Strong => write!(out, "<strong>")?,
        Style::Emphasis => write!(out, "<em>")?,
        Style::Strikethrough => write!(out, "<s>")?,
        Style::Raw => write!(out, "<code>")?,
        //FIXME: This is wrong, we need to resolve the link
        Style::Link(link) => write!(out, "<a href={link}>")?,
    }

    out.write_all(run.text.as_bytes())?;

    match &run.style {
        Style::None => (),
        Style::Strong => write!(out, "</strong>")?,
        Style::Emphasis => write!(out, "</em>")?,
        Style::Strikethrough => write!(out, "</s>")?,
        Style::Raw => write!(out, "</code>")?,
        Style::Link(_) => write!(out, "</a>")?,
    }

    Ok(())
}

fn render_list(list: &doc::List, out: &mut impl io::Write) -> io::Result<()> {
    //TODO: Handle list style
    writeln!(out, "<ul>")?;
    for item in &list.items {
        render_list_item(item, out)?;
    }
    writeln!(out, "</ul>")?;
    Ok(())
}

fn render_list_item(item: &doc::ListItem, out: &mut impl io::Write) -> io::Result<()> {
    write!(out, "<li>")?;
    match item {
        doc::ListItem::Text(text) => render_text(text, out)?,
        doc::ListItem::SubList(sub_list) => {
            writeln!(out, "<ul>")?;
            for item in sub_list {
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
