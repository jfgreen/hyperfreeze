use std::io;

use crate::document::*;

//TODO: Have some kind of helper for writing HTML, with indent
pub fn render_html(document: &Document, out: &mut impl io::Write) -> io::Result<()> {
    writeln!(out, "<!DOCTYPE html>")?;
    writeln!(out, "<html>")?;
    writeln!(out, "  <head>")?;
    writeln!(out, "    <meta charset=\"UTF-8\">")?;
    if let Some(title) = &document.metadata.title {
        writeln!(out, "    <title>{}</title>", title)?;
    }
    writeln!(out, "  </head>")?;
    writeln!(out, "  <body>")?;

    for element in document.contents.iter() {
        match element {
            Element::Block(block) => render_block(block, out)?,
            Element::Container(container) => {
                for block in container.content.iter() {
                    render_block(block, out)?;
                }
            }
            Element::Section(_) => todo!(),
        }
    }

    writeln!(out, "  </body>")?;
    write!(out, "</html>")?;

    Ok(())
}

fn render_block(block: &Block, out: &mut impl io::Write) -> io::Result<()> {
    match block {
        Block::Paragraph(text) => render_paragraph(text, out)?,
        Block::List(_) => todo!(),
    };
    Ok(())
}

fn render_paragraph(text_runs: &[TextRun], out: &mut impl io::Write) -> io::Result<()> {
    writeln!(out, "    <p>")?;
    writeln!(out, "      ")?;
    for run in text_runs.iter() {
        match run.style {
            Style::None => (),
            Style::Strong => write!(out, "<strong>")?,
            Style::Emphasis => write!(out, "<em>")?,
            Style::Strikethrough => write!(out, "<s>")?,
            Style::Raw => write!(out, "<code>")?,
            Style::Link(_) => todo!(),
        };

        out.write_all(run.text.as_bytes())?;

        match run.style {
            Style::None => (),
            Style::Strong => write!(out, "</strong>")?,
            Style::Emphasis => write!(out, "</em>")?,
            Style::Strikethrough => write!(out, "</s>")?,
            Style::Raw => write!(out, "</code>")?,
            Style::Link(_) => todo!(),
        };
    }
    writeln!(out, "    </p>")?;
    Ok(())
}
