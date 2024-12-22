use std::io;

use crate::parse::{Block, Document, Paragraph, Style};

//TODO: Have some kind of helper for writing HTML, with indent
pub fn render_html(document: &Document, out: &mut impl io::Write) -> io::Result<()> {
    writeln!(out, "<!DOCTYPE html>")?;
    writeln!(out, "<html>")?;
    writeln!(out, "  <head>")?;
    writeln!(out, "    <meta charset=\"UTF-8\">")?;
    writeln!(out, "    <title>{}</title>", document.metadata.title)?;
    writeln!(out, "  </head>")?;
    writeln!(out, "  <body>")?;

    for block in document.blocks.iter() {
        match block {
            Block::Paragraph(paragraph) => render_paragraph(paragraph, out)?,
            Block::Alert(alert) => {
                for paragraph in alert.content.iter() {
                    render_paragraph(paragraph, out)?;
                }
            }
        }
    }

    writeln!(out, "  </body>")?;
    write!(out, "</html>")?;

    Ok(())
}

fn render_paragraph(paragraph: &Paragraph, out: &mut impl io::Write) -> io::Result<()> {
    writeln!(out, "    <p>")?;
    writeln!(out, "      ")?;
    let text_runs = paragraph.runs();
    for run in text_runs.iter() {
        match run.style {
            Style::None => (),
            Style::Strong => write!(out, "<strong>")?,
            Style::Emphasis => write!(out, "<em>")?,
            Style::Strikethrough => write!(out, "<s>")?,
            Style::Raw => write!(out, "<code>")?,
        };

        out.write_all(run.text.as_bytes())?;

        match run.style {
            Style::None => (),
            Style::Strong => write!(out, "</strong>")?,
            Style::Emphasis => write!(out, "</em>")?,
            Style::Strikethrough => write!(out, "</s>")?,
            Style::Raw => write!(out, "</code>")?,
        };
    }
    writeln!(out, "    </p>")?;
    Ok(())
}
