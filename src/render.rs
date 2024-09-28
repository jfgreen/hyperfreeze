use std::io;

use crate::parse::{Block, Document, Style, TextRun};

//TODO: Have some kind of helper for writing HTML, with indent
pub fn render_html(document: &Document, out: &mut impl io::Write) -> io::Result<()> {
    write!(out, "<!DOCTYPE html>\n")?;
    write!(out, "<html>\n")?;
    write!(out, "  <head>\n")?;
    write!(out, "    <meta charset=\"UTF-8\">\n")?;
    write!(out, "    <title>{}</title>\n", document.metadata.title)?;
    write!(out, "  </head>\n")?;
    write!(out, "  <body>\n")?;

    for block in document.blocks.iter() {
        match block {
            Block::Paragraph(text_runs) => render_paragraph(&text_runs, out)?,
        }
    }

    write!(out, "  </body>\n")?;
    write!(out, "</html>")?;

    Ok(())
}

fn render_paragraph(text_runs: &[TextRun], out: &mut impl io::Write) -> io::Result<()> {
    write!(out, "    <p>\n")?;
    write!(out, "      ")?;
    for run in text_runs.iter() {
        match run.style {
            Style::None => (),
            Style::Bold => write!(out, "<strong>")?,
            Style::Emphasis => write!(out, "<em>")?,
            Style::Strikethrough => write!(out, "<s>")?,
            Style::Raw => write!(out, "<code>")?,
        };

        out.write(run.text.as_bytes())?;

        match run.style {
            Style::None => (),
            Style::Bold => write!(out, "</strong>")?,
            Style::Emphasis => write!(out, "</em>")?,
            Style::Strikethrough => write!(out, "</s>")?,
            Style::Raw => write!(out, "</code>")?,
        };
    }
    write!(out, "\n    </p>\n")?;
    Ok(())
}
