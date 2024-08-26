use std::io;

use crate::parse::{Block, Document, Style, TextRun};

//TODO: Have some kind of helper for writing HTML, with indent
pub fn render_html(document: &Document, out: &mut impl io::Write) -> io::Result<()> {
    let head = concat!(
        "<!DOCTYPE html>\n",
        "<html>\n",
        "  <head>\n",
        "    <meta charset=\"UTF-8\">\n",
        "    <title>Page</title>\n",
        "  </head>\n",
        "  <body>\n"
    );

    out.write(head.as_bytes())?;

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