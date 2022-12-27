use std::{collections::BTreeMap, fmt::Display, path::Path};

use crossterm::style::{Color, Stylize};

use crate::script::token::{Position, Span};

#[derive(Debug, Clone, Copy)]
pub enum DiagnosticKind {
    Error,
    Note,
    Help,
}

#[must_use]
pub fn get_color(kind: DiagnosticKind) -> Color {
    match kind {
        DiagnosticKind::Error => Color::Red,
        DiagnosticKind::Note => Color::Blue,
        DiagnosticKind::Help => Color::Yellow,
        // UnderscoreType::Warning => Color::Yellow,
        // UnderscoreType::Info => Color::Cyan,
    }
}

#[derive(Debug, Clone, Copy)]
pub enum AnnotationKind {
    Note,
    Help,
}

#[derive(Debug)]
struct AnnotationSet {
    span: Span,
    annotations: Vec<AnnotationPriv>,
}

#[derive(Debug)]
struct AnnotationPriv {
    message: String,
    kind: AnnotationKind,
}

pub struct Annotation {
    pub kind: AnnotationKind,
    pub message: String,
    pub at: Span,
}

pub struct Diagnostic<'r> {
    kind: DiagnosticKind,
    annotations: Vec<AnnotationSet>,
    notes: Vec<(AnnotationKind, String)>,
    message: String,
    annotated_notes: BTreeMap<Position, Diagnostic<'r>>,
    file: &'r Path,
    script: &'r str,
}

pub struct DiagnosticData {
    pub message: String,
    pub spans: Vec<Span>,
    pub annotations: Vec<Annotation>,
    pub notes: Vec<(AnnotationKind, String)>,
}

impl DiagnosticData {
    #[must_use]
    pub fn new<S: ToString + ?Sized>(message: &S) -> Self {
        Self {
            message: message.to_string(),
            spans: Vec::new(),
            annotations: Vec::new(),
            notes: Vec::new(),
        }
    }

    #[must_use]
    pub fn add_note(mut self, kind: AnnotationKind, message: String) -> Self {
        self.notes.push((kind, message));
        self
    }

    #[must_use]
    pub fn add_span(mut self, sp: Span) -> Self {
        self.spans.push(sp);
        self
    }

    #[must_use]
    pub fn add_annotation<S: ToString + ?Sized>(
        mut self,
        at: Span,
        kind: AnnotationKind,
        message: &S,
    ) -> Self {
        self.annotations.push(Annotation {
            kind,
            message: message.to_string(),
            at,
        });
        self
    }
}

impl<'r> Diagnostic<'r> {
    fn find_spans(
        spans: Vec<Span>,
        kind: DiagnosticKind,
        message: &str,
        file: &'r Path,
        script: &'r str,
    ) -> (Vec<AnnotationSet>, BTreeMap<Position, Diagnostic<'r>>) {
        let mut annotation_sets: Vec<AnnotationSet> = Vec::new();
        let mut annotated_notes = BTreeMap::new();

        for at in spans {
            let mut index = 0;
            let mut insert_at: u8 = 2;

            for (i, set) in annotation_sets.iter_mut().enumerate() {
                if set.span.overlaps(at) {
                    set.span = set.span.join(at);
                    insert_at = 0;
                    break;
                }

                if !at.is_singleline()
                    && set.span.is_singleline()
                    && at.end.line == set.span.start.line
                {
                    // Banish the span
                    annotated_notes.insert(
                        at.start,
                        Diagnostic::new(
                            kind,
                            DiagnosticData {
                                message: String::from(message),
                                spans: vec![at],
                                annotations: Vec::new(),
                                notes: Vec::new(),
                            },
                            file,
                            script,
                        ),
                    );
                    insert_at = 0;
                    break;
                }

                if at.start > set.span.start {
                    index = i;
                } else {
                    insert_at = 1;
                    break;
                }
            }

            match insert_at {
                0 => (),
                1 => annotation_sets.insert(
                    index,
                    AnnotationSet {
                        span: at,
                        annotations: Vec::new(),
                    },
                ),
                2 => annotation_sets.push(AnnotationSet {
                    span: at,
                    annotations: Vec::new(),
                }),
                _ => unreachable!(),
            }
        }

        (annotation_sets, annotated_notes)
    }

    #[must_use]
    pub fn new(
        kind: DiagnosticKind,
        data: DiagnosticData,
        file: &'r Path,
        script: &'r str,
    ) -> Self {
        let (mut annotation_sets, mut annotated_notes) =
            Diagnostic::find_spans(data.spans, kind, &data.message, file, script);

        for ann in data.annotations {
            let mut index = 0;
            let mut insert_at: u8 = 2;

            for (i, annotation) in annotation_sets.iter_mut().enumerate() {
                if annotation.span == ann.at {
                    // If there's a common span, attach both messages to one span.
                    annotation.annotations.push(AnnotationPriv {
                        message: ann.message.clone(),
                        kind: ann.kind,
                    });

                    insert_at = 0;
                    break;
                } else if annotation.span.overlaps(ann.at) {
                    // If there is an overlap, banish the note to annotated_notes.
                    annotated_notes.insert(
                        ann.at.start,
                        Diagnostic::new(
                            match ann.kind {
                                AnnotationKind::Help => DiagnosticKind::Help,
                                AnnotationKind::Note => DiagnosticKind::Note,
                            },
                            DiagnosticData {
                                message: ann.message.clone(),
                                spans: vec![ann.at],
                                annotations: Vec::new(),
                                notes: Vec::new(),
                            },
                            file,
                            script,
                        ),
                    );

                    insert_at = 0;
                    break;
                }

                if ann.at.start > annotation.span.start {
                    index = i;
                } else {
                    insert_at = 1;
                    break;
                }
            }

            // If the function has not broken before, the annotation should be inserted at the given index.
            match insert_at {
                0 => (),
                1 => annotation_sets.insert(
                    index,
                    AnnotationSet {
                        span: ann.at,
                        annotations: vec![AnnotationPriv {
                            message: ann.message.clone(),
                            kind: ann.kind,
                        }],
                    },
                ),
                2 => annotation_sets.push(AnnotationSet {
                    span: ann.at,
                    annotations: vec![AnnotationPriv {
                        message: ann.message.clone(),
                        kind: ann.kind,
                    }],
                }),
                _ => unreachable!(),
            }
        }

        Diagnostic {
            kind,
            annotations: annotation_sets,
            notes: Vec::new(),
            message: data.message,
            annotated_notes,
            file,
            script,
        }
    }
}

impl<'r> Display for Diagnostic<'r> {
    #[allow(clippy::too_many_lines)]
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        // Display the main message
        writeln!(
            f,
            "{}{} {}",
            match self.kind {
                DiagnosticKind::Error => "error".red().bold(),
                DiagnosticKind::Note => "note".blue().bold(),
                DiagnosticKind::Help => "help".yellow().bold(),
            },
            ":".white().bold(),
            self.message.clone().white().bold()
        )?;
        let vertical = "|".blue().bold();

        // Only execute the rest if there are any annotations.
        let note_indent = if let Some(last) = self.annotations.last() {
            let first = self.annotations.first().unwrap();
            // Calculate the necessary indent (how much space to leave for line numbers).
            let indent = last.span.end.line.to_string().len();
            // Write the file path
            writeln!(
                f,
                "{:indent$}{} {}:{}:{}",
                "",
                "-->".blue().bold(),
                self.file.display(),
                first.span.start.line,
                first.span.start.column
            )?;

            let mut lines = self.script.lines().skip(first.span.start.line - 1);

            // First "    | "
            writeln!(f, "{:indent$} {vertical}", "")?;

            let mut annotations = self.annotations.iter().peekable();

            // Do for each annotation
            while let Some(ann) = annotations.next() {
                if ann.span.is_singleline() {
                    // If single line, only render the line of code
                    writeln!(
                        f,
                        "{:indent$} {vertical} {}",
                        ann.span.start.line.to_string().blue().bold(),
                        lines.next().unwrap()
                    )?;
                    // And simple squiggles
                    write!(
                        f,
                        "{:<indent$} {vertical} {:squiggle_offset$}{}",
                        "",
                        "",
                        "^".repeat(ann.span.end.column - ann.span.start.column)
                            .with(get_color(self.kind))
                            .bold(),
                        squiggle_offset = ann.span.start.column - 1
                    )?;
                } else {
                    // Otherwise
                    if ann.span.start.column == 1 {
                        // If range starts at column 1, start with '/'
                        writeln!(
                            f,
                            "{:<indent$} {vertical} {} {}",
                            ann.span.start.line.to_string().blue().bold(),
                            "/".with(get_color(self.kind)).bold(),
                            lines.next().unwrap()
                        )?;
                    } else {
                        // Otherwise with an indicator of where the span starts
                        writeln!(
                            f,
                            "{:indent$} {vertical} {}",
                            ann.span.start.line.to_string().blue().bold(),
                            lines.next().unwrap()
                        )?;
                        writeln!(
                            f,
                            "{:indent$} {vertical} {}",
                            "",
                            (String::from(" ") + &"_".repeat(ann.span.start.column - 2) + "^")
                                .with(get_color(self.kind))
                                .bold()
                        )?;
                    }

                    // Then add '|' for each line in span.
                    for i in ann.span.start.line..ann.span.end.line {
                        writeln!(
                            f,
                            "{:<indent$} {vertical} {} {}",
                            (i + 1).to_string().blue().bold(),
                            "|".with(get_color(self.kind)).bold(),
                            lines.next().unwrap()
                        )?;
                    }

                    // And end with ending indicator.
                    write!(
                        f,
                        "{:indent$} {vertical} {}",
                        "",
                        (String::from("|") + &"_".repeat(ann.span.start.column) + "^")
                            .with(get_color(self.kind))
                            .bold()
                    )?;
                }

                let mut messages: Vec<(usize, Vec<&AnnotationPriv>)> = vec![(
                    ann.span.end.column,
                    ann.annotations
                        .iter()
                        .filter(|x| !x.message.is_empty())
                        .collect(),
                )];

                // While we can, we put squiggles in the same line.
                let mut previous = ann;
                let mut next_ann = annotations.peek().copied();
                while let Some(next) = next_ann {
                    if next.span.start.line == previous.span.end.line {
                        annotations.next();
                        write!(
                            f,
                            "{:squiggle_offset$}{}",
                            "",
                            "^".repeat(next.span.end.column - next.span.start.column)
                                .with(get_color(self.kind))
                                .bold(),
                            squiggle_offset = next.span.start.column - previous.span.end.column
                        )?;

                        messages.push((
                            next.span.end.column,
                            next.annotations
                                .iter()
                                .filter(|x| !x.message.is_empty())
                                .collect(),
                        ));

                        previous = next;
                        next_ann = annotations.peek().copied();
                    } else {
                        break;
                    }
                }

                let l = messages.len();
                let mut messages: Vec<(usize, Vec<&AnnotationPriv>)> = messages
                    .into_iter()
                    .enumerate()
                    .filter(|(i, (_, x))| !x.is_empty() || *i == l - 1)
                    .map(|x| x.1)
                    .collect();

                // We first dispatch the last message, since it can follow the squiggles immediately.
                if let Some(v) = messages.last().unwrap().1.first() {
                    write!(
                        f,
                        "{}",
                        format!(
                            " {}: {}",
                            match v.kind {
                                AnnotationKind::Note => "note",
                                AnnotationKind::Help => "help",
                            },
                            v.message
                        )
                        .with(get_color(self.kind))
                        .bold()
                    )?;
                }

                if !messages.last_mut().unwrap().1.is_empty() {
                    messages.last_mut().unwrap().1.remove(0);
                }
                writeln!(f)?;

                let mut offsets: Vec<usize> = messages.iter().map(|x| x.0).collect();
                offsets.pop();

                // And then for each annotation
                for (offset, msg) in messages.into_iter().rev() {
                    // For each message.
                    for ann in msg {
                        write!(f, "{:indent$} {vertical} ", "")?;
                        let mut last = 1;

                        // We write the `|` for other messages.
                        for off in &offsets {
                            write!(
                                f,
                                "{:ind$}{}",
                                "",
                                "|".with(get_color(self.kind)).bold(),
                                ind = *off - last - 1
                            )?;
                            last = *off;
                        }

                        // And then write the message itself.
                        writeln!(
                            f,
                            "{:ind$}{}",
                            "",
                            format!(
                                " {}: {}",
                                match ann.kind {
                                    AnnotationKind::Note => "note",
                                    AnnotationKind::Help => "help",
                                },
                                ann.message
                            )
                            .with(get_color(self.kind))
                            .bold(),
                            ind = offset - last - 1
                        )?;
                    }

                    offsets.pop();
                }

                // Advance the source display
                if let Some(next) = next_ann {
                    match next.span.start.line - previous.span.end.line {
                        1 => (),
                        2 => writeln!(
                            f,
                            "{:<indent$} {vertical} {}",
                            ann.span.start.line.to_string().blue().bold(),
                            lines.next().unwrap()
                        )?,
                        _ => writeln!(f, "{:indent$}{}", "", "...".blue().bold())?,
                    }
                }
            }

            indent
        } else {
            0
        };

        // Display notes.
        for note in &self.notes {
            writeln!(
                f,
                "{:note_indent$} = {}",
                "",
                format!(
                    "{}: {}",
                    match &note.0 {
                        AnnotationKind::Note => "note",
                        AnnotationKind::Help => "help",
                    },
                    note.1
                )
                .bold()
            )?;
        }

        // Display annotated notes
        if !self.annotated_notes.is_empty() {
            writeln!(f, "{:note_indent$} {vertical}", "")?;
        }

        for diagnostic in self.annotated_notes.values() {
            write!(f, "{diagnostic}")?;
        }

        Ok(())
    }
}
