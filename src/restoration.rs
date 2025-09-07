use crate::Config;
use crate::formatter::wrap_text_with_indent;
use crate::types::AskamaNode;

pub(crate) fn restore_nodes(html: &str, nodes: &[AskamaNode], config: &Config) -> String {
    let mut result = html.to_string();

    // Process in reverse to avoid index shifting
    for (idx, node) in nodes.iter().enumerate().rev() {
        let placeholder = node.placeholder(idx);
        if let Some(pos) = result.find(&placeholder) {
            let indent = detect_indent(&result, pos, config);
            let formatted = format_node(node, indent, config);
            result = result.replace(&placeholder, &formatted);
        }
    }

    result
}

pub(crate) fn format_node(node: &AskamaNode, indent: usize, config: &Config) -> String {
    let (open, close, inner) = match node {
        AskamaNode::Control { dlmts, inner, .. }
        | AskamaNode::Expression { dlmts, inner }
        | AskamaNode::Comment { dlmts, inner } => (&dlmts.0, &dlmts.1, inner),
    };

    if inner.is_empty() {
        format!("{}{}", open, close)
    } else if should_wrap(inner, config.max_line_length) {
        format_wrapped_node(open, close, inner, indent, config)
    } else {
        format!("{} {} {}", open, inner, close)
    }
}

fn format_wrapped_node(
    open: &str,
    close: &str,
    inner: &str,
    indent: usize,
    config: &Config,
) -> String {
    let base_indent = " ".repeat(indent * config.indent_size);
    let inner_indent = " ".repeat((indent + 1) * config.indent_size);

    if inner.contains('\n') {
        let lines: Vec<&str> = inner.lines().collect();
        let mut formatted_lines = Vec::new();

        for (i, line) in lines.iter().enumerate() {
            let trimmed = line.trim();
            if trimmed.is_empty() && i > 0 && i < lines.len() - 1 {
                formatted_lines.push(String::new());
            } else if !trimmed.is_empty() {
                let wrap_threshold = config.max_line_length + (config.max_line_length / 2);
                if trimmed.len() + inner_indent.len() > wrap_threshold {
                    let wrapped =
                        wrap_text_with_indent(trimmed, &inner_indent, config.max_line_length);
                    formatted_lines.extend(wrapped);
                } else {
                    formatted_lines.push(format!("{}{}", inner_indent, trimmed));
                }
            }
        }

        format!(
            "{}\n{}\n{}{}",
            open,
            formatted_lines.join("\n"),
            base_indent,
            close
        )
    } else {
        let wrapped = wrap_text_with_indent(inner, &inner_indent, config.max_line_length);
        format!("{}\n{}\n{}{}", open, wrapped.join("\n"), base_indent, close)
    }
}

fn detect_indent(text: &str, pos: usize, config: &Config) -> usize {
    let line_start = text[..pos].rfind('\n').map_or(0, |p| p + 1);
    let line_part = &text[line_start..pos];

    let leading_spaces = line_part.len() - line_part.trim_start().len();
    leading_spaces / config.indent_size
}

fn should_wrap(content: &str, max_len: usize) -> bool {
    content.contains('\n') || content.len() > max_len / 2
}
