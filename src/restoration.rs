use crate::Config;
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
        let prefix = " ".repeat((indent + 1) * config.indent_size);
        format!(
            "{}\n{}{}\n{}{}",
            open,
            prefix,
            inner,
            " ".repeat(indent * config.indent_size),
            close
        )
    } else {
        format!("{} {} {}", open, inner, close)
    }
}

fn detect_indent(text: &str, pos: usize, config: &Config) -> usize {
    let line_start = text[..pos].rfind('\n').map_or(0, |p| p + 1);
    let spaces = pos - line_start;
    spaces / config.indent_size
}

fn should_wrap(content: &str, max_len: usize) -> bool {
    content.contains('\n') || content.len() > max_len / 2
}
