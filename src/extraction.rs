use crate::types::{AskamaNode, Block};
use tree_sitter::Node;

pub(crate) fn extract_nodes(
    source: &str,
    root: &Node,
) -> Result<(String, Vec<AskamaNode>), Box<dyn std::error::Error>> {
    let mut html = String::new();
    let mut nodes = Vec::new();
    let mut pos = 0;

    let mut cursor = root.walk();
    for child in root.children(&mut cursor) {
        let start = child.start_byte();
        let end = child.end_byte();

        // Add text before node
        if start > pos {
            html.push_str(&source[pos..start]);
        }

        // Process node
        if let Some(node) = parse_askama_node(child, source) {
            html.push_str(&node.placeholder(nodes.len()));
            nodes.push(node);
        } else {
            html.push_str(child.utf8_text(source.as_bytes())?);
        }

        pos = end;
    }

    // Add remaining text
    if pos < source.len() {
        html.push_str(&source[pos..]);
    }

    Ok((html, nodes))
}

fn parse_askama_node(node: Node, source: &str) -> Option<AskamaNode> {
    let (open, close, inner) = extract_delimiters(node, source)?;

    match node.kind() {
        "control_tag" => {
            let maybe_block = is_block(node)?;
            Some(AskamaNode::Control {
                inner,
                dlmts: (open, close),
                maybe_block: Some(maybe_block),
            })
        }
        "render_expression" => Some(AskamaNode::Expression {
            inner,
            dlmts: (open, close),
        }),
        "comment" => Some(AskamaNode::Comment {
            inner,
            dlmts: (open, close),
        }),
        _ => None,
    }
}

fn is_block(node: Node) -> Option<Block> {
    let child = node.child(1)?;
    match child.kind() {
        "if_statement"
        | "for_statement"
        | "block_statement"
        | "filter_statement"
        | "match_statement"
        | "macro_statement"
        | "call_statement"
        | "macro_call_statement" => Some(Block::Open),

        "else_statement" | "else_if_statement" => Some(Block::Clause),

        "when_statement" => Some(Block::Inner),

        "endif_statement"
        | "endfor_statement"
        | "endblock_statement"
        | "endfilter_statement"
        | "endmatch_statement"
        | "endmacro_statement"
        | "endcall_statement" => Some(Block::Close),

        "let_statement" | "extends_statement" | "include_statement" | "import_statement" => None,

        unknown => {
            eprintln!(
                "Unknown statement type '{}' in node '{}'",
                unknown,
                child.kind()
            );
            None
        }
    }
}

fn extract_delimiters(node: Node, source: &str) -> Option<(String, String, String)> {
    let first = node.child(0)?;
    let last = node.child(node.child_count() - 1)?;

    let open = first.utf8_text(source.as_bytes()).ok()?.trim().to_string();
    let close = last.utf8_text(source.as_bytes()).ok()?.trim().to_string();

    let start = first.end_byte();
    let end = last.start_byte();

    let inner = if start < end {
        source[start..end].trim().to_string()
    } else {
        String::new()
    };

    Some((open, close, inner))
}
