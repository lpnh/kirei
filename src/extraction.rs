use tree_sitter::Node;

use crate::types::{AskamaNode, Block, BlockType, Delimiters};

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

        if start > pos {
            html.push_str(&source[pos..start]);
        }

        if let Some(node) = parse_askama_node(child, source) {
            html.push_str(&node.placeholder(nodes.len()));
            nodes.push(node);
        } else {
            html.push_str(child.utf8_text(source.as_bytes())?);
        }

        pos = end;
    }

    if pos < source.len() {
        html.push_str(&source[pos..]);
    }

    Ok((html, nodes))
}

fn parse_askama_node(node: Node, source: &str) -> Option<AskamaNode> {
    let (dlmts, inner) = extract_delimiters(node, source)?;

    match node.kind() {
        "control_tag" => {
            let block_info = detect_block_type(node);
            Some(AskamaNode::Control {
                dlmts,
                inner,
                block_info,
            })
        }
        "render_expression" => Some(AskamaNode::Expression { dlmts, inner }),
        "comment" => Some(AskamaNode::Comment { dlmts, inner }),
        _ => None,
    }
}

fn detect_block_type(node: Node) -> Option<(Block, BlockType)> {
    let child = node.child(1)?;
    match child.kind() {
        "if_statement" => Some((Block::Open, BlockType::If)),
        "for_statement" => Some((Block::Open, BlockType::For)),
        "block_statement" => Some((Block::Open, BlockType::Block)),
        "filter_statement" => Some((Block::Open, BlockType::Filter)),
        "match_statement" => Some((Block::Open, BlockType::Match)),
        "macro_statement" => Some((Block::Open, BlockType::Macro)),
        "macro_call_statement" => Some((Block::Open, BlockType::MacroCall)),

        "else_statement" | "else_if_statement" => Some((Block::Clause, BlockType::If)),
        "when_statement" => Some((Block::Inner, BlockType::Match)),

        "endif_statement" => Some((Block::Close, BlockType::If)),
        "endfor_statement" => Some((Block::Close, BlockType::For)),
        "endblock_statement" => Some((Block::Close, BlockType::Block)),
        "endfilter_statement" => Some((Block::Close, BlockType::Filter)),
        "endmatch_statement" => Some((Block::Close, BlockType::Match)),
        "endmacro_statement" => Some((Block::Close, BlockType::Macro)),
        "endcall_statement" => Some((Block::Close, BlockType::MacroCall)),

        _ => None,
    }
}

fn extract_delimiters(node: Node, source: &str) -> Option<(Delimiters, String)> {
    let first = node.child(0)?;
    let last = node.child(node.child_count() - 1)?;

    let open = first.utf8_text(source.as_bytes()).ok()?.to_string();
    let close = last.utf8_text(source.as_bytes()).ok()?.to_string();

    let start = first.end_byte();
    let end = last.start_byte();

    let inner = if start < end {
        source[start..end].to_string()
    } else {
        String::new()
    };

    Some((Delimiters { open, close }, inner))
}
