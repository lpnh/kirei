use std::borrow::Cow;
use textwrap::{Options, wrap};

use crate::{
    config::Config,
    html::HtmlNode,
    sakura_tree::{Branch, BranchStyle, Leaf, Root, SakuraTree, Twig},
};

pub fn print(tree: &SakuraTree) -> String {
    let estimated_inked_tree_size = tree.leaves.iter().map(|l| l.content.len()).sum::<usize>()
        + tree.leaves.len() * (tree.config.indent_size * 4);
    let mut inked_tree = String::with_capacity(estimated_inked_tree_size);

    for branch in &tree.branches {
        match branch.style {
            BranchStyle::Inline => ink_inline(&mut inked_tree, tree, branch),
            BranchStyle::OpenClose => ink_open_close(&mut inked_tree, tree, branch),
            BranchStyle::SingleHtmlText => ink_wrapped_text(&mut inked_tree, tree, branch),
            BranchStyle::AskamaComment => ink_askama_comment(&mut inked_tree, tree, branch),
            BranchStyle::Multiple => ink_wrapped_multiple(&mut inked_tree, tree, branch),
            BranchStyle::Raw => ink_raw(&mut inked_tree, tree, branch),
        }
    }

    inked_tree
}

fn ink_inline(inked_tree: &mut String, tree: &SakuraTree, branch: &Branch) {
    let indent_str = indent_for(&tree.config, branch.indent);
    let mut line_content = String::new();

    for (i, leaf_idx) in branch.twig.indices().enumerate() {
        if let Some(leaf) = tree.leaves.get(leaf_idx) {
            let content = content_normalized(leaf);

            if i > 0 && should_add_space_before_leaf(tree, leaf_idx, branch.twig, i) {
                line_content.push(' ');
            }
            line_content.push_str(&content);
        }
    }

    push_indented_line(inked_tree, &indent_str, &line_content);
}

fn ink_open_close(inked_tree: &mut String, tree: &SakuraTree, branch: &Branch) {
    let indent_str = indent_for(&tree.config, branch.indent);
    debug_assert!(branch.twig.has_same_idx());
    let leaf_idx = branch.twig.start();

    if let Some(leaf) = tree.leaves.get(leaf_idx) {
        let content = content_normalized(leaf);
        push_indented_line(inked_tree, &indent_str, &content);
    }
}

fn ink_wrapped_text(inked_tree: &mut String, tree: &SakuraTree, branch: &Branch) {
    debug_assert!(branch.twig.has_same_idx());
    let leaf_idx = branch.twig.start();

    if let Some(leaf) = tree.leaves.get(leaf_idx) {
        let content = content_normalized(leaf);
        let wrapped_content = wrap_inline_content(&tree.config, &content, branch.indent);
        inked_tree.push_str(&wrapped_content);
        inked_tree.push('\n');
    }
}

fn ink_askama_comment(inked_tree: &mut String, tree: &SakuraTree, branch: &Branch) {
    let indent_str = indent_for(&tree.config, branch.indent);
    debug_assert!(branch.twig.has_same_idx());
    let leaf_idx = branch.twig.start();

    if let Some(leaf) = tree.leaves.get(leaf_idx) {
        let content = content_normalized(leaf);

        for (i, line) in content.lines().enumerate() {
            if i > 0 {
                inked_tree.push('\n');
            }
            inked_tree.push_str(&indent_str);
            inked_tree.push_str(line.trim_end());
        }
        inked_tree.push('\n');
    }
}

fn ink_wrapped_multiple(inked_tree: &mut String, tree: &SakuraTree, branch: &Branch) {
    let indent_str = indent_for(&tree.config, branch.indent);
    let available_width = tree.config.max_width - indent_str.len();

    let mut curr_line = String::new();
    let mut lines = Vec::new();

    for (i, leaf_idx) in branch.twig.indices().enumerate() {
        if let Some(leaf) = tree.leaves.get(leaf_idx) {
            let content = content_normalized(leaf);
            let needs_space = i > 0 && should_add_space_before_leaf(tree, leaf_idx, branch.twig, i);
            let space_len = usize::from(needs_space);
            let would_exceed = curr_line.len() + space_len + content.len() > available_width;

            if would_exceed && !curr_line.is_empty() {
                let is_start_tag = matches!(&leaf.root, Root::Html(HtmlNode::StartTag { .. }));

                if is_start_tag {
                    lines.push(curr_line.trim_end().to_string());
                    curr_line = String::new();
                    curr_line.push_str(&content);
                    continue;
                } else if leaf.is_html_text() {
                    if !curr_line.is_empty() {
                        lines.push(curr_line.trim_end().to_string());
                        curr_line = String::new();
                    }
                    let wrapped = wrap_inline_content(&tree.config, &content, branch.indent);
                    for wrapped_line in wrapped.lines() {
                        lines.push(wrapped_line.trim_start().to_string());
                    }
                    continue;
                }
            }

            if needs_space && !curr_line.is_empty() {
                curr_line.push(' ');
            }

            curr_line.push_str(&content);
        }
    }

    if !curr_line.trim().is_empty() {
        lines.push(curr_line.trim_end().to_string());
    }

    for line in lines {
        if !line.trim().is_empty() {
            push_indented_line(inked_tree, &indent_str, &line);
        }
    }
}

fn ink_raw(inked_tree: &mut String, tree: &SakuraTree, branch: &Branch) {
    let mut curr_indent = branch.indent;

    for leaf_idx in branch.twig.indices() {
        process_raw_leaf(inked_tree, tree, leaf_idx, &mut curr_indent);
    }
}

fn process_raw_leaf(
    inked_tree: &mut String,
    tree: &SakuraTree,
    leaf_idx: usize,
    curr_indent: &mut i32,
) {
    if let Some(leaf) = tree.leaves.get(leaf_idx) {
        let content = &leaf.content;

        if !content.trim().is_empty() {
            for line in content.lines() {
                if line.trim().is_empty() {
                    inked_tree.push('\n');
                } else {
                    let trimmed = line.trim();
                    let temp_indent_decrease = trimmed.starts_with('}');

                    if temp_indent_decrease {
                        *curr_indent = curr_indent.saturating_sub(1);
                    }

                    let indent_str = indent_for(&tree.config, *curr_indent);
                    inked_tree.push_str(&indent_str);
                    inked_tree.push_str(trimmed);
                    inked_tree.push('\n');

                    if trimmed.ends_with('{') {
                        *curr_indent += 1;
                    }

                    let open_braces = trimmed.matches('{').count();
                    let close_braces = trimmed.matches('}').count();
                    let net_change = open_braces as i32 - close_braces as i32;

                    let net_change = if trimmed.ends_with('{') && net_change > 0 {
                        net_change - 1
                    } else {
                        net_change
                    };

                    if net_change > 0 {
                        *curr_indent += net_change;
                    } else if net_change < 0 {
                        let decrease = (-net_change) as usize;
                        if !temp_indent_decrease || decrease > 1 {
                            let actual_decrease = if temp_indent_decrease {
                                decrease - 1
                            } else {
                                decrease
                            };
                            *curr_indent = curr_indent.saturating_sub(actual_decrease as i32);
                        }
                    }
                }
            }
        }
    }
}

fn content_normalized(leaf: &Leaf) -> Cow<'_, str> {
    if leaf.is_html_text() {
        Cow::Owned(normalize_text_content(&leaf.content))
    } else {
        Cow::Borrowed(&leaf.content)
    }
}

fn normalize_text_content(text: &str) -> String {
    let has_trailing = text.ends_with(char::is_whitespace);
    let trailing = if has_trailing { " " } else { "" };
    let internal = text.split_whitespace().collect::<Vec<_>>().join(" ");
    format!("{}{}", internal, trailing)
}

fn should_add_space_before_leaf(
    tree: &SakuraTree,
    curr_leaf_idx: usize,
    twig: Twig,
    position_in_branch: usize,
) -> bool {
    if position_in_branch == 0 {
        return false;
    }

    let current = tree.leaves.get(curr_leaf_idx).unwrap();
    let prev_idx = twig.indices().nth(position_in_branch - 1).unwrap();
    let prev = tree.leaves.get(prev_idx).unwrap();

    let curr_content = content_normalized(current);
    let prev_content = content_normalized(prev);

    if curr_content.starts_with(' ') || prev_content.ends_with(' ') {
        return false;
    }

    match (&prev.root, &current.root) {
        (
            Root::Html(HtmlNode::Text(_)),
            Root::Html(HtmlNode::Entity(_) | HtmlNode::StartTag { .. }),
        )
        | (Root::Html(HtmlNode::Entity(_)), Root::Html(HtmlNode::Text(_))) => true,
        (Root::Askama(node), _) if node.is_match_arm() => true,
        (Root::Askama(node), Root::Html(HtmlNode::Text(_))) => {
            node.is_expr()
                && !current
                    .content
                    .starts_with(|c: char| c.is_ascii_punctuation())
        }
        (Root::Html(HtmlNode::Text(_)), Root::Askama(node)) => {
            node.is_expr() && !prev.content.ends_with(|c: char| c.is_ascii_punctuation())
        }
        (Root::Html(HtmlNode::EndTag { .. }), Root::Html(HtmlNode::Text(_))) => {
            curr_content.starts_with(char::is_alphabetic)
        }
        (Root::Html(HtmlNode::EndTag { .. }), Root::Askama(node)) => node.is_expr(),

        _ => false,
    }
}

fn wrap_inline_content(config: &Config, content: &str, indent: i32) -> String {
    let prefix = indent_for(config, indent);
    let available_width = config.max_width;

    let options = Options::new(available_width)
        .initial_indent(&prefix)
        .subsequent_indent(&prefix);

    wrap(content, &options).join("\n")
}

fn indent_for(config: &Config, indent: i32) -> String {
    " ".repeat(indent as usize * config.indent_size)
}

fn push_indented_line(inked_tree: &mut String, indent_str: &str, content: &str) {
    inked_tree.push_str(indent_str);
    inked_tree.push_str(content.trim_end());
    inked_tree.push('\n');
}
