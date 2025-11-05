use std::borrow::Cow;
use textwrap::{Options, wrap};

use crate::{
    config::Config,
    sakura_tree::{Branch, BranchStyle, Leaf, SakuraTree, Twig},
};

pub fn print(tree: &SakuraTree) -> String {
    let estimated_inked_tree_size = tree.leaves.iter().map(Leaf::chars_count).sum::<usize>()
        + tree.leaves.len() * (tree.config.indent_size * 4);
    let mut inked_tree = String::with_capacity(estimated_inked_tree_size);

    for branch in &tree.branches {
        match branch.style {
            BranchStyle::Inline => ink_inline(&mut inked_tree, tree, branch),
            BranchStyle::OpenClose => ink_open_close(&mut inked_tree, tree, branch),
            BranchStyle::SingleHtmlText => ink_wrapped_text(&mut inked_tree, tree, branch),
            BranchStyle::MultilineComment => ink_multiline_comment(&mut inked_tree, tree, branch),
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
            let content = content_normalized_with_context(tree, leaf, branch.twig, i);

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

fn ink_multiline_comment(inked_tree: &mut String, tree: &SakuraTree, branch: &Branch) {
    debug_assert!(branch.twig.has_same_idx());
    let leaf_idx = branch.twig.start();

    if let Some(leaf) = tree.leaves.get(leaf_idx) {
        match leaf {
            Leaf::HtmlComment(_) => {
                ink_html_comment(inked_tree, &tree.config, branch, leaf.content());
            }
            _ => {
                ink_askama_comment(inked_tree, &tree.config, branch.indent, leaf);
            }
        }
    }
}

fn ink_askama_comment(inked_tree: &mut String, config: &Config, indent_level: i32, leaf: &Leaf) {
    let indent = indent_for(config, indent_level);

    for line in leaf.content().lines() {
        if !line.is_empty() {
            push_indented_line(inked_tree, &indent, line);
        } else {
            inked_tree.push('\n');
        }
    }
}

fn ink_html_comment(inked_tree: &mut String, config: &Config, branch: &Branch, content: &str) {
    const OPEN: &str = "<!--";
    const CLOSE: &str = "-->";

    let indent = indent_for(config, branch.indent);
    let inner = &content[OPEN.len()..content.len() - CLOSE.len()];
    let trimmed = inner.trim();

    push_indented_line(inked_tree, &indent, OPEN);

    if !trimmed.is_empty() {
        if trimmed.len() <= config.max_width {
            let content_indent = indent_for(config, branch.indent + 1);
            push_indented_line(inked_tree, &content_indent, trimmed);
        } else {
            let wrapped = wrap_inline_content(config, trimmed, branch.indent + 1);
            inked_tree.push_str(&wrapped);
            inked_tree.push('\n');
        }
    }

    push_indented_line(inked_tree, &indent, CLOSE);
}

fn ink_raw(inked_tree: &mut String, tree: &SakuraTree, branch: &Branch) {
    let mut curr_indent = branch.indent;

    // Concatenate all leaves to preserve inline Askama expressions
    let mut combined_content = String::new();
    for leaf_idx in branch.twig.indices() {
        if let Some(leaf) = tree.leaves.get(leaf_idx) {
            combined_content.push_str(leaf.content());
        }
    }

    process_raw_content(
        inked_tree,
        &combined_content,
        &mut curr_indent,
        &tree.config,
    );
}

fn process_raw_content(
    inked_tree: &mut String,
    content: &str,
    curr_indent: &mut i32,
    config: &Config,
) {
    if content.is_empty() {
        return;
    }

    for line in content.lines() {
        if line.is_empty() {
            inked_tree.push('\n');
        } else {
            let temp_indent_decrease = line.starts_with('}');

            if temp_indent_decrease {
                *curr_indent = curr_indent.saturating_sub(1);
            }

            let indent_str = indent_for(config, *curr_indent);
            inked_tree.push_str(&indent_str);
            inked_tree.push_str(line);
            inked_tree.push('\n');

            if line.ends_with('{') {
                *curr_indent += 1;
            }

            let open_braces = line.matches('{').count();
            let close_braces = line.matches('}').count();
            let net_change = open_braces as i32 - close_braces as i32;

            let net_change = if line.ends_with('{') && net_change > 0 {
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

fn content_normalized_with_context<'a>(
    tree: &SakuraTree,
    leaf: &'a Leaf,
    twig: Twig,
    position: usize,
) -> Cow<'a, str> {
    let prev_expr = is_askama_expr(tree, twig, position, -1);
    let next_expr = is_askama_expr(tree, twig, position, 1);
    normalize_leaf_content(leaf, prev_expr, next_expr)
}

fn content_normalized(leaf: &Leaf) -> Cow<'_, str> {
    normalize_leaf_content(leaf, false, false)
}

fn normalize_leaf_content(leaf: &Leaf, prev_expr: bool, next_expr: bool) -> Cow<'_, str> {
    let content = leaf.content();

    if !matches!(leaf, Leaf::HtmlText(_)) {
        return Cow::Borrowed(content);
    }

    let has_leading = content.starts_with(char::is_whitespace);
    let has_trailing = content.ends_with(char::is_whitespace);

    let leading = if has_leading && prev_expr { " " } else { "" };
    let trailing = if has_trailing && next_expr { " " } else { "" };
    let normalized = crate::normalize_whitespace(content);

    Cow::Owned(format!("{}{}{}", leading, normalized, trailing))
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

    match (prev, current) {
        (Leaf::HtmlText(_), Leaf::HtmlEntity(_) | Leaf::HtmlStartTag { .. })
        | (Leaf::HtmlEntity(_), Leaf::HtmlText(_)) => true,
        (Leaf::AskamaControl { tag, .. }, _) => tag.is_match_arm(),
        (Leaf::HtmlEndTag { .. }, _) => {
            current.is_expr() || current.content().starts_with(char::is_alphabetic)
        }
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
    inked_tree.push_str(content);
    inked_tree.push('\n');
}

fn is_askama_expr(tree: &SakuraTree, twig: Twig, position: usize, offset: isize) -> bool {
    position
        .checked_add_signed(offset)
        .and_then(|pos| twig.indices().nth(pos))
        .and_then(|idx| tree.leaves.get(idx))
        .is_some_and(Leaf::is_expr)
}
