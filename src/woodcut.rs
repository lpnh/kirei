use textwrap::{Options, wrap};

use crate::{
    config::Config,
    sakura_tree::{Branch, BranchStyle, Leaf, SakuraTree},
};

pub fn print(tree: &SakuraTree) -> String {
    let estimated_inked_tree_size = tree.leaves.iter().map(Leaf::chars_count).sum::<usize>()
        + tree.leaves.len() * (tree.config.indent_size * 4);
    let mut inked_tree = String::with_capacity(estimated_inked_tree_size);

    for branch in &tree.branches {
        match branch.style {
            BranchStyle::Inline => ink_inline(&mut inked_tree, tree, branch),
            BranchStyle::WrappedText => ink_wrapped_text(&mut inked_tree, tree, branch),
            BranchStyle::Comment => ink_comment(&mut inked_tree, tree, branch),
            BranchStyle::Raw => ink_raw(&mut inked_tree, tree, branch),
        }
    }

    inked_tree
}

fn ink_inline(inked_tree: &mut String, tree: &SakuraTree, branch: &Branch) {
    let indent_str = indent_for(&tree.config, branch.indent);
    let line_content = branch_content(tree, branch.start, branch.end);
    push_indented_line(inked_tree, &indent_str, &line_content);
}

fn ink_wrapped_text(inked_tree: &mut String, tree: &SakuraTree, branch: &Branch) {
    let text_content = branch_content(tree, branch.start, branch.end);
    let wrapped_content = wrap_inline_content(&tree.config, &text_content, branch.indent);

    inked_tree.push_str(&wrapped_content);
    inked_tree.push('\n');
}

fn ink_comment(inked_tree: &mut String, tree: &SakuraTree, branch: &Branch) {
    let content = branch_content(tree, branch.start, branch.end);

    // Single-line
    if !content.contains('\n') {
        let indent_str = indent_for(&tree.config, branch.indent);
        push_indented_line(inked_tree, &indent_str, &content);
        return;
    }

    // Multiline
    let lines: Vec<&str> = content.lines().collect();
    let delimiter_indent = indent_for(&tree.config, branch.indent);
    let content_indent = indent_for(&tree.config, branch.indent + 1);

    for (i, line) in lines.iter().enumerate() {
        let trimmed = line.trim_start();

        if trimmed.is_empty() {
            inked_tree.push('\n');
        } else if i == 0 || i == lines.len() - 1 {
            push_indented_line(inked_tree, &delimiter_indent, trimmed);
        } else {
            push_indented_line(inked_tree, &content_indent, trimmed);
        }
    }
}

fn ink_raw(inked_tree: &mut String, tree: &SakuraTree, branch: &Branch) {
    let mut curr_indent = branch.indent;

    process_raw_content(
        inked_tree,
        &branch_content(tree, branch.start, branch.end),
        &mut curr_indent,
        &tree.config,
    );
}

fn branch_content(tree: &SakuraTree, start: usize, end: usize) -> String {
    let mut content = String::new();
    let mut prev_idx = None;

    for leaf_idx in start..=end {
        if let Some(leaf) = tree.leaves.get(leaf_idx) {
            if let Some(prev) = prev_idx
                && tree.has_space(prev, leaf_idx)
            {
                content.push(' ');
            }
            content.push_str(leaf.content());
            prev_idx = Some(leaf_idx);
        }
    }

    content
}

fn process_raw_content(
    inked_tree: &mut String,
    content: &str,
    curr_indent: &mut usize,
    config: &Config,
) {
    for line in content.lines() {
        if line.is_empty() {
            inked_tree.push('\n');
            continue;
        }

        let leading_close = usize::from(line.starts_with('}'));
        *curr_indent = curr_indent.saturating_sub(leading_close);

        let indent_str = indent_for(config, *curr_indent);
        push_indented_line(inked_tree, &indent_str, line);

        let open_braces = line.matches('{').count() as isize;
        let close_braces = line.matches('}').count() as isize;
        let net_change = open_braces - close_braces + leading_close as isize;
        *curr_indent = curr_indent.saturating_add_signed(net_change);
    }
}

fn wrap_inline_content(config: &Config, content: &str, indent: usize) -> String {
    let prefix = indent_for(config, indent);
    let available_width = config.max_width;

    let options = Options::new(available_width)
        .initial_indent(&prefix)
        .subsequent_indent(&prefix);

    wrap(content, &options).join("\n")
}

fn indent_for(config: &Config, indent: usize) -> String {
    " ".repeat(indent * config.indent_size)
}

fn push_indented_line(inked_tree: &mut String, indent_str: &str, content: &str) {
    inked_tree.push_str(indent_str);
    inked_tree.push_str(content);
    inked_tree.push('\n');
}
