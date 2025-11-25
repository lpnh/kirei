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
    let indent_str = indent_for(&tree.config, branch.indent);
    let lines: Vec<&str> = content.lines().collect();

    // Find minimum indentation across all non-empty lines
    let min_indent = lines
        .iter()
        .filter(|line| !line.trim().is_empty())
        .map(|line| line.len() - line.trim_start().len())
        .min()
        .unwrap_or(0);

    for line in &lines {
        if line.trim().is_empty() {
            inked_tree.push('\n');
        } else {
            let current_indent = line.len() - line.trim_start().len();
            let extra_indent = current_indent.saturating_sub(min_indent);
            let extra_spaces = " ".repeat(extra_indent);

            inked_tree.push_str(&indent_str);
            inked_tree.push_str(&extra_spaces);
            inked_tree.push_str(line.trim_start());
            inked_tree.push('\n');
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
