use crate::sakura_tree::{Branch, BranchStyle, Leaf, Ring, SakuraTree, Twig};

pub fn wire(tree: &mut SakuraTree) {
    let indent_map = analyze_indentation_structure(tree);
    wire_branches(tree, &indent_map);
}

fn analyze_indentation_structure(tree: &SakuraTree) -> Vec<i32> {
    let mut indent_map = vec![0; tree.leaves.len()];
    let mut curr_indent = 0;

    for (i, leaf) in tree.leaves.iter().enumerate() {
        if matches!(leaf, Leaf::HtmlEndTag { .. }) {
            curr_indent = (curr_indent - 1).max(0);
        }

        let (pre_delta, post_delta) = match leaf {
            Leaf::AskamaControl { tag, .. } => tag.indent_delta(),
            _ => (0, 0),
        };
        curr_indent = (curr_indent + pre_delta).max(0);
        indent_map[i] = curr_indent;
        curr_indent = (curr_indent + post_delta).max(0);

        if let Leaf::HtmlStartTag { .. } = leaf {
            curr_indent += 1;
        }
    }
    indent_map
}

fn wire_branches(tree: &mut SakuraTree, indent_map: &[i32]) {
    let rings: Vec<Ring> = tree.rings.clone();

    for ring in &rings {
        wire_branch(tree, ring, indent_map);
    }
}

fn wire_branch(tree: &mut SakuraTree, ring: &Ring, indent_map: &[i32]) {
    let fits = ring.total_chars(tree) <= tree.config.max_width;
    let has_block = ring.has_block(tree);

    match ring {
        // Compound
        Ring::Element(twig, inner) | Ring::ControlBlock(twig, inner) => {
            if fits && !has_block {
                push_branch(tree, twig, BranchStyle::Inline, indent_map);
            } else {
                wire_open_close_branches(tree, twig, inner.as_deref(), indent_map);
            }
        }
        Ring::TextSequence(twig, inner) => {
            if fits {
                push_branch(tree, twig, BranchStyle::Inline, indent_map);
            } else if is_text_sequence(inner, tree) {
                push_branch(tree, twig, BranchStyle::WrappedText, indent_map);
            } else {
                split_text_sequence(tree, twig, inner, indent_map);
            }
        }
        Ring::MatchArm(_, _) => push_branch(tree, &ring.twig(), BranchStyle::Inline, indent_map),

        // Atomic
        Ring::RawText(twig) => push_branch(tree, twig, BranchStyle::Raw, indent_map),
        Ring::Comment(twig) => {
            let style = if fits {
                BranchStyle::Inline
            } else {
                BranchStyle::MultilineComment
            };
            push_branch(tree, twig, style, indent_map);
        }
        Ring::Other(twig) => {
            let style = if fits {
                BranchStyle::Inline
            } else {
                BranchStyle::OpenClose
            };
            push_branch(tree, twig, style, indent_map);
        }
    }
}

fn wire_open_close_branches(
    tree: &mut SakuraTree,
    twig: &Twig,
    inner: Option<&[Ring]>,
    indent_map: &[i32],
) {
    let indent = get_indent(indent_map, twig);

    tree.branches.push(Branch::grow(
        twig.start().into(),
        BranchStyle::OpenClose,
        indent,
    ));

    if let Some(inner) = inner {
        for ring in inner {
            wire_branch(tree, ring, indent_map);
        }
    }

    tree.branches.push(Branch::grow(
        twig.end().into(),
        BranchStyle::OpenClose,
        indent,
    ));
}

// Indent based on the start leaf
fn get_indent(indent_map: &[i32], twig: &Twig) -> i32 {
    indent_map.get(twig.start()).copied().unwrap_or(0)
}

fn is_text_sequence(inner: &[Ring], tree: &SakuraTree) -> bool {
    inner.iter().all(|ring| {
        let twig = ring.twig();
        if !twig.has_same_idx() {
            return false;
        }
        tree.leaves
            .get(twig.start())
            .is_some_and(|leaf| matches!(leaf, Leaf::HtmlText(_) | Leaf::HtmlEntity(_)))
    })
}

fn split_text_sequence(tree: &mut SakuraTree, twig: &Twig, inner: &[Ring], indent_map: &[i32]) {
    let indent = get_indent(indent_map, twig);
    let indent_width = (indent as usize) * tree.config.indent_size;
    let available_width = tree.config.max_width.saturating_sub(indent_width);

    let mut line_start = twig.start();
    let mut line_end = twig.start();
    let mut line_width = 0;

    for (i, ring) in inner.iter().enumerate() {
        let ring_width = ring.total_chars(tree);
        let space_width = usize::from(i > 0 && line_width > 0);
        let total_width = line_width + space_width + ring_width;

        if total_width > available_width && line_width > 0 {
            // Emit current line
            tree.branches.push(Branch::grow(
                (line_start, line_end).into(),
                BranchStyle::Inline,
                indent,
            ));

            // Start new line
            line_start = ring.twig().start();
            line_end = ring.twig().end();
            line_width = ring_width;
        } else {
            // Add to current line
            line_end = ring.twig().end();
            line_width = total_width;
        }
    }

    // Emit final line
    if line_width > 0 {
        tree.branches.push(Branch::grow(
            (line_start, line_end).into(),
            BranchStyle::Inline,
            indent,
        ));
    }
}

fn push_branch(tree: &mut SakuraTree, twig: &Twig, style: BranchStyle, indent_map: &[i32]) {
    let indent = get_indent(indent_map, twig);
    tree.branches.push(Branch::grow(*twig, style, indent));
}
