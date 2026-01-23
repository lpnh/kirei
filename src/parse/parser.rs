use tree_sitter::Parser;
use tree_sitter_askama::LANGUAGE as ASKAMA_LANGUAGE;
use tree_sitter_css::LANGUAGE as CSS_LANGUAGE;
use tree_sitter_html::LANGUAGE as HTML_LANGUAGE;

use super::{
    askama::{self, AskamaNode},
    check,
    css::{self, CssNode},
    html::{self, HtmlNode},
    seed::Seed,
};

use crate::{ErrorKind, session::Session};

pub struct SakuraParser {
    askama: Parser,
    html: Parser,
    css: Parser,
}

impl Default for SakuraParser {
    fn default() -> Self {
        let mut askama = Parser::new();
        askama
            .set_language(&ASKAMA_LANGUAGE.into())
            .expect("failed to set Askama language");

        let mut html = Parser::new();
        html.set_language(&HTML_LANGUAGE.into())
            .expect("failed to set HTML language");

        let mut css = Parser::new();
        css.set_language(&CSS_LANGUAGE.into())
            .expect("failed to set CSS language");

        Self { askama, html, css }
    }
}

impl SakuraParser {
    pub fn parse(&mut self, sess: &mut Session, src: &str, path: &str) -> Option<Seed> {
        let askama_tree = parse_tree(&mut self.askama, src, "Askama", &[], sess, path)?;
        let (askama, content) = askama::extract_askama(&askama_tree.root_node(), src);
        if content.is_empty() {
            return Some(new_seed(askama, Vec::new(), Vec::new(), src.to_string()));
        }

        let html_tree = parse_tree(&mut self.html, src, "HTML", &content, sess, path)?;
        let (mut html, raw) = html::extract_html(sess, &html_tree.root_node(), src, &content, path);
        let crossing_indices = check::element_across_control(sess, &html, &askama, src, path);
        html::unpair_crossing_tags(&mut html, &crossing_indices);
        if raw.is_empty() {
            return Some(new_seed(askama, html, Vec::new(), src.to_string()));
        }

        let shadow_src = shadow(src, &askama);
        let css_tree = parse_tree(&mut self.css, &shadow_src, "CSS", &raw, sess, path)?;
        let css = css::extract_css(sess, &css_tree.root_node(), src, &raw, path);
        Some(new_seed(askama, html, css, src.to_string()))
    }
}

fn new_seed(askama: Vec<AskamaNode>, html: Vec<HtmlNode>, css: Vec<CssNode>, src: String) -> Seed {
    Seed {
        askama,
        html,
        css,
        src,
    }
}

fn parse_tree(
    parser: &mut Parser,
    src: &str,
    lang: &str,
    ranges: &[tree_sitter::Range],
    sess: &mut Session,
    path: &str,
) -> Option<tree_sitter::Tree> {
    if parser.set_included_ranges(ranges).is_err() {
        sess.emit_error(&ErrorKind::ParserFailed { lang: lang.into() });
        return None;
    }

    let tree = parser.parse(src, None)?;

    if tree.root_node().has_error() {
        if let Some(err) = check::syntax_error(&tree.root_node(), lang.into(), src, path) {
            sess.emit_error(&err);
        }
        return None;
    }

    Some(tree)
}

fn shadow(source: &str, askama_nodes: &[AskamaNode]) -> String {
    let mut shadow = source.to_string();

    let mut nodes: Vec<_> = askama_nodes.iter().collect();
    nodes.sort_by_key(|n| std::cmp::Reverse(n.start()));

    for node in nodes {
        let placeholder = match node {
            AskamaNode::Expression { range, .. } => {
                let next_char = source.as_bytes().get(range.end).copied();
                if next_char == Some(b'%') {
                    "0".repeat(range.len())
                } else {
                    "_".repeat(range.len())
                }
            }
            AskamaNode::Control { range, .. } | AskamaNode::Comment { range, .. } => {
                format!("/*{}*/", "_".repeat(range.len() - 4))
            }
        };
        shadow.replace_range(node.start()..node.end(), &placeholder);
    }

    shadow
}
