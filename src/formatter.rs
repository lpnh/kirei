use tree_sitter::Parser;
use tree_sitter_askama::LANGUAGE as ASKAMA_LANGUAGE;
use tree_sitter_html::LANGUAGE as HTML_LANGUAGE;

use crate::{
    context::{FormattingContext, tokenize},
    extraction::extract_nodes,
    restoration::restore_nodes,
    types::*,
};

pub(crate) struct Config {
    pub indent_size: usize,
    pub max_line_length: usize,
}

pub struct AskamaFormatter {
    askama_parser: Parser,
    html_parser: Parser,
    config: Config,
}

impl Default for AskamaFormatter {
    fn default() -> Self {
        Self::new().expect("Failed to create formatter")
    }
}

impl AskamaFormatter {
    fn new() -> Result<Self, Box<dyn std::error::Error>> {
        let mut askama_parser = Parser::new();
        askama_parser.set_language(&ASKAMA_LANGUAGE.into())?;

        let mut html_parser = Parser::new();
        html_parser.set_language(&HTML_LANGUAGE.into())?;

        Ok(Self {
            askama_parser,
            html_parser,
            config: Config {
                indent_size: 4,
                max_line_length: 80,
            },
        })
    }

    pub fn format(&mut self, source: &str) -> Result<String, Box<dyn std::error::Error>> {
        // 1: Use tree-sitter-askama to parse the input
        let ast_tree = self
            .askama_parser
            .parse(source, None)
            .ok_or("Parse failed")?;

        // 2: Split the AST into html content String and AskamaNode
        let (html, nodes) = extract_nodes(source, &ast_tree.root_node())?;

        // 3: Format the html String with placeholders (AskamaNode)
        let formatted = if is_just_template(&html, &nodes) {
            format_template_only(&html, &nodes, &self.config)
        } else {
            format_template_with_html(&mut self.html_parser, &html, &nodes, &self.config)?
        };

        // 4: Restore the original Askama template code
        Ok(restore_nodes(&formatted, &nodes, &self.config))
    }
}

pub(crate) fn format_template_only(html: &str, nodes: &[AskamaNode], config: &Config) -> String {
    let mut context = FormattingContext::new(nodes, config);
    let tokens = tokenize(html);
    context.format_token_stream(&tokens);
    context.finish()
}

pub(crate) fn format_template_with_html(
    html_parser: &mut Parser,
    html: &str,
    nodes: &[AskamaNode],
    config: &Config,
) -> Result<String, Box<dyn std::error::Error>> {
    let tree = html_parser.parse(html, None).ok_or("HTML parse failed")?;
    let mut context = FormattingContext::new(nodes, config);

    context.format_html_node(&tree.root_node(), html.as_bytes())?;

    Ok(context.finish().trim_end().to_string())
}

fn is_just_template(html: &str, nodes: &[AskamaNode]) -> bool {
    let mut stripped_html = html.to_string();
    for (i, _) in nodes.iter().enumerate() {
        stripped_html = stripped_html.replace(&nodes[i].placeholder(i), "");
    }
    stripped_html.trim().is_empty()
}
