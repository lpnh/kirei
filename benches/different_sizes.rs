use criterion::{BenchmarkId, Criterion, Throughput, criterion_group, criterion_main};
use kirei::session::Session;
use std::hint::black_box;

struct TemplateBuilder {
    content: String,
}

impl TemplateBuilder {
    fn new() -> Self {
        Self {
            content: String::new(),
        }
    }

    fn push(&mut self, s: &str) {
        self.content.push_str(s);
    }

    fn add_doctype_and_html_start(mut self) -> Self {
        self.push("<!DOCTYPE html>\n");
        self.push("<html lang=\"en\">\n");
        self.push("<head>\n");
        self.push("    <meta charset=\"UTF-8\">\n");
        self.push(
            "    <meta name=\"viewport\" content=\"width=device-width, initial-scale=1.0\">\n",
        );
        self.push("    {# Page metadata configuration #}\n");
        self.push("    {% let page_class = \"default\" %}\n");
        self.push("    <title>{% block title %}{{ page_title | escape }}{% endblock %}</title>\n");
        self.push("    {% block head %}{% endblock %}\n");
        self
    }

    fn add_style_block(mut self) -> Self {
        self.push("    <style>\n");
        self.push("        :root {\n");
        self.push("            --primary-color: #3490dc;\n");
        self.push("            --secondary-color: #ffed4a;\n");
        self.push("            --danger-color: #e3342f;\n");
        self.push("        }\n");
        self.push("        .container { max-width: 1200px; margin: 0 auto; padding: 1rem; }\n");
        self.push("        .grid { display: grid; grid-template-columns: repeat(auto-fit, minmax(300px, 1fr)); gap: 1rem; }\n");
        self.push(
            "        .card { border: 1px solid #ddd; border-radius: 0.5rem; padding: 1rem; }\n",
        );
        self.push(
            "        .btn { padding: 0.5rem 1rem; border-radius: 0.25rem; cursor: pointer; }\n",
        );
        self.push("        .btn-primary { background: var(--primary-color); color: white; }\n");
        self.push("        .btn-danger { background: var(--danger-color); color: white; }\n");
        self.push(
            "        .alert { padding: 1rem; border-radius: 0.25rem; margin-bottom: 1rem; }\n",
        );
        self.push("        .alert-success { background: #d4edda; border: 1px solid #c3e6cb; }\n");
        self.push("        .alert-danger { background: #f8d7da; border: 1px solid #f5c6cb; }\n");
        self.push("        .form-group { margin-bottom: 1rem; }\n");
        self.push(
            "        .form-control { width: 100%; padding: 0.5rem; border: 1px solid #ddd; }\n",
        );
        self.push("        table { width: 100%; border-collapse: collapse; }\n");
        self.push("        th, td { padding: 0.75rem; border-bottom: 1px solid #ddd; text-align: left; }\n");
        self.push("        .badge { padding: 0.25rem 0.5rem; border-radius: 0.25rem; font-size: 0.875rem; }\n");
        self.push("        .badge-success { background: #28a745; color: white; }\n");
        self.push("        .badge-warning { background: #ffc107; color: black; }\n");
        self.push("        .badge-danger { background: #dc3545; color: white; }\n");
        self.push("    </style>\n");
        self
    }

    fn add_script_block(mut self) -> Self {
        self.push("    <script>\n");
        self.push("        document.addEventListener('DOMContentLoaded', function() {\n");
        self.push("            const forms = document.querySelectorAll('form');\n");
        self.push("            forms.forEach(form => {\n");
        self.push("                form.addEventListener('submit', function(e) {\n");
        self.push("                    const required = form.querySelectorAll('[required]');\n");
        self.push("                    let valid = true;\n");
        self.push("                    required.forEach(field => {\n");
        self.push("                        if (!field.value.trim()) {\n");
        self.push("                            valid = false;\n");
        self.push("                            field.classList.add('error');\n");
        self.push("                        }\n");
        self.push("                    });\n");
        self.push("                    if (!valid) e.preventDefault();\n");
        self.push("                });\n");
        self.push("            });\n");
        self.push("        });\n");
        self.push("    </script>\n");
        self
    }

    fn close_head_open_body(mut self) -> Self {
        self.push("</head>\n");
        self.push("<body class=\"{{ page_class }}\">\n");
        self
    }

    fn add_header_with_nav(mut self) -> Self {
        self.push("    <header class=\"site-header\">\n");
        self.push("        {# Main navigation component #}\n");
        self.push("        {% let nav_count = nav_items.len() %}\n");
        self.push(
            "        <nav class=\"navbar\" role=\"navigation\" aria-label=\"Main navigation\">\n",
        );
        self.push("            <div class=\"nav-brand\">\n");
        self.push(
            "                <a href=\"/\" class=\"brand-link\">{{ site_name | escape }}</a>\n",
        );
        self.push("            </div>\n");
        self.push("            <ul class=\"nav-menu\">\n");
        self.push("                {% for item in nav_items %}\n");
        self.push("                <li class=\"nav-item{% if loop.first %} first{% endif %}{% if loop.last %} last{% endif %}\">\n");
        self.push("                    {% if item.children.is_empty() %}\n");
        self.push("                    <a href=\"{{ item.url }}\" {% if item.active %}class=\"active\" aria-current=\"page\"{% endif %}>{{ item.label }}</a>\n");
        self.push("                    {% else %}\n");
        self.push("                    <details class=\"dropdown\">\n");
        self.push("                        <summary>{{ item.label }}</summary>\n");
        self.push("                        <ul class=\"dropdown-menu\">\n");
        self.push("                            {% for child in item.children %}\n");
        self.push("                            <li><a href=\"{{ child.url }}\">{{ child.label }}</a></li>\n");
        self.push("                            {% endfor %}\n");
        self.push("                        </ul>\n");
        self.push("                    </details>\n");
        self.push("                    {% endif %}\n");
        self.push("                </li>\n");
        self.push("                {% endfor %}\n");
        self.push("            </ul>\n");
        self.push("            {% if user is defined %}\n");
        self.push("            <div class=\"user-menu\">\n");
        self.push("                <span>Welcome, {{ user.name | escape }}</span>\n");
        self.push("                <a href=\"/logout\" class=\"btn btn-sm\">Logout</a>\n");
        self.push("            </div>\n");
        self.push("            {% endif %}\n");
        self.push("        </nav>\n");
        self.push("    </header>\n");
        self
    }

    fn add_breadcrumbs(mut self) -> Self {
        self.push("    {% if breadcrumbs is defined %}\n");
        self.push("    <nav aria-label=\"Breadcrumb\" class=\"breadcrumbs\">\n");
        self.push("        <ol itemscope itemtype=\"https://schema.org/BreadcrumbList\">\n");
        self.push("            {% for crumb in breadcrumbs %}\n");
        self.push("            <li itemprop=\"itemListElement\" itemscope itemtype=\"https://schema.org/ListItem\">\n");
        self.push("                {% if loop.last %}\n");
        self.push("                <span itemprop=\"name\">{{ crumb.title | escape }}</span>\n");
        self.push("                {% else %}\n");
        self.push("                <a itemprop=\"item\" href=\"{{ crumb.url }}\"><span itemprop=\"name\">{{ crumb.title | escape }}</span></a>\n");
        self.push("                <span class=\"separator\" aria-hidden=\"true\">/</span>\n");
        self.push("                {% endif %}\n");
        self.push("                <meta itemprop=\"position\" content=\"{{ loop.index }}\">\n");
        self.push("            </li>\n");
        self.push("            {% endfor %}\n");
        self.push("        </ol>\n");
        self.push("    </nav>\n");
        self.push("    {% endif %}\n");
        self
    }

    fn add_flash_messages(mut self) -> Self {
        self.push("    {% if flash_messages is defined %}\n");
        self.push("    <div class=\"flash-messages\" role=\"alert\">\n");
        self.push("        {% for message in flash_messages %}\n");
        self.push("        {% match message.level %}\n");
        self.push("        {% when MessageLevel::Success %}\n");
        self.push("        <div class=\"alert alert-success\">\n");
        self.push("            <strong>Success!</strong> {{ message.text | escape }}\n");
        self.push("        </div>\n");
        self.push("        {% when MessageLevel::Warning %}\n");
        self.push("        <div class=\"alert alert-warning\">\n");
        self.push("            <strong>Warning:</strong> {{ message.text | escape }}\n");
        self.push("        </div>\n");
        self.push("        {% when MessageLevel::Error %}\n");
        self.push("        <div class=\"alert alert-danger\">\n");
        self.push("            <strong>Error:</strong> {{ message.text | escape }}\n");
        self.push("        </div>\n");
        self.push("        {% when MessageLevel::Info %}\n");
        self.push("        <div class=\"alert alert-info\">\n");
        self.push("            <strong>Info:</strong> {{ message.text | escape }}\n");
        self.push("        </div>\n");
        self.push("        {% endmatch %}\n");
        self.push("        {% endfor %}\n");
        self.push("    </div>\n");
        self.push("    {% endif %}\n");
        self
    }

    fn add_main_content_start(mut self) -> Self {
        self.push("    <main class=\"main-content\" id=\"main\">\n");
        self.push("        <div class=\"container\">\n");
        self.push("            {% block content %}\n");
        self
    }

    fn add_hero_section(mut self) -> Self {
        self.push("            <section class=\"hero\">\n");
        self.push("                <h1>{{ hero_title | escape }}</h1>\n");
        self.push("                {% if hero_subtitle is defined %}\n");
        self.push("                <p class=\"lead\">{{ hero_subtitle | escape }}</p>\n");
        self.push("                {% endif %}\n");
        self.push("                {% if hero_cta is defined %}\n");
        self.push("                <a href=\"{{ hero_cta.url }}\" class=\"btn btn-primary btn-lg\">{{ hero_cta.text }}</a>\n");
        self.push("                {% endif %}\n");
        self.push("            </section>\n");
        self
    }

    fn add_card_grid(mut self, count: usize) -> Self {
        self.push("            <section class=\"card-grid\">\n");
        self.push("                <h2>{{ cards_title | default(\"Featured Items\") }}</h2>\n");
        self.push("                <div class=\"grid\">\n");
        self.push("                    {% for card in cards %}\n");
        self.push("                    {% if loop.index > ");
        self.push(&count.to_string());
        self.push(" %}{% break %}{% endif %}\n");
        self.push("                    <article class=\"card\" id=\"card-{{ loop.index0 }}\">\n");
        self.push("                        {% if let Some(image) = card.image %}\n");
        self.push("                        <img src=\"{{ image.src }}\" alt=\"{{ image.alt | escape }}\" loading=\"lazy\">\n");
        self.push("                        {% endif %}\n");
        self.push("                        <div class=\"card-body\">\n");
        self.push(
            "                            <h3 class=\"card-title\">{{ card.title | escape }}</h3>\n",
        );
        self.push("                            <p class=\"card-text\">{{ card.description | escape | truncate(150) }}</p>\n");
        self.push("                            {% if let Some(tags) = card.tags %}\n");
        self.push("                            <div class=\"card-tags\">\n");
        self.push("                                {% for tag in tags %}\n");
        self.push(
            "                                <span class=\"badge\">{{ tag | lower }}</span>\n",
        );
        self.push("                                {% endfor %}\n");
        self.push("                            </div>\n");
        self.push("                            {% endif %}\n");
        self.push("                            <a href=\"{{ card.url }}\" class=\"btn btn-primary\">Read more</a>\n");
        self.push("                        </div>\n");
        self.push("                        <footer class=\"card-footer\">\n");
        self.push(
            "                            <small>{{ card.date | date(\"%B %d, %Y\") }}</small>\n",
        );
        self.push("                        </footer>\n");
        self.push("                    </article>\n");
        self.push("                    {% endfor %}\n");
        self.push("                </div>\n");
        self.push("            </section>\n");
        self
    }

    fn add_data_table(mut self, extra_columns: usize) -> Self {
        self.push("            <section class=\"data-section\">\n");
        self.push("                <h2>{{ table_title | escape }}</h2>\n");
        self.push("                {% if table_data.is_empty() %}\n");
        self.push("                <p class=\"empty-state\">No data available.</p>\n");
        self.push("                {% else %}\n");
        self.push("                <div class=\"table-responsive\">\n");
        self.push("                    <table class=\"data-table\">\n");
        self.push("                        <thead>\n");
        self.push("                            <tr>\n");
        self.push("                                <th scope=\"col\">#</th>\n");
        self.push("                                <th scope=\"col\">Name</th>\n");
        self.push("                                <th scope=\"col\">Email</th>\n");
        self.push("                                <th scope=\"col\">Status</th>\n");
        for i in 0..extra_columns {
            self.push(&format!(
                "                                <th scope=\"col\">Field {}</th>\n",
                i + 1
            ));
        }
        self.push("                                <th scope=\"col\">Actions</th>\n");
        self.push("                            </tr>\n");
        self.push("                        </thead>\n");
        self.push("                        <tbody>\n");
        self.push("                            {% for row in table_data %}\n");
        self.push("                            <tr class=\"{% if loop.index0 % 2 == 0 %}even{% else %}odd{% endif %}\">\n");
        self.push("                                <td>{{ loop.index }}</td>\n");
        self.push("                                <td>\n");
        self.push("                                    {% if let Some(avatar) = row.avatar %}\n");
        self.push("                                    <img src=\"{{ avatar }}\" alt=\"\" class=\"avatar\" width=\"32\" height=\"32\">\n");
        self.push("                                    {% endif %}\n");
        self.push("                                    {{ row.name | escape }}\n");
        self.push("                                </td>\n");
        self.push("                                <td><a href=\"mailto:{{ row.email }}\">{{ row.email }}</a></td>\n");
        self.push("                                <td>\n");
        self.push("                                    {% match row.status %}\n");
        self.push("                                    {% when Status::Active %}\n");
        self.push("                                    <span class=\"badge badge-success\">Active</span>\n");
        self.push("                                    {% when Status::Pending %}\n");
        self.push("                                    <span class=\"badge badge-warning\">Pending</span>\n");
        self.push("                                    {% when Status::Inactive %}\n");
        self.push("                                    <span class=\"badge badge-danger\">Inactive</span>\n");
        self.push("                                    {% when _ %}\n");
        self.push("                                    <span class=\"badge\">Unknown</span>\n");
        self.push("                                    {% endmatch %}\n");
        self.push("                                </td>\n");
        for i in 0..extra_columns {
            self.push(&format!("                                <td>{{{{ row.field_{} | default(\"-\") }}}}</td>\n", i + 1));
        }
        self.push("                                <td class=\"actions\">\n");
        self.push("                                    <a href=\"/items/{{ row.id }}/view\" class=\"btn btn-sm\">View</a>\n");
        self.push("                                    {% if can_edit %}\n");
        self.push("                                    <a href=\"/items/{{ row.id }}/edit\" class=\"btn btn-sm btn-primary\">Edit</a>\n");
        self.push("                                    {% endif %}\n");
        self.push("                                    {% if can_delete %}\n");
        self.push("                                    <form method=\"post\" action=\"/items/{{ row.id }}/delete\" class=\"inline-form\">\n");
        self.push("                                        <input type=\"hidden\" name=\"_token\" value=\"{{ csrf_token }}\">\n");
        self.push("                                        <button type=\"submit\" class=\"btn btn-sm btn-danger\" onclick=\"return confirm('Are you sure?')\">Delete</button>\n");
        self.push("                                    </form>\n");
        self.push("                                    {% endif %}\n");
        self.push("                                </td>\n");
        self.push("                            </tr>\n");
        self.push("                            {% else %}\n");
        self.push("                            <tr>\n");
        self.push(&format!(
            "                                <td colspan=\"{}\">No records found</td>\n",
            5 + extra_columns
        ));
        self.push("                            </tr>\n");
        self.push("                            {% endfor %}\n");
        self.push("                        </tbody>\n");
        self.push("                    </table>\n");
        self.push("                </div>\n");
        self.push("                {% endif %}\n");
        self.push("            </section>\n");
        self
    }

    fn add_form_section(mut self, field_count: usize) -> Self {
        self.push("            <section class=\"form-section\">\n");
        self.push("                <h2>{{ form_title | escape }}</h2>\n");
        self.push("                {% if form_errors is defined %}\n");
        self.push("                <div class=\"alert alert-danger\" role=\"alert\">\n");
        self.push("                    <h4>Please fix the following errors:</h4>\n");
        self.push("                    <ul>\n");
        self.push("                        {% for error in form_errors %}\n");
        self.push("                        <li>{{ error | escape }}</li>\n");
        self.push("                        {% endfor %}\n");
        self.push("                    </ul>\n");
        self.push("                </div>\n");
        self.push("                {% endif %}\n");
        self.push("                <form method=\"post\" action=\"{{ form_action }}\" enctype=\"{{ form_enctype | default(\"application/x-www-form-urlencoded\") }}\">\n");
        self.push("                    <input type=\"hidden\" name=\"_token\" value=\"{{ csrf_token }}\">\n");
        self.push("                    {% if form_method is defined %}\n");
        self.push("                    <input type=\"hidden\" name=\"_method\" value=\"{{ form_method }}\">\n");
        self.push("                    {% endif %}\n");

        for i in 0..field_count {
            let field_type = match i % 5 {
                0 => "text",
                1 => "email",
                2 => "password",
                3 => "number",
                _ => "tel",
            };
            self.push("                    <div class=\"form-group\">\n");
            self.push(&format!("                        <label for=\"field_{}\">{{{{ field_{}_label | escape }}}}</label>\n", i, i));
            if i % 7 == 0 {
                self.push(&format!("                        <textarea id=\"field_{}\" name=\"field_{}\" class=\"form-control\" rows=\"4\" {{% if field_{}_required %}}required{{% endif %}}>{{{{ field_{}_value | default(\"\") | escape }}}}</textarea>\n", i, i, i, i));
            } else if i % 5 == 4 {
                self.push(&format!("                        <select id=\"field_{}\" name=\"field_{}\" class=\"form-control\" {{% if field_{}_required %}}required{{% endif %}}>\n", i, i, i));
                self.push(
                    "                            <option value=\"\">Select an option</option>\n",
                );
                self.push(&format!(
                    "                            {{% for option in field_{}_options %}}\n",
                    i
                ));
                self.push(&format!("                            <option value=\"{{{{ option.value }}}}\" {{% if option.value == field_{}_value %}}selected{{% endif %}}>{{{{ option.label | escape }}}}</option>\n", i));
                self.push("                            {% endfor %}\n");
                self.push("                        </select>\n");
            } else {
                self.push(&format!("                        <input type=\"{}\" id=\"field_{}\" name=\"field_{}\" class=\"form-control\" value=\"{{{{ field_{}_value | default(\"\") | escape }}}}\" {{% if field_{}_required %}}required{{% endif %}}>\n", field_type, i, i, i, i));
            }
            self.push(&format!(
                "                        {{% if field_{}_error is defined %}}\n",
                i
            ));
            self.push(&format!("                        <span class=\"error-message\">{{{{ field_{}_error | escape }}}}</span>\n", i));
            self.push("                        {% endif %}\n");
            self.push("                    </div>\n");
        }

        self.push("                    <div class=\"form-actions\">\n");
        self.push("                        <button type=\"submit\" class=\"btn btn-primary\">{{ submit_label | default(\"Submit\") }}</button>\n");
        self.push("                        <button type=\"reset\" class=\"btn\">Reset</button>\n");
        self.push("                        {% if cancel_url is defined %}\n");
        self.push(
            "                        <a href=\"{{ cancel_url }}\" class=\"btn\">Cancel</a>\n",
        );
        self.push("                        {% endif %}\n");
        self.push("                    </div>\n");
        self.push("                </form>\n");
        self.push("            </section>\n");
        self
    }

    fn add_nested_conditionals(mut self, depth: usize) -> Self {
        self.push("            <section class=\"conditional-content\">\n");
        for i in 0..depth {
            let indent = "                ".to_string() + &"    ".repeat(i);
            self.push(&format!("{}{{% if condition_{} %}}\n", indent, i));
            self.push(&format!("{}<div class=\"level-{}\">\n", indent, i));
            self.push(&format!(
                "{}    <h{}>Level {} Content</h{}>\n",
                indent,
                (i % 5) + 2,
                i,
                (i % 5) + 2
            ));
            self.push(&format!(
                "{}    {{% let level_var = \"level_{}\" %}}\n",
                indent, i
            ));
            self.push(&format!(
                "{}    <p>Variable: {{{{ level_var }}}}</p>\n",
                indent
            ));
        }

        self.push(&format!(
            "{}    <p>Deepest content here</p>\n",
            "                ".to_string() + &"    ".repeat(depth - 1)
        ));

        for i in (0..depth).rev() {
            let indent = "                ".to_string() + &"    ".repeat(i);
            self.push(&format!("{}</div>\n", indent));
            self.push(&format!("{}{{% else %}}\n", indent));
            self.push(&format!("{}<p>Condition {} not met</p>\n", indent, i));
            self.push(&format!("{}{{% endif %}}\n", indent));
        }
        self.push("            </section>\n");
        self
    }

    fn add_match_expressions(mut self, count: usize) -> Self {
        self.push("            <section class=\"match-section\">\n");
        self.push("                <h2>Dynamic Content</h2>\n");
        for i in 0..count {
            self.push(&format!("                {{% match item_{}_type %}}\n", i));
            self.push("                {% when ItemType::Article %}\n");
            self.push("                <article class=\"content-article\">\n");
            self.push("                    <header>\n");
            self.push(&format!(
                "                        <h3>{{{{ item_{}.title | escape }}}}</h3>\n",
                i
            ));
            self.push(&format!("                        <time datetime=\"{{{{ item_{}.date }}}}\">{{{{ item_{}.date | date(\"%Y-%m-%d\") }}}}</time>\n", i, i));
            self.push("                    </header>\n");
            self.push(&format!(
                "                    <div class=\"content\">{{{{ item_{}.body | safe }}}}</div>\n",
                i
            ));
            self.push("                </article>\n");
            self.push("                {% when ItemType::Gallery with (images) %}\n");
            self.push("                <div class=\"gallery\">\n");
            self.push("                    {% for img in images %}\n");
            self.push("                    <figure>\n");
            self.push("                        <img src=\"{{ img.src }}\" alt=\"{{ img.alt | escape }}\" loading=\"lazy\">\n");
            self.push("                        {% if let Some(caption) = img.caption %}\n");
            self.push("                        <figcaption>{{ caption | escape }}</figcaption>\n");
            self.push("                        {% endif %}\n");
            self.push("                    </figure>\n");
            self.push("                    {% endfor %}\n");
            self.push("                </div>\n");
            self.push("                {% when ItemType::Video with (url, poster) %}\n");
            self.push("                <div class=\"video-container\">\n");
            self.push("                    <video controls poster=\"{{ poster }}\">\n");
            self.push("                        <source src=\"{{ url }}\" type=\"video/mp4\">\n");
            self.push("                        Your browser does not support the video tag.\n");
            self.push("                    </video>\n");
            self.push("                </div>\n");
            self.push("                {% when ItemType::Quote(quote) %}\n");
            self.push("                <blockquote class=\"quote\">\n");
            self.push("                    <p>{{ quote.text | escape }}</p>\n");
            self.push(
                "                    <footer>— <cite>{{ quote.author | escape }}</cite></footer>\n",
            );
            self.push("                </blockquote>\n");
            self.push("                {% when _ %}\n");
            self.push(
                "                <div class=\"unknown-content\">Unknown content type</div>\n",
            );
            self.push("                {% endmatch %}\n");
        }
        self.push("            </section>\n");
        self
    }

    fn add_loop_with_features(mut self, iterations: usize) -> Self {
        self.push("            <section class=\"loop-section\">\n");
        self.push("                <h2>Iterative Content</h2>\n");
        self.push("                <ul class=\"item-list\">\n");
        self.push("                    {% for item in items %}\n");
        self.push("                    {% if loop.index > ");
        self.push(&iterations.to_string());
        self.push(" %}{% break %}{% endif %}\n");
        self.push("                    <li class=\"{% if loop.first %}first{% endif %}{% if loop.last %}last{% endif %}\" data-index=\"{{ loop.index0 }}\">\n");
        self.push("                        <span class=\"index\">{{ loop.index }}.</span>\n");
        self.push("                        {% if item.skip %}{% continue %}{% endif %}\n");
        self.push("                        <span class=\"name\">{{ item.name | escape }}</span>\n");
        self.push("                        {% if let Some(desc) = item.description %}\n");
        self.push("                        <p class=\"description\">{{ desc | escape | wordcount }} words</p>\n");
        self.push("                        {% endif %}\n");
        self.push("                    </li>\n");
        self.push("                    {% else %}\n");
        self.push("                    <li class=\"empty\">No items to display</li>\n");
        self.push("                    {% endfor %}\n");
        self.push("                </ul>\n");
        self.push("            </section>\n");
        self
    }

    fn add_filter_examples(mut self) -> Self {
        self.push("            <section class=\"filter-examples\">\n");
        self.push("                <h2>Text Transformations</h2>\n");
        self.push("                <dl>\n");
        self.push("                    <dt>Uppercase</dt>\n");
        self.push("                    <dd>{{ sample_text | upper }}</dd>\n");
        self.push("                    <dt>Lowercase</dt>\n");
        self.push("                    <dd>{{ sample_text | lower }}</dd>\n");
        self.push("                    <dt>Capitalized</dt>\n");
        self.push("                    <dd>{{ sample_text | capitalize }}</dd>\n");
        self.push("                    <dt>Title Case</dt>\n");
        self.push("                    <dd>{{ sample_text | title }}</dd>\n");
        self.push("                    <dt>Trimmed</dt>\n");
        self.push("                    <dd>{{ sample_text | trim }}</dd>\n");
        self.push("                    <dt>Truncated</dt>\n");
        self.push("                    <dd>{{ long_text | truncate(50) }}</dd>\n");
        self.push("                    <dt>Word count</dt>\n");
        self.push("                    <dd>{{ long_text | wordcount }}</dd>\n");
        self.push("                    <dt>Line count</dt>\n");
        self.push("                    <dd>{{ long_text | linecount }}</dd>\n");
        self.push("                </dl>\n");
        self.push("                {% filter lower %}\n");
        self.push("                <div class=\"filter-block\">\n");
        self.push("                    <p>THIS ENTIRE BLOCK WILL BE LOWERCASED</p>\n");
        self.push("                    <p>INCLUDING {{ dynamic_content | upper }}</p>\n");
        self.push("                </div>\n");
        self.push("                {% endfilter %}\n");
        self.push("            </section>\n");
        self
    }

    fn add_svg_icon_set(mut self) -> Self {
        self.push("            <section class=\"icons\">\n");
        self.push("                <h2>Icon Set</h2>\n");
        self.push("                <div class=\"icon-grid\">\n");
        self.push("                    <svg xmlns=\"http://www.w3.org/2000/svg\" viewBox=\"0 0 24 24\" class=\"icon icon-home\">\n");
        self.push("                        <path d=\"M10 20v-6h4v6h5v-8h3L12 3 2 12h3v8z\"/>\n");
        self.push("                    </svg>\n");
        self.push("                    <svg xmlns=\"http://www.w3.org/2000/svg\" viewBox=\"0 0 24 24\" class=\"icon icon-user\">\n");
        self.push("                        <path d=\"M12 12c2.21 0 4-1.79 4-4s-1.79-4-4-4-4 1.79-4 4 1.79 4 4 4zm0 2c-2.67 0-8 1.34-8 4v2h16v-2c0-2.66-5.33-4-8-4z\"/>\n");
        self.push("                    </svg>\n");
        self.push("                    <svg xmlns=\"http://www.w3.org/2000/svg\" viewBox=\"0 0 24 24\" class=\"icon icon-settings\">\n");
        self.push("                        <path d=\"M19.14 12.94c.04-.31.06-.63.06-.94 0-.31-.02-.63-.06-.94l2.03-1.58c.18-.14.23-.41.12-.61l-1.92-3.32c-.12-.22-.37-.29-.59-.22l-2.39.96c-.5-.38-1.03-.7-1.62-.94l-.36-2.54c-.04-.24-.24-.41-.48-.41h-3.84c-.24 0-.43.17-.47.41l-.36 2.54c-.59.24-1.13.57-1.62.94l-2.39-.96c-.22-.08-.47 0-.59.22L2.74 8.87c-.12.21-.08.47.12.61l2.03 1.58c-.04.31-.06.63-.06.94s.02.63.06.94l-2.03 1.58c-.18.14-.23.41-.12.61l1.92 3.32c.12.22.37.29.59.22l2.39-.96c.5.38 1.03.7 1.62.94l.36 2.54c.05.24.24.41.48.41h3.84c.24 0 .44-.17.47-.41l.36-2.54c.59-.24 1.13-.56 1.62-.94l2.39.96c.22.08.47 0 .59-.22l1.92-3.32c.12-.22.07-.47-.12-.61l-2.01-1.58zM12 15.6c-1.98 0-3.6-1.62-3.6-3.6s1.62-3.6 3.6-3.6 3.6 1.62 3.6 3.6-1.62 3.6-3.6 3.6z\"/>\n");
        self.push("                    </svg>\n");
        self.push("                </div>\n");
        self.push("            </section>\n");
        self
    }

    fn add_definition_list(mut self, items: usize) -> Self {
        self.push("            <section class=\"definitions\">\n");
        self.push("                <h2>Glossary</h2>\n");
        self.push("                <dl>\n");
        for i in 0..items {
            self.push(&format!(
                "                    <dt id=\"term-{}\">{{{{ term_{}_name | escape }}}}</dt>\n",
                i, i
            ));
            self.push(&format!(
                "                    <dd>{{{{ term_{}_definition | escape }}}}</dd>\n",
                i
            ));
            if i % 3 == 0 {
                self.push(&format!(
                    "                    {{% if term_{}_example is defined %}}\n",
                    i
                ));
                self.push(&format!("                    <dd class=\"example\"><code>{{{{ term_{}_example | escape }}}}</code></dd>\n", i));
                self.push("                    {% endif %}\n");
            }
        }
        self.push("                </dl>\n");
        self.push("            </section>\n");
        self
    }

    fn add_aside_content(mut self) -> Self {
        self.push("            <aside class=\"sidebar\">\n");
        self.push("                <h3>Related Content</h3>\n");
        self.push("                {% if related_articles is defined %}\n");
        self.push("                <ul class=\"related-list\">\n");
        self.push("                    {% for article in related_articles %}\n");
        self.push("                    <li>\n");
        self.push("                        <a href=\"{{ article.url }}\">\n");
        self.push("                            <span class=\"title\">{{ article.title | escape }}</span>\n");
        self.push("                            <time datetime=\"{{ article.date }}\">{{ article.date | date(\"%b %d\") }}</time>\n");
        self.push("                        </a>\n");
        self.push("                    </li>\n");
        self.push("                    {% endfor %}\n");
        self.push("                </ul>\n");
        self.push("                {% else %}\n");
        self.push("                <p>No related content available.</p>\n");
        self.push("                {% endif %}\n");
        self.push("                <h3>Tags</h3>\n");
        self.push("                <div class=\"tag-cloud\">\n");
        self.push("                    {% for tag in tags %}\n");
        self.push("                    {% let size = (tag.weight as f32) * 0.5 + 0.75 %}\n");
        self.push("                    <a href=\"/tags/{{ tag.slug }}\" class=\"tag\" style=\"font-size: {{ size }}rem\">{{ tag.name | escape }}</a>\n");
        self.push("                    {% endfor %}\n");
        self.push("                </div>\n");
        self.push("            </aside>\n");
        self
    }

    fn add_pagination(mut self) -> Self {
        self.push("            {% if pagination is defined %}\n");
        self.push("            <nav class=\"pagination\" aria-label=\"Page navigation\">\n");
        self.push("                <ul>\n");
        self.push("                    {% if pagination.has_prev %}\n");
        self.push("                    <li><a href=\"{{ pagination.prev_url }}\" rel=\"prev\">&laquo; Previous</a></li>\n");
        self.push("                    {% else %}\n");
        self.push(
            "                    <li class=\"disabled\"><span>&laquo; Previous</span></li>\n",
        );
        self.push("                    {% endif %}\n");
        self.push("                    {% for page in pagination.pages %}\n");
        self.push("                    {% match page %}\n");
        self.push("                    {% when PageItem::Number(n) %}\n");
        self.push("                    <li{% if n == pagination.current %} class=\"active\"{% endif %}>\n");
        self.push("                        <a href=\"{{ pagination.base_url }}?page={{ n }}\">{{ n }}</a>\n");
        self.push("                    </li>\n");
        self.push("                    {% when PageItem::Ellipsis %}\n");
        self.push("                    <li class=\"ellipsis\"><span>&hellip;</span></li>\n");
        self.push("                    {% endmatch %}\n");
        self.push("                    {% endfor %}\n");
        self.push("                    {% if pagination.has_next %}\n");
        self.push("                    <li><a href=\"{{ pagination.next_url }}\" rel=\"next\">Next &raquo;</a></li>\n");
        self.push("                    {% else %}\n");
        self.push("                    <li class=\"disabled\"><span>Next &raquo;</span></li>\n");
        self.push("                    {% endif %}\n");
        self.push("                </ul>\n");
        self.push("            </nav>\n");
        self.push("            {% endif %}\n");
        self
    }

    fn add_comments_section(mut self) -> Self {
        self.push("            <section class=\"comments\" id=\"comments\">\n");
        self.push("                <h2>Comments ({{ comments.len() }})</h2>\n");
        self.push("                {% for comment in comments %}\n");
        self.push("                <article class=\"comment\" id=\"comment-{{ comment.id }}\">\n");
        self.push("                    <header class=\"comment-meta\">\n");
        self.push("                        <img src=\"{{ comment.author.avatar | default(\"/default-avatar.png\") }}\" alt=\"\" class=\"avatar\" width=\"48\" height=\"48\">\n");
        self.push("                        <strong class=\"author\">{{ comment.author.name | escape }}</strong>\n");
        self.push("                        <time datetime=\"{{ comment.created_at }}\">{{ comment.created_at | date(\"%B %d, %Y at %H:%M\") }}</time>\n");
        self.push("                        {% if let Some(_) = comment.edited_at %}\n");
        self.push("                        <span class=\"edited\">(edited)</span>\n");
        self.push("                        {% endif %}\n");
        self.push("                    </header>\n");
        self.push("                    <div class=\"comment-body\">\n");
        self.push("                        {{ comment.content | escape | linebreaks }}\n");
        self.push("                    </div>\n");
        self.push("                    <footer class=\"comment-actions\">\n");
        self.push("                        <button type=\"button\" class=\"btn btn-sm\" data-action=\"reply\" data-comment-id=\"{{ comment.id }}\">Reply</button>\n");
        self.push(
            "                        {% if user is defined && user.id == comment.author.id %}\n",
        );
        self.push("                        <a href=\"/comments/{{ comment.id }}/edit\" class=\"btn btn-sm\">Edit</a>\n");
        self.push("                        <form method=\"post\" action=\"/comments/{{ comment.id }}/delete\" class=\"inline-form\">\n");
        self.push("                            <input type=\"hidden\" name=\"_token\" value=\"{{ csrf_token }}\">\n");
        self.push("                            <button type=\"submit\" class=\"btn btn-sm btn-danger\">Delete</button>\n");
        self.push("                        </form>\n");
        self.push("                        {% endif %}\n");
        self.push("                    </footer>\n");
        self.push("                    {% if comment.replies.len() > 0 %}\n");
        self.push("                    <div class=\"replies\">\n");
        self.push("                        {% for reply in comment.replies %}\n");
        self.push("                        <article class=\"comment reply\" id=\"comment-{{ reply.id }}\">\n");
        self.push("                            <header class=\"comment-meta\">\n");
        self.push(
            "                                <strong>{{ reply.author.name | escape }}</strong>\n",
        );
        self.push("                                <time datetime=\"{{ reply.created_at }}\">{{ reply.created_at | date(\"%B %d, %Y\") }}</time>\n");
        self.push("                            </header>\n");
        self.push("                            <div class=\"comment-body\">{{ reply.content | escape }}</div>\n");
        self.push("                        </article>\n");
        self.push("                        {% endfor %}\n");
        self.push("                    </div>\n");
        self.push("                    {% endif %}\n");
        self.push("                </article>\n");
        self.push("                {% else %}\n");
        self.push("                <p class=\"no-comments\">No comments yet. Be the first to comment!</p>\n");
        self.push("                {% endfor %}\n");
        self.push("                {% if user is defined %}\n");
        self.push(
            "                <form method=\"post\" action=\"/comments\" class=\"comment-form\">\n",
        );
        self.push("                    <input type=\"hidden\" name=\"_token\" value=\"{{ csrf_token }}\">\n");
        self.push("                    <input type=\"hidden\" name=\"article_id\" value=\"{{ article.id }}\">\n");
        self.push("                    <div class=\"form-group\">\n");
        self.push("                        <label for=\"comment-content\">Add a comment</label>\n");
        self.push("                        <textarea id=\"comment-content\" name=\"content\" class=\"form-control\" rows=\"4\" required></textarea>\n");
        self.push("                    </div>\n");
        self.push("                    <button type=\"submit\" class=\"btn btn-primary\">Post Comment</button>\n");
        self.push("                </form>\n");
        self.push("                {% else %}\n");
        self.push("                <p class=\"login-prompt\"><a href=\"/login?redirect={{ current_url | urlencode }}\">Log in</a> to leave a comment.</p>\n");
        self.push("                {% endif %}\n");
        self.push("            </section>\n");
        self
    }

    fn add_raw_block(mut self) -> Self {
        self.push("            {% raw %}\n");
        self.push("            <script type=\"text/x-template\" id=\"vue-template\">\n");
        self.push("                <div class=\"component\">\n");
        self.push("                    <h3>{{ title }}</h3>\n");
        self.push("                    <p v-for=\"item in items\">{{ item.name }}</p>\n");
        self.push("                    {% if condition %}\n");
        self.push("                    <span>This would be Askama syntax but is preserved literally</span>\n");
        self.push("                    {% endif %}\n");
        self.push("                </div>\n");
        self.push("            </script>\n");
        self.push("            {% endraw %}\n");
        self
    }

    fn add_variable_assignments(mut self) -> Self {
        self.push("            {# Variable assignment examples #}\n");
        self.push("            {% let computed_value = base_value * multiplier + offset %}\n");
        self.push("            {% set formatted_date = date | date(\"%Y-%m-%d\") %}\n");
        self.push("            {% let user_display = user.name ~ \" (\" ~ user.email ~ \")\" %}\n");
        self.push("            {% let is_special = item_count > 100 && has_premium %}\n");
        self.push("            {% let bit_check = flags bitand 0x0F %}\n");
        self.push("            {% let combined = a bitor b %}\n");
        self.push("            {% let toggled = value xor mask %}\n");
        self.push("            {% let converted = amount as i64 %}\n");
        self.push("            <div class=\"computed-values\">\n");
        self.push("                <p>Computed: {{ computed_value }}</p>\n");
        self.push("                <p>Formatted: {{ formatted_date }}</p>\n");
        self.push("                <p>User: {{ user_display }}</p>\n");
        self.push("                <p>Special: {{ is_special }}</p>\n");
        self.push(
            "                <p>Bits: {{ bit_check }} | {{ combined }} | {{ toggled }}</p>\n",
        );
        self.push("                <p>Converted: {{ converted }}</p>\n");
        self.push("            </div>\n");
        self
    }

    fn add_if_let_examples(mut self) -> Self {
        self.push("            <section class=\"optional-content\">\n");
        self.push("                {% if let Some(featured) = featured_item %}\n");
        self.push("                <div class=\"featured-item\">\n");
        self.push("                    <h3>{{ featured.title | escape }}</h3>\n");
        self.push("                    <p>{{ featured.description | escape }}</p>\n");
        self.push("                    {% if let Some(price) = featured.price %}\n");
        self.push("                    <span class=\"price\">${{ price }}</span>\n");
        self.push("                    {% endif %}\n");
        self.push("                </div>\n");
        self.push("                {% else %}\n");
        self.push("                <p>No featured item available.</p>\n");
        self.push("                {% endif %}\n");
        self.push("                {% match fetch_result %}\n");
        self.push("                {% when Ok(data) %}\n");
        self.push("                <div class=\"fetched-data\">\n");
        self.push("                    <pre>{{ data | escape }}</pre>\n");
        self.push("                </div>\n");
        self.push("                {% when Err(e) %}\n");
        self.push("                <div class=\"error\">Failed to fetch: {{ e | escape }}</div>\n");
        self.push("                {% endmatch %}\n");
        self.push("            </section>\n");
        self
    }

    fn add_close_main_content(mut self) -> Self {
        self.push("            {% endblock %}\n");
        self.push("        </div>\n");
        self.push("    </main>\n");
        self
    }

    fn add_footer(mut self) -> Self {
        self.push("    <footer class=\"site-footer\">\n");
        self.push("        <div class=\"container\">\n");
        self.push("            <div class=\"footer-grid\">\n");
        self.push("                <div class=\"footer-section\">\n");
        self.push("                    <h4>About</h4>\n");
        self.push("                    <p>{{ footer_about | escape | truncate(200) }}</p>\n");
        self.push("                </div>\n");
        self.push("                <div class=\"footer-section\">\n");
        self.push("                    <h4>Links</h4>\n");
        self.push("                    <ul>\n");
        self.push("                        {% for link in footer_links %}\n");
        self.push("                        <li><a href=\"{{ link.url }}\">{{ link.label | escape }}</a></li>\n");
        self.push("                        {% endfor %}\n");
        self.push("                    </ul>\n");
        self.push("                </div>\n");
        self.push("                <div class=\"footer-section\">\n");
        self.push("                    <h4>Contact</h4>\n");
        self.push("                    <address>\n");
        self.push("                        {{ contact.address | escape | linebreaks }}<br>\n");
        self.push("                        <a href=\"mailto:{{ contact.email }}\">{{ contact.email }}</a><br>\n");
        self.push(
            "                        <a href=\"tel:{{ contact.phone }}\">{{ contact.phone }}</a>\n",
        );
        self.push("                    </address>\n");
        self.push("                </div>\n");
        self.push("                <div class=\"footer-section\">\n");
        self.push("                    <h4>Social</h4>\n");
        self.push("                    <div class=\"social-links\">\n");
        self.push("                        {% for social in social_links %}\n");
        self.push("                        <a href=\"{{ social.url }}\" aria-label=\"{{ social.name }}\" rel=\"noopener noreferrer\" target=\"_blank\">\n");
        self.push("                            <svg class=\"icon\"><use href=\"#icon-{{ social.icon }}\"></use></svg>\n");
        self.push("                        </a>\n");
        self.push("                        {% endfor %}\n");
        self.push("                    </div>\n");
        self.push("                </div>\n");
        self.push("            </div>\n");
        self.push("            <div class=\"footer-bottom\">\n");
        self.push("                <p>&copy; {{ current_year }} {{ site_name | escape }}. All rights reserved.</p>\n");
        self.push("                <p>Built with <a href=\"https://github.com/askama-rs/askama\">Askama</a></p>\n");
        self.push("            </div>\n");
        self.push("        </div>\n");
        self.push("    </footer>\n");
        self
    }

    fn close_html(mut self) -> Self {
        self.push("</body>\n");
        self.push("</html>\n");
        self
    }

    fn repeat_section(
        mut self,
        times: usize,
        section: fn(TemplateBuilder) -> TemplateBuilder,
    ) -> Self {
        for _ in 0..times {
            self = section(self);
        }
        self
    }

    fn build(self) -> String {
        self.content
    }
}

// Small template: ~3KB - Simple page with basic Askama features
fn small_template() -> String {
    TemplateBuilder::new()
        .add_doctype_and_html_start()
        .close_head_open_body()
        .add_header_with_nav()
        .add_flash_messages()
        .add_main_content_start()
        .add_hero_section()
        .add_card_grid(2)
        .add_close_main_content()
        .add_footer()
        .close_html()
        .build()
}

// Medium template: ~20KB - Moderate page with forms and tables
fn medium_template() -> String {
    TemplateBuilder::new()
        .add_doctype_and_html_start()
        .add_style_block()
        .close_head_open_body()
        .add_header_with_nav()
        .add_breadcrumbs()
        .add_flash_messages()
        .add_main_content_start()
        .add_hero_section()
        .add_card_grid(6)
        .add_data_table(3)
        .add_form_section(10)
        .add_filter_examples()
        .add_variable_assignments()
        .add_if_let_examples()
        .add_pagination()
        .add_close_main_content()
        .add_aside_content()
        .add_footer()
        .close_html()
        .build()
}

// Large template: ~100KB - Complex page with deep nesting and many features
fn large_template() -> String {
    TemplateBuilder::new()
        .add_doctype_and_html_start()
        .add_style_block()
        .add_script_block()
        .close_head_open_body()
        .add_header_with_nav()
        .add_breadcrumbs()
        .add_flash_messages()
        .add_main_content_start()
        .add_hero_section()
        .repeat_section(2, |b| b.add_card_grid(12))
        .repeat_section(2, |b| b.add_data_table(8))
        .repeat_section(2, |b| b.add_form_section(20))
        .add_nested_conditionals(10)
        .repeat_section(2, |b| b.add_match_expressions(5))
        .add_loop_with_features(50)
        .repeat_section(2, |b| b.add_filter_examples())
        .add_svg_icon_set()
        .repeat_section(2, |b| b.add_definition_list(15))
        .repeat_section(2, |b| b.add_variable_assignments())
        .repeat_section(2, |b| b.add_if_let_examples())
        .add_raw_block()
        .add_comments_section()
        .add_pagination()
        .add_close_main_content()
        .add_aside_content()
        .add_footer()
        .close_html()
        .build()
}

// Extra large template: ~400KB - Very complex template with repeated sections
fn extra_large_template() -> String {
    TemplateBuilder::new()
        .add_doctype_and_html_start()
        .add_style_block()
        .add_script_block()
        .close_head_open_body()
        .add_header_with_nav()
        .add_breadcrumbs()
        .add_flash_messages()
        .add_main_content_start()
        .add_hero_section()
        .repeat_section(10, |b| b.add_card_grid(10))
        .repeat_section(6, |b| b.add_data_table(12))
        .repeat_section(8, |b| b.add_form_section(15))
        .repeat_section(6, |b| b.add_nested_conditionals(10))
        .repeat_section(10, |b| b.add_match_expressions(8))
        .repeat_section(8, |b| b.add_loop_with_features(100))
        .repeat_section(6, |b| b.add_filter_examples())
        .repeat_section(4, |b| b.add_svg_icon_set())
        .repeat_section(8, |b| b.add_definition_list(20))
        .repeat_section(6, |b| b.add_variable_assignments())
        .repeat_section(6, |b| b.add_if_let_examples())
        .repeat_section(4, |b| b.add_raw_block())
        .repeat_section(4, |b| b.add_comments_section())
        .add_pagination()
        .add_close_main_content()
        .repeat_section(6, |b| b.add_aside_content())
        .add_footer()
        .close_html()
        .build()
}

fn format_benchmark(c: &mut Criterion) {
    // save_bench_files();

    let mut group = c.benchmark_group("different_sizes");

    let templates = [
        ("small", small_template()),
        ("medium", medium_template()),
        ("large", large_template()),
        ("extra_large", extra_large_template()),
    ];

    for (name, template) in &templates {
        group.throughput(Throughput::Bytes(template.len() as u64));
        group.bench_with_input(BenchmarkId::from_parameter(name), template, |b, tmpl| {
            let mut session = Session::default();
            b.iter(|| session.format(black_box(tmpl), "bench.html"));
        });
    }

    group.finish();
}

criterion_group!(different_sizes_benches, format_benchmark);
criterion_main!(different_sizes_benches);

fn save_bench_files() {
    use std::fs;
    use std::path::Path;

    let output_dir = Path::new("target/bench_templates");
    fs::create_dir_all(output_dir).unwrap();

    let templates = [
        ("small.html", small_template()),
        ("medium.html", medium_template()),
        ("large.html", large_template()),
        ("extra_large.html", extra_large_template()),
    ];

    for (filename, content) in &templates {
        let path = output_dir.join(filename);
        fs::write(&path, content).unwrap();
        println!("Generated: {} ({} bytes)", path.display(), content.len());
    }
}
