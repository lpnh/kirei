use kirei::formatter::AskamaFormatter;

#[test]
fn layout() {
    let mut formatter = AskamaFormatter::default();

    let input = r#"
{#-
    This is the basic layout of our example application.
    It is the core skeleton shared between all pages.
    It expects the struct of any template that `{% extends %}` this layout to contain
    (at least) a field `lang: Lang`, so it can be used in the `<html lang=` attribute.
-#}
<!DOCTYPE html>
<html lang="{{lang}}">
<head>
<meta charset="UTF-8" />
{#-
    A base template can contain `blocks`, which my be overridden templates that use
    this base template. A block may contain a default content, if the extending
    template does want to / need to override the content of a block.

    E.g. maybe you would like to have "Askama example application" as default title for
    your pages, then simply add this text (without quotation marks) in the block!

    The default content can be as complex as you need it to be.
    E.g. it may contain any nodes like `{% if … %}`, and even other blocks.
~#}
<title>{% block title %}{% endblock %}</title>
<meta http-equiv="expires" content="Sat, 01 Dec 2001 00:00:00 GMT" />
<meta http-equiv="cache-control" content="no-cache, no-store, must-revalidate" />
<meta http-equiv="pragma" content="no-cache" />
<meta name="viewport" content="width=device-width, initial-scale=1" />
<meta name="robots" content="noindex, nofollow" />
{#-
    In a real application you most likely would want to link style sheets,
    any JavaScripts etc. using e.g. `actix-files`, instead of embedding the content
    in your generated HTML.

    As you can see, this comment starts with `-`, which will tells the comment
    to strip all white spaces before it, until it finds the first non-white space
    character, a `>`.

    The comment is also terminated with `~`. This also strips white spaces, but
    will leave one space, or a newline character, if the stripped content contained
    a newline.
~#}
<style>
/*<![CDATA[*/
    {%~ include "_layout.css" ~%}
/*]]>*/
</style>
</head>
<body>
{%~ block content %}{% endblock ~%}
</body>
</html>
{%- macro lang_select(page, query="") -%}
<ul id="lang-select">
{%- if lang != Lang::en -%}
<li><a href="/en/{{ page }}.html{{ query }}">This page in English</a></li>
{%- endif -%}
{%- if lang != Lang::de -%}
<li><a href="/de/{{ page }}.html{{ query }}">Diese Seite auf deutsch.</a></li>
{%- endif -%}
{%- if lang != Lang::fr -%}
<li><a href="/fr/{{ page }}.html{{ query }}">Cette page est en français.</a></li>
{%- endif -%}
</ul>
{%- endmacro lang_select -%}
"#;

    let formatted_output = formatter.format(input).expect("Formatting failed");

    insta::assert_snapshot!(input);
    insta::assert_snapshot!(formatted_output);
}

#[test]
fn error() {
    let mut formatter = AskamaFormatter::default();

    let input = r#"
{% extends "_layout.html" %}
{%- block title -%}
{%- match err -%}
{% when AppError::NotFound -%} 404: Not Found
{% else -%} 500: Internal Server Error
{%- endmatch -%}
{%- endblock -%}
{%- block content -%}
<h1>
{%- match err -%}
{% when AppError::NotFound -%} 404: Not Found
{% else -%} 500: Internal Server Error
{%- endmatch -%}
</h1>
{%- match err -%}
{% when AppError::NotFound -%}
{% when AppError::Render(err) -%}
<pre>{{ err }}</pre>
{%- endmatch -%}
<h2><a href="/">Back to the first page.</a></h2>
{%- endblock -%}
"#;

    let formatted_output = formatter.format(input).expect("Formatting failed");

    insta::assert_snapshot!(input);
    insta::assert_snapshot!(formatted_output);
}

#[test]
fn greet() {
    let mut formatter = AskamaFormatter::default();

    let input = r#"
{% extends "_layout.html" %}
{%- block title -%}
{%- match lang -%}
{%- when Lang::en -%} Hello, {{name}}!
{%- when Lang::de -%} Hallo, {{name}}!
{%- when Lang::fr -%} Bonjour, {{name}}!
{%- endmatch -%}
{%- endblock -%}
{%- block content -%}
<h1>
{%- match lang -%}
{%- when Lang::en -%} Hello!
{%- when Lang::de -%} Hallo!
{%- when Lang::fr -%} Bonjour!
{%- endmatch -%}
</h1>
<p>
{%- match lang -%}
{%- when Lang::en -%}
Hello, <strong>{{name}}</strong>, nice to meet you! {#-~#}
I'm a <a href="https://askama.readthedocs.io/">Askama</a> example application.
{%- when Lang::de -%}
Hallo, <strong>{{name}}</strong>, schön dich kennenzulernen! {#-~#}
Ich bin eine <a href="https://askama.readthedocs.io/">Askama</a>-Beispielanwendung.
{%- when Lang::fr -%}
Bonjour, <strong>{{name}}</strong>, ravi de vous rencontrer ! {#-~#}
Je suis une application d'exemple de <a href="https://askama.readthedocs.io/">Askama</a>.
{%- endmatch -%}
</p>
<h2>
<a href="/{{ lang }}/index.html?name={{ name | urlencode }}">
{%- match lang -%}
{%- when Lang::en -%} Back to the first page.
{%- when Lang::de -%} Zurück zur ersten Seite.
{%- when Lang::fr -%} Retour à la première page.
{%- endmatch -%}
</a>
</h2>
{%- call lang_select("greet-me", name|urlencode|fmt("?name={}")) -%}{%- endcall -%}
{%- endblock -%}
"#;

    let formatted_output = formatter.format(input).expect("Formatting failed");

    insta::assert_snapshot!(input);
    insta::assert_snapshot!(formatted_output);
}

#[test]
fn index() {
    let mut formatter = AskamaFormatter::default();

    let input = r#"
{% extends "_layout.html" %}
{%- block title -%}
{#-
    In here you can see how to use the language URL compment to select the text to display.
-#}
{%- match lang -%}
{%- when Lang::en -%} Hello!
{%- when Lang::de -%} Hallo!
{%- when Lang::fr -%} Bonjour!
{%- endmatch -%}
{%- endblock -%}
{%- block content -%}
<h1>
{%- match lang -%}
{%- when Lang::en -%} Hello!
{%- when Lang::de -%} Hallo!
{%- when Lang::fr -%} Bonjour!
{%- endmatch -%}
</h1>
<form
method="GET"
action="/{{ lang }}/greet-me.html"
autocomplete="off"
>
<p>
{#-
    If your text contains long lines, you may want to split them,
    so you as a developer have an easier time reading them.

    If you don't want to end up with a multitude of spaces in the
    generated content, you can use empty comments as seen below: `#-~#`.
    This would strip the space before the comment, and leave a newline
    character after the comment. Another option would be `#~-#`,
    so that single space remains.
-#}
{%- match lang -%}
{%- when Lang::en -%}
I would like to say <em>hello</em>. {#-~#}
Would you please tell me your name?
{%- when Lang::de -%}
Ich möchte dir gerne <em>hallo</em> sagen. {#-~#}
Bitte nenne mir doch deinen Namen!
{%- when Lang::fr -%}
Je voudrais vous dire <em>bonjour</em>. {#-~#}
Pourriez-vous me donner votre nom ?
{%- endmatch -%}
</p>
<p>
<label>
{%- match lang -%}
{%- when Lang::en -%} My name is
{%- when Lang::de -%} Ich heiße
{%- when Lang::fr -%} Je m'appelle
{%- endmatch -%}:
<input
type="text"
value="{{name}}"
name="name"
required
style="width: 10em"
/>
</label>
</p>
<p>
<label>
<button type="submit">
{%- match lang -%}
{%- when Lang::en -%} Greet me, then!
{%- when Lang::de -%} Dann begrüße mich!
{%- when Lang::fr -%} Saluons-nous !
{%- endmatch -%}
</button>
</label>
</p>
</form>
{#-
    The called macro is defined in base template "_layout.html",
    and used to display the language selection footer.
~#}
{%- call lang_select("index") -%}{%- endcall -%}
{%- endblock -%}
"#;

    let formatted_output = formatter.format(input).expect("Formatting failed");

    insta::assert_snapshot!(input);
    insta::assert_snapshot!(formatted_output);
}
