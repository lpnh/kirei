use assert_cmd::{Command, cargo};
use assert_fs::{TempDir, prelude::*};
use predicates::str::contains;
use std::fs::write;

const ERR_PARSE_ASKAMA: &str = "failed to parse Askama: syntax error";
const ERR_PARSE_HTML: &str = "failed to parse HTML: syntax error";

#[test]
fn fails_when_unformatted() {
    let temp = TempDir::new().unwrap();
    let file = temp.child("test.html");
    file.write_str("<div>  {% for item in items %}  {{ item }}  {% endfor %}  </div>")
        .unwrap();

    Command::new(cargo::cargo_bin!("kirei"))
        .arg("--check")
        .arg(file.path())
        .assert()
        .failure()
        .code(1);
}

#[test]
fn prints_error_to_stderr() {
    let temp = TempDir::new().unwrap();
    let file = temp.child("test.html");
    file.write_str("{% if user.logged_in %}<p>  Welcome {{ user.name }}!  </p>{% endif %}")
        .unwrap();

    Command::new(cargo::cargo_bin!("kirei"))
        .arg("--check")
        .arg(file.path())
        .assert()
        .failure()
        .code(1)
        .stderr(contains("File needs formatting"));
}

#[test]
fn stdin_fails_when_unformatted() {
    Command::new(cargo::cargo_bin!("kirei"))
        .arg("-")
        .arg("--check")
        .write_stdin("{% match status %}{% when Status::Active %}<span>Active</span>{% endmatch %}")
        .assert()
        .failure()
        .code(1);
}

#[test]
fn list_different_shows_unformatted() {
    let temp = TempDir::new().unwrap();
    let file = temp.child("test.html");
    file.write_str("<ul>{% for user in users %}  <li>{{ user }}</li>  {% endfor %}</ul>")
        .unwrap();

    Command::new(cargo::cargo_bin!("kirei"))
        .arg("--list-different")
        .arg(file.path())
        .assert()
        .failure()
        .code(1)
        .stdout(contains("test.html"));
}

#[test]
fn list_different_with_stdin_shows_stdin() {
    Command::new(cargo::cargo_bin!("kirei"))
        .arg("-")
        .arg("--list-different")
        .write_stdin("{% if show %}<div>  <p>{{ content }}</p>  </div>{% endif %}")
        .assert()
        .failure()
        .code(1)
        .stdout(contains("<stdin>"));
}

#[test]
fn stdin_filepath_with_list_different() {
    Command::new(cargo::cargo_bin!("kirei"))
        .arg("-")
        .arg("--list-different")
        .arg("--stdin-filepath")
        .arg("custom/path.html")
        .write_stdin("{% for i in items %}<div>  {{ i.name }}  </div>{% endfor %}")
        .assert()
        .failure()
        .code(1)
        .stdout(contains("custom/path.html"));
}

#[test]
fn fails_on_missing_file() {
    Command::new(cargo::cargo_bin!("kirei"))
        .arg("this_file_does_not_exist.html")
        .assert()
        .failure();
}

#[test]
fn fails_on_invalid_utf8() {
    let temp = TempDir::new().unwrap();
    let file = temp.child("invalid.html");

    write(file.path(), [0xFF, 0xFE, 0xFD]).unwrap();

    Command::new(cargo::cargo_bin!("kirei"))
        .arg(file.path())
        .assert()
        .failure();
}

#[test]
fn fails_on_null_character() {
    let temp = TempDir::new().unwrap();
    let file = temp.child("null_char.html");
    file.write_str("<div>\u{0000}</div>").unwrap();

    Command::new(cargo::cargo_bin!("kirei"))
        .arg(file.path())
        .assert()
        .failure()
        .stderr(contains(ERR_PARSE_ASKAMA));
}

#[test]
fn fails_on_malformed_askama() {
    let temp = TempDir::new().unwrap();
    let file = temp.child("malformed_tmpl.html");
    file.write_str("{% block foo %}{% endblock").unwrap();

    Command::new(cargo::cargo_bin!("kirei"))
        .arg(file.path())
        .assert()
        .failure()
        .stderr(contains(ERR_PARSE_ASKAMA));
}

#[test]
fn fails_on_malformed_html() {
    let temp = TempDir::new().unwrap();
    let file = temp.child("malformed_html.html");
    file.write_str("<div").unwrap();

    Command::new(cargo::cargo_bin!("kirei"))
        .arg(file.path())
        .assert()
        .failure()
        .stderr(contains(ERR_PARSE_HTML));
}

#[test]
fn fails_on_invalid_block() {
    let temp = TempDir::new().unwrap();
    let file = temp.child("invalid_block.html");
    file.write_str("{% blop %}<p>A cool paragraph</p>").unwrap();

    Command::new(cargo::cargo_bin!("kirei"))
        .arg(file.path())
        .assert()
        .failure()
        .stderr(contains(ERR_PARSE_ASKAMA));
}

#[test]
fn fails_on_invalid_nested_block() {
    let temp = TempDir::new().unwrap();
    let file = temp.child("invalid_nested_block.html");
    file.write_str("<body>{% block first %}{% blop %}<p>A cool paragraph</p>{% endblock %}{% endblock %}</body>")
        .unwrap();

    Command::new(cargo::cargo_bin!("kirei"))
        .arg(file.path())
        .assert()
        .failure()
        .stderr(contains(ERR_PARSE_ASKAMA));
}

#[test]
fn fails_on_mixed_unclosed_attr_quote() {
    let temp = TempDir::new().unwrap();
    let file = temp.child("mixed_unclosed_attr_quote.html");
    file.write_str("<div class=\"{{ color }}>something</div>")
        .unwrap();

    Command::new(cargo::cargo_bin!("kirei"))
        .arg(file.path())
        .assert()
        .failure()
        .stderr(contains(ERR_PARSE_HTML));
}

#[test]
fn fails_on_unclosed_attr_quote() {
    let temp = TempDir::new().unwrap();
    let file = temp.child("unclosed_attr_quote.html");
    file.write_str(
        r#"<form id="login-form" class=user-form needs-validation" action="/login-submit" method="post" novalidate>
  <input type="email" id="user-email" class="form-control" name="email placeholder="Enter your email" required aria-label="Email Address">
</form>"#,
    )
    .unwrap();

    Command::new(cargo::cargo_bin!("kirei"))
        .arg(file.path())
        .assert()
        .failure()
        .stderr(contains(ERR_PARSE_HTML));
}

#[test]
fn fails_on_unclosed_paragraph() {
    let temp = TempDir::new().unwrap();
    let file = temp.child("unclosed_paragraph.html");
    file.write_str("<p>Hello!").unwrap();

    Command::new(cargo::cargo_bin!("kirei"))
        .arg(file.path())
        .assert()
        .failure()
        .stderr(contains(ERR_PARSE_HTML));
}
