use assert_cmd::Command;
use assert_fs::{TempDir, prelude::*};
use predicates::str::contains;
use std::fs::{read_to_string, write};

#[test]
fn formats_simple_template() {
    let temp = TempDir::new().unwrap();
    let file = temp.child("template.html");
    file.write_str("{# A Comment #}\n{% if let Some(user) = user %}<i>Bonjour</i> {{ user.name }} !{% endif %}").unwrap();

    Command::cargo_bin("kirei")
        .unwrap()
        .arg(file.path())
        .assert()
        .success()
        .stdout(contains("{# A Comment #}"))
        .stdout(contains("{% if let Some(user) = user %}"))
        .stdout(contains("<i>Bonjour</i>"))
        .stdout(contains("{{ user.name }}"))
        .stdout(contains("!"))
        .stdout(contains("{% endif %}"));
}

#[test]
fn reads_from_stdin() {
    Command::cargo_bin("kirei")
        .unwrap()
        .arg("-")
        .write_stdin("{% if active %}<div><p>{{ message }}</p></div>{% endif %}")
        .assert()
        .success()
        .stdout(contains("{% if active %}"))
        .stdout(contains("{{ message }}"));
}

#[test]
fn output_ends_with_newline() {
    let temp = TempDir::new().unwrap();
    let file = temp.child("test.html");
    file.write_str("{% for n in numbers %}<span>{{ n }}</span>{% endfor %}")
        .unwrap();

    let mut cmd = Command::cargo_bin("kirei").unwrap();
    let output = cmd.arg(file.path()).output().unwrap();
    let formatted = String::from_utf8(output.stdout).unwrap();

    assert!(formatted.ends_with('\n'), "Output should end with newline");
}

#[test]
fn format_is_idempotent() {
    let temp = TempDir::new().unwrap();
    let file = temp.child("test.html");
    file.write_str(
        "{% if enabled %}<div>  {{ title }}  </div>{% else %}<span>Disabled</span>{% endif %}",
    )
    .unwrap();

    Command::cargo_bin("kirei")
        .unwrap()
        .arg("--write")
        .arg(file.path())
        .assert()
        .success();

    let first_format = read_to_string(file.path()).unwrap();

    Command::cargo_bin("kirei")
        .unwrap()
        .arg("--write")
        .arg(file.path())
        .assert()
        .success();

    let second_format = read_to_string(file.path()).unwrap();

    assert_eq!(
        first_format, second_format,
        "Formatting should be idempotent"
    );
}

#[test]
fn passes_when_formatted() {
    let temp = TempDir::new().unwrap();
    let file = temp.child("test.html");
    file.write_str("").unwrap();

    let mut cmd = Command::cargo_bin("kirei").unwrap();
    let output = cmd.arg(file.path()).output().unwrap();
    let formatted = String::from_utf8(output.stdout).unwrap();

    file.write_str(&formatted).unwrap();

    Command::cargo_bin("kirei")
        .unwrap()
        .arg("--check")
        .arg(file.path())
        .assert()
        .success();
}

#[test]
fn fails_when_unformatted() {
    let temp = TempDir::new().unwrap();
    let file = temp.child("test.html");
    file.write_str("<div>  {% for item in items %}  {{ item }}  {% endfor %}  </div>")
        .unwrap();

    Command::cargo_bin("kirei")
        .unwrap()
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

    Command::cargo_bin("kirei")
        .unwrap()
        .arg("--check")
        .arg(file.path())
        .assert()
        .failure()
        .code(1)
        .stderr(contains("File needs formatting"));
}

#[test]
fn stdin_passes_when_formatted() {
    Command::cargo_bin("kirei")
        .unwrap()
        .arg("-")
        .arg("--check")
        .write_stdin("<div></div>\n")
        .assert()
        .success();
}

#[test]
fn stdin_fails_when_unformatted() {
    Command::cargo_bin("kirei")
        .unwrap()
        .arg("-")
        .arg("--check")
        .write_stdin("{% match status %}{% when Status::Active %}<span>Active</span>{% endmatch %}")
        .assert()
        .failure()
        .code(1);
}

#[test]
fn write_modifies_file() {
    let temp = TempDir::new().unwrap();
    let file = temp.child("test.html");
    let unformatted =
        "{% if let Some(user) = user %}<div>  <p>{{ user.name }}</p>  </div>{% endif %}";
    file.write_str(unformatted).unwrap();

    Command::cargo_bin("kirei")
        .unwrap()
        .arg("--write")
        .arg(file.path())
        .assert()
        .success();

    let content = read_to_string(file.path()).unwrap();
    assert_ne!(content, unformatted);
    assert!(content.contains("{% if let Some(user) = user %}"));
    assert!(content.contains("{{ user.name }}"));
}

#[test]
fn write_with_stdin_prints_to_stdout() {
    Command::cargo_bin("kirei")
        .unwrap()
        .arg("-")
        .arg("--write")
        .write_stdin("{% for tag in tags %}<span>  {{ tag }}  </span>{% endfor %}")
        .assert()
        .success()
        .stdout(contains("{% for tag in tags %}"))
        .stdout(contains("{{ tag }}"));
}

#[test]
fn list_different_shows_unformatted() {
    let temp = TempDir::new().unwrap();
    let file = temp.child("test.html");
    file.write_str("<ul>{% for user in users %}  <li>{{ user }}</li>  {% endfor %}</ul>")
        .unwrap();

    Command::cargo_bin("kirei")
        .unwrap()
        .arg("--list-different")
        .arg(file.path())
        .assert()
        .failure()
        .code(1)
        .stdout(contains("test.html"));
}

#[test]
fn list_different_with_stdin_shows_stdin() {
    Command::cargo_bin("kirei")
        .unwrap()
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
    Command::cargo_bin("kirei")
        .unwrap()
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
    Command::cargo_bin("kirei")
        .unwrap()
        .arg("this_file_does_not_exist.html")
        .assert()
        .failure();
}

#[test]
fn handles_empty_files() {
    let temp = TempDir::new().unwrap();
    let file = temp.child("empty.html");
    file.write_str("").unwrap();

    Command::cargo_bin("kirei")
        .unwrap()
        .arg(file.path())
        .assert()
        .success();
}

#[test]
fn handles_whitespace_only() {
    let temp = TempDir::new().unwrap();
    let file = temp.child("whitespace_only.html");
    file.write_str(" \t \n \t \n").unwrap();

    Command::cargo_bin("kirei")
        .unwrap()
        .arg(file.path())
        .assert()
        .success();
}

#[test]
fn handles_malformed_tmpl() {
    let temp = TempDir::new().unwrap();
    let file = temp.child("malformed_tmpl.html");
    file.write_str("{% block foo %}{% endblock").unwrap();

    Command::cargo_bin("kirei")
        .unwrap()
        .arg(file.path())
        .assert()
        .success();
}

#[test]
fn handles_malformed_html() {
    let temp = TempDir::new().unwrap();
    let file = temp.child("malformed_html.html");
    file.write_str("<div").unwrap();

    Command::cargo_bin("kirei")
        .unwrap()
        .arg(file.path())
        .assert()
        .success();
}

#[test]
fn fails_on_invalid_utf8() {
    let temp = TempDir::new().unwrap();
    let file = temp.child("invalid.html");

    write(file.path(), [0xFF, 0xFE, 0xFD]).unwrap();

    Command::cargo_bin("kirei")
        .unwrap()
        .arg(file.path())
        .assert()
        .failure();
}

#[test]
fn handles_utf8_special_chars() {
    let temp = TempDir::new().unwrap();
    let file = temp.child("utf8.html");
    file.write_str("<div>\u{FFFD}\u{0000}</div>").unwrap();

    Command::cargo_bin("kirei")
        .unwrap()
        .arg(file.path())
        .assert()
        .success();
}

#[test]
fn version_flag_works() {
    Command::cargo_bin("kirei")
        .unwrap()
        .arg("--version")
        .assert()
        .success()
        .stdout(contains("kirei"));
}

#[test]
fn help_shows_description() {
    Command::cargo_bin("kirei")
        .unwrap()
        .arg("--help")
        .assert()
        .success()
        .stdout(contains("Askama formatter"));
}
