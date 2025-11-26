use assert_cmd::{Command, cargo};
use assert_fs::{TempDir, prelude::*};
use predicates::str::contains;
use std::fs::read_to_string;

#[test]
fn formats_simple_template() {
    let temp = TempDir::new().unwrap();
    let file = temp.child("template.html");
    file.write_str("{# A Comment #}\n{% if let Some(user) = user %}<i>Bonjour</i> {{ user.name }} !{% endif %}").unwrap();

    Command::new(cargo::cargo_bin!("kirei"))
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
    Command::new(cargo::cargo_bin!("kirei"))
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

    let mut cmd = Command::new(cargo::cargo_bin!("kirei"));
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

    Command::new(cargo::cargo_bin!("kirei"))
        .arg("--write")
        .arg(file.path())
        .assert()
        .success();

    let first_format = read_to_string(file.path()).unwrap();

    Command::new(cargo::cargo_bin!("kirei"))
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
fn format_is_idempotent_2() {
    let temp = TempDir::new().unwrap();
    let file = temp.child("test.html");
    file.write_str(
        "<div>\n<p>\n{#\n    This\n    is a\n    multiline-comment\n#}\n</p>\n</div>\n{% block comment %}\n    {#\n        This is a list:\n        - Lorem ipsum dolor sit amet, qui minim labore adipisicing minim sint cillum sint consectetur cupidatat.\n        - Lorem ipsum dolor sit amet, qui minim labore adipisicing minim sint cillum sint consectetur cupidatat.\n        Lorem ipsum dolor sit amet, qui minim labore adipisicing minim sint cillum sint consectetur cupidatat.\n        - Lorem ipsum dolor sit amet, qui minim labore adipisicing minim sint cillum sint consectetur cupidatat.\n    #}\n{% endblock %}\n",
    )
    .unwrap();

    Command::new(cargo::cargo_bin!("kirei"))
        .arg("--write")
        .arg(file.path())
        .assert()
        .success();

    let first_format = read_to_string(file.path()).unwrap();

    Command::new(cargo::cargo_bin!("kirei"))
        .arg("--write")
        .arg(file.path())
        .assert()
        .success();

    let second_format = read_to_string(file.path()).unwrap();

    assert_eq!(
        first_format, second_format,
        "Formatting should be idempotent for nested multiline comments"
    );
}

#[test]
fn passes_when_formatted() {
    let temp = TempDir::new().unwrap();
    let file = temp.child("test.html");
    file.write_str("").unwrap();

    let mut cmd = Command::new(cargo::cargo_bin!("kirei"));
    let output = cmd.arg(file.path()).output().unwrap();
    let formatted = String::from_utf8(output.stdout).unwrap();

    file.write_str(&formatted).unwrap();

    Command::new(cargo::cargo_bin!("kirei"))
        .arg("--check")
        .arg(file.path())
        .assert()
        .success();
}

#[test]
fn stdin_passes_when_formatted() {
    Command::new(cargo::cargo_bin!("kirei"))
        .arg("-")
        .arg("--check")
        .write_stdin("<div></div>\n")
        .assert()
        .success();
}

#[test]
fn write_modifies_file() {
    let temp = TempDir::new().unwrap();
    let file = temp.child("test.html");
    let unformatted =
        "{% if let Some(user) = user %}<div>  <p>{{ user.name }}</p>  </div>{% endif %}";
    file.write_str(unformatted).unwrap();

    Command::new(cargo::cargo_bin!("kirei"))
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
    Command::new(cargo::cargo_bin!("kirei"))
        .arg("-")
        .arg("--write")
        .write_stdin("{% for tag in tags %}<span>  {{ tag }}  </span>{% endfor %}")
        .assert()
        .success()
        .stdout(contains("{% for tag in tags %}"))
        .stdout(contains("{{ tag }}"));
}

#[test]
fn handles_empty_files() {
    let temp = TempDir::new().unwrap();
    let file = temp.child("empty.html");
    file.write_str("").unwrap();

    Command::new(cargo::cargo_bin!("kirei"))
        .arg(file.path())
        .assert()
        .success();
}

#[test]
fn handles_whitespace_only() {
    let temp = TempDir::new().unwrap();
    let file = temp.child("whitespace_only.html");
    file.write_str(" \t \n \t \n").unwrap();

    Command::new(cargo::cargo_bin!("kirei"))
        .arg(file.path())
        .assert()
        .success();
}

#[test]
fn handles_utf8_replacement_char() {
    let temp = TempDir::new().unwrap();
    let file = temp.child("utf8.html");
    file.write_str("<div>\u{FFFD}</div>").unwrap();

    Command::new(cargo::cargo_bin!("kirei"))
        .arg(file.path())
        .assert()
        .success();
}

#[test]
fn version_flag_works() {
    Command::new(cargo::cargo_bin!("kirei"))
        .arg("--version")
        .assert()
        .success()
        .stdout(contains("kirei"));
}

#[test]
fn help_shows_description() {
    Command::new(cargo::cargo_bin!("kirei"))
        .arg("--help")
        .assert()
        .success()
        .stdout(contains("Askama formatter"));
}
