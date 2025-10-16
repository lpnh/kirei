use assert_cmd::Command;
use assert_fs::prelude::*;
use predicates::prelude::*;

#[test]
fn formats_file_to_stdout() {
    let temp = assert_fs::TempDir::new().unwrap();
    let file = temp.child("comment.html");
    file.write_str("{# A Comment #}\n").unwrap();

    Command::cargo_bin("kirei")
        .unwrap()
        .arg(file.path())
        .assert()
        .success()
        .stdout(predicate::str::contains("{# A Comment #}"));
}

#[test]
fn formats_askama_variables() {
    let temp = assert_fs::TempDir::new().unwrap();
    let file = temp.child("template.html");
    file.write_str("{% if user %}Hello {{ user.name }}!{% endif %}")
        .unwrap();

    Command::cargo_bin("kirei")
        .unwrap()
        .arg(file.path())
        .assert()
        .success()
        .stdout(predicate::str::contains("{% if user %}"))
        .stdout(predicate::str::contains("{{ user.name }}"));
}

#[test]
fn reads_from_stdin() {
    Command::cargo_bin("kirei")
        .unwrap()
        .arg("-")
        .write_stdin("{% if active %}<div><p>{{ message }}</p></div>{% endif %}")
        .assert()
        .success()
        .stdout(predicate::str::contains("{% if active %}"))
        .stdout(predicate::str::contains("{{ message }}"));
}

#[test]
fn output_ends_with_newline() {
    let temp = assert_fs::TempDir::new().unwrap();
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
    let temp = assert_fs::TempDir::new().unwrap();
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

    let first_format = std::fs::read_to_string(file.path()).unwrap();

    Command::cargo_bin("kirei")
        .unwrap()
        .arg("--write")
        .arg(file.path())
        .assert()
        .success();

    let second_format = std::fs::read_to_string(file.path()).unwrap();

    assert_eq!(
        first_format, second_format,
        "Formatting should be idempotent"
    );
}

#[test]
fn passes_when_formatted() {
    let temp = assert_fs::TempDir::new().unwrap();
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
    let temp = assert_fs::TempDir::new().unwrap();
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
    let temp = assert_fs::TempDir::new().unwrap();
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
        .stderr(predicate::str::contains("File needs formatting"));
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
    let temp = assert_fs::TempDir::new().unwrap();
    let file = temp.child("test.html");
    let unformatted = "{% if user %}<div>  <p>{{ user.name }}</p>  </div>{% endif %}";
    file.write_str(unformatted).unwrap();

    Command::cargo_bin("kirei")
        .unwrap()
        .arg("--write")
        .arg(file.path())
        .assert()
        .success();

    let content = std::fs::read_to_string(file.path()).unwrap();
    assert_ne!(content, unformatted);
    assert!(content.contains("{% if user %}"));
    assert!(content.contains("{{ user.name }}"));
}

#[test]
fn write_mode_with_stdin_prints_to_stdout() {
    Command::cargo_bin("kirei")
        .unwrap()
        .arg("-")
        .arg("--write")
        .write_stdin("{% for tag in tags %}<span>  {{ tag }}  </span>{% endfor %}")
        .assert()
        .success()
        .stdout(predicate::str::contains("{% for tag in tags %}"))
        .stdout(predicate::str::contains("{{ tag }}"));
}

#[test]
fn list_different_shows_unformatted() {
    let temp = assert_fs::TempDir::new().unwrap();
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
        .stdout(predicate::str::contains("test.html"));
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
        .stdout(predicate::str::contains("<stdin>"));
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
        .stdout(predicate::str::contains("custom/path.html"));
}

#[test]
fn handles_empty_files() {
    let temp = assert_fs::TempDir::new().unwrap();
    let file = temp.child("empty.html");
    file.write_str("").unwrap();

    Command::cargo_bin("kirei")
        .unwrap()
        .arg(file.path())
        .assert()
        .success();
}

#[test]
fn handles_malformed_html() {
    let temp = assert_fs::TempDir::new().unwrap();
    let file = temp.child("malformed.html");
    file.write_str("<div").unwrap();

    Command::cargo_bin("kirei")
        .unwrap()
        .arg(file.path())
        .assert()
        .success();
}

#[test]
fn handles_utf8_special_chars() {
    let temp = assert_fs::TempDir::new().unwrap();
    let file = temp.child("utf8.html");
    file.write_str("<div>\u{FFFD}\u{0000}</div>").unwrap();

    Command::cargo_bin("kirei")
        .unwrap()
        .arg(file.path())
        .assert()
        .success();
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
fn fails_on_invalid_utf8() {
    let temp = assert_fs::TempDir::new().unwrap();
    let file = temp.child("invalid.html");

    std::fs::write(file.path(), &[0xFF, 0xFE, 0xFD]).unwrap();

    Command::cargo_bin("kirei")
        .unwrap()
        .arg(file.path())
        .assert()
        .failure();
}

#[test]
fn help_shows_description() {
    Command::cargo_bin("kirei")
        .unwrap()
        .arg("--help")
        .assert()
        .success()
        .stdout(predicate::str::contains("Askama formatter"));
}

#[test]
fn version_flag_works() {
    Command::cargo_bin("kirei")
        .unwrap()
        .arg("--version")
        .assert()
        .success()
        .stdout(predicate::str::contains("kirei"));
}
