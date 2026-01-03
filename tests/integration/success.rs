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
fn format_is_idempotent_3() {
    let temp = TempDir::new().unwrap();
    let file = temp.child("test.html");
    file.write_str(r#"<div>\n<div>\n<div>\n<div>\n<div>\n<div>\n<div>\n<div>\n<p class="text-sm opacity-70">\nSensação: {{ "{:.1}"|format(data.current.feels_like) }}°\n</p>\n</div>\n</div>\n</div>\n</div>\n</div>\n</div>\n</div>\n</div>"#).unwrap();

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
        "Formatting should be idempotent for expressions"
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

#[test]
fn writes_multiple_files() {
    let temp = TempDir::new().unwrap();
    let file1 = temp.child("file1.html");
    let file2 = temp.child("file2.html");

    file1
        .write_str("{% if x %}<div>  {{ title }}  </div>{% endif %}")
        .unwrap();
    file2
        .write_str("{% for item in items %}<p>  {{ item }}  </p>{% endfor %}")
        .unwrap();

    Command::new(cargo::cargo_bin!("kirei"))
        .arg("--write")
        .arg(file1.path())
        .arg(file2.path())
        .assert()
        .success();

    let content1 = read_to_string(file1.path()).unwrap();
    let content2 = read_to_string(file2.path()).unwrap();

    assert!(content1.contains("{% if x %}"));
    assert!(content2.contains("{% for item in items %}"));
}

#[test]
fn checks_multiple_files_all_formatted() {
    let temp = TempDir::new().unwrap();
    let file1 = temp.child("file1.html");
    let file2 = temp.child("file2.html");

    file1.write_str("<div></div>\n").unwrap();
    file2.write_str("<p></p>\n").unwrap();

    Command::new(cargo::cargo_bin!("kirei"))
        .arg("--check")
        .arg(file1.path())
        .arg(file2.path())
        .assert()
        .success();
}

#[test]
fn glob_pattern_matches_multiple_files() {
    let temp = TempDir::new().unwrap();
    temp.child("test1.html")
        .write_str("{% if a %}<div></div>{% endif %}")
        .unwrap();
    temp.child("test2.html")
        .write_str("{% if b %}<p></p>{% endif %}")
        .unwrap();
    temp.child("ignore.txt").write_str("text").unwrap();

    Command::new(cargo::cargo_bin!("kirei"))
        .arg("--write")
        .arg("*.html")
        .current_dir(temp.path())
        .assert()
        .success();

    assert!(temp.child("test1.html").path().exists());
    assert!(temp.child("test2.html").path().exists());
}

#[test]
fn multiple_files_to_stdout_concatenates() {
    let temp = TempDir::new().unwrap();
    let file1 = temp.child("file1.html");
    let file2 = temp.child("file2.html");

    file1.write_str("<div>  first  </div>").unwrap();
    file2.write_str("<p>  second  </p>").unwrap();

    Command::new(cargo::cargo_bin!("kirei"))
        .arg(file1.path())
        .arg(file2.path())
        .assert()
        .success()
        .stdout(contains("<div>first</div>"))
        .stdout(contains("<p>second</p>"));
}

#[test]
fn respects_gitignore_by_default() {
    let temp = TempDir::new().unwrap();

    Command::new("git")
        .arg("init")
        .current_dir(temp.path())
        .output()
        .unwrap();

    temp.child(".gitignore")
        .write_str("ignored.html\n")
        .unwrap();
    temp.child("test.html")
        .write_str("<div>  test  </div>")
        .unwrap();
    temp.child("ignored.html")
        .write_str("<div>  ignored  </div>")
        .unwrap();

    Command::new(cargo::cargo_bin!("kirei"))
        .arg("--write")
        .arg("*.html")
        .current_dir(temp.path())
        .assert()
        .success();

    let test_content = read_to_string(temp.child("test.html").path()).unwrap();
    let ignored_content = read_to_string(temp.child("ignored.html").path()).unwrap();

    assert!(
        test_content.contains("<div>test</div>"),
        "test.html should be formatted"
    );
    assert!(
        ignored_content.contains("<div>  ignored  </div>"),
        "ignored.html should NOT be formatted"
    );
}

#[test]
fn no_ignore_flag_formats_ignored_files() {
    let temp = TempDir::new().unwrap();

    Command::new("git")
        .arg("init")
        .current_dir(temp.path())
        .output()
        .unwrap();

    temp.child(".gitignore")
        .write_str("ignored.html\n")
        .unwrap();
    temp.child("test.html")
        .write_str("<div>  test  </div>")
        .unwrap();
    temp.child("ignored.html")
        .write_str("<div>  ignored  </div>")
        .unwrap();

    Command::new(cargo::cargo_bin!("kirei"))
        .arg("--write")
        .arg("--no-ignore")
        .arg("*.html")
        .current_dir(temp.path())
        .assert()
        .success();

    let test_content = read_to_string(temp.child("test.html").path()).unwrap();
    let ignored_content = read_to_string(temp.child("ignored.html").path()).unwrap();

    assert!(
        test_content.contains("<div>test</div>"),
        "test.html should be formatted"
    );
    assert!(
        ignored_content.contains("<div>ignored</div>"),
        "ignored.html SHOULD be formatted with --no-ignore"
    );
}

#[test]
fn formats_directory_recursively() {
    let temp = TempDir::new().unwrap();
    let src_dir = temp.child("src");
    src_dir.create_dir_all().unwrap();

    src_dir
        .child("file1.html")
        .write_str("<div>  test1  </div>")
        .unwrap();
    src_dir
        .child("file2.html")
        .write_str("<div>  test2  </div>")
        .unwrap();
    src_dir
        .child("ignore.txt")
        .write_str("<div>  should not format  </div>")
        .unwrap();

    Command::new(cargo::cargo_bin!("kirei"))
        .arg("--write")
        .arg(src_dir.path())
        .assert()
        .success();

    let file1_content = read_to_string(src_dir.child("file1.html").path()).unwrap();
    let file2_content = read_to_string(src_dir.child("file2.html").path()).unwrap();
    let txt_content = read_to_string(src_dir.child("ignore.txt").path()).unwrap();

    assert!(
        file1_content.contains("<div>test1</div>"),
        "file1.html should be formatted"
    );
    assert!(
        file2_content.contains("<div>test2</div>"),
        "file2.html should be formatted"
    );
    assert!(
        txt_content.contains("<div>  should not format  </div>"),
        ".txt file should NOT be formatted (still has extra spaces)"
    );
}

#[test]
fn glob_pattern_only_matches_current_directory() {
    let temp = TempDir::new().unwrap();
    let subdir = temp.child("subdir");
    subdir.create_dir_all().unwrap();

    temp.child("root.html")
        .write_str("<div>  root  </div>")
        .unwrap();
    subdir
        .child("nested.html")
        .write_str("<div>  nested  </div>")
        .unwrap();

    let output = Command::new(cargo::cargo_bin!("kirei"))
        .arg("*.html")
        .current_dir(temp.path())
        .output()
        .unwrap();

    let stdout = String::from_utf8(output.stdout).unwrap();

    assert!(
        stdout.contains("<div>root</div>"),
        "root.html should be formatted"
    );
    assert!(
        !stdout.contains("nested"),
        "subdir/nested.html should NOT be matched by *.html"
    );
}

#[test]
fn glob_pattern_with_write_only_formats_current_directory() {
    let temp = TempDir::new().unwrap();
    let subdir = temp.child("subdir");
    subdir.create_dir_all().unwrap();

    temp.child("root.html")
        .write_str("<div>  root  </div>")
        .unwrap();
    subdir
        .child("nested.html")
        .write_str("<div>  nested  </div>")
        .unwrap();

    Command::new(cargo::cargo_bin!("kirei"))
        .arg("--write")
        .arg("*.html")
        .current_dir(temp.path())
        .assert()
        .success();

    let root_content = read_to_string(temp.child("root.html").path()).unwrap();
    let nested_content = read_to_string(subdir.child("nested.html").path()).unwrap();

    assert!(
        root_content.contains("<div>root</div>"),
        "root.html should be formatted"
    );
    assert!(
        nested_content.contains("<div>  nested  </div>"),
        "subdir/nested.html should NOT be formatted (still has extra spaces)"
    );
}

#[test]
fn glob_pattern_with_check_only_checks_current_directory() {
    let temp = TempDir::new().unwrap();
    let subdir = temp.child("subdir");
    subdir.create_dir_all().unwrap();

    temp.child("root.html")
        .write_str("<div>  root  </div>")
        .unwrap();
    subdir
        .child("nested.html")
        .write_str("<div>formatted</div>\n")
        .unwrap();

    Command::new(cargo::cargo_bin!("kirei"))
        .arg("--check")
        .arg("*.html")
        .current_dir(temp.path())
        .assert()
        .failure();
}

#[test]
fn glob_pattern_with_list_different_only_lists_current_directory() {
    let temp = TempDir::new().unwrap();
    let subdir = temp.child("subdir");
    subdir.create_dir_all().unwrap();

    temp.child("root.html")
        .write_str("<div>  root  </div>")
        .unwrap();
    temp.child("formatted.html")
        .write_str("<div>formatted</div>\n")
        .unwrap();
    subdir
        .child("nested.html")
        .write_str("<div>  nested  </div>")
        .unwrap();

    let output = Command::new(cargo::cargo_bin!("kirei"))
        .arg("--list-different")
        .arg("*.html")
        .current_dir(temp.path())
        .output()
        .unwrap();

    let stderr = String::from_utf8(output.stderr).unwrap();

    assert!(
        stderr.contains("root.html"),
        "root.html should be listed as needing formatting"
    );
    assert!(
        !stderr.contains("nested.html"),
        "subdir/nested.html should NOT be listed"
    );
    assert!(
        !stderr.contains("formatted.html"),
        "formatted.html should NOT be listed (already formatted)"
    );
}

#[test]
fn recursive_glob_pattern_matches_all_directories() {
    let temp = TempDir::new().unwrap();
    let subdir = temp.child("subdir");
    subdir.create_dir_all().unwrap();

    temp.child("root.html")
        .write_str("<div>  root  </div>")
        .unwrap();
    subdir
        .child("nested.html")
        .write_str("<div>  nested  </div>")
        .unwrap();

    Command::new(cargo::cargo_bin!("kirei"))
        .arg("--write")
        .arg("**/*.html")
        .current_dir(temp.path())
        .assert()
        .success();

    let root_content = read_to_string(temp.child("root.html").path()).unwrap();
    let nested_content = read_to_string(subdir.child("nested.html").path()).unwrap();

    assert!(
        root_content.contains("<div>root</div>"),
        "root.html should be formatted"
    );
    assert!(
        nested_content.contains("<div>nested</div>"),
        "subdir/nested.html SHOULD be formatted with **/*.html"
    );
}

#[test]
fn respects_gitignore_with_literal_paths() {
    let temp = TempDir::new().unwrap();

    Command::new("git")
        .arg("init")
        .current_dir(temp.path())
        .output()
        .unwrap();

    temp.child(".gitignore")
        .write_str("ignored.html\n")
        .unwrap();
    temp.child("test.html")
        .write_str("<div>  test  </div>")
        .unwrap();
    temp.child("ignored.html")
        .write_str("<div>  ignored  </div>")
        .unwrap();

    Command::new(cargo::cargo_bin!("kirei"))
        .arg("--write")
        .arg(temp.child("test.html").path())
        .arg(temp.child("ignored.html").path())
        .assert()
        .success();

    let test_content = read_to_string(temp.child("test.html").path()).unwrap();
    let ignored_content = read_to_string(temp.child("ignored.html").path()).unwrap();

    assert!(
        test_content.contains("<div>test</div>"),
        "test.html should be formatted"
    );
    assert!(
        ignored_content.contains("<div>  ignored  </div>"),
        "ignored.html should NOT be formatted (in .gitignore)"
    );
}

#[test]
fn respects_gitignore_with_single_literal_path() {
    let temp = TempDir::new().unwrap();

    Command::new("git")
        .arg("init")
        .current_dir(temp.path())
        .output()
        .unwrap();

    temp.child(".gitignore")
        .write_str("ignored.html\n")
        .unwrap();
    temp.child("ignored.html")
        .write_str("<div>  ignored  </div>")
        .unwrap();

    Command::new(cargo::cargo_bin!("kirei"))
        .arg(temp.child("ignored.html").path())
        .assert()
        .success()
        .stdout("");

    let content = read_to_string(temp.child("ignored.html").path()).unwrap();
    assert!(
        content.contains("<div>  ignored  </div>"),
        "ignored.html should NOT be formatted (in .gitignore)"
    );
}

#[test]
fn respects_ignore_file() {
    let temp = TempDir::new().unwrap();

    temp.child(".ignore").write_str("ignored.html\n").unwrap();
    temp.child("test.html")
        .write_str("<div>  test  </div>")
        .unwrap();
    temp.child("ignored.html")
        .write_str("<div>  ignored  </div>")
        .unwrap();

    Command::new(cargo::cargo_bin!("kirei"))
        .arg("--write")
        .arg("*.html")
        .current_dir(temp.path())
        .assert()
        .success();

    let test_content = read_to_string(temp.child("test.html").path()).unwrap();
    let ignored_content = read_to_string(temp.child("ignored.html").path()).unwrap();

    assert!(
        test_content.contains("<div>test</div>"),
        "test.html should be formatted"
    );
    assert!(
        ignored_content.contains("<div>  ignored  </div>"),
        "ignored.html should NOT be formatted (in .ignore)"
    );
}

#[test]
fn respects_nested_gitignore() {
    let temp = TempDir::new().unwrap();

    Command::new("git")
        .arg("init")
        .current_dir(temp.path())
        .output()
        .unwrap();

    let subdir = temp.child("subdir");
    subdir.create_dir_all().unwrap();

    subdir
        .child(".gitignore")
        .write_str("nested-ignored.html\n")
        .unwrap();

    temp.child("root.html")
        .write_str("<div>  root  </div>")
        .unwrap();
    subdir
        .child("nested.html")
        .write_str("<div>  nested  </div>")
        .unwrap();
    subdir
        .child("nested-ignored.html")
        .write_str("<div>  nested-ignored  </div>")
        .unwrap();

    Command::new(cargo::cargo_bin!("kirei"))
        .arg("--write")
        .arg("**/*.html")
        .current_dir(temp.path())
        .assert()
        .success();

    let root_content = read_to_string(temp.child("root.html").path()).unwrap();
    let nested_content = read_to_string(subdir.child("nested.html").path()).unwrap();
    let nested_ignored_content =
        read_to_string(subdir.child("nested-ignored.html").path()).unwrap();

    assert!(
        root_content.contains("<div>root</div>"),
        "root.html should be formatted"
    );
    assert!(
        nested_content.contains("<div>nested</div>"),
        "nested.html should be formatted"
    );
    assert!(
        nested_ignored_content.contains("<div>  nested-ignored  </div>"),
        "nested-ignored.html should NOT be formatted (in subdir/.gitignore)"
    );
}

#[test]
fn respects_gitignore_pattern_matching() {
    let temp = TempDir::new().unwrap();

    Command::new("git")
        .arg("init")
        .current_dir(temp.path())
        .output()
        .unwrap();

    temp.child(".gitignore").write_str("*.tmp.html\n").unwrap();
    temp.child("test.html")
        .write_str("<div>  test  </div>")
        .unwrap();
    temp.child("file.tmp.html")
        .write_str("<div>  ignored  </div>")
        .unwrap();

    Command::new(cargo::cargo_bin!("kirei"))
        .arg("--write")
        .arg("*.html")
        .current_dir(temp.path())
        .assert()
        .success();

    let test_content = read_to_string(temp.child("test.html").path()).unwrap();
    let tmp_content = read_to_string(temp.child("file.tmp.html").path()).unwrap();

    assert!(
        test_content.contains("<div>test</div>"),
        "test.html should be formatted"
    );
    assert!(
        tmp_content.contains("<div>  ignored  </div>"),
        "file.tmp.html should NOT be formatted (matches *.tmp.html pattern)"
    );
}
