use assert_cmd::{Command, cargo};
use assert_fs::{TempDir, prelude::*};
use std::fs::read_to_string;
use std::path::Path;

const IGNORED_FILE: &str = "ignored.html";
const NON_IGNORED_FILE: &str = "non-ignored.html";
const REQUIRES_FORMATTING_SNIPPET: &str = "<div>  some   text  </div>";
const FORMATTED_SNIPPET: &str = "<div>some text</div>";

#[test]
fn handles_multiple_file_arguments_from_shell_glob() {
    let temp = TempDir::new().unwrap();
    temp.child("test1.html")
        .write_str("<div>  test1  </div>")
        .unwrap();
    temp.child("test2.html")
        .write_str("<div>  test2  </div>")
        .unwrap();
    temp.child("ignore.txt").write_str("text").unwrap();

    let mut cmd = Command::new(cargo::cargo_bin!("kirei"));
    cmd.arg("--write");
    cmd.arg(temp.child("test1.html").path());
    cmd.arg(temp.child("test2.html").path());
    cmd.assert().success();

    let content1 = read_to_string(temp.child("test1.html").path()).unwrap();
    let content2 = read_to_string(temp.child("test2.html").path()).unwrap();

    assert_eq!(content1, "<div>test1</div>\n");
    assert_eq!(content2, "<div>test2</div>\n");
}

#[test]
fn processes_shell_expanded_files_with_check_flag() {
    let temp = TempDir::new().unwrap();
    temp.child("needs_format.html")
        .write_str("<div>  needs format  </div>")
        .unwrap();
    temp.child("already_formatted.html")
        .write_str("<div>already formatted</div>\n")
        .unwrap();

    let output = Command::new(cargo::cargo_bin!("kirei"))
        .arg("--check")
        .arg(temp.child("needs_format.html").path())
        .arg(temp.child("already_formatted.html").path())
        .output()
        .unwrap();

    assert!(!output.status.success());
}

#[test]
fn mixed_directories_and_files_from_shell_expansion() {
    let temp = TempDir::new().unwrap();
    let subdir = temp.child("src");
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
        .arg(temp.child("root.html").path())
        .arg(subdir.path())
        .assert()
        .success();

    let root_content = read_to_string(temp.child("root.html").path()).unwrap();
    let nested_content = read_to_string(subdir.child("nested.html").path()).unwrap();

    assert_eq!(root_content, "<div>root</div>\n");
    assert_eq!(nested_content, "<div>nested</div>\n");
}

#[test]
fn handles_many_file_arguments() {
    let temp = TempDir::new().unwrap();

    for i in 1..=10 {
        temp.child(format!("file{}.html", i))
            .write_str(&format!("<div>  file{}  </div>", i))
            .unwrap();
    }

    let mut cmd = Command::new(cargo::cargo_bin!("kirei"));
    cmd.arg("--write");
    for i in 1..=10 {
        cmd.arg(temp.child(format!("file{}.html", i)).path());
    }
    cmd.assert().success();

    for i in 1..=10 {
        let content = read_to_string(temp.child(format!("file{}.html", i)).path()).unwrap();
        assert_eq!(content, format!("<div>file{}</div>\n", i));
    }
}

#[test]
fn directory_argument_formats_recursively() {
    let temp = TempDir::new().unwrap();
    let subdir = temp.child("nested");
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
        .arg(temp.path())
        .assert()
        .success();

    let root_content = read_to_string(temp.child("root.html").path()).unwrap();
    let nested_content = read_to_string(subdir.child("nested.html").path()).unwrap();

    assert_eq!(root_content, "<div>root</div>\n");
    assert_eq!(nested_content, "<div>nested</div>\n");
}

#[test]
fn respects_gitignore_by_default() {
    let temp = TempDir::new().unwrap();

    Command::new("git")
        .arg("init")
        .current_dir(temp.path())
        .output()
        .unwrap();

    temp.child(".gitignore").write_str(IGNORED_FILE).unwrap();
    temp.child(NON_IGNORED_FILE)
        .write_str(REQUIRES_FORMATTING_SNIPPET)
        .unwrap();
    temp.child(IGNORED_FILE)
        .write_str(REQUIRES_FORMATTING_SNIPPET)
        .unwrap();

    Command::new(cargo::cargo_bin!("kirei"))
        .arg("--write")
        .arg(".")
        .current_dir(temp.path())
        .assert()
        .success();

    let non_ignored_content = read_to_string(temp.child(NON_IGNORED_FILE).path()).unwrap();
    let ignored_content = read_to_string(temp.child(IGNORED_FILE).path()).unwrap();

    assert!(non_ignored_content.contains(FORMATTED_SNIPPET));
    assert!(ignored_content.contains(REQUIRES_FORMATTING_SNIPPET));
}

#[test]
fn no_ignore_flag_formats_ignored_files() {
    let temp = TempDir::new().unwrap();

    Command::new("git")
        .arg("init")
        .current_dir(temp.path())
        .output()
        .unwrap();

    temp.child(".gitignore").write_str(IGNORED_FILE).unwrap();
    temp.child(NON_IGNORED_FILE)
        .write_str(REQUIRES_FORMATTING_SNIPPET)
        .unwrap();
    temp.child(IGNORED_FILE)
        .write_str(REQUIRES_FORMATTING_SNIPPET)
        .unwrap();

    Command::new(cargo::cargo_bin!("kirei"))
        .arg("--write")
        .arg("--no-ignore")
        .arg(".")
        .current_dir(temp.path())
        .assert()
        .success();

    let non_ignored_content = read_to_string(temp.child(NON_IGNORED_FILE).path()).unwrap();
    let ignored_content = read_to_string(temp.child(IGNORED_FILE).path()).unwrap();

    assert!(non_ignored_content.contains(FORMATTED_SNIPPET));
    assert!(ignored_content.contains(FORMATTED_SNIPPET));
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

    assert!(file1_content.contains("<div>test1</div>"));
    assert!(file2_content.contains("<div>test2</div>"));
    assert!(txt_content.contains("<div>  should not format  </div>"));
}

#[test]
fn formats_non_ignored_file_with_relative_path_in_cwd() {
    let temp = TempDir::new().unwrap();

    Command::new("git")
        .arg("init")
        .current_dir(temp.path())
        .output()
        .unwrap();

    temp.child(".gitignore").write_str("other.html\n").unwrap();
    temp.child("test.html")
        .write_str("<div>  test  </div>")
        .unwrap();

    Command::new(cargo::cargo_bin!("kirei"))
        .arg("--write")
        .arg("test.html")
        .current_dir(temp.path())
        .assert()
        .success();

    let content = read_to_string(temp.child("test.html").path()).unwrap();
    assert!(content.contains("<div>test</div>"));
}

#[test]
fn formats_single_file_with_absolute_path() {
    let temp = TempDir::new().unwrap();
    temp.child("test.html")
        .write_str("<div>  test  </div>")
        .unwrap();

    Command::new(cargo::cargo_bin!("kirei"))
        .arg("--write")
        .arg(temp.child("test.html").path())
        .assert()
        .success();

    let content = read_to_string(temp.child("test.html").path()).unwrap();
    assert!(content.contains("<div>test</div>"));
}

#[test]
fn formats_multiple_files_with_absolute_paths() {
    let temp = TempDir::new().unwrap();
    temp.child("file1.html")
        .write_str("<div>  file1  </div>")
        .unwrap();
    temp.child("file2.html")
        .write_str("<div>  file2  </div>")
        .unwrap();

    Command::new(cargo::cargo_bin!("kirei"))
        .arg("--write")
        .arg(temp.child("file1.html").path())
        .arg(temp.child("file2.html").path())
        .assert()
        .success();

    let file1_content = read_to_string(temp.child("file1.html").path()).unwrap();
    let file2_content = read_to_string(temp.child("file2.html").path()).unwrap();

    assert!(file1_content.contains("<div>file1</div>"));
    assert!(file2_content.contains("<div>file2</div>"));
}

#[test]
fn formats_nested_directory_recursively() {
    let temp = TempDir::new().unwrap();
    let src_dir = temp.child("src");
    let nested_dir = src_dir.child("components");
    nested_dir.create_dir_all().unwrap();

    src_dir
        .child("root.html")
        .write_str("<div>  root  </div>")
        .unwrap();
    nested_dir
        .child("nested.html")
        .write_str("<div>  nested  </div>")
        .unwrap();

    Command::new(cargo::cargo_bin!("kirei"))
        .arg("--write")
        .arg(src_dir.path())
        .assert()
        .success();

    let root_content = read_to_string(src_dir.child("root.html").path()).unwrap();
    let nested_content = read_to_string(nested_dir.child("nested.html").path()).unwrap();

    assert!(root_content.contains("<div>root</div>"));
    assert!(nested_content.contains("<div>nested</div>"));
}

#[test]
fn formats_relative_path_to_file() {
    let temp = TempDir::new().unwrap();
    let subdir = temp.child("subdir");
    subdir.create_dir_all().unwrap();
    subdir
        .child("test.html")
        .write_str("<div>  test  </div>")
        .unwrap();

    Command::new(cargo::cargo_bin!("kirei"))
        .arg("--write")
        .arg("subdir/test.html")
        .current_dir(temp.path())
        .assert()
        .success();

    let content = read_to_string(subdir.child("test.html").path()).unwrap();
    assert!(content.contains("<div>test</div>"));
}

#[test]
fn formats_relative_path_to_directory() {
    let temp = TempDir::new().unwrap();
    let subdir = temp.child("subdir");
    subdir.create_dir_all().unwrap();
    subdir
        .child("test.html")
        .write_str("<div>  test  </div>")
        .unwrap();

    Command::new(cargo::cargo_bin!("kirei"))
        .arg("--write")
        .arg("subdir")
        .current_dir(temp.path())
        .assert()
        .success();

    let content = read_to_string(subdir.child("test.html").path()).unwrap();
    assert!(content.contains("<div>test</div>"));
}

#[test]
fn formats_current_directory_with_dot() {
    let temp = TempDir::new().unwrap();
    temp.child("test.html")
        .write_str("<div>  test  </div>")
        .unwrap();

    Command::new(cargo::cargo_bin!("kirei"))
        .arg("--write")
        .arg(".")
        .current_dir(temp.path())
        .assert()
        .success();

    let content = read_to_string(temp.child("test.html").path()).unwrap();
    assert!(content.contains("<div>test</div>"));
}

#[test]
fn formats_deeply_nested_directories() {
    let temp = TempDir::new().unwrap();

    let level1 = temp.child("level1");
    let level2 = level1.child("level2");
    let level3 = level2.child("level3");
    level3.create_dir_all().unwrap();

    temp.child("root.html")
        .write_str("<div>  root  </div>")
        .unwrap();
    level1
        .child("l1.html")
        .write_str("<div>  level1  </div>")
        .unwrap();
    level2
        .child("l2.html")
        .write_str("<div>  level2  </div>")
        .unwrap();
    level3
        .child("l3.html")
        .write_str("<div>  level3  </div>")
        .unwrap();

    level2
        .child("readme.txt")
        .write_str("should not be formatted")
        .unwrap();

    Command::new(cargo::cargo_bin!("kirei"))
        .arg("--write")
        .arg(temp.path())
        .assert()
        .success();

    let root_content = read_to_string(temp.child("root.html").path()).unwrap();
    let l1_content = read_to_string(level1.child("l1.html").path()).unwrap();
    let l2_content = read_to_string(level2.child("l2.html").path()).unwrap();
    let l3_content = read_to_string(level3.child("l3.html").path()).unwrap();
    let txt_content = read_to_string(level2.child("readme.txt").path()).unwrap();

    assert_eq!(root_content, "<div>root</div>\n");
    assert_eq!(l1_content, "<div>level1</div>\n");
    assert_eq!(l2_content, "<div>level2</div>\n");
    assert_eq!(l3_content, "<div>level3</div>\n");
    assert_eq!(txt_content, "should not be formatted");
}

#[test]
fn formats_multiple_directories_with_nested_files() {
    let temp = TempDir::new().unwrap();

    let src = temp.child("src");
    let templates = temp.child("templates");
    src.child("components").create_dir_all().unwrap();
    templates.child("emails").create_dir_all().unwrap();

    src.child("index.html")
        .write_str("<div>  src index  </div>")
        .unwrap();
    src.child("components")
        .child("button.html")
        .write_str("<button>  click  </button>")
        .unwrap();
    templates
        .child("base.html")
        .write_str("<html>  base  </html>")
        .unwrap();
    templates
        .child("emails")
        .child("welcome.html")
        .write_str("<p>  welcome  </p>")
        .unwrap();

    Command::new(cargo::cargo_bin!("kirei"))
        .arg("--write")
        .arg(src.path())
        .arg(templates.path())
        .assert()
        .success();

    let index = read_to_string(src.child("index.html").path()).unwrap();
    let button = read_to_string(src.child("components").child("button.html").path()).unwrap();
    let base = read_to_string(templates.child("base.html").path()).unwrap();
    let welcome = read_to_string(templates.child("emails").child("welcome.html").path()).unwrap();

    assert_eq!(index, "<div>src index</div>\n");
    assert_eq!(button, "<button> click </button>\n");
    assert_eq!(base, "<html>base</html>\n");
    assert_eq!(welcome, "<p>welcome</p>\n");
}

#[test]
fn continues_walk_despite_errors_and_warnings() {
    let temp = TempDir::new().unwrap();

    let level1 = temp.child("level1");
    let level2 = level1.child("level2");
    level2.create_dir_all().unwrap();

    temp.child("valid.html")
        .write_str("<div>  valid  </div>")
        .unwrap();

    level1
        .child("syntax_error.html")
        .write_str("{% blorp %}<div>invalid syntax</div>")
        .unwrap();

    level1
        .child("another_valid.html")
        .write_str("<p>  another  </p>")
        .unwrap();

    level2
        .child("warning.html")
        .write_str("{% if x %}<div>{% else %}</div>{% endif %}")
        .unwrap();

    level2
        .child("final_valid.html")
        .write_str("<div>  final  </div>")
        .unwrap();

    let output = Command::new(cargo::cargo_bin!("kirei"))
        .arg("--write")
        .arg(temp.path())
        .output()
        .unwrap();

    assert!(!output.status.success());

    let stderr = String::from_utf8(output.stderr).unwrap();
    assert!(stderr.contains("syntax error"));
    assert!(stderr.contains("unbalanced HTML"));

    let valid = read_to_string(temp.child("valid.html").path()).unwrap();
    let another = read_to_string(level1.child("another_valid.html").path()).unwrap();
    let final_valid = read_to_string(level2.child("final_valid.html").path()).unwrap();

    assert_eq!(
        valid, "<div>valid</div>\n",
        "valid files should still be formatted"
    );
    assert_eq!(
        another, "<p>another</p>\n",
        "files after errors should still be formatted"
    );
    assert_eq!(
        final_valid, "<div>final</div>\n",
        "files in subdirs should still be formatted"
    );
}

#[test]
fn relative_equals_expanded() {
    let tests_dir = Path::new(env!("CARGO_MANIFEST_DIR")).join("tests");
    let fixtures_dir = tests_dir.join("fixtures");

    let output_dir = Command::new(cargo::cargo_bin!("kirei"))
        .arg("--list-different")
        .arg("fixtures")
        .current_dir(&tests_dir)
        .output()
        .unwrap();

    let stderr_dir = String::from_utf8(output_dir.stderr).unwrap();
    let count_dir = stderr_dir.lines().count();

    let expanded: Vec<String> = std::fs::read_dir(&fixtures_dir)
        .unwrap()
        .filter_map(|e| {
            let p = e.ok()?.path();
            Some(p.to_string_lossy().to_string())
        })
        .collect();

    let mut cmd = Command::new(cargo::cargo_bin!("kirei"));
    cmd.arg("--list-different").current_dir(&tests_dir);

    for path in expanded {
        cmd.arg(path);
    }

    let output_expanded = cmd.output().unwrap();
    let stderr_expanded = String::from_utf8(output_expanded.stderr).unwrap();
    let count_expanded = stderr_expanded.lines().count();

    assert_eq!(count_dir, count_expanded);
    assert!(count_dir > 0);
}
