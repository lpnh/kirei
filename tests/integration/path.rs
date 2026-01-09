use assert_cmd::{Command, cargo};
use assert_fs::{TempDir, prelude::*};
use std::fs::read_to_string;

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

#[test]
fn respects_gitignore_with_relative_path_in_cwd() {
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
        .arg("ignored.html")
        .current_dir(temp.path())
        .assert()
        .success()
        .stdout("");

    let content = read_to_string(temp.child("ignored.html").path()).unwrap();
    assert!(
        content.contains("<div>  ignored  </div>"),
        "ignored.html should NOT be formatted (in .gitignore, relative path in cwd)"
    );
}

#[test]
fn formats_non_ignored_file_with_relative_path_in_cwd() {
    let temp = TempDir::new().unwrap();

    Command::new("git")
        .arg("init")
        .current_dir(temp.path())
        .output()
        .unwrap();

    temp.child(".gitignore")
        .write_str("other.html\n")
        .unwrap();
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
    assert!(
        content.contains("<div>test</div>"),
        "test.html should be formatted (NOT in .gitignore, relative path in cwd)"
    );
}
