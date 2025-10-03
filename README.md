# kirei

[![crates][crates]](https://crates.io/crates/kirei)

Askama formatter

> **Note:** kirei is currently experimental and under active development.
> Expect things to change as we carefully prune all these trees.

## Usage

Format and output to stdout:

```sh
kirei index.html
```

Edit files in-place (overwrite with formatted output):

```sh
kirei --write index.html
```

## Neovim Integration

Example using `conform.nvim`:

```lua
require("conform").setup({
  formatters = {
    kirei = {
      command = 'kirei',
      args = {
        '--stdin-filepath',
        '$FILENAME',
        '-',
      },
    },
  },
})
```

[crates]: https://img.shields.io/crates/v/kirei?logo=rust
