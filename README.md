
# nov-highlights

Powerful highlighting and annotation features for Nov Mode (Emacs EPUB reader).

## Features

- **Highlight text** in multiple colors (green, orange, purple, pink) plus blue underline and red strikeout
- **Add annotations** with yellow highlights and notes (Markdown editing by default)
- **Create bookmarks** for quick navigation between important locations
- **Export** all highlights and annotations to Org mode or Markdown files
- **Persistent storage** — highlights and bookmarks survive file moves/renames (metadata-based identification)
- **Navigate annotations** with keyboard shortcuts and automatic editor opening
- **Mouse hover tooltips** to preview annotations
- **Zoom controls** for comfortable reading

## Installation

### From MELPA

```elisp
;; M-x package-install RET nov-highlights RET
(with-eval-after-load 'nov
  (nov-highlights-global-mode-enable))
```

### Manual

Download [nov-highlights.el](nov-highlights.el), place in your `load-path`, then:

```elisp
(require 'nov-highlights)
(with-eval-after-load 'nov
  (nov-highlights-global-mode-enable))
```

Storage: `~/.emacs.d/nov-highlights.el` (highlights), `~/.emacs.d/nov-bookmarks/` (bookmarks)

## Video Demo

[Watch overview](https://youtu.be/HSxXWzGnMVI)

## Usage

### Navigation (nov-mode)
- `]` / `[` — next/previous chapter
- `t` — table of contents
- `l` / `r` — back/forward in history
- `TAB` / `S-TAB` — next/previous link
- `SPC` / `S-SPC` — scroll (auto-advances chapters)
- `g` — refresh display

### Highlighting
- `y` — green highlight
- `h` — orange highlight
- `,` — purple highlight
- `j` — pink highlight
- `u` — blue underline
- `s` — red strikeout
- `n` — add annotation (yellow highlight)
- `v` — view annotation
- `r` — remove highlight at point
- `D` — remove highlights in region
- `M-l` — list all highlights
- `M-n` / `M-p` — next/previous annotation
- `ESC` — close annotation windows

### Bookmarks
- `' b` — create bookmark
- `' g` — access bookmark
- `' l` — go back
- `' d` — delete bookmark
- `' r` — rename bookmark

### Export
- `C-c C-e` — export to Org mode
- `C-c C-m` — export to Markdown

### Zoom
- `C-+` / `C-=` — zoom in
- `C--` — zoom out
- `C-0` — reset zoom
- Press `g` after zooming for proper text wrapping

## Configuration

### Better tooltips (optional)
```elisp
(setq x-gtk-use-system-tooltips nil)
```

### Sentence navigation for EPUBs (recommended)
```elisp
(setq sentence-end "\\([.!?,;:""''][]\"')}]*\\|[:][[:space:]]\\)[[:space:]]*")
```
Improves text selection with `M-e` / `M-a` commands.

### Annotation mode
```elisp
;; Default is markdown-mode
(setq nov-highlights-annotation-mode 'markdown-mode)  ; or 'org-mode or 'text-mode
```

### Default font size
```elisp
(add-hook 'nov-mode-hook
          (lambda ()
            (face-remap-add-relative 'variable-pitch :height 1.3)))
```

## Migration (for existing users)

If upgrading from older versions:
- **Start fresh:** `rm ~/.emacs.d/nov-highlights.el`
- **Migrate per-book:** Open EPUB, run `M-x nov-highlights-migrate-current-book`

## Support

[PayPal donation](https://www.paypal.com/paypalme/revrari)
