
# nov-highlights: Highlights and Annotations for Nov Mode

`nov-highlights` is an Emacs package that adds powerful highlighting and annotation features to Nov Mode (the EPUB reader for Emacs). With this package, you can:

- Highlight text in green, orange, purple, pink, blue underline, or red strikeout
- Add annotations to highlighted text (creates a yellow highlight with a note)
- Export all highlights and annotations to Org mode or Markdown files
- Persistently store highlights and annotations across sessions
- **Highlights persist even if you move or rename your epub files!**

This makes it easy to mark up, annotate, and review your eBooks directly within Emacs.

## Installation

### From MELPA

Once nov-highlights is available on MELPA:

```elisp
;; Install via M-x package-install RET nov-highlights RET

;; Then add to your init file (~/.emacs or ~/.emacs.d/init.el):
(with-eval-after-load 'nov
  (nov-highlights-global-mode-enable))
```

### Manual Installation

1. Download [nov-highlights.el](nov-highlights.el)
2. Place it in your `load-path`
3. Add to your init file:

```elisp
(require 'nov-highlights)
(with-eval-after-load 'nov
  (nov-highlights-global-mode-enable))
```

## Key Features

**Stable Highlight Storage**: Highlights are tied to book metadata (title and author), not file paths. This means your annotations and highlights will persist even if you:
- Move epub files to different directories
- Rename epub files
- Sync books across different machines

The highlights are stored in `~/.emacs.d/nov-highlights.el` by default.

---

## Support & Donations

If you find this project helpful, consider supporting it!

[Donate via PayPal](https://www.paypal.com/paypalme/revrari)

## 📺 Video Overview

[Watch a video about this package](https://youtu.be/HSxXWzGnMVI)

# Main Commands

# nov-mode Navigation Cheat-Sheet

## Chapter Navigation
- `]` — next document or chapter  
- `[` — previous document or chapter  
- `t` — jump to Table of Contents  
- `l` — go back in history  
- `r` — go forward in history  

## Links & Within-Page
- `TAB` — jump to next link  
- `S-TAB` — jump to previous link  
- `RET` — follow link under cursor  

## Scrolling
- `SPC` — scroll down; at end of chapter jumps to next chapter  
- `S-SPC` / `DEL` — scroll up; at start of chapter jumps to previous chapter  
- `<home>` — jump to beginning of buffer  
- `<end>` — jump to end of buffer  

## Refresh
- `g` — redisplay the current document  

## Highlighting Commands

nov-highlights-green        	        `g`
nov-highlights-orange	                `h`
nov-highlights-purple	                `,`
nov-highlights-pink	                    `j`
nov-highlights-underline	            `u`
nov-highlights-strikeout	            `s`
nov-highlights-annotate	                `n`
nov-highlights-view-annotation	        `v`
nov-highlights-remove-at-point	        `r`
nov-highlights-remove-in-region	        `D`
nov-highlights-list	                    `M-l`
nov-highlights-next-annotation	        `M-n`
nov-highlights-previous-annotation	    `M-p`
nov-highlights-close-annotation-windows	`<ESC>`

**Export Functions** (available via M-x):
- `M-x nov-highlights-export-to-org` — Export highlights to Org mode file
- `M-x nov-highlights-export-to-markdown` — Export highlights to Markdown file

## Bookmark Commands

Quickly mark and navigate between important locations in your EPUB books.

nov-highlights-bookmarks-create         `C-b c`
nov-highlights-bookmarks-access         `C-b b`
nov-highlights-bookmarks-delete         `C-b d`
nov-highlights-bookmarks-rename         `C-b r`

**Features:**
- Bookmarks persist across sessions
- Survive file moves and renames (metadata-based identification)
- Smart defaulting for quick navigation between bookmarks
- Sorted by chapter and position order
- Quick toggle between current bookmark and last accessed bookmark

**Storage:** `~/.emacs.d/nov-bookmarks/`

## Configuration

### Recommended Nov Mode Settings

For better text navigation and selection in Nov mode (especially for highlighting), add this to your `~/.emacs` or `~/.emacs.d/init.el`:

```elisp
;; Improved sentence navigation for better text selection in Nov mode
;; This helps Emacs better recognize sentence boundaries in EPUBs by
;; handling various punctuation marks and quotation styles correctly
(setq sentence-end "\\([.!?,;:“”‘’][]\"')}]*\\|[:][[:space:]]\\)[[:space:]]*")
```

**What this does**: This setting configures Emacs to properly recognize sentence boundaries when using commands like `M-e` (forward-sentence) and `M-a` (backward-sentence). This is particularly useful in EPUBs which often contain:
- Various quotation marks (straight quotes, curly quotes, etc.)
- Different punctuation styles
- Inline citations and parenthetical notes

With this configuration, selecting text for highlighting becomes much more intuitive, as Emacs will correctly jump between sentences instead of stopping at unexpected punctuation.

### Annotation Mode

By default, annotations are edited and viewed in **Markdown mode**. You can customize this to use Org mode or plain text.

Add to `~/.emacs` or `~/.emacs.d/init.el`:

```elisp
;; Markdown (default - no config needed)
(setq nov-highlights-annotation-mode 'markdown-mode)

;; Or switch to Org mode
(setq nov-highlights-annotation-mode 'org-mode)

;; Or plain text
(setq nov-highlights-annotation-mode 'text-mode)
```

**Note**: This only affects the editing experience - annotations are stored as plain text, so existing annotations will work regardless of which mode you choose.

## Migration for Existing Users

If you were using an older version of nov-highlights and have existing highlights, you have two options:

### Option 1: Start Fresh (Recommended if you have few highlights)
Simply delete the old database and start over:
```bash
rm ~/.emacs.d/nov-highlights.el
```
New highlights will automatically use the metadata-based storage system.

### Option 2: Migrate Per-Book
If you have many highlights to preserve:

1. Open an epub file that has highlights
2. Run: `M-x nov-highlights-migrate-current-book`

This will find highlights stored under the old filename and migrate them to use the book's metadata (title + author). Repeat for each book with highlights you want to keep.

**Note**: After migration, your highlights will be tied to the book's metadata instead of file paths, so they'll persist across file moves and renames.

## Zoom commands (in Nov mode):

nov-highlights-zoom-in      `C-+` or `C-=`
nov-highlights-zoom-out     `C--`
nov-highlights-zoom-reset   `C-0`  



