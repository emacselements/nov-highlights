
# nov-highlights: Highlights and Annotations for Nov Mode

`nov-highlights` is an Emacs package that adds powerful highlighting and annotation features to Nov Mode (the EPUB reader for Emacs). With this package, you can:

- Highlight text in green, orange, purple, blue underline, or red strikeout
- Add annotations to highlighted text (creates a yellow highlight with a note)
- Export all highlights and annotations to Org mode or Markdown files
- Persistently store highlights and annotations across sessions
- **Highlights persist even if you move or rename your epub files!**

This makes it easy to mark up, annotate, and review your eBooks directly within Emacs.

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
nov-highlights-underline	            `u`
nov-highlights-strikeout	            `s`
nov-highlights-annotate	                `n`
nov-highlights-view-annotation	        `v`
nov-highlights-remove-at-point	        `r`
nov-highlights-remove-in-region	        `D`  
nov-highlights-export-to-org	        `e`  
nov-highlights-export-to-markdown	    `m`  
nov-highlights-list	                    `M-l`
nov-highlights-next-annotation	        `M-n`
nov-highlights-previous-annotation	    `M-p`
nov-highlights-close-annotation-windows	`<ESC>`

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

nov-zoom-in	    `C-+` or `C-=`  
nov-zoom-out	`C--`  
nov-zoom-reset	`C-0`  



