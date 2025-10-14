# Implementation Guide: Adding Bookmarks to nov-highlights

**Author: Raoul Comninos**

## Quick Overview

This guide will help you add the PDF bookmarks feature to nov-highlights. The implementation is ~90% identical to the PDF version, with key differences in:
1. Position tracking (chapter + point vs. page number)
2. Book identification (already solved via metadata)
3. Navigation (async chapter rendering vs. direct page jump)

---

## Step 1: Review the Technical Spec

Read [`BOOKMARK_FEATURE_SPEC.md`](BOOKMARK_FEATURE_SPEC.md) first to understand the architecture.

---

## Step 2: Copy the Core Code Structure

The easiest approach is to copy the entire bookmark section from `pdf-tools-settings-fixes.el` and adapt it.

**What to copy:** Lines 334-625 (entire section 9)

**Where to paste:** End of `nov-highlights.el`, before the final `provide` statement

---

## Step 3: Rename All Functions and Variables

Use find-replace (case-sensitive):

| Find | Replace |
|------|---------|
| `pdf-bookmarks` | `nov-bookmarks` |
| `pdf-view-mode` | `nov-mode` |
| `pdf-view-current-page` | (custom function - see below) |
| `pdf-view-goto-page` | (custom function - see below) |

---

## Step 4: Update Storage Directory

**Change:**
```elisp
(defvar pdf-bookmarks-storage-directory
  (expand-file-name "pdf-bookmarks/" user-emacs-directory)
  "Directory to store PDF bookmark files.")
```

**To:**
```elisp
(defvar nov-bookmarks-storage-directory
  (expand-file-name "nov-bookmarks/" user-emacs-directory)
  "Directory to store EPUB bookmark files.")
```

---

## Step 5: Replace Hash Function with Book ID

**Delete:**
```elisp
(defun pdf-bookmarks-get-pdf-hash ()
  "Calculate a hash of the PDF file content for stable bookmark identification.
Uses the first 10MB of the file to avoid reading huge files entirely."
  (when-let ((pdf-file (buffer-file-name)))
    (when (file-exists-p pdf-file)
      (with-temp-buffer
        (set-buffer-multibyte nil)
        ;; Read first 10MB or entire file if smaller
        (insert-file-contents-literally pdf-file nil 0 (* 10 1024 1024))
        (secure-hash 'sha256 (current-buffer))))))
```

**Replace with:**
```elisp
(defun nov-bookmarks-get-book-hash ()
  "Get content-based hash for the current EPUB book.
Uses existing nov-highlights book ID system based on metadata."
  (when-let ((book-id (nov-highlights--current-book-id)))
    ;; Hash the book ID to create filesystem-safe filename
    (secure-hash 'sha256 book-id)))
```

**Why?** nov-highlights already has robust book identification via metadata. Reuse it!

---

## Step 6: Update File Path Function

**Change:**
```elisp
(defun pdf-bookmarks-get-file-path ()
  "Get the bookmark file path for the current PDF.
Uses content-based hash for robust identification across moves/renames.
Falls back to filename-based lookup for backward compatibility with existing bookmarks."
  (when-let ((pdf-file (buffer-file-name)))
    (pdf-bookmarks-ensure-directory)
    (let* ((hash (pdf-bookmarks-get-pdf-hash))
           (hash-based-path (when hash
                             (expand-file-name
                              (concat hash ".bookmarks")
                              pdf-bookmarks-storage-directory)))
           (filename-based-path (expand-file-name
                                (concat (file-name-base pdf-file) ".bookmarks")
                                pdf-bookmarks-storage-directory)))
      ;; Return hash-based path if available, otherwise filename-based
      (or hash-based-path filename-based-path))))
```

**To:**
```elisp
(defun nov-bookmarks-get-file-path ()
  "Get the bookmark file path for the current EPUB.
Uses book metadata hash for robust identification across moves/renames."
  (when-let ((book-hash (nov-bookmarks-get-book-hash)))
    (nov-bookmarks-ensure-directory)
    (expand-file-name
     (concat book-hash ".bookmarks")
     nov-bookmarks-storage-directory)))
```

**Simpler!** No migration needed since nov-highlights doesn't have legacy bookmarks.

---

## Step 7: Update Load Function (Simplified)

**Change:**
```elisp
(defun pdf-bookmarks-load ()
  "Load bookmarks for the current PDF file.
Automatically migrates old filename-based bookmarks to hash-based system."
  (when-let ((pdf-file (buffer-file-name)))
    (pdf-bookmarks-ensure-directory)
    (let* ((hash (pdf-bookmarks-get-pdf-hash))
           (hash-based-path ...)
           (filename-based-path ...)
           (bookmarks nil))
      ;; ... migration logic ...
      (setq pdf-bookmarks-current-file-bookmarks bookmarks))))
```

**To:**
```elisp
(defun nov-bookmarks-load ()
  "Load bookmarks for the current EPUB file."
  (when-let ((bookmark-file (nov-bookmarks-get-file-path)))
    (setq nov-bookmarks-current-file-bookmarks
          (if (file-exists-p bookmark-file)
              (condition-case nil
                  (with-temp-buffer
                    (insert-file-contents bookmark-file)
                    (read (current-buffer)))
                (error nil))
            nil))))
```

**Simpler!** No migration needed for new feature.

---

## Step 8: Update Bookmark Data Structure

**Change bookmark creation from:**
```elisp
(bookmark-entry (list :name bookmark-name
                     :page current-page
                     :timestamp (current-time)))
```

**To:**
```elisp
(bookmark-entry (list :name bookmark-name
                     :chapter current-chapter
                     :position current-position
                     :timestamp (current-time)))
```

---

## Step 9: Create Position Tracking Functions

**Add these new functions:**

```elisp
(defun nov-bookmarks-current-position ()
  "Get current chapter and position in EPUB."
  (list :chapter (nov-highlights--get-chapter-id)
        :position (point)))

(defun nov-bookmarks-goto-position (chapter position)
  "Navigate to CHAPTER and POSITION in current EPUB."
  (when (and chapter position)
    ;; First navigate to chapter
    (nov-goto-document chapter)
    ;; Wait for rendering, then jump to position
    (run-with-timer 0.2 nil
      (lambda ()
        (goto-char position)
        (recenter)))))
```

**Important:** EPUB rendering is async, hence the timer!

---

## Step 10: Update Create Function

**Find:** `(let ((current-page (pdf-view-current-page)))`

**Replace with:**
```elisp
(let* ((pos (nov-bookmarks-current-position))
       (current-chapter (plist-get pos :chapter))
       (current-position (plist-get pos :position)))
  (unless (and current-chapter current-position)
    (error "Could not get current position"))

  (let* ((bookmark-name (read-string "Bookmark name: "
                                     (format "Chapter %d" current-chapter)))
         (bookmark-entry (list :name bookmark-name
                              :chapter current-chapter
                              :position current-position
                              :timestamp (current-time))))
    ;; ... rest of function same ...
```

**Also update:** Sort key from page to chapter:
```elisp
;; Change:
(lambda (a b) (< (plist-get a :page) (plist-get b :page)))

;; To:
(lambda (a b)
  (let ((a-ch (plist-get a :chapter))
        (b-ch (plist-get b :chapter)))
    (if (= a-ch b-ch)
        (< (plist-get a :position) (plist-get b :position))
      (< a-ch b-ch))))
```

---

## Step 11: Update Access Function

**Find:** `(pdf-view-goto-page page)`

**Replace with:**
```elisp
(nov-bookmarks-goto-position
 (plist-get selected-bookmark :chapter)
 (plist-get selected-bookmark :position))
```

**Find:** `(let ((current-page (pdf-view-current-page)))`

**Replace with:**
```elisp
(let* ((pos (nov-bookmarks-current-position))
       (current-chapter (plist-get pos :chapter))
       (current-position (plist-get pos :position)))
```

---

## Step 12: Update Format Function

**Change:**
```elisp
(defun pdf-bookmarks-format-choice (bookmark)
  "Format a BOOKMARK as a completion choice string."
  (format "%s (Page %d)"
          (plist-get bookmark :name)
          (plist-get bookmark :page)))
```

**To:**
```elisp
(defun nov-bookmarks-format-choice (bookmark)
  "Format a BOOKMARK as a completion choice string."
  (format "%s (Chapter %d)"
          (plist-get bookmark :name)
          (plist-get bookmark :chapter)))
```

---

## Step 13: Update Find Function

**Change:**
```elisp
(defun pdf-bookmarks-find-at-page (page)
  "Find the bookmark at PAGE, returning the bookmark entry or nil."
  (cl-find-if (lambda (bm)
                (= (plist-get bm :page) page))
              pdf-bookmarks-current-file-bookmarks))
```

**To:**
```elisp
(defun nov-bookmarks-find-at-position (chapter position)
  "Find the bookmark at CHAPTER and POSITION, returning the bookmark entry or nil.
Matches if bookmark is within 100 characters of current position in same chapter."
  (cl-find-if (lambda (bm)
                (and (= (plist-get bm :chapter) chapter)
                     (< (abs (- (plist-get bm :position) position)) 100)))
              nov-bookmarks-current-file-bookmarks))
```

**Why fuzzy match?** EPUB rendering can shift positions slightly. 100-char tolerance is reasonable.

---

## Step 14: Add Keybindings

**Add to nov-highlights.el, after mode definition:**

```elisp
(with-eval-after-load 'nov
  (define-key nov-mode-map (kbd "b") #'nov-bookmarks-create)
  (define-key nov-mode-map (kbd "B") #'nov-bookmarks-access)
  (define-key nov-mode-map (kbd "M-d") #'nov-bookmarks-delete)
  (define-key nov-mode-map (kbd "M-n") #'nov-bookmarks-rename))
```

**Check for conflicts:** Make sure `b` and `B` aren't already used in nov-mode.

---

## Step 15: Update Error Messages and Prompts

**Find and replace all:**
- "PDF" → "EPUB" (in messages)
- "page" → "position" or "chapter" (context-dependent)

---

## Step 16: Handle Edge Cases

### Missing Metadata

**Add validation:**
```elisp
(defun nov-bookmarks-create ()
  "Create a bookmark at the current position."
  (interactive)
  (unless (eq major-mode 'nov-mode)
    (error "Not in a nov-mode buffer"))

  ;; Validate we can identify the book
  (unless (nov-bookmarks-get-book-hash)
    (error "Cannot identify book - missing metadata"))

  ;; ... rest of function ...
```

### Empty Chapters

**Add validation:**
```elisp
(let* ((pos (nov-bookmarks-current-position))
       (current-chapter (plist-get pos :chapter))
       (current-position (plist-get pos :position)))
  (unless (and current-chapter
               current-position
               (> current-position 0))
    (error "Invalid position for bookmark"))
  ;; ... continue ...
```

---

## Step 17: Testing Checklist

Create this test script:

```elisp
;; Test nov-bookmarks implementation
(defun test-nov-bookmarks ()
  "Test bookmark functionality."
  (interactive)

  ;; 1. Open an EPUB file
  (message "Test 1: Open EPUB and verify mode")
  (unless (eq major-mode 'nov-mode)
    (error "Not in nov-mode"))

  ;; 2. Create bookmark
  (message "Test 2: Create bookmark")
  (call-interactively #'nov-bookmarks-create)

  ;; 3. Navigate away
  (message "Test 3: Navigate away")
  (nov-next-document)
  (sit-for 1)

  ;; 4. Return to bookmark
  (message "Test 4: Access bookmark")
  (call-interactively #'nov-bookmarks-access)
  (sit-for 1)

  ;; 5. Verify bookmarks file exists
  (message "Test 5: Verify storage")
  (let ((bookmark-file (nov-bookmarks-get-file-path)))
    (unless (file-exists-p bookmark-file)
      (error "Bookmark file not created: %s" bookmark-file))
    (message "✓ Bookmark file exists: %s" bookmark-file))

  (message "✓ All tests passed!"))
```

---

## Step 18: Update Documentation

Add to nov-highlights.el commentary:

```elisp
;;; Commentary:
;; This package provides highlighting, annotation, and bookmark functionality for Nov Mode
;; Features:
;; - Highlight text in various colors
;; - Add annotations to highlighted text
;; - Navigate between annotations
;; - Export highlights and annotations to Org/Markdown
;; - CREATE AND NAVIGATE BOOKMARKS (new!)
;;   - Create bookmarks at any position (b)
;;   - Quick navigation between bookmarks (B)
;;   - Rename (M-n) and delete (M-d) bookmarks
;;   - Bookmarks persist across file moves/renames (metadata-based)
;; - Persistent storage across sessions
```

---

## Step 19: Add to README

Create a new section in the nov-highlights README:

```markdown
### Bookmarks

Quickly mark and navigate between important locations in your EPUB books.

**Usage:**
- `b` - Create bookmark at current position
- `B` - Navigate to bookmark (defaults to previously visited for quick toggling)
- `M-n` - Rename bookmark
- `M-d` - Delete bookmark

**Features:**
- Bookmarks persist across sessions
- Survive file moves and renames (metadata-based identification)
- Smart defaulting for quick navigation
- Sorted by chapter order

**Storage:** `~/.emacs.d/nov-bookmarks/`
```

---

## Complete Code Template

Here's a minimal working implementation you can paste:

```elisp
;;;; EPUB Bookmarks System -------------------------------------------------

(require 'cl-lib)

(defvar nov-bookmarks-storage-directory
  (expand-file-name "nov-bookmarks/" user-emacs-directory)
  "Directory to store EPUB bookmark files.")

(defvar nov-bookmarks-current-file-bookmarks nil
  "List of bookmarks for the current EPUB file.")

(defvar nov-bookmarks-last-accessed nil
  "The last accessed bookmark name.")

(defvar nov-bookmarks-previous-accessed nil
  "The previously accessed bookmark name.")

(defun nov-bookmarks-ensure-directory ()
  "Ensure the bookmarks storage directory exists."
  (unless (file-directory-p nov-bookmarks-storage-directory)
    (make-directory nov-bookmarks-storage-directory t)))

(defun nov-bookmarks-get-book-hash ()
  "Get content-based hash for the current EPUB book."
  (when-let ((book-id (nov-highlights--current-book-id)))
    (secure-hash 'sha256 book-id)))

(defun nov-bookmarks-get-file-path ()
  "Get the bookmark file path for the current EPUB."
  (when-let ((book-hash (nov-bookmarks-get-book-hash)))
    (nov-bookmarks-ensure-directory)
    (expand-file-name
     (concat book-hash ".bookmarks")
     nov-bookmarks-storage-directory)))

(defun nov-bookmarks-load ()
  "Load bookmarks for the current EPUB file."
  (when-let ((bookmark-file (nov-bookmarks-get-file-path)))
    (setq nov-bookmarks-current-file-bookmarks
          (if (file-exists-p bookmark-file)
              (condition-case nil
                  (with-temp-buffer
                    (insert-file-contents bookmark-file)
                    (read (current-buffer)))
                (error nil))
            nil))))

(defun nov-bookmarks-save ()
  "Save bookmarks for the current EPUB file."
  (when-let ((bookmark-file (nov-bookmarks-get-file-path)))
    (with-temp-file bookmark-file
      (prin1 nov-bookmarks-current-file-bookmarks (current-buffer)))))

(defun nov-bookmarks-current-position ()
  "Get current chapter and position in EPUB."
  (list :chapter (nov-highlights--get-chapter-id)
        :position (point)))

(defun nov-bookmarks-goto-position (chapter position)
  "Navigate to CHAPTER and POSITION in current EPUB."
  (when (and chapter position)
    (nov-goto-document chapter)
    (run-with-timer 0.2 nil
      (lambda ()
        (goto-char position)
        (recenter)))))

(defun nov-bookmarks-find-at-position (chapter position)
  "Find bookmark at CHAPTER and POSITION (with tolerance)."
  (cl-find-if (lambda (bm)
                (and (= (plist-get bm :chapter) chapter)
                     (< (abs (- (plist-get bm :position) position)) 100)))
              nov-bookmarks-current-file-bookmarks))

(defun nov-bookmarks-format-choice (bookmark)
  "Format BOOKMARK for completion."
  (format "%s (Chapter %d)"
          (plist-get bookmark :name)
          (plist-get bookmark :chapter)))

(defun nov-bookmarks-create ()
  "Create a bookmark at the current position."
  (interactive)
  (unless (eq major-mode 'nov-mode)
    (error "Not in a nov-mode buffer"))

  (unless (nov-bookmarks-get-book-hash)
    (error "Cannot identify book"))

  (nov-bookmarks-load)

  (let* ((pos (nov-bookmarks-current-position))
         (current-chapter (plist-get pos :chapter))
         (current-position (plist-get pos :position)))

    (unless (and current-chapter current-position)
      (error "Could not get current position"))

    (let* ((bookmark-name (read-string "Bookmark name: "
                                       (format "Chapter %d" current-chapter)))
           (bookmark-entry (list :name bookmark-name
                                :chapter current-chapter
                                :position current-position
                                :timestamp (current-time))))

      (setq nov-bookmarks-current-file-bookmarks
            (cl-remove-if (lambda (bm) (string= (plist-get bm :name) bookmark-name))
                          nov-bookmarks-current-file-bookmarks))

      (push bookmark-entry nov-bookmarks-current-file-bookmarks)

      (setq nov-bookmarks-current-file-bookmarks
            (sort nov-bookmarks-current-file-bookmarks
                  (lambda (a b)
                    (let ((a-ch (plist-get a :chapter))
                          (b-ch (plist-get b :chapter)))
                      (if (= a-ch b-ch)
                          (< (plist-get a :position) (plist-get b :position))
                        (< a-ch b-ch))))))

      (nov-bookmarks-save)
      (message "Bookmark '%s' created at chapter %d" bookmark-name current-chapter))))

;; Add nov-bookmarks-access, nov-bookmarks-delete, nov-bookmarks-rename
;; (follow same pattern as PDF versions with adaptations from above)

;; Keybindings
(with-eval-after-load 'nov
  (define-key nov-mode-map (kbd "b") #'nov-bookmarks-create)
  (define-key nov-mode-map (kbd "B") #'nov-bookmarks-access)
  (define-key nov-mode-map (kbd "M-d") #'nov-bookmarks-delete)
  (define-key nov-mode-map (kbd "M-n") #'nov-bookmarks-rename))
```

---

## Summary of Key Differences

| Aspect | PDF Bookmarks | EPUB Bookmarks |
|--------|---------------|----------------|
| **Mode check** | `pdf-view-mode` | `nov-mode` |
| **Position** | Single integer (page) | Plist with chapter + position |
| **Current pos** | `(pdf-view-current-page)` | `(nov-bookmarks-current-position)` |
| **Navigation** | `(pdf-view-goto-page N)` | `(nov-bookmarks-goto-position ch pos)` |
| **Identification** | File content hash | Metadata hash (reuse nov-highlights) |
| **Sort key** | Page number | Chapter, then position |
| **Find match** | Exact page match | Fuzzy position match (±100 chars) |
| **Async rendering** | No | Yes (use timer) |
| **Migration** | Yes (filename → hash) | No (new feature) |

---

## Next Steps

1. **Read** `BOOKMARK_FEATURE_SPEC.md`
2. **Copy** bookmark code from pdf-tools-settings-fixes.el
3. **Adapt** using this guide
4. **Test** with the test script
5. **Document** in README
6. **Commit** with message: "Add bookmark navigation system"

---

If you find this project helpful, consider supporting it!

[Donate via PayPal](https://www.paypal.com/paypalme/revrari)
