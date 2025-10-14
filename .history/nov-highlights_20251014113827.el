;;; nov-highlights.el --- Highlights and annotations for Nov Mode -*- lexical-binding: t -*-
;;; Author: Raoul Comninos
;;; Commentary:
;; This package provides highlighting and annotation functionality for Nov Mode (ePub reader)
;; Features:
;; - Highlight text in green (g), orange (o), purple (,), blue underline (u), and strikeout in red (s)
;; - Add annotations to highlighted text (n) - creates yellow highlight with note
;; - View annotations with mouse hover popup (shows wrapped text with larger font)
;; - Click on annotation to edit it immediately, or press 'n' when cursor is on annotation
;; - Navigate between annotations with Alt-n and Alt-p (opens editor at bottom automatically)
;; - Navigation prompts before wrapping to first/last annotation
;; - Export all highlights and annotations to Org mode file (e) or Markdown file (m)
;; - Exports maintain proper chapter and position order
;; - Persistent storage of highlights across sessions

;;; Code:

(require 'nov)
(require 'org)
(require 'cl-lib)

(defgroup nov-highlights nil
  "Highlighting and annotation settings for Nov Mode."
  :group 'nov)

(defcustom nov-highlights-file
  (expand-file-name "nov-highlights.el" user-emacs-directory)
  "File to store highlights and annotations persistently."
  :type 'file
  :group 'nov-highlights)

(defface nov-highlight-green
  '((t (:background "light green" :foreground "black")))
  "Face for green highlights in Nov mode.")

(defface nov-highlight-orange
  '((t (:background "orange" :foreground "black")))
  "Face for orange highlights in Nov mode.")

(defface nov-highlight-yellow
  '((t (:background "yellow" :foreground "black")))
  "Face for yellow highlights (annotations) in Nov mode.")

(defface nov-highlight-underline
  '((t (:underline (:color "blue" :style line) :foreground "blue")))
  "Face for blue underline highlights in Nov mode.")

(defface nov-highlight-strikeout
  '((t (:strike-through t :background "misty rose" :foreground "dark red")))
  "Face for strikeout highlights in Nov mode.")

(defface nov-highlight-purple
  '((t (:background "plum" :foreground "black")))
  "Face for purple highlights in Nov mode.")

;; Data structure to store highlights
(defvar-local nov-highlights-data nil
  "List of highlights for the current buffer.
Each element is a plist with :start :end :type :text :annotation :chapter")

(defvar nov-highlights-db (make-hash-table :test 'equal)
  "Global database of highlights indexed by book file path.")

;; Configure tooltip appearance
(setq x-gtk-use-system-tooltips nil)  ; Use Emacs tooltips instead of system tooltips

;; Reduce tooltip frame padding and make background opaque
(defun nov-highlights--configure-tooltip-frame ()
  "Configure tooltip frame parameters for compact, opaque display."
  (when (boundp 'tooltip-frame-parameters)
    (setq tooltip-frame-parameters
          (append '((internal-border-width . 2)
                    (border-width . 0)
                    (alpha . 100))
                  tooltip-frame-parameters))))

(nov-highlights--configure-tooltip-frame)

;;; Core Functions

(defun nov-highlights--current-book-id ()
  "Get unique identifier for current book based on metadata.
Uses title and creator from epub metadata for a stable ID that persists
across file moves and renames. Falls back to filename if metadata unavailable."
  (if (bound-and-true-p nov-metadata)
      (let ((title (cdr (assq 'title nov-metadata)))
            (creator (cdr (assq 'creator nov-metadata))))
        (if (and title creator)
            (format "%s::%s" title creator)
          ;; Fallback to title only if creator missing
          (or title
              ;; Final fallback to filename
              (when (bound-and-true-p nov-file-name)
                (file-name-nondirectory nov-file-name)))))
    ;; Fallback when metadata not available
    (when (bound-and-true-p nov-file-name)
      (file-name-nondirectory nov-file-name))))

(defun nov-highlights--get-chapter-id ()
  "Get current chapter number."
  (when (bound-and-true-p nov-documents-index)
    nov-documents-index))

(defun nov-highlights--save-db ()
  "Save highlights database to disk."
  (with-temp-file nov-highlights-file
    (prin1 (let ((alist nil))
             (maphash (lambda (k v) (push (cons k v) alist)) nov-highlights-db)
             alist)
           (current-buffer))))

(defun nov-highlights--load-db ()
  "Load highlights database from disk."
  (when (file-exists-p nov-highlights-file)
    (with-temp-buffer
      (insert-file-contents nov-highlights-file)
      (let ((data (read (current-buffer))))
        (setq nov-highlights-db (make-hash-table :test 'equal))
        (dolist (entry data)
          (puthash (car entry) (cdr entry) nov-highlights-db))))))

(defun nov-highlights--wrap-text (text width)
  "Wrap TEXT to WIDTH characters per line."
  (with-temp-buffer
    (insert text)
    (goto-char (point-min))
    (let ((fill-column width))
      (fill-region (point-min) (point-max)))
    (buffer-string)))

(defun nov-highlights--apply-highlight (start end type &optional annotation)
  "Apply highlight overlay from START to END with TYPE and optional ANNOTATION."
  (let ((overlay (make-overlay start end))
        (face (cond
               ((eq type 'green) 'nov-highlight-green)
               ((eq type 'orange) 'nov-highlight-orange)
               ((eq type 'yellow) 'nov-highlight-yellow)
               ((eq type 'underline) 'nov-highlight-underline)
               ((eq type 'strikeout) 'nov-highlight-strikeout)
               ((eq type 'purple) 'nov-highlight-purple))))
    (overlay-put overlay 'face face)
    (overlay-put overlay 'nov-highlight t)
    (overlay-put overlay 'nov-highlight-type type)
    (overlay-put overlay 'evaporate t)

    ;; Add tooltip and mouse click for annotations
    (when (and annotation (> (length annotation) 0))
      ;; Add tooltip that shows on hover with wrapped text and larger font
      (overlay-put overlay 'help-echo
                   (lambda (window object pos)
                     (let ((wrapped-text (nov-highlights--wrap-text annotation 60)))
                       (propertize wrapped-text 'face '(:height 1.1)))))
      ;; Make annotation editable on double-click (to avoid conflict with nov-mode)
      (overlay-put overlay 'mouse-face 'highlight)
      (let ((map (make-sparse-keymap)))
        (define-key map [double-mouse-1]
          (lambda (event)
            (interactive "e")
            (let ((pos (posn-point (event-start event))))
              (when pos
                (goto-char pos)
                (nov-highlights-annotate)))))
        (overlay-put overlay 'keymap map)))
    overlay))

(defun nov-highlights--remove-overlays ()
  "Remove all highlight overlays from current buffer."
  (remove-overlays (point-min) (point-max) 'nov-highlight t))

(defun nov-highlights--restore-highlights ()
  "Restore highlights for current chapter."
  (nov-highlights--remove-overlays)
  (let ((book-id (nov-highlights--current-book-id))
        (chapter-id (nov-highlights--get-chapter-id)))
    (when (and book-id chapter-id)
      (let ((book-highlights (gethash book-id nov-highlights-db)))
        (dolist (highlight book-highlights)
          (when (= (plist-get highlight :chapter) chapter-id)
            (condition-case nil
                (nov-highlights--apply-highlight
                 (plist-get highlight :start)
                 (plist-get highlight :end)
                 (plist-get highlight :type)
                 (plist-get highlight :annotation))
              (error nil))))))))

(defun nov-highlights--create-highlight (type)
  "Create a highlight of TYPE for selected region."
  (if (use-region-p)
      (let* ((start (region-beginning))
             (end (region-end))
             (text (buffer-substring-no-properties start end))
             (book-id (nov-highlights--current-book-id))
             (chapter-id (nov-highlights--get-chapter-id)))
        (when (and book-id chapter-id)
          ;; Store highlight data
          (let* ((highlight (list :start start
                                 :end end
                                 :type type
                                 :text text
                                 :chapter chapter-id
                                 :annotation nil))
                 (book-highlights (gethash book-id nov-highlights-db)))
            (puthash book-id (cons highlight book-highlights) nov-highlights-db)
            (nov-highlights--save-db)
            (nov-highlights--restore-highlights))
          
          (deactivate-mark)
          (message "Added %s highlight" type)))
    (message "No region selected")))

;;; Interactive Commands

(defun nov-highlights-green ()
  "Highlight selected text in green."
  (interactive)
  (nov-highlights--create-highlight 'green))

(defun nov-highlights-orange ()
  "Highlight selected text in orange."
  (interactive)
  (nov-highlights--create-highlight 'orange))

(defun nov-highlights-underline ()
  "Apply blue underline to selected text."
  (interactive)
  (nov-highlights--create-highlight 'underline))

(defun nov-highlights-strikeout ()
  "Apply strikeout to selected text."
  (interactive)
  (nov-highlights--create-highlight 'strikeout))

(defun nov-highlights-purple ()
  "Highlight selected text in purple."
  (interactive)
  (nov-highlights--create-highlight 'purple))

(defvar-local nov-highlights--annotation-callback nil
  "Callback function to save annotation after editing.")

(defvar-local nov-highlights--annotation-buffer nil
  "Original buffer where annotation was initiated.")

(defun nov-highlights--annotation-commit ()
  "Commit the annotation and close the annotation buffer."
  (interactive)
  (when nov-highlights--annotation-callback
    (let* ((callback nov-highlights--annotation-callback)
           (original-buffer nov-highlights--annotation-buffer)
           (annotation-window (selected-window))
           ;; Get all text and clean it line by line
           (raw-text (buffer-substring-no-properties (point-min) (point-max)))
           (lines (split-string raw-text "\n"))
           (clean-lines nil))
      
      ;; Filter out all header/comment lines
      (dolist (line lines)
        (unless (string-match-p "^#" line)
          (push line clean-lines)))
      
      (let ((annotation-text (string-trim (mapconcat 'identity (nreverse clean-lines) "\n"))))
        (when (buffer-live-p original-buffer)
          (with-current-buffer original-buffer
            (funcall callback annotation-text)))
        (when (window-live-p annotation-window)
          (delete-window annotation-window))))))

(defvar-local nov-highlights--original-annotation-text nil
  "Store the original annotation text to detect changes.")

(defun nov-highlights--annotation-cancel ()
  "Cancel annotation editing with smart change detection."
  (interactive)
  (let* ((annotation-window (selected-window))
         (annotation-buffer (current-buffer))
         (current-text (buffer-substring-no-properties (point-min) (point-max)))
         (lines (split-string current-text "\n"))
         (clean-lines nil))

    ;; Filter out header lines
    (dolist (line lines)
      (unless (string-match-p "^#" line)
        (push line clean-lines)))

    (let ((current-annotation (string-trim (mapconcat 'identity (nreverse clean-lines) "\n")))
          (original-annotation (or nov-highlights--original-annotation-text "")))

      ;; Check if changes were made
      (if (string= current-annotation original-annotation)
          ;; No changes, just close
          (progn
            (when (window-live-p annotation-window)
              (delete-window annotation-window))
            (when (buffer-live-p annotation-buffer)
              (kill-buffer annotation-buffer)))
        ;; Changes detected, ask to save
        (if (y-or-n-p "Save changes to annotation? ")
            (nov-highlights--annotation-commit)
          (when (window-live-p annotation-window)
            (delete-window annotation-window))
          (when (buffer-live-p annotation-buffer)
            (kill-buffer annotation-buffer))
          (message "Annotation changes discarded"))))))

(defun nov-highlights-close-annotation-windows ()
  "Close any open annotation windows from main nov buffer."
  (interactive)
  (let ((annotation-buf (get-buffer "*Nov Annotation*"))
        (annotation-view-buf (get-buffer "*Nov Annotation View*")))
    (when annotation-buf
      (let ((win (get-buffer-window annotation-buf)))
        (when win (delete-window win)))
      (kill-buffer annotation-buf))
    (when annotation-view-buf
      (let ((win (get-buffer-window annotation-view-buf)))
        (when win (delete-window win)))
      (kill-buffer annotation-view-buf))))

(defun nov-highlights--open-annotation-buffer (initial-text callback-fn quoted-text)
  "Open an annotation buffer with INITIAL-TEXT, calling CALLBACK-FN on commit.
QUOTED-TEXT is shown as context in the header."
  (let* ((buf (get-buffer-create "*Nov Annotation*"))
         (original-buffer (current-buffer))
         (view-buf (get-buffer "*Nov Annotation View*"))
         (existing-view-win (when view-buf (get-buffer-window view-buf)))
         (existing-edit-win (get-buffer-window buf))
         win)

    ;; Reuse existing window or create new one
    (if (and existing-edit-win (window-live-p existing-edit-win))
        (progn
          ;; Reuse the existing annotation edit window
          (setq win existing-edit-win)
          (set-window-buffer win buf))
      (if (and existing-view-win (window-live-p existing-view-win))
          (progn
            ;; Reuse the view window for edit
            (setq win existing-view-win)
            (set-window-buffer win buf)
            (when view-buf (kill-buffer view-buf)))
        ;; Create new window at same size as view
        (setq win (split-window (frame-root-window)
                                (- (floor (* (window-height (frame-root-window)) 0.25)))
                                'below))
        (set-window-buffer win buf)))
    (select-window win)
    
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (org-mode)
        (insert "# Annotation for: " quoted-text "\n")
        (insert "# ──────────────────────────────────────────────\n")
        (when initial-text
          (insert initial-text))

        (setq nov-highlights--annotation-callback callback-fn)
        (setq nov-highlights--annotation-buffer original-buffer)
        (setq nov-highlights--original-annotation-text (or initial-text ""))

        ;; Set up local keybindings
        (local-set-key (kbd "C-c C-c") 'nov-highlights--annotation-commit)
        (local-set-key (kbd "C-c C-k") 'nov-highlights--annotation-cancel)
        (local-set-key (kbd "q") 'nov-highlights--annotation-cancel)
        (local-set-key (kbd "<escape>") 'nov-highlights--annotation-cancel)

        ;; Position cursor after header
        (goto-char (point-max))))))

(defun nov-highlights-annotate ()
  "Add annotation to highlighted text at point or selected region."
  (interactive)
  (let* ((book-id (nov-highlights--current-book-id))
         (chapter-id (nov-highlights--get-chapter-id))
         (pos (point))
         (book-highlights (gethash book-id nov-highlights-db))
         (highlight-found nil))
    
    ;; Find highlight at point or create new one
    (if (use-region-p)
        ;; Create new highlight with annotation
        (let* ((start (region-beginning))
               (end (region-end))
               (text (buffer-substring-no-properties start end))
               (quoted-text (truncate-string-to-width
                            (replace-regexp-in-string "\n" " " text) 60)))
          (deactivate-mark)
          (nov-highlights--open-annotation-buffer
           ""
           (lambda (annotation)
             (nov-highlights--apply-highlight start end 'yellow annotation)
             (let ((highlight (list :start start
                                   :end end
                                   :type 'yellow
                                   :text text
                                   :chapter chapter-id
                                   :annotation annotation)))
               (puthash book-id (cons highlight (gethash book-id nov-highlights-db)) 
                       nov-highlights-db)
               (nov-highlights--save-db)
               (message "Added annotated highlight")))
           quoted-text))
      
      ;; Find existing highlight at point
      (dolist (highlight book-highlights)
        (when (and (= (plist-get highlight :chapter) chapter-id)
                   (>= pos (plist-get highlight :start))
                   (<= pos (plist-get highlight :end))
                   (not highlight-found))
          (let ((text (plist-get highlight :text))
                (existing-annotation (plist-get highlight :annotation)))
            (setq highlight-found t)
            (nov-highlights--open-annotation-buffer
             (or existing-annotation "")
             (lambda (annotation)
               (plist-put highlight :annotation annotation)
               (nov-highlights--save-db)
               (message "Annotation updated"))
             (truncate-string-to-width
              (replace-regexp-in-string "\n" " " text) 60)))))
      
      (unless highlight-found
        (message "No highlight at point. Select text to create annotated highlight.")))))

(defun nov-highlights-remove-at-point ()
  "Remove highlight at point, including annotation if present. Close annotation window if open."
  (interactive)
  (let* ((book-id (nov-highlights--current-book-id))
         (chapter-id (nov-highlights--get-chapter-id))
         (pos (point))
         (book-highlights (gethash book-id nov-highlights-db))
         (new-highlights nil)
         (removed nil)
         (had-annotation nil)
         (annotation-buf (get-buffer "*Nov Annotation View*"))
         (annotation-win (when annotation-buf (get-buffer-window annotation-buf))))
    
    (dolist (highlight book-highlights)
      (if (and (= (plist-get highlight :chapter) chapter-id)
               (>= pos (plist-get highlight :start))
               (<= pos (plist-get highlight :end)))
          (progn
            (setq removed t)
            (when (plist-get highlight :annotation)
              (setq had-annotation t)))
        (push highlight new-highlights)))
    
    (when removed
      (puthash book-id (nreverse new-highlights) nov-highlights-db)
      (nov-highlights--save-db)
      (nov-highlights--restore-highlights)
      
      ;; Close annotation window if it was open and we removed an annotation
      (when (and had-annotation annotation-win (window-live-p annotation-win))
        (delete-window annotation-win))
      
      (if had-annotation
          (message "Highlight and annotation removed")
        (message "Highlight removed")))
    
    (unless removed
      (message "No highlight at point"))))

(defun nov-highlights-remove-in-region ()
  "Remove all highlights and annotations in the selected region."
  (interactive)
  (if (not (use-region-p))
      (message "No region selected")
    (let* ((book-id (nov-highlights--current-book-id))
           (chapter-id (nov-highlights--get-chapter-id))
           (region-start (region-beginning))
           (region-end (region-end))
           (book-highlights (gethash book-id nov-highlights-db))
           (new-highlights nil)
           (removed-count 0)
           (removed-annotations 0)
           (annotation-buf (get-buffer "*Nov Annotation View*"))
           (annotation-win (when annotation-buf (get-buffer-window annotation-buf))))
      
      ;; Filter out highlights that overlap with the selected region
      (dolist (highlight book-highlights)
        (let ((hl-start (plist-get highlight :start))
              (hl-end (plist-get highlight :end))
              (hl-chapter (plist-get highlight :chapter)))
          ;; Check if highlight overlaps with region in current chapter
          (if (and (= hl-chapter chapter-id)
                   (not (or (>= hl-start region-end)
                           (<= hl-end region-start))))
              (progn
                (setq removed-count (1+ removed-count))
                (when (plist-get highlight :annotation)
                  (setq removed-annotations (1+ removed-annotations))))
            (push highlight new-highlights))))
      
      (if (= removed-count 0)
          (message "No highlights found in region")
        (puthash book-id (nreverse new-highlights) nov-highlights-db)
        (nov-highlights--save-db)
        (nov-highlights--restore-highlights)
        (deactivate-mark)
        
        ;; Close annotation window if any annotations were removed
        (when (and (> removed-annotations 0) annotation-win (window-live-p annotation-win))
          (delete-window annotation-win))
        
        (message "Removed %d highlight%s (%d with annotations)"
                removed-count
                (if (= removed-count 1) "" "s")
                removed-annotations)))))

(defun nov-highlights-view-annotation ()
  "View annotation at point in a read-only buffer."
  (interactive)
  (let* ((book-id (nov-highlights--current-book-id))
         (chapter-id (nov-highlights--get-chapter-id))
         (pos (point))
         (book-highlights (gethash book-id nov-highlights-db))
         (annotation-found nil)
         (original-buffer (current-buffer))
         (original-pos pos))
    (dolist (highlight book-highlights)
      (when (and (= (plist-get highlight :chapter) chapter-id)
                 (>= pos (plist-get highlight :start))
                 (<= pos (plist-get highlight :end))
                 (not annotation-found))
        (let ((annotation (plist-get highlight :annotation))
              (text (plist-get highlight :text)))
          (when annotation
            (setq annotation-found t)
            (let* ((buf (get-buffer-create "*Nov Annotation View*"))
                   (existing-win (get-buffer-window buf))
                   (win (or existing-win
                            (split-window (frame-root-window)
                                          (- (floor (* (window-height (frame-root-window)) 0.25)))
                                          'below))))
              (set-window-buffer win buf)
              (with-current-buffer buf
                (let ((inhibit-read-only t))
                  (erase-buffer)
                  (org-mode)
                  ;; Replace newlines with spaces in the header text
                  (let ((header-text (replace-regexp-in-string "\n" " " text)))
                    (insert "# Annotation for: " (truncate-string-to-width header-text 60) "\n"))
                  (insert "# ────────────────────────────────────────────\n")
                  (insert annotation)
                  (goto-char (point-min))
                  (view-mode 1)
                  ;; Close window
                  (local-set-key
                   (kbd "q")
                   (lambda ()
                     (interactive)
                     (let ((win (selected-window)))
                       (when (window-live-p win)
                         (delete-window win)))))
                  (local-set-key
                   (kbd "<escape>")
                   (lambda ()
                     (interactive)
                     (let ((win (selected-window)))
                       (when (window-live-p win)
                         (delete-window win)))))
                  ;; Edit annotation
                  (local-set-key
                   (kbd "RET")
                   (lambda ()
                     (interactive)
                     (let ((win (selected-window)))
                       (when (window-live-p win)
                         (delete-window win)))
                     (when (buffer-live-p original-buffer)
                       (with-current-buffer original-buffer
                         (goto-char original-pos)
                         (nov-highlights-annotate)))))
                  ;; Click to edit
                  (local-set-key
                   (kbd "<mouse-1>")
                   (lambda (event)
                     (interactive "e")
                     (let ((win (selected-window)))
                       (when (window-live-p win)
                         (delete-window win)))
                     (when (buffer-live-p original-buffer)
                       (with-current-buffer original-buffer
                         (goto-char original-pos)
                         (nov-highlights-annotate)))))
                  (local-set-key
                   (kbd "<double-mouse-1>")
                   (lambda (event)
                     (interactive "e")
                     (let ((win (selected-window)))
                       (when (window-live-p win)
                         (delete-window win)))
                     (when (buffer-live-p original-buffer)
                       (with-current-buffer original-buffer
                         (goto-char original-pos)
                         (nov-highlights-annotate))))))))))))
    ;; <-- still inside the let* here
    (unless annotation-found
      (message "No annotation at point"))))

;;;;;;;;;;;;;;;; 

(defun nov-highlights--get-annotated-highlights ()
  "Get all highlights with annotations in the current chapter, sorted by position."
  (let* ((book-id (nov-highlights--current-book-id))
         (chapter-id (nov-highlights--get-chapter-id))
         (book-highlights (gethash book-id nov-highlights-db))
         (annotated nil))
    (dolist (highlight book-highlights)
      (when (and (= (plist-get highlight :chapter) chapter-id)
                 (plist-get highlight :annotation))
        (push highlight annotated)))
    (sort annotated (lambda (a b)
                     (< (plist-get a :start) (plist-get b :start))))))

(defun nov-highlights-next-annotation ()
  "Jump to the next annotation in the current chapter and view it."
  (interactive)
  ;; Close any existing annotation windows
  (let ((annotation-buf (get-buffer "*Nov Annotation*"))
        (annotation-view-buf (get-buffer "*Nov Annotation View*")))
    (when annotation-buf
      (let ((win (get-buffer-window annotation-buf)))
        (when win (delete-window win)))
      (kill-buffer annotation-buf))
    (when annotation-view-buf
      (let ((win (get-buffer-window annotation-view-buf)))
        (when win (delete-window win)))
      (kill-buffer annotation-view-buf)))

  (let* ((annotated-highlights (nov-highlights--get-annotated-highlights))
         (current-pos (point))
         (next-highlight nil))

    (if (not annotated-highlights)
        (message "No annotations in current chapter")

      ;; Find the first annotation after current position
      (dolist (highlight annotated-highlights)
        (when (and (not next-highlight)
                   (> (plist-get highlight :start) current-pos))
          (setq next-highlight highlight)))

      ;; If no annotation found after current position, prompt to wrap
      (unless next-highlight
        (when (y-or-n-p "Reached last annotation. Wrap to first? ")
          (setq next-highlight (car annotated-highlights))))

      (when next-highlight
        (goto-char (plist-get next-highlight :start))
        (nov-highlights-view-annotation)))))

(defun nov-highlights-previous-annotation ()
  "Jump to the previous annotation in the current chapter and view it."
  (interactive)
  ;; Close any existing annotation windows
  (let ((annotation-buf (get-buffer "*Nov Annotation*"))
        (annotation-view-buf (get-buffer "*Nov Annotation View*")))
    (when annotation-buf
      (let ((win (get-buffer-window annotation-buf)))
        (when win (delete-window win)))
      (kill-buffer annotation-buf))
    (when annotation-view-buf
      (let ((win (get-buffer-window annotation-view-buf)))
        (when win (delete-window win)))
      (kill-buffer annotation-view-buf)))

  (let* ((annotated-highlights (nov-highlights--get-annotated-highlights))
         (current-pos (point))
         (prev-highlight nil))

    (if (not annotated-highlights)
        (message "No annotations in current chapter")

      ;; Find the last annotation before current position
      (dolist (highlight annotated-highlights)
        (when (< (plist-get highlight :end) current-pos)
          (setq prev-highlight highlight)))

      ;; If no annotation found before current position, prompt to wrap
      (unless prev-highlight
        (when (y-or-n-p "Reached first annotation. Wrap to last? ")
          (setq prev-highlight (car (last annotated-highlights)))))

      (when prev-highlight
        (goto-char (plist-get prev-highlight :start))
        (nov-highlights-view-annotation)))))

(defun nov-highlights-export-to-markdown ()
  "Export all highlights and annotations to a Markdown file."
  (interactive)
  (let* ((book-id (nov-highlights--current-book-id))
         (book-highlights (gethash book-id nov-highlights-db))
         (book-name (file-name-sans-extension
                    (file-name-nondirectory book-id)))
         (md-file (read-file-name "Export to Markdown file: "
                                  nil
                                  (format "%s-notes.md" book-name)
                                  nil
                                  (format "%s-notes.md" book-name))))

    (if (not book-highlights)
        (message "No highlights to export")

      ;; Sort highlights by chapter and position
      (let ((sorted-highlights
             (sort (copy-sequence book-highlights)
                   (lambda (a b)
                     (let ((ch-a (plist-get a :chapter))
                           (ch-b (plist-get b :chapter))
                           (pos-a (plist-get a :start))
                           (pos-b (plist-get b :start)))
                       (if (= ch-a ch-b)
                           (< pos-a pos-b)
                         (< ch-a ch-b)))))))

        ;; Group by chapter
        (let ((by-chapter (make-hash-table :test 'equal)))
          (dolist (highlight sorted-highlights)
            (let ((chapter (plist-get highlight :chapter)))
              (puthash chapter
                      (append (gethash chapter by-chapter) (list highlight))
                      by-chapter)))

          ;; Write to Markdown file
          (with-temp-file md-file
            (insert (format "# Notes from %s\n\n" book-name))
            (insert (format "*Date: %s*\n\n" (format-time-string "%Y-%m-%d")))

            (let ((chapters (sort (hash-table-keys by-chapter) #'<)))
              (dolist (chapter chapters)
                (insert (format "## Chapter %d\n\n" (1+ chapter)))

                (dolist (highlight (gethash chapter by-chapter))
                  (let ((type (plist-get highlight :type))
                        (text (plist-get highlight :text))
                        (annotation (plist-get highlight :annotation)))

                    ;; Format based on type
                    (insert (format "### %s\n\n"
                                   (cond
                                    ((eq type 'green) "🟢 GREEN")
                                    ((eq type 'orange) "🟠 ORANGE")
                                    ((eq type 'purple) "🟣 PURPLE")
                                    ((eq type 'yellow) "🟡 YELLOW/NOTE")
                                    ((eq type 'underline) "📘 UNDERLINE")
                                    ((eq type 'strikeout) "❌ STRIKEOUT")
                                    (t "📌 HIGHLIGHT"))))

                    (insert "> ")
                    ;; Replace newlines with spaces for clean export
                    (insert (replace-regexp-in-string "\n" " " text))
                    (insert "\n\n")

                    (when annotation
                      (insert "**Note:** ")
                      (insert annotation)
                      (insert "\n\n"))

                    (insert "---\n\n"))))))

          (message "Exported to %s" md-file)
          (when (y-or-n-p "Open exported file? ")
            (find-file md-file)))))))

(defun nov-highlights-export-to-org ()
  "Export all highlights and annotations to an Org file."
  (interactive)
  (let* ((book-id (nov-highlights--current-book-id))
         (book-highlights (gethash book-id nov-highlights-db))
         (book-name (file-name-sans-extension 
                    (file-name-nondirectory book-id)))
         (org-file (read-file-name "Export to Org file: "
                                  nil
                                  (format "%s-notes.org" book-name)
                                  nil
                                  (format "%s-notes.org" book-name))))
    
    (if (not book-highlights)
        (message "No highlights to export")

      ;; Sort highlights by chapter and position
      (let ((sorted-highlights
             (sort (copy-sequence book-highlights)
                   (lambda (a b)
                     (let ((ch-a (plist-get a :chapter))
                           (ch-b (plist-get b :chapter))
                           (pos-a (plist-get a :start))
                           (pos-b (plist-get b :start)))
                       (if (= ch-a ch-b)
                           (< pos-a pos-b)
                         (< ch-a ch-b)))))))

        ;; Group by chapter (preserving order)
        (let ((by-chapter (make-hash-table :test 'equal)))
          (dolist (highlight sorted-highlights)
            (let ((chapter (plist-get highlight :chapter)))
              (puthash chapter
                      (append (gethash chapter by-chapter) (list highlight))
                      by-chapter)))

          ;; Write to Org file
          (with-temp-file org-file
            (insert (format "#+TITLE: Notes from %s\n" book-name))
            (insert (format "#+DATE: %s\n\n" (format-time-string "%Y-%m-%d")))

            (let ((chapters (sort (hash-table-keys by-chapter) #'<)))
              (dolist (chapter chapters)
                (insert (format "* Chapter %d\n\n" (1+ chapter)))

                (dolist (highlight (gethash chapter by-chapter))
                  (let ((type (plist-get highlight :type))
                        (text (plist-get highlight :text))
                        (annotation (plist-get highlight :annotation)))

                    ;; Format based on type
                    (insert (format "** %s\n"
                                   (cond
                                    ((eq type 'green) "[GREEN]")
                                    ((eq type 'orange) "[ORANGE]")
                                    ((eq type 'purple) "[PURPLE]")
                                    ((eq type 'yellow) "[YELLOW/NOTE]")
                                    ((eq type 'underline) "[UNDERLINE]")
                                    ((eq type 'strikeout) "[STRIKEOUT]")
                                    (t "[HIGHLIGHT]"))))

                    (insert "#+BEGIN_QUOTE\n")
                    ;; Replace newlines with spaces for clean export
                    (insert (replace-regexp-in-string "\n" " " text))
                    (insert "\n#+END_QUOTE\n")

                    (when annotation
                      (insert "\n*Note:* ")
                      (insert annotation)
                      (insert "\n"))

                    (insert "\n"))))))

          (message "Exported to %s" org-file)
          (when (y-or-n-p "Open exported file? ")
            (find-file org-file)))))))


(defun nov-highlights-list ()
  "List all highlights in the current book (call from a nov-mode buffer)."
  (interactive)
  (let* ((book-id (nov-highlights--current-book-id))
         (book-highlights (or (gethash book-id nov-highlights-db) '()))
         (buf (get-buffer-create "*Nov Highlights*")))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (special-mode)                       ; nice read-only listing
        (insert (format "Highlights for: %s\n\n"
                        (file-name-nondirectory book-id)))
        (if (null book-highlights)
            (insert "No highlights found.\n")
          (dolist (h (reverse book-highlights))
            (let* ((chapter (plist-get h :chapter))
                   (type    (plist-get h :type))
                   (text    (or (plist-get h :text) ""))
                   (note    (plist-get h :annotation)))
              (insert (format "Chapter %d | %s | %s\n"
                              (1+ (or chapter 0))
                              (upcase (symbol-name (or type 'unknown)))
                              (truncate-string-to-width text 60)))
              (when (and note (stringp note) (> (length note) 0))
                (insert (format "  → %s\n" note)))
              (insert "\n")))))
      (goto-char (point-min)))
    (display-buffer buf)))

;; (defun nov-highlights-list ()
;;   "List all highlights in current book."
;;   (interactive)
;;   (let* ((book-id (nov-highlights--get-book-id))
;;          (book-highlights (gethash book-id nov-highlights-db))
;;          (buf (get-buffer-create "*Nov Highlights*")))
    
;;     (with-current-buffer buf
;;       (erase-buffer)
;;       (insert (format "Highlights for: %s\n\n" 
;;                      (file-name-nondirectory book-id)))
      
;;       (if (not book-highlights)
;;           (insert "No highlights found.\n")
        
;;         (dolist (highlight (reverse book-highlights))
;;           (insert (format "Chapter %d | %s | %s\n"
;;                          (1+ (plist-get highlight :chapter))
;;                          (upcase (symbol-name (plist-get highlight :type)))
;;                          (truncate-string-to-width 
;;                           (plist-get highlight :text) 60)))
          
;;           (when (plist-get highlight :annotation)
;;             (insert (format "  → %s\n" 
;;                            (plist-get highlight :annotation))))
;;           (insert "\n"))))
    
;;     (display-buffer buf)))

;;; Setup and Mode Definition

(defun nov-highlights-setup ()
  "Set up highlights when entering Nov mode."
  (nov-highlights--load-db)
  (nov-highlights--restore-highlights)
  (add-hook 'nov-post-html-render-hook 'nov-highlights--restore-highlights nil t))

(define-minor-mode nov-highlights-mode
  "Minor mode for highlights and annotations in Nov mode."
  :lighter " NovHL"
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "g") 'nov-highlights-green)
            (define-key map (kbd "o") 'nov-highlights-orange)
            (define-key map (kbd ",") 'nov-highlights-purple)
            (define-key map (kbd "u") 'nov-highlights-underline)
            (define-key map (kbd "s") 'nov-highlights-strikeout)
            (define-key map (kbd "n") 'nov-highlights-annotate)
            (define-key map (kbd "v") 'nov-highlights-view-annotation)
            (define-key map (kbd "e") 'nov-highlights-export-to-org)
            (define-key map (kbd "M-r") 'nov-highlights-remove-at-point)
            (define-key map (kbd "D") 'nov-highlights-remove-in-region)
            (define-key map (kbd "m") 'nov-highlights-export-to-markdown)
            (define-key map (kbd "M-l") 'nov-highlights-list)
            (define-key map (kbd "A-n") 'nov-highlights-next-annotation)      ; Alt-N
            (define-key map (kbd "M-n") 'nov-highlights-next-annotation)      ; Also support M-n
            (define-key map (kbd "A-p") 'nov-highlights-previous-annotation)  ; Alt-P
            (define-key map (kbd "M-p") 'nov-highlights-previous-annotation)  ; Also support M-p
            (define-key map (kbd "<escape>") 'nov-highlights-close-annotation-windows)  ; ESC closes annotations
            map)
  
  (if nov-highlights-mode
      (nov-highlights-setup)
    (nov-highlights--remove-overlays)))

;; Auto-enable in Nov mode
(add-hook 'nov-mode-hook 'nov-highlights-mode)

;; Clean up annotation windows when nov buffer is closed
(defun nov-highlights--cleanup-windows ()
  "Close annotation windows when nov buffer is quit or killed."
  (when (derived-mode-p 'nov-mode)
    (let ((annotation-buf (get-buffer "*Nov Annotation View*"))
          (annotation-edit-buf (get-buffer "*Nov Annotation*")))
      (when annotation-buf
        (let ((win (get-buffer-window annotation-buf)))
          (when win (delete-window win)))
        (kill-buffer annotation-buf))
      (when annotation-edit-buf
        (let ((win (get-buffer-window annotation-edit-buf)))
          (when win (delete-window win)))
        (kill-buffer annotation-edit-buf)))))

(add-hook 'nov-mode-hook
          (lambda ()
            (add-hook 'kill-buffer-hook 'nov-highlights--cleanup-windows nil t)
            (add-hook 'quit-window-hook 'nov-highlights--cleanup-windows nil t)))

;; Zoom Functions

(defun nov-zoom-in ()
  "Increase text size in nov-mode."
  (interactive)
  (text-scale-increase 1))

(defun nov-zoom-out ()
  "Decrease text size in nov-mode."
  (interactive)
  (text-scale-decrease 1))

(defun nov-zoom-reset ()
  "Reset text size in nov-mode."
  (interactive)
  (text-scale-set 0))

;; Add keybindings for zoom
(add-hook 'nov-mode-hook
          (lambda ()
            (local-set-key (kbd "C-+") 'nov-zoom-in)
            (local-set-key (kbd "C-=") 'nov-zoom-in)  ; Alternative
            (local-set-key (kbd "C--") 'nov-zoom-out)
            (local-set-key (kbd "C-0") 'nov-zoom-reset)))

(provide 'nov-highlights)
;;; nov-highlights.el ends here
