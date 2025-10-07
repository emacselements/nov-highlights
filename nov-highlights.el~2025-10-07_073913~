;;; nov-highlights.el --- Highlights and annotations for Nov Mode -*- lexical-binding: t -*-

;;; Commentary:
;; This package provides highlighting and annotation functionality for Nov Mode (ePub reader)
;; Features:
;; - Highlight text in green (g), orange (h), blue underline (u), and strikeout in red (s)
;; - Add annotations to highlighted text (n) - creates yellow highlight with note
;; - Export all highlights and annotations to Org mode file (e)
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

;; Data structure to store highlights
(defvar-local nov-highlights-data nil
  "List of highlights for the current buffer.
Each element is a plist with :start :end :type :text :annotation :chapter")

(defvar nov-highlights-db (make-hash-table :test 'equal)
  "Global database of highlights indexed by book file path.")

;;; Core Functions

(defun nov-highlights--get-book-id ()
  "Get unique identifier for current book."
  (when (bound-and-true-p nov-file-name)
    (expand-file-name nov-file-name)))

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

(defun nov-highlights--apply-highlight (start end type)
  "Apply highlight overlay from START to END with TYPE."
  (let ((overlay (make-overlay start end))
        (face (cond
               ((eq type 'green) 'nov-highlight-green)
               ((eq type 'orange) 'nov-highlight-orange)
               ((eq type 'yellow) 'nov-highlight-yellow)
               ((eq type 'underline) 'nov-highlight-underline)
               ((eq type 'strikeout) 'nov-highlight-strikeout))))
    (overlay-put overlay 'face face)
    (overlay-put overlay 'nov-highlight t)
    (overlay-put overlay 'nov-highlight-type type)
    (overlay-put overlay 'evaporate t)
    overlay))

(defun nov-highlights--remove-overlays ()
  "Remove all highlight overlays from current buffer."
  (remove-overlays (point-min) (point-max) 'nov-highlight t))

(defun nov-highlights--restore-highlights ()
  "Restore highlights for current chapter."
  (nov-highlights--remove-overlays)
  (let ((book-id (nov-highlights--get-book-id))
        (chapter-id (nov-highlights--get-chapter-id)))
    (when (and book-id chapter-id)
      (let ((book-highlights (gethash book-id nov-highlights-db)))
        (dolist (highlight book-highlights)
          (when (= (plist-get highlight :chapter) chapter-id)
            (condition-case nil
                (nov-highlights--apply-highlight
                 (plist-get highlight :start)
                 (plist-get highlight :end)
                 (plist-get highlight :type))
              (error nil))))))))

(defun nov-highlights--create-highlight (type)
  "Create a highlight of TYPE for selected region."
  (if (use-region-p)
      (let* ((start (region-beginning))
             (end (region-end))
             (text (buffer-substring-no-properties start end))
             (book-id (nov-highlights--get-book-id))
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

(defun nov-highlights--annotation-cancel ()
  "Cancel annotation editing and close the buffer."
  (interactive)
  (let ((annotation-window (selected-window)))
    (when (window-live-p annotation-window)
      (delete-window annotation-window)))
  (message "Annotation cancelled"))

(defun nov-highlights--open-annotation-buffer (initial-text callback-fn quoted-text)
  "Open an annotation buffer with INITIAL-TEXT, calling CALLBACK-FN on commit.
QUOTED-TEXT is shown as context in the header."
  (let* ((buf (get-buffer-create "*Nov Annotation*"))
         (original-buffer (current-buffer))
         (existing-win (get-buffer-window buf))
         win)
    
    ;; Close any existing annotation windows first
    (when (and existing-win (window-live-p existing-win))
      (delete-window existing-win))
    
    ;; Also close the view window if it exists
    (let* ((view-buf (get-buffer "*Nov Annotation View*"))
           (view-win (when view-buf (get-buffer-window view-buf))))
      (when (and view-win (window-live-p view-win))
        (delete-window view-win)))
    
    ;; Create new window
    (setq win (split-window (frame-root-window) 
                            (- (floor (* (window-height (frame-root-window)) 0.25)))
                            'below))
    (set-window-buffer win buf)
    (select-window win)
    
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (org-mode)
        (insert "# Annotation for: " quoted-text "\n")
        (insert "# Press C-c C-c to save, C-c C-k to cancel\n")
        (insert "# ──────────────────────────────────────────────\n")
        (when initial-text
          (insert initial-text))
        
        (setq nov-highlights--annotation-callback callback-fn)
        (setq nov-highlights--annotation-buffer original-buffer)
        
        ;; Set up local keybindings
        (local-set-key (kbd "C-c C-c") 'nov-highlights--annotation-commit)
        (local-set-key (kbd "C-c C-k") 'nov-highlights--annotation-cancel)
        
        ;; Position cursor after header
        (goto-char (point-max))))))

(defun nov-highlights-annotate ()
  "Add annotation to highlighted text at point or selected region."
  (interactive)
  (let* ((book-id (nov-highlights--get-book-id))
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
               (quoted-text (truncate-string-to-width text 60)))
          (deactivate-mark)
          (nov-highlights--open-annotation-buffer
           ""
           (lambda (annotation)
             (nov-highlights--apply-highlight start end 'yellow)
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
             (truncate-string-to-width text 60)))))
      
      (unless highlight-found
        (message "No highlight at point. Select text to create annotated highlight.")))))

(defun nov-highlights-remove-at-point ()
  "Remove highlight at point, including annotation if present. Close annotation window if open."
  (interactive)
  (let* ((book-id (nov-highlights--get-book-id))
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
    (let* ((book-id (nov-highlights--get-book-id))
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
  (let* ((book-id (nov-highlights--get-book-id))
         (chapter-id (nov-highlights--get-chapter-id))
         (pos (point))
         (book-highlights (gethash book-id nov-highlights-db))
         (annotation-found nil))
    
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
                  (insert "# Annotation for: " (truncate-string-to-width text 60) "\n")
                  (insert "# Press 'q' to close\n")
                  (insert "# ────────────────────────────────────────────\n")
                  (insert annotation)
                  (goto-char (point-min))
                  (view-mode 1)
                  (local-set-key (kbd "q") 
                                (lambda () 
                                  (interactive)
                                  (quit-window t))))))))))) 
    
    (unless annotation-found
      (message "No annotation at point")))

(defun nov-highlights--get-annotated-highlights ()
  "Get all highlights with annotations in the current chapter, sorted by position."
  (let* ((book-id (nov-highlights--get-book-id))
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
  "Jump to the next annotation in the current chapter."
  (interactive)
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
      
      ;; If no annotation found after current position, wrap to first
      (unless next-highlight
        (setq next-highlight (car annotated-highlights))
        (message "Wrapped to first annotation"))
      
      (when next-highlight
        (goto-char (plist-get next-highlight :start))
        (nov-highlights-view-annotation)))))

(defun nov-highlights-previous-annotation ()
  "Jump to the previous annotation in the current chapter."
  (interactive)
  (let* ((annotated-highlights (nov-highlights--get-annotated-highlights))
         (current-pos (point))
         (prev-highlight nil))
    
    (if (not annotated-highlights)
        (message "No annotations in current chapter")
      
      ;; Find the last annotation before current position
      (dolist (highlight annotated-highlights)
        (when (< (plist-get highlight :end) current-pos)
          (setq prev-highlight highlight)))
      
      ;; If no annotation found before current position, wrap to last
      (unless prev-highlight
        (setq prev-highlight (car (last annotated-highlights)))
        (message "Wrapped to last annotation"))
      
      (when prev-highlight
        (goto-char (plist-get prev-highlight :start))
        (nov-highlights-view-annotation)))))

(defun nov-highlights-export-to-org ()
  "Export all highlights and annotations to an Org file."
  (interactive)
  (let* ((book-id (nov-highlights--get-book-id))
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
      
      ;; Group highlights by chapter
      (let ((by-chapter (make-hash-table :test 'equal)))
        (dolist (highlight book-highlights)
          (let ((chapter (plist-get highlight :chapter)))
            (puthash chapter 
                    (cons highlight (gethash chapter by-chapter))
                    by-chapter)))
        
        ;; Write to Org file
        (with-temp-file org-file
          (insert (format "#+TITLE: Notes from %s\n" book-name))
          (insert (format "#+DATE: %s\n\n" (format-time-string "%Y-%m-%d")))
          
          (let ((chapters (sort (hash-table-keys by-chapter) #'<)))
            (dolist (chapter chapters)
              (insert (format "* Chapter %d\n\n" (1+ chapter)))
              
              (dolist (highlight (nreverse (gethash chapter by-chapter)))
                (let ((type (plist-get highlight :type))
                      (text (plist-get highlight :text))
                      (annotation (plist-get highlight :annotation)))
                  
                  ;; Format based on type
                  (insert (format "** %s\n" 
                                 (cond
                                  ((eq type 'green) "[GREEN]")
                                  ((eq type 'orange) "[ORANGE]")
                                  ((eq type 'yellow) "[YELLOW/NOTE]")
                                  ((eq type 'underline) "[UNDERLINE]")
                                  ((eq type 'strikeout) "[STRIKEOUT]")
                                  (t "[HIGHLIGHT]"))))
                  
                  (insert "#+BEGIN_QUOTE\n")
                  (insert text)
                  (insert "\n#+END_QUOTE\n")
                  
                  (when annotation
                    (insert "\n*Note:* ")
                    (insert annotation)
                    (insert "\n"))
                  
                  (insert "\n"))))))
        
        (message "Exported to %s" org-file)
        (when (y-or-n-p "Open exported file? ")
          (find-file org-file))))))

(defun nov-highlights-list ()
  "List all highlights in current book."
  (interactive)
  (let* ((book-id (nov-highlights--get-book-id))
         (book-highlights (gethash book-id nov-highlights-db))
         (buf (get-buffer-create "*Nov Highlights*")))
    
    (with-current-buffer buf
      (erase-buffer)
      (insert (format "Highlights for: %s\n\n" 
                     (file-name-nondirectory book-id)))
      
      (if (not book-highlights)
          (insert "No highlights found.\n")
        
        (dolist (highlight (reverse book-highlights))
          (insert (format "Chapter %d | %s | %s\n"
                         (1+ (plist-get highlight :chapter))
                         (upcase (symbol-name (plist-get highlight :type)))
                         (truncate-string-to-width 
                          (plist-get highlight :text) 60)))
          
          (when (plist-get highlight :annotation)
            (insert (format "  → %s\n" 
                           (plist-get highlight :annotation))))
          (insert "\n"))))
    
    (display-buffer buf)))

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
            (define-key map (kbd "h") 'nov-highlights-orange)
            (define-key map (kbd "u") 'nov-highlights-underline)
            (define-key map (kbd "s") 'nov-highlights-strikeout)
            (define-key map (kbd "n") 'nov-highlights-annotate)
            (define-key map (kbd "v") 'nov-highlights-view-annotation)
            (define-key map (kbd "M-r") 'nov-highlights-remove-at-point)
            (define-key map (kbd "D") 'nov-highlights-remove-in-region)
            (define-key map (kbd "e") 'nov-highlights-export-to-org)
            (define-key map (kbd "M-l") 'nov-highlights-list)
            (define-key map (kbd "M-n") 'nov-highlights-next-annotation)
            (define-key map (kbd "M-p") 'nov-highlights-previous-annotation)
            map)
  
  (if nov-highlights-mode
      (nov-highlights-setup)
    (nov-highlights--remove-overlays)))

;; Auto-enable in Nov mode
(add-hook 'nov-mode-hook 'nov-highlights-mode)

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
