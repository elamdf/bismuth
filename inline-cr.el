;;; inline-cr.el --- Lightweight inline code review tools -*- lexical-binding: t; -*-

;; Author: Elam Day-Friedland <elamdf@berkeley.edu>
;; Version: 0.4
;; Package-Requires: ((emacs "26.1"))
;; Keywords: code-review, navigation, review
;; URL: https://github.com/elamdf/bismuth

;;; Commentary:

;; Highlights and navigates inline code review threads in org or markdown.
;; Syntax:
;;   > CR reviewer for author: comment  ;; author must respond
;;   > XCR reviewer for author: comment ;; reviewer must respond
;;   > name: comment                    ;; reply

;; Keybindings:
;;   C-c C-n  → next actionable comment (wraps)
;;   C-c C-p  → previous actionable comment (wraps)
;;   C-c RET  → toggle CR/XCR at point
;;   C-c t  → list all CR/XCR lines involving youa
;;   RET      → jump to end of comment thread and insert "> you: "
;;


;;; Code:

(require 'tabulated-list)

(defgroup inline-cr nil
  "Inline code review utilities."
  :group 'tools)

(defun inline-cr-header-regex ()
  "Regex to match the reviewer and author of an [X]CR header."
  (format  "^\s*%s\s*> \\(\\(X?\\)?CR\\) \\([^ ]+\\) for \\([^:]+\\):.*" (or comment-start "")))
(defun inline-cr-thread-regex ()
  "Regex to match non-header lines of an inline CR, optionally capturing an author name."
  (format "^\s*%s\s*>\s*\\(\\(\\S-*\\):\\)?.*" (or comment-start "")))

;; TODO C-RET to make a cr. TODO figure out author smartly

(defcustom inline-cr-user(getenv "USER")
  "Username used to determine which comments require your response."
  :type 'string
  :group 'inline-cr)

(defface inline-cr-author-face
  '((t :foreground "#fabd2f" :weight bold))
  "Face for CRs where author (you) must respond."
  :group 'inline-cr)

(defface inline-cr-reviewer-face
  '((t :foreground "#d3869b" :weight bold))
  "Face for XCRs where reviewer (you) must respond."
  :group 'inline-cr)

(defface inline-cr-reply-face
  '((t :foreground "#83a598" :slant italic))
  "Face for reply lines."
  :group 'inline-cr)

(defface inline-cr-actionable-face
  '((t :weight bold :foreground "orange"))
  "Face for actionable CR/XCR lines.")

(defface inline-cr-nonactionable-face
  '((t :foreground "gray50"))
  "Face for non-actionable CR/XCR lines.")


(defface inline-cr-block-face
  '((t (:foreground "black" :background "#D9FFE2")))
  "Face for non-actionable inline code review blocks.")

(defface inline-cr-actionable-block-face
  '((t (:foreground "black" :background "#F4B9D7")))
  "Face for actionable inline Scode review blocks.")


(defun inline-cr--apply-actionable-overlay (start limit)
  "Search for CR/XCR lines up to LIMIT, and apply face based on `inline-cr-actionable` property."
  (save-excursion
    (goto-char start)  
    (while (re-search-forward (inline-cr-header-regex) nil t)
      (let ((start (match-beginning 0))
            (end (match-end 0)))
        (when (get-text-property start 'inline-cr-actionable)
          (let ((ov (make-overlay start end)))
            (overlay-put ov 'face 'inline-cr-actionable-face)
            (overlay-put ov 'inline-cr t)))
        (unless (get-text-property start 'inline-cr-actionable)
          (let ((ov (make-overlay start end)))
            (overlay-put ov 'face 'inline-cr-nonactionable-face)
            (overlay-put ov 'inline-cr t))))
      t)))



(defun inline-cr--virtual-org-buffer ()
  "Return a buffer with actionable CRs as TODO entries."
  (let ((buf (get-buffer-create "*inline-cr-org-agenda*")))
    (with-current-buffer buf
      (erase-buffer)
      (org-mode)
      (let ((mentions (inline-cr--collect-cr-mentions)))
        (dolist (entry mentions)
          (let* ((file (file-name-nondirectory (car entry)))
                 (line (cadr entry))
                 (text (string-trim (caddr entry)))
                 (link (format "[[file:%s::%d]]" file line)))
            (insert (format "* TODO %s\n  %s\n\n" text link)))))
      buf)))


(defun inline-cr--scan-for-actionables (start end)
  "Apply `inline-cr-actionable` text property to actionable CR/XCR lines between START and END."

  
  (save-excursion
    (goto-char start)

    (let ((case-fold-search nil))

      (while (re-search-forward (inline-cr-header-regex) nil t)
        (let* ((kind (match-string 1)) ;; "CR" or "XCR"
               (who (match-string 3))
               (whom (match-string 4))
               (beg (match-beginning 0))
               (end (match-end 0)))
          (if (or
               (and (string= kind "CR")
                    (string= whom inline-cr-user))
               (and (string= kind "XCR")
                    (string= who inline-cr-user)))xo
              (put-text-property beg end 'inline-cr-actionable t)
            (put-text-property beg end 'inline-cr-actionable nil)))))))



(defun inline-cr--goto-actionable (direction)
  "Move to next/previous actionable CR/XCR comment for `inline-cr-user`, wrapping if needed."
  (let* ((step (if (eq direction 'next) 1 -1))
         (search-fn (if (eq direction 'next) #'re-search-forward #'re-search-backward))
         (restart (if (eq direction 'next) (point-min) (point-max)))
         (limit (if (eq direction 'next) (point-max) (point-min)))
         (user (regexp-quote inline-cr-user))
         (patterns (list (concat "^.*> CR .* for " user ":")
                         (concat "^.*> XCR " user " for .*:")))
         (found nil))
    ;; Search forward or backward
    (save-excursion
      (forward-line step)
      (while (and (not found)
                  (or (funcall search-fn (car patterns) limit t)
                      (funcall search-fn (cadr patterns) limit t)))
        (setq found (line-beginning-position))))
    ;; Wrap around if not found
    (unless found
      (save-excursion
        (goto-char restart)
        (while (and (not found)
                    (or (funcall search-fn (car patterns) limit t)
                        (funcall search-fn (cadr patterns) limit t)))
          (setq found (line-beginning-position)))))
    (if found
        (goto-char found)
      (message "No %s actionable comments for %s." (symbol-name direction) inline-cr-user))
    found))


(defun inline-cr-next-actionable ()
  "Jump to next CR/XCR comment requiring your response."
  (interactive)
  (inline-cr--goto-actionable 'next))

(defun inline-cr-prev-actionable ()
  "Jump to previous CR/XCR comment requiring your response."
  (interactive)
  (inline-cr--goto-actionable 'prev))

(defun inline-cr--at-thread-header-p ()
  "Return t if point is on a CR or XCR header line."
  (save-excursion
    (beginning-of-line)
    (looking-at (inline-cr-header-regex))))


(defun inline-cr--at-thread-body-p ()
  "Return t if point is on a CR or XCR header line."
  (save-excursion
    (beginning-of-line)
    (looking-at (inline-cr-thread-regex))))


(defun inline-cr-maybe-insert-within-thread ()
  "Smart RET within an inline CR thread.

- If point is on a thread header, jump to end and insert a reply.
- If already in a thread, insert '> ' or '> user:'.
- Only prefix with '> user:' if the last authored line was not by `inline-cr-user`.
- Otherwise insert '> '.
- Outside a thread, insert a normal newline."
  (interactive)
  (let ((user inline-cr-user)
        (prefix (or comment-start ""))
        (last-author nil))
    (cond
     ;; Case 1: On header → go to end of thread and decide whether to prefix user
     ( (or (inline-cr--at-thread-header-p) (inline-cr--at-thread-body-p))

      ;; (save-excursion
        (forward-line 1)
        (while (and (not (eobp)) (looking-at (inline-cr-thread-regex)))
      (when (match-string 2)
        (setq last-author (match-string 2)))
      (forward-line 1))
      (forward-line -1)


      (goto-char (line-end-position))      
      (insert (if (and last-author (not (string= last-author user)))
                  (format "\n%s> %s: " prefix user)
                (format "\n%s> " prefix)))
      (goto-char (line-end-position))
      )

     ;; Case 3: Fallback
     (t
      (call-interactively #'newline)))))








(defun inline-cr-toggle-cr-xcr ()
  "Toggle the CR/XCR tag at the start of the current review thread."
  (interactive)
  (save-excursion
    ;; Move upward until not a >-prefixed line, or we hit BOF
    (while (and (not (bobp))
                (save-excursion
                  (forward-line -1)
                  (looking-at (inline-cr-thread-regex))))
      (forward-line -1))
    (beginning-of-line)
    (cond
     ((looking-at "^.*> CR ") (replace-match ( format "%s> XCR " (or comment-start ""))))
     ((looking-at "^.*> XCR ") (replace-match ( format "%s> CR " (or comment-start ""))))
     (start (user-error "Not inside a CR/XCR comment thread")))
  (inline-cr--refresh-display)))


;;;###autoload
;; TODO replace with (defun inline-cr-list-all-file-mentions () that uses inline--cr-mention-mode
(defun inline-cr-find-cr-mentions ()
  "Open a buffer with clickable links to CR/XCR comments involving you."
  (interactive)
  (let* ((user (regexp-quote inline-cr-user))
         (patterns (list (concat "^.*> CR .* for " user ":")
                         (concat "^.*> XCR " user " for .*:")))
         (source-buf (current-buffer))
         (report-buf (get-buffer-create  (format "*%s CR Actionables*" (buffer-name source-buf)))))
    (with-current-buffer report-buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        ;; (insert (format "Actionables for %s in: %s\n\n" inline-cr-user (buffer-name source-buf)))
        (with-current-buffer source-buf
          (save-excursion
            (goto-char (point-min))
            (dolist (pat patterns)
              (goto-char (point-min))
              (while (re-search-forward pat nil t)
                (let ((line (buffer-substring-no-properties
                             (line-beginning-position) (line-end-position)))
                      (lineno (line-number-at-pos))
                      (pos (point)))
                  (with-current-buffer report-buf
                    (insert-button
                     (format "Line %d: %s\n" lineno line)
                     'action (lambda (_)
                               (pop-to-buffer source-buf)
                               (goto-char pos)
                               (recenter))
                     'follow-link t))))))))
      (goto-char (point-min))
      (read-only-mode 1))
    (pop-to-buffer report-buf)))

(defun inline-cr--thread-boundaries ()
  "Return (START . END) of current CR/XCR thread, or nil."
  (save-excursion
    (let (start end)
      ;; Move to top of thread
      (while (and (not (bobp))
                  (save-excursion
                    (forward-line -1)
                    (looking-at (inline-cr-thread-regex))))
        (forward-line -1))
      (when (looking-at (inline-cr-header-regex))
        (setq start (point))
        ;; Move to end of thread
        (while (and (not (eobp))
                    (progn
                      (forward-line 1)
                      (looking-at (inline-cr-thread-regex)))))
        (setq end (point))
        (cons start end)))))



(defvar inline-cr--mention-buffer "*Inline CR Mentions*")

(defun inline-cr--collect-cr-mentions ()
  "Return a list of (FILE LINE-NUM TEXT) for actionable CR/XCR threads in .org and .md files."
  (unless (projectile-project-p)
    (error "Not in a projectile project"))
  (let* ((root (projectile-project-root))
         (files (cl-remove-if-not
                 (lambda (f)
                   (string-match-p "\\.\\(org\\|md\\)\\'" f))
                 (projectile-current-project-files)))
         (results '()))
    (dolist (file files)
      (let ((full-path (expand-file-name file root)))
        (when (file-readable-p full-path)
          (with-temp-buffer
            (message "inline-cr: checking %s as user %s" full-path inline-cr-user)
            
            (insert-file-contents full-path)
            (delay-mode-hooks (normal-mode t))
            ;; Annotate actionables
            (inline-cr--scan-for-actionables (point-min) (point-max))
            ;; Scan for actionable lines
            (goto-char (point-min))
            (let ((line-num 1))
              (while (not (eobp))
                (when (get-text-property (point) 'inline-cr-actionable)
                  (let ((line (buffer-substring-no-properties
                               (line-beginning-position) (line-end-position))))
                    (push (list full-path line-num line) results)))
                (forward-line 1)
                (cl-incf line-num)))))))
    (nreverse results)))

(defun inline-cr--agenda-source ()
  "Generate Org agenda items for actionable inline CR/XCRs."
  (let ((items '()))
    (dolist (entry (inline-cr--collect-cr-mentions))
      (let* ((file (car entry))
             (line (cadr entry))
             (text (cl-caddr entry))
             (marker (with-current-buffer (find-file-noselect file)
                       (save-excursion
                         (goto-char (point-min))
                         (forward-line (1- line))
                         (point-marker))))
             (heading (format "[inline-cr] %s:%d %s"
                              (file-name-nondirectory file) line text)))
        (push (list heading marker nil) items)))
    (nreverse items)))



(defun inline-cr--mention-mode ()
  "Major mode for displaying inline CR mentions."
  (kill-all-local-variables)
  (setq major-mode 'inline-cr--mention-mode)
  (setq mode-name "Inline CR Mentions")
  (use-local-map tabulated-list-mode-map)
  (setq tabulated-list-format [("File" 40 t)
                               ("Line" 6 t)
                               ("Text" 0 nil)])
  (setq tabulated-list-padding 2)
  (setq tabulated-list-entries
        (mapcar (lambda (entry)
                  (let ((file (file-name-nondirectory (car entry)))
                        (line (cadr entry))
                        (text (cl-caddr entry)))
                    ;; Store file and line in the entry ID for lookup
                    (list (cons file line)
                          (vector
                           (file-relative-name file (projectile-project-root))
                           (number-to-string line)
                           text))))
                (inline-cr--collect-cr-mentions)))
  (setq tabulated-list-sort-key (cons "File" nil))
  (add-hook 'tabulated-list-revert-hook #'inline-cr-list-all-project-mentions nil t)

  ;; Add RET handler
  (define-key tabulated-list-mode-map (kbd "RET") #'inline-cr--visit-entry)

  (tabulated-list-init-header)
  (tabulated-list-print))

(defun inline-cr--visit-entry ()
  "Visit the file and line at point in the CR/XCR list."
  (interactive)
  (let* ((id (tabulated-list-get-id)) ;; ID is a (file . line) pair
         (file (car id))
         (line (cdr id)))
    (find-file file)
    (goto-char (point-min))
    (forward-line (1- line))))


(defun inline-cr-list-all-project-mentions ()
  "Display a list of all CR/XCR mentions in a project in a clickable buffer."
  (interactive)
  (let ((buf (get-buffer-create (format "* %s CR Actionables*" (projectile-project-name)))))
    (with-current-buffer buf
      (inline-cr--mention-mode))
    (pop-to-buffer buf)))







;; TODO (elamdf) this should do markdown formatting even in non-markdown files
(defun inline-cr--highlight-thread (start end)
  "Apply background face to CR/XCR thread from START to END.
If the head has `inline-cr-actionable` property, use the actionable face."
  (save-excursion
    (goto-char start)
    (while (re-search-forward (inline-cr-header-regex) end t)
      (let* ((head-start (line-beginning-position))
             (head-end (line-end-position))
             ;; Check for actionable property on any character in the head
             (actionable (get-text-property head-start 'inline-cr-actionable))
             (face (if actionable
                       'inline-cr-actionable-block-face
                     'inline-cr-block-face)))
        ;; Highlight head line
        (let ((ov (make-overlay head-start head-end)))
          (overlay-put ov 'face face)
          (overlay-put ov 'inline-cr t))
        ;; Highlight thread body
        (forward-line 1)
        (while (and (not (eobp)) (looking-at "^.*> "))
          (let ((ov (make-overlay (line-beginning-position) (line-end-position))))
            (overlay-put ov 'face face)
            (overlay-put ov 'inline-cr t))
          (forward-line 1))))))




(defun inline-cr--refresh-display ()
  "Refresh visual display of inline CRs."
  ;; TODO this may be overkill, but I don't really understand how the jit stuff works
  (inline-cr--scan-for-actionables (point-min) (point-max))  
  (remove-overlays (point-min) (point-max) 'inline-cr t)
  (inline-cr--apply-actionable-overlay (point-min) (point-max))
  (inline-cr--highlight-thread (point-min) (point-max)))

(defun inline-cr--cleanup ()
  (remove-overlays (point-min) (point-max) 'inline-cr t)  
  )

;; kbd map
(defvar inline-cr-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-n") #'inline-cr-next-actionable)
    (define-key map (kbd "C-c C-p") #'inline-cr-prev-actionable)
    (define-key map (kbd "C-c t") #'inline-cr-find-cr-mentions)
    (define-key map (kbd "C-c T") #'inline-cr-list-all-project-mentions)    
    (define-key map (kbd "C-c RET") #'inline-cr-toggle-cr-xcr)
    (define-key map (kbd "RET") #'inline-cr-maybe-insert-within-thread)
    map)
  "Keymap for inline-cr-mode.")

(defun inline-cr--refresh-display-rest (&rest _)
  (inline-cr--refresh-display))

;;;###autoload
(define-minor-mode inline-cr-mode
  "Minor mode to highlight and navigate inline code review threads."
  :lighter " CR"
  :keymap inline-cr-mode-map
  (if inline-cr-mode
      (progn
        (jit-lock-register #'inline-cr--scan-for-actionables)
        (add-hook 'after-change-functions
                  #'inline-cr--refresh-display-rest)
        (inline-cr--refresh-display)
        )
    (progn 
      (jit-lock-unregister #'inline-cr--scan-for-actionables)
        (remove-hook 'after-change-functions
                     #'inline-cr--refresh-display-rest)

        (inline-cr--cleanup)
        )
      ))


(provide 'inline-cr)

;;; inline-cr.el ends here




