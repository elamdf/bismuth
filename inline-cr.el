;;; inline-cr.el --- Lightweight inline code review tools -*- lexical-binding: t; -*-

;; Author: Your Name
;; Version: 0.4
;; Package-Requires: ((emacs "26.1"))
;; Keywords: code-review, navigation, review
;; URL: https://github.com/yourname/inline-cr

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
;;   RET      → jump to end of comment thread and insert "> you: "
;;
;; Command:
;;   M-x inline-cr-find-cr-mentions → list all CR/XCR lines involving you

;;; Code:

(defgroup inline-cr nil
  "Inline code review utilities."
  :group 'tools)

(defcustom inline-cr-user (getenv "USER")
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


(defun inline-cr--font-lock-keywords ()
  "Font-lock rules for CR/XCR/reply faces only."
  (list
   '("^> CR \\([^ ]+\\) for \\([^:]+\\):.*$" . 'inline-cr-author-face)
   '("^> XCR \\([^ ]+\\) for \\([^:]+\\):.*$" . 'inline-cr-reviewer-face)
   '("^> [^:]+:.*$" . 'inline-cr-reply-face)))

(defun inline-cr--scan-for-actionables (start end)
  "Apply `inline-cr-actionable` text properties between START and END."
  (save-excursion
    (goto-char start)
    (let ((case-fold-search nil))
      (while (re-search-forward "^> \\(C\\|XC\\)R \\([^ ]+\\) for \\([^:]+\\):" end t)
        (let ((who (match-string 2))
              (whom (match-string 3))
              (beg (match-beginning 0))
              (end (match-end 0)))
          (when (or
                 (and (string= (match-string 1) "CR")
                      (string= whom inline-cr-user))
                 (and (string= (match-string 1) "XCR")
                      (string= who inline-cr-user)))
            (put-text-property beg end 'inline-cr-actionable t)))))))



(defun inline-cr--goto-actionable (direction)
  "Move to next/previous actionable CR/XCR comment for `inline-cr-user`, wrapping if needed."
  (let* ((step (if (eq direction 'next) 1 -1))
         (search-fn (if (eq direction 'next) #'re-search-forward #'re-search-backward))
         (restart (if (eq direction 'next) (point-min) (point-max)))
         (limit (if (eq direction 'next) (point-max) (point-min)))
         (user (regexp-quote inline-cr-user))
         (patterns (list (concat "^> CR .* for " user ":")
                         (concat "^> XCR " user " for .*:")))
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
    (looking-at "^> \\(C\\|XC\\)R ")))

(defun inline-cr-jump-to-thread-end ()
  "Smart RET: If on CR/XCR header, jump to end and insert reply.
If already inside a thread, insert newline prefixed with '> '.
Otherwise, insert plain newline."
  (interactive)
  (cond
   ;; Case 1: On thread header → jump to end and insert "> user: "
   ((inline-cr--at-thread-header-p)
    (let ((user inline-cr-user))
      (forward-line 1)
      (while (and (not (eobp)) (looking-at "^> ")) (forward-line 1))
      (insert (format "> %s: " user))
      (recenter)))

   ;; Case 2: Inside thread → insert new line with "> "
   ((save-excursion
      (beginning-of-line)
      (looking-at "^> "))
    (end-of-line)
    (insert "\n> "))

   ;; Case 3: Else → insert regular newline
   (t
    (call-interactively #'newline))))



(defun inline-cr-toggle-cr-xcr ()
  "Toggle the CR/XCR tag at the start of the current review thread."
  (interactive)
  (save-excursion
    ;; Move upward until not a >-prefixed line, or we hit BOF
    (while (and (not (bobp))
                (save-excursion
                  (forward-line -1)
                  (looking-at "^> ")))
      (forward-line -1))
    (beginning-of-line)
    (cond
     ((looking-at "^> CR ") (replace-match "> XCR "))
     ((looking-at "^> XCR ") (replace-match "> CR "))
     (t (user-error "Not inside a CR/XCR comment thread")))))



(defvar inline-cr-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-n") #'inline-cr-next-actionable)
    (define-key map (kbd "C-c C-p") #'inline-cr-prev-actionable)
    (define-key map (kbd "C-c RET") #'inline-cr-toggle-cr-xcr)
    (define-key map (kbd "RET") #'inline-cr-jump-to-thread-end)
    map)
  "Keymap for inline-cr-mode.")

;;;###autoload
(define-minor-mode inline-cr-mode
  "Minor mode to highlight and navigate inline code review threads."
  :lighter " CR"
  :keymap inline-cr-mode-map
  (if inline-cr-mode
      (progn
        (font-lock-add-keywords nil (inline-cr--font-lock-keywords))
        (jit-lock-register #'inline-cr--scan-for-actionables)
        (font-lock-flush)
        (font-lock-ensure))
    (font-lock-remove-keywords nil (inline-cr--font-lock-keywords))
    (jit-lock-unregister #'inline-cr--scan-for-actionables)
    (remove-text-properties (point-min) (point-max) '(inline-cr-actionable nil))
    (font-lock-flush)))



;;;###autoload
(defun inline-cr-enable-in-reviewable-modes ()
  "Enable inline-cr-mode in markdown and org files."
  (when (derived-mode-p 'markdown-mode 'org-mode)
    (inline-cr-mode)))

;;;###autoload
(add-hook 'markdown-mode-hook #'inline-cr-enable-in-reviewable-modes)
;;;###autoload
(add-hook 'org-mode-hook #'inline-cr-enable-in-reviewable-modes)

;;;###autoload
(defun inline-cr-find-cr-mentions ()
  "Open a buffer with clickable links to CR/XCR comments involving you."
  (interactive)
  (let* ((user (regexp-quote inline-cr-user))
         (patterns (list (concat "^> CR .* for " user ":")
                         (concat "^> XCR " user " for .*:")))
         (source-buf (current-buffer))
         (report-buf (get-buffer-create "*CR Mentions*")))
    (with-current-buffer report-buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (format "Mentions of %s in: %s\n\n" inline-cr-user (buffer-name source-buf)))
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

(provide 'inline-cr)

;;; inline-cr.el ends here


