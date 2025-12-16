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
;;   M-n  → next actionable comment (wraps)
;;   M-p  → previous actionable comment (wraps)
;;   C-c RET  → toggle CR/XCR at point
;;   C-c t  → list all CR/XCR lines involving youa
;;   RET      → jump to end of comment thread and insert "> you: "
;;


;;; Code:

(require 'tabulated-list)
(require 'outline)
(require 'projectile)
(require 'cl-lib)
(require 'subr-x)
(require 'magit-section)

(defgroup inline-cr nil
  "Inline code review utilities."
  :group 'tools)

(defun inline-cr-header-regex ()
  "Regex to match the reviewer and author of an [X]CR header."
  (format
   "^\s*> \\(\\(N\\)?\\(X\\)?CR\\) \\([^ ]+\\) for \\([^:]+\\):.*\\(?:\n>\\s-*$\\)?"))
(defun inline-cr-thread-regex ()
  "Regex to match non-header lines of an inline CR, optionally capturing an author name."
  (format "^\s*>\s*\\(\\(\\S-*\\):\\)?.*"))


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
  '((t :weight bold :foreground "black"))
  "Face for actionable CR/XCR lines.")

(defface inline-cr-nonactionable-face
  '((t :foreground "gray50"))
  "Face for non-actionable CR/XCR lines.")

(defface inline-cr-inactive-block-face
  '((t (:foreground "black" :background "#E3E3E3")))
  "Face for inactive (N-prefixed) inline code review blocks.")

(defface inline-cr-block-face
  '((t (:foreground "black" :background "#D9FFE2")))
  "Face for non-actionable inline code review blocks.")

(defface inline-cr-actionable-block-face
  '((t (:foreground "black" :background "#F4B9D7")))

  "Face for actionable inline code review blocks.")


;; > CR elamdf for elamdf: fix inline crs for non-markdown source files (i.e. allow comment prefix for > CR
;; cont
(defun inline-cr--scan-for-actionables (start end)
  "Apply `inline-cr-actionable` text property to actionable CR/XCR lines between START and END."
  (save-excursion
    (goto-char start)

    (let ((case-fold-search nil))

      (while (re-search-forward (inline-cr-header-regex) nil t)
        (let* ((kind (match-string 1)) ;; "CR" or "XCR"
               (who (match-string 4))
               (whom (match-string 5))
               (beg (match-beginning 0))
               (end (match-end 0)))
          (progn (if (or
                      (and (string= kind "CR")
                           (string= whom inline-cr-user))
                      (and (string= kind "XCR")
                           (string= who inline-cr-user)))
                     ( put-text-property beg end 'inline-cr-actionable t)
                   (put-text-property beg end 'inline-cr-actionable nil)
                   )
                 (if (not (or (string= kind "NXCR") (string= kind "NCR")))
                     (put-text-property beg end 'inline-cr-active t)
                   (put-text-property beg end 'inline-cr-active nil)))
          )))))



(defun inline-cr--goto-actionable (direction)
  "Move to next/previous actionable CR/XCR comment for `inline-cr-user`, wrapping if needed."
  (let* ((step (if (eq direction 'next) 1 -1))
         (search-fn
          (if (eq direction 'next) #'re-search-forward
            #'re-search-backward))
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
      (message "No %s actionable comments for %s."
               (symbol-name direction) inline-cr-user))
    found))


(defun inline-cr-next-actionable ()
  "Jump to next CR/XCR comment requiring your response."
  (interactive)
  (inline-cr--goto-actionable 'next))

(defun inline-cr-prev-actionable ()
  "Jump to previous CR/XCR comment requiring your response."
  (interactive)
  (inline-cr--goto-actionable 'prev))

(defun inline-cr--at-thread-header-p  ()
  "Return t if point is on a CR or XCR header line."
  (save-excursion
    (beginning-of-line)
    (looking-at (inline-cr-header-regex))))


(defun inline-cr--at-thread-body-p ()
  "Return t if point is on a CR or XCR header line."
  (save-excursion
    (beginning-of-line)
    (looking-at (inline-cr-thread-regex))))


(defun inline-cr-insert-review-comment ()
  (goto-char (line-end-position))
  (insert (format "\n> CR %s for " inline-cr-user))

  )

(defun inline-cr-maybe-extend-thread ()
  "Smart RET within an inline CR thread.

- If point is on a thread header, jump to end and insert a reply.
- If already in a thread, insert '> ' or '> user:'.
- Only prefix with '> user:' if the last authored line was not by `inline-cr-user`.
- Otherwise insert '> '.
- Outside a thread, insert a normal newline."
  (interactive)
  (let ((user inline-cr-user)
        (prefix "")
        (last-author nil)
        (thread-bounds (inline-cr--thread-boundaries))
        )

    (cond
     (thread-bounds
      (progn
        ;; (save-excursion
        (goto-char (car thread-bounds))
        (forward-line 1)
        (while
            (and (not (eobp)) (looking-at (inline-cr-thread-regex)))
          (when (match-string 2)
            (setq last-author (match-string 2)))
          (forward-line 1))
        (forward-line -1)


        (goto-char (line-end-position))
        (insert (if (not (string= last-author user))
                    (format "\n%s> %s: " prefix user)
                  (format "\n%s> " prefix)))
        (goto-char (line-end-position))
        ))

     (t
      (call-interactively #'newline)))))


(defun inline-cr-maybe-toggle-cr-xcr ()
  "Toggle the CR/XCR tag at the start of the current review thread."
  (interactive)
  (if
      (or (inline-cr--at-thread-header-p)
          (inline-cr--at-thread-body-p))
      (save-excursion
        ;; Move upward until not a >-prefixed line, or we hit BOF
        (while (and (not (bobp))
                    (save-excursion
                      (forward-line -1)
                      (looking-at (inline-cr-thread-regex))))
          (forward-line -1))
        (beginning-of-line)
        (cond
         ((looking-at "^\s*> CR ")
          (replace-match ( format "> XCR ") t t))
         ((looking-at "^\s*> XCR ")
          (replace-match ( format "> CR ") t t))
         ())
        (inline-cr--refresh-display))))


(defun inline-cr--back-to-header ()
  (goto-char (car  (inline-cr--thread-boundaries))))

(defun inline-cr--end-of-header ()
  (progn (inline-cr--back-to-header) (end-of-line)))

(defun inline-cr--end-of-thread ()
  (goto-char (cdr (inline-cr--thread-boundaries))))

(defun inline-cr--thread-boundaries ()
  "Return (START . END) of current CR/XCR thread, or nil."
  (save-excursion
    (goto-char (line-beginning-position))
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




;; TODO (elamdf) this should do markdown formatting even in non-markdown files
(defun inline-cr--highlight-thread (start end)
  "Apply background face to CR/XCR thread from START to END.
If the head has `inline-cr-actionable` property, use the actionable face."
  (save-excursion
    (goto-char start)
    (while (re-search-forward (inline-cr-header-regex) end t)
      (let* ((head-start (if (save-excursion
                               (beginning-of-line)
                               (looking-at "^>\\s-*$"))
                             (save-excursion
                               (forward-line -1)
                               (line-beginning-position))
                           (line-beginning-position)))
             (head-end (line-end-position))
             (actionable
              (get-text-property head-start 'inline-cr-actionable))
             (active
              (get-text-property head-start 'inline-cr-active))
             (block (if active (if actionable
                                   'inline-cr-actionable-block-face
                                 'inline-cr-block-face)
                       'inline-cr-inactive-block-face))
             (header (if actionable
                         'inline-cr-actionable-face
                       'inline-cr-nonactionable-face))
             )
        ;; Highlight head line, but with a lower priority than the actionable highlight
        (let ((ov (make-overlay head-start head-end)))
          (overlay-put ov 'face header)
          (overlay-put ov 'priority 1)
          (overlay-put ov 'inline-cr t))
        ;; for some reason if we try to fold this into the highlight thread body code, it doesn't work
        ;; > CR elamdf for elamdf: dedup this code section

        (let ((ov (make-overlay head-start head-end)))
          (overlay-put ov 'face block)
          (overlay-put ov 'priority 2)
          (overlay-put ov 'inline-cr t))
        ;; Highlight thread body
        (forward-line 1)
        (while (and (not (eobp)) (looking-at "^.*> "))
          (let
              ((ov
                (make-overlay (line-beginning-position)
                              (line-end-position))))
            (overlay-put ov 'face block)
            (overlay-put ov 'priority 2)

            (overlay-put ov 'inline-cr t))
          (forward-line 1))
        ))))

(defun inline-cr--refresh-display ()
  "Refresh visual display of inline CRs."
  ;; TODO this may be overkill, but I don't really understand how the jit stuff works
  (inline-cr--scan-for-actionables (point-min) (point-max))
  (remove-overlays (point-min) (point-max) 'inline-cr t)
  ;; (inline-cr--apply-actionable-overlay (point-min) (point-max))
  (inline-cr--highlight-thread (point-min) (point-max)))

(defun inline-cr--cleanup ()
  (remove-overlays (point-min) (point-max) 'inline-cr t)
  )

;; kbd map
(defvar inline-cr-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "M-n") #'inline-cr-next-actionable)
    (define-key map (kbd "M-p") #'inline-cr-prev-actionable)
    (define-key map (kbd "C-c t") #'inline-cr-find-cr-mentions)
    (define-key map (kbd "C-c T") #'inline-cr-find-project-cr-mentions)

    (define-key map (kbd "C-c RET") #'inline-cr-maybe-toggle-cr-xcr)
    (define-key map (kbd "RET") #'inline-cr-maybe-extend-thread)
    (define-key map (kbd "C-RET") #'inline-cr-insert-review-comment)
    (define-key map (kbd "C-RET") #'inline-cr-insert-review-comment)
    (define-key map (kbd "<TAB>") #' inline-cr-maybe-toggle-children)


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


;; TODO make <cr expand to > CR $user for (fill out):
;; todo make <td expand to >CR $user for $user:
;; asd
;; asd

;; we want a top of thread, end of heading, and end of thread functions
(defun inline-cr--hide-thread ()
  "Hide a thread, foldering after the header line"
  (outline-flag-region
   (progn (inline-cr--end-of-header) (point))
   (progn (inline-cr--end-of-thread) (point))
   t
   )
  )
(defun inline-cr--show-thread ()
  "Hide a thread, foldering after the header line"
  (outline-flag-region
   (progn (inline-cr--end-of-header) (point))
   (progn (inline-cr--end-of-thread) (point))
   nil
   )
  )

(defun inline-cr--toggle-children ()
  "Show or hide the current subtree depending on its current state."
  (interactive)
  (save-excursion
    (inline-cr--back-to-header)
    (if (not (outline-invisible-p (pos-eol)))
        (inline-cr--hide-thread)
      (inline-cr--show-thread)
      )))
(defun inline-cr-maybe-toggle-children ()
  (interactive)
  (if (inline-cr--at-thread-header-p)
      (inline-cr--toggle-children)
    (call-interactively (local-key-binding (kbd "TAB")))
    ))

(provide 'inline-cr)

;;; inline-cr.el ends here
