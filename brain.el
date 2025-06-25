;;; brain-mode.el --- Brain-Based Code Review -*- lexical-binding: t; -*-

;; Author: Elam Day-Friedland <elamdf@berkeley.edu>
;; Homepage: https://github.com/elamdf/bismuth-cr
;; Keywords: tools, vc
;; Package-Version: 0.1
;; Package-Requires: (
;;     (emacs "26.1")
;;     (magit "2.90.1"))

;;; Commentary:
; This minor mode Magit status. If the current branch ends
; in "brain-$USER", it enables `brain-mode`, which replaces Magit push
; and pull behavior and adds utilities for managing review patches.

;; Code:

;;; Configuration Variables
(require 'transient)

(defvar brain-user (or (getenv "USER") "user")
  "Current user name used in brain branch naming.")

(defvar brain-target-branch "main"
  "Default target branch. Hardcoded for now.")

(defvar brain-remote "origin"
  "Default remote. Hardcoded for now.")

(defvar brain--root
  (file-name-directory (or load-file-name buffer-file-name))
  "Root directory of the current package.")


(defun brain--repo-base-name ()
  "Return the base name of the Git repository without the .git suffix."
  (let* ((remote-url (string-trim (shell-command-to-string "git config --get remote.origin.url")))
         (base-name (string-trim (shell-command-to-string
                                  (format "basename -s .git \"%s\"" remote-url)))))
    base-name))

;;; Branch and Worktree Utilities

(defun brain--current-branch ()
  (magit-get-current-branch))

(defun brain--is-brain-branch-p ()
  (let ((branch (brain--current-branch)))
         (and branch (string-suffix-p (concat "brain-" brain-user) branch))))

(defcustom brain-user(getenv "USER")
  "Username used to determine who is using a brain to review."
  :type 'string
  :group 'brain)

(defcustom brain-base-dir "~/.brains"
  "Username used to determine where brains go by default."
  :type 'string
  :group 'brain)

(defun brain--feature-branch ()
  "Infer the feature branch name by removing the brain suffix."
  (let ((branch (brain--current-branch)))
    (when (and branch (string-match (format "^\\(.*\\)-brain-%s$" brain-user) branch))
      (match-string 1 branch))))

(defun worktree-dir-for-branch (branch)
  "Return the Git worktree directory for BRANCH, or nil if not found."
  (let ((output (shell-command-to-string "git worktree list --porcelain"))
        (case-fold-search nil))
    (with-temp-buffer
      (insert output)
      (goto-char (point-min))
      (let (dir found)
        (while (and (not found)
                    (re-search-forward "^worktree \\(.*\\)$" nil t))
          (setq dir (match-string 1))
          (let ((block-end (or (save-excursion
                                 (if (re-search-forward "^worktree " nil t)
                                     (match-beginning 0)
                                   (point-max)))
                               (point-max))))
            (when (re-search-forward (format "^branch refs/heads/%s$" (regexp-quote branch)) block-end t)
              (setq found dir))))
        found))))

(defun path-relative-to-git-root (file)
  "Return FILE path relative to the Git root."
  (let ((git-root (locate-dominating-file file ".git")))
    (when git-root
      (file-relative-name file git-root))))

;;;; Feature <-> Brain Branch Navigation


(defun brain-toggle-worktree (&optional current)
  "Switch between a feature branch and its brain branch, opening the appropriate worktree."
  (interactive)
  (let* ((magit-display-buffer-function 'magit-display-buffer-same-window-except-diff-v1)
         (current (or current (brain--current-branch)))
         (user brain-user)
         (is-brain (string-match (format "-brain-%s\\'" user) current))
         (base (if is-brain
                   (replace-regexp-in-string (format "-brain-%s" user) "" current)
                 current))
         (target (if is-brain base (format "%s-brain-%s" base user)))
         (dir (worktree-dir-for-branch target)))
    (if (and dir (file-directory-p dir))
        (magit-status dir) ;; if exists, go there
      (if is-brain ;; else offer to create 
          
          (if (y-or-n-p (concat "No feature worktree exists for branch " base ". Create one?"))
              (let (
                    (feat-dir (read-directory-name  "Choose feature worktree dir: " (concat brain-base-dir "/" (brain--repo-base-name) "/" (concat base "-working")) nil t))
                )
                (warn feat-dir)            
            ) nil)
        (if (y-or-n-p (concat "No brain exists for branch " base ". Create one?"))
            (progn (brain-create)
                   (message "brain created!")
                   ;; TODO at what dir
                   )
          nil)        
            
         ))
        t
        ))


(defun brain--goto-feature-version (&rest args)
  "In a brain branch, jump from a Magit hunk to the corresponding line in the feature worktree."
  (interactive)
  (unless (brain--is-brain-branch-p)
    (user-error "This only works in Magit brain buffers"))
  (let* ((feature-branch (brain--feature-branch))
         (feature-dir (and feature-branch (worktree-dir-for-branch feature-branch))))
    (unless feature-dir
      (user-error "Could not determine feature worktree for branch %s" feature-branch))
    (let ((pos (magit-diff-hunk-line (magit-diff-visit--hunk)
                                     (and (magit-diff-visit--hunk)
                                          (magit-diff-visit--goto-from-p (magit-diff-visit--hunk) nil))))
          (file-relative (path-relative-to-git-root (car (car args)))))
      (find-file (expand-file-name file-relative feature-dir))
      (goto-char pos))))

(defun brain--visit-thing-advice (orig-fn &rest args)
  "Intercept `magit-diff-visit-file` to jump to feature version in brain-mode."
  (if (and (bound-and-true-p brain-mode)
           (magit-section-match 'hunk))
      (brain--goto-feature-version args)
    (apply orig-fn args)))

(advice-add 'magit-diff-visit-file :around #'brain--visit-thing-advice)

;;;; Brain Branch Management Commands
(defun brain--call-brain-utils (func feat targ remote brain_user brain_base_dir)
  (call-process (concat brain--root "brain_utils.sh") nil t nil func feat targ remote brain_user brain_base_dir)
  )

(defun brain-create (&optional follow)
  "Create a new brain feature branch. Optionally switch to it"
  (interactive)
  (let* ((feature-branch-inp (magit-read-branch-or-commit "Feature Branch"))
        (target-branch-inp (magit-read-branch-or-commit "Target Branch"))
        (remote (magit-read-remote "Remote"))
     (feature-branch (replace-regexp-in-string (concat remote "/") "" feature-branch-inp))
          (target-branch (replace-regexp-in-string (concat remote "/") "" target-branch-inp)))
    (brain--call-brain-utils "create" feature-branch target-branch remote brain-user brain-base-dir)
    (when follow (brain-toggle-worktree feature-branch-inp))
    ))

(defun brain-update ()
  "Update the current brain branch from its feature/target sources."
  (interactive)
  (let ((default-directory (magit-toplevel)))
      (brain--call-brain-utils "create" (brain--feature-branch) brain-target-branch brain-remote brain-user brain-base-dir))
  (magit-refresh-buffer))

;;;; Magit Integration

(defun brain--add-status-section ()
  "Add a Brain Mode section to the Magit status buffer."
  (when brain-mode
    (let ((inhibit-read-only t))
      (magit-insert-section (brain-mode-status)
        (insert (propertize (format "[Brain Mode Active] Reviewing branch: %s\n"
                                    (brain--feature-branch))
                            'face '(:foreground "cyan" :weight bold)))
        (insert "\n")))))

(defun brain-replace-magit-push-pull ()
  (advice-add #'magit-pull :override #'brain-update))

(defun brain-unreplace-magit-push-pull ()
  (advice-remove #'magit-pull #'brain-update))


;; define a brain prefix
(transient-define-prefix magit-brain (branch)
  "Manipulate brains. mmmm.... brains..."
  [["Brain"
    ("b" "Toggle Brain/Feature"   brain-toggle-worktree)
    ("n" "Create new Brain"      (lambda () (interactive) (brain-create t)))]]
  (interactive (list (magit-get-current-branch)))
  (transient-setup 'magit-brain nil nil :scope branch))

(with-eval-after-load 'magit
  (progn
    (transient-insert-suffix 'magit-branch "b"
      '("g" "Switch brain/feature" brain-toggle-worktree))
    (define-key magit-mode-map (kbd "C-;") #'magit-brain)))

              
  

;;;; Minor Mode Definition

;;;###autoload
(define-minor-mode brain-mode
  "Minor mode for brain-based review branches."
  :lighter " Brain"
  :global nil
  (if brain-mode
      (progn
        (message "brain-mode enabled")
        (brain-replace-magit-push-pull)
        (add-hook 'magit-status-sections-hook #'brain--add-status-section))

    (brain-unreplace-magit-push-pull)
    (remove-hook 'magit-status-sections-hook #'brain--add-status-section)))


;;;###autoload
(defun brain-maybe-enable ()
  "Enable brain-mode if current branch matches brain pattern."
  (if (brain--is-brain-branch-p)
      (brain-mode 1)
    (brain-mode 0)))

(add-hook 'magit-status-mode-hook #'brain-maybe-enable)
(add-hook 'magit-refresh-buffer-hook #'brain-maybe-enable)

(provide 'brain)
;;; brain-mode.el ends here
