;;; bismuth-magit.el --- minimal Magit section for CR/XCR actionables -*- lexical-binding: t; -*-
(require 'cl-lib)

(require 'magit)
(require 'magit-section)
(require 'json)
(require 'subr-x)
(require 'compile)
(require 'inline-cr)

(defgroup bismuth-magit nil
  "Show CR/XCR actionables from bismuth in Magit status."
  :group 'magit)


;; > CR elamdf for elamdf: fix hardcoded path
(defcustom bismuth-magit-binary (concat user-emacs-directory "/bismuth/parser/target/release/crscan")
  "Path to the crscan executable."
  :type 'string
  :group 'bismuth-magit)


(defcustom bismuth-magit-args '("--ext" "el" "--ext" "rs" "--ext" "toml" "--ext" "md" "--ext" "org" "--ext" "markdown" "--ext" "txt"  "--ext" "rust")

  "Args passed to bismuth when scanning a repo."
  :type '(repeat string)
  :group 'bismuth-magit)


(defcustom inline-cr--bismuth-dir (concat user-emacs-directory "/bismuth/")
  "Path to the bismuth directory."
  :type 'string
  :group 'inline-cr)

;; > CR elamdf for elamdf: fix hardcoded path
(defcustom inline-cr-gh-sync-script "scripts/inline-cr-gh-sync.py"
  "Path to the gh sync script executable, relative to bismuth."
  :type 'string
  :group 'inline-cr)

(defun bismuth-magit--repo-root ()
  (or (magit-toplevel) (user-error "Not in a Git repository")))

(defun bismuth-magit--pull-inline-cr-for-pullreq (pullreq)
  "Pull inline-cr comments for PULLREQ without prompting."
  (condition-case err
      (let* ((pr (forge-get-pullreq pullreq))
             (number (oref pr number)))
        (when number
          (inline-cr-sync-gh number "pull" nil)))
    (error (message "inline-cr: pull after checkout failed: %s" err))))

(defun bismuth-magit--current-pr-number ()
  "Return the current Forge PR number, or nil when unavailable."
  (when (fboundp 'forge-current-pullreq)
    (condition-case nil
        (let ((pr (forge-current-pullreq)))
          (and pr (oref pr number)))
      (error nil))))

(defun bismuth-magit--wrap-push-sentinel (process pr-number)
  "Attach a sentinel to PROCESS to push inline-cr after a successful push."
  (let ((orig-sentinel (process-sentinel process)))
    (set-process-sentinel
     process
     (lambda (proc event)
       (when orig-sentinel
         (funcall orig-sentinel proc event))
       (when (and pr-number
                  (string-match-p "finished" event)
                  (eq (process-status proc) 'exit)
                  (zerop (process-exit-status proc)))
         (inline-cr-sync-gh pr-number "push-only" nil))))))

(defun bismuth-magit--magit-git-push-advice (orig-fn branch target args)
  "Advice to trigger inline-cr sync after Magit push."
  (let* ((pr-number (bismuth-magit--current-pr-number))
         (process (funcall orig-fn branch target args)))
    (when (and pr-number (processp process))
      (bismuth-magit--wrap-push-sentinel process pr-number))
    process))

(defun inline-cr--project-root ()
  "Return the current project root if available."
  (or (and (fboundp 'magit-toplevel) (magit-toplevel))
      (and (fboundp 'projectile-project-root) (projectile-project-root))
      default-directory))

(defun inline-cr-sync-gh (pr-number action auto-push)
  "Sync inline-cr threads with GitHub PR review comments."
  (interactive
   (let* ((default-pr (bismuth-magit--current-pr-number))
          (pr (read-number
               (if default-pr
                   (format "PR number (default %s): " default-pr)
                 "PR number: ")
               default-pr))
          (action (completing-read "Sync action: " '("pull" "push" "push-only" "pull-then-push") nil t "pull")))
     (cond
      ((string= action "pull")
       (list pr action nil))
      (t
       (list pr action
             (y-or-n-p "Auto-push after sync? "))))))
  (let* ((root (inline-cr--project-root))
         (script (expand-file-name inline-cr-gh-sync-script inline-cr--bismuth-dir)))
    (if (not (file-exists-p script))
        (message "inline-cr: sync script not found at %s" script)
      (let* ((default-directory root)
             (mode (cond
                    ((string= action "pull") "gh-to-inline")
                    ((string= action "push") "inline-to-gh")
                    ((string= action "push-only") "inline-to-gh")
                    (t "both")))
             (cmd (format "python3 %s --pr %s --mode %s"
                          (shell-quote-argument script)
                          (shell-quote-argument (number-to-string pr-number))
                          (shell-quote-argument mode)))
             (args ""))
        (when (string= action "pull")
          (setq auto-push nil)
          (setq args (concat args " --pull-only")))
        (when auto-push
          (setq args (concat args " --auto-push")))
        (setq cmd (concat cmd args))
        (let* ((display-buffer-alist
                (cons
                 '("\\*inline-cr sync\\*"
                   (display-buffer-in-side-window)
                   (side . bottom)
                   (slot . 0)
                   (window-height . 0.25))
                 display-buffer-alist))
               (comp-buf (compilation-start cmd 'compilation-mode
                                            (lambda (_) "*inline-cr sync*"))))
          (with-current-buffer comp-buf
            (add-hook
             'compilation-finish-functions
             (lambda (_buf msg)
               (when (and (string-match-p "finished" msg)
                          (member action '("pull" "pull-then-push")))
                 (save-excursion
                   (goto-char (point-min))
                   (while (re-search-forward "^updated: \\(.*\\)$" nil t)
                     (let* ((rel (match-string 1))
                            (path (expand-file-name rel root)))
                       (when-let ((file-buf (find-buffer-visiting path)))
                         (with-current-buffer file-buf
                           (revert-buffer nil t))))))
                 (when (fboundp 'magit-refresh)
                   (magit-refresh))))
             nil
             t)))))))
(defun bismuth-magit--run (root)
  "Run bismuth on ROOT and return parsed JSON as an alist (file -> alist(line -> list))."
  (let* ((default-directory root)
         (out (with-temp-buffer
                (let ((exit (apply #'process-file
                                   bismuth-magit-binary
                                   nil (current-buffer) nil
                                   (append bismuth-magit-args (list root)))))
                  (unless (and (integerp exit) (zerop exit))
                    (user-error "bismuth failed (exit %s): %s" exit (buffer-string))))
                (buffer-string))))
    (let ((json-object-type 'alist)  ;; objects -> alists
          (json-array-type  'list)   ;; arrays -> lists
          (json-key-type    'string) ;; keys -> strings
          (json-false       nil))
      (json-read-from-string out))))



(defun bismuth-magit--visit ()
  "Jump to actionable described by BUTTON text properties."
  (interactive)
  (let* ((file (get-text-property (point) 'bismuth-file))
         (line (get-text-property (point) 'bismuth-line)))
    (unless (and file line) (user-error "No actionable at point"))
    (find-file (expand-file-name file (bismuth-magit--repo-root)))
    (goto-char (point-min))
    (forward-line (1- line))
    (recenter)))

(defun bismuth-magit-insert-actionables ()
    "Insert a Magit status section listing CR/XCR actionables for the current repo."
  (let ((inhibit-read-only t)) ;; we need this to mutate the magit buffer


    (let* ((root (bismuth-magit--repo-root))
           (data (bismuth-magit--run root)))
      (when (and data (consp data))
        (magit-insert-section (bismuth-actionables (bismuth-magit--repo-root))
          (magit-insert-heading "Actionable Code Review Threads")
          ;; data: ((\"path/to/file\" . ((\"12\" . [..]) ...)) ...)
          (dolist (file-pair data)
            (let* ((file (car file-pair))
                   (items (cdr file-pair)))
              ;; not very performant or pretty but whaever
              (if (cl-some (lambda (li) (let* ((line-str (car li))
                         (v (cdr li)) ;; now a LIST: (kind reviewer author header thread)
                         (line (string-to-number line-str))
                         (kind (nth 0 v))
                         (reviewer (nth 1 v))
                         (author (nth 2 v))
                         (header (nth 3 v)))
                                 (or
                         (and (string= kind "CR")
                              (string= author inline-cr-user))
                         (and (string= kind "XCR")
                              (string= reviewer inline-cr-user))))) items
                                )

              (magit-insert-section (bismuth-file (list :file file))
                (magit-insert-heading (propertize file 'face 'magit-section-heading))
                ;; items: alist (line-string . vector[kind reviewer author header thread])
                (dolist (li items)
                  (let* ((line-str (car li))
                         (v (cdr li)) ;; now a LIST: (kind reviewer author header thread)
                         (line (string-to-number line-str))
                         (kind (nth 0 v))
                         (reviewer (nth 1 v))
                         (author (nth 2 v))
                         (header (nth 3 v)))

                    (if (or
                         (and (string= kind "CR")
                              (string= author inline-cr-user))
                         (and (string= kind "XCR")
                              (string= reviewer inline-cr-user)))
                        (magit-insert-section (bismuth-item (list :file file :line line))
                          (insert
                           (propertize
                            (format "  %5d  %-3s  %s → %s  %s\n"
                                    line kind reviewer author (string-trim header))
                            'bismuth-file file
                            'bismuth-line line
                            'mouse-face 'highlight
                            'help-echo "RET: visit actionable"
                            'keymap (let ((m (make-sparse-keymap)))
                                      (define-key m (kbd "RET") #'bismuth-magit--visit)
                                      (define-key m (kbd "<return>") #'bismuth-magit--visit)
                                      (define-key m [mouse-1] #'bismuth-magit--visit)
                                      m))))))))))))))))

(with-eval-after-load 'magit
  (define-key magit-status-mode-map (kbd "C-c C-i") #'inline-cr-sync-gh))

(with-eval-after-load 'forge
  (advice-add 'forge-checkout-pullreq :after #'bismuth-magit--pull-inline-cr-for-pullreq)
  (advice-add 'forge-checkout-worktree :after
              (lambda (_path pullreq)
                (bismuth-magit--pull-inline-cr-for-pullreq pullreq))))

(with-eval-after-load 'magit-push
  (advice-add 'magit-git-push :around #'bismuth-magit--magit-git-push-advice))

(provide 'bismuth-magit)
