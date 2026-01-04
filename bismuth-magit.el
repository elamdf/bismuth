;;; bismuth-magit.el --- minimal Magit section for CR/XCR actionables -*- lexical-binding: t; -*-
(require 'cl-lib)

(require 'magit)
(require 'magit-section)
(require 'json)
(require 'subr-x)

(defgroup bismuth-magit nil
  "Show CR/XCR actionables from bismuth in Magit status."
  :group 'magit)

;; > CR elamdf for elamdf: fix hardcoded path
(defcustom bismuth-magit-binary (concat user-emacs-directory "/bismuth/parser/target/release/crscan")
  "Path to the bismuth executable."
  :type 'string
  :group 'bismuth-magit)

(defcustom bismuth-magit-args '("--ext" "md" "--ext" "org" "--ext" "markdown" "--ext" "txt")
  "Args passed to bismuth when scanning a repo."
  :type '(repeat string)
  :group 'bismuth-magit)

(defun bismuth-magit--repo-root ()
  (or (magit-toplevel) (user-error "Not in a Git repository")))

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

(provide 'bismuth-magit)
