;;; prjf.el --- Magic `project-find-file'. -*- lexical-binding: t; -*-

(eval-when-compile (require 'subr-x))

(defvar prjf-hash nil
  "Hash table to store project files per project.")

(defvar prjf-loaded-projects nil
  "List tracking loaded projects.

Project is considered to be loaded if `project-files' has been called on it.")

(defcustom prjf-is-project-fn
  #'always
  "Function used to determine if `prjf' should take over `project-find-file'.

If this is nil, `prjf' will not work."
  :type `(choice
          (const :tag "No highlighting" nil)
          (const :tag "Always use prjf." ,'always)
          (function :tag "Custom function"))
  :group 'prjf)

(defcustom prjf-build-find-command-fn 'prjf-build-find-command
  "Function used to build find command."
  :type 'function
  :group 'prjf)

(defcustom prjf-project-directories-fn 'prjf-find-project-directories
  "Function used to return directories `prjf' should find in."
  :type 'function
  :group 'prjf)

(defcustom prjf-project-ignores-fn 'prjf-find-ignores
  "Functioned used to return directories `prjf' should ignore during find. "
  :type 'function
  :group 'prjf)

(defcustom prjf-track-new-files t
  "Whether or not to track `find-file'."
  :type 'boolean
  :group 'prjf)

(defcustom prjf-commands-to-track '(find-file)
  "Commands to advise."
  :type 'list
  :group 'prjf)

(defun prjf-build-find-command (dirs _ignores)
  "Return find command."
  (format
   "find -H %s -type f"
   (mapconcat #'shell-quote-argument dirs " ")))

(defun prjf-find-project-files (dirs &optional ignores)
  "Use `prjf-build-find-command-fn' to find files in DIRS."
  (let ((command (funcall prjf-build-find-command-fn dirs ignores)))
    ;; (message command)
    (with-temp-buffer
      (let ((status
             (process-file-shell-command command nil t))
            (pt (point-min)))
        (unless (zerop status)
          (goto-char (point-min))
          (if (and
               (not (eql status 127))
               (search-forward "Permission denied\n" nil t))
              (let ((end (1- (point))))
                (re-search-backward "\\`\\|\0")
                (error "File listing failed: %s"
                       (buffer-substring (1+ (point)) end)))
            (error "File listing failed: %s" (buffer-string))))
        (goto-char pt)
        (split-string (buffer-string) nil t)))))

(defun prjf-project-p ()
  "Return whether or not `prjf' is handling `project-find-file'."
  (and
   prjf-mode
   (funcall prjf-is-project-fn)))

(defun prjf-find-project-directories ()
  "Return directories for `project-current'."
  (list (project-root (project-current))))

(defun prjf-find-ignores ()
  "Return directories to ignore for `project-current'."
  (mapcar (lambda (dir)
            (project--value-in-dir 'project-vc-ignores dir))
          (funcall prjf-project-directories-fn)))

(defun prjf-count-char-in-string (c str)
  "Count number of C in STR."
  (if (and (char-or-string-p c)
           (char-or-string-p str))
      (let ((c (if (characterp c) c (string-to-char c))))
        (cl-count c (string-to-list str)))
    (error "Verification failed.")))

(cl-defmethod project-files :around ((project (head vc)) &optional _dirs)
  (if (prjf-project-p)
      (if-let* ((hash prjf-hash)
                ;; Hash could have been built elsewhere, only go down this route
                ;; if we've ran down the else block and pushed project to
                ;; `prjf-loaded-projects'.
                (files (and (member project prjf-loaded-projects)
                            (gethash project hash))))
          files
        (let* ((project-dirs (funcall prjf-project-directories-fn))
               (ignore-dirs (funcall prjf-project-ignores-fn))
               (project-files (prjf-find-project-files
                               (cl-remove-duplicates
                                project-dirs
                                :test 'equal)
                               ignore-dirs)))
          (unless prjf-hash
            (setf prjf-hash (make-hash-table :test 'equal)))

          (if (member project prjf-loaded-projects)
              (setf (gethash project prjf-hash)
                    (cl-remove-duplicates
                     (append project-files (gethash project prjf-hash))
                     :test 'equal))
            (push project prjf-loaded-projects)
            (puthash project project-files prjf-hash))

          project-files))
    (cl-call-next-method)
    ;; (mapcan
    ;;  (lambda (dir)
    ;;    (message dir)
    ;;    (let ((ignores (project--value-in-dir 'project-vc-ignores dir))
    ;;          backend)
    ;;      (if (and (file-equal-p dir (project-root project))
    ;;               (setq backend (vc-responsible-backend dir))
    ;;               (cond
    ;;                ((eq backend 'Hg))
    ;;                ((and (eq backend 'Git)
    ;;                      (or
    ;;                       (not ignores)
    ;;                       (version<= "1.9" (vc-git--program-version)))))))
    ;;          (project--vc-list-files dir backend ignores)
    ;;        (project--files-in-directory
    ;;         dir
    ;;         (project--dir-ignores project dir)))))
    ;;  (or dirs
    ;;      (list (project-root project))))
    ))

;;;###autoload
(define-minor-mode prjf-mode
  "Global minor mode for `prjf'."
  :group 'prjf
  :global t)

(provide 'prjf)
;;; prjf.el ends here
