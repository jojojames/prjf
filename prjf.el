;;; prjf.el --- Magic `project-find-file'. -*- lexical-binding: t; -*-

(defvar-local prjf-hash nil
  "Hash table to store project files per project.")

(defcustom prjf-is-project-fn
  #'always
  "Function used to determine if `prjf' should take over handling `project-find-file'.

If this is nil, `prjf' will not work."
  :type `(choice
          (const :tag "No highlighting" nil)
          (const :tag "Always use prjf." ,'always)
          (function :tag "Custom function"))
  :group 'prjf)

(defcustom prjf-project-directories-fn nil
  "Function used to return directories `prjf' should find in.")

(defcustom prjf-recent-remove-regex ""
  "Regex used to filter out `recentf' files."
  :type 'string)

(defcustom prjf-find-command "find -H %s -type f"
  "Command to call to gather project files."
  :type 'string)

(defun prjf-recent-should-remove (recent)
  "Return whether or not this RECENT file should be removed."
  (and (string-match-p prjf-recent-remove-regex recent)
       (file-exists-p recent)))

(defun prjf-recent-directory (recent)
  "Return directory of RECENT."
  (file-name-directory recent))

(defun prjf-recent-directories ()
  "Return recent directories used."
  (require 'recentf)
  (thread-last
    recentf-list
    (cl-remove-if-not 'prjf-recent-should-remove)
    (cl-mapcar 'prjf-recent-directory)
    (cl-remove-duplicates)))

(defun prjf-find-project-files (dirs)
  (let ((command
         (format prjf-find-command
                 (mapconcat #'shell-quote-argument dirs " ")))
        res)
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

(defun prjf-project-directories ()
  "Wrapper around `prjf-project-directories-fn'."
  (funcall prjf-project-directories-fn))

(defun prjf-project-p ()
  "Return whether or not `prjf' is handling `project-find-file'."
  (and
   prjf-mode
   (funcall prjf-is-project-fn)))

(defun prjf-track-recent (&rest _args)
  "Update cache with new files."
  (when-let* (buffer-file-name
              prjf-hash
              (project (project-current))
              (gethash project prjf-hash))
    (let* ((recent-dir (prjf-recent-directory buffer-file-name))
           (prjf-find-fn (symbol-function 'proj-find-project-files)))
      (require 'async)
      (async-start
       `(lambda ()
          (funcall ,prjf-find-fn (list ,recent-dir)))
       (lambda (result)
         (let ((project-files (gethash project prjf-hash)))
           (setf (gethash project prjf-hash)
                 (cl-remove-duplicates
                  (append project-files result)))))))))

(cl-defmethod project-files :around ((project (head vc)) &optional dirs)
  (if (prjf-project-p)
      (if-let* ((hash prjf-hash)
                (files (gethash project hash)))
          files
        (let* ((project-dirs (prjf-project-directories))
               (recent-dirs (prjf-recent-directories))
               (project-files (prjf-find-project-files
                               (cl-remove-duplicates
                                (append project-dirs recent-dirs)))))
          (unless prjf-hash
            (setf prjf-hash (make-hash-table :test 'equal)))
          (puthash project project-files)
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
  :global t
  (if prjf-mode
      (progn
        (advice-add 'switch-to-buffer :after 'prjf-track-recent)
        (advice-add 'find-file :after 'prjf-track-recent))
    (advice-remove 'switch-to-buffer 'prjf-track-recent)
    (advice-remove 'find-file 'prjf-track-recent)))

(provide 'prjf)
;;; prjf.el ends here
