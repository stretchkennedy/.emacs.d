;; derived from rails-project:root from
;; https://github.com/remvee/emacs-rails
(defun git-root ()
  "Return GIT_ROOT if this file is a part of a git repo,
else return nil"
  (let ((curdir default-directory)
        (max 10)
        (found nil))
    (while (and (not found) (> max 0))
      (progn
        (if (file-directory-p (concat curdir ".git"))
            (progn
              (setq found t))
          (progn
            (setq curdir (concat curdir "../"))
            (setq max (- max 1))))))
    (if found (expand-file-name curdir))))

;; Derived from `vc-git-grep'.
(defun git-grep (regexp &optional dir)
  "Run git grep, searching for REGEXP in directory DIR.

DIR defaults to the root directory of the git project.

With \\[universal-argument] prefix, you can edit the constructed shell command line
before it is executed.
With two \\[universal-argument] prefixes, directly edit and run `grep-command'.

Collect output in a buffer.  While git grep runs asynchronously, you
can use \\[next-error] (M-x next-error), or \\<grep-mode-map>\\[compile-goto-error] \
in the grep output buffer,
to go to the lines where grep found matches.

This command shares argument histories with \\[rgrep] and \\[grep]."
  (interactive
   (progn
     (grep-compute-defaults)
     (cond
      ((equal current-prefix-arg '(16))
       (list (read-from-minibuffer "Run: " "git grep"
  			   nil nil 'grep-history)
	     nil))
      (t (let* ((regexp (grep-read-regexp))
                (dir (let ((tmp-git-root (git-root)))
                       (read-directory-name "In directory: "
                                            tmp-git-root tmp-git-root t))))
	   (list regexp dir))))))
  (require 'grep)
  (when (and (stringp regexp) (> (length regexp) 0))
    (let ((command regexp))
      (if (string= command "git grep")
          (setq command nil))
      (setq dir (file-name-as-directory (expand-file-name dir)))
      (setq command
            (grep-expand-template "git grep -n -e <R> -- <F>"
                                  regexp "*"))
      (when command
        (if (equal current-prefix-arg '(4))
            (setq command
                  (read-from-minibuffer "Confirm: "
                                        command nil nil 'grep-history))
          (add-to-history 'grep-history command)))
      (when command
	(let ((default-directory dir)
	      (compilation-environment (cons "PAGER=" compilation-environment)))
	  ;; Setting process-setup-function makes exit-message-function work
	  ;; even when async processes aren't supported.
	  (compilation-start command 'grep-mode))
	(if (eq next-error-last-buffer (current-buffer))
	    (setq default-directory dir))))))

(provide 'custom-git-grep)
