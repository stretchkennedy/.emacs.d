;; MELPA
(require 'package)
(add-to-list 'package-archives
	     '("melpa" . "http://melpa.milkbox.net/packages/") t)

;; window size
(defun set-frame-size-according-to-resolution ()
  (interactive)
  (if window-system
      (progn
        (if (> (x-display-pixel-width) 1280)
          (add-to-list 'default-frame-alist (cons 'width 279))
          (add-to-list 'default-frame-alist (cons 'width 80)))
        (add-to-list 'default-frame-alist
                     (cons 'height (/ (x-display-pixel-height)
                                      (frame-char-height)))))))

(set-frame-size-according-to-resolution)

;; windmove/framemove
(require 'framemove)
(global-set-key [(shift meta up)]    'fm-up-frame)                           ; `M-S-up'
(global-set-key [(shift meta down)]  'fm-down-frame)                         ; `M-S-down'
(global-set-key [(shift meta left)]  'fm-left-frame)                         ; `M-S-left'
(global-set-key [(shift meta right)] 'fm-right-frame)                        ; `M-S-right'

;; robe
(defadvice inf-ruby-console-auto (before activate-rvm-for-robe activate) (rvm-activate-corresponding-ruby))
(add-hook 'robe-mode-hook 'ac-robe-setup)

;; dired
(setq ls-lisp-use-insert-directory-program t)
(setq insert-directory-program "gls")

;; search path
(let ((default-directory "~/.emacs.d/"))
  (normal-top-level-add-subdirs-to-load-path))

(add-to-list 'load-path "~/.emacs.d/")

;; $PATH and friends
(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize))

;; indentation
(setq-default indent-tabs-mode nil)

;; line numbers
;; (require 'nlinum)
(global-linum-mode 1)

;; line highlighting
(global-hl-line-mode 1)

;; auto-complete
(require 'auto-complete-config)
(ac-config-default)
(setq ac-delay 2)

;; trailing whitespace
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; git-grep
(require 'custom-git-grep)
(global-set-key (kbd "M-g f") 'git-grep)

;; RVM
(require 'rvm)
(rvm-use-default)

;; ruby-mode
(add-hook 'ruby-mode-hook 'robe-mode)

(add-hook 'ruby-mode-hook
	  (lambda ()
	    (define-key ruby-mode-map "\C-c#" 'comment-or-uncomment-region)))

(defadvice comment-or-uncomment-region (before slick-comment activate compile)
  "When called interactively with no active region, comment a single line instead."
  (interactive
   (if mark-active (list (region-beginning) (region-end))
     (list (line-beginning-position)
	   (line-beginning-position 2)))))

;; css-mode
(setq-default css-indent-offset 2)

;; sass-mode
(autoload 'scss-mode "scss-mode")
(add-to-list 'auto-mode-alist '("\\.scss\\'" . css-mode))
(setq scss-compile-at-save nil)

;; coffee-mode
(custom-set-variables '(coffee-tab-width 2))

;; slim-mode
(setq slim-indent-offset 2)

;; web-mode
(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.jsp\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))

(setq web-mode-markup-indent-offset 2)

;;; yasnippet and web-mode are incompatible
(add-hook 'web-mode-hook (lambda()
                           (setq yas-dont-activate t)))

(add-to-list 'auto-mode-alist '("\\.mahtml\\'" . web-mode))
(setq web-mode-engines-alist
      '(("erb"    . "\\.mahtml\\'")))

;; use ruby-mode for DSLs
(setq auto-mode-alist (cons '("\\.rabl$" . ruby-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.jbuilder$" . ruby-mode) auto-mode-alist))

(setq auto-mode-alist (cons '("\\^Gemfile$" . ruby-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\^Gemfile.lock$" . ruby-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\^Rakefile$" . ruby-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.rake$" . ruby-mode) auto-mode-alist))

;; set up hide show for ruby
(add-to-list 'hs-special-modes-alist
	     '(ruby-mode
	       "\\(def\\|do\\|{\\)" "\\(end\\|end\\|}\\)" "#"
	       (lambda (arg) (ruby-end-of-block)) nil))

;; auto-indent
(electric-indent-mode 1)

(dolist (command '(yank yank-pop))
   (eval `(defadvice ,command (after indent-region activate)
            (and (not current-prefix-arg)
                 (member major-mode '(emacs-lisp-mode lisp-mode
                                      clojure-mode    scheme-mode
                                      haskell-mode    ruby-mode
                                      rspec-mode      python-mode
                                      c-mode          c++-mode
                                      objc-mode       latex-mode
                                      plain-tex-mode))
                 (let ((mark-even-if-inactive transient-mark-mode))
                   (indent-region (region-beginning) (region-end) nil))))))

(defadvice kill-line (before check-position activate)
  (if (member major-mode
              '(emacs-lisp-mode scheme-mode lisp-mode
                                c-mode c++-mode objc-mode
                                latex-mode plain-tex-mode))
      (if (and (eolp) (not (bolp)))
          (progn (forward-char 1)
                 (just-one-space 0)
                 (backward-char 1)))))

; speedbar

;(speedbar t)

;(require 'speedbar)
;  (defconst my-speedbar-buffer-name "SPEEDBAR")
;  ; (defconst my-speedbar-buffer-name " SPEEDBAR") ; try this if you get "Wrong type argument: stringp, nil"
;  (defun my-speedbar-no-separate-frame ()
;    (interactive)
;    (when (not (buffer-live-p speedbar-buffer))
;      (setq speedbar-buffer (get-buffer-create my-speedbar-buffer-name)
;            speedbar-frame (selected-frame)
;            dframe-attached-frame (selected-frame)
;            speedbar-select-frame-method 'attached
;            speedbar-verbosity-level 0
;            speedbar-last-selected-file nil)
;      (set-buffer speedbar-buffer)
;      (speedbar-mode)
;      (speedbar-reconfigure-keymaps)
;      (speedbar-update-contents)
;      (speedbar-set-timer 1)
;      (make-local-hook 'kill-buffer-hook)
;      (add-hook 'kill-buffer-hook
;                (lambda () (when (eq (current-buffer) speedbar-buffer)
;                             (setq speedbar-frame nil
;                                   dframe-attached-frame nil
;                                   speedbar-buffer nil)
;                             (speedbar-set-timer nil)))))
;    (set-window-buffer (selected-window)
;                       (get-buffer my-speedbar-buffer-name)))
(put 'upcase-region 'disabled nil)
