;;; init.el --- Initialise emacs
;;; Commentary:
;;; Code:
;; scrollbars (leave this before window config)
(dolist (mode '(menu-bar-mode tool-bar-mode scroll-bar-mode))
  (when (fboundp mode) (funcall mode -1)))

;; MELPA (should be placed before any packages are used)
(require 'package)
(add-to-list 'package-archives
	     '("melpa" . "http://melpa.milkbox.net/packages/") t)

(setq package-enable-at-startup nil)
(package-initialize)

;; store saves somewhere more sensible
(setq backup-directory-alist `(("." . "~/.saves")))

;; enable flycheck globally
(add-hook 'after-init-hook #'global-flycheck-mode)

;; windmove/framemove
(require 'framemove)
(if (eq system-type 'darwin)
    (windmove-default-keybindings 'super)
  (windmove-default-keybindings 'ctrl))

(setq framemove-hook-into-windmove t)

;; disable annoying bell
(setq ring-bell-function 'ignore)


;; robe
(defadvice inf-ruby-console-auto (before activate-rvm-for-robe activate) (rvm-activate-corresponding-ruby))
(add-hook 'robe-mode-hook 'ac-robe-setup)

;; dired
(setq ls-lisp-use-insert-directory-program t)
(if (eq system-type 'darwin)
    (setq insert-directory-program "gls")
  (setq insert-directory-program "ls"))

;; search path
(let ((default-directory "~/.emacs.d/manual"))
  (normal-top-level-add-subdirs-to-load-path))

(add-to-list 'load-path "~/.emacs.d/manual")

;; $PATH and friends
(exec-path-from-shell-initialize)
(exec-path-from-shell-copy-env "PATH")
(exec-path-from-shell-copy-env "GOPATH")

;; indentation
(setq-default indent-tabs-mode nil)

;; tab width
(setq-default tab-width 4)

;; line numbers
;; (require 'nlinum)
(global-linum-mode 1)

;; line highlighting
(global-hl-line-mode 1)

;; replace yes confirmation with y and no confirmation with n
(defalias 'yes-or-no-p 'y-or-n-p)

;; auto-complete
(require 'auto-complete-config)
(ac-config-default)
(setq ac-delay 2)

;; ido (interactive file completion)
(setq ido-enable-flex-matching t)
(setq ido-everywhere t)

(ido-mode 1)

;; smex (ido for M-x
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)
(global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)

;; trailing whitespace
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; git-grep
(require 'custom-git-grep)
(global-set-key (kbd "M-g f") 'git-grep)

;; magit
(require 'magit)
(global-set-key (kbd "M-g s") 'magit-status)
(global-set-key (kbd "M-g b") 'magit-blame)
(setq magit-last-seen-setup-instructions "1.4.0")(setq magit-last-seen-setup-instructions "1.4.0") ; bugger off

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

;; markdown-mode
(add-hook
 'markdown-mode-hook
 '(lambda ()
    (define-key markdown-mode-map (kbd "M-<left>") 'backward-word)
    (define-key markdown-mode-map (kbd "M-<right>") 'forward-word)
    (define-key markdown-mode-map (kbd "C-s-<left>") 'markdown-promote)
    (define-key markdown-mode-map (kbd "C-s-<right>") 'markdown-demote)))




;; css-mode
(setq-default css-indent-offset 2)

;; sass-mode
(autoload 'scss-mode "scss-mode")
(add-to-list 'auto-mode-alist '("\\.scss\\'" . css-mode))
(setq scss-compile-at-save nil)

;; coffee-mode
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(coffee-tab-width 2)
 '(custom-safe-themes
   (quote
    ("708df3cbb25425ccbf077a6e6f014dc3588faba968c90b74097d11177b711ad1" "9dae95cdbed1505d45322ef8b5aa90ccb6cb59e0ff26fef0b8f411dfc416c552" "8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" "a8245b7cc985a0610d71f9852e9f2767ad1b852c2bdea6f4aadc12cce9c4d6d0" "d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879" default)))
 '(js-indent-level 2)
 '(jsx-indent-level 2))

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

;; use jsx mode for all javascript, because whatever
(add-to-list 'auto-mode-alist '("\\.js$" . jsx-mode))
(add-to-list 'auto-mode-alist '("\\.es6$" . jsx-mode))



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
(put 'downcase-region 'disabled nil)

;; w3m
(setq browse-url-browser-function 'w3m-browse-url)
(autoload 'w3m-browse-url "w3m" "Ask a WWW browser to show a URL." t)
(setq w3m-use-cookies t)

;; theme
(load-theme 'solarized-dark)

;; fullscreen
(if (memq window-system '(mac ns))
    (global-set-key [(super shift f)] 'toggle-frame-fullscreen))

(provide 'init)
;;; init.el ends here
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
