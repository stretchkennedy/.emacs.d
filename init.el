;;; init.el --- Initialise emacs
;;; Commentary:
;;; Code:
;; scrollbars (leave this before window config)
(dolist (mode '(menu-bar-mode tool-bar-mode scroll-bar-mode))
  (when (fboundp mode) (funcall mode -1)))

;; MELPA (should be placed before any packages are used)
(require 'package)
(add-to-list 'package-archives
	         '("melpa" . "https://melpa.org/packages/") t)

(setq package-enable-at-startup nil)
(package-initialize)

;; window splitting
(setq split-height-threshold nil)
(setq split-width-threshold 0)

;; store saves somewhere more sensible
(setq backup-directory-alist `(("." . "~/.saves")))

;; flycheck
(global-flycheck-mode)

;; tide (typescript/tsx/etc.)
(defun setup-tide-mode ()
  (interactive)
  (setq typescript-indent-level       2
        typescript-expr-indent-offset 2)
  (tide-setup)
  (flycheck-mode +1)
  (setq flycheck-check-syntax-automatically '(save mode-enabled))
  (eldoc-mode +1)
  (tide-hl-identifier-mode +1)
  ;; company is an optional dependency. You have to
  ;; install it separately via package-install
  ;; `M-x package-install [ret] company`
  (company-mode +1))

;; aligns annotation to the right hand side
(setq company-tooltip-align-annotations t)

;; formats the buffer before saving
(add-hook 'before-save-hook 'tide-format-before-save)

(add-hook 'typescript-mode-hook #'setup-tide-mode)

(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.tsx\\'" . web-mode))
(add-hook 'web-mode-hook
          (lambda ()
            (when (member (file-name-extension buffer-file-name)
                          '("tsx" "ts"))
              (setup-tide-mode))))
(flycheck-add-mode 'typescript-tslint 'web-mode)

;; js2
(setq js2-mode-show-parse-errors nil)
(setq js2-mode-show-strict-warnings nil)

;; rust
(with-eval-after-load 'rust-mode
  (add-hook 'flycheck-mode-hook #'flycheck-rust-setup))

;; flow
(defun my/use-flow-from-node-modules ()
  (let* ((root (locate-dominating-file
                (or (buffer-file-name) default-directory)
                "node_modules"))
         (flow (and root
                    (expand-file-name "node_modules/flow-bin/cli.js" root))))
    (when (and flow (file-executable-p flow))
      (setq-local flycheck-javascript-flow-executable flow))))

(add-hook 'flycheck-mode-hook #'my/use-flow-from-node-modules)

(add-hook 'js2-mode-hook 'flow-minor-enable-automatically)
(require 'flycheck-flow)
(with-eval-after-load 'flycheck
  (flycheck-add-mode 'javascript-flow 'flow-minor-mode)
  (flycheck-add-mode 'javascript-eslint 'flow-minor-mode)
  (flycheck-add-next-checker 'javascript-flow 'javascript-eslint))

;; eslint
(defun my/use-eslint-from-node-modules ()
  (let* ((root (locate-dominating-file
                (or (buffer-file-name) default-directory)
                "node_modules"))
         (eslint (and root
                      (expand-file-name "node_modules/eslint/bin/eslint.js" root))))
    (when (and eslint (file-executable-p eslint))
      (setq-local flycheck-javascript-eslint-executable eslint))))

(add-hook 'flycheck-mode-hook #'my/use-eslint-from-node-modules)

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

;; undo tree mode (much better than vanilla)
(global-undo-tree-mode 1)

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
(add-hook 'before-save-hook
          (lambda ()
            (unless (derived-mode-p '(markdown-mode))
              'delete-trailing-whitespace)))

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

;; use js-mode for all javascript
(add-to-list 'auto-mode-alist '("\\.js[x]?$" . js-mode))
(add-to-list 'auto-mode-alist '("\\.es6$" . js-mode))

(setq web-mode-content-types-alist
      '(("jsx" . "\\.js[x]?\\'")))

(add-hook 'web-mode-hook
          (lambda ()
            ;; short circuit js mode and just do everything in jsx-mode
            (if (equal web-mode-content-type "javascript")
                (web-mode-set-content-type "jsx")
              (message "now set to: %s" web-mode-content-type))))

;; auto-indent
(electric-indent-mode 1)

;; better killing
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

(setq kill-ring-max 1024) ; 60 is just stupid

(defadvice kill-line (before check-position activate)
  (if (member major-mode
              '(emacs-lisp-mode scheme-mode lisp-mode
                                c-mode c++-mode objc-mode
                                latex-mode plain-tex-mode))
      (if (and (eolp) (not (bolp)))
          (progn (forward-char 1)
                 (just-one-space 0)
                 (backward-char 1)))))

;; allow opening a new temporary buffer
(defun new-temp-buffer ()
  "Open up a temporary buffer."
  (interactive)
  (switch-to-buffer (make-temp-name "scratch")))

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
(load-theme 'solarized-light t)

;; fullscreen
(if (memq window-system '(mac ns))
    (global-set-key [(super shift f)] 'toggle-frame-fullscreen))

(add-hook 'delete-frame-functions ; emacs crashes on OSX when closing a fullscreen frame
          (lambda (frame)
            (set-frame-parameter nil 'fullscreen nil)
            (other-frame)))

;; prompt before quitting
(setq confirm-kill-emacs 'y-or-n-p)

;; Custom's bullshit
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(coffee-tab-width 2)
 '(custom-safe-themes
   '("c433c87bd4b64b8ba9890e8ed64597ea0f8eb0396f4c9a9e01bd20a04d15d358" "2809bcb77ad21312897b541134981282dc455ccd7c14d74cc333b6e549b824f3" "708df3cbb25425ccbf077a6e6f014dc3588faba968c90b74097d11177b711ad1" "9dae95cdbed1505d45322ef8b5aa90ccb6cb59e0ff26fef0b8f411dfc416c552" "8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" "a8245b7cc985a0610d71f9852e9f2767ad1b852c2bdea6f4aadc12cce9c4d6d0" "d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879" default))
 '(flycheck-javascript-flow-args nil)
 '(js-indent-level 2)
 '(js2-basic-offset 2)
 '(js2-strict-trailing-comma-warning nil)
 '(jsx-indent-level 2)
 '(package-selected-packages
   '(lsp-go lsp-html lsp-javascript-flow lsp-javascript-typescript lsp-rust vcl-mode gradle-mode undo-tree toml-mode smex slim-mode scss-mode rinari markdown-mode+ magit-filenotify jsx-mode jade-mode go-autocomplete framemove fold-dwim flymake-ruby flymake-json flymake-jshint 2048-game))
 '(web-mode-code-indent-offset 2))

;; finalise
(provide 'init)
;;; init.el ends here
