;;; package --- Summary
;;; Commentary:
;;; Code:

;; auto-save settings
(let ((target-dir (expand-file-name "~/"))
      (dest-dir   (expand-file-name "~/.Trash/")))
  (add-to-list 'auto-save-file-name-transforms
               `(,(concat target-dir "\\([^/]*/\\)*\\([^/]*\\)$")
                 ,(concat dest-dir   "\\2")
                 t))
  (add-to-list 'backup-directory-alist (cons target-dir dest-dir))
  (setq auto-save-list-file-prefix (expand-file-name ".save-" dest-dir)))
(setq make-backup-files nil)

;; disable the annoying bell ring
(setq ring-bell-function 'ignore)

;; nice scrolling
(setq scroll-margin 0
  scroll-conservatively 100000
  scroll-preserve-screen-position 1)

;; enable y/n answers
(fset 'yes-or-no-p 'y-or-n-p)

(setq hippie-expand-try-functions-list '(try-expand-dabbrev
                                         try-expand-dabbrev-all-buffers
                                         try-expand-dabbrev-from-kill
                                         try-complete-file-name-partially
                                         try-complete-file-name
                                         try-expand-all-abbrevs
                                         try-expand-list
                                         try-expand-line
                                         try-complete-lisp-symbol-partially
                                         try-complete-lisp-symbol))
(bind-key "M-/" #'hippie-expand)

;; iBuffer
(bind-key "C-x C-b" 'ibuffer)

;; swap left-option and left-command keys
(when (eq system-type 'darwin)
  (defvar ns-command-modifier (quote meta))
  (defvar ns-alternate-modifier (quote super)))

(use-package which-key :ensure t :delight
  :config
  (which-key-mode +1))

;; Dired settings
(use-package dired :ensure nil
  :bind (:map dired-mode-map ("r" . wdired-change-to-wdired-mode))
  :config
  (setq dired-listing-switches (purecopy "-Ahl"))
  (put 'dired-find-alternate-file 'disabled nil)
  (custom-set-variables
    '(dired-dwim-target t)))

;; setting chraracter code
(prefer-coding-system 'utf-8) ;; set system character code
(setq indent-tabs-mode nil) ;; use space for indentation instead of tab

;; スクロールした際のカーソルの移動行数
(setq scroll-conservatively 1)
;; スクロール開始のマージンの行数
(setq scroll-margin 10)
;; 1 画面スクロール時に重複させる行数
(setq next-screen-context-lines 10)
;; 1 画面スクロール時にカーソルの画面上の位置をなるべく変えない
(setq scroll-preserve-screen-position t)
(electric-pair-mode t) ;; 自動の閉じ括弧
(show-paren-mode t) ;; 括弧の強調表示
(column-number-mode nil) ;; カラム数
(menu-bar-mode -1) ;; メニューバーの非表示

(use-package autorevert :ensure nil
  :delight auto-revert-mode
  :config
  (global-auto-revert-mode t))

(use-package zop-to-char :ensure t
  :bind (("M-z" . zop-up-to-char)
          ("M-Z" . zop-to-char)))

(use-package windmove
  :config
  ;; use shift + arrow keys to switch between visible buffers
  (windmove-default-keybindings))

;; auto completion with company-mode
(use-package company :ensure t :delight
  :bind (:map company-active-map
          ("C-n" . company-select-next)
          ("C-p" . company-select-previous)
          ("M-n" . nil)
          ("M-p" . nil)
          ("C-h" . nil))
  :config
  (custom-set-variables
    '(company-dabbrev-downcase nil)
    '(company-idle-delay 0.5)
    '(company-minimum-prefix-length 2)
    '(company-selection-wrap-around t)
    '(company-tooltip-align-annotations t)
    '(company-tooltip-flip-when-above t))
  (global-company-mode)
  )

(use-package smart-mode-line :ensure t
  :config
  (setq sml/no-confirm-load-theme t)
  (setq sml/theme 'dark)
  (setq sml/show-eol t)
  (setq sml/shorten-directory nil)
  (setq sml/shorten-modes t)
  (setq sml/col-number-format "%3")
  (setq sml/line-number-format "%4")
  (setq sml/name-width (cons 35 50))
  (add-to-list 'sml/replacer-regexp-list '("^~/projects/github.com/" ":PG:/") t)
  (add-to-list 'sml/replacer-regexp-list '("\\(/.\\)[^/]+" "\\1") t)
  (sml/setup))

(use-package rainbow-mode :delight
  :config
  (add-hook 'web-mode-hook 'rainbow-mode)
  (add-hook 'emacs-lisp-mode-hook 'rainbow-mode)
  )

;; https://www.emacswiki.org/emacs/WhiteSpace
(use-package whitespace
  :delight whitespace
  :config
  ;; visualize only fullwidth space
  (setq whitespace-space-regexp "\\(\u3000+\\)")
  (set-face-attribute 'whitespace-space nil
    :background "#dddddd"
    :underline nil)
  (add-hook 'before-save-hook 'delete-trailing-whitespace)
  (setq whitespace-style '(face trailling tabs empty tab-mark spaces))
  (setq whitespace-action '(auto-cleanup))
  (global-whitespace-mode t))

;; Hide mode name in mode-line
(use-package delight :ensure t)

(use-package editorconfig :delight
  :config
  (editorconfig-mode t))

(use-package undo-tree :delight
  :config
  (global-undo-tree-mode t)
  )

(use-package fzf
  :ensure-system-package fzf
  :bind ("C-x p" . fzf-all-file)
  :functions
  fzf/start
  :init
  (eval-when-compile (require 'fzf))
  (defun fzf-all-file ()
    "Get all files."
    (interactive)
    (fzf/start
      default-directory
      (concat "ag -a --hidden -g '' " (ido-read-directory-name "Directory: "))))
  :config
  (setq fzf/args "-x --color 16 --print-query")
)
;;; projectile
(use-package projectile
  :delight
  :bind-keymap (("C-c p" . projectile-command-map)
                ("s-p" . projectile-command-map))
  :config
  (setq projectile-project-search-path '("~/projects"))
  (setq projectile-completion-system 'ivy)
  (setq projectile-switch-project-action #'projectile-dired)
  (projectile-mode +1))

;; (use-package tramp
;;   :config
;;   (setq tramp-default-method "ssh"))

(use-package ivy :delight)
(use-package counsel
  :bind (
          ("\C-s" . swiper)
          ("M-x" . counsel-M-x)
          ("C-x C-f" . counsel-find-file)
          ("<f1> f" . counsel-describe-function)
          ("<f1> v" . counsel-describe-variable)
          ("<f1> l" . counsel-find-library)
          ("<f2> i" . counsel-info-lookup-symbol)
          ("<f2> u" . counsel-unicode-char)
          ("C-c g" . counsel-git)
          ("C-c j" . counsel-git-grep)
          ("C-c k" . counsel-ag)
          ("C-x l" . counsel-locate)
          (:map minibuffer-local-map ("C-r" . counsel-minibuffer-history))
          )
  :config
  (ivy-mode t)
  (defvar ivy-use-virtual-buffers t)
  (setq enable-recursive-minibuffers t)
  )

(use-package counsel-etags
  :config
  :bind ("C-c ]" . counsel-etags-find-tag-at-point))
(use-package counsel-projectile
  :config
  (counsel-projectile-mode))
(use-package direnv
  :config
  (direnv-mode))

;; Flycheck
(use-package flycheck :ensure t
  :config
  (add-hook 'after-init-hook #'global-flycheck-mode)
  )

(use-package flycheck-color-mode-line :ensure t
  :requires flycheck
  :config
  (setq flycheck-color-mode-line-face-to-color 'sml/filename)
  (eval-after-load "flycheck"
    '(add-hook 'flycheck-mode-hook 'flycheck-color-mode-line-mode))
)

(use-package anzu :ensure t
  :bind (("M-%" . anzu-query-replace-regexp))
  :config
  (global-anzu-mode)
  (custom-set-variables
    '(anzu-mode-lighter "")
    '(anzu-deactivate-region t)
    '(anzu-search-threshold 1000))
  )

;; magit
(use-package magit :ensure t
  :config
  (setq magit-completing-read-function 'ivy-completing-read)
  )

;; Google translate settings
(use-package google-translate :ensure t
  :config
  (bind-key "\C-ct" 'google-translate-smooth-translate)
  (setq google-translate-pop-up-buffer-set-focus t)
  (defvar google-translate-translation-directions-alist
    '(("en" . "ja") ("ja" . "en") ("ja" . "vi") ("en" . "vi")))
  )
(eval-after-load "google-translate"
  '(defun google-translate--get-b-d1 ()
    ;; TKK='427110.1469889687'
  (list 427110 1469889687))
  )

;;Quickrun
(use-package quickrun
  :config
  (setq quickrun-focus-p nil))
(provide '00_base)

(use-package highlight-indent-guides
  :config
  (setq highlight-indent-guides-character ?\|)
  (setq highlight-indent-guides-method 'character)
  (add-hook 'prog-mode-hook 'highlight-indent-guides-mode)
  (add-hook 'yaml-mode-hook 'highlight-indent-guides-mode)
  )

(use-package unicode-fonts)
(use-package indium)
;;; 00_base ends here
