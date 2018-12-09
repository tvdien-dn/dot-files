;;; package --- Summary
;;; Commentary:
;;; Code:

;;  auto-save settings
(let ((target-dir (expand-file-name "~/"))
      (dest-dir   (expand-file-name "~/.Trash/")))
  (add-to-list 'auto-save-file-name-transforms
               `(,(concat target-dir "\\([^/]*/\\)*\\([^/]*\\)$")
                 ,(concat dest-dir   "\\2")
                 t))
  (add-to-list 'backup-directory-alist (cons target-dir dest-dir))
  (setq auto-save-list-file-prefix (expand-file-name ".save-" dest-dir)))
(setq make-backup-files nil)

;; Diredの設定
(add-hook 'dired-load-hook (lambda()
                             (define-key dired-mode-map "r" 'wdired-change-to-wdired-mode)))
(setq dired-listing-switches (purecopy "-aAhl"))
(put 'dired-find-alternate-file 'disabled nil)
(setq dired-dwim-target t)

;; swap left-option and left-command keys
(when (eq system-type 'darwin)
  (defvar ns-command-modifier (quote meta))
  (defvar ns-alternate-modifier (quote super)))

;; Other
(prefer-coding-system 'utf-8) ;; set system character code
(setq-default indent-tabs-mode nil) ;; use space for indentation instead of tab

;; スクロールした際のカーソルの移動行数
(setq scroll-conservatively 1)
;; スクロール開始のマージンの行数
(setq scroll-margin 10)
;; 1 画面スクロール時に重複させる行数
(setq next-screen-context-lines 10)
;; 1 画面スクロール時にカーソルの画面上の位置をなるべく変えない
(setq scroll-preserve-screen-position t)
;; (windmove-default-keybindings) ;; Windownsの移動 Shift-arrow_key
(electric-pair-mode t) ;; 自動の閉じ括弧
(show-paren-mode t) ;; 括弧の強調表示
(auto-revert-mode t) ;; 自動revert-buffer
(column-number-mode nil) ;; カラム数
(menu-bar-mode -1)

;; auto completion with company-mode
(use-package company
  :ensure t
  :bind (:map company-active-map
          ("C-n" . company-select-next)
          ("C-p" . company-select-previous)
          ("M-n" . nil)
          ("M-p" . nil)
          ("C-h" . nil))
  :init
  (add-hook 'after-init-hook 'global-company-mode)
  (setq company-dabbrev-downcase nil)
  (setq company-idle-delay 0)
  (setq company-minimum-prefix-length 2)
  (setq company-selection-wrap-around t)
  )

(use-package smart-mode-line
  :ensure t
  :config
  (setq sml/theme 'dark)
  (setq sml/col-number t)
  (setq sml/show-eol t)
  (setq sml/shorten-directory t)
  (setq sml/rm-blacklist '("Helm-Gtags"))
  (setq sml/no-confirm-load-theme t)
  (add-to-list 'sml/replacer-regexp-list '("^~/jp_projects/github-private/1pac" "1pac_git") t)
  (sml/setup)
  )

;; whitespace (build-in)
;; https://www.emacswiki.org/emacs/WhiteSpace
(use-package whitespace
  :config
  (global-whitespace-mode t)
  (setq whitespace-space-regexp "\\(\u3000\\)") ;; visualize only fullwidth space
  (setq whitespace-style '(face trailling tabs spaces empty tab-mark))
  (setq whitespace-action '(auto-cleanup)))

(use-package editorconfig
  :config
  (editorconfig-mode t))
(use-package undo-tree
  :config
  (global-undo-tree-mode t)
  )
(use-package fzf
  :ensure-system-package fzf
  :bind ("\C-xp" . fzf-git)
  :config
  (setq fzf/args "-x --color 16 --print-query"))
(use-package projectile
  :config
  (define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
  (setq projectile-project-search-path '("~/jp_projects/github-private/1pac/"))
  (projectile-mode +1))

;; (use-package tramp
;;   :config
;;   (setq tramp-default-method "ssh"))

;;ivy-mode http://oremacs.com/swiper/#installation
(use-package counsel
  :config
  (ivy-mode t)
  (defvar ivy-use-virtual-buffers t)
  (setq enable-recursive-minibuffers t)
  (global-set-key "\C-s" 'swiper)
  (global-set-key (kbd "M-x") 'counsel-M-x)
  (global-set-key (kbd "C-x C-f") 'counsel-find-file)
  (global-set-key (kbd "<f1> f") 'counsel-describe-function)
  (global-set-key (kbd "<f1> v") 'counsel-describe-variable)
  (global-set-key (kbd "<f1> l") 'counsel-find-library)
  ;; (global-set-key (kbd "<f2> i") 'counsel-info-lookup-symbol)
  ;; (global-set-key (kbd "<f2> u") 'counsel-unicode-char)
  (global-set-key (kbd "C-c g") 'counsel-git)
  (global-set-key (kbd "C-c j") 'counsel-git-grep)
  (global-set-key (kbd "C-c k") 'counsel-ag)
  (global-set-key (kbd "C-x l") 'counsel-locate)
  ;; (global-set-key (kbd "C-S-o") 'counsel-rhythmbox)
  ;; (define-key minibuffer-local-map (kbd "C-r") 'counsel-minibuffer-history)
  )
(provide '00_base)
;;; 00_base ends here
