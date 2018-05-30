;;; package --- Summary
;;; Commentary:
;;; Code:

(require 'company)
(add-hook 'after-init-hook 'global-company-mode)
(setq company-dabbrev-downcase nil)
(setq company-idle-delay 0)
(setq company-minimum-prefix-length 2)
(setq company-selection-wrap-around t)
(define-key company-active-map (kbd "M-n") nil)
(define-key company-active-map (kbd "M-p") nil)
(define-key company-active-map (kbd "C-n") 'company-select-next)
(define-key company-active-map (kbd "C-p") 'company-select-previous)
(define-key company-active-map (kbd "C-h") nil)

;; 自動保存設定
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
(ffap-bindings)
(add-hook 'dired-load-hook (lambda()
                             (define-key dired-mode-map "r" 'wdired-change-to-wdired-mode)))
(setq dired-listing-switches (purecopy "-aAhl"))
(put 'dired-find-alternate-file 'disabled nil)
(setq dired-dwim-target t)

;; smart mode line
(setq sml/theme 'respectful)
(setq sml/col-number t)
(setq sml/show-eol t)
(setq sml/shorten-directory t)
(setq sml/name-width 20)
;; (setq sml/rm-blacklist '("Helm-Gtags"))
(setq sml/no-confirm-load-theme t)
(sml/setup)
(add-to-list 'sml/replacer-regexp-list '("^~/jp_projects/" "HOME") t)

;; Other
(prefer-coding-system 'utf-8) ;; 文字コード utf-8
(require 'framemove)
(windmove-default-keybindings)
(setq framemove-hook-into-windmove t)
;; (windmove-default-keybindings) ;; Windownsの移動 Shift-arrow_key
(electric-pair-mode t) ;; 自動の閉じ括弧
(show-paren-mode t) ;; 括弧の強調表示
(auto-revert-mode t) ;; 自動revert-buffer
(column-number-mode nil) ;; カラム数
(menu-bar-mode -1)

;; whitespace (build-in)
(require 'whitespace)
(setq whitespace-style '(face          ; faceで可視化
                        trailing       ; 行末
                        tabs           ; タブ
                        spaces         ; スペース
                        empty          ; 先頭/末尾の空行
                        ;; space-mark     ; 表示のマッピング
                        tab-mark
                        ))
;; (setq whitespace-display-mappings
;;      '((space-mark ?\u3000 [?\u2015])))
(setq whitespace-space-regexp "\\(\u3000\\)") ;; スペースは全角のみを可視化
(setq whitespace-action '(auto-cleanup)) ;; 保存前に自動でクリーンアップ
(global-whitespace-mode 1)

(setq-default indent-tabs-mode nil) ;; インデントをTAB->SPACEに変更

(require 'editorconfig)
(editorconfig-mode t)

;; undo-tree
(require 'undo-tree)
(global-undo-tree-mode t)
(global-set-key (kbd "M-/") 'undo-tree-redo)

;; Customize keybindings
(require 'fzf)
(setq fzf/args "-x --color 16 --print-query")
(global-set-key "\C-xp" 'fzf-jp-projects)
(defun fzf-jp-projects()
  (interactive)
  (fzf/start (replace-regexp-in-string "\\(.*jp_projects/[^/]+/\\).*" "\\1" default-directory)))

;;SSH editor
(require 'tramp)
(setq tramp-default-method "ssh")

;; スクロールした際のカーソルの移動行数
(setq scroll-conservatively 1)
;; スクロール開始のマージンの行数
(setq scroll-margin 10)
;; 1 画面スクロール時に重複させる行数
(setq next-screen-context-lines 10)
;; 1 画面スクロール時にカーソルの画面上の位置をなるべく変えない
(setq scroll-preserve-screen-position t)

;; ivy-mode
(ivy-mode 1)
(defvar ivy-use-virtual-buffers t)
(setq enable-recursive-minibuffers t)
(global-set-key "\C-s" 'swiper)
(global-set-key (kbd "C-c C-r") 'ivy-resume)
(global-set-key (kbd "<f6>") 'ivy-resume)
(global-set-key (kbd "M-x") 'counsel-M-x)
;; (global-set-key (kbd "C-x C-f") 'counsel-find-file)
;; (global-set-key (kbd "<f1> f") 'counsel-describe-function)
;; (global-set-key (kbd "<f1> v") 'counsel-describe-variable)
;; (global-set-key (kbd "<f1> l") 'counsel-find-library)
;; (global-set-key (kbd "<f2> i") 'counsel-info-lookup-symbol)
;; (global-set-key (kbd "<f2> u") 'counsel-unicode-char)
(global-set-key (kbd "C-c g") 'counsel-git)
(global-set-key (kbd "C-c j") 'counsel-git-grep)
(global-set-key (kbd "C-c k") 'counsel-ag)
(global-set-key (kbd "C-x l") 'counsel-locate)
(global-set-key (kbd "C-S-o") 'counsel-rhythmbox)
(define-key minibuffer-local-map (kbd "C-r") 'counsel-minibuffer-history)

;; swap left-option and left-command keys
(when (eq system-type 'darwin)
  (defvar ns-command-modifier "meta"))

(provide '00base)
;;; 00base.el ends here
