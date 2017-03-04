;; auto-completion
(require 'company)
(global-company-mode)
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
(setq dired-listing-switches (purecopy "-ahl"))
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

;; Other
(prefer-coding-system 'utf-8) ;; 文字コード utf-8
(windmove-default-keybindings) ;; Windownsの移動 Shift-arrow_key
(electric-pair-mode t) ;; 自動の閉じ括弧
(show-paren-mode t) ;; 括弧の強調表示
(auto-revert-mode) ;; 自動revert-buffer
(column-number-mode) ;; カラム数
(menu-bar-mode -1)

;; whitespace (build-in)
(require 'whitespace)
(setq whitespace-style '(face          ; faceで可視化
                        trailing       ; 行末
                        tabs           ; タブ
                        spaces         ; スペース
                        empty          ; 先頭/末尾の空行
                        space-mark     ; 表示のマッピング
                        tab-mark
                        ))
(setq whitespace-display-mappings
     '((space-mark ?\u3000 [?\u25a1])))
(setq whitespace-space-regexp "\\(\u3000+\\)") ;; スペースは全角のみを可視化
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
(global-set-key "\C-xl" 'goto-line)
(global-set-key "\C-xp" 'my_rails_fzf)
(require 'fzf)
(defun my_rails_fzf()
  (interactive)
  (fzf-directory (replace-regexp-in-string "\\(.*jp_projects/[^/]+/\\).*" "\\1" default-directory)))

(provide '00base)
