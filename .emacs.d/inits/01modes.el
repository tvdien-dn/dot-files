;;; web-mode --- summary
;;; web-mode:

(add-to-list 'auto-mode-alist '("\\.inc$" . php-mode)) ;;; - For Drupal
(add-to-list 'auto-mode-alist '("\\.\\(module\\|test\\|install\\|theme\\)$" . php-mode)) ;; - For Drupal
(add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.blade\\.php\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.jsp\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("/\\(public\\|includes\\|views\\|html\\|theme\\|templates\\)/.*\\.php\\'" . web-mode))
(setq web-mode-enable-auto-closing t) ;; 自動HTMLタグを閉じる
(setq web-mode-auto-close-style 1)
(setq web-mode-tag-auto-close-style t)
(setq web-mode-enable-auto-pairing t) ;; 自動組み込みタグを閉じる

;; Customizations
(setq web-mode-markup-indent-offset 2)
(setq web-mode-css-indent-offset 2)
(setq web-mode-code-indent-offset 2)

;; (setq web-mode-disable-autocompletion t)
;; (local-set-key (kbd "RET") 'newline-and-indent)

;; インデント設定
;; php-mode
(setq php-mode-force-pear t)
(add-hook 'php-mode-hook
          (lambda ()
            (setq tab-width 4)
            (setq c-basic-offset 4)
            (setq indent-tabs-mode nil)))
(add-hook 'php-mode-hook
          (lambda ()
            (c-set-offset 'case-label' 4)
            (c-set-offset 'arglist-intro' 4)
            (c-set-offset 'arglist-cont-nonempty' 4)
            (c-set-offset 'arglist-close' 0)))

;; css-mode
(setq css-indent-offset 2)

;; js-mode
(setq js-indent-level 2)

;; ruby-mode
(add-hook 'ruby-mode-hook
          '(lambda()
             (setq flycheck-checker 'ruby-rubocop)
             (flycheck-mode 1)))
(require 'flycheck-color-mode-line)
(eval-after-load "flycheck"
  '(add-hook 'flycheck-mode-hook 'flycheck-color-mode-line-mode))
(add-hook 'ruby-mode-hook (lambda () (setq flycheck-disabled-checkers '(ruby-rubylint))))

(setq ruby-insert-encoding-magic-comment nil)

;; sql-indent
(eval-after-load "sql"
  '(load-library "sql-indent"))
(defun sql-mode-hooks()
  (setq sql-indent-offset 2)
  (setq indent-tabs-mode nil)
  (setq sql-mysql-login-params (append sql-mysql-login-params '(port)))
  (sql-set-product "mysql"))
(add-hook 'sql-mode-hook 'sql-mode-hooks)

;; markdown-preview mode
;; (custom-set-variables '(markdown-command "/usr/local/bin/markdown"))

;;(add-to-list 'markdown-preview-stylesheets "https://github.com/sindresorhus/github-markdown-css/blob/gh-pages/github-markdown.css")
;; (setq markdown-preview-stylesheets (list "https://github.com/sindresorhus/github-markdown-css/blob/gh-pages/github-markdown.css"))
(provide '01modes)
