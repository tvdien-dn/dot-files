;;; 01modes --- setting for each mode
;;; Commentary:

;;; Code:
(add-to-list 'auto-mode-alist '("\\.\\(module\\|test\\|install\\|theme\\|inc\\)$" . php-mode)) ;; - For Drupal
(add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.blade\\.php\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.jsp\\'" . web-mode))
;; (add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("/\\(public\\|includes\\|views\\|html\\|theme\\|templates\\)/.*\\.php\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.js[x]?\\'" . (lambda ()
                                                   (web-mode)
                                                   (js-flycheck-settings))))
(add-to-list 'auto-mode-alist '("\\.json\\'" . json-mode))
(add-to-list 'auto-mode-alist '("\\.lua\\'" . lua-mode))


;; JSON mode
'(json-reformat:indent-width 2)
(add-hook 'json-mode-hook
          '(lambda ()
             (message "aaa")
             (setq js-indent-level 2)))
;; WEB mode
(add-hook 'web-mode-hook
          '(lambda ()
             (setq js-indent-level 2)
             (setq web-mode-attr-indent-offset 2)
             (setq web-mode-markup-indent-offset 2)
             (setq web-mode-css-indent-offset 2)
             (setq web-mode-code-indent-offset 2)
             (setq web-mode-sql-indent-offset 2)
             (setq web-mode-html-offset   2)
             (setq web-mode-script-offset 2)
             (setq indent-tabs-mode nil)
             (setq tab-width 2)
             (setq web-mode-enable-auto-closing t) ;; 自動HTMLタグを閉じる
             (setq web-mode-auto-close-style 1)
             (setq web-mode-tag-auto-close-style t)
             (setq web-mode-enable-auto-pairing t) ;; 自動組み込みタグを閉じる
             )
          )
;; LUA mode
(add-hook 'lua-mode-hook
          '(lambda ()
             (setq lua-indent-level 2)))
;; SQL mode
(add-hook 'sql-mode-hook
          '(lambda ()
             (setq sql-indent-offset 2)
             (setq indent-tabs-mode nil)
             (setq sql-mysql-login-params (append sql-mysql-login-params '(port)))
             (sql-set-product "mysql")))
(eval-after-load "sql" '(load-library "sql-indent"))
;; shell script mode indentation
(add-hook 'shell-script-mode
          '(lambda ()
             (setq sh-basic-offset 2)
             (setq sh-indentation 2)
             (setq sh-indent-for-case-label 0)
             (setq sh-indent-for-case-alt '+)))
;; php-mode
(add-hook 'php-mode-hook
          '(lambda ()
             (setq php-mode-force-pear t)
             (setq tab-width 4)
             (setq c-basic-offset 4)
             (setq indent-tabs-mode nil)
             (c-set-offset 'case-label' 4)
             (c-set-offset 'arglist-intro' 4)
             (c-set-offset 'arglist-cont-nonempty' 4)
             (c-set-offset 'arglist-close' 0)))
;; Flycheck
(require 'flycheck)
(flycheck-add-mode 'javascript-eslint 'web-mode)
(eval-after-load 'flycheck
  '(add-hook 'flycheck-mode-hook 'flycheck-color-mode-line-mode))
(add-hook 'after-init-hook #'global-flycheck-mode)
;;;; js-mode
(defun js-flycheck-settings ()
  "Set checker for JS."
  (add-hook 'web-mode-hook
            '(lambda ()
               (eval-after-load 'flycheck
                 '(lambda ()
                    (setq flycheck-checker 'javascript-eslint)
                    ;; (setq flycheck-disabled-checkers '(javascript-jshint javascript-jscs))
                    )))))
;; ruby-mode
(add-hook 'ruby-mode-hook
          '(lambda()
             (setq flycheck-checker 'ruby-rubocop)
             (setq flycheck-disabled-checkers '(ruby-rubylint))
             (setq ruby-insert-encoding-magic-comment nil)
             (flycheck-mode 1)))

(provide '01modes)

;;; 01modes.el ends here
