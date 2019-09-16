;;; package --- Summary
;;; Commentary:

;;; Code:

;; Python

(use-package python-mode
  :ensure t
  :config
  (add-hook 'python-mode-hook
    '(lambda ()
       (setq python-indent-offset 4)
      )
   )
)
;; Ruby
(use-package ruby-mode
  :ensure t
  :config
  (setq ruby-insert-encoding-magic-comment nil)
  (add-hook 'ruby-mode-hook #'subword-mode)
  (add-hook 'ruby-mode-hook '(lambda()
                               (setq flycheck-checker 'ruby-rubocop)
                               (setq flycheck-disabled-checkers '(ruby-rubylint))
                               (flycheck-mode 1))))
;; PHP
(use-package ac-php :ensure t)
(use-package cl :ensure nil)
(use-package company-php :ensure t
  :config
  (ac-php-core-eldoc-setup)
  )
(use-package php-mode
  :ensure t
  :mode "\\.\\(module\\|test\\|install\\|theme\\|inc\\)$"
  :bind (:map php-mode-map
          ("C-]" . ac-php-find-symbol-at-point) ;; goto define
          ("C-t" . ac-php-location-stack-back)  ;; go back
          )
  :config
  (add-hook 'php-mode-hook
          '(lambda ()
             (setq php-mode-force-pear t)
             (c-set-offset 'case-label' 4)
             (c-set-offset 'arglist-intro' 4)
             (c-set-offset 'arglist-cont-nonempty' 4)
             (c-set-offset 'arglist-close' 0)
             (make-local-variable 'company-backends)
             (add-to-list 'company-backends 'company-ac-php-backend)
             ))
  )

(use-package web-mode :ensure t
  :mode "/\\(public\\|includes\\|include\\|views\\|html\\|theme\\|templates\\)/.*\\.php\\'"
  :mode "\\.erb\\'"
  :mode "\\.jsp\\'"
  :mode "\\.blade\\.php\\'"
  :mode "\\.tpl\\.php\\'"
  :mode "\\.as[cp]x\\'"
  :mode "\\.html?\\'"
  :config
  (add-hook 'web-mode-hook
    '(lambda ()
       (setq js-indent-level 2)
       (setq web-mode-attr-indent-offset 2)
       (setq web-mode-markup-indent-offset 2)
       (setq web-mode-css-indent-offset 2)
       (setq web-mode-code-indent-offset 2)
       (setq web-mode-sql-indent-offset 2)
       (setq web-mode-html-offset   2)
       ;; (setq web-mode-script-offset 2)
       (setq indent-tabs-mode nil)
       (setq tab-width 2)
       (setq web-mode-enable-auto-closing t) ;; 自動HTMLタグを閉じる
       (setq web-mode-auto-close-style 1)
       (setq web-mode-tag-auto-close-style t)
       (setq web-mode-enable-auto-pairing t) ;; 自動組み込みタグを閉じる
       )
    )
  (defun js-flycheck-settings ()
    "Set checker for JS."
    (add-hook 'web-mode-hook
      '(lambda ()
         (eval-after-load 'flycheck
           '(lambda ()
              (setq flycheck-checker 'javascript-eslint))))))
  (add-to-list 'auto-mode-alist '("\\.js[x]?\\'" . (lambda () (web-mode) (js-flycheck-settings))))
  )

(use-package json-reformat)
(use-package json-mode :ensure t
  :mode "\\.json\\'"
  :init
  (add-hook 'json-mode-hook '(lambda ()
                               (setq js-indent-level 2))))

(use-package lua-mode :ensure t
  :mode "\\.lua\\'"
  :init
  (add-hook 'lua-mode-hook
          '(lambda ()
             (setq lua-indent-level 2)))
  )

;; SQL mode
(use-package sql :ensure t
  :config
  (add-hook 'sql-mode-hook '(lambda ()
                              (setq sql-indent-offset 2)
                              (setq indent-tabs-mode nil)
                              (setq sql-mysql-login-params (append sql-mysql-login-params '(port)))
                              (sql-set-product "mysql")
                              (eval-after-load "sql" '(load-library "sql-indent"))
                              (sqlind-minor-mode))))
(use-package sql-indent :ensure t
  :after sql)

;; Docker
(use-package dockerfile-mode :ensure t)
(use-package docker-compose-mode :ensure t)

;; Ansible
(use-package ansible :ensure t)

;; CSS

;; JS
(use-package vue-mode :ensure t
  :config
  (setq mmm-submode-decoration-level 0))
(use-package vue-html-mode)

;; Shell mode
(add-hook 'shell-script-mode
  '(lambda ()
     (setq sh-basic-offset 2)
     (setq sh-indentation 2)
     (setq sh-indent-for-case-label 0)
     (setq sh-indent-for-case-alt '+)))

;; AsciiDoc

(use-package adoc-mode)
(provide '01_modes)

;; Gherkin/Cucumber major mode
(use-package feature-mode
  :mode "\\.feature\\'")

(eval-after-load "hideshow"
  '(add-to-list 'hs-special-modes-alist
    `(ruby-mode
      ,(rx (or "def" "class" "module" "do" "{" "[")) ; Block start
      ,(rx (or "}" "]" "end"))                       ; Block end
      ,(rx (or "#" "=begin"))                        ; Comment start
      ruby-forward-sexp nil)))

;;; 01_modes ends here
