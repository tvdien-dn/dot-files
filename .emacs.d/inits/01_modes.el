;;; package --- Summary
;;; Commentary:

;;; Code:
;; magit
(use-package magit
  :ensure t
  :config
  (setq magit-completing-read-function 'ivy-completing-read)
  )

;; Google translate settings
(use-package google-translate
  :ensure t
  :config
  (global-set-key "\C-ct" 'google-translate-smooth-translate)
  (setq google-translate-translation-directions-alist
    '(("en" . "ja") ("ja" . "en") ("ja" . "vi") ("en" . "vi")))
  )


;; Ruby
(use-package ruby-mode
  :ensure t
  :config
  (add-hook 'ruby-mode-hook '(lambda()
                               (setq flycheck-checker 'ruby-rubocop)
                               (setq flycheck-disabled-checkers '(ruby-rubylint))
                               (setq ruby-insert-encoding-magic-comment nil)
                               (flycheck-mode 1)))
  )

;; PHP
(use-package ac-php :ensure t)
(use-package company-php :ensure t)
(use-package php-mode
  :ensure t
  :mode "\\.\\(module\\|test\\|install\\|theme\\|inc\\)$"
  :config
  (add-hook 'php-mode-hook
          '(lambda ()
             (setq php-mode-force-pear t)
             (setq tab-width 4)
             (setq c-basic-offset 4)
             (setq indent-tabs-mode nil)
             (c-set-offset 'case-label' 4)
             (c-set-offset 'arglist-intro' 4)
             (c-set-offset 'arglist-cont-nonempty' 4)
             (c-set-offset 'arglist-close' 0)
             (require 'company-php)
             (auto-complete-mode t)
             (company-mode t)
             (ac-php-core-eldoc-setup) ;; enable eldoc
             (make-local-variable 'company-backends)
             (add-to-list 'company-backends 'company-ac-php-backend)
             ;; (define-key php-mode-map  (kbd "C-]") 'ac-php-find-symbol-at-point)   ;goto define
             ;; (define-key php-mode-map  (kbd "C-t") 'ac-php-location-stack-back)    ;go back
             ))
  )

(use-package web-mode
  :ensure t
  :mode "/\\(public\\|includes\\|views\\|html\\|theme\\|templates\\)/.*\\.php\\'"
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
       (setq web-mode-script-offset 2)
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
              (setq flycheck-checker 'javascript-eslint)
              ;; (setq flycheck-disabled-checkers '(javascript-jshint javascript-jscs))
              )))))
  (add-to-list 'auto-mode-alist '("\\.js[x]?\\'" . (lambda () (web-mode) (js-flycheck-settings))))
  )

(use-package json-mode
  :ensure t
  :mode "\\.json\\'"
  )

(use-package lua-mode
  :ensure t
  :mode "\\.lua\\'"
  :init
  (add-hook 'lua-mode-hook
          '(lambda ()
             (setq lua-indent-level 2)))
  )

(use-package json-mode
  :init
  (add-hook 'json-mode-hook '(lambda ()
                               (setq js-indent-level 2))))
(use-package json-reformat)

;; SQL mode
(use-package sql
  :config
  (add-hook 'sql-mode-hook '(lambda ()
                              (setq sql-indent-offset 2)
                              (setq indent-tabs-mode nil)
                              (setq sql-mysql-login-params (append sql-mysql-login-params '(port)))
                              (sql-set-product "mysql")
                              (eval-after-load "sql" '(load-library "sql-indent"))
                              (sqlind-minor-mode))))
(use-package sql-indent
  :ensure t
  :after sql)

;; Docker
(use-package dockerfile-mode :ensure t)
(use-package docker-compose-mode :ensure t)

;; Ansible
(use-package ansible :ensure t)

;; CSS

;; JS
(use-package vue-mode
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

;; Flycheck
(use-package flycheck-color-mode-line
  :ensure t)
(use-package flycheck
  :requires flycheck-color-mode-line
  :ensure t
  :config
  (flycheck-add-mode 'javascript-eslint 'web-mode)
  (eval-after-load 'flycheck
    '(add-hook 'flycheck-mode-hook 'flycheck-color-mode-line-mode))
  (add-hook 'after-init-hook #'global-flycheck-mode)
  )

(use-package anzu
  :ensure t
  :config
  (custom-set-variables
    '(anzu-mode-lighter "")
    '(anzu-deactivate-region t)
    '(anzu-search-threshold 1000))
  )
(provide '01_modes)
;;01_modes ends here
