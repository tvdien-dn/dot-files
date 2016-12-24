;; web-mode
(add-to-list 'auto-mode-alist '("\\.inc$" . php-mode)) ;; - For Drupal
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

;; css-mode
(setq css-indent-offset 2)

;; ruby-mode
(require 'rubocop)
(add-hook 'ruby-mode-hook
          '(lambda()
             (setq flycheck-checker 'ruby-rubocop)
             (flycheck-mode 1)))
(require 'flycheck-color-mode-line)
(eval-after-load "flycheck"
  '(add-hook 'flycheck-mode-hook 'flycheck-color-mode-line-mode))
(setq ruby-insert-encoding-magic-comment nil)
(provide '01modes)
