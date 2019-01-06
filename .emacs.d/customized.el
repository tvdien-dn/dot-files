;;; package --- Summary
;;; Commentary:
;;; Code:
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(company-dabbrev-downcase nil)
 '(company-idle-delay 0.5)
 '(company-minimum-prefix-length 2)
 '(company-selection-wrap-around t)
 '(company-tooltip-align-annotations t)
 '(company-tooltip-flip-when-above t)
  '(custom-safe-themes
     (quote
       ("f523744a93ed8dce610c57c0de09977685fb24375a86dfd47e858766c9dc0415" "9399db70f2d5af9c6e82d4f5879b2354b28bc7b5e00cc8c9d568e5db598255c4" default)))
 '(dired-dwim-target t)
  '(package-selected-packages
     (quote
       (powerline rainbow-mode ivy-mode delight which-key zop-to-char dired company-php ac-php ansible flycheck-color-mode-line docker-compose-mode dockerfile-mode google-translate-smooth-ui google-translate auto-package-update magit counsel projectile fzf use-package-ensure-system-package undo-tree editorconfig framemove use-package init-loader el-init)))
 '(sml/col-number-format "%3c")
 '(sml/line-number-format "%4l")
 '(sml/shorten-directory t)
 '(sml/shorten-modes t)
 '(sml/show-eol t)
 '(sml/theme (quote dark)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:background "#000000"))))
 '(flycheck-color-mode-line-error-face ((t (:inherit flycheck-fringe-error :inverse-video t))))
 '(flycheck-color-mode-line-warning-face ((t (:inherit flycheck-fringe-warning :inverse-video t))))
 '(highlight ((t (:background "#585858" :foreground "#00ff00"))))
 '(mode-line ((t (:background "#303035" :foreground "gray60" :inverse-video nil :box (:line-width 2 :color "#303035") :weight normal))))
 '(mode-line-inactive ((t (:background "#000000" :foreground "gray60" :inverse-video nil :box (:line-width 2 :color "#303035") :weight bold))))
 '(region ((t (:inherit highlight)))))

(provide 'customized)
;;; customized.el ends here
