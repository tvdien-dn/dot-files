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
 '(flycheck-color-mode-line-error-face ((t (:inherit flycheck-fringe-error :inverse-video t))))
 '(flycheck-color-mode-line-warning-face ((t (:inherit flycheck-fringe-warning :inverse-video t))))
 '(highlight ((t (:background "#585858" :foreground "#00ff00"))))
 '(region ((t (:inherit highlight)))))

(provide 'customized)
;;; customized.el ends here
