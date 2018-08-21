;;; init: --- common settings
;;; Commentary:

;;;; Code:
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))
(fset 'package-desc-version 'package--ac-desc-version)
(package-initialize)

;; load configurations from inits directory

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(eval-when-compile (require 'use-package))
 ;; auto install all specified packages which require by use-package syntax
(use-package use-package-ensure-system-package
  :ensure t
  :init
  (setq use-package-always-ensure t))

(use-package auto-package-update
  :ensure t
  :init
  (setq auto-package-update-delete-old-versions t)
  (setq auto-package-update-hide-results t)
  (auto-package-update-maybe))

(use-package el-init)
(el-init-load "~/.emacs.d/inits")


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(anzu-deactivate-region t)
 '(anzu-mode-lighter "")
 '(anzu-search-threshold 1000)
 '(package-selected-packages
   (quote
    (company-php ac-php ansible flycheck-color-mode-line docker-compose-mode dockerfile-mode google-translate-smooth-ui google-translate auto-package-update magit counsel projectile fzf use-package-ensure-system-package undo-tree editorconfig framemove use-package init-loader el-init))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(highlight ((t (:background "color-240" :foreground "brightyellow"))))
 '(region ((t (:inherit highlight)))))

(provide 'init)
;;; init ends here
