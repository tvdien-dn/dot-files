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

(use-package kaolin-themes
  :config
  (load-theme 'kaolin-dark t)
  )

(setq custom-file "~/.emacs.d/customized.el")
(when (file-exists-p custom-file)
  (load custom-file))

(provide 'init)
;;; init ends here
