;;; init: --- common settings
;;; Commentary:

(require 'package)

;;;; Code:
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))
(add-to-list 'package-archives
             '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(fset 'package-desc-version 'package--ac-desc-version)
(package-initialize)

(require 'el-init)
(el-init-load "~/.emacs.d/inits")
(load-theme 'desert t)
;; Emacs server
(require 'server)
(unless (server-running-p)
  (server-start))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(column-number-mode t)
 '(custom-safe-themes
   (quote
    ("e26e879d250140e0d4c4d5ab457c32bcb29742599bd28c1ce31301344c6f2a11" "57d7e8b7b7e0a22dc07357f0c30d18b33ffcbb7bcd9013ab2c9f70748cfa4838" "31772cd378fd8267d6427cec2d02d599eee14a1b60e9b2b894dd5487bd30978e" "6b4f7bde1ce64ea4604819fe56ff12cda2a8c803703b677fdfdb603e8b1f8bcb" "ec0c9d1715065a594af90e19e596e737c7b2cdaa18eb1b71baf7ef696adbefb0" "0f302165235625ca5a827ac2f963c102a635f27879637d9021c04d845a32c568" "80a23d559a5c5343a0882664733fd2c9e039b4dbf398c70c424c8d6858b39fc5" "1342a81078bdac27f80b86807b19cb27addc1f9e4c6a637a505ae3ba4699f777" "0973b33d2f15e6eaf88400eee3dc8357ad8ae83d2ca43c125339b25850773a70" "11e5e95bd3964c7eda94d141e85ad08776fbdac15c99094f14a0531f31a156da" "45482e7ddf47ab1f30fe05f75e5f2d2118635f5797687e88571842ff6f18b4d5" "011d4421eedbf1a871d1a1b3a4d61f4d0a2be516d4c94e111dfbdc121da0b043" "b4fd44f653c69fb95d3f34f071b223ae705bb691fb9abaf2ffca3351e92aa374" "2d5c40e709543f156d3dee750cd9ac580a20a371f1b1e1e3ecbef2b895cf0cd2" "fe349b21bb978bb1f1f2db05bc87b2c6d02f1a7fe3f27584cd7b6fbf8e53391a" "deb7ae3a735635a85c984ece4ce70317268df6027286998b0ea3d10f00764c9b" default)))
 '(markdown-command "/usr/local/bin/markdown")
 '(package-selected-packages
   (quote
    (ac-js2 js2-mode js2-refactor adoc-mode format-sql plantuml-mode flymake-lua lua-mode fzf markdown-preview-mode google-translate ag jinja2-mode mmm-jinja2 highlight-indent-guides sql-indent framemove anything-tramp markdown-preview-eww flymd vue-mode egg hideshow-org anzu yaml-mode color-theme-modern undo-tree markdown-mode php-mode flycheck-color-mode-line flycheck editorconfig rubocop web-mode web-mode-edit-element smart-mode-line xelb el-init company))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(flycheck-error ((t (:background "green" :box (:line-width 2 :color "grey75" :style released-button)))))
 '(flycheck-error-list-id-with-explainer ((t (:inherit flycheck-error-list-id :box (:line-width 1 :style released-button)))))
 '(flycheck-error-list-info ((t (:inherit success))))
 '(flycheck-fringe-info ((t (:inherit success :background "red"))))
 '(flycheck-info ((t (:inherit success :background "orange"))))
 '(font-lock-type-face ((t (:foreground "PaleGreen3" :weight bold)))))
(set-face-background 'default "black")

(provide 'init)
;;; init ends here
