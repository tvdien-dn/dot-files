;;; package --- Summary
;;; Commentary:
;;; Code:
(require 'google-translate)
(require 'google-translate-smooth-ui)
(global-set-key "\C-ct" 'google-translate-smooth-translate)
;; Google translate smooth setting
(setq google-translate-translation-directions-alist
      '(("en" . "ja") ("ja" . "en") ("ja" . "vi") ("en" . "vi")))


(provide '90google-translate)
;;; 90google-translate.el ends here
