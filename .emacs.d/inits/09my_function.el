(require 'align)
(add-to-list 'align-rules-list
            '(php-assignment
              (regexp . "[^-=!^&*+<>/.| \t\n]\\(\\s-*[.-=!^&*+<>/|]*\\)=>?\\(\\s-*\\)\\([^= \t\n]\\|$\\)")
              (justify . t)
              (tab-stop . nil)
              (modes . '(php-mode))))
(add-to-list 'align-dq-string-modes 'php-mode)
(add-to-list 'align-sq-string-modes 'php-mode)
(add-to-list 'align-open-comment-modes 'php-mode)
(setq align-region-separate (concat "\\(^\\s-*$\\)\\|"
                                   "\\([({}\\(/\*\\)]$\\)\\|"
                                   "\\(^\\s-*[)}\\(\*/\\)][,;]?$\\)\\|"
                                   "\\(^\\s-*\\(}\\|for\\|while\\|if\\|else\\|"
                                   "switch\\|case\\|break\\|continue\\|do\\)[ ;]\\)"
                                   ))
(add-to-list 'align-rules-list
             '(ruby-comma-delimiter
               (regexp . ",\\(\\s-*\\)[^# \t\n]")
               (repeat . t)
               (modes  . '(ruby-mode))))

(add-to-list 'align-rules-list
             '(ruby-hash-literal
               (regexp . "\\(\\s-*\\)=>\\s-*[^# \t\n]")
               (group 2 3)
               (repeat . t)
               (modes  . '(ruby-mode))))

(add-to-list 'align-rules-list
             '(ruby-hash-literal2
               (regexp . "[a-z0-9]:\\(\\s-*\\)[^# \t\n]")
               (repeat . t)
               (modes  . '(ruby-mode))))

(add-to-list 'align-rules-list
             '(ruby-assignment-literal
               (regexp . "\\(\\s-*\\)=\\s-*[^# \t\n]")
               (repeat . t)
               (modes  . '(ruby-mode))))

(add-to-list 'align-rules-list
             '(ruby-xmpfilter-mark
               (regexp . "\\(\\s-*\\)# => [^#\t\n]")
               (repeat . nil)
               (modes  . '(ruby-mode))))
;; 文字列を"で囲む
(global-set-key "\C-x\"" 'surround)
(defun surround (begin end string)
  (interactive  "r\nsString:")
  (when (string= string "{")
    (setq open string)
    (setq close "}"))
  (when (string= string "(")
    (setq open string)
    (setq close ")"))
  (when (string= string "\"")
    (setq open string)
    (setq close "\""))
  (when (string= string "\'")
    (setq open string)
    (setq close "\'"))
  (save-excursion
    (goto-char end)
    (insert close)
    (goto-char begin)
    (insert open)))

;; copy to clipboard on OS X
;; REF: https://gist.github.com/the-kenny/267162
(defun copy-from-osx ()
 (shell-command-to-string "pbpaste"))

(defun paste-to-osx (text &optional push)
 (let ((process-connection-type nil))
     (let ((proc (start-process "pbcopy" "*Messages*" "pbcopy")))
       (process-send-string proc text)
       (process-send-eof proc))))

(setq interprogram-cut-function 'paste-to-osx)
(setq interprogram-paste-function 'copy-from-osx)

(provide '09my_function)
