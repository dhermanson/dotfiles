(require 'package)
  (add-to-list
   'package-archives
   '("melpa" . "http://melpa.org/packages/")
   t)
  (package-initialize)

(when (memq window-system '(mac ns))
  (setq ns-function-modifier 'hyper)
  (global-set-key (kbd "H-SPC") 'ace-jump-word-mode)
  (global-set-key (kbd "H-S-SPC") 'ace-jump-char-mode)

  (global-set-key (kbd "H-j") "(")
  (global-set-key (kbd "H-k") ")")
  (global-set-key (kbd "H-h") "[")
  (global-set-key (kbd "H-l") "]")
  (global-set-key (kbd "H-i") "{")
  (global-set-key (kbd "H-o") "}"))


(scroll-bar-mode -1)
(menu-bar-mode -1)
(tool-bar-mode -1)

;; org-mode stuff
(defun org-summary-todo (n-done n-not-done)
       "Switch entry to DONE when all subentries are done, to TODO otherwise."
       (let (org-log-done org-log-states)   ; turn off logging
         (org-todo (if (= n-not-done 0) "DONE" "TODO"))))
     
(add-hook 'org-after-todo-statistics-hook 'org-summary-todo)


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("d43120398682953ef18fd7e11e69c94e44d39bb2ab450c4e64815311542acbff" "aed73c6d0afcf2232bb25ed2d872c7a1c4f1bda6759f84afc24de6a1aec93da8" "b6db49cec08652adf1ff2341ce32c7303be313b0de38c621676122f255ee46db" "e24679edfdea016519c0e2d4a5e57157a11f928b7ef4361d00c23a7fe54b8e01" default))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
