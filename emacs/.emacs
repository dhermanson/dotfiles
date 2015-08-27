(require 'package)
  (push '("marmalade" . "http://marmalade-repo.org/packages/")
        package-archives )
  (push '("melpa" . "http://melpa.milkbox.net/packages/")
        package-archives)
  (package-initialize)

(defun require-package (package)
  (setq-default highlight-tabs t)
  "Install given PACKAGE."
  (unless (package-installed-p package)
    (unless (assoc package package-archive-contents)
      (package-refresh-contents))
    (package-install package)))

(require-package 'evil)
(require-package 'evil-leader)
(require-package 'ace-jump-mode)
(require-package 'base16-theme)

(tool-bar-mode -1)
(scroll-bar-mode -1)
(menu-bar-mode -1)

(require 'evil)
(require 'evil-leader)
(require 'ace-jump-mode)

(setq evil-leader/in-all-states 1)
(global-evil-leader-mode)
(evil-leader/set-leader ",")
(evil-leader/set-key
  "b" 'switch-to-buffer
  "k" 'kill-buffer
  "SPC w" 'ace-jump-word-mode
  "SPC c" 'ace-jump-char-mode
  "SPC l" 'ace-jump-line-mode)

(evil-mode 1)

(load-theme 'base16-tomorrow-dark)

(add-hook 'prog-mode-hook 'linum-mode t)




(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes (quote ("75c0b1d2528f1bce72f53344939da57e290aa34bea79f3a1ee19d6808cb55149" default))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
