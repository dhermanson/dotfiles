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
(require-package 'evil-nerd-commenter)
(require-package 'evil-matchit)
(require-package 'powerline-evil)
(require-package 'ace-jump-mode)
(require-package 'base16-theme)
(require-package 'rvm)
(require-package 'robe)
(require-package 'evil-surround)
(require-package 'smartparens)
(require-package 'neotree)
(require-package 'auto-indent-mode)

(setq make-backup-files nil)

(tool-bar-mode -1)
(scroll-bar-mode -1)
(menu-bar-mode -1)

(require 'rvm)
(rvm-use-default)
(add-hook 'ruby-mode-hook 'robe-mode)

(require 'evil)
(require 'evil-leader)
(require 'evil-matchit)
(require 'ace-jump-mode)
(require 'powerline)
(require 'smartparens-config)
(require 'neotree)
(require 'auto-indent-mode)

(auto-indent-global-mode)

;; powerline settings
(powerline-evil-vim-color-theme)
(display-time-mode t)

(setq evil-leader/in-all-states 1)

(global-evil-leader-mode)
(global-evil-matchit-mode 1)

(evil-leader/set-leader ",")
(evil-leader/set-key
  "b" 'switch-to-buffer
  "k" 'kill-buffer
  ", w" 'ace-jump-word-mode
  ", c" 'ace-jump-char-mode
  "q" 'neotree-toggle
  "ci" 'evilnc-comment-or-uncomment-lines
  "cl" 'evilnc-quick-comment-or-uncomment-to-the-line
  "ll" 'evilnc-quick-comment-or-uncomment-to-the-line
  "cc" 'evilnc-copy-and-comment-lines
  "cp" 'evilnc-comment-or-uncomment-paragraphs
  "cr" 'comment-or-uncomment-region
  "cv" 'evilnc-toggle-invert-comment-line-by-line)

(evil-mode 1)
(global-evil-surround-mode 1)

(load-theme 'base16-monokai-dark)

(add-hook 'prog-mode-hook 'linum-mode t)
(add-hook 'prog-mode-hook 'smartparens-mode t)

(add-hook 'neotree-mode-hook
            (lambda ()
              (define-key evil-normal-state-local-map (kbd "o") 'neotree-enter)
              (define-key evil-normal-state-local-map (kbd "q") 'neotree-hide)
              (define-key evil-normal-state-local-map (kbd "r") 'neotree-refresh)
              (define-key evil-normal-state-local-map (kbd "i") 'neotree-hidden-file-toggle)
              (define-key evil-normal-state-local-map (kbd "m a") 'neotree-create-node)
              (define-key evil-normal-state-local-map (kbd "m m") 'neotree-rename-node)
              (define-key evil-normal-state-local-map (kbd "m d") 'neotree-delete-node)
              (define-key evil-normal-state-local-map (kbd "c d") 'neotree-change-root)
              (define-key evil-normal-state-local-map (kbd "RET") 'neotree-enter)))


 
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes (quote ("3328e7238e0f6d0a5e1793539dfe55c2685f24b6cdff099c9a0c185b71fbfff9" "37783713b151d949b0da66ff7cd8736dd0893089cbad12eb5a71f3a72e201b47" "5d139820639cd941c60033dcdd462bf5fffa76da549e6bdf1d83945803d30f01" "9f3a4edb56d094366afed2a9ba3311bbced0f32ca44a47a765d8ef4ce5b8e4ea" "d72836155cd3b3e52fd86a9164120d597cbe12a67609ab90effa54710b2ac53b" "3539b3cc5cbba41609117830a79f71309a89782f23c740d4a5b569935f9b7726" "6ebb2401451dc6d01cd761eef8fe24812a57793c5ccc427b600893fa1d767b1d" "cda6cb17953b3780294fa6688b3fe0d3d12c1ef019456333e3d5af01d4d6c054" "0240d45644b370b0518e8407f5990a243c769fb0150a7e74297e6f7052a04a72" "75c0b1d2528f1bce72f53344939da57e290aa34bea79f3a1ee19d6808cb55149" default)))
 '(display-time-mode t)
 '(tool-bar-mode nil))
 
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "DejaVu Sans Mono" :foundry "PfEd" :slant normal :weight normal :height 98 :width normal)))))
