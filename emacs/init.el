(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))

(defun ensure-package-installed (&rest packages)
  "Assure every package is installed, ask for installation if it’s not.

Return a list of installed packages or nil for every skipped package."
  (mapcar
   (lambda (package)
     (if (package-installed-p package)
         nil
       (if (y-or-n-p (format "Package %s is missing. Install it? " package))
           (package-install package)
         package)))
   packages))

;; Make sure to have downloaded archive description.
(or (file-exists-p package-user-dir)
    (package-refresh-contents))

;; Activate installed packages
(package-initialize)

;; Ensure packages installed
(ensure-package-installed
 'ace-jump-mode
 'ag
 'which-key
 'key-chord
 'helm
 'relative-line-numbers
 'smartparens

 ;; themes
 'base16-theme
 
 ;; tools
 'flycheck
 'company
 'ack
 'helm-ack

 ;; evil
 'evil
 'evil-leader
 'evil-matchit
 'evil-surround
 'evil-smartparens
 'evil-nerd-commenter
 'evil-magit
 
 ;; projectile
 'projectile
 'helm-projectile
 
 ;; git
 'magit
 
 ;; ruby
 'robe
 'rbenv
 
 ;; elixir
 'alchemist
 
 ;; php
 'php-mode)



;; general emacs appearance
(menu-bar-mode -1)
(toggle-scroll-bar -1)
(tool-bar-mode -1)
;;(global-linum-mode 1)

;; set evil-mode configuration
(setq evil-shift-width 2)
(setq evil-regexp-search t)
(setq evil-search-wrap t)
(setq evil-want-C-i-jump t)
(setq evil-want-C-u-scroll t)

(global-flycheck-mode)

(require 'evil)
(evil-mode 1)

;; include _'s as part of word
(modify-syntax-entry ?_ "w")

(require 'evil-matchit)
(global-evil-matchit-mode 1)

(require 'evil-surround)
(global-evil-surround-mode 1)

(require 'evil-magit)

(require 'key-chord)
(key-chord-mode 1)

(require 'smartparens-config)
;; Parens handling
;; Show and create matching parens automatically
(show-paren-mode t)
(show-smartparens-global-mode nil)
(setq sp-autoescape-string-quote nil)
;; Do not highlight paren area
(setq sp-highlight-pair-overlay nil)
(setq sp-highlight-wrap-overlay nil)
(setq sp-highlight-wrap-tag-overlay nil)
;; Do not use default slight delay
(setq show-paren-delay 0)

(require 'helm)
(global-set-key (kbd "M-x") 'helm-M-x)
(helm-mode 1)

(which-key-mode)

(global-company-mode t)
(setq company-tooltip-limit 12)                      ; bigger popup window
(setq company-idle-delay .1)                         ; decrease delay before autocompletion popup shows
(setq company-echo-delay 0)                          ; remove annoying blinking

;; projectile config
(setq projectile-enable-caching t)

;; show relative line numbers
(global-relative-line-numbers-mode)

(global-evil-leader-mode)
(evil-leader/set-leader "<SPC>")
(setq evil-leader/in-all-states 1)

;; use 'jk' to exit insert mode and enter normal mode
(key-chord-define evil-insert-state-map "jk" 'evil-normal-state)

;; use space-; for ace-jump character search
(define-key evil-normal-state-map (kbd "<SPC>;") 'evil-ace-jump-char-mode)
;; set valid keys when doing ace jump search
(setq ace-jump-mode-move-keys
      (nconc (cl-loop for i from ?a to ?z collect i)
	     (list ?a ?s ?d ?f ?j ?k ?l ?\;)))

(define-key evil-normal-state-map (kbd "RET") 'save-buffer)

;; evil-leader
(evil-leader/set-key
  "ws" 'evil-window-split
  "wv" 'evil-window-vsplit
  "wc" 'evil-window-delete
  "wo" 'delete-other-windows
  "wl" 'evil-window-right
  "wh" 'evil-window-left
  "wj" 'evil-window-down
  "wk" 'evil-window-up

  "cc" 'evilnc-comment-or-uncomment-lines

  "ns" 'evil-window-new
  "nv" 'evil-window-vnew

  "q" 'kill-buffer
  
  "s" 'helm-ack

  "b" 'helm-buffers-list
  "f" 'helm-find-files)


;; ruby
(global-rbenv-mode)
(add-to-list 'auto-mode-alist '("\\.rb\\'" . ruby-mode))
(add-hook 'ruby-mode-hook #'smartparens-mode)
(add-hook 'ruby-mode-hook 'turn-on-eldoc-mode)
(add-hook 'ruby-mode-hook 'robe-mode)
(eval-after-load 'company
  '(push 'company-robe company-backends))

;; elisp
;;(add-hook 'emacs-lisp-mode-hook #'smartparens-mode)

;;
;;(eval-after-load 'company
;;  '(push 'company-robe company-backends))
;;(add-hook 'ruby-mode-hook 'robe-start)
(add-hook 'ruby-mode-hook (lambda () (print "hello ruby!")))

(defun toggle-current-window-dedication ()
 (interactive)
 (let* ((window    (selected-window))
        (dedicated (window-dedicated-p window)))
   (set-window-dedicated-p window (not dedicated))
   (message "Window %sdedicated to %s"
            (if dedicated "no longer " "")
            (buffer-name))))

;;(define-key evil-normal-state-map (kbd "<SPC>w d") 'toggle-curent-window-dedication)

(load-theme 'base16-chalk-dark)



(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("cdfb22711f64d0e665f40b2607879fcf2607764b2b70d672ddaa26d2da13049f" "f21caace402180ab3dc5157d2bb843c4daafbe64aadc362c9f4558ac17ce43a2" "90b1aeef48eb5498b58f7085a54b5d2c9efef2bb98d71d85e77427ce37aec223" "aed73c6d0afcf2232bb25ed2d872c7a1c4f1bda6759f84afc24de6a1aec93da8" "e24679edfdea016519c0e2d4a5e57157a11f928b7ef4361d00c23a7fe54b8e01" "8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" default))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(global-set-key (kbd "M-;") 'evilnc-comment-or-uncomment-lines)
