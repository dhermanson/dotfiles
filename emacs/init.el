(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))

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
 'ggtags

 ;; themes
 'base16-theme
 'solarized-theme
 
 ;; tools
 'flycheck
 'company
 'ack
 'helm-ack

 ;; osx
 'exec-path-from-shell

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
 'php-mode
 'php-eldoc
 'ac-php
 'php-extras
 'psysh

 ;; haskell
 'haskell-mode

 ;; typescript
 'tide
 )


(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize))


;; general emacs appearance
(menu-bar-mode -1)
(toggle-scroll-bar -1)
(tool-bar-mode -1)


;; (global-flycheck-mode)

;;(require 'evil)

;; include _'s as part of word
(modify-syntax-entry ?_ "w")


;; Parens handling
;; Show and create matching parens automatically
(show-paren-mode t)
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

;; use space-; for ace-jump character search
;;(define-key evil-normal-state-map (kbd "<SPC>;") 'evil-ace-jump-char-mode)
;; set valid keys when doing ace jump search
(setq ace-jump-mode-move-keys
      (nconc (cl-loop for i from ?a to ?z collect i)
	     (list ?a ?s ?d ?f ?j ?k ?l ?\;)))

;; php
(add-hook 'php-mode-hook 'my-php-mode-hook)
(defun my-php-mode-hook ()
  "My PHP mode configuration."
  (setq indent-tabs-mode nil
	tab-width 2
	c-basic-offset 2)
  (flycheck-mode 0)
  (ggtags-mode t))
	  

;; ruby
(global-rbenv-mode)
(add-to-list 'auto-mode-alist '("\\.rb\\'" . ruby-mode))
(add-hook 'ruby-mode-hook 'turn-on-eldoc-mode)
(add-hook 'ruby-mode-hook 'robe-mode)
(eval-after-load 'company
  '(push 'company-robe company-backends))

;;
;;(eval-after-load 'company
;;  '(push 'company-robe company-backends))
;;(add-hook 'ruby-mode-hook 'robe-start)
(add-hook 'ruby-mode-hook (lambda () (print "hello ruby!")))

;; typescript
(defun toggle-current-window-dedication ()
 (interactive)
 (let* ((window    (selected-window))
        (dedicated (window-dedicated-p window)))
   (set-window-dedicated-p window (not dedicated))
   (message "Window %sdedicated to %s"
            (if dedicated "no longer " "")
            (buffer-name))))

(defun setup-tide-mode ()
  (interactive)
  (tide-setup)
  (flycheck-mode +1)
  (setq flycheck-check-syntax-automatically '(save mode-enabled))
  (eldoc-mode +1)
  ;; company is an optional dependency. You have to
  ;; install it separately via package-install
  (company-mode +1))

;; aligns annotation to the right hand side
(setq company-tooltip-align-annotations t)

;; formats the buffer before saving
(add-hook 'before-save-hook 'tide-format-before-save)

;; format options
(setq tide-format-options '(:insertSpaceAfterFunctionKeywordForAnonymousFunctions t :placeOpenBraceOnNewLineForFunctions nil))
;; see https://github.com/Microsoft/TypeScript/blob/cc58e2d7eb144f0b2ff89e6a6685fb4deaa24fde/src/server/protocol.d.ts#L421-473 for the full list available options

(add-hook 'typescript-mode-hook #'setup-tide-mode)



(load-theme 'base16-twilight-dark)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("1a2b131a7844bad234832963d565097efc88111b196fb75757885c159c5f8137" "d1a42ed39a15a843cccadf107ee0242b5f78bfbb5b70ba3ce19f3ea9fda8f52d" "b6db49cec08652adf1ff2341ce32c7303be313b0de38c621676122f255ee46db" "e24679edfdea016519c0e2d4a5e57157a11f928b7ef4361d00c23a7fe54b8e01" "d43120398682953ef18fd7e11e69c94e44d39bb2ab450c4e64815311542acbff" default))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
