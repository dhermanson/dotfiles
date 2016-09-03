(require 'package)
(require 'eldoc)

;; add package archives
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/"))

;; list the packages I want
(setq package-list
      '(
	ace-jump-mode
	company
	company-php
	company-tern
	css-eldoc
	helm
	helm-projectile
	js2-mode
	js-comint
	magit
	markdown-mode
	paredit
	projectile
	rainbow-delimiters
	robe
	sass-mode
	scss-mode
	smartparens
	web-mode
	which-key
	
	;; themes
	gruvbox-theme
	monokai-theme
	solarized-theme
	
	;; php
	php-mode
	ac-php
	))

;; activate all the packages (in particular autoloads)
(package-initialize)

;; fetch the list of packages available 
(unless package-archive-contents
  (package-refresh-contents))

;; install the missing packages
(dolist (package package-list)
  (unless (package-installed-p package)
    (package-install package)))

(require 'company)
(require 'helm-config)
(require 'smartparens-config)
(require 'css-eldoc)
(require 'ace-jump-mode)
(require 'rainbow-delimiters)
(require 'js-comint)
(require 'web-mode)

(global-set-key (kbd "s-x") nil)

;; set path
(exec-path-from-shell-initialize)

;; themes
;;(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")
(load-theme 'gruvbox t)

;; fonts
(set-default-font "Source Code Pro Regular-14")

;; handy tweaks
(scroll-bar-mode -1)
(menu-bar-mode -1)
(tool-bar-mode -1)
(global-linum-mode -1) ; display line numbers
(column-number-mode 1) ; display column/row of cursor in mode-line
(show-paren-mode -1)

;; tramp
(setq tramp-default-method "ssh")

;; windmove
(define-key global-map (kbd "H-h") 'windmove-left)
(define-key global-map (kbd "H-j") 'windmove-down)
(define-key global-map (kbd "H-k") 'windmove-up)
(define-key global-map (kbd "H-l") 'windmove-right)

;; helm
(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "C-x C-f") 'helm-find-files)
(helm-mode 1)
(define-key global-map (kbd "C-x b") 'helm-buffers-list)

;; wind move
(when (fboundp 'windmove-default-keybindings)
  (windmove-default-keybindings))

;; TODO: fix these hooks...i think i should only
;;       have a single mode hook per mode
;; smartparens
(add-hook 'js-mode-hook #'smartparens-mode)
(add-hook 'php-mode-hook #'smartparens-mode)

;; eldoc
(add-hook 'emacs-lisp-mode-hook 'eldoc-mode)
(add-hook 'lisp-interaction-mode-hook 'eldoc-mode)
(add-hook 'ielm-mode-hook 'eldoc-mode)
(add-hook 'css-mode-hook 'eldoc-mode)
(add-hook 'php-mode-hook 'eldoc-mode)


;; css-eldoc
(add-hook 'css-mode-hook 'css-eldoc-enable)

;; company
;; https://www.reddit.com/r/emacs/comments/3s5bkf/companymode_configuration_make_editing_slow/?st=is0w3bc0&sh=f6db1e9c
(global-company-mode 1)
(add-to-list 'company-backends 'company-tern)
;;(setq company-idle-delay nil) ; never start completions automatically
(setq company-idle-delay 0.1)
(define-key global-map (kbd "s-i") 'company-complete)
(setq company-show-numbers t)



;; ace-jump
(define-key global-map (kbd "s-SPC") 'ace-jump-char-mode)

;; rainbow delimiters
(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)

;; which key
(which-key-mode)
(setq which-key-side-window-max-width 0.5)
(setq which-key-side-window-max-height 0.5)
(which-key-setup-side-window-bottom)
(setq which-key-idle-delay 1.0)
(setq which-key-max-description-length 50)


;; mac osx specific
(when (memq window-system '(mac ns))
  (setq ns-function-modifier 'hyper)
  )

;; js-comint
(js-do-use-nvm)
(add-hook 'js2-mode-hook '(lambda ()
                            (local-set-key "\C-x\C-e" 'js-send-last-sexp)
                            (local-set-key "\C-\M-x" 'js-send-last-sexp-and-go)
                            (local-set-key "\C-cb" 'js-send-buffer)
                            (local-set-key "\C-c\C-b" 'js-send-buffer-and-go)
                            (local-set-key "\C-cl" 'js-load-file-and-go)))

;; js2-mode
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
(add-to-list 'interpreter-mode-alist '("node" . js2-mode))
;;(add-to-list 'auto-mode-alist '("\\.jsx?\\'" . js2-jsx-mode))
;;(add-to-list 'interpreter-mode-alist '("node" . js2-jsx-mode))

;; tern
(add-to-list 'load-path "~/.emacs.d/repos/tern/emacs/")
(autoload 'tern-mode "tern.el" nil t)
(add-hook 'js-mode-hook (lambda () (tern-mode t)))

;; org-mode stuff
(defun org-summary-todo (n-done n-not-done)
       "Switch entry to DONE when all subentries are done, to TODO otherwise."
       (let (org-log-done org-log-states)   ; turn off logging
         (org-todo (if (= n-not-done 0) "DONE" "TODO"))))
     
(add-hook 'org-after-todo-statistics-hook 'org-summary-todo)

;; paredit
(autoload 'enable-paredit-mode "paredit" "Turn on pseudo-structural editing of Lisp code." t)
(add-hook 'emacs-lisp-mode-hook       #'enable-paredit-mode)
(add-hook 'eval-expression-minibuffer-setup-hook #'enable-paredit-mode)
(add-hook 'ielm-mode-hook             #'enable-paredit-mode)
(add-hook 'lisp-mode-hook             #'enable-paredit-mode)
(add-hook 'lisp-interaction-mode-hook #'enable-paredit-mode)
(add-hook 'scheme-mode-hook           #'enable-paredit-mode)

;; projectile
(projectile-global-mode)
(setq projectile-completion-system 'helm)
(helm-projectile-on)
(setq projectile-switch-project-action 'helm-projectile)

;; magit
(define-key global-map (kbd "s-x g s") 'magit-status)
(define-key global-map (kbd "s-x g i") 'magit-init)

;; markdown
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))
;;(add-to-list 'auto-mode-alist '("\\.markdown\\'" . gfm-mode))
;;(add-to-list 'auto-mode-alist '("\\.md\\'" . gfm-mode))

;; php
(add-hook 'php-mode-hook
          '(lambda ()
             (require 'company-php)
             (company-mode t)
             (add-to-list 'company-backends 'company-ac-php-backend )))

;; sass-mode
(add-to-list 'auto-mode-alist '("\\.scss\\'" . scss-mode))
(add-to-list 'auto-mode-alist '("\\.sass\\'" . sass-mode))

;; web-mode
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.[agj]sp\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))

(setq web-mode-engines-alist
      '(("php"    . "\\.phtml\\'")
        ("blade"  . "\\.blade\\."))
      )

(defun custom-web-mode-hook ()
  "Hooks for Web mode."
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-code-indent-offset 2)
  (setq web-mode-enable-auto-pairing t))

(add-hook 'web-mode-hook  'custom-web-mode-hook)

;; ruby
(defun my-ruby-hook ()
  "My ruby hook"
  (eldoc-mode t)
  (robe-mode t))

(add-hook 'ruby-mode-hook 'my-ruby-hook)


;;(add-hook 'ruby-mode-hook 'robe-mode)
;;(add-hook 'ruby-hook 'eldoc-mode)
(eval-after-load 'company
  '(push 'company-robe company-backends))

;;(global-set-key (kbd "H-SPC") 'ace-jump-word-mode)
;;(global-set-key (kbd "H-S-SPC") 'ace-jump-char-mode)

;; (global-set-key (kbd "H-j") "(")
;; (global-set-key (kbd "H-k") ")")
;; (global-set-key (kbd "H-,") "[")
;; (global-set-key (kbd "H-.") "]")
;; (global-set-key (kbd "H-i") "{")
;; (global-set-key (kbd "H-o") "}")

