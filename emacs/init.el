(require 'package)
(require 'eldoc)

;; add package archives
(add-to-list
 'package-archives
 '("melpa" . "http://melpa.org/packages/")
 t)

;; list the packages I want
(setq package-list '(
		     ace-jump-mode
		     company
		     company-tern
		     css-eldoc
		     helm
		     js2-mode
		     js-comint
		     magit
		     paredit
		     rainbow-delimiters
		     smartparens
		     which-key

		     ;; themes
		     gruvbox-theme
		     monokai-theme
		     solarized-theme
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

(require 'helm-config)
(require 'smartparens-config)
(require 'css-eldoc)
(require 'ace-jump-mode)
(require 'rainbow-delimiters)
(require 'js-comint)

;; load themes
;;(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")

;; remove scrollbar, menubar, and toolbar
(scroll-bar-mode -1)
(menu-bar-mode -1)
(tool-bar-mode -1)

;; helm
(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "C-x C-f") 'helm-find-files)
(helm-mode 1)

;; wind move
(when (fboundp 'windmove-default-keybindings)
  (windmove-default-keybindings))

;; smartparens
(add-hook 'js-mode-hook #'smartparens-mode)

;; eldoc
(add-hook 'emacs-lisp-mode-hook 'eldoc-mode)
(add-hook 'lisp-interaction-mode-hook 'eldoc-mode)
(add-hook 'ielm-mode-hook 'eldoc-mode)
(add-hook 'css-mode-hook 'eldoc-mode)

;; css-eldoc
(add-hook 'css-mode-hook 'css-eldoc-enable)

;; company
(add-hook 'after-init-hook 'global-company-mode)
(define-key global-map (kbd "H-SPC") 'company-complete)
(with-eval-after-load 'company
  (add-to-list 'company-backends 'company-tern))

;; ace-jump
(define-key global-map (kbd "C-;") 'ace-jump-word-mode)

;; rainbow delimiters
(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)

;; which key
(which-key-mode)


;; mac osx specific
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

(load-theme 'gruvbox t)
