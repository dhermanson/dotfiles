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
		     css-eldoc
		     magit
		     paredit
		     rainbow-delimiters
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

;; load themes
;;(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")

;; remove scrollbar, menubar, and toolbar
(scroll-bar-mode -1)
(menu-bar-mode -1)
(tool-bar-mode -1)

;; wind move
(when (fboundp 'windmove-default-keybindings)
  (windmove-default-keybindings))

;; eldoc
(add-hook 'emacs-lisp-mode-hook 'eldoc-mode)
(add-hook 'lisp-interaction-mode-hook 'eldoc-mode)
(add-hook 'ielm-mode-hook 'eldoc-mode)
(add-hook 'css-mode-hook 'eldoc-mode)

;; css-eldoc
(require 'css-eldoc)
(add-hook 'css-mode-hook 'css-eldoc-enable)

;; company
(add-hook 'after-init-hook 'global-company-mode)

;; ace-jump
(require 'ace-jump-mode)
(define-key global-map (kbd "C-;") 'ace-jump-word-mode)

;; rainbow delimiters
(require 'rainbow-delimiters)
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
