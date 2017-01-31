;;; init.el --- Emacs configuration -*- lexical-binding: t; -*-

(require 'package)
(require 'eldoc)


;; (add-to-list 'load-path "~/.emacs.d/my-lisp")
;;(load-file "~/.emacs.d/my-lisp/my-php.el")


;; turn on ede mode
;; (global-ede-mode t)

;; add package archives
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/"))

;; list the packages I want
(setq package-list
      '(
	ac-anaconda
	alchemist
	anaconda-mode
	apib-mode
	auto-complete
	auto-complete-c-headers
	ace-jump-mode
	cider
	company
	company-anaconda
	company-jedi
	company-php
	company-restclient
	company-tern
	csharp-mode
	css-eldoc
	csv-mode
	elixir-mode
	emmet-mode
	emms
	evil
	evil-surround
	evil-nerd-commenter
	exec-path-from-shell
	expand-region
	flycheck
	helm
	helm-projectile
	js2-mode
	js-comint
	key-chord
	linum-relative
	magit
	markdown-mode
	multiple-cursors
	;; omnisharp
	paredit
	plantuml-mode
	projectile
	rainbow-delimiters
	rake
	restclient
	robe
	sass-mode
	scss-mode
	shut-up
	smartparens
	string-utils
	undo-tree
	vue-mode
	web-mode
	which-key
	yasnippet
	
	;; themes
	gruvbox-theme
	monokai-theme
	solarized-theme
	color-theme-sanityinc-tomorrow
	
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

;; python
(elpy-enable)


;; (global-set-key (kbd "s-x") nil)

(setq inhibit-startup-screen t)

;; shells and stuff...?
;;(setq comint-prompt-read-only t)
(setq comint-scroll-to-bottom-on-input t)
(setq comint-scroll-to-bottom-on-output t)
(setq comint-move-point-for-output t)

;; set path
(exec-path-from-shell-initialize)

;; themes
;;(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")
;;(load-theme 'gruvbox t)
(load-theme 'solarized-dark t)
;;(load-theme 'solarized-light t)
;; (load-theme 'zenburn t)
;;(load-theme 'monokai t)
;;(require 'color-theme-sanityinc-tomorrow)
;;(color-theme-sanityinc-tomorrow 'night)
;;(color-theme-sanityinc-tomorrow-night)
;;(color-theme-sanityinc-tomorrow-day)

;; fonts
;; (set-default-font "Source Code Pro Regular-14")
(set-default-font "Monaco-14" nil t)
;;(set-default-font "Inconsolata-dz for Powerline-14" nil t)

;; backups
(setq backup-directory-alist `(("." . "~/.saves")))

;; handy tweaks
(scroll-bar-mode -1)
(menu-bar-mode -1)
(tool-bar-mode -1)
(global-linum-mode -1) ; display line numbers
(column-number-mode 1) ; display column/row of cursor in mode-line
(show-paren-mode -1)

;; relative line numbers
(require 'linum-relative)
(linum-mode t)

(setq dired-dwim-target t)

;; auto-complete
;;(require 'auto-complete)
;;;; do default config for auto-complete
;;(require 'auto-complete-config)
;;(ac-config-default)
;;(setq-default ac-sources '(ac-source-semantic))

;; start yasnippet with emacs
(require 'yasnippet)
(yas-global-mode 1)
(define-key global-map (kbd "H-s") 'yas-insert-snippet)

;; lets define a function which initializes auto-complete-c-headers and gets called for c/c++
(defun my-ac-c-header-init ()
  (require 'auto-complete-c-headers)
  (require 'ede)
  (ede-mode t)
  (add-to-list 'ac-sources 'ac-source-c-headers)
  (add-to-list 'achead:include-directories '"/Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/bin/../lib/clang/7.3.0/include")
  (add-to-list 'achead:include-directories '"/usr/local/include")
  (add-to-list 'achead:include-directories '"/Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/include")
  (add-to-list 'achead:include-directories '"/usr/include")
  )
;; now lets call this function from c/c++ hooks
;;(add-hook 'c++-mode-hook 'my-ac-c-header-init)
;;(add-hook 'c-mode-hook 'my-ac-c-header-init)
(add-hook 'c-mode-hook (lambda ()
			 (semantic-mode t)
			 (company-clang t)))


;; flycheck
;;(global-flycheck-mode)

(define-key global-map (kbd "H-u") 'universal-argument)

;; evil mode
(setq evil-want-C-u-scroll t)
(setq evil-want-C-d-scroll t)
(setq evil-want-C-i-jump t)
(require 'evil)
(setq evil-default-state 'emacs)
;;(evil-mode t)
(require 'evil-surround)
(global-evil-surround-mode t)
(require 'evil-matchit)
(global-evil-matchit-mode 1)

;; normal state mappings
(define-key evil-normal-state-map (kbd "SPC ;") 'ace-jump-char-mode)
;; (define-key evil-normal-state-map (kbd ";") 'evil-ex)
(define-key evil-normal-state-map (kbd "SPC c c") 'evilnc-comment-or-uncomment-lines)
;;(define-key evil-normal-state-map (kbd "-") 'dired)

;; ;; visual state mappings
(define-key evil-visual-state-map (kbd "SPC ;") 'ace-jump-char-mode)
;; ;; (define-key evil-visual-state-map (kbd ";") 'evil-ex)
(define-key evil-visual-state-map (kbd "SPC c c") 'evilnc-comment-or-uncomment-lines)

;; insert state mappings
;; Exit insert mode by pressing j and then j quickly
;; (setq key-chord-two-keys-delay 0.5)
;; (key-chord-define evil-insert-state-map "jk" 'evil-normal-state)
;; (key-chord-mode nil)
(define-key evil-insert-state-map (kbd "C-SPC") 'company-complete)
(define-key evil-insert-state-map (kbd "C-h") 'backward-delete-char)

;; ;; tramp
;; (setq tramp-default-method "ssh")

;; multiple cursors
(require 'multiple-cursors)

;; rake
(setq rake-enable-caching nil)
(setq rake-completion-system 'helm)

;; run shell command in new frame
(define-key global-map (kbd "H-&") (lambda (command)
				     (interactive "sCommand: ")
				     (save-excursion
				       (make-frame)
				       (async-shell-command command))))
  

;; frame management
(define-key global-map (kbd "H-C-o") (lambda () (interactive) (other-frame 1)))
(define-key global-map (kbd "H-C-O") (lambda () (interactive) (other-frame -1)))
(define-key global-map (kbd "H-C-n") (lambda () (interactive) (make-frame)))

;; window management
(define-key global-map (kbd "H-M-o") (lambda () (interactive) (other-window 1)))
(define-key global-map (kbd "H-M-O") (lambda () (interactive) (other-window -1)))
(define-key global-map (kbd "H-M-n") (lambda () (interactive)
				     (split-window-below)
				     (windmove-down)))
(define-key global-map (kbd "H-M-h") 'windmove-left)
(define-key global-map (kbd "H-M-j") 'windmove-down)
(define-key global-map (kbd "H-M-k") 'windmove-up)
(define-key global-map (kbd "H-M-l") 'windmove-right)
(define-key global-map (kbd "H-M-H") 'evil-window-move-far-left)
(define-key global-map (kbd "H-M-J") 'evil-window-move-very-bottom)
(define-key global-map (kbd "H-M-K") 'evil-window-move-very-top)
(define-key global-map (kbd "H-M-L") 'evil-window-move-far-right)
;; (define-key global-map (kbd "H-M-h") (lambda () (interactive)
;; 				       (evil-window-vsplit)))
;; (define-key global-map (kbd "H-M-j") (lambda () (interactive)
;; 				       (progn
;; 					 (evil-window-split)
;; 					 (windmove-down))))
;; (define-key global-map (kbd "H-M-k") (lambda () (interactive)
;; 				       (progn
;; 					 (evil-window-split))))
;; (define-key global-map (kbd "H-M-l") (lambda () (interactive)
;; 				       (progn
;; 					 (evil-window-vsplit)
;; 					 (windmove-right))))
;;(define-key global-map (kbd "H-o") 'delete-other-windows)
;;(define-key global-map (kbd "H-c") 'delete-window)

;; helm
(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "H-x x") 'execute-extended-command)
(global-set-key (kbd "C-x C-f") 'helm-find-files)
(helm-mode 1)
(setq helm-follow-mode 'follow)
(define-key global-map (kbd "C-x b") 'helm-buffers-list)
;;(define-key global-map (kbd "H-m h i m") 'helm-imenu)
(define-key global-map (kbd "H-.") 'helm-etags-select)
(define-key global-map (kbd "H-i") 'helm-imenu)

;; wind move
(when (fboundp 'windmove-default-keybindings)
  (windmove-default-keybindings))

;; TODO: fix these hooks...i think i should only
;;       have a single mode hook per mode
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
(define-key global-map (kbd "H-SPC") 'company-complete)
(define-key global-map (kbd "C-c c") 'company-complete)
;; https://www.reddit.com/r/emacs/comments/3s5bkf/companymode_configuration_make_editing_slow/?st=is0w3bc0&sh=f6db1e9c
(global-company-mode 1)

;; (add-to-list 'company-backends 'company-tern)
;;(setq company-idle-delay nil) ; never start completions automatically

; (define-key global-map (kbd "s-i") 'company-complete)
(setq company-show-numbers t)

;; powerline
;;(require 'powerline)
;;(powerline-default-theme)


;; ace-jump
(define-key global-map (kbd "C-c j") 'ace-jump-word-mode)
(define-key global-map (kbd "H-;") 'ace-jump-char-mode)
;; (define-key global-map (kbd "H-c") 'ace-jump-char-mode)
(define-key global-map (kbd "H-w") 'ace-jump-word-mode)
(define-key global-map (kbd "H-c") 'ace-jump-char-mode)

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
  ;;(setq mac-command-modifier 'meta)
  ;;(setq mac-option-modifier 'super)
  )

;; shell
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)
(defun preamble-regexp-alternatives (regexps)
  "Return the alternation of a list of regexps."
  (mapconcat (lambda (regexp)
               (concat "\\(?:" regexp "\\)"))
             regexps "\\|"))

(defvar non-sgr-control-sequence-regexp nil
  "Regexp that matches non-SGR control sequences.")

(setq non-sgr-control-sequence-regexp
      (preamble-regexp-alternatives
       '(;; icon name escape sequences
         "\033\\][0-2];.*?\007"
         ;; non-SGR CSI escape sequences
         "\033\\[\\??[0-9;]*[^0-9;m]"
         ;; noop
         "\012\033\\[2K\033\\[1F"
         )))

(defun filter-non-sgr-control-sequences-in-region (begin end)
  (save-excursion
    (goto-char begin)
    (while (re-search-forward
            non-sgr-control-sequence-regexp end t)
      (replace-match ""))))

(defun filter-non-sgr-control-sequences-in-output (ignored)
  (let ((start-marker
         (or comint-last-output-start
             (point-min-marker)))
        (end-marker
         (process-mark
          (get-buffer-process (current-buffer)))))
    (filter-non-sgr-control-sequences-in-region
     start-marker
     end-marker)))

(add-hook 'comint-output-filter-functions
          'filter-non-sgr-control-sequences-in-output)


;; js-comint
(setq inferior-js-program-command "node")
(setq inferior-js-program-arguments '("/Users/derick/node_repl.js"))
(js-do-use-nvm)
(add-hook 'js2-mode-hook '(lambda ()
                            (local-set-key "\C-x\C-e" 'js-send-last-sexp)
                            (local-set-key "\C-\M-x" 'js-send-last-sexp-and-go)
                            (local-set-key "\C-cb" 'js-send-buffer)
                            (local-set-key "\C-c\C-b" 'js-send-buffer-and-go)
                            (local-set-key "\C-cl" 'js-load-file-and-go)))

;; node
(setenv "NODE_NO_READLINE" "1")

;; js2-mode
(add-to-list 'auto-mode-alist '("\\.json\\'" . js-mode))
(add-to-list 'auto-mode-alist '("\\.tern-project\\'" . js-mode))
(add-to-list 'auto-mode-alist '("\\.babelrc\\'" . js-mode))
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
(add-to-list 'interpreter-mode-alist '("node" . js2-mode))
;;(add-to-list 'auto-mode-alist '("\\.jsx?\\'" . js2-jsx-mode))
;;(add-to-list 'interpreter-mode-alist '("node" . js2-jsx-mode))



;; tern
(add-to-list 'load-path "~/.emacs.d/repos/tern/emacs/")
(autoload 'tern-mode "tern.el" nil t)
(add-hook 'js-mode-hook (lambda ()
			  (add-to-list 'company-backends 'company-tern)
			  ;; (add-to-list 'company-backends 'company-files)
			  ;; (add-to-list 'company-backends 'company-keywords)
			  ;;(company-mode t)
			  ;;(setq company-idle-delay 0.1)
			  ;;(setq company-show-numbers t)
			  (linum-relative-on)
			  (linum-relative-mode t)
			  (setq js-indent-level 2)
			  (tern-mode t)))
(add-hook 'js2-mode-hook (lambda ()
			   (add-to-list 'company-backends 'company-tern)
			   ;; (add-to-list 'company-backends 'company-files)
			   ;; (add-to-list 'company-backends 'company-keywords)
			   ;;(company-mode t)
			   ;;(setq company-idle-delay 0.1)
			   ;;(setq company-show-numbers t)
			   (linum-relative-mode t)
			   (linum-relative-on)
			   (tern-mode t)
			   (setq js2-basic-offset 2)))

;; geiser
(require 'geiser)

;; vue-mode
(require 'vue-mode)
(add-to-list 'auto-mode-alist '("\\.vue\\'" . vue-mode))
(add-hook 'vue-mode-hook 'tern-mode)


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
;;(setq projectile-keymap-prefix (kbd "C-c C-p"))
(require 'projectile)
(projectile-global-mode)
(setq projectile-completion-system 'helm)
(helm-projectile-on)
(setq projectile-switch-project-action 'helm-projectile)


;; expand-region
(require 'expand-region)
(global-set-key (kbd "H-e") 'er/expand-region)

;; magit
(define-key global-map (kbd "H-x g s") 'magit-status)
(define-key global-map (kbd "H-x g i") 'magit-init)

;; markdown
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))
;;(add-to-list 'auto-mode-alist '("\\.markdown\\'" . gfm-mode))
;;(add-to-list 'auto-mode-alist '("\\.md\\'" . gfm-mode))

;; php
(add-hook 'php-mode-hook
          (lambda ()
	    ;; (require 'company-php)
	    ;;(require 'php-auto-yasnippets)
	    
	    ;; modes
	    (eldoc-mode t)
	    (smartparens-mode t)
	    (company-mode t)
	    (linum-relative-mode t)
	    ;; (flycheck-mode t)
	    

	    ;; variables
	    (setq c-basic-offset 2)	 
	    (setq tab-width 2)		 

	    ;; company-mode configuration
	    (set (make-local-variable 'company-backends)
		 '((
		    company-etags
		    ;; company-dabbrev-code
		    company-yasnippet
		    company-dabbrev
		    company-files
		    ;; company-ac-php-backend
		    )))
	    
	    (setq-local company-idle-delay )
	    (setq-local company-dabbrev-code-other-buffers t) ;; irrelevant
	    (setq-local company-dabbrev-ignore-buffers "nil")
	    (setq-local company-dabbrev-downcase nil)
	    (setq-local company-dabbrev-ignore-case "case-replace")
	    (setq-local company-dabbrev-code-ignore-case "case-replace")

	    ;; mappings
	    (define-key php-mode-map (kbd "H-m H-s") 'yas/create-php-snippet)

	    ;; yasnippets
	    (setq-local yas-snippet-dirs
		  '("~/.emacs.d/snippets"                 ;; personal snippets
		    ))
	    ))

(defun helm-imenu-all-buffers-in-new-frame ()
  "docstring"
  (interactive)
  (make-frame)
  (helm-imenu-in-all-buffers))

;;(require 'php-mode)
;;(define-key php-mode-map (kbd "H-m i") 'helm-imenu-all-buffers-in-new-frame)

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
(add-to-list 'auto-mode-alist '("\\.hbs\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))

(setq web-mode-engines-alist
      '(("php"    . "\\.phtml\\'")
        ("blade"  . "\\.blade\\."))
      )

(add-to-list 'web-mode-comment-formats '("blade" . "{{-- "))

(setq web-mode-comment-style 2)

(defun custom-web-mode-hook ()
  "Hooks for Web mode."
  (emmet-mode t)
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-code-indent-offset 2)
  (setq web-mode-javascript-indentation 2)
  (setq web-mode-enable-auto-pairing t)
  (linum-relative-mode t)
  (linum-relative-on)
  )

(add-hook 'web-mode-hook  'custom-web-mode-hook)

(require 'emmet-mode)
(add-hook 'sgml-mode-hook 'emmet-mode) ;; Auto-start on any markup modes
(add-hook 'html-mode-hook 'emmet-mode)
(add-hook 'css-mode-hook  'emmet-mode)

;; restclient
(add-to-list 'company-backends 'company-restclient)

;; clojure
(add-hook 'cider-repl-mode-hook #'eldoc-mode)

;; ruby
(defun my-ruby-hook ()
  "My ruby hook"
  (eldoc-mode t)
  (robe-mode t)
  ;;(evil-normal-state t)
  (smartparens-mode t)
  ;;(setq company-idle-delay nil) ; never start completions automatically
  ;;(auto-complete-mode nil)
  (company-mode t)
  (linum-relative-mode t)
  (linum-relative-on)
  ;;(setq company-idle-delay 0.1)
  ;;(setq company-show-numbers t)
  )

(add-hook 'ruby-mode-hook 'my-ruby-hook)

(eval-after-load 'company
  '(push 'company-robe company-backends))

;; php
(defun my-brace-and-bracket-close-action (id action context)
  "Create line between braces or backets and indent. "
  (when (eq action 'insert)
    (newline-and-indent)
    (forward-line -1)
    (indent-according-to-mode)))

(sp-with-modes '(php-mode js-mode js2-mode csharp-mode)
  (sp-local-pair "{" "}"
		 :when '(("RET"))
		 :post-handlers '(:add my-brace-and-bracket-close-action)
		 :actions '(insert)))

(sp-with-modes '(php-mode)
  (sp-local-pair "[" "]"
		 :when '(("RET"))
		 :post-handlers '(:add my-brace-and-bracket-close-action)
		 :actions '(insert)))

;; elixir
(defun my-elixir-do-end-close-action (id action context)
  (when (eq action 'insert)
    (newline-and-indent)
    (forward-line -1)
    (indent-according-to-mode)))

(sp-with-modes '(elixir-mode)
  (sp-local-pair "->" "end"
                 :when '(("RET"))
                 :post-handlers '(:add my-elixir-do-end-close-action)
                 :actions '(insert)))

(sp-with-modes '(elixir-mode)
  (sp-local-pair "do" "end"
                 :when '(("SPC" "RET"))
                 :post-handlers '(:add my-elixir-do-end-close-action)
                 :actions '(insert)))

(defun my-elixir-hook ()
  "My elixir hook"
  (eldoc-mode t)
  (alchemist-mode t)
  ;;(relative-line-numbers-mode)
  ;; (linum-mode t)
  (linum-relative-mode t)
  (linum-relative-on)
  (smartparens-mode t))

(add-hook 'elixir-mode-hook 'my-elixir-hook)


;; (eval-after-load 'company
;;   '(add-to-list 'company-backends 'company-omnisharp))

;; csharp
;; omnisharp-emacs
(add-to-list 'load-path "~/.emacs.d/repos/omnisharp-emacs")
(autoload 'omnisharp-mode "omnisharp.el" nil t)
(add-hook 'csharp-mode-hook (lambda ()
			      (omnisharp-mode t)
			      (smartparens-mode t)
			      (flycheck-mode t)
			      (eldoc-mode t)
			      (setq company-idle-delay 0.1)
			      (add-to-list 'company-backends 'company-omnisharp)
			      ;; company-mode configuration
			      (set (make-local-variable 'company-backends)
				   '(company-omnisharp))
			      (linum-relative-mode t)))
(setq omnisharp-server-executable-path "/Users/derick/.repositories/github/OmniSharp/omnisharp-roslyn/artifacts/scripts/OmniSharp")
;; (setq omnisharp-server-executable-path "/Users/derick/.repositories/github/OmniSharp/omnisharp-server/OmniSharp/bin/Debug/OmniSharp.exe")

;; org
;;(setq org-src-fontify-natively t)
;; active Org-babel languages
(org-babel-do-load-languages
 'org-babel-load-languages
 '(;; other Babel languages
   (plantuml . t)))
(add-to-list
  'org-src-lang-modes '("plantuml" . plantuml))

(setq org-plantuml-jar-path "~/plantuml.jar")
(add-hook 'plantuml-mode-hook (lambda ()
                                (interactive)
                                (setq-default indent-tabs-mode nil)
                                (setq-default tab-width 2)
                                (setq indent-line-function 'insert-tab)
                                (linum-relative-mode)
                                ))



;; lets define a function which adds semantic as a suggestion backend to auto complete andhook this function to c-mode-common-hook
;; (defun my-add-semantic-to-autocomplete ()
;;   (add-to-list 'ac-sources 'ac-source-semantic))

;; (add-hook 'c-mode-common-hook 'my-add-semantic-to-autocomplete)

;; (defun my-php-init ()
;;   '(lambda ()
;;      (require 'ac-php)
;;      (semantic-mode t)
;;      (setq ac-sources '(ac-source-php))))

;;(add-to-list 'load-path "~/.emacs.d/repos/ede-php-autoload")
;;(require 'ede-php-autoload-mode)
;;(add-hook 'php-mode-hook #'semantic-mode)
;; (add-hook 'php-mode-hook #'ede-php-autoload-mode)
;; (add-hook 'php-mode-hook 'my-add-semantic-to-autocomplete)
;;(ede-php-autoload-project "Demo project" :file "~/workspace/demos/php/emacs/composer.json")


(require 'emms-setup)
(emms-standard)
(emms-default-players)
;; (emms-add-directory-tree "~/Dropbox/Apps/Easy Voice Recorder")
(let ((dir "~/Dropbox/Apps/Easy Voice Recorder"))
  (if (file-accessible-directory-p dir)
      (emms-add-directory-tree dir)))


(put 'dired-find-alternate-file 'disabled nil)


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" "10e231624707d46f7b2059cc9280c332f7c7a530ebc17dba7e506df34c5332c4" default)))
 '(package-selected-packages
   (quote
    (elpy plantuml-mode emms fzf zenburn-theme zenburn which-key web-mode vue-mode tco string-utils sr-speedbar solarized-theme smyx-theme smartparens shut-up scss-mode sass-mode robe rake rainbow-delimiters php-auto-yasnippets perspective paredit multiple-cursors monokai-theme moe-theme magit linum-relative key-chord js2-mode js-comint jdee icicles helm-projectile gruvbox-theme geiser geben expand-region exec-path-from-shell evil-surround evil-nerd-commenter evil-matchit emmet-mode doom-themes dashboard csv-mode css-eldoc csharp-mode company-tern company-restclient company-php company-jedi company-anaconda color-theme-sanityinc-tomorrow cider auto-complete-c-headers apib-mode anti-zenburn-theme alchemist ace-jump-mode ac-php ac-anaconda))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
