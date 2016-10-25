(require 'package)
(require 'eldoc)


;; (add-to-list 'load-path "~/.emacs.d/my-lisp")
(load-file "~/.emacs.d/my-lisp/my-php.el")


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
	anaconda-mode
	auto-complete
	auto-complete-c-headers
	ace-jump-mode
	company
	company-anaconda
	company-jedi
	company-php
	company-tern
	css-eldoc
	csv-mode
	evil
	expand-region
	flycheck
	helm
	helm-projectile
	js2-mode
	js-comint
	key-chord
	magit
	markdown-mode
	paredit
	projectile
	rainbow-delimiters
	robe
	sass-mode
	scss-mode
	smartparens
	vue-mode
	web-mode
	which-key
	yasnippet
	
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

;; fonts
(set-default-font "Source Code Pro Regular-14")

;; backups
(setq backup-directory-alist `(("." . "~/.saves")))

;; handy tweaks
(scroll-bar-mode -1)
(menu-bar-mode -1)
(tool-bar-mode -1)
(global-linum-mode -1) ; display line numbers
(column-number-mode 1) ; display column/row of cursor in mode-line
(show-paren-mode -1)

;; auto-complete
;;(require 'auto-complete)
;;;; do default config for auto-complete
;;(require 'auto-complete-config)
;;(ac-config-default)
;;(setq-default ac-sources '(ac-source-semantic))

;; start yasnippet with emas
(require 'yasnippet)
(yas-global-mode 1)
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


;; ;; evil mode
;; (setq evil-want-C-u-scroll t)
;; (setq evil-want-C-d-scroll t)
;; (setq evil-want-C-i-jump t)
;; (require 'evil)
;; ;;(setq evil-default-state 'emacs)
;; (evil-mode)
;; (require 'evil-surround)
;; (global-evil-surround-mode t)

;; ;; normal state mappings
;; (define-key evil-normal-state-map (kbd "SPC ;") 'ace-jump-char-mode)
;; ;; (define-key evil-normal-state-map (kbd ";") 'evil-ex)
;; (define-key evil-normal-state-map (kbd "SPC c c") 'evilnc-comment-or-uncomment-lines)
;; (define-key evil-normal-state-map (kbd "-") 'dired)

;; ;; visual state mappings
;; (define-key evil-visual-state-map (kbd "SPC ;") 'ace-jump-char-mode)
;; ;; (define-key evil-visual-state-map (kbd ";") 'evil-ex)
;; (define-key evil-visual-state-map (kbd "SPC c c") 'evilnc-comment-or-uncomment-lines)

;; ;; insert state mappings
;; ;;Exit insert mode by pressing j and then j quickly
;; (setq key-chord-two-keys-delay 0.5)
;; (key-chord-define evil-insert-state-map "jk" 'evil-normal-state)
;; (key-chord-mode 1)

;; ;; tramp
;; (setq tramp-default-method "ssh")

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
(define-key global-map (kbd "s-SPC") 'company-complete)
;; https://www.reddit.com/r/emacs/comments/3s5bkf/companymode_configuration_make_editing_slow/?st=is0w3bc0&sh=f6db1e9c
(global-company-mode 1)
;; (add-to-list 'company-backends 'company-tern)
(setq company-idle-delay nil) ; never start completions automatically
;;(setq company-idle-delay 0.1)
; (define-key global-map (kbd "s-i") 'company-complete)
(setq company-show-numbers t)

;; powerline
;;(require 'powerline)
;;(powerline-default-theme)


;; ace-jump
(define-key global-map (kbd "H-SPC") 'ace-jump-char-mode)

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
			  ;;(company-mode t)
			  ;;(setq company-idle-delay 0.1)
			  ;;(setq company-show-numbers t)
			  (setq js-indent-level 2)
			  (tern-mode t)))
(add-hook 'js2-mode-hook (lambda ()
			   (add-to-list 'company-backends 'company-tern)
			   ;;(company-mode t)
			   ;;(setq company-idle-delay 0.1)
			   ;;(setq company-show-numbers t)
			   (tern-mode t)
			   (setq js2-basic-offset 2)))

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
(projectile-global-mode)
(setq projectile-completion-system 'helm)
(helm-projectile-on)
(setq projectile-switch-project-action 'helm-projectile)

;; expand-region
(require 'expand-region)
(global-set-key (kbd "C-=") 'er/expand-region)

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
(add-to-list 'auto-mode-alist '("\\.hbs\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))

(setq web-mode-engines-alist
      '(("php"    . "\\.phtml\\'")
        ("blade"  . "\\.blade\\."))
      )

(defun custom-web-mode-hook ()
  "Hooks for Web mode."
  (emmet-mode t)
  (setq web-mode-markup-indent-offset 4)
  (setq web-mode-css-indent-offset 4)
  (setq web-mode-code-indent-offset 4)
  (setq web-mode-javascript-indentation 4)
  (setq web-mode-enable-auto-pairing t))

(add-hook 'web-mode-hook  'custom-web-mode-hook)

(require 'emmet-mode)
(add-hook 'sgml-mode-hook 'emmet-mode) ;; Auto-start on any markup modes
(add-hook 'html-mode-hook 'emmet-mode)
(add-hook 'css-mode-hook  'emmet-mode)

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
  ;;(setq company-idle-delay 0.1)
  ;;(setq company-show-numbers t)
  )

(add-hook 'ruby-mode-hook 'my-ruby-hook)

(eval-after-load 'company
  '(push 'company-robe company-backends))


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
  (linum-mode t)
  (smartparens-mode t))

(add-hook 'elixir-mode-hook 'my-elixir-hook)



;; lets define a function which adds semantic as a suggestion backend to auto complete andhook this function to c-mode-common-hook
(defun my-add-semantic-to-autocomplete ()
  (add-to-list 'ac-sources 'ac-source-semantic))

(add-hook 'c-mode-common-hook 'my-add-semantic-to-autocomplete)

(defun my-php-init ()
  '(lambda ()
     (require 'ac-php)
     (semantic-mode t)
     (setq ac-sources '(ac-source-php))))

(add-to-list 'load-path "~/.emacs.d/repos/ede-php-autoload")
(require 'ede-php-autoload-mode)
(add-hook 'php-mode-hook #'semantic-mode)
(add-hook 'php-mode-hook #'ede-php-autoload-mode)
(add-hook 'php-mode-hook 'my-add-semantic-to-autocomplete)
(ede-php-autoload-project "Demo project" :file "~/workspace/demos/php/emacs/composer.json")

;; create a project for our program
;; (ede-cpp-root-project "my project" :file "~/temp.cpp")
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(sql-connection-alist
   (quote
    (("homestead"
      (sql-product
       (quote mysql))
      (sql-user "homestead")
      (sql-database "homestead")
      (sql-server "192.168.10.11"))))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
