;;(setq package-check-signature nil)

(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)
(package-initialize)

(when (not package-archive-contents)
  (package-refresh-contents))

(if (not (package-installed-p 'use-package))
    (package-install 'use-package))

(require 'use-package)
(require 'bind-key)
;; home direcotyr should be default
(setq default-directory "~/")

(bind-key "C-x k" 'kill-this-buffer)

;; Disable toobar
(tool-bar-mode 0)

;; Disable prompts: get rid of yes or no and replace it with y or n
(fset 'yes-or-no-p 'y-or-n-p)

;; ESHELL
(message "Adding `emacs'")
(defun eshell/emacs (file)
  (find-file file))

(use-package solarized-theme
  :ensure t
  :init
  (load-theme 'solarized-light t)
  )

(use-package shell
  :bind ("C-x C-m" . shell)
  )

(use-package eshell
  :bind ("C-x m" . eshell)
  )

;; HELM
;(use-package helm
;  :ensure t
;  :init
;  (progn
;    (require 'helm-config)
;    (bind-key "C-c h" 'helm-command-prefix)
;    (unbind-key "C-x c")
;    
;    (bind-key "C-x C-f" 'helm-find-files)
;    (bind-key "C-x b" 'helm-mini)
;    (bind-key "M-x" 'helm-M-x)
;    (bind-key "C-x r b" 'helm-filtered-bookmarks)
;    (bind-key "C-c h o" 'helm-occur)
;    )
;  :config
;  (progn 
;    (helm-mode 1)
;    (bind-key "<tab>" 'helm-execut-parsisten-action helm-map)
;    (bind-key "C-i" 'helm-execut-parsisten-action helm-map)
;    (bind-key "C-z" 'helm-select-action helm-map)
;    )
;  :defer t)

;; DIRED-MODE
(use-package dired
  :config
  (progn
     (message "Loading dired hooks")
     (message "ALT dired-mode: Setting [z] to (`find-alternate-file' \"..\") ")
     (define-key dired-mode-map (kbd "z") (lambda () (interactive) (find-alternate-file "..")))  ; was dired-up-directory

     (when (equal user-login-name "cloud")
	   (defun dired-get-size ()
	     (interactive)
	     (let ((files (dired-get-marked-files)))
	       (with-temp-buffer
		 (apply 'call-process "/usr/bin/du" nil t nil "-sch" files)
		 (switch-to-buffer (current-buffer))
		 (message "Size of all marked files: %s"
			  (progn 
			    (re-search-backward "\\(^ *[0-9.,]+[A-Za-z]+\\).*total$")
			    (match-string 1))))))
	   
	   (define-key dired-mode-map (kbd "?") 'dired-get-size)

	   (load-file "~/.emacs.d/dired-compress.el"))
     )
  :defer t)

;; NXML-MODE
(use-package nxml-mode
  :init
  (add-to-list 'auto-mode-alist '("\\.html\\'" . nxml-mode))
  :config
  (progn
     (message "Loading nxml hooks")
     
     (setq nxml-sexp-element-flag t)
     ;; start kdb marco for nxml mode
     (fset 'close-xml-tag
	   [?\C-c ?\C-f ?\C-a return up tab])
     ;; end kdb macro for nxml-mode
     (message "ALT nxml-mode: Setting C-c C-g to `close-xml-tag'")
     (define-key nxml-mode-map (kbd "C-c C-g") 'close-xml-tag))
  :defer t)

(add-hook 'emacs-lisp-mode-hook 'show-paren-mode)

(use-package clojure-mode
  :ensure t
  :init
  (add-hook 'clojure-mode-hook 'show-paren-mode)
  :defer t)

(use-package paredit
  :ensure t
  :defer t)

; https://github.com/clojure-emacs/cider
(use-package cider
  :ensure t
  :init
  (progn
    (add-hook 'cider-mode-hook 'rainbow-delimiters-mode)
    (add-hook 'cider-mode-hook 'paredit-mode))
  :defer t)

; http://www.emacswiki.org/emacs/RainbowDelimiters
(use-package rainbow-delimiters
  :init
  (progn
    (add-hook 'clojure-mode-hook 'rainbow-delimiters-mode))
  :defer t)

;; PREFERENCES
(toggle-frame-maximized)
(if (equal user-login-name "cloud")
    (progn 
      (add-to-list 'default-frame-alist '(font . "Courier New-16"))
      (add-to-list 'default-frame-alist '(fullscreen . maximized))
      ))

(if (equal user-login-name "lyaksta")
    (progn
      (add-to-list 'default-frame-alist '(font . "Courier New-12"))
      (add-to-list 'default-frame-alist '(fullscreen . maximized))
      
      (setq comint-completion-addsuffix (quote ("\\" . " ")))
      
      (load-file "~/.emacs.d/abfx2.el")

      (load-file "~/.emacs.d/proxy.el")
      ))


(put 'dired-find-alternate-file 'disabled nil)

;; Prettify mode to show lambda like λ
;; http://ergoemacs.org/emacs/emacs_pretty_lambda.html
(global-prettify-symbols-mode 1)
