(setq package-check-signature nil)

(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)
(package-initialize)

(when (not package-archive-contents)
  (package-refresh-contents))

(if (not (package-installed-p 'req-package))
    (package-install 'req-package))

(require 'req-package)

;; home direcotyr should be default
(setq default-directory "~/")

;; KEYBINDINGS
;; eshell or switch to it if it's active.
(global-set-key (kbd "C-x m") 'eshell)

;; Start a regular shell if you prefer that.
(global-set-key (kbd "C-x C-m") 'shell)

(global-set-key (kbd "C-x k") 'kill-this-buffer)

;; Disable toobar
(tool-bar-mode 0)

;; Disable prompts: get rid of yes or no and replace it with y or n
(fset 'yes-or-no-p 'y-or-n-p)

;; ESHELL
(message "Adding `emacs'")
(defun eshell/emacs (file)
  (find-file file))

;; HELM
(req-package helm
  :require helm-config
  :config
  (progn 
    (helm-mode 1)

    (global-set-key (kbd "C-x b") 'helm-buffers-list)
    (global-set-key (kbd "C-x C-r") 'helm-mini)
    (global-set-key (kbd "M-x") 'helm-M-x)
    (global-set-key (kbd "C-x r b") 'helm-filtered-bookmarks)
    
    ;; The default "C-x c" is quite close to "C-x C-c", which quits Emacs.
    ;; Changed to "C-c h". Note: We must set "C-c h" globally, because we
    ;; cannot change `helm-command-prefix-key' once `helm-config' is loaded.
    (global-set-key (kbd "C-c h") 'helm-command-prefix)
    (global-unset-key (kbd "C-x c"))

    (define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action) ; rebind tab to run persistent action
    (define-key helm-map (kbd "C-i") 'helm-execute-persistent-action) ; make TAB works in terminal
    (define-key helm-map (kbd "C-z")  'helm-select-action))) ; list actions using C-z

;; DIRED-MODE
(req-package dired
  :config
  (progn
     (message "Loading dired hooks")
     (message "ALT dired-mode: Setting [z] to (`find-alternate-file' \"..\") ")
     (define-key dired-mode-map (kbd "z") (lambda () (interactive) (find-alternate-file "..")))  ; was dired-up-directory

     (if (equal user-login-name "cloud")
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

       (load-file "~/.emacs.d/dired-compress.el")
       )
     ))

;; NXML-MODE
(req-package nxml-mode
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
     (define-key nxml-mode-map (kbd "C-c C-g") 'close-xml-tag)))

(add-hook 'emacs-lisp-mode-hook 'show-paren-mode)

(req-package clojure-mode
  :init
  (add-hook 'clojure-mode-hook 'show-paren-mode))

; https://github.com/clojure-emacs/cider
(req-package cider
  :require paredit
  :init
  (progn
    (add-hook 'cider-mode-hook 'rainbow-delimiters-mode)
    (add-hook 'cider-mode-hook 'paredit-mode)))

; http://www.emacswiki.org/emacs/RainbowDelimiters
(req-package rainbow-delimiters
  :init
  (progn
    (add-hook 'clojure-mode-hook 'rainbow-delimiters-mode)))

(req-package-finish)

;; PREFERENCES
(if (equal user-login-name "cloud")
    (progn 
      (add-to-list 'default-frame-alist '(font . "Courier New-16"))
      (add-to-list 'default-frame-alist '(fullscreen . maximized))
      (load-theme 'tango-dark)
      ))

(if (equal user-login-name "lyaksta")
    (progn
      (add-to-list 'default-frame-alist '(font . "Courier New-12"))
      (add-to-list 'default-frame-alist '(fullscreen . maximized))
      (load-theme 'tango-dark)
      
      (setq comint-completion-addsuffix (quote ("\\" . " ")))
      
      (load-file "~/.emacs.d/abfx2.el")
      ))


(put 'dired-find-alternate-file 'disabled nil)
