(require 'package)
(add-to-list 'package-archives
  '("melpa" . "http://melpa.milkbox.net/packages/") t)

(package-initialize)
(setq inhibit-startup-message t)

;;load package specified by package-name and if not exist intall it.
(defun load-package-hard (package-name)
  (progn
    (if (not (package-installed-p package-name))
      (progn 
	;;This informs Emacs about the latest versions of all packages, and makes them available for download.
	(when (not package-archive-contents)
	  (package-refresh-contents))
	(package-install package-name)))
  (require package-name)))


;;for go-mode config
(defun set-exec-path-from-shell-PATH ()
  (let ((path-from-shell (replace-regexp-in-string
                          "[ \t\n]*$"
                          ""
                          (shell-command-to-string "$SHELL --login -i -c 'echo $PATH'"))))
    (setenv "PATH" path-from-shell)
    (setq eshell-path-env path-from-shell) ; for eshell users
    (setq exec-path (split-string path-from-shell path-separator))))

(defun my-go-mode-hook ()
  ; Call Gofmt before saving                                                    
  (add-hook 'before-save-hook 'gofmt-before-save)
  ; Godef jump key binding                                                      
  (local-set-key (kbd "M-.") 'godef-jump)
  (local-set-key (kbd "M-,") 'pop-tag-mark)
  )

(defun auto-complete-for-go ()
  (auto-complete-mode 1))
 
(defun load-go-mode()
  (progn 
    (load-package-hard 'go-mode)
    (load-package-hard 'go-autocomplete)
    (load-package-hard 'exec-path-from-shell)
    (when window-system (set-exec-path-from-shell-PATH))
    (setenv "GOPATH" "/home/mason/projects/go")
    (add-to-list 'exec-path "/home/mason/projects/go/bin")
    (add-hook 'go-mode-hook 'my-go-mode-hook)
    (add-hook 'go-mode-hook 'auto-complete-for-go)
    (go-mode)))

;;invoke load-go-mode when open a go file
(add-to-list 'auto-mode-alist '("\\.go\\'" . load-go-mode))




;;for c/c++ mode
(defun load-c-c++-config ()
  (progn
    (load-package-hard 'setup-helm)
    (load-package-hard 'helm-gtags)
    (setq
     helm-gtags-ignore-case t
     helm-gtags-auto-update t
     helm-gtags-use-input-at-cursor t
     helm-gtags-pulse-at-cursor t
     helm-gtags-prefix-key "\C-cg"
     helm-gtags-suggested-key-mapping t
     )
    (define-key helm-gtags-mode-map (kbd "C-c g a") 'helm-gtags-tags-in-this-function)
    (define-key helm-gtags-mode-map (kbd "C-j") 'helm-gtags-select)
    (define-key helm-gtags-mode-map (kbd "M-.") 'helm-gtags-dwim)
    (define-key helm-gtags-mode-map (kbd "M-,") 'helm-gtags-pop-stack)
    (define-key helm-gtags-mode-map (kbd "C-c <") 'helm-gtags-previous-history)
    (define-key helm-gtags-mode-map (kbd "C-c >") 'helm-gtags-next-history)

     (helm-gtags-mode)
))

(add-hook 'c-mode-hook 'load-c-c++-config)
(add-hook 'c++-mode-hook 'load-c-c++-config)
