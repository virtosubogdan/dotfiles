;;; emacs-config --- Custom stuff - copied from https://github.com/comandrei/dotfiles/blob/master/init.el
;;; Code:
;;; Commentary:
(setq auto-mode-alist (cons '("emacs" . lisp-mode) auto-mode-alist))
(ido-mode 1)
(show-paren-mode 1)
(tool-bar-mode 0)
(menu-bar-mode 0)
(scroll-bar-mode -1)
(column-number-mode 1)
(global-linum-mode t)
(setq frame-title-format
      '((:eval (if (buffer-file-name)
                   (abbreviate-file-name (buffer-file-name))
                 "%b"))))
;; Delete selected region
(delete-selection-mode 1)
(setq-default show-trailing-whitespace t)
;; Disable splash screen
(setq inhibit-splash-screen t)

;; Prerequisite: Emacs >= 24
(require 'package)
(add-to-list 'package-archives
	     '("melpa" . "http://melpa.milkbox.net/packages/"))
(add-to-list 'package-archives
	     '("marmalade" . "http://marmalade-repo.org/packages/") t)
(package-initialize)

;; Cloned from https://github.com/jaypei/emacs-neotree.git. You need to do this manually
(add-to-list 'load-path "~/Tools/emacsTools/neotree")
(require 'neotree)
(global-set-key [f8] 'neotree-toggle)

(require 'highlight-symbol)
(global-set-key (kbd "M-.") 'highlight-symbol-at-point)
(global-set-key (kbd "M-,") 'highlight-symbol-next)

;; Rebind undo
(global-unset-key [(control z)])
(global-set-key [(control z)] 'undo)

(defun install-if-needed (package)
  "Install package only if needed.
\(PACKAGE) is the package that will be installed"
  (unless (package-installed-p package)
    (package-refresh-contents)
    (package-install package)))

;; make more packages available with the package installer
;; jedi requires special attention. Run M-x jedi:install-server when you remember this anyway to late and after you read this comment.
(setq to-install
      '(highlight-symbol exec-path-from-shell less-css-mode markdown-mode zenburn-theme jedi smartparens yaml-mode flycheck python-mode))

(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize))

(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.sls?\\'" . yaml-mode))

(mapc 'install-if-needed to-install)
;; (require 'yasnippet)
(require 'smartparens)
(show-smartparens-global-mode +1)
(load-theme 'zenburn t)
;; (yas/initialize)
;; (yas/load-directory "~/.emacs.d/snippets/")

(add-hook 'after-init-hook #'global-flycheck-mode)

(require 'python-mode)
(add-hook 'python-mode-hook
          (lambda ()
            (jedi:setup)
            (jedi:ac-setup)
            (local-set-key "\C-cd" 'jedi:show-doc)
            (local-set-key (kbd "M-SPC") 'jedi:complete)
            (local-set-key (kbd "M-/") 'jedi:goto-definition))
	  )

;; Attempt to use C-d for copy current line
;; TODO: revert cursor to initial position
(global-set-key "\C-c\C-d" "\C-a\C- \C-n\M-w\C-y")

(defun revert-all-buffers ()
  "Refreshes all open buffers from their respective files."
  (interactive)
  (let* ((list (buffer-list))
         (buffer (car list)))
    (while buffer
      (when (and (buffer-file-name buffer)
                 (not (buffer-modified-p buffer)))
        (set-buffer buffer)
        (revert-buffer t t t))
      (setq list (cdr list))
      (setq buffer (car list))))
  (message "Refreshed open files"))
(global-set-key [(control shift f5)] 'revert-all-buffers)

(defun kill-other-buffers ()
  "Kill all buffers but the current one.
Don't mess with special buffers."
  (interactive)
  (dolist (buffer (buffer-list))
    (unless (or (eql buffer (current-buffer)) (not (buffer-file-name buffer)))
      (kill-buffer buffer))))

(server-start)

;; Python macros

(define-key
  python-mode-map
  (kbd "C-c C-x i")
  (lambda ()
    (interactive)
    (insert "import ipdb;ipdb.set_trace()")))

(provide 'init)
;;; init.el ends here
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (python-mode zenburn-theme yaml-mode smartparens markdown-mode less-css-mode jedi highlight-symbol flycheck exec-path-from-shell))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
