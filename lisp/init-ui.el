;;; init-ui.el --- INitializes UI configuration for Emacs. General configuration for
;;; all modes is encapsulated by this file.
;;; Commentary:
;;; - Changes yes/no responses to y/n
;;; - Lock files are no longer generated
;;; - Installs ripgrep utilities (requires installation of ripgrep)


;;; Code:
(require 'init-elpa)
(require-package 'atom-one-dark-theme)
(require-package 'company)
(require-package 'flycheck)
(require-package 'rg)

(require 'company)
(require 'saveplace)

;; Highlight matching parentheses
(show-paren-mode 1)

;; Highlight current line
(global-hl-line-mode 1)

(global-set-key (kbd "C-s") 'isearch-forward-regexp)
(global-set-key (kbd "C-r") 'isearch-backward-regexp)
(global-set-key (kbd "C-M-s") 'isearch-forward)
(global-set-key (kbd "C-M-r") 'isearch-backward)
(global-set-key (kbd "M-F") 'forward-to-word)
(global-set-key (kbd "M-f") 'forward-word)
(global-set-key (kbd "M-b") 'backward-word)
(global-set-key (kbd "M-B") 'backward-to-word)

(fset 'yes-or-no-p 'y-or-n-p)

;; No need for ~ files when editing
(setq create-lockfiles nil)

(set-face-attribute 'default nil :font "Inconsolata" :height 140)
(setq-default line-spacing 0.4)
;; When you visit a file, point goes to the last place where it
;; was when you previously visited the same file.
(setq-default save-place t)
(setq-default word-wrap t)
(setq-default cursor-type 'bar)
(set-cursor-color "#cccccc")

(setq company-minimum-prefix-length 1)

(setq inhibit-startup-message t)
(menu-bar-mode -1)
(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))
(when (fboundp 'scroll-bar-mode)
  (scroll-bar-mode -1))

(setq
 x-select-enable-clipboard t
 x-select-enable-primary t
 save-interprogram-paste-before-kill t
 apropos-do-all t
 mouse-yank-at-point t)

(load-theme 'atom-one-dark t)
(blink-cursor-mode 1)
;; Keep track of saved places in ~/.emacs.d/places
(setq save-place-file (concat user-emacs-directory "places"))
;; Emacs can automaticall create backup files. This tells Emacs to
;; put all backups in ~/.emacs.d/backups.
(setq backup-directory-alist `(("." . ,(concat user-emacs-directory
                                               "backups"))))
(setq auto-save-default nil)
(setq ring-bell-function 'ignore)
(setq company-tooltip-align-annotations t)

(add-hook 'after-init-hook 'global-flycheck-mode)
(add-hook 'after-init-hook 'company-mode)

(provide 'init-ui)
;;; init-ui.el ends here
