;;; init-navigation.el --- General navigation Emacs configuration

;;; Commentary:
;;; - Saves list of 40 recently accessed files
;;; - General ido configuration
;;; - Auto-completion of M-x commands with ido
;;; - Enables project detection with Projectile
;;; - Disables ido use filename at point
;;; - Disables ido auto-merge work directories
;;; - Enables accessing closed buffers by enabling virtual buffers

;;; Code:
(require 'init-elpa)
(require 'ido)
(require 'recentf)
(require-package 'smex)
(require-package 'projectile)

(defun switch-to-previous-buffer()
  "Switch to previously open buffer.
Repeated invocations toggle between the two most recently open buffers."
  (interactive)
  (switch-to-buffer (other-buffer (current-buffer) 1)))

(defun beginning-of-line-or-indentation ()
  "Move to beginning of line or indentation."
  (interactive)
  (if (bolp)
      (back-to-indentation)
    (beginning-of-line)))

(setq recentf-save-file (concat user-emacs-directory ".recentf"))
(recentf-mode 1)
(setq recentf-max-menu-items 40)

(ido-mode t)
(setq ido-use-filename-at-point nil)
(setq ido-auto-merge-work-directories-length -1)
(setq ido-use-virtual-buffers t)

;; Override beginning of line behavior
(global-set-key (kbd "C-a") 'beginning-of-line-or-indentation)

;; Shows a list of buffers
(global-set-key (kbd "C-x C-b") 'ibuffer)
(global-set-key (kbd "C-c b") 'switch-to-previous-buffer)

(setq smex-save-file (concat user-emacs-directory ".smex-items"))
(smex-initialize)
(global-set-key (kbd "M-x") 'smex)

(projectile-global-mode)
(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)

;; Enable move point from window to window using Shift and the arrow keys
(windmove-default-keybindings)

(provide 'init-navigation)
;;; init-navigation.el ends here
