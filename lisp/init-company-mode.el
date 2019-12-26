;;; init-company-mode.el --- Initialization for Emacs Company Mode.

;;; Commentary:
;;; sets tooltip align annotations and adds hook to start company mode.

;;; Code:
(require 'init-elpa)
(require-package 'company)
(require 'company)

(setq company-tooltip-align-annotations t)
(add-hook 'prog-mode-hook 'company-mode)

(provide 'init-company-mode)
;;; init-company-mode.el ends here
