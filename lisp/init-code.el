;;; init-code.el --- Emacs configuration for any code editing in Emacs.
;;; Commentary:
;;; - Enables helm-ag for refactoring code.
;;; Code:
(require 'init-elpa)

(require-package 'helm)
(require-package 'helm-ag)

(require 'helm)
(require 'helm-ag)

(provide 'init-code)
;;; init-code.el ends here
