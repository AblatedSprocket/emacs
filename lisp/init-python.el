;;; init-python.el --- Emacs configuration for Python
;;; Commentary:
;;; - Enable virtual environment support in Emacs
;;; Code:
(require 'init-elpa)
(require 'init-code)

;; (require-package 'pyvenv)

(add-hook 'python-mode-hook 'init-code)

(provide 'init-python)
;;; init-python.el ends here
