;;; init-python.el --- Emacs configuration for Python
;;; Commentary:
;;; - Enable virtual environment support in Emacs
;;; Code:
(require 'init-elpa)
(require 'init-code)

(require-package 'auto-virtualenvwrapper)

(require 'auto-virtualenvwrapper)

(add-hook 'python-mode-hook 'autovirtualenvwrapper-activate)
(add-hook 'python-mode-hook 'init-code)
(add-hook 'python-mode-hook 'lsp)
(add-hook 'python-mode-hook (lambda ()
                              (local-unset-key (kbd "C-c C-f"))))
                             

(provide 'init-python)
;;; init-python.el ends here
