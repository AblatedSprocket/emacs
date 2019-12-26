;;; init-miscellaneous-el --- Changes all yes/no questions to y/n
;;; Commentary:
;;; - Changes yes/no responses to y/n
;;; - Lock files are no longer generated

;;; Code:
(fset 'yes-or-no-p 'y-or-n-p)

;; No need for ~ files when editing
(setq create-lockfiles nil)

(provide 'init-miscellaneous)
;;; init-miscellaneous.el ends here
