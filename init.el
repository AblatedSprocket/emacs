
;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

(require 'init-elpa)

(require 'init-ui)

(require 'init-editing)

(require 'init-navigation)

(require 'init-miscellaneous)

(require 'init-company-mode)

(require 'init-rust)

(provide 'init)
	     
