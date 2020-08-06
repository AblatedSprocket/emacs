;;; init-org.el --- Configuration for org-mode
;;; Commentary:
;;; - Manually install org-habit
;;; - Manually install org-mu4e
;;; Code:
(require-package 'org-journal)
(require-package 'org-mime)

(require 'init-writing)
(require 'init-mail)
(require 'org-habit)
(require 'org-journal)
(require 'org-mu4e)

;; Stuff for icons
(setq org-agenda-category-icon-alist '(("Appointment" "~/.config/emacs/icons/bell.svg" nil nil :ascent center)
                                       ("Cleaning" "~/.config/emacs/icons/flower.svg" nil nil :ascent center)
                                       ("Contractor" "~/.config/emacs/icons/tools.svg" nil nil :ascent center)
                                       ("Exercise" "~/.config/emacs/icons/barbell.svg" nil nil :ascent center)
                                       ("Finance" "~/.config/emacs/icons/columns.svg" nil nil :ascent center)
                                       ("Learning" "~/.config/emacs/icons/flask.svg" nil nil :ascent center)
                                       ("life" "~/.config/emacs/icons/leaf.svg" nil nil :ascent center)
                                       ("Journal" "~/.config/emacs/icons/journal.svg" nil nil :ascent center)
                                       ("Organization" "~/.config/emacs/icons/folder.svg" nil nil :ascent center)
                                       ("Party" "~/.config/emacs/icons/beer.svg" nil nil :ascent center)
                                       ("Repair" "~/.config/emacs/icons/wrench.svg" nil nil :ascent center)
                                       ("todo" "~/.config/emacs/icons/gears.svg" nil nil :ascent center)))
(add-to-list 'org-agenda-custom-commands
             '("x" "Testing tags for negating DONE" tags "-TODO=\"DONE\"" nil nil ))

(setq org-directory "~/org")
(setq org-journal-dir "~/org/journal/")
(setq org-journal-file-format "%Y.org")
(setq org-journal-enable-agenda-integration t)
(setq org-journal-skip-carryover-drawers t)
(setq org-agenda-files '("~/org/todo.org" "~/org/life.org"))
(add-to-list 'org-agenda-files org-journal-dir)
(setq org-mu4e-convert-to-html t)
(setq org-mu4e-link-query-in-headers-mode nil)
(setq org-capture-templates
      '(("t" "todo" entry (file+headline "~/org/todo.org" "Tasks")
         "* TODO %?\nSCHEDULED: %(org-insert-time-stamp (org-read-date nil t \"+0d\"))\n%a\n")))
(setq org-agenda-breadcrumbs-separator " ❱ ")
(setq org-journal-date-format "%A, %B %d %Y")
(setq org-journal-file-type "yearly")
(setq org-log-into-drawer "LOGBOOK")
(setq org-modules '(org-habit))

;; Functions
(defun org-summary-todo (n-done n-not-done)
  "Switch entry to DONE when all subentries are done, to TODO otherwise."
  (let (org-log-done org-log-states)    ; turn off logging
    (org-todo (if (= n-not-done 0) "DONE" "TODO"))))

(eval-after-load 'org
  '(org-load-modules-maybe t))

(add-hook 'mu4e-compose-mode-hook 'org-mu4e-compose-org-mode)
(add-hook 'org-after-todo-statistics-hook 'org-summary-todo)
(add-hook 'org-mode-hook 'flyspell-mode)
(add-hook 'org-mode-hook 'org-indent-mode)
(add-hook 'org-mode-hook 'set-printing-font)
(add-hook 'org-mode-hook 'toggle-truncate-lines)

(provide 'init-org)
;;; init-org.el ends here.