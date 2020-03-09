;;; init-rust.el --- Emacs configuration for Rust Mode
;;; Commentary:
;;; Mode included for Racer compatibility.
;;; Code:
(require 'init-elpa)
(require-package 'company)
(require-package 'rust-mode)
(require-package 'flycheck)
(require-package 'flycheck-rust)

(require 'company)
(require 'rust-mode)
(require 'flycheck)
(require 'flycheck-rust)
(require 'lsp-mode)

(setq lsp-rust-server 'rust-analyzer)
(setq lsp-rust-analyzer-server-command '("~/.cargo/bin/rust-analyzer"))


(add-to-list 'auto-mode-alist '("\\.rs\\'" . rust-mode))
(add-hook 'rust-mode-hook 'lsp)
(add-hook 'rust-mode-hook 'company-mode)
(add-hook 'flycheck-mode-hook 'flycheck-rust-setup)

;; (require-package 'racer)
;; (require 'racer)
;; (add-hook 'rust-mode-hook 'racer-mode)
;; (add-hook 'racer-mode-hook 'eldoc-mode)
;; (add-hook 'rust-mode-hook
          ;; '(lambda ()
             ;; (setq racer-cmd (concat (getenv "HOME") "/.cargo/bin/racer"))
             ;; (setq racer-rust-src-path (concat (getenv "HOME") "/.rustup/toolchains/stable-x86_64-unknown-linux-gnu/lib/rustlib/src/rust/src"))
;; (electric-pair-mode 1)))

(provide 'init-rust)
;;; init-rust.el ends here


