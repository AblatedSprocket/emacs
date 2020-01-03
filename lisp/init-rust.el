;;; init-rust.el --- Emacs configuration for Emacs
;;; Commentary:
;;; - Enable Company Mode in Rust Mode
;;; - Enable Racer Mode with Rust Mode
;;; - Enable Cargo Mode with Rust Mode
;;; - Enable Flycheck Mode with Rust mode
;;; - Set directories for Racer commands
;;; - Uses spaces instead of tabs
;;; - Enables debugging
;;; Code:
(require 'init-elpa)
(require-package 'company)
(require-package 'racer)
(require-package 'rust-mode)
(require-package 'flycheck)
(require-package 'flycheck-rust)
(require-package 'cargo)

(require 'company)
(require 'racer)
(require 'rust-mode)
(require 'electric)
(require 'eldoc)
(require 'flycheck)
(require 'flycheck-rust)
(require 'cargo)

(add-to-list 'auto-mode-alist '("\\.rs\\'" . rust-mode))
(add-hook 'rust-mode-hook #'company-mode)
(add-hook 'rust-mode-hook #'racer-mode)
(add-hook 'rust-mode-hook #'cargo-minor-mode)
(add-hook 'rust-mode-hook #'linum-mode)
(add-hook 'racer-mode-hook #'eldoc-mode)
(add-hook 'flycheck-mode-hook #'flycheck-rust-setup)
(add-hook 'rust-mode-hook
	  '(lambda ()
	     (setq racer-cmd (concat (getenv "HOME") "/.cargo/bin/racer"))
	     (setq racer-rust-src-path (concat (getenv "HOME") "/.rustup/toolchains/stable-x86_64-unknown-linux-gnu/lib/rustlib/src/rust/src"))
	     (local-set-key (kbd "TAB") #'company-indent-or-complete-common)
	     (electric-pair-mode 1)))

(global-set-key (kbd "C-c C-g C-d") 'rust-debug)

(provide 'init-rust)
;;; init-rust.el ends here
