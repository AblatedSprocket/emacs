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

(require 'company)
(require 'racer)
(require 'rust-mode)
(require 'electric)
(require 'eldoc)
(require 'flycheck)
(require 'flycheck-rust)
(require 'projectile)

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

(defun cargo-build (args)
  "Call cargo build command with provided ARGS."
  (interactive "sArgs:")
  (shell-command (concat "cargo build " args)))

(defun cargo-check ()
  "Call cargo check command."
  (interactive)
  (shell-command "cargo check"))

(defun cargo-doc (args)
  "Call cargo doc command with provided ARGS."
  (interactive "sArgs:")
  (shell-command (concat "cargo doc " args)))

(defun cargo-fmt ()
  "Call cargo format command."
  (interactive)
  (shell-command "cargo fmt"))

(defun cargo-run (args)
  "Call cargo run command with provided ARGS."
  (interactive "sArgs:")
  (shell-command (concat "cargo run " args)))

(defun cargo-test ()
  "Call cargo test command."
  (interactive)
  (shell-command "cargo test"))

(global-set-key (kbd "C-c C-e") 'racer-describe)
(global-set-key (kbd "C-c C-r") 'cargo-run)
(global-set-key (kbd "C-c C-b") 'cargo-build)
(global-set-key (kbd "C-c C-t") 'cargo-test)
(global-set-key (kbd "C-c C-f") 'cargo-fmt)
(global-set-key (kbd "C-c C-k") 'cargo-check)
(global-set-key (kbd "C-c C-d") 'carago-doc)
(provide 'init-rust)
;;; init-rust.el ends here
