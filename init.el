;; INIT.EL - Structured and Optimized Emacs Configuration

;; ==============================
;; BASIC SETTINGS
;; ==============================
(setq custom-file (concat user-emacs-directory "custom.el"))
(when (file-exists-p custom-file)
  (load custom-file))
(tool-bar-mode -1)
(find-file (concat user-emacs-directory "emacs_welcome.org"))
(setq inhibit-startup-message t
      initial-scratch-message nil
      initial-major-mode 'text-mode)
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(if (fboundp 'set-fringe-mode) (set-fringe-mode 0))
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(pixel-scroll-precision-mode 1)
(icomplete-mode 1)
(global-auto-revert-mode t)
(setq mac-command-modifier 'control)

;; Disable default keybindings
(global-unset-key (kbd "C-w"))
(global-unset-key (kbd "C-<down-mouse-1>"))
(global-unset-key (kbd "<left>"))
(global-unset-key (kbd "<right>"))
(global-unset-key (kbd "<up>"))
(global-unset-key (kbd "<down>"))
(global-unset-key (kbd "<C-left>"))
(global-unset-key (kbd "<C-right>"))
(global-unset-key (kbd "<C-up>"))
(global-unset-key (kbd "<C-down>"))


;; ==============================
;; KEYBINDINGS
;; ==============================
(defvar shrcts-mode-map (make-sparse-keymap) "Keymap for shrcts minor mode.")

(define-minor-mode shrcts-mode
  "A minor mode for global shortcuts, overriding all major modes."
  :init-value t
  :lighter " shrcts"
  :keymap shrcts-mode-map)

;; Ensure shrcts-mode is always enabled
(shrcts-mode 1)

(define-key shrcts-mode-map (kbd "C-c C-c") 'kill-ring-save)
(define-key shrcts-mode-map (kbd "C-z") 'undo-only)
(define-key shrcts-mode-map (kbd "C-y") 'undo-redo)
(define-key shrcts-mode-map (kbd "C-v") 'yank)
(define-key shrcts-mode-map (kbd "C-M-p") 'scroll-down-command)
(define-key shrcts-mode-map (kbd "C-M-n") 'scroll-up-command)
(define-key shrcts-mode-map (kbd "C-c RET") 'set-mark-command)
(define-key shrcts-mode-map (kbd "M-p") (lambda () (interactive) (next-line -10)))
(define-key shrcts-mode-map (kbd "M-n") (lambda () (interactive) (next-line 10)))
(define-key shrcts-mode-map (kbd "C-c C-x")
                (lambda ()
                  (interactive)
                  (when (use-region-p)
                    (clipboard-kill-ring-save (region-beginning) (region-end))
                    (kill-region (region-beginning) (region-end)))))
(define-key shrcts-mode-map (kbd "C-w") 'delete-window)

;; ==============================
;; PACKAGE MANAGEMENT
;; ==============================
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(require 'use-package)

;; ==============================
;; THEMES & FONTS
;; ==============================
(use-package monokai-theme :ensure t)
(use-package poet-theme :ensure t)
(set-face-attribute 'default nil :family "Menlo" :height 120)

;; ==============================
;; AUTO-SAVE & BACKUPS
;; ==============================
(setq backup-directory (concat user-emacs-directory "backups/")
      auto-save-directory (concat user-emacs-directory "auto-saves/")
      lock-file-directory (concat user-emacs-directory "lock-files/"))

(dolist (dir (list backup-directory auto-save-directory lock-file-directory))
  (unless (file-exists-p dir) (make-directory dir t)))

(setq backup-directory-alist `(("." . ,backup-directory))
      auto-save-file-name-transforms `((".*" ,auto-save-directory t))
      lock-file-name-transforms `((".*" ,lock-file-directory t)))

;; ==============================
;; ADDITIONAL PACKAGES
;; ==============================
(use-package drag-stuff
  :ensure t
  :config
  (drag-stuff-global-mode 1)
  (define-key shrcts-mode-map (kbd "M-<down>") 'drag-stuff-down)
  (define-key shrcts-mode-map (kbd "M-<up>") 'drag-stuff-up)
  (define-key shrcts-mode-map (kbd "M-<left>") 'drag-stuff-left)
  (define-key shrcts-mode-map (kbd "M-<right>") 'drag-stuff-right))

(use-package obsidian
  :ensure t
  :config
  (obsidian-specify-path "~/Proton Drive/Obsidian")
  (setq obsidian-wiki-link-create-file-in-inbox nil
        obsidian-daily-notes-directory "Zapiski")
  (global-obsidian-mode t))

(use-package multiple-cursors
  :ensure t
  :bind (("C-d" . mc/mark-next-like-this)
         ("C-c C-d" . mc/mark-all-like-this)))

(use-package exec-path-from-shell
  :ensure t
  :config
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize)))

(use-package eglot
  :ensure nil
  :config
  (add-to-list 'eglot-server-programs '((c-mode c++-mode) . ("clangd")))
  (add-to-list 'eglot-server-programs '((python-mode) . ("pylsp")))
  (add-hook 'c-mode-hook 'eglot-ensure)
  (add-hook 'c++-mode-hook 'eglot-ensure)
  (add-hook 'python-mode-hook 'eglot-ensure))

(use-package company
  :ensure t
  :init (global-company-mode 1)
  :config
  (setq company-idle-delay 0.2
        company-minimum-prefix-length 2)
  (add-to-list 'company-backends 'company-files))

(use-package org-download :ensure t)
(require 'org-download)
(setq-default org-download-heading-lvl nil
              org-download-image-dir "./images")
(org-display-inline-images 1)

(use-package undo-tree
  :ensure t
  :init
  (global-undo-tree-mode 1)
  :config
  (setq undo-tree-auto-save-history t
        undo-tree-history-directory-alist `(("." . ,(concat user-emacs-directory "undo-tree/")))))
