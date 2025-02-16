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
(delete-selection-mode 1)
(setq mac-command-modifier 'control)

;; Enable relative line numbers globally
(setq display-line-numbers-type 'relative) ;; Use relative numbers
(setq display-line-numbers-exempt-modes '(org-mode))
(add-hook 'prog-mode-hook 'display-line-numbers-mode)

;; Disable default keybindings
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
(defun my-delete-line ()
  "Delete from point to end of line. If the line is empty, delete the whole line."
  (interactive)
  (if (save-excursion
        (beginning-of-line)
        (looking-at-p "^[ \t]*$"))
      (delete-region (line-beginning-position) (min (point-max) (1+ (line-end-position))))
    (delete-region (point) (line-end-position))))

(define-key shrcts-mode-map (kbd "C-k") 'my-delete-line)
(define-key shrcts-mode-map (kbd "M-P") 'beginning-of-buffer)
(define-key shrcts-mode-map (kbd "M-N") 'end-of-buffer)

;; ==============================
;; DIRED
;; ==============================
(defvar dired-preview-mode-active nil
  "Indicates if Dired Preview Mode is active.")

(defvar dired-preview-window nil
  "Stores the preview window so it can be reused.")

(defun dired-preview-toggle ()
  "Toggle Dired Preview Mode."
  (interactive)
  (if dired-preview-mode-active
      (progn
        (setq dired-preview-mode-active nil)
        (remove-hook 'post-command-hook 'dired-preview-update t)
        (when (window-live-p dired-preview-window)
          (delete-window dired-preview-window)
          (setq dired-preview-window nil)))) ;; Properly reset the window variable
    (setq dired-preview-mode-active t)
    (add-hook 'post-command-hook 'dired-preview-update nil t)
    (dired-preview-update))

(defun dired-preview-update ()
  "Update the preview window based on the selected file/folder."
  (when (and dired-preview-mode-active (eq major-mode 'dired-mode))
    (let ((file (dired-get-file-for-visit)))
      (if (file-directory-p file)
          (dired-preview-show-dired file)
        (dired-preview-show-file file)))))

(defun dired-preview-show-dired (dir)
  "Show a Dired buffer for DIR in the preview window, reusing the same window."
  (let ((buf (dired-noselect dir)))
    (dired-preview-display-buffer buf)))

(defun dired-preview-show-file (file)
  "Show a read-only preview of FILE in the preview window, reusing the same buffer."
  (let ((buf (get-buffer-create "*Dired Preview*")))
    (with-current-buffer buf
      (setq buffer-read-only t)
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert-file-contents file)))
    (dired-preview-display-buffer buf)))

(defun dired-preview-display-buffer (buf)
  "Display BUF in the preview window, reusing the same window."
  (if (and (window-live-p dired-preview-window))
      (set-window-buffer dired-preview-window buf)
    (setq dired-preview-window
          (display-buffer buf '(display-buffer-below-selected . ((window-height . 10)))))))

(defun dired-preview-enter ()
  "Handle pressing RET in Dired Preview Mode."
  (interactive)
  (let ((file (dired-get-file-for-visit)))
    (if (file-directory-p file)
        (dired-find-file)  ;; Normal behavior for directories
      (progn
        (dired-find-file)
        (setq dired-preview-mode-active nil) ;; Disable preview mode
        (remove-hook 'post-command-hook 'dired-preview-update t)
        (when (window-live-p dired-preview-window)
          (delete-window dired-preview-window))
        (setq dired-preview-window nil))))) ;; Close preview

;; Bindings
(define-key dired-mode-map (kbd "SPC") 'dired-preview-toggle)
(define-key dired-mode-map (kbd "RET") 'dired-preview-enter)

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
        undo-tree-history-directory-alist `(("." . ,(concat user-emacs-directory "undo-tree/"))))
  (define-key shrcts-mode-map (kbd "C-z") 'undo-tree-undo)
  (define-key shrcts-mode-map (kbd "C-y") 'undo-tree-redo))

(use-package treemacs
  :ensure t
  :defer t
  :config
  (setq treemacs-is-never-other-window t)
  (define-key shrcts-mode-map (kbd "M-0") 'treemacs-select-window))
