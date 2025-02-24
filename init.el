;; INIT.EL - Structured and Optimized Emacs Configuration

;; ==============================
;; BASIC SETTINGS
;; ==============================
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file))
(tool-bar-mode -1)
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
;;(electric-indent-mode 0)
(setq mac-command-modifier 'control)
(setq mode-line-percent-position "") ;; Remove percentage
(setq-default cursor-type 'bar)
(setq shift-select-mode t)
;;(setq scroll-preserve-screen-position t)
(scroll-lock-mode 1)

(setq gc-cons-threshold most-positive-fixnum) ;; Disable GC temporarily
(setq gc-cons-percentage 0.6)

(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold (* 20 1024 1024)) ;; Set to 20MB after startup
            (setq gc-cons-percentage 0.1)
            (message "Ready")))

(use-package display-line-numbers
  :hook ((prog-mode . display-line-numbers-mode))
  :config
  (setq display-line-numbers-exempt-modes '(org-mode)))

;; Disable default keybindings
(global-unset-key (kbd "C-<down-mouse-1>"))

(add-hook 'emacs-startup-hook
          (lambda ()
            (find-file (concat user-emacs-directory "emacs_welcome.org"))))

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
(add-hook 'emacs-startup-hook 'shrcts-mode)

(define-key shrcts-mode-map (kbd "C-c C-c") 'kill-ring-save)
(define-key shrcts-mode-map (kbd "C-z") 'undo-tree-undo)
(define-key shrcts-mode-map (kbd "C-y") 'undo-tree-redo)
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
(define-key shrcts-mode-map (kbd "C-x C-o") 'other-window)
(define-key shrcts-mode-map (kbd "C-t")
            (lambda ()
              (interactive)
              (find-file (concat user-emacs-directory "emacs_welcome.org"))))

(when (eq system-type 'darwin)  ;; Apply only on macOS
  (define-key input-decode-map (kbd "M-ž") (kbd "\\"))
  (define-key input-decode-map (kbd "M-Ž") (kbd "|"))
  (define-key input-decode-map (kbd "M-<") (kbd "`"))
  (define-key input-decode-map (kbd "M->") (kbd "~"))
  (define-key input-decode-map (kbd "M-š") (kbd "["))
  (define-key input-decode-map (kbd "M-đ") (kbd "]"))
  (define-key input-decode-map (kbd "M-Š") (kbd "{"))
  (define-key input-decode-map (kbd "M-Đ") (kbd "}")))

;; ==============================
;; DIRED
;; ==============================

;;; dired-preview.el --- Preview files/folders in Dired automatically

(defvar dired-preview-window nil
  "Window used to display Dired previews.")

(defun dired-preview-display-buffer (buf)
  "Display BUF in the preview window."
  (if (window-live-p dired-preview-window)
      (set-window-buffer dired-preview-window buf)
    (setq dired-preview-window
          (display-buffer buf
                          '(display-buffer-below-selected
                            . ((window-height . 10)))))))

(defun dired-preview-show-file (file)
  "Display FILE contents in a read-only buffer, in `dired-preview-window`."
  (let ((buf (get-buffer-create "*Dired Preview*")))
    (with-current-buffer buf
      (setq buffer-read-only t)
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert-file-contents file)))
    (dired-preview-display-buffer buf)))

(defun dired-preview-show-dired (dir)
  "Show a Dired buffer for DIR in the preview window."
  (let ((buf (dired-noselect dir)))
    (dired-preview-display-buffer buf)))

(defun dired-preview-update ()
  "Update the preview window based on the currently selected item in Dired."
  (when (and dired-preview-mode (eq major-mode 'dired-mode))
    (let ((file (dired-get-file-for-visit)))
      (if (file-directory-p file)
          (dired-preview-show-dired file)
        (dired-preview-show-file file)))))

(defun dired-preview-enter ()
  "If the selected item is a file, open it and turn off preview mode."
  (interactive)
  (let ((file (dired-get-file-for-visit)))
    (if (file-directory-p file)
        (dired-find-file)  ;; normal for directories
      (progn
        (dired-find-file)
        (dired-preview-mode -1)))))

(defun dired-preview-resume ()
  "Re-attach the local hook after Dired reads a new directory."
  (when dired-preview-mode
    ;; Re-add our local post-command-hook
    (add-hook 'post-command-hook #'dired-preview-update nil t)
    (dired-preview-update)))

(define-minor-mode dired-preview-mode
  "Minor mode that auto-previews the selected file/folder in Dired."
  :lighter " dPrev"
  (if dired-preview-mode
      (progn
        (add-hook 'post-command-hook #'dired-preview-update nil t)
        (add-hook 'dired-after-readin-hook #'dired-preview-resume nil t)
        (dired-preview-update))
    (remove-hook 'post-command-hook #'dired-preview-update t)
    (remove-hook 'dired-after-readin-hook #'dired-preview-resume t)
    (when (window-live-p dired-preview-window)
      (delete-window dired-preview-window))
    (setq dired-preview-window nil)))

(with-eval-after-load 'dired
  (define-key dired-mode-map (kbd "SPC") #'dired-preview-mode)
  (define-key dired-mode-map (kbd "RET") #'dired-preview-enter))

(provide 'dired-preview)

;; ==============================
;; PACKAGE MANAGEMENT
;; ==============================
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)

(eval-when-compile
  (require 'use-package))
(setq use-package-always-ensure t) ;; Auto-install missing packages
(setq use-package-always-defer t)  ;; Defer loading unless explicitly needed

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
;; INDENTATION SETTINGS
;; ==============================

;; (defun my-set-tab-width (width)
;;   "Set tab width interactively."
;;   (interactive "nTab width: ")
;;   (setq-default tab-width width)
;;   (message "Tab width set to %d" width))

;; (defun my-toggle-indent-tabs-mode ()
;;   "Toggle between spaces and tabs."
;;   (interactive)
;;   (setq-default indent-tabs-mode (not indent-tabs-mode))
;;   (message "Using %s for indentation" (if indent-tabs-mode "tabs" "spaces")))

;; (transient-define-prefix my-indent-menu ()
;;   "Indentation settings menu."
;;   [["Indentation Mode"
;;     ("s" "Use Spaces" (lambda () (interactive) (setq-default indent-tabs-mode nil) (message "Using spaces")))
;;     ("t" "Use Tabs" (lambda () (interactive) (setq-default indent-tabs-mode t) (message "Using tabs")))]
;;    ["Tab Width"
;;     ("2" "Set tab width to 2" (lambda () (interactive) (my-set-tab-width 2)))
;;     ("4" "Set tab width to 4" (lambda () (interactive) (my-set-tab-width 4)))
;;     ("8" "Set tab width to 8" (lambda () (interactive) (my-set-tab-width 8)))]
;;    ["Other"
;;     ("d" "Detect automatically (dtrt-indent)" dtrt-indent-mode)
;;     ("q" "Quit" transient-quit-one)]])

;; (global-set-key (kbd "C-c i") 'my-indent-menu)

;; ;; Automatically detect indentation settings based on file contents
;; (use-package dtrt-indent
;;   :ensure t
;;   :config
;;   (dtrt-indent-global-mode 1))

;; ;; Function to indent the selected region or the current line
;; (defun my-indent ()
;;   "Indent the region if active, otherwise the current line."
;;   (interactive)
;;   (if (use-region-p)
;;       (indent-rigidly (region-beginning) (region-end) tab-width)
;;     (indent-rigidly (line-beginning-position) (line-end-position) tab-width)))

;; ;; Function to unindent the selected region or the current line
;; (defun my-unindent ()
;;   "Unindent the region if active, otherwise the current line."
;;   (interactive)
;;   (if (use-region-p)
;;       (indent-rigidly (region-beginning) (region-end) (- tab-width))
;;     (indent-rigidly (line-beginning-position) (line-end-position) (- tab-width))))

;; ;; Override the TAB key to indent
;; (global-set-key (kbd "TAB") 'my-indent)

;; ;; Bind Shift-TAB (Backtab) to unindent
;; (global-set-key (kbd "<backtab>") 'my-unindent)

;; ==============================
;; MODE LINE INDENTATION INFO
;; ==============================

(defun my-indent-mode-line-info ()
  "Return current indentation mode for display in the mode line."
  (if indent-tabs-mode
      (format "T%d" tab-width) ;; ␉ symbol for tabs
    (format "S%d" tab-width))) ;; ␠ symbol for spaces

;; Customize mode line
(setq-default mode-line-format
  '("%e"  ;; Error message indicator
    " " mode-line-buffer-identification
    "  " mode-line-position
    ;;"  " (vc-mode vc-mode)  ;; This shows Git, REMOVE IT
    "  " minions-mode-line-modes
    "  " (:eval (my-indent-mode-line-info))))  ;; Show hidden minor modes via Minions

;; ==============================
;; ADDITIONAL PACKAGES
;; ==============================

;; Easy drag-edit
(use-package drag-stuff
  :config
  (drag-stuff-global-mode 1))
(define-key shrcts-mode-map (kbd "M-<down>") 'drag-stuff-down)
(define-key shrcts-mode-map (kbd "M-<up>") 'drag-stuff-up)
(define-key shrcts-mode-map (kbd "M-<left>") 'drag-stuff-left)
(define-key shrcts-mode-map (kbd "M-<right>") 'drag-stuff-right)


;; Obsidian note indexing, linking
(use-package obsidian
  :config
  (obsidian-specify-path "~/Proton Drive/Obsidian")
  (setq obsidian-wiki-link-create-file-in-inbox nil
        obsidian-daily-notes-directory "Dnevni zapiski")
  (global-obsidian-mode t))


;; Multiple cursors
(use-package multiple-cursors
  :bind (("C-d" . mc/mark-next-like-this)
         ("C-c C-d" . mc/mark-all-like-this)))

;; (use-package exec-path-from-shell
;;   :ensure t
;;   :config
;;   (when (memq window-system '(mac ns x))
;;     (exec-path-from-shell-initialize)))

;; (use-package eglot
;;   :ensure nil
;;   :config
;;   (add-to-list 'eglot-server-programs '((c-mode c++-mode) . ("clangd")))
;;   (add-to-list 'eglot-server-programs '((python-mode) . ("pylsp")))
;;   (add-hook 'c-mode-hook 'eglot-ensure)
;;   (add-hook 'c++-mode-hook 'eglot-ensure)
;;   (add-hook 'python-mode-hook 'eglot-ensure))


;; completion package with nice menu when typing
(use-package company
  :init (global-company-mode 1)
  :config
  (setq company-idle-delay 0.2
        company-minimum-prefix-length 2)
  (add-to-list 'company-backends 'company-files))

(with-eval-after-load 'company
  (add-to-list 'company-backends 'obsidian-company-backend))

(use-package undo-tree
  :init
  (global-undo-tree-mode 1) ;; Enable globally
  :config
  (setq undo-tree-auto-save-history t
        undo-tree-history-directory-alist `(("." . ,(expand-file-name "undo-tree/" user-emacs-directory))))
  (unless (file-exists-p (expand-file-name "undo-tree/" user-emacs-directory))
    (make-directory (expand-file-name "undo-tree/" user-emacs-directory) t)))

;; Advanced sublime-style sidebar
(use-package treemacs
  :config
  (setq treemacs-is-never-other-window t)
  
  ;; Custom function to toggle Treemacs or select its window
  (defun my-treemacs-toggle-or-select ()
    "Open Treemacs if it's not running, otherwise select its window."
    (interactive)
    (if (treemacs-current-visibility)
        (treemacs-select-window)
      (treemacs))))
(define-key shrcts-mode-map (kbd "M-0") 'treemacs)

;; Hide mode panel modes into a menu
(use-package minions
  :config
  (minions-mode 1)
  :bind ("M-m" . minions-minor-modes-menu))

(add-hook 'after-init-hook
          (lambda ()
            (minions-mode 1)))

;; Pdf display in emacs!
(use-package pdf-tools)

;; Used mainly to enable images within markdown
(use-package markdown-mode
  :config
  (add-hook 'markdown-mode-hook #'markdown-toggle-inline-images)
  (setq markdown-enable-wiki-links t)  ;; Enable wiki-style links
  (setq markdown-follow-wiki-link-on-enter t))  ;; Follow link on Enter key

;; Advanced obsidian-like org-mode
(use-package org-roam
  :custom
  (org-roam-directory "~/Proton Drive/org-roam") ;; Change to your preferred directory
  :config
  (org-roam-db-autosync-mode))

;; Can paste images into org mode files
(use-package org-download
  :init
  (setq-default org-download-heading-lvl nil
                org-download-image-dir "./images")
  :hook (org-mode . org-download-enable)
  :config
  (add-hook 'org-mode-hook #'org-display-inline-images)
  (add-hook 'org-mode-hook (lambda () (setq org-image-actual-width nil))))

;; ==============================
;; CUSTOM MINOR MODES
;; ==============================

(define-minor-mode md-pretty-mode
  "Minor mode for pretty-printing Markdown files in Emacs."
  :lighter " MD-Pretty"
  :global nil
  (if md-pretty-mode
      (progn
        (visual-line-mode 1)
        (variable-pitch-mode 1)
        (setq-local left-margin-width 10)
        (setq-local right-margin-width 10)
        (set-window-buffer (selected-window) (current-buffer)))
    (progn
      (visual-line-mode -1)
      (variable-pitch-mode -1)
      (setq-local left-margin-width 0)
      (setq-local right-margin-width 0)
      (set-window-buffer (selected-window) (current-buffer)))))

(provide 'md-pretty-mode)
(add-hook 'markdown-mode-hook #'md-pretty-mode)
