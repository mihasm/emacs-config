;; Load custom settings from a separate file
(setq custom-file (concat user-emacs-directory "custom.el"))

;; If the custom settings file exists, load it
(when (file-exists-p custom-file)
  (load custom-file))

(setq mac-command-modifier 'control)
(global-set-key (kbd "C-c C-c") 'kill-ring-save) ;; Map C-c to copy
(global-set-key (kbd "C-z") 'undo-only)
(global-set-key (kbd "C-y") 'undo-redo)
(global-set-key (kbd "C-v") 'yank)
(global-set-key (kbd "C-M-p") 'scroll-down-command)
(global-set-key (kbd "C-M-n") 'scroll-up-command)
(global-set-key (kbd "C-c RET") 'set-mark-command) ;; Where RET = C-m due to historical reasons

;; Remove the startup screen and scratch message
(setq inhibit-startup-message t)
(setq initial-scratch-message nil)

;; Remove UI elements
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(if (fboundp 'set-fringe-mode) (set-fringe-mode 0))
(tool-bar-mode -1)
(icomplete-mode 1)
(global-unset-key (kbd "C-<down-mouse-1>")) ;; some weird buffer menu

;; Auto-revert buffers when files change externally
(global-auto-revert-mode t)

;; Set initial mode to text-mode
(setq initial-major-mode 'text-mode)

;; Enable line numbers only in programming modes
(add-hook 'prog-mode-hook 'display-line-numbers-mode)
(setq display-line-numbers-type 'relative) ;; Use relative line numbers

;; Remove weird shortcuts
(global-unset-key (kbd "C-w")) ;; removes shortcut for delete until end of buffer

;; Show welcome text on startup
(find-file (concat user-emacs-directory "emacs_welcome.org"))

;; Enable MELPA for package management
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

;; Install use-package if not installed
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(require 'use-package)

;; Auto-save, backup, and lock file configuration
(setq backup-directory (concat user-emacs-directory "backups/"))
(setq auto-save-directory (concat user-emacs-directory "auto-saves/"))
(setq lock-file-directory (concat user-emacs-directory "lock-files/"))
(unless (file-exists-p backup-directory)
  (make-directory backup-directory t))
(unless (file-exists-p auto-save-directory)
  (make-directory auto-save-directory t))
(unless (file-exists-p lock-file-directory)
  (make-directory lock-file-directory t))
(setq backup-directory-alist `(("." . ,backup-directory)))
(setq auto-save-file-name-transforms `((".*" ,auto-save-directory t)))
(setq lock-file-name-transforms `((".*" ,lock-file-directory t)))

;; Theme and Font
(use-package monokai-theme
  :ensure t)

(set-face-attribute 'default nil
                    :family "Menlo"
                    :height 120)

;; Disable arrow keys for movement
(global-unset-key (kbd "<left>"))
(global-unset-key (kbd "<right>"))
(global-unset-key (kbd "<up>"))
(global-unset-key (kbd "<down>"))
(global-unset-key (kbd "<C-left>"))
(global-unset-key (kbd "<C-right>"))
(global-unset-key (kbd "<C-up>"))
(global-unset-key (kbd "<C-down>"))

;; drag-stuff package
(use-package drag-stuff
  :ensure t
  :config
  (drag-stuff-global-mode 1)
  (global-set-key (kbd "ESC <down>") 'drag-stuff-down)
  (global-set-key (kbd "M-<down>") 'drag-stuff-down)
  (global-set-key (kbd "ESC <up>") 'drag-stuff-up)
  (global-set-key (kbd "M-<up>") 'drag-stuff-up)
  (global-set-key (kbd "ESC <left>") 'drag-stuff-left)
  (global-set-key (kbd "M-<left>") 'drag-stuff-left)
  (global-set-key (kbd "ESC <right>") 'drag-stuff-right)
  (global-set-key (kbd "M-<right>") 'drag-stuff-right))

;; Obsidian package
(use-package obsidian
  :ensure t
  :config
  (obsidian-specify-path "~/Proton Drive/Obsidian")
  (setq obsidian-wiki-link-create-file-in-inbox nil)
  (setq obsidian-daily-notes-directory "Zapiski")
  (global-obsidian-mode t)
  (add-hook 'obsidian-mode-hook
            (lambda ()
              (local-set-key (kbd "C-c C-o") 'obsidian-follow-link-at-point)
              (local-set-key (kbd "C-c C-l") 'obsidian-insert-wikilink)
              (local-set-key (kbd "C-c C-b") 'obsidian-backlink-jump)))
)

;; Multiple cursors
(use-package multiple-cursors
  :ensure t
  :bind (("C-d" . mc/mark-next-like-this)
         ("C-c C-d" . mc/mark-all-like-this))
)

(defun force-global-C-d ()
  (local-set-key (kbd "C-d") 'mc/mark-next-like-this))
(add-hook 'after-change-major-mode-hook 'force-global-C-d)


;; Shell mode shortcuts
(add-hook 'shell-mode-hook
          (lambda ()
            (define-key shell-mode-map (kbd "<up>") 'comint-previous-input)
            (define-key shell-mode-map (kbd "<down>") 'comint-next-input)))

;; exec-path-from-shell
(use-package exec-path-from-shell
  :ensure t
  :config
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize)))

;; C-mode configuration
(defun set-compile-command-for-c ()
  (when (eq major-mode 'c-mode)
    (let* ((file (file-name-nondirectory buffer-file-name))
           (output-file (file-name-sans-extension file)))
      (setq compile-command (format "gcc %s -o %s" file output-file)))))
(add-hook 'c-mode-hook 'set-compile-command-for-c)

;; Poet theme
(use-package poet-theme
  :ensure t
)

;; Eglot - LSP client built into Emacs 29+
(use-package eglot
  :ensure nil  ;; Since eglot is already included, no need to install
  :config
  ;; Add a list of language servers to be used
  (add-to-list 'eglot-server-programs
               '((c-mode c++-mode) . ("clangd")))
  (add-to-list 'eglot-server-programs
               '((python-mode) . ("pylsp")))
  ;; Start Eglot automatically when opening a supported file
  (add-hook 'c-mode-hook 'eglot-ensure)
  (add-hook 'c++-mode-hook 'eglot-ensure)
  (add-hook 'python-mode-hook 'eglot-ensure)
  )

;; Org mode settings
(use-package org-download
  :ensure t
)
(require 'org-download)
(pixel-scroll-precision-mode 1)
(org-display-inline-images 1)
(setq-default org-download-heading-lvl nil)
(setq-default org-download-image-dir "./images")

;; Dired
(require 'dired)
(put 'dired-find-alternate-file 'disabled nil)
(define-key dired-mode-map (kbd "<mouse-2>") 'dired-find-alternate-file)

(require 'dired-x)
(setq dired-omit-files "^\\...+$")
(add-hook 'dired-mode-hook (lambda () (dired-omit-mode 1)))
(local-set-key (kbd "M-o") 'dired-omit-mode)
