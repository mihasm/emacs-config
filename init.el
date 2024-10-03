;; Load custom settings from a separate file
(setq custom-file (concat user-emacs-directory "custom.el"))

;; If the custom settings file exists, load it
(when (file-exists-p custom-file)
  (load custom-file))

;; Remove the startup screen and scratch message
(setq inhibit-startup-message t)
(setq initial-scratch-message nil)

;; Remove UI elements
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(if (fboundp 'set-fringe-mode) (set-fringe-mode 0))
(tool-bar-mode -1)
(icomplete-mode 1)

;; Auto-revert buffers when files change externally
(global-auto-revert-mode t)

;; Set initial mode to text-mode
(setq initial-major-mode 'text-mode)

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
              (local-set-key (kbd "C-c C-b") 'obsidian-backlink-jump))))

;; Multiple cursors
(use-package multiple-cursors
  :ensure t
  :bind (("C-d" . mc/mark-next-like-this)
         ("C-c C-d" . mc/mark-all-like-this)))

(defun force-global-C-d ()
  (local-set-key (kbd "C-d") 'mc/mark-next-like-this))
(add-hook 'after-change-major-mode-hook 'force-global-C-d)

;; Company mode
(use-package company
  :ensure t
  :hook (after-init . global-company-mode))

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

;; ggtags for navigation in C/C++
(use-package ggtags
  :ensure t
  :hook ((c-mode c++-mode) . ggtags-mode))
(global-set-key (kbd "M-.") 'ggtags-find-definition)
(global-set-key (kbd "M-,") 'pop-tag-mark)
(setq gtags-suggested-key-mapping t)
(setq ggtags-include-directories '("/Library/Developer/CommandLineTools/SDKs/MacOSX.sdk/usr/include"))
(global-set-key (kbd "C-c g") (lambda () (interactive) (shell-command "gtags --incremental")))

;; Disable mouse interactions globally
(use-package disable-mouse
  :ensure t
  :config
  (global-disable-mouse-mode))

;; Poet theme
(use-package poet-theme
  :ensure t
  ;:config
  ;(load-theme 'poet t)
  ;; Enable variable-pitch-mode in text modes (org-mode, markdown-mode, etc.)
  ;(add-hook 'text-mode-hook
  ;          (lambda ()
  ;            (variable-pitch-mode 1)))
  ;; Set custom fonts for different text faces
  ;(set-face-attribute 'default nil :family "DejaVu Sans Mono" :height 130)
  ;(set-face-attribute 'fixed-pitch nil :family "DejaVu Sans Mono")
  ;(set-face-attribute 'variable-pitch nil :family "IBM Plex Serif")
  )
