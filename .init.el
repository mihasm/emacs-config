; removes start screen
(setq inhibit-startup-message t) 
(setq initial-scratch-message nil)
; removes menu bar
(menu-bar-mode -1)
; adds melpa repo
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)
; sets initial mode to text mode on emacs startup
(setq initial-major-mode 'text-mode)
; enables drag stuff and keyboard shortcuts

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages '(drag-stuff)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(drag-stuff-global-mode 1)
(global-set-key (kbd "ESC <down>") 'drag-stuff-down)
(global-set-key (kbd "ESC <up>") 'drag-stuff-up)
(global-set-key (kbd "ESC <left>") 'drag-stuff-left)
(global-set-key (kbd "ESC <right>") 'drag-stuff-right)
