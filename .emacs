(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(package-initialize)

;;; pure vanilla emacs {{{
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(show-paren-mode 1)
(blink-cursor-mode 0)
(linum-mode 1)

(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

(require 'dired-x)
(setq-default dired-omit-files-p t) ; Buffer-local variable
(setq dired-omit-files (concat dired-omit-files "\\|^\\..+$"))
(add-hook 'dired-mode-hook 'dired-hide-details-mode)

(setq initial-scratch-message "")
(setq org-startup-indented t)

;;; Make C-w into backward-delete-word like it should be {{{
(defun delete-word (arg)
  "Delete characters forward until the end of the word.
With argument, do this that many times"
  (interactive "p")
  (if (use-region-p)
      (delete-region (region-beginning) (region-end))
    (delete-region (point) (progn (forward-word arg) (point)))))

(defun backward-delete-word (arg)
  "Delete characters backward until the end of the word.
With argument, do this that many times"
  (interactive "p")
  (delete-word (- arg)))

(global-set-key (kbd "C-w") 'backward-delete-word)
;;; }}}

;;;  Tab settings {{{
(setq-default indent-tabs-mode nil
              tab-width 4)
(global-set-key (kbd "TAB") 'tab-to-tab-stop)
;;; }}}

;;;  scroll more vim-like {{{
(setq scroll-margin 3
      scroll-step 1
      scroll-conservatively 0
      scroll-up-aggressively 0.01
      scroll-down-aggressively 0.01)
(setq-default scroll-up-aggressively 0.01
              scroll-down-aggressively 0.01)
;;; }}}
;;; }}}

;;;  Prettify stuff {{{
(setq moe-light-pure-white-background-in-terminal t)
(add-to-list 'custom-theme-load-path ".emacs.d/elpa/moe-theme-20170914.2111/")
(add-to-list 'default-frame-alist '(font . "Iosevka-14"))
;;; }}}


;;;  evil
;;;  evil utils
(defun load-current-file ()
  "Load the current file as elisp"
  (interactive)
  (load (buffer-file-name)))

(defun evil-define-weak-key (map key action)
  "Define a key that works in move-y evil states!"
  (evil-define-key 'motion map key action)
  (evil-define-key 'visual map key action)
  (evil-define-key 'normal map key action)
  (evil-define-key 'operator map key action))

(defun evil-define-strong-key (map key action)
  "Define a key that works in both type-y and move-y evil states!"
  (evil-define-key 'insert map key action)
  (evil-define-key 'replace map key action)
  (evil-define-weak-key map key action))

(define-minor-mode evil-staples
  "Basic conveniences"
  :global t
  :keymap (make-sparse-keymap))

(defun evie-init-evil ()
  "Configure vanilla evil"

  (evil-ex-define-cmd      "source"                         'load-current-file)

  (evil-define-weak-key    evil-staples-map (kbd "C-u")     'evil-scroll-up) ; for some reason this is not bound by default

  (evil-define-weak-key    evil-staples-map ";"             'evil-ex)
  (evil-define-weak-key    evil-staples-map ":"             'evil-repeat-find-char)
  (evil-define-weak-key    evil-staples-map "9"             'evil-first-non-blank)
  (evil-define-weak-key    evil-staples-map "7"             'evil-scroll-line-down)
  (evil-define-weak-key    evil-staples-map "8"             'evil-scroll-line-up)
  (evil-define-weak-key    evil-staples-map (kbd "SPC w")   'save-buffer)
  (evil-define-key 'insert evil-staples-map (kbd "C-x C-p") 'evil-complete-previous)
  (evil-define-key 'insert evil-staples-map (kbd "C-x C-n") 'evil-complete-next)
  (evil-define-key 'insert evil-staples-map (kbd "C-x C-l") 'evil-complete-previous-line)
  (evil-define-strong-key  evil-staples-map (kbd "C-h")     'evil-window-left)
  (evil-define-strong-key  evil-staples-map (kbd "C-j")     'evil-window-down)
  (evil-define-strong-key  evil-staples-map (kbd "C-k")     'evil-window-up)
  (evil-define-strong-key  evil-staples-map (kbd "C-l")     'evil-window-right)
  (add-hook 'emacs-startup-hook 'evil-staples))

(defun evil-packagelist ()
  "Use evil keybindings in M-x package-list-packages"
  (evil-define-key 'normal package-menu-mode-map "i" #'package-menu-mark-install)
  (evil-define-key 'normal package-menu-mode-map "u" #'package-menu-mark-unmark)
  (evil-define-key 'normal package-menu-mode-map "x" #'package-menu-execute)
  (evil-set-initial-state 'packake-menu-mode 'normal))

(require 'use-package)
(require 'delight)

(use-package evil
  :ensure t
  :init
  (setq evil-shift-width 4)
  (setq evil-repeat-move-cursor nil)
  (setq evil-insert-state-cursor '(bar))
  (setq evil-replace-state-cursor '(hbar))
  (setq evil-operator-state-cursor '(hbar))
  :config
  (evil-mode 1)
  (evie-init-evil)
  (evil-packagelist)
  (evil-staples))

(use-package evil-surround
  :ensure t
  :after (evil)
  :config
  (global-evil-surround-mode 1))

(use-package evil-escape
  :ensure t
  :delight
  :after (evil)
  :init
  (setq evil-escape-key-sequence "jk"
        evil-escape-unordered-key-sequence t)
  :config (evil-escape-mode 1))

(use-package avy
  :ensure t
  :after (evil)
  :config
  (evil-define-weak-key evil-staples-map (kbd ", w") 'avy-goto-word-0-below)
  (evil-define-weak-key evil-staples-map (kbd ", b") 'avy-goto-word-0-above)
  (evil-define-weak-key evil-staples-map (kbd ", j") 'avy-goto-line-below)
  (evil-define-weak-key evil-staples-map (kbd ", k") 'avy-goto-line-above))

(use-package evil-magit
  :ensure t
  :after (evil))

(use-package fzf
  :ensure t
  :config
  
  (global-set-key (kbd "C-w") 'backward-kill-word))

(use-package ivy
  :ensure t
  :delight
  :config
  (ivy-mode 1))

(use-package counsel
  :ensure t
  :delight
  :config
  (counsel-mode)
  (global-set-key (kbd "C-x C-f") 'counsel-file-jump)
  (evil-define-weak-key evil-staples-map (kbd "SPC e") 'counsel-file-jump))

(use-package mips-mode
  :ensure t
  :mode "\\.s$")

(use-package folding
  :ensure t
  :delight
  :config
  (folding-add-to-marks-list 'emacs-lisp-mode "{{{" "}}}" nil t)
  (folding-mode 1))

;;; generated section {{{

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector
   ["#5f5f5f" "#ff4b4b" "#a1db00" "#fce94f" "#5fafd7" "#d18aff" "#afd7ff" "#ffffff"])
 '(custom-enabled-themes (quote (ujelly)))
 '(custom-safe-themes
   (quote
    ("b9cbfb43711effa2e0a7fbc99d5e7522d8d8c1c151a3194a4b176ec17c9a8215" "a19265ef7ecc16ac4579abb1635fd4e3e1185dcacbc01b7a43cf7ad107c27ced" "7527f3308a83721f9b6d50a36698baaedc79ded9f6d5bd4e9a28a22ab13b3cb1" "9deeab438d1d798c26d41c759d74a2406802427ff6acb7dec8cec961bcb4e7d5" default)))
 '(inhibit-startup-screen t)
 '(package-selected-packages
   (quote
    (delight folding counsel vimish-fold dired-rainbow git-gutter flycheck-haskell haskell-mode hasky-stack hindent cargo rust-mode auctex avy use-package moe-theme evil-surround evil-escape ivy ujelly-theme org-link-minor-mode circe evil)))
 '(undo-tree-visualizer-diff t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;;; }}}
