(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))

(package-initialize)

;;; function declarations: shut up the linter some
(declare-function evil-define-key        nil)
(declare-function evil-delay             nil)
(declare-function evil-ex-define-cmd     nil)
(declare-function evil-set-initial-state nil)
(declare-function lispy-set-key-theme    nil)

;;; pure vanilla emacs
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(show-paren-mode 1)
(blink-cursor-mode 0)
(global-linum-mode 1)

(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

(require 'dired-x)
(setq-default dired-omit-files-p t)     ; Buffer-local variable
(setq dired-omit-files (concat dired-omit-files "\\|^\\..+$"))
(add-hook 'dired-mode-hook 'dired-hide-details-mode)



;;; make C-w into backward-delete-word like it should be
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

(setq initial-scratch-message "")
(setq-default org-startup-indented t)


;;; <tab> settings
(setq-default indent-tabs-mode nil
              tab-width 4)
(global-set-key (kbd "TAB") 'tab-to-tab-stop)


;;; scroll more vim-like
(setq scroll-margin 3
      scroll-step 1
      scroll-conservatively 0
      scroll-up-aggressively 0.01
      scroll-down-aggressively 0.01)
(setq-default scroll-up-aggressively 0.01
              scroll-down-aggressively 0.01)

;;; prettify stuff
(setq-default moe-light-pure-white-background-in-terminal t)
(add-to-list 'custom-theme-load-path ".emacs.d/elpa/moe-theme-20170914.2111/")
(add-to-list 'default-frame-alist '(font . "Iosevka-14"))

(defun daytime-p (&optional dawn dusk)
  "Is it daytime? It is if it's between dawn and dusk o'clock, inclusive."
  (let* ((time-to-minutes (lambda (hours minutes)
                            "Convert hour:minutes format to just minutes"
                            (+ minutes (* 60 hours))))
         (time-  (decode-time))
         (hour   (nth 2 time-))
         (minute (nth 1 time-))
         (now    (funcall time-to-minutes hour minute))
         (dawn   (funcall time-to-minutes 6 0))
         (dusk   (funcall time-to-minutes 19 30)))
    
    (if (and (>= now dawn) (< now dusk))
        t
      nil)))

(defun eliza-load-theme (daytime nighttime &optional dawn dusk)
  "Use the appropriate theme: one for daytime, one for nighttime. And no themes for terminals!"
  (interactive "SDaytime theme: \nSNighttime theme: ")
  (when (display-graphic-p)
    (load-theme (if (daytime-p dawn dusk)
                    daytime
                  nighttime))))

(add-hook 'server-visit-hook (lambda () (eliza-load-theme 'moe-light 'zerodark)))


;;; evil utils
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

;;;  evil staples
(define-minor-mode evil-staples
  "Basic conveniences"
  :global t
  :keymap (make-sparse-keymap))

(defun eliza-init-evil ()
  "Configure vanilla evil"

  (evil-ex-define-cmd      "source"                         'load-current-file)

  (evil-define-weak-key    evil-staples-map (kbd "C-u")     'evil-scroll-up) ; for some reason this is not bound by default

  (evil-define-weak-key    evil-staples-map ";"             'evil-ex)
  (evil-define-weak-key    evil-staples-map ":"             'evil-repeat-find-char)
  (evil-define-weak-key    evil-staples-map "9"             'evil-first-non-blank)
  (evil-define-weak-key    evil-staples-map "7"             'evil-scroll-line-down)
  (evil-define-weak-key    evil-staples-map "8"             'evil-scroll-line-up)
  (evil-define-weak-key    evil-staples-map (kbd "SPC w")   'save-buffer)
  (evil-define-weak-key    evil-staples-map (kbd "SPC SPC") #'execute-extended-command)
  (evil-define-key 'insert evil-staples-map (kbd "C-x C-p") 'evil-complete-previous)
  (evil-define-key 'insert evil-staples-map (kbd "C-x C-n") 'evil-complete-next)
  (evil-define-key 'insert evil-staples-map (kbd "C-x C-l") 'evil-complete-previous-line)
  (evil-define-strong-key  evil-staples-map (kbd "C-h")     'evil-window-left)
  (evil-define-strong-key  evil-staples-map (kbd "C-j")     'evil-window-down)
  (evil-define-strong-key  evil-staples-map (kbd "C-k")     'evil-window-up)
  (evil-define-strong-key  evil-staples-map (kbd "C-l")     'evil-window-right)

  (evil-staples 1))

(defun evil-packagelist ()
  "Use evil keybindings in M-x package-list-packages"
  (evil-define-key 'normal package-menu-mode-map "i" #'package-menu-mark-install)
  (evil-define-key 'normal package-menu-mode-map "u" #'package-menu-mark-unmark)
  (evil-define-key 'normal package-menu-mode-map "x" #'package-menu-execute)
  (evil-set-initial-state 'packake-menu-mode 'normal))

;;;  eliza-outline
(define-minor-mode eliza-outline
  "Evil keybindings for some useful outline commands"
  :keymap (make-sparse-keymap))

(defun eliza-init-outline ()
  "Outline minor mode keybindings"
  (evil-define-key 'normal eliza-outline-map (kbd "g O")   'outline-hide-body)
  (evil-define-key 'normal eliza-outline-map (kbd "g o")   'outline-toggle-children)
  (evil-define-key 'normal eliza-outline-map (kbd "[ s")   'outline-previous-visible-heading)
  (evil-define-key 'normal eliza-outline-map (kbd "] s")   'outline-next-visible-heading)
  (eliza-outline))
(provide 'eliza-outline)

(add-hook 'outline-minor-mode-hook (lambda () (eliza-outline t)))
(add-hook 'emacs-lisp-mode-hook (lambda () (eliza-outline t)))

;;; package-specific settings
(require 'use-package)
(require 'delight)

;;;  evil
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
  (eliza-init-evil)
  (eliza-init-outline)
  (evil-packagelist))

;;;  evil-surround
(use-package evil-surround
  :ensure t
  :after (evil)
  :config
  (global-evil-surround-mode 1))

;;;  evil-escape
(use-package evil-escape
  :ensure t
  :delight
  :after (evil)
  :init
  (setq evil-escape-key-sequence "jk"
        evil-escape-unordered-key-sequence t)
  :config (evil-escape-mode 1))

;;;  avy
(use-package avy
  :ensure t
  :after (evil)
  :config
  (evil-define-weak-key evil-staples-map (kbd ", w") 'avy-goto-word-0-below)
  (evil-define-weak-key evil-staples-map (kbd ", b") 'avy-goto-word-0-above)
  (evil-define-weak-key evil-staples-map (kbd ", j") 'avy-goto-line-below)
  (evil-define-weak-key evil-staples-map (kbd ", k") 'avy-goto-line-above))

;;;  evil-magit
(use-package evil-magit
  :ensure t
  :after (evil)
  :config
  (evil-define-weak-key evil-staples-map (kbd "SPC m g") 'magit))

;;;  git-gutter
(use-package git-gutter
  :ensure t
  :config
  (evil-define-weak-key evil-staples-map (kbd "[ h") 'git-gutter:previous-hunk)
  (evil-define-weak-key evil-staples-map (kbd "] h") 'git-gutter:next-hunk)
  (global-git-gutter-mode 1))

;;;  ivy
(use-package ivy
  :ensure t
  :delight
  :config
  (define-key ivy-minibuffer-map (kbd "C-k") 'ivy-previous-line)
  (define-key ivy-minibuffer-map (kbd "C-j") 'ivy-next-line)
  (define-key ivy-minibuffer-map (kbd "M-j") 'ivy-avy) ; TODO make this the keychord kl
  (ivy-mode 1))

;;;  counsel
(use-package counsel
  :ensure t
  :delight
  :config
  (counsel-mode)
  (global-set-key (kbd "C-x C-f") 'counsel-file-jump)
  (evil-define-weak-key evil-staples-map (kbd "SPC e") 'counsel-file-jump))

;;;  lispyville 
(use-package lispyville
  :ensure lispy
  :ensure t
  :delight                         ; delight lispyville, but not lispy
  :config
  (setq-default lispy-comment-use-single-semicolon t)
  (lispy-set-key-theme '(lispy
                         operators
                         c-w
                         (escape insert)
                         additional
                         slurp/barf-lispy))
  (evil-define-key 'normal lispyville-mode-map (kbd "M-k") #'lispy-move-up)
  (evil-define-key 'normal lispyville-mode-map (kbd "M-h") #'lispy-move-up)
  (evil-define-key 'normal lispyville-mode-map (kbd "M-j") #'lispy-move-down)
  (evil-define-key 'normal lispyville-mode-map (kbd "M-l") #'lispy-move-down)

  ;; don't use lispy's expansion of (; -> ;;) and (;; -> autoload cookie)
  (define-key lispy-mode-map (kbd ";") nil)
  ;; lispy's default behavior is to escape all quotes inside of quotes, rather than exiting the quote
  (define-key lispy-mode-map (kbd "\"") nil)

  (add-hook 'lispy-mode-hook #'lispyville-mode)
  (add-hook 'emacs-lisp-mode-hook (lambda ()
                                    (lispy-mode 1))))

;;;  mips-mode
(use-package mips-mode
  :ensure t
  :mode "\\.s$")
        
;;;  flycheck
(use-package flycheck
  :ensure t
  :init
  (setq-default flycheck-disabled-checkers '(emacs-lisp-checkdoc))
  (evil-define-weak-key evil-staples-map (kbd "[ e") 'flycheck-previous-error)
  (evil-define-weak-key evil-staples-map (kbd "] e") 'flycheck-next-error)
  (global-flycheck-mode 1))

;;; generated section

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector
   ["#5f5f5f" "#ff4b4b" "#a1db00" "#fce94f" "#5fafd7" "#d18aff" "#afd7ff" "#ffffff"])
 '(custom-enabled-themes nil)
 '(custom-safe-themes
   (quote
    ("bce3ae31774e626dce97ed6d7781b4c147c990e48a35baedf67e185ebc544a56" "ff7625ad8aa2615eae96d6b4469fcc7d3d20b2e1ebc63b761a349bebbb9d23cb" "b9cbfb43711effa2e0a7fbc99d5e7522d8d8c1c151a3194a4b176ec17c9a8215" "a19265ef7ecc16ac4579abb1635fd4e3e1185dcacbc01b7a43cf7ad107c27ced" "7527f3308a83721f9b6d50a36698baaedc79ded9f6d5bd4e9a28a22ab13b3cb1" "9deeab438d1d798c26d41c759d74a2406802427ff6acb7dec8cec961bcb4e7d5" default)))
 '(flycheck-disabled-checkers (quote (emacs-lisp-checkdoc)))
 '(inhibit-startup-screen t)
 '(package-selected-packages
   (quote
    (zerodark-theme dracula-theme lispyville delight folding counsel vimish-fold dired-rainbow git-gutter flycheck-haskell haskell-mode hasky-stack hindent cargo rust-mode auctex avy use-package moe-theme evil-surround evil-escape ivy ujelly-theme org-link-minor-mode circe evil)))
 '(undo-tree-visualizer-diff t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
