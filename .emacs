(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))

(package-initialize)

;;; function declarations: shut up the linter some
(declare-function evil-define-key        nil)
(declare-function evil-ex-define-cmd     nil)
(declare-function evil-set-initial-state nil)

;;; pure vanilla emacs
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(show-paren-mode 1)
(blink-cursor-mode 0)
(global-linum-mode 0)
(electric-pair-mode)

(setq-default undo-tree-auto-save-history t)
(setq-default vc-follow-symlinks t)

(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

(require 'dired-x)
(setq-default dired-omit-files-p t)     ; Buffer-local variable
(setq dired-omit-files (concat dired-omit-files "\\|^\\..+$"))
(add-hook 'dired-mode-hook 'dired-hide-details-mode)

(setq-default browse-url-generic-program "min"
              browse-url-browser-function "min"
              gnus-button-url 'browse-url-generic)

;;; make C-w into backward-delete-word like it should be
(defun delete-word (arg)
  "Delete characters forward until the end of the word.
With ARG, do this that many times"
  (interactive "p")
  (if (use-region-p)
      (delete-region (region-beginning) (region-end))
    (delete-region (point) (progn (forward-word arg) (point)))))

(defun backward-delete-word (arg)
  "Delete characters backward until the end of the word.
With ARG, do this that many times"
  (interactive "p")
  (delete-word (- arg)))

(global-set-key (kbd "C-w") 'backward-delete-word)

(setq initial-scratch-message "")
(setq-default org-startup-indented t)


;;; <tab> settings
(setq-default indent-tabs-mode nil
              tab-width 4)


;;; scroll more vim-like
(setq scroll-margin 3
      scroll-step 1
      scroll-conservatively 0
      scroll-up-aggressively 0.01
      scroll-down-aggressively 0.01)
(setq-default scroll-up-aggressively 0.01
              scroll-down-aggressively 0.01)

;;; prettify stuff
;; (require 'moe-theme)
;; (require 'moe-theme-switcher)
;; (setq-default moe-light-pure-white-background-in-terminal t)
(add-to-list 'default-frame-alist '(font . "Iosevka-14"))

(defun daytime-p (&optional dawn dusk)
  "Is it daytime? It is if it's between DAWN and DUSK o'clock, inclusive."
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
  "Use the appropriate theme: one for daytime, one for nighttime.  And no themes for terminals!"
  (interactive "SDaytime theme: \nSNighttime theme: ")
  (when (display-graphic-p)
    (load-theme (if (daytime-p dawn dusk)
                    daytime
                  nighttime))))

(setq-default eliza-daytime-theme 'minimal-light)
(setq-default eliza-nighttime-theme 'minimal)

(add-hook 'server-visit-hook (apply-partially #'eliza-load-theme eliza-daytime-theme eliza-nighttime-theme))


;;; evil utils
(defun load-current-file ()
  "Load the current file as elisp."
  (interactive)
  (load (buffer-file-name)))

(defun evil-define-weak-key (map key action)
  "Define a key that works in move-y evil states!"
  (evil-define-key 'motion   map key action)
  (evil-define-key 'visual   map key action)
  (evil-define-key 'normal   map key action)
  (evil-define-key 'operator map key action))

(defun evil-define-strong-key (map key action)
  "Define a key that works in both type-y and move-y evil states!"
  (evil-define-key 'insert  map key action)
  (evil-define-key 'replace map key action)
  (evil-define-weak-key map key action))


;;;  evil-staples
(define-minor-mode evil-staples
  "Basic conveniences"
  :global t
  :keymap (make-sparse-keymap))

(defun eliza-init-evil ()
  "Configure vanilla evil."

  ;; Ex commands
  (evil-ex-define-cmd      "source"                         'load-current-file)

  ;; Keybindings
  (evil-define-weak-key    evil-staples-map (kbd "C-u")     'evil-scroll-up) ; for some reason this is not bound by default

  (evil-define-weak-key    evil-staples-map ";"             'evil-ex)
  (evil-define-weak-key    evil-staples-map ":"             'evil-repeat-find-char)
  (evil-define-weak-key    evil-staples-map "9"             'evil-first-non-blank)
  (evil-define-weak-key    evil-staples-map "7"             'evil-scroll-line-down)
  (evil-define-weak-key    evil-staples-map "8"             'evil-scroll-line-up)
  (evil-define-weak-key    evil-staples-map (kbd "SPC w")   'save-buffer)
  (evil-define-weak-key    evil-staples-map (kbd "SPC SPC") 'execute-extended-command)
  (evil-define-weak-key    evil-staples-map (kbd "SPC h k") 'describe-key)
  (evil-define-weak-key    evil-staples-map (kbd "SPC h f") 'describe-function)
  (evil-define-weak-key    evil-staples-map (kbd "SPC h v") 'describe-variable)
  (evil-define-weak-key    evil-staples-map (kbd "SPC h p") 'describe-package)
  (evil-define-key 'normal evil-staples-map (kbd "M-p")     'switch-to-prev-buffer)
  (evil-define-key 'normal evil-staples-map (kbd "M-n")     'switch-to-next-buffer)
  (evil-define-key 'insert evil-staples-map (kbd "C-x C-p") 'evil-complete-previous)
  (evil-define-key 'insert evil-staples-map (kbd "C-x C-n") 'evil-complete-next)
  (evil-define-key 'insert evil-staples-map (kbd "C-x C-l") 'evil-complete-previous-line)
  (evil-define-strong-key  evil-staples-map (kbd "C-h")     'evil-window-left)
  (evil-define-strong-key  evil-staples-map (kbd "C-j")     'evil-window-down)
  (evil-define-strong-key  evil-staples-map (kbd "C-k")     'evil-window-up)
  (evil-define-strong-key  evil-staples-map (kbd "C-l")     'evil-window-right)
  (evil-define-strong-key  evil-staples-map (kbd "C-l")     'evil-window-right)

  (add-hook 'evil-normal-state-entry-hook    (lambda () (set-face-background 'mode-line "#a4c9f5")))
  (add-hook 'evil-insert-state-entry-hook    (lambda () (set-face-background 'mode-line "#f5fffa")))
  (add-hook 'evil-replace-state-entry-hook   (lambda () (set-face-background 'mode-line "#f43a66")))
  (add-hook 'evil-visual-state-entry-hook    (lambda () (set-face-background 'mode-line "#8ebfff")))
  (add-hook 'evil-motion-state-entry-hook    (lambda () (set-face-background 'mode-line "#c186d6")))
  (add-hook 'evil-operator-state-entry-hook  (lambda () (set-face-background 'mode-line "#d4c9f5")))
  (add-hook 'evil-emacs-state-entry-hook     (lambda () (set-face-background 'mode-line "#0fadb1")))

  (evil-staples 1))

(defun evil-packagelist ()
  "Use evil keybindings in M-x package-list-packages."
  (evil-define-key 'normal package-menu-mode-map "i" #'package-menu-mark-install)
  (evil-define-key 'normal package-menu-mode-map "u" #'package-menu-mark-unmark)
  (evil-define-key 'normal package-menu-mode-map "x" #'package-menu-execute)
  (evil-set-initial-state 'package-menu-mode 'normal))

;;;  eliza-outline
(require 'outline)

(define-minor-mode eliza-outline
  "Evil keybindings for some useful outline commands"
  :keymap (make-sparse-keymap))

(defun eliza-init-outline ()
  (interactive)
  "Outline minor mode keybindings + settings"
  (evil-define-key 'normal eliza-outline-map (kbd "g O")   'outline-hide-body)
  (evil-define-key 'normal eliza-outline-map (kbd "g o")   'outline-toggle-children)
  (evil-define-key 'normal eliza-outline-map (kbd "[ s")   'outline-previous-visible-heading)
  (evil-define-key 'normal eliza-outline-map (kbd "] s")   'outline-next-visible-heading)
  (evil-define-weak-key    eliza-outline-map (kbd "RET")   (lambda () (interactive)
                                                             (if (outline-on-heading-p)
                                                                 (outline-toggle-children))))
  (setq outline-regexp ";;; +")
  (set-display-table-slot standard-display-table
                          'selective-display
                          (string-to-vector " (...)"))
  (eliza-outline))

(add-hook 'outline-minor-mode-hook (lambda () (eliza-init-outline)))
(add-hook 'emacs-lisp-mode-hook (lambda () (outline-minor-mode t)))

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

;;;  evil-commentary
(use-package evil-commentary
  :ensure t
  :after (evil)
  :config
  (evil-commentary-mode t))

;;;  key-chord
(use-package key-chord
  :ensure t
  :after (ivy)
  :config
  (eval-after-load "ivy" (progn
                           (defvar ivy-minibuffer-map)
                           (key-chord-define ivy-minibuffer-map "kl" 'ivy-avy)))
  (key-chord-mode 1))

;;;  company
(use-package company
  :ensure t
  :init
  (setq-default company-idle-delay nil)
  (defun eliza-complete ()
    "Use company if possible, otherwise use hippie expand"
    (interactive)
    (if company-mode (company-complete) (make-hippie-expand-function ; back-up completion
                                         '(try-expand-dabbrev
                                           try-complete-lisp-symbol-partially
                                           try-expand-dabbrev-all-buffers))))
  (global-set-key (kbd "TAB") 'eliza-complete)

  :config
  (global-company-mode t))

;;;  sr-speedbar
(use-package sr-speedbar
  :ensure t)
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
  (evil-define-weak-key evil-staples-map (kbd "M-f") 'swiper)
  (ivy-mode 1))

;;;  counsel
(use-package counsel
  :ensure t
  :delight
  :config
  (counsel-mode)
  (global-set-key (kbd "C-x C-f") 'counsel-file-jump)
  (evil-define-weak-key evil-staples-map (kbd "SPC e") 'counsel-find-file))

;;;  projectile
(use-package projectile
  :ensure t
  :ensure counsel-projectile)

;;;  mips
(use-package mips-mode
  :ensure t
  :mode "\\.s$")

;;;  flycheck
(use-package flycheck
  :ensure t
  :init
  (evil-define-weak-key evil-staples-map (kbd "[ e") 'flycheck-previous-error)
  (evil-define-weak-key evil-staples-map (kbd "] e") 'flycheck-next-error)
  (setq-default flycheck-indication-mode nil)
  :config
  (global-flycheck-mode 1))

;;;  telephone-line
(use-package telephone-line
  :ensure t
  :init
  (defface eliza-secondary '((t (:background "#e096a0" :foreground "#ebfff0"))) "" :group 'mode-line)
  (setq telephone-line-faces
        '((secondary . (eliza-secondary . eliza-secondary))
          (evil      . telephone-line-evil-face)
          (nil       . (mode-line . mode-line-inactive))))

  (telephone-line-defsegment eliza-telephone-vc ()
    (replace-regexp-in-string "Git[:-]" "" vc-mode))
  (telephone-line-defsegment eliza-telephone-buffer ()
    mode-line-buffer-identification)
  (telephone-line-defsegment eliza-telephone-position ()
    `("%l / "
      ,(number-to-string (line-number-at-pos (point-max)))))
  (telephone-line-defsegment eliza-telephone-modified ()
    (if (buffer-modified-p)
        (if buffer-read-only
            "[RO+]"
          "[+]")
      (if buffer-read-only
          "[RO]"
        "")))

  (setq telephone-line-lhs
        '((evil . (telephone-line-vc-segment))
          (secondary . (eliza-telephone-buffer eliza-telephone-modified))
          (nil       . "")))
  (setq telephone-line-rhs
        '((nil . (telephone-line-simple-major-mode-segment))
          (secondary . (eliza-telephone-position))))

  :config
  (telephone-line-mode t))

;;; generated section

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(TeX-view-program-selection
   (quote
    (((output-dvi has-no-display-manager)
      "dvi2tty")
     ((output-dvi style-pstricks)
      "dvips and gv")
     (output-dvi "xdvi")
     (output-pdf "Zathura")
     (output-html "xdg-open"))))
 '(ansi-color-names-vector [])
 '(custom-enabled-themes (quote (flatui)))
 '(custom-safe-themes
   (quote
    ("8db4b03b9ae654d4a57804286eb3e332725c84d7cdab38463cb6b97d5762ad26" "cc0dbb53a10215b696d391a90de635ba1699072745bf653b53774706999208e3" "3e335d794ed3030fefd0dbd7ff2d3555e29481fe4bbb0106ea11c660d6001767" "15348febfa2266c4def59a08ef2846f6032c0797f001d7b9148f30ace0d08bcf" "bce3ae31774e626dce97ed6d7781b4c147c990e48a35baedf67e185ebc544a56" "291588d57d863d0394a0d207647d9f24d1a8083bb0c9e8808280b46996f3eb83" default)))
 '(evil-commentary-mode t)
 '(fci-rule-color "#f1c40f" t)
 '(flycheck-disabled-checkers (quote (emacs-lisp-checkdoc tex-chktex)))
 '(hl-paren-background-colors (quote ("#2492db" "#95a5a6" nil)))
 '(hl-paren-colors (quote ("#ecf0f1" "#ecf0f1" "#c0392b")))
 '(inhibit-startup-screen t)
 '(package-selected-packages
   (quote
    (counsel-projectile projectile-speedbar projectile minimal-theme flatui-theme lenlen-theme key-chord company flycheck-inline zerodark-theme vimish-fold use-package ujelly-theme telephone-line outshine moe-theme mips-mode lispyville hindent hasky-stack git-gutter fzf folding flycheck-haskell evil-surround evil-magit evil-escape evil-commentary dracula-theme dired-rainbow delight counsel circe cargo auctex)))
 '(recentf-mode t)
 '(sml/active-background-color "#34495e")
 '(sml/active-foreground-color "#ecf0f1")
 '(sml/inactive-background-color "#dfe4ea")
 '(sml/inactive-foreground-color "#34495e")
 '(undo-tree-visualizer-diff t)
 '(vc-annotate-background "#ecf0f1")
 '(vc-annotate-color-map
   (quote
    ((30 . "#e74c3c")
     (60 . "#c0392b")
     (90 . "#e67e22")
     (120 . "#d35400")
     (150 . "#f1c40f")
     (180 . "#d98c10")
     (210 . "#2ecc71")
     (240 . "#27ae60")
     (270 . "#1abc9c")
     (300 . "#16a085")
     (330 . "#2492db")
     (360 . "#0a74b9"))))
 '(vc-annotate-very-old-color "#0a74b9"))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
