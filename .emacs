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

(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

(require 'dired-x)
(setq-default dired-omit-files-p t)     ; Buffer-local variable
(setq dired-omit-files (concat dired-omit-files "\\|^\\..+$"))
(add-hook 'dired-mode-hook 'dired-hide-details-mode)

(setq-default browse-url-generic-program "min")

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
(global-set-key (kbd "TAB") (make-hippie-expand-function
                             '(try-expand-dabbrev
                               try-complete-lisp-symbol-partially
                               try-expand-dabbrev-all-buffers)))


;;; scroll more vim-like
(setq scroll-margin 3
      scroll-step 1
      scroll-conservatively 0
      scroll-up-aggressively 0.01
      scroll-down-aggressively 0.01)
(setq-default scroll-up-aggressively 0.01
              scroll-down-aggressively 0.01)

;;; prettify stuff
(require 'moe-theme)
(require 'moe-theme-switcher)
(setq-default moe-light-pure-white-background-in-terminal t)
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

(add-hook 'server-visit-hook (lambda () (eliza-load-theme 'moe-light 'moe-dark)))


;;; evil utils
(defun load-current-file ()
  "Load the current file as elisp"
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
  "Configure vanilla evil"

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
  (evil-define-weak-key    evil-staples-map (kbd "SPC SPC") #'execute-extended-command)
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
  "Use evil keybindings in M-x package-list-packages"
  (evil-define-key 'normal package-menu-mode-map "i" #'package-menu-mark-install)
  (evil-define-key 'normal package-menu-mode-map "u" #'package-menu-mark-unmark)
  (evil-define-key 'normal package-menu-mode-map "x" #'package-menu-execute)
  (evil-set-initial-state 'package-menu-mode 'normal))

;;;  eliza-outline
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
  :config
  (global-flycheck-mode 1))

;;;    flycheck-inline
(use-package flycheck-inline
  :ensure t
  :ensure flycheck
  :after (flycheck)
  :config
  (flycheck-inline-enable))

;;;  purpose
(use-package window-purpose
  :ensure t
  :config
  (purpose-mode))
;;;  telephone-line
(use-package telephone-line
  :ensure t
  :init
  (telephone-line-defsegment eliza-telephone-vc ()
    (replace-regexp-in-string "Git:" "" vc-mode))
  (telephone-line-defsegment eliza-telephone-position ()
    (concat (line-number-at-pos (point)) " / " (line-number-at-pos (point-max))))
  
  )

;;; mode-line
(defun eliza-add-center-padding (left right)
  "Takes lists LEFT and RIGHT and concatenates them.
Adding enough spaces in between to fill out the modeline.
If the result is too big for the modeline, adds a single space between because then it's no longer my problem."
  (let ((distance (- (window-total-width) (length left) (length right))))
    (if (natnump distance)
        (concat left (repeat distance ? ) right)
      (concat left " " right))))

(defun eliza-buffer-changed-text (&optional buf)
  "Blank if the buffer has not been changed.
[+] if the buffer has been changed.
[RO] if the buffer is read-only.
[RO+] if the buffer is read-only but has been changed anyway."
 (cond ((and buffer-read-only
              (buffer-modified-p buf))
         "[RO+]")
        ((and buffer-read-only
              (not (buffer-modified-p buf)))
         "[RO]")
        ((and (not buffer-read-only)
              (buffer-modified-p buf))
         "[+]")
        ("")))

(defun eliza-git-head ()
  "Get a prettier version of (vc-mode vc-mode)"
  (replace-regexp-in-string "Git:" "" vc-mode))

(defun eliza-all-strings (&rest strings)
  "If anything in STRINGS is the empty string, returns an empty list.
Otherwise, concatenates all the element of STRINGS."
  (if (seq-every-p (apply-partially #'string< "") strings) ; the empty string is less than all non-empty strings
      (apply #'concat strings)
    '("")))

(defun eliza-flycheck-status ()
  "Get a prettier version of flycheck's status"
  (let ((status (flycheck-mode-line-status-text)))
    (concat (if (string-match "0/" status)
                ""
              (replace-regexp-in-string ".*\([0-9]+\)/.*" "\1 error(s)" status))
            " "
            (if (string-match "/0" status)
                ""
              (replace-regexp-in-string ".*/\([0-9]+\).*" "\1 warning(s)" status)))))

;;;  mode-line-format definition
(let ((mode-line-edges-color))
  (setq mode-line-format
        `((:propertize mode-line-front-space
                       face (:background ,mode-line-edges-color))
          (:propertize (:eval (eliza-all-strings (eliza-git-head) " "))
                       face (:background "#eb6086" :foreground "#ebfff0"))
          (:propertize (25 " " mode-line-buffer-identification " " (:eval (eliza-buffer-changed-text)))
                       face (:background "#e096a0" :foreground "#ebfff0"))
          mode-line-misc-info

          (:eval (make-string 20 ? ))

          (:propertize (:eval (eliza-all-strings "-" (cadr (git-gutter:statistic)) " "))
                       face (:background "red" :foreground "ebfff0"))
          (:propertize (:eval (eliza-all-strings "+" (car (git-gutter:statistic)) " "))
                       face (:background "green" :foreground "ebfff0"))
          "Line %l (%p) "
          (:eval (eliza-flycheck-status))
          " "
          (:propertize mode-line-end-spaces
                       face (:background ,mode-line-edges-color)))))

;;; generated section

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector [])
 '(custom-safe-themes
   (quote
    ("291588d57d863d0394a0d207647d9f24d1a8083bb0c9e8808280b46996f3eb83" default)))
 '(flycheck-disabled-checkers (quote (emacs-lisp-checkdoc)))
 '(inhibit-startup-screen t)
 '(package-selected-packages
   (quote
    (window-purpose flycheck-inline zerodark-theme vimish-fold use-package ujelly-theme telephone-line outshine moe-theme mips-mode lispyville hindent hasky-stack git-gutter fzf folding flycheck-haskell evil-surround evil-magit evil-escape evil-commentary embrace dracula-theme dired-rainbow delight counsel circe cargo auctex)))
 '(undo-tree-visualizer-diff t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
