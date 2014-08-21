;; INTERFACE SETUP
;; font Anonymous Pro, fallback DejaVu
(set-face-attribute 'default nil
                    :family "Anonymous Pro"
                    :height 115
                    :weight 'normal
                    :width 'normal)

(when (functionp 'set-fontset-font)
  (set-fontset-font "fontset-default"
                    'unicode
                    (font-spec :family "DejaVu Sans Mono"
                               :width 'normal
                               :size 11.4
                               :weight 'normal)))

;; enable syntax highlighting
(global-font-lock-mode 1)

;; turn off blinking cursor
(blink-cursor-mode 0)

;; turn off mouse interface early in startup to avoid momentary display
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(if (fboundp 'horizontal-scroll-bar-mode) (horizontal-scroll-bar-mode -1))

;; scrolling
(setq redisplay-dont-pause t
      scroll-margin 1
      scroll-step 1
      scroll-conservatively 10000
      scroll-preserve-screen-position 1)

;; disable annoying prompts
(fset 'yes-or-no-p 'y-or-n-p)
(setq kill-buffer-query-functions
  (remq 'process-kill-buffer-query-function
         kill-buffer-query-functions))

;; disable splash screen
(setq inhibit-startup-message t
      inhibit-startup-echo-area-message t)

;; disable scratch buffer message
(setq initial-scratch-message nil)

;; tooltips in the echo area
(tooltip-mode -1)
(setq tooltip-use-echo-area t)

;; turn on ido-mode for better buffers switching
(setq ido-enable-flex-matching t)
(setq ido-everywhere t)
(setq ido-create-new-buffer 'always)
(ido-mode 1)

;; better backspacing
(global-set-key (kbd "C-?") 'help-command)
(global-set-key (kbd "M-?") 'mark-paragraph)
(global-set-key (kbd "C-h") 'delete-backward-char)
(global-set-key (kbd "M-h") 'backward-kill-word)

;; better forward and backward paragraph
;; (see http://endlessparentheses.com/meta-binds-part-2-a-peeve-with-paragraphs.html)
(global-set-key "\M-a" 'custom/backward-paragraph)
(global-set-key "\M-e" 'custom/forward-paragraph)

(defun custom/forward-paragraph (&optional n)
  "Advance just past next blank line."
  (interactive "p")
  (let ((m (use-region-p))
        (para-commands
         '(custom/forward-paragraph custom/backward-paragraph)))
    ;; only push mark if it's not active and we're not repeating.
    (or m
        (not (member this-command para-commands))
        (member last-command para-commands)
        (push-mark))
    ;; the actual movement.
    (dotimes (_ (abs n))
      (if (> n 0)
          (skip-chars-forward "\n[:blank:]")
        (skip-chars-backward "\n[:blank:]"))
      (if (search-forward-regexp
           "\n[[:blank:]]*\n[[:blank:]]*" nil t (cl-signum n))
          (goto-char (match-end 0))
        (goto-char (if (> n 0) (point-max) (point-min)))))
    ;; if mark wasn't active, I like to indent the line too.
    (unless m
      (indent-according-to-mode)
      ;; this looks redundant, but it's surprisingly necessary.
      (back-to-indentation))))

(defun custom/backward-paragraph (&optional n)
  "Go back up to previous blank line."
  (interactive "p")
  (custom/forward-paragraph (- n)))

;; better window movings
(global-set-key "\C-x\C-n" 'other-window)
(global-set-key "\C-x\C-p" 'other-window-backward)

(defun other-window-backward (&optional n)
  "Select Nth previous window."
  (interactive "P")
  (other-window (- (prefix-numeric-value n))))

;; minor mode to hide the mode line
;; (see http://bzg.fr/emacs-hide-mode-line.html)
(defvar-local hidden-mode-line-mode nil)
(defvar-local hide-mode-line nil)

(define-minor-mode hidden-mode-line-mode
  "Minor mode to hide the mode-line in the current buffer."
  :init-value nil
  :global t
  :variable hidden-mode-line-mode
  :group 'editing-basics
  (if hidden-mode-line-mode
      (setq hide-mode-line mode-line-format
            mode-line-format nil)
    (setq mode-line-format hide-mode-line
          hide-mode-line nil))
  (force-mode-line-update)
  ;; apparently force-mode-line-update is not always enough to
  ;; redisplay the mode-line
  (redraw-display)
  (when (and (called-interactively-p 'interactive)
             hidden-mode-line-mode)
    (run-with-idle-timer
     0 nil 'message "Hidden Mode Line Mode enabled.")))

;; if you want to hide the mode-line in every buffer by default
(add-hook 'after-change-major-mode-hook 'hidden-mode-line-mode)

;; custom keybindings activated with C^x t
;; (see http://endlessparentheses.com/the-toggle-map-and-wizardry.html)
(define-prefix-command 'toggle-map)
;; The manual recommends C-c for user keys, but C-x t is
;; always free, whereas C-c t is used by some modes.
(define-key ctl-x-map "t" 'toggle-map)
(define-key toggle-map "v" 'visual-line-mode)
(define-key toggle-map "c" 'column-number-mode)
(define-key toggle-map "l" 'linum-mode)
(define-key toggle-map "h" 'hidden-mode-line-mode)
(define-key toggle-map "s" 'eshell)
(define-key toggle-map "m" 'magit-status)

;; kill only the current buffer
(global-set-key "\C-x\C-k" 'kill-this-buffer)

;; C^n adds new line when at the end of a line
(setq next-line-add-newlines t)

;; set the directory where all backup and autosave files will be saved
(defvar backup-dir "~/tmp/")
(setq backup-directory-alist
      `((".*" . ,backup-dir)))
(setq auto-save-file-name-transforms
      `((".*" ,backup-dir t)))

;; set solarized theme
(add-to-list 'load-path "~/.emacs.d/emacs-color-theme-solarized")
(require 'solarized-dark-theme)
(load-theme 'solarized-dark t)

;; DIRED SETUP
(require 'dired)
(define-key dired-mode-map (kbd "<return>") 'dired-find-alternate-file) ; was dired-advertised-find-file
(define-key dired-mode-map (kbd "^") (lambda () (interactive) (find-alternate-file "..")))  ; was dired-up-directory
(put 'dired-find-alternate-file 'disabled nil)

;;(add-to-list 'load-path "~/.emacs.d/el-get/dired+")
;;(require 'dired+)

;; auto refresh buffers
(global-auto-revert-mode 1)

;; also auto refresh dired, but be quiet about it
(setq global-auto-revert-non-file-buffers t)
(setq auto-revert-verbose nil)

;; PACKAGES SETUP
(require 'package)
;; add the original Emacs Lisp Package Archive
(add-to-list 'package-archives
             '("elpa" . "http://tromey.com/elpa/"))
;; add the user-contributed repository
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/"))
;; add Melpa
(add-to-list 'package-archives
	     '("melpa" . "http://melpa.milkbox.net/packages/") t)

;; packages installed via package-el are activated
;; AFTER .emacs is loaded. So I need to call initialize
;; to be able to use theme.
(package-initialize)

;; use El-Get to sync repos and dependencies.
(add-to-list 'load-path "~/.emacs.d/el-get/el-get")

(unless (require 'el-get nil 'noerror)
  (with-current-buffer
      (url-retrieve-synchronously
       "https://raw.github.com/dimitri/el-get/master/el-get-install.el")
    (let (el-get-master-branch)
      (goto-char (point-max))
      (eval-print-last-sexp))))

;; PO-MODE SETUP
(add-to-list 'load-path "~/.emacs.d/el-get/po-mode")
(require 'po-mode)
(setq auto-mode-alist
      (cons '("\\.po\\'\\|\\.po\\." . po-mode) auto-mode-alist))
(autoload 'po-mode "po-mode" "Major mode for translators to edit PO files" t)

;; SCHEME SETUP
;; associate Scheme with GNUGuile
(setq scheme-program-name "guile")
;; parenthesis and syntax highlighting
(setq show-paren-delay 0
      show-paren-style 'parenthesis)
(show-paren-mode 1)

;; SLIME SETUP
;;get slime to associate with sbcl
;;the path MAY be emacs or emacs24, depending on build
(setq slime-backend "/usr/share/common-lisp/source/slime/swank-loader.lisp")
(add-to-list 'load-path "/usr/share/emacs/site-lisp/slime/")
(setq inferior-lisp-program "/usr/bin/sbcl")
;;(require 'slime)
(load-file "/usr/share/emacs/site-lisp/slime/slime.el")
;;(slime-setup '(slime-fancy))
(slime-setup)

;; PER-FILE SETUP
;; C-specific Indentation
(setq c-default-style "linux"
      c-basic-offset 4)

;; delete trailing whitespaces
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; ORG-MODE SETUP
(add-to-list 'load-path (expand-file-name "~/githubs/org-mode/lisp"))
(add-to-list 'auto-mode-alist '("\\.\\(org\\|org_archive\\|txt\\)$" . org-mode))
(require 'org)

;; Org-mode keys
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-cc" 'org-capture)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)

;; Org-mode log DONE tasks
(setq org-log-done 'time)

;; Org-mode: update parent nodes when child is removed
(defun myorg-update-parent-cookie ()
  (when (equal major-mode 'org-mode)
    (save-excursion
      (ignore-errors
        (org-back-to-heading)
        (org-update-parent-todo-statistics)))))

(defadvice org-kill-line (after fix-cookies activate)
  (myorg-update-parent-cookie))

(defadvice kill-whole-line (after fix-cookies activate)
  (myorg-update-parent-cookie))

;; customized agenda view
(setq org-agenda-custom-commands
            '(("g" "Agenda and giulia-tagged tasks"
               ((agenda "")
                (tags-todo "giulia")
                (tags "giulia")))
              ("m" "Agenda and manuel-tagged tasks"
               ((agenda "")
                (tags-todo "manuel")
                (tags "manuel")))))

;; ORG2BLOG SETUP
(add-to-list 'load-path "~/.emacs.d/el-get/metaweblog")
(add-to-list 'load-path "~/.emacs.d/el-get/xml-rpc-el")
(add-to-list 'load-path "~/.emacs.d/el-get/org2blog")
(add-to-list 'load-path "~/.emacs.d/el-get/htmlize")
(require 'org2blog-autoloads)

(setq org2blog/wp-blog-alist
           '(("informatica.boccaperta.com"

              :url "http://informatica.boccaperta.com/xmlrpc.php"
              :username "manuel")))

(setq org2blog/wp-use-sourcecode-shortcode 't)
(setq org2blog/wp-sourcecode-default-params nil)

;; DOC-VIEW-MODE SETUP
(setq doc-view-continuous t)

;; E-SHELL SETUP
;; clear eshell buffer
;; (see http://www.khngai.com/emacs/eshell.php)
(defun eshell/clear ()
  "Clear the eshell buffer."
  (interactive)
  (let ((inhibit-read-only t))
    (erase-buffer)))

;; MAGIT SETUP
(require 'magit)

;; full screen magit-status
;; see http://whattheemacsd.com/setup-magit.el-01.html
(defadvice magit-status (around magit-fullscreen activate)
  (window-configuration-to-register :magit-fullscreen)
  ad-do-it
  (delete-other-windows))

(defun magit-quit-session ()
  "Restores the previous window configuration and kills the magit buffer"
  (interactive)
  (kill-buffer)
  (jump-to-register :magit-fullscreen))

(define-key magit-status-mode-map (kbd "q") 'magit-quit-session)
