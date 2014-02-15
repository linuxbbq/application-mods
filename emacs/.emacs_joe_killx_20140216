;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;          Custom killx .emacs
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; Basic no-X killx options
(menu-bar-mode 0)
(winner-mode 1)
(iswitchb-mode 1)

;; C Programming options
(setq c-default-style "linux"
      c-basic-offset 4)

;; Joe's Quick Code Folder
(defun joe-toggle-selective-display ()
 ;; Quick code folding ;;
  (interactive)
  (set-selective-display (if selective-display nil 1)))
(global-set-key [f1] 'joe-toggle-selective-display)

;;Allow iswitchb to dmenu switch
(defun iswitchb-local-keys ()
  (mapc (lambda (K)
	  (let* ((key (car K)) (fun (cdr K)))
	    (define-key iswitchb-mode-map (edmacro-parse-keys key) fun)))
	'(("<right>"  . iswitchb-next-match)
	  ("<left>"   . iswitchb-prev-match)
	  ("<up>"     . ignore)
	  ("<down>"   . ignore))))
(add-hook 'iswitchb-define-mode-map-hook 'iswitchb-local-keys)
(require 'edmacro)
