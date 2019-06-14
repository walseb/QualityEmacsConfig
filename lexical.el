;;; -*- lexical-binding: t -*-
(remove-hook 'post-self-insert-hook
	     #'blink-paren-post-self-insert-function)
(setq blink-matching-paren 'show)

;;(defvar my/show-paren-no-reset nil)
;;
;;(defun my/show-paren-move-right-setup ()
;;  ;; if point at evil end of line
;;  (setq evil-move-beyond-eol nil)
;;  (if (eolp)
;;      (setq my/show-paren-cursor-need-reset t)
;;    (forward-char 1)))
;;
;;(defun my/show-paren-move-left-reset ()
;;  (setq evil-move-beyond-eol nil)
;;  (when (not my/show-paren-no-reset)
;;  (forward-char 1)))

(defun blink-matching-open ()
  "Momentarily highlight the beginning of the sexp before point."
  (interactive)
  (when (and (not (bobp))
	     blink-matching-paren)
    (let* ((oldpos (point))
	   (message-log-max nil) ; Don't log messages about paren matching.
	   (blinkpos
	    (save-excursion
	      (save-restriction
		(if blink-matching-paren-distance
		    (narrow-to-region
		     (max (minibuffer-prompt-end) ;(point-min) unless minibuf.
			  (- (point) blink-matching-paren-distance))
		     oldpos))
		(let ((parse-sexp-ignore-comments
		       (and parse-sexp-ignore-comments
			    (not blink-matching-paren-dont-ignore-comments))))
		  (condition-case ()
		      (progn
			(syntax-propertize (point))
			;; CHANGE - makes it so that it displays the paren your cursor is over
			(backward-up-list)
			;; backward-sexp skips backward over prefix chars,
			;; so move back to the matching paren.
			(while (and (< (point) (1- oldpos))
				    (let ((code (syntax-after (point))))
				      (or (eq (syntax-class code) 6)
					  (eq (logand 1048576 (car code))
					      1048576))))
			  (forward-char 1))
			(point))
		    (error nil))))))
	   (mismatch (funcall blink-matching-check-function blinkpos oldpos)))
      (cond
       (mismatch
	(if blinkpos
	    (if (minibufferp)
		(minibuffer-message "Mismatched parentheses")
	      (message "Mismatched parentheses"))
	  (if (minibufferp)
	      (minibuffer-message "No matching parenthesis found")
	    (message "No matching parenthesis found"))))
       ((not blinkpos) nil)
       ((or
	 (eq blink-matching-paren 'jump-offscreen)
	 (pos-visible-in-window-p blinkpos))
	;; Matching open within window, temporarily move to or highlight
	;; char after blinkpos but only if `blink-matching-paren-on-screen'
	;; is non-nil.
	(and blink-matching-paren-on-screen
	     (not show-paren-mode)
	     (if (memq blink-matching-paren '(jump jump-offscreen))
		 (save-excursion
		   (goto-char blinkpos)
		   (sit-for blink-matching-delay))
	       (unwind-protect
		   (progn
		     (move-overlay blink-matching--overlay blinkpos (1+ blinkpos)
				   (current-buffer))
		     (sit-for blink-matching-delay))
		 (delete-overlay blink-matching--overlay)))))
       (t
	(let ((open-paren-line-string
	       (save-excursion
		 (goto-char blinkpos)
		 ;; Show what precedes the open in its line, if anything.
		 (cond
		  ((save-excursion (skip-chars-backward " \t") (not (bolp)))
		   (buffer-substring (line-beginning-position)
				     (1+ blinkpos)))
		  ;; Show what follows the open in its line, if anything.
		  ((save-excursion
		     (forward-char 1)
		     (skip-chars-forward " \t")
		     (not (eolp)))
		   (buffer-substring blinkpos
				     (line-end-position)))
		  ;; Otherwise show the previous nonblank line,
		  ;; if there is one.
		  ((save-excursion (skip-chars-backward "\n \t") (not (bobp)))
		   (concat
		    (buffer-substring (progn
					(skip-chars-backward "\n \t")
					(line-beginning-position))
				      (progn (end-of-line)
					     (skip-chars-backward " \t")
					     (point)))
		    ;; Replace the newline and other whitespace with `...'.
		    "..."
		    (buffer-substring blinkpos (1+ blinkpos))))
		  ;; There is nothing to show except the char itself.
		  (t (buffer-substring blinkpos (1+ blinkpos)))))))
	  (minibuffer-message
	   "%s"
	   (substring-no-properties open-paren-line-string))))))))

(let ((ov nil)) ; keep track of the overlay
  (advice-add
   #'show-paren-function
   :after
   (defun show-paren--off-screen+ (&rest _args)
     "Display matching line for off-screen paren."
     (when (overlayp ov)
       (delete-overlay ov))
     ;; check if it's appropriate to show match info,
     ;; see `blink-paren-post-self-insert-function'
     (when (and (overlay-buffer show-paren--overlay)
		(not (or cursor-in-echo-area
			 executing-kbd-macro
			 noninteractive
			 (minibufferp)
			 this-command))
		(and (not (bobp))
		     (memq (char-syntax (char-before)) '(?\) ?\$)))
		(= 1 (logand 1 (- (point)
				  (save-excursion
				    (forward-char -1)
				    (skip-syntax-backward "/\\")
				    (point))))))
       ;; rebind `minibuffer-message' called by
       ;; `blink-matching-open' to handle the overlay display
       (cl-letf (((symbol-function #'minibuffer-message)
		  (lambda (msg &rest args)
		    (let ((msg (apply #'format-message msg args)))
		      (setq ov (display-line-overlay+
				(window-start) msg 'show-paren-offscreen-face))))))
	 (blink-matching-open))))))

(defun display-line-overlay+ (pos str &optional face)
  "Display line at POS as STR with FACE.

FACE defaults to inheriting from default and highlight."
  (let ((ol (save-excursion
	      (goto-char pos)
	      (make-overlay (line-beginning-position)
			    (line-end-position)))))
    (overlay-put ol 'display str)
    (overlay-put ol 'face
		 (or face '(:inherit default :inherit highlight)))
    ol))
