(in-package :next)

;; * Evil mode
;; From https://gist.github.com/lehmacdj/b3c897ab46af34574b2947f8dc4e937d
;; ** Definitions
(defparenstatic scroll-half-page-down
    (ps:chain window (scroll-by 0 (/ (ps:@ window inner-height) 2))))
(defparenstatic scroll-half-page-up
    (ps:chain window (scroll-by 0 (/ (ps:@ window inner-height) -2))))
(defparenstatic scroll-page-down
    (ps:chain window (scroll-by 0 (ps:@ window inner-height))))
(defparenstatic scroll-page-up
    (ps:chain window (scroll-by 0 (- (ps:@ window inner-height)))))

;; evil mode replicates all of the history functionality of document-mode
;; because it overwrites document mode
(defvar *evil-mode-map* (make-hash-table :test 'equalp))

(defclass evil-mode (mode)
  ((active-history-node :accessor active-history-node :initarg :active-node)
   (link-hints :accessor link-hints)))

(defun evil-mode ()
  "Vim-like document-mode replacement"
  (let* ((root (make-node :data "about:blank"))
	 (mode (make-instance 'evil-mode
			      :name "Evil-Mode"
			      :keymap *evil-mode-map*
			      :active-node root)))
    mode))

(defmethod setup ((mode evil-mode) buffer)
  (call-next-method)
  (interface:web-view-set-url-loaded-callback
   (view buffer)
   (lambda () (add-or-traverse-history mode))))

;; insert-mode does almost nothing and should be enabled to interact with
;; web-pages using the keyboard
(defvar *insert-mode-map* (make-hash-table :test 'equalp))

(defclass insert-mode (mode) ())

(defun insert-mode ()
  "Vim-like application mode, with some maps for changing modes back"
  (make-instance 'insert-mode
		 :name "Insert-Mode"
		 :keymap *insert-mode-map*))

;; ** Maps

;; navigation
(define-key *evil-mode-map*
    (kbd "n")
  'scroll-down)
(define-key *evil-mode-map*
    (kbd "p")
  'scroll-up)
(define-key *evil-mode-map*
    (kbd "l")
  'scroll-right)
(define-key *evil-mode-map*
    (kbd "h")
  'scroll-left)
(define-key *evil-mode-map*
    (kbd "G")
  'scroll-to-bottom)
(define-key *evil-mode-map*
    (kbd "g g")
  'scroll-to-top)
(define-key *evil-mode-map*
    (kbd "C-w")
  'scroll-half-page-down)
(define-key *evil-mode-map*
    (kbd "C-u")
  'scroll-half-page-up)

;; zoom
(define-key *evil-mode-map*
    (kbd "=")
  'zoom-in-page)
(define-key *evil-mode-map*
    (kbd "-")
  'zoom-out-page)
(define-key *evil-mode-map*
    (kbd "+")
  'unzoom-page)
(define-key *evil-mode-map*
    (kbd "_")
  'unzoom-page)

;; buffer navigation
(define-key *evil-mode-map*
    (kbd "L")
  'history-forwards)
(define-key *evil-mode-map*
    (kbd "H")
  'history-backwards)
(define-key *evil-mode-map*
    (kbd "N")
  'switch-buffer-next)
(define-key *evil-mode-map*
    (kbd "P")
  'switch-buffer-previous)

(define-key *evil-mode-map*
    (kbd "k")
  'delete-active-buffer)

(define-key *evil-mode-map*
    (kbd "o")
  'set-url-current-buffer)

(define-key *evil-mode-map*
    (kbd "f")
  'go-anchor)

;; (define-key *evil-mode-map*
;; (kbd "B")
;; (:input-complete *minibuffer* switch-buffer buffer-complete))
;; (define-key *evil-mode-map*
;; (kbd "o")
;; (:input-complete *minibuffer* set-url history-typed-complete :empty-complete t))
;; (define-key *evil-mode-map*
;; (kbd "t")
;; (:input-complete *minibuffer* set-url-new-buffer history-typed-complete :empty-complete t))
;; (define-key *evil-mode-map*
;; (kbd "b")
;; (:input-complete *minibuffer* set-url bookmark-complete))

;; link-hints
;; (define-key *evil-mode-map* (kbd "f") (:input *minibuffer* go-anchor :setup #'setup-anchor :cleanup #'remove-link-hints))
;; (define-key *evil-mode-map*
;; (kbd "F")
;; (:input *minibuffer* go-anchor-new-buffer :setup #'setup-anchor :cleanup #'remove-link-hints))
;; (define-key *evil-mode-map*
;; (kbd "g f")
;; (:input *minibuffer* go-anchor-new-buffer-focus :setup #'setup-anchor))

;; mode switching
(define-key *evil-mode-map* (kbd "i")
  (lambda () (add-or-switch-to-mode *active-buffer* (insert-mode))))
(define-key *insert-mode-map* (kbd "C-[")
  (lambda () (add-or-switch-to-mode *active-buffer* (evil-mode))))
(define-key *insert-mode-map* (kbd "ESCAPE")
  (lambda () (add-or-switch-to-mode *active-buffer* (evil-mode))))


;; overwrite the generator for document mode to replace it with evil-mode
(defun document-mode () (evil-mode))
