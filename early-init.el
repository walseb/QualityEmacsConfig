;; -*- lexical-binding:t -*-
;; * Empty out mode-maps
;; Mode-maps needs to be emptied early since some packages might require them
(setq compilation-mode-map (make-sparse-keymap)
      compilation-minor-mode-map (make-sparse-keymap)
      compilation-shell-minor-mode-map (make-sparse-keymap)
      compilation-mode-tool-bar-map (make-sparse-keymap)

      exwm-mode-map (make-sparse-keymap)
      help-mode-map (make-sparse-keymap)
      ibuffer-mode-map (make-sparse-keymap)
      evil-mc-key-map (make-sparse-keymap)
      Man-mode-map (make-sparse-keymap)
      calendar-mode-map (make-sparse-keymap)
      org-agenda-mode-map (make-sparse-keymap)
      org-brain-visualize-mode-map (make-sparse-keymap)
      outshine-mode-map (make-sparse-keymap)
      ivy-minibuffer-map (make-sparse-keymap)
      company-active-map (make-sparse-keymap)
      flycheck-mode-map (make-sparse-keymap)
      yas-minor-mode-map (make-sparse-keymap)
      winner-mode-map (make-sparse-keymap)
      dired-mode-map (make-sparse-keymap)
      dap-mode-map (make-sparse-keymap)
      racket-repl-mode-map (make-sparse-keymap)
      csharp-mode-map (make-sparse-keymap)
      shr-map (make-sparse-keymap)
      eww-mode-map (make-sparse-keymap)
      grep-mode-map (make-sparse-keymap)
      wgrep-mode-map (make-sparse-keymap)
      occur-mode-map (make-sparse-keymap)
      occur-edit-mode-map (make-sparse-keymap)
      ivy-occur-mode-map (make-sparse-keymap)
      flyspell-mode-map (make-sparse-keymap)
      picture-mode-map (make-sparse-keymap)
      artist-mode-map (make-sparse-keymap)
      pdf-view-mode-map (make-sparse-keymap)
      eimp-minor-mode-map (make-sparse-keymap)
      image-mode-map (make-sparse-keymap)
      olivetti-mode-map (make-sparse-keymap)
      undo-tree-visualizer-mode-map (make-sparse-keymap))

;; * Ignore x-resources
(advice-add #'x-apply-session-resources :override #'ignore)

;; * GC
(setq garbage-collection-messages t)
(setq my/gc-mem gc-cons-threshold)
(setq gc-cons-threshold most-positive-fixnum)

;; * Disable gui
;; Prevent the glimpse of un-styled Emacs by disabling these UI elements early.
(push '(menu-bar-lines . nil) default-frame-alist)
(push '(tool-bar-lines . nil) default-frame-alist)
(push '(vertical-scroll-bars . nil) default-frame-alist)
(push '(horizontal-scroll-bar . nil) default-frame-alist)

;; (menu-bar-mode -1)
;; (tool-bar-mode -1)
;; (scroll-bar-mode -1)

;; * Disable frame resize on font change
(setq frame-inhibit-implied-resize t)

;; * Stop custom from writing to init.el
;; Also stop it from being loaded at all
(setq custom-file (concat user-emacs-directory ".emacs-custom.el"))

;; * Disable mode-line
(setq mode-line-format nil)
(setq-default mode-line-format nil)
