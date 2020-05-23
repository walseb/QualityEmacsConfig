;; -*- lexical-binding:t -*-
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
