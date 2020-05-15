;; -*- lexical-binding:t -*-
(when (string< emacs-version "27")
  (load-file (concat user-emacs-directory "early-init.el")))

(load-theme 'myTheme t)

(let ((byte-compile-warnings nil))
  (byte-recompile-file (concat user-emacs-directory "config.el") nil 0 t))
(put 'list-timers 'disabled nil)
