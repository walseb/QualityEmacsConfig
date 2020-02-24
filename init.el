;; -*- lexical-binding:t -*-
(load-theme 'myTheme t)

(let ((byte-compile-warnings nil))
  (byte-recompile-file (concat user-emacs-directory "config.el") nil 0 t))
