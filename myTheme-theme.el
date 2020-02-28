(require 'color)
(require 'cl-macs)

(defun my/theme-remove-color ()
  (cl-loop for face in (face-list) do
	   ;; Don't change magit faces
	   (if (and (not (string-match "magit" (symbol-name face))) (not (string-match "w3m" (symbol-name face))))
	       (set-face-attribute face nil :foreground nil :background nil))))

(my/theme-remove-color)

(deftheme myTheme "myTheme")

(defconst my/color-cells (display-color-cells))

(defun my/ifc (a b)
  (if (> my/color-cells 8)
      (eval a)
    (eval b)))

(let* (
       (class '((class color) (min-colors 1)))

       (my/foreground-color (my/ifc "#E6E1DC" "white"))
       (my/foreground-color-1 (my/ifc (color-darken-name my/foreground-color 5) "white"))
       (my/foreground-color-2 (my/ifc (color-darken-name my/foreground-color 10) "white"))
       (my/foreground-color-3 (my/ifc (color-darken-name my/foreground-color 15) "white"))
       (my/foreground-color-4 (my/ifc (color-darken-name my/foreground-color 20) "white"))
       (my/foreground-color-5 (my/ifc (color-darken-name my/foreground-color 25) "white"))
       (my/foreground-color-6 (my/ifc (color-darken-name my/foreground-color 30) "white"))

       ;; "#121212"
       ;; "#212121"
       ;; "#232323"
       (my/background-color (my/ifc (color-darken-name "#292b2e" 10) "black"))
       (my/background-color-1 (my/ifc (color-lighten-name my/background-color 5) "black"))
       (my/background-color-2 (my/ifc (color-lighten-name my/background-color 10) "black"))
       (my/background-color-3 (my/ifc (color-lighten-name my/background-color 15) "black"))
       (my/background-color-4 (my/ifc (color-lighten-name my/background-color 20) "black"))

       (my/prompt-color (my/ifc "purple" "magenta"))

       (my/diff-added-color (my/ifc "#335533" "green"))
       (my/diff-added-hl-color (my/ifc (color-lighten-name "#335533" 20) my/background-color))
       (my/diff-changed-color (my/ifc "#aaaa22" "yellow"))
       (my/diff-changed-hl-color (my/ifc (color-lighten-name "#aaaa22" 20) my/background-color))
       (my/diff-removed-color (my/ifc "#553333" "red"))
       (my/diff-removed-hl-color (my/ifc (color-lighten-name "#553333" 20) my/background-color))
       (my/diff-ancestor-color (my/ifc "#5f06b26ccd93" "blue"))
       (my/diff-ancestor-hl-color (my/ifc (color-lighten-name "#5f06b26ccd93" 20) my/background-color))

       (my/mark-color (my/ifc 'my/diff-changed-color "yellow"))
       (my/mark-color-1 (my/ifc (color-darken-name my/diff-changed-color 5) "yellow"))
       (my/mark-color-2 (my/ifc (color-darken-name my/diff-changed-color 10) "yellow"))
       (my/mark-color-3 (my/ifc (color-darken-name my/diff-changed-color 15) "yellow"))
       (my/mark-color-4 (my/ifc (color-darken-name my/diff-changed-color 20) "yellow"))
       (my/mark-color-5 (my/ifc (color-darken-name my/diff-changed-color 25) "yellow"))
       (my/mark-color-6 (my/ifc (color-darken-name my/diff-changed-color 30) "yellow"))

       ;; "deep sky blue"
       (my/error-color (my/ifc "blue" "blue"))
       (my/warning-color (my/ifc "LightBlue" "LightBlue"))

       (my/spell-error-color "green")
       (my/spell-warning-color "green3")

       ;; :background "#052000"
       ;; #5d4d7a
       (my/mode-line-color (color-darken-name "DeepSkyBlue4" 10))

       (my/hl-line-color "#212026")

       )
  (custom-theme-set-faces
   'myTheme
   `(default ((,class (:foreground ,my/foreground-color :background ,my/background-color))))
   `(link ((,class (:foreground ,my/background-color :background ,my/foreground-color))))
   `(highlight ((,class (:background ,my/mark-color))))
   `(region ((,class (:foreground ,my/foreground-color :background ,my/mark-color))))
   `(error ((,class (:foreground ,my/error-color))))
   `(warning ((,class (:foreground ,my/warning-color))))

   `(flycheck-error ((,class (:underline (:style wave :color ,my/error-color)))))
   `(flymake-error ((,class (:inherit flycheck-error))))
   `(haskell-error-face ((,class (:inherit flycheck-error))))

   `(flycheck-warning ((,class (:underline (:style wave :color ,my/warning-color)))))
   `(flymake-warning ((,class (:inherit flycheck-warning))))
   `(haskell-warning-face ((,class (:inherit flycheck-warning))))

   `(font-lock-comment-face ((,class (:foreground ,(color-lighten-name my/background-color 30)))))
   `(font-lock-comment-delimiter-face ((,class (:foreground ,my/background-color-4 :background ,my/background-color-2))))
   `(font-lock-string-face ((,class (:inherit default))))
   `(font-lock-function-name-face ((,class (:inherit default))))
   `(font-lock-keyword-face ((,class (:inherit default))))
   `(font-lock-doc-face ((,class (:inherit default))))
   `(font-lock-builtin-face ((,class (:inherit default))))
   `(font-lock-constant-face ((,class (:inherit default))))
   `(font-lock-negation-char-face ((,class (:inherit default))))
   `(font-lock-preprocessor-face ((,class (:inherit default))))
   `(font-lock-regexp-grouping-backslash ((,class (:inherit default :weight bold))))
   `(font-lock-regexp-grouping-construct ((,class (:inherit default :weight bold))))
   `(font-lock-type-face ((,class (:inherit default))))
   `(font-lock-variable-name-face ((,class (:inherit default))))
   `(font-lock-warning-face ((,class (:inherit warning))))

   `(hl-line ((,class (:foreground ,my/foreground-color :background ,my/hl-line-color :underline nil))))

   `(outline-1 ((,class (:foreground ,(color-lighten-name my/background-color 2) :background ,(color-darken-name my/foreground-color 50)))))
   `(outline-2 ((,class (:foreground ,(color-lighten-name my/background-color 2) :background ,(color-darken-name my/foreground-color 50)))))
   `(outline-3 ((,class (:foreground ,(color-lighten-name my/background-color 2) :background ,(color-darken-name my/foreground-color 50)))))
   `(outline-4 ((,class (:foreground ,(color-lighten-name my/background-color 2) :background ,(color-darken-name my/foreground-color 50)))))
   `(outline-5 ((,class (:foreground ,(color-lighten-name my/background-color 2) :background ,(color-darken-name my/foreground-color 50)))))
   `(outline-6 ((,class (:foreground ,(color-lighten-name my/background-color 2) :background ,(color-darken-name my/foreground-color 50)))))
   `(outline-7 ((,class (:foreground ,(color-lighten-name my/background-color 2) :background ,(color-darken-name my/foreground-color 50)))))
   `(outline-8 ((,class (:foreground ,(color-lighten-name my/background-color 2) :background ,(color-darken-name my/foreground-color 50)))))

   `(org-level-1 ((,class (:inherit outline-1))))
   `(org-level-2 ((,class (:inherit outline-2))))
   `(org-level-3 ((,class (:inherit outline-3))))
   `(org-level-4 ((,class (:inherit outline-4))))
   `(org-level-5 ((,class (:inherit outline-5))))
   `(org-level-6 ((,class (:inherit outline-6))))
   `(org-level-7 ((,class (:inherit outline-7))))
   `(org-level-8 ((,class (:inherit outline-8))))

   `(header-line ((,class (:foreground ,my/foreground-color :background ,my/mode-line-color))))

   `(my/mode-line-highlight ((,class (:foreground "#063000" :background ,my/foreground-color))))

   `(undo-tree-visualizer-current-face ((,class (:foreground ,my/foreground-color))))
   `(undo-tree-visualizer-register-face ((,class (:foreground ,my/error-color))))
   `(undo-tree-visualizer-unmodified-face ((,class (:foreground "blue"))))
   `(undo-tree-visualizer-default-face ((,class (:foreground ,my/foreground-color-3))))

   `(diff-added ((,class (:background ,my/diff-added-color))))
   `(diff-changed ((,class (:background ,my/diff-changed-color))))
   `(diff-removed ((,class (:background ,my/diff-removed-color))))

   `(diff-refine-added ((,class (:background ,my/diff-added-hl-color))))
   `(diff-refine-changed ((,class (:background ,my/diff-changed-hl-color))))
   `(diff-refine-removed ((,class (:background ,my/diff-removed-hl-color))))

   `(ediff-current-diff-A ((,class (:background ,my/diff-removed-color))))
   `(ediff-current-diff-Ancestor ((,class (:background ,my/diff-ancestor-color))))
   `(ediff-current-diff-B ((,class (:background ,my/diff-added-color))))
   `(ediff-current-diff-C ((,class (:background ,my/diff-changed-color))))
   `(ediff-even-diff-A ((,class (:background ,(color-darken-name my/diff-removed-color 18)))))
   `(ediff-even-diff-Ancestor ((,class (:background ,(color-darken-name my/diff-ancestor-color 30)))))
   `(ediff-even-diff-B ((,class (:background ,(color-darken-name my/diff-added-color 18)))))
   `(ediff-even-diff-C ((,class (:background ,(color-darken-name my/diff-changed-color 18)))))
   `(ediff-fine-diff-A ((,class (:background ,my/diff-removed-hl-color))))
   `(ediff-fine-diff-Ancestor ((,class (:background ,my/diff-ancestor-hl-color))))
   `(ediff-fine-diff-B ((,class (:background ,my/diff-added-hl-color))))
   `(ediff-fine-diff-C ((,class (:background ,my/diff-changed-hl-color))))
   `(ediff-odd-diff-A ((,class (:background ,(color-darken-name my/diff-removed-color 20)))))
   `(ediff-odd-diff-Ancestor ((,class (:background ,(color-darken-name my/diff-ancestor-color 50)))))
   `(ediff-odd-diff-B ((,class (:background ,(color-darken-name my/diff-added-color 20)))))
   `(ediff-odd-diff-C ((,class (:background ,(color-darken-name my/diff-changed-color 20)))))

   `(org-verbatim ((,class (:weight bold))))
   `(org-quote ((,class (:slant italic))))
   `(org-mode-line-clock ((,class (:foreground ,my/foreground-color :background ,my/foreground-color :height unspecified))))
   `(org-mode-line-clock-overrun ((,class (:foreground ,my/foreground-color :background ,my/error-color :height unspecified))))
   `(org-agenda-filter-effort ((,class (:foreground ,my/foreground-color :background ,my/background-color :height unspecified))))
   `(org-agenda-filter-regexp ((,class (:foreground ,my/foreground-color :background ,my/background-color :height unspecified))))
   `(org-agenda-filter-tags ((,class (:foreground ,my/foreground-color :background ,my/background-color :height unspecified))))

   `(org-code ((,class (:background ,my/background-color-3))))
   `(org-block ((,class (:background ,my/background-color-1))))
   `(org-block-begin-line ((,class (:background ,my/background-color-3))))
   `(org-block-end-line ((,class (:inherit org-block-begin-line))))

   `(org-meta-line ((,class (:background unspecified))))
   `(org-table ((,class (:foreground ,my/mark-color))))

   `(org-brain-parent ((,class (:foreground ,my/background-color :background ,my/diff-ancestor-color))))
   `(org-brain-child ((,class (:foreground ,my/background-color :background ,my/diff-added-color))))
   `(org-brain-friend ((,class (:foreground ,my/background-color :background ,my/mark-color))))
   `(org-brain-title ((,class (:inherit outline-1))))

   ;; Used by org src-blocks when in use, might also be used for other things
   `(secondary-selection ((,class (:background ,(color-darken-name my/background-color-1 5)))))

   `(show-paren-match ((,class (:background ,my/foreground-color :foreground ,my/background-color))))
   `(show-paren-match-expression ((,class (:background ,my/foreground-color :foreground ,my/background-color))))
   `(my/show-paren-offscreen-face ((,class (:inherit highlight))))

   `(wgrep-file-face ((,class (:background ,my/foreground-color-6 :foreground ,my/background-color))))

   `(ivy-grep-info ((,class (:background ,my/foreground-color-6 :foreground ,my/background-color))))

   `(symbol-overlay-default-face ((,class (:foreground ,my/foreground-color :background ,my/mark-color-5))))

   `(dired-directory ((,class (:foreground ,my/background-color :background ,my/foreground-color))))
   `(dired-perm-write ((,class (:inherit default))))
   `(dired-symlink ((,class (:inherit font-lock-comment-face))))
   `(dired-header ((,class (:foreground ,my/mark-color))))

   `(spray-accent-face ((,class (:foreground ,my/foreground-color :background ,my/background-color :underline t))))

   `(isearch ((,class (:foreground ,my/background-color :background ,my/foreground-color))))
   `(lazy-highlight ((,class (:foreground ,my/background-color :background ,my/foreground-color))))

   `(haskell-literate-comment-face ((,class (:foreground unspecified :background unspecified :inherit font-lock-comment-face))))

   `(company-scrollbar-bg ((,class (:background ,my/background-color))))
   `(company-scrollbar-fg ((,class (:background ,my/foreground-color))))

   `(company-tooltip-selection ((,class (:background ,my/foreground-color :foreground ,my/background-color))))
   `(company-tooltip ((,class (:foreground ,my/foreground-color :background ,my/background-color-1))))
   `(company-tooltip-common ((,class (:foreground ,my/background-color :background ,my/foreground-color))))

   `(popup-menu-selection-face ((,class (:foreground ,my/background-color :background ,my/foreground-color))))
   `(popup-menu-face ((,class (:foreground ,my/foreground-color :background ,my/background-color-1))))


   `(minibuffer-prompt ((,class (:foreground ,my/mark-color))))

   `(ivy-current-match ((,class (:foreground ,my/background-color :background ,my/mark-color-3))))
   `(ivy-cursor ((,class (:foreground ,my/background-color :background ,my/foreground-color))))
   `(ivy-minibuffer-match-highlight ((,class (:foreground ,my/background-color :background ,my/foreground-color))))

   `(ivy-minibuffer-match-face-1 ((,class (:foreground ,my/background-color :background ,my/foreground-color))))
   `(ivy-minibuffer-match-face-2 ((,class (:foreground ,my/background-color :background ,my/foreground-color-2))))
   `(ivy-minibuffer-match-face-3 ((,class (:foreground ,my/background-color :background ,my/foreground-color-4))))
   `(ivy-minibuffer-match-face-4 ((,class (:foreground ,my/background-color :background ,my/foreground-color-6))))

   `(ivy-minibuffer-match-face-4 ((,class (:foreground ,my/background-color :background ,my/foreground-color-6))))

   `(ivy-yasnippet-key ((,class (nil :foreground :foreground unspecified :background unspecified :inherit font-lock-comment-face))))

   `(my/ivy-rich-doc-face ((,class (:foreground unspecified :background unspecified :inherit font-lock-comment-face))))
   `(my/ivy-rich-switch-buffer-indicator-face ((,class (:foreground unspecified :background unspecified :inherit font-lock-comment-face))))
   `(my/ivy-rich-switch-buffer-major-mode-face ((,class (:foreground unspecified :background unspecified :inherit font-lock-comment-face))))
   `(my/ivy-rich-switch-buffer-size-face ((,class (:foreground unspecified :background unspecified :inherit font-lock-comment-face))))
   `(my/ivy-rich-switch-buffer-path-face ((,class (:foreground unspecified :background unspecified :inherit font-lock-comment-face))))
   `(my/ivy-rich-switch-buffer-project-face ((,class (:foreground unspecified :background unspecified :inherit font-lock-comment-face))))
   `(my/ivy-rich-find-file-symlink-face ((,class (:foreground unspecified :background unspecified :inherit font-lock-comment-face))))

   `(swiper-match-face-1 ((,class (:foreground ,my/background-color :background ,my/foreground-color))))
   `(swiper-match-face-2 ((,class (:foreground ,my/background-color :background ,my/foreground-color-2))))
   `(swiper-match-face-3 ((,class (:foreground ,my/background-color :background ,my/foreground-color-4))))
   `(swiper-match-face-4 ((,class (:foreground ,my/background-color :background ,my/foreground-color-6))))

   `(avy-lead-face ((,class (:foreground ,my/background-color :background ,my/foreground-color-6))))
   `(avy-lead-face-0 ((,class (:foreground ,my/background-color :background ,my/foreground-color-2))))
   `(avy-lead-face-1 ((,class (:foreground ,my/background-color :background ,my/foreground-color-4))))
   `(avy-lead-face-2 ((,class (:foreground ,my/background-color :background ,my/foreground-color-6))))

   `(eshell-prompt ((,class (:foreground ,my/prompt-color))))

   `(eshell-prompt ((,class (:foreground ,my/prompt-color))))

   `(yascroll:thumb-fringe ((,class (:background "slateblue" :foreground "slateblue"))))
   `(yascroll:thumb-text-area ((,class (:background "slateblue"))))

   `(term-color-black ((,class (:foreground "black" :background "black"))))
   `(term-color-blue ((,class (:foreground "blue" :background "blue"))))
   `(term-color-cyan ((,class (:foreground "cyan" :background "cyan"))))
   `(term-color-green ((,class (:foreground "green" :background "green"))))
   `(term-color-magenta ((,class (:foreground "magenta" :background "magenta"))))
   `(term-color-red ((,class (:foreground "red" :background "red"))))
   `(term-color-white ((,class (:foreground "white" :background "white"))))
   `(term-color-yellow ((,class (:foreground "yellow" :background "yellow"))))

   `(flyspell-incorrect ((,class (:underline (:style wave :color ,my/spell-error-color)))))
   `(flyspell-duplicate ((,class (:underline (:style wave :color ,my/spell-warning-color)))))

   `(litable-result-face ((,class (:foreground ,my/foreground-color :background ,my/background-color :weight 'bold))))
   `(litable-substitution-face ((,class (:foreground ,my/foreground-color :background ,my/background-color :weight 'bold))))

   `(diff-hl-change ((,class (:background ,my/diff-changed-color))))

   `(show-paren-match ((,class (:foreground ,my/background-color :background ,my/foreground-color))))
   `(show-paren-mismatch ((,class (:background ,my/error-color))))

   `(flycheck-posframe-background-face ((,class (:foreground ,my/foreground-color :background "#000000"))))

   `(lsp-ui-doc-background ((,class (:inherit default))))

   `(lsp-ui-doc-header ((,class (:foreground ,my/foreground-color :background ,my/background-color-4))))
   `(lsp-ui-doc-url ((,class (:foreground ,my/background-color :background ,my/foreground-color))))

   `(lsp-ui-peek-filename ((,class (:inherit default))))
   `(lsp-ui-peek-footer ((,class (:inherit default))))
   `(lsp-ui-peek-header ((,class (:inherit default))))
   `(lsp-ui-peek-highlight ((,class (:inherit default))))
   `(lsp-ui-peek-line-number ((,class (:inherit default))))
   `(lsp-ui-peek-list ((,class (:inherit default))))
   `(lsp-ui-peek-peek ((,class (:inherit default))))
   `(lsp-ui-peek-selection ((,class (:inherit default))))
   `(lsp-ui-sideline-current-symbol ((,class (:inherit default))))
   `(lsp-ui-sideline-code-action ((,class (:inherit default))))
   `(lsp-ui-sideline-global ((,class (:inherit default))))

   `(lsp-ui-sideline-symbol ((,class (:foreground nil :background nil))))
   `(lsp-ui-sideline-symbol-info ((,class (:foreground ,my/mark-color :background ,my/background-color))))

   `(lsp-lens-face ((,class (:foreground unspecified :background ,my/background-color :inherit font-lock-comment-face))))

   ))

(provide-theme 'myTheme)
