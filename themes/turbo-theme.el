(deftheme turbo "turbo")

(let ((colors '(
		(fg . "#efeff9")

		;; Black
		;; (bg . "#000000")

		;; Blue
		(bg . "#0b0c17")

		;; (bg . "#110811dc131b")

		(prompt . "purple")
		(diff-add . "#0000b6b50000")
		(diff-add . "#335533")
		(diff-change . "#ffff50")
		(diff-remove . "#af0000")
		(diff-ancestor . "#5f06b26ccd93")
		(mark . "#00a8af")
		(error . "blue")
		(warning . "LightBlue")
		(info . "CadetBlue")
		(spell-error . "green")
		(spell-warning . "green3")
		(outline-1 . "#4f97d7")
		(outline-1-bg . "#293239")
		(outline-2 . "#2d9574")
		(outline-2-bg . "#293235")
		(outline-3 . "#67b11d")
		(outline-3-bg . "#293235")
		(outline-4 . "#b1951d")
		(outline-4-bg . "#32322c")
		(scrollbar . "light sea green")
		(link . "#67b11d")
		;; Either t or nil. If t then use background color as foreground color
		(hover-overlay-invert-fg . nil)
		(hover-overlay-bg . "#7c7c00000000")
		)
	      )
      (theme 'turbo))
  
  (my/create-theme theme colors t)
  (provide-theme theme))
