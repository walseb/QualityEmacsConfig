(deftheme myTheme "myTheme")

(let ((colors '((fg . "#E6E1DC")
		;; "#121212"
		;; "#212121"
		;; "#232323"
		;; (my/bg-color (my/ifc (color-darken-name "#292b2e" 10) "black"))
		;; Gruvbox
		;; (my/bg-color "#32302f")
		;; (color-darken-name "#292b2e" 10)
		(bg . "#110811dc131b")
		(prompt . "purple")
		(diff-add . "#335533")
		(diff-change . "#aaaa22")
		(diff-remove . "#553333")
		(diff-ancestor . "#5f06b26ccd93")
		(mark . "#aaaa22")
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
		(link . "#3fff3fff0ccc")
		;; Either t or nil. If t then use background color as foreground color
		(hover-overlay-invert-fg . nil)
		(hover-overlay-bg . "#3fff3fff0ccc")
		))
      (theme 'myTheme))

  (my/create-theme theme colors)
  (provide-theme theme))
