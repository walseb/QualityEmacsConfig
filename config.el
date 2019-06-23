;; -*- lexical-binding:t -*-
;; * Docs
;; ** Requirements
;; *** All
;; =xorg-server xorg-xrandr xorg-xinit xorg-setxkbmap mlocate atool unzip mono pulseaudio pavucontrol firefox the_silver_searcher imagemagick ttf-inconsolata ttf-opensans ttf-dejavu aspell aspell-en aspell-sv mpd mpc poppler poppler-glib=
;; *** All optional
;; =msbuild dotnet-sdk godot-mono guile fsharp ghc ghc-static=

;; *** UTF-8 support
;; =ttf-dejavu= fills in the gaps where opensans doesn't have the character

;; *** Music
;; =mpd= =mpc=
;; =pulse audio= if alsa, etc, edit "MPD/Create config".

;; *** Search
;; =the silver searcher/ ag=
;; =grep=

;; *** Code
;; **** Documentation
;; =zeal=

;; **** C#
;; =mono=
;; =M-x omnisharp-install-server=

;; **** F#
;; =mono= (3.10.X or higher) =F#= (3.0 or higher)
;; To create solution file, check github
;; On windows, you may need to manually point to server path, see github

;; **** Clojure
;; =leiningen= =clojure=

;; *** Screenshots
;; =imagemagick=

;; *** Overtone
;; =jack2= =leiningen= =cider=

;; To start =leiningen= in emacs do =cider-jack-in=

;; Input =(use 'overtone.live)= into leiningen to install

;; **** To use with pulse audio
;; =cadence= and =pulseaudio-jack=

;; *** Spelling
;; =aspell-en= etc

;; *** Gnus
;; Set adress in gnus.el

;; *** Laptop
;; =xorg-xbacklight=
;; =wpa_supplicant=

;; *** Switch Ctrl and Caps
;; =xmodmap= =xsession=
;;  functions
;; *** PDF tools
;; =poppler= =poppler-glib=
;; M-x =pdf-tools-install=

;; *** GPG pinentry
;; =gpg2= =pinentry=

;; *** Misc
;; =redshift=

;; ** Firefox vimium
;; *** Config
;; #+begin_src
;; # Rebind up/down
;; unbind n
;; unbind N
;; map n scrollDown
;; map N previousTab
;; unbind p
;; unbind P
;; map p scrollUp
;; map P nextTab

;; unbind k
;; unbind K
;; map k openCopiedUrlInCurrentTab
;; map K openCopiedUrlInNewTab

;; # Rebind scroll
;; unbind <c-u>
;; unbind <c-d>
;; map <c-u> scrollPageUp
;; map <c-d> scrollPageDown

;; # Rebind tab close
;; unbind s
;; map s removeTab
;; #+end_src

;; *** Hint characters
;; #+begin_src
;; anetoshdirgmlwyfubxcvkp,.q;j/z
;; #+end_src

;; ** FSharp
;; *** File is not part of the loaded projects
;; Try having the =fsproj= file open in a buffer or try moving the project folder directly to the home folder

;; ** Overtone
;; *** Pulse-jack
;; **** How to configure jack2 to work with pulse
;; https://wiki.archlinux.org/index.php/PulseAudio/Examples#PulseAudio_through_JACK
;; (The KXStudio method)

;; 1. get =cadence= and =pulseaudio-jack=

;; 2. Bridge alsa -> pulse -> jack

;; 3. Autostart maybe???

;; 4. Configure -> Alsa -> Good settings

;; **** Pauvcontrol measures sound but can't hear anything
;; Unmute the sound device in pauvcontrol

;; **** How should jack be started???
;; Let overtone (actually it's supercollider, since overtone is just a wrapper for using supercollider in clojure) do it (it does it automatically when inputting =(use 'overtone.live)= in leiningen)

;; *** Jack only
;; 1. add =autospawn = no= in =~/.config/pulse/client.conf=
;; 2. kill pulseaudio with =pulseaudio -k=
;; 3. get =jack2= (NOT jack2-dbus, does't work)
;; 4. start overtone

;; **** Still doesn't work
;; Get =qjackctl= and start =jack2= from there

;; *** Sounds only play in one ear??
;; https://github.com/overtone/overtone/wiki/Multi-channel-expansion,-stereo-and-panning
;; You have to specify sound channel in the code when using certain sounds

;; *** Overtone errors out all the time
;; Execute script up to down, put =(use 'overtone.live)= on top of file, and in leiningen

;; *** Shit doesn't work
;; just fiddle around with cadence (check if bridge type is correct, then force restart until it works)

;; ** Dired
;; *** Archives in dired
;; Press c to compress, Z to extract
;; To stop dired from creating new folder when unpacking, change in section "Dired atool"

;; ** Can't find package error
;; run
;; =package-refresh-contents=
;; or restart emacs
;; ** Gnus
;; *** How to setup name and password
;; Create authinfo.pgp file. It is auto encrypted/decrypted

;; Format for gmail is currently
;; #+begin_src
;; machine imap.gmail.com login <USER> password <APP-PASSWORD> port imaps
;; machine smtp.gmail.com login <USER> password <APP-PASSWORD> port 587
;; machine imap-mail.outlook.com login my-username@hotmail.com password my-secret-password port 993
;; #+end_src

;; *** gnus mails are not updating
;; Try doing C-u M-g twice inside that inbox

;; *** Where are my servers/passwords stored?
;; =~/.authinfo.gpg=

;; *** How to download articles using gnus?
;; use =M-x gnus-agent-add-server=

;; *** Mail server mails aren't marked as read when marking as read in gnus, and gnus isn't marking them as read either after exit
;; You have to press =q= in order to save changes

;; ** wpa supplicant
;; https://wiki.archlinux.org/index.php/WPA_supplicant#Connecting_with_wpa_cli

;; ** MPD
;; *** "no mpd daemon running"
;; Disable daemon if using systemctl with =systemctl disable mpd.service mpd.socket=

;; ** Eww
;; *** Opening local file results in raw page
;; This is because the file isn't named =FILE.html=, when eww saves pages, it doesn't add =.html= at the end

;; ** Keyboard setup
;; *** Change keyboard layout
;; To list keymaps, do =localectl list-keymaps=

;; =carpalx= is example layout
;; To load keymaps, in terminal do: =loadkeys carpalx=

;; To make permanent:
;; in =/etc/vconsole.conf=
;; #+begin_src
;; KEYMAP=carpalx
;; FONT=lat9w-16
;; #+end_src

;; *** Swap Ctrl and Caps_Lock
;; Load correct keymap
;; 1. Do =sudo dumpkeys | head -1 > ~/Keys.map=
;; 2. Add this under the one line long Keys.map
;; #+begin_src maps
;; keycode 58 = Control # Makes Caps Lock act as ctrl
;; keycode 29 = Caps_Lock # Makes ctrl act as caps
;; # alt_is_meta # Fix the alt key?
;; #+end_src
;; 3. Do =sudo loadkeys ~/Keys.map=

;; ** Color picking
;; Get =gpick=

;; ** C libraries not imported
;; Add a .ccls file and format it like this
;; #+begin_src
;;   g++
;;   -lstdc++
;;   -I/usr/include/SDL2
;; #+end_src
;; (can also use clang++, etc)

;; *** Other reason
;; It could be that LSP doesn't tell CCLS the correct workspace
;; To fix this do =M-x lsp-workspace-folders-remove= then select what you think is the current workspace then =M-x lsp-workspace-folders-add= and select the actual root (the file with a =.ccls= file in it)

;; ** GDB doesn't work properly
;; make sure you compiled with the =-g= flag

;; ** Compatibility
;; *** Mesa 3d software rendering
;; Makes godot work with old computers
;; #+begin_src command
;; LIBGL_ALWAYS_SOFTWARE=1 godot-mono
;; #+end_src

;; ** Sharing folders via virtualbox
;; https://wiki.archlinux.org/index.php/VirtualBox#Enable_shared_folders
;; 1.
;; Devices -> Insert guest additions CD images

;; 2.
;; On guest if arch install =virtualbox-guest-utils=

;; 3.
;; Run
;; #+begin_src bash
;; sudo mount -t vboxsf -o uid=1000,gid=1000 SHARED_FOLDER_NAME MOUNT_DIR
;; #+end_src
;; 1000 in the command is fetched from running =id=

;; ** Omnisharp
;; ALWAYS check *omnisharp-log* for errors
;; Try building the program atleast once first before trying any of this, it could just fix the problem

;; *** Errors everywhere
;; Probably missing system.dll, etc reference
;; Could be that the references in your csproj are tailored to windows, etc

;; *** Only basic errors
;; Check this https://github.com/OmniSharp/omnisharp-emacs/issues/459
;; Otherwise it's probably because there are errors in *omnisharp-log*

;; *** Errors everywhere because of missing references
;; Check your csproj file
;; Remember that wildstars probably aren't supported in omnisharp! Add every script manually via counsel-locate, macros or whatever
;; Not like this
;; <Compile Include="*.cs" />
;; Like this
;; <Compile Include="Assets/Script.cs" />

;; ** Magit
;; *** Rename commit
;; magit replace (r) in log buffer -> w for reword

;; ** Regexps
;; Make regexps easier by using (rx)
;; E.g.
;; #+begin_src
;;   (rx bol "*.$" space)
;; #+end_src
;; Where =bol= is beginning of line, and =space= is anything that has whitespace syntax
;; For more symbols read =rx= help docs, it has everything

;; ** WSL
;; *** When typing citation mark an @ is pasted using X11 passthrough
;; X11 probably uses UK language layout. Fix it with
;; #+begin_src shell
;;   setxkbmap us
;; #+end_src

;; * Todo
;; ** Packages to try
;; nix-buffer

;; ** Fix s key moving around the view when searching down
;; ** PR evil-mc changes
;; ** Delete around char 'c'
;; Need to find how to use the "inside" operator, etc
;; #+begin_src
;;   (evil-define-motion evil-find-char (char)
;;     (interactive "<C>")
;;     (evil-find-char 1 char)
;;     )
;; #+end_src

;; ** Firefox
;; *** Link-hint
;; There was a great and fast plugin for this somewhere

;; *** Always create window instead of tab
;; ** Bookmarks
;; https://www.reddit.com/r/emacs/comments/9bly3d/linkmarksel_use_orgmode_links_for_bookmarks/
;; https://www.emacswiki.org/emacs/BookMarks

;; ** Annotations
;; https://github.com/bastibe/annotate.el

;; ** Find out what is taking so long when opening config
;; Does it happen in vanilla?
;; Use error on quit, quit when loading is happening then get backtrace.
;; ** Make magit-status faster during huge edits, or create new magit-status-fast command
;; ** Should save-window-excursion be disabled?
;; Steps to reproduce: open two window split, do M-x, close the window you focused when doing M-x, cancel the M-x with C-g

;; ** GPG doesn't remember last password when saving
;; ** Automate gnus
;; *** Notmuch gnus integration
;; *** Dovecot docker process
;; Add to and configure in nixos config

;; ** Use lexical bindings in config
;; ** Add more haskell tools
;; HIE, etc

;; ** Better folding binds
;; ** Add is wsl/VM in config
;; This will disable volume controls for example

;; ** Locate
;; *** Locate should cut out the default directory from the prompt
;; *** Lisp only locate
;; Look into changing from locate to using "directory-files-recursively" to cache all files on the pc
;; Problem seems to be that it needs sudo to do this

;; *** Use find instead
;; You can have a lisp list with all the dirs that you want scanned in a variable
;; #+begin_src example
;; '(
;;   ("~" '("file1" "file2"))

;;   ("/mnt/c" '("file3" "file4")))
;; #+end_src

;; When function first runs, find gets everything in directories and caches that in emacs to variable
;; If you want you can dump those variables to disk

;; ** Fix ivy grep/occur
;; Colors change when you put your cursor over custom faces

;; ** Fix change defalut directory to change save dir
;; ** Customize ivy more

;; ** % should go to closest paren if not on one
;; ** Dedicated auto comment key?
;; ** g-n g-p highlighting stays
;; ** Pressing e in dired insert mode should kill the older buffer
;; ** Make macros faster
;; Temporarily disable "global-hl-line-mode" while running macro (takes like 70% cpu in worst cases)
;; Disable symbol-overlay while in macro (takes little cpu, but you can still gain speed)

;; ** C-h in ivy-find-file
;; ** When hiding comments make comment face same as background
;; Seems like the comment background color is still left when hiding comments

;; ** Should org mode indent source blocks by 2 spaces?
;; Checkout heading: Disable code block indent

;; ** Read large files package
;; There is one for dired too

;; ** Refractor config
;; Maybe split up because of bad performance?
;; Should probably create some sort of guideline for where to write down prefix keys, where to write visual headers
;; I should probably not just have a header named just "visuals"

;; ** Track down more performance problems
;; Especially when using highlight-indent-guides on large files

;; ** A quick spellchecker in comments and org-mode

;; ** Fix low-res nix heading fonts
;; No idea why they are low res

;; ** Org-noter
;; Great for commenting pdfs

;; ** Org-capture
;; Great for referencing to source code

;; ** lsp-mode in org src-buffers
;; Read
;; https://github.com/emacs-lsp/lsp-mode/issues/377

;; ** M-n and M-p to move visual line
;; ** Fix swiper in man mode
;; ** Easier way of accesing nix docs
;; man 5 configuration.nix
;; ??
;; man configuration.nix

;; ** Bake xdefaults into nix config

;; ** Improve nix config
;; https://github.com/magnetophon/nixosConfig/blob/master/common.nix

;; ** Fix lsp-ui
;; eldoc-doesn't work so i have to use  lsp-ui-sideline-show-hover

;; ** Overhaul imenu
;; imenu-anywhere
;; imenu-list

;; ** I shouldn't have one function that modifies all faces
;; Since then i have to load every package i want to modify the faces of before running the function

;; ** Bind recenter screen to singe key in normal mode
;; ** Structured-haskell-mode
;; ** Navigate headers like parens
;; ** Make locate work on all harddrives
;; Save the database on the main c drive under name of the drive though

;; ** Eshell status in modeline
;; Checkout
;; (setq-default eshell-status-in-mode-line nil)

;; Then you can have a number like how many processes are running

;; ** C-k in dired should go back dir

;; ** Indicator in modeline for when loccur is narrowing the buffer

;; ** Flycheck posframe should be in top right
;; Atleast not at point

;; ** Add heading face to my/theme function
;; Make them bold?
;; Maybe different background color

;; ** Maybe remove bold font from ivy match faces
;; I think bold fonts are slightly higher than normal fonts, which I think causes the minibuffer to not match ivy fully sometimes

;; ** Make org-indent work with outlines
;; 1. (setq org-outline-regexp "^;;\s\\*+")
;; 2. Modify =org-indent--compute-prefixes= so that =org-indent--heading-line-prefixes= and maybe other variables are correct

;; ** Fix config compile errors

;; ** Compile before loading config for faster start times after modifying config
;; This gives me an error which seems to be related to straight.el where it can't require basic libraries
;; It might be related to the lexical bindings?
;; Right now I put a compilation step at the end of the config

;; ** clone-indirect-buffer shouldn't make the new buffer appear in a different window than the selected one

;; ** Make use of global mode map
;; evil-universal-define-key overwrites the evil mode map, this should use global-mode-map instead


;; ** Fix keys
;; Exwm keys are really messy, remove 'my/keys-mode-map'
;; *** Clean global mode map
;; It's currently full of unused keys
;; *** Maybe only use evil-edit instead

;; * First
;; Things to do first
(setq mode-line-format nil)
(setq-default mode-line-format nil)

;; * Security
(setq network-security-level 'high)

;; ** Cert settings
(setq gnutls-verify-error t)
(setq tls-checktrust t)

;; ** Make authinfo gpg file
(setq netrc-file "~/.authinfo.gpg")
(setq auth-sources '("~/.authinfo.gpg"))

;; * Package management
;; Bootstrap straight.el
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
	(url-retrieve-synchronously
	 "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
	 'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; * Private config
(defun my/load-if-exists (f)
  "load the elisp file only if it exists and is readable"
  (if (file-readable-p f)
      (load-file f)))

;; ** Private config
(my/load-if-exists (concat user-emacs-directory "private.el"))

;; ** Device config
;; If a device config is not made, load the default one
(if (not (my/load-if-exists (concat user-emacs-directory "device.el")))
    (load-file (concat user-emacs-directory "device-template.el")))

;; * Libraries
(straight-use-package 's)
(straight-use-package 'dash)
(straight-use-package 'ov)
(require 's)
(require 'dash)

;; ** Elpatch
(straight-use-package 'el-patch)

;; * Persistent keys
(defvar my/keys-mode-map (make-sparse-keymap))

;; Emacs 27 doesn't support :init-value, :keymap, etc
;; (if (string< emacs-version "27")
;; (define-minor-mode my/keys-mode
;; ;; init value t to enable it in fundamental mode
;; ;; More info: http://emacs.stackexchange.com/q/16693/115
;; :init-value t
;; :keymap my/keys-mode-map)
(define-minor-mode my/keys-mode nil t nil my/keys-mode-map)

(add-to-list 'emulation-mode-map-alists `((my/keys-mode . ,my/keys-mode-map)))

;; ** Mode specific settings
;; Disable keys in minibuffers such as ivy, etc
;; (add-hook 'minibuffer-setup-hook 'my/keys-mode-turn-off)
;; (add-hook 'messages-buffer-mode-hook 'my/keys-mode-turn-on)

;; * Global setting
;; Define my mode for setting global settings in all buffers
;; (define-minor-mode my/mode nil t nil nil)

;; (define-globalized-minor-mode my/global-mode my/mode
;; (lambda ()
;; (setq my/truncate-lines nil)))
;; ;;(toggle-truncate-lines -1)))

;; (my/global-mode 1)

;; * Generic functions and variables
;; ** File management
;; *** Create directory if directory doesn't exist
(defun my/create-dir-if-not-exist (dir)
  (if (not (file-directory-p dir))
      (progn
	(make-directory dir)
	(message (concat "dir: " dir " created!")))))

;; *** Create file if file doesn't exist
(defun my/create-file-if-not-exist (file)
  (if (not (file-exists-p file))
      (progn
	(write-region "" nil file)
	(message (concat "Wrote file: " file " created!")))))

;; *** Create file with content if file doesn't exist
(defun my/create-file-with-content-if-not-exist (file content)
  (if (not (file-exists-p file))
      (progn
	(write-region content nil file)
	(message (concat "Wrote file: " file " with contents")))))
;; " created with content: " content

;; *** Add to content to file or create file with content if file doesn't exist
(defun my/add-to-or-create-file-with-content (file content)
  (write-region (concat "\n" content) nil file t)
  (message (concat "Wrote file: " file " with contents")))

;; ** Is external package installed
;; Checks variable =exec-path= for package
(defun my/is-system-package-installed (package)
  (if (executable-find (symbol-name package))
      (symbol-name package)
    (message (concat "Package: " (symbol-name package) " not installed"))
    ()))

;; *** Set exec-path by system
;; (if (string-match-p "guixsd" (system-name))
;; (add-to-list 'exec-path "/bin/" ))

;; ** Give buffer unique name
(defun my/give-buffer-unique-name (base-name)
  (rename-buffer base-name t))

;; ** Is font installed
(defvar my/font-family-list (font-family-list))
(defun my/font-installed (font)
  (if (member font my/font-family-list)
      t
    nil))

;; ** Fake key
;; *** Normal emacs buffers 
;; Doesn't work on keys that are not english
;; (defun my/fake-key (key key-symbol)
;;  (interactive)
;;  (setq unread-command-events (listify-key-sequence "ö")))

(defun my/fake-key (key key-symbol)
  (interactive)
  (let ((command (key-binding key)))
    (setq last-command-event key-symbol)
    (setq this-command command)
    (call-interactively command)))

(defun my/fake-open-keymap (key)
  (setq unread-command-events
	(mapcar (lambda (e) `(t . ,e))
		(listify-key-sequence (kbd key)))))

;; *** Exwm
(defun my/exwm-fake-key (key)
  "Key is a string"
  (interactive)
  (exwm-input--fake-key (string-to-char key)))

;; ** Fold ellipsis
(defvar my/fold-ellipsis)
(defvar my/fold-ellipsis-char)

(if window-system
    (progn
      (setq my/fold-ellipsis "↴")
      (setq my/fold-ellipsis-char ?↴))
  ;; The terminal probably doesn't support unicode
  (setq my/fold-ellipsis "↓")
  (setq my/fold-ellipsis-char ?↓))

;; ** File size human readable
;; Default file-size-human-readable returns decimal values
(require 'files)

(defun my/file-size-human-readable (file-size &optional flavor decimal)
  "Produce a string showing FILE-SIZE in human-readable form.
 
   Optional second argument FLAVOR controls the units and the display format:
 
    If FLAVOR is nil or omitted, each kilobyte is 1024 bytes and the produced
       suffixes are \"k\", \"M\", \"G\", \"T\", etc.
    If FLAVOR is `si', each kilobyte is 1000 bytes and the produced suffixes
       are \"k\", \"M\", \"G\", \"T\", etc.
    If FLAVOR is `iec', each kilobyte is 1024 bytes and the produced suffixes
       are \"KiB\", \"MiB\", \"GiB\", \"TiB\", etc.
    If DECIMAL is true, a decimal number is returned"
  (setq 1024Decimal (if decimal 1024.0 1024))
  (setq 1000Decimal (if decimal 1000.0 1000))
  
  (let ((power (if (or (null flavor) (eq flavor 'iec))
		   1024Decimal
		 1000Decimal))
	(post-fixes
	 ;; none, kilo, mega, giga, tera, peta, exa, zetta, yotta
	 (list "" "k" "M" "G" "T" "P" "E" "Z" "Y")))
    (while (and (>= file-size power) (cdr post-fixes))
      (setq file-size (/ file-size power)
	    post-fixes (cdr post-fixes)))
    (format (if (> (mod file-size 1.0) 0.05)
		"%.1f%s%s"
	      "%.0f%s%s")
	    file-size
	    (if (and (eq flavor 'iec) (string= (car post-fixes) "k"))
		"K"
	      (car post-fixes))
	    (if (eq flavor 'iec) "iB" ""))))

;; ** Set font
(defun my/set-default-font (font)
  (if window-system
      (set-face-attribute 'default nil
			  :family font
			  :height my/default-face-height)))

;; ** Overlay
(defun my/inline-overlay-print (string)
  (let ((inline-overlay (make-overlay (point) (line-end-position))))
    ;; Put overlay
    (overlay-put inline-overlay 'after-string
		 (propertize
		  (concat
		   " ;=>"
		   string
		   )
		  'face '(:foreground "light blue")
		  ))
    ;; Just sit for 100 seconds
    (sit-for 100)
    ;; Then delete overlay
    (delete-overlay inline-overlay)))

;; (my/inline-overlay-print "test")

;; ** Repeat char
(defun my/repeat-char (char initial-string n)
  (setq initial-string (concat char initial-string))
  (if (> n 1)
      (my/repeat-char char initial-string (- n 1))
    initial-string))

;; * Fonts
;; ** Normal fonts
(defvar my/is-font-mono nil)
(defvar my/font nil)
(defvar my/font-raw nil)

(if (my/font-installed "FreeSans")
    (progn
      (setq my/font-raw "FreeSans")
      (setq my/font "FreeSans"))
  (if (my/font-installed "Open Sans")
      (progn
	(setq my/font-raw "Open Sans")
	(setq my/font "opensans"))
    (if (my/font-installed "dejavu sans")
	(progn
	  (setq my/font-raw "dejavu sans")
	  (setq my/font "DejaVuSans")))))

;;  (if my/font
;;      (my/set-default-font my/font))

;; ** Mono font
(defun my/get-best-mono-font ()
  (if my/is-font-mono
      my/font
    (if (my/font-installed "Inconsolata LGC")
	"Inconsolata LGC"
      (if (my/font-installed "Inconsolata")
	  "Inconsolata"
	(if (my/font-installed "dejavu sans mono")
	    "DejaVuSansMono"
	  (if (my/font-installed "Noto Sans Mono")
	      "NotoSansMono"
	    (if (my/font-installed "Perfect DOS VGA 437")
		"Perfect DOS VGA 437")))))))

(defvar my/mono-font (my/get-best-mono-font))

(if my/mono-font
    (my/set-default-font my/mono-font))

;; * Startup processes
;; ** Prevent async command from opening new window
;; Buffers that I don't want popping up by default
(add-to-list 'display-buffer-alist
	     '("\\*Async Shell Command\\*.*" display-buffer-no-window))

;; ** Check if OS is fully compatible
(defvar fully-compatible-system (or (eq system-type 'gnu/linux)(eq system-type 'gnu)(eq system-type 'gnu/kfreebsd)))

;; ** Redshift
(if (my/is-system-package-installed 'redshift)
    (start-process "redshift" nil "redshift"))

;; ** Garbage collection
(setq garbage-collection-messages t)

(setq my/after-gc-mem gc-cons-threshold)
(setq gc-cons-threshold 800000000)

;; ** Disable custom
;; Stop custom from editing init.el
(setq custom-file (concat user-emacs-directory ".emacs-custom.el"))

;; * Evil
(setq evil-search-module 'evil-search)
(setq evil-vsplit-window-right t)
(setq evil-split-window-below t)
(setq evil-shift-round nil)

;; Makes swiper A LOT faster
(setq evil-ex-interactive-search-highlight t)
(setq evil-ex-search-persistent-highlight nil)

(straight-use-package 'evil)
(require 'evil)

;; (fset 'evil-visual-update-x-selection 'ignore)
(evil-mode)

;; ** Minibuffer
;; Enable evil in minibuffer
(setq evil-want-minibuffer t)

;; This fixes evil minibuffer binds
(add-hook 'minibuffer-setup-hook 'evil-insert-state)

;; *** Set max minibuffer height
;; (setq max-mini-window-height 1)

;; ** Bind evil key functions
(defun my/evil-emacs-define-key (key command)
  (interactive)
  (define-key evil-emacs-state-map (kbd key) command))

(defun my/evil-insert-define-key (key command)
  (interactive)
  (define-key evil-insert-state-map (kbd key) command))

(defun my/evil-normal-define-key (key command)
  (interactive)
  (define-key evil-normal-state-map (kbd key) command)
  (define-key evil-motion-state-map (kbd key) command))

(defun my/evil-replace-define-key (key command)
  (interactive)
  (define-key evil-replace-state-map (kbd key) command))

(defun my/evil-visual-define-key (key command)
  (interactive)
  (define-key evil-visual-state-map (kbd key) command))

(defun my/evil-universal-define-key (key command)
  (interactive)
  (my/evil-insert-define-key key command)
  (my/evil-normal-define-key key command)
  (my/evil-visual-define-key key command)
  (my/evil-replace-define-key key command))

;; ** Evil-multiple cursors
(straight-use-package 'evil-mc)
;; (straight-use-package '(evil-mc :type git :host github :repo "walseb/evil-mc"))

(setq evil-mc-key-map nil)

(require 'evil-mc)
;; (setq evil-mc-key-map nil)

(global-evil-mc-mode 1)
;; (setq evil-mc-key-map nil)

(add-to-list 'evil-mc-custom-known-commands
	     '(delete-char . ((:default . evil-mc-execute-default-call-with-count))))

(add-to-list 'evil-mc-custom-known-commands
	     '(org-delete-char . ((:default . evil-mc-execute-default-call-with-count))))

(add-to-list 'evil-mc-custom-known-commands
	     '(csharp-maybe-insert-codedoc . ((:default . evil-mc-execute-default-call-with-count))))

;; *** Clear default keys
(setq evil-mc-key-map nil)


;; *** Disable on keybord-quit (C-g)
(setq evil-mc-undo-cursors-on-keyboard-quit t)

;; *** Keys
(define-key evil-visual-state-map "A" 'evil-mc-make-cursor-in-visual-selection-end)
(define-key evil-visual-state-map "I" 'evil-mc-make-cursor-in-visual-selection-beg)

;; ** Settings
;; *** Disable messages in echo area
;; Evil spams message area
(setq
 evil-emacs-state-message nil
 evil-operator-state-message nil
 evil-insert-state-message nil
 evil-replace-state-message nil
 evil-motion-state-message nil
 evil-normal-state-message nil
 evil-visual-state-message nil)

;; *** Cursor states
(setq evil-emacs-state-cursor '("purple" box))
(setq evil-normal-state-cursor '("white" box))
(setq evil-visual-state-cursor '("yellow" box))
(setq evil-insert-state-cursor '("orange" box))
(setq evil-replace-state-cursor '("green" box))
(setq evil-operator-state-cursor '("white" hollow))

;; *** Disable emacs mode
(setq evil-emacs-state-modes nil)

;; *** Set which modes use which evil state by default
;; Example
(setq evil-insert-state-modes nil)

(if (string< emacs-version "24.3")
    (error "Since emacs version is under 24.3, you need to remove cl-... in this section, and add (require 'cl) (not recommended to do in later versions)"))

(cl-loop for (mode . state) in '(
				 ;; So i C-leader works for exwm windows
				 (exwm-mode . emacs)
				 ;;(eshell-mode . insert)
				 (term-mode . insert)
				 ;;(org-agenda-mode . insert)
				 (magit-popup-mode . insert)
				 (proced-mode . insert)
				 (emms-playlist-mode . insert))
	 do (evil-set-initial-state mode state))

;; *** Switching to normal state without moving cursor
(defun my/evil-normal-state (&optional arg)
  (if (not(eq evil-state 'normal))
      (progn
	(evil-normal-state arg)
	(move-to-column (+ 1 (current-column))))))

;; *** Make one space enough to end work for use with evil sentence motion
(setq sentence-end-double-space nil)

;; *** Make dd and cc act on lines
(my/evil-normal-define-key "D" 'evil-delete-whole-line)
(my/evil-normal-define-key "C" 'evil-change-whole-line)

;; ** Text objects
;; *** Evil-entire-buffer
;; Modify entire buffer - for example: "d a e"
;; https://github.com/supermomonga/evil-textobj-entire
(evil-define-text-object evil-entire-entire-buffer (count &optional beg end type)
  "Select entire buffer"
  (evil-range (point-min) (point-max)))

(define-key evil-outer-text-objects-map "e" 'evil-entire-entire-buffer)
(define-key evil-inner-text-objects-map "e" 'evil-entire-entire-buffer)

;; *** Evil-line
;; https://github.com/syohex/evil-textobj-line
(defun my/evil-line-range (count beg end type &optional inclusive)
  (if inclusive
      (evil-range (line-beginning-position) (line-end-position))
    (let ((start (save-excursion
		   (back-to-indentation)
		   (point)))
	  (end (save-excursion
		 (goto-char (line-end-position))
		 (skip-syntax-backward " " (line-beginning-position))
		 (point))))
      (evil-range start end))))

(evil-define-text-object my/evil-a-line (count &optional beg end type)
  "Select range between a character by which the command is followed."
  (my/evil-line-range count beg end type t))
(evil-define-text-object my/evil-inner-line (count &optional beg end type)
  "Select inner range between a character by which the command is followed."
  (my/evil-line-range count beg end type))

(define-key evil-outer-text-objects-map "l" 'my/evil-a-line)
(define-key evil-inner-text-objects-map "l" 'my/evil-inner-line)

;; *** Evil-indent-plus
;; Allows for using indention as text objects
(straight-use-package 'evil-indent-plus)

(define-key evil-inner-text-objects-map "i" 'evil-indent-plus-i-indent)
(define-key evil-outer-text-objects-map "i" 'evil-indent-plus-a-indent)
(define-key evil-inner-text-objects-map "I" 'evil-indent-plus-i-indent-up)
(define-key evil-outer-text-objects-map "I" 'evil-indent-plus-a-indent-up)
(define-key evil-inner-text-objects-map "C-i" 'evil-indent-plus-i-indent-up-down)
(define-key evil-outer-text-objects-map "C-i" 'evil-indent-plus-a-indent-up-down)

;; *** Evil textobject block
(straight-use-package 'evil-textobj-anyblock)

(define-key evil-inner-text-objects-map "b" 'evil-textobj-anyblock-inner-block)
(define-key evil-outer-text-objects-map "b" 'evil-textobj-anyblock-a-block)

;; (define-key evil-motion-state-map "!" 'evil-textobj-anyblock-forward-open-block-start)

(setq evil-textobj-anyblock-blocks
      '(("(" . ")")
	("{" . "}")
	("\\[" . "\\]")
	("<" . ">")
	("\"" . "\"")
	("“" . "”")))

;; *** Evil-surround
;; (straight-use-package 'evil-surround)
;; (global-evil-surround-mode 1)

;; *** Evil-args
(straight-use-package 'evil-args)

;; bind evil-args text objects
(define-key evil-inner-text-objects-map "a" 'evil-inner-arg)
(define-key evil-outer-text-objects-map "a" 'evil-outer-arg)

;; ** Evil-lion
;; (straight-use-package 'evil-lion)

;; (evil-lion-mode)

;; ** Evil-goggles
(straight-use-package 'evil-goggles)
(evil-goggles-mode)
;; Disable pulse which both fixes so that you can set foreground color on the pulse font and saves on performance
(setq evil-goggles-pulse nil)
(setq evil-goggles-duration 60)

(evil-goggles-use-diff-faces)

;; ** Match paren
;; The normal evil-jump-item gives up easily. This tries to get to a paren more
(defun my/match-paren ()
  (interactive)
  (when (not (ignore-errors (call-interactively #'evil-jump-item)))
    (backward-up-list)))

(my/evil-normal-define-key "%" 'my/match-paren)

;; ** Goto end of line
;; By default evil goes to the last line, first char. This goes to the very last char in the buffer
(evil-define-motion evil-goto-line (count)
  "Go to the first non-blank character of line COUNT.
   By default the last line."
  :jump t
  :type line
  (if (null count)
      (with-no-warnings (end-of-buffer))
    (goto-char (point-min))
    (forward-line (1- count)))
  (end-of-line))

;; ** Keys
;; Prevent emacs state from being exited with esc, fixes exwm since it uses emacs state and to exit hydra you have to do esc
(define-key evil-emacs-state-map (kbd "<escape>") 'keyboard-quit)

;; Couldn't bother to create custom evil-join
;; P is normally bound to manual, make this key useful
(my/evil-normal-define-key "P" 'delete-indentation)

;; *** Rebind evil case change
(my/evil-normal-define-key "g u" 'evil-downcase)
(my/evil-normal-define-key "g U" 'evil-upcase)

;; *** RET in normal mode should insert enter
(my/evil-normal-define-key "RET" #'newline)

;; *** Add perspective movement to g
(my/evil-normal-define-key "gb" 'evil-scroll-line-to-bottom)
(my/evil-normal-define-key "gf" 'evil-scroll-line-to-top)
(my/evil-normal-define-key "ge" 'evil-scroll-line-to-center)
;; (my/evil-normal-define-key "/" 'evil-scroll-line-to-center)

;; *** Don't complete from all buffers
(setq evil-complete-all-buffers nil)

;; *** Don't add pasted over thing to killring
(setq evil-kill-on-visual-paste nil)

;; *** Go down visual line with M-p, M-n
(my/evil-universal-define-key "M-n" #'evil-next-visual-line)
(my/evil-universal-define-key "M-p" #'evil-previous-visual-line)

;; *** Move by paragraph easier, switch with evil-replace
(my/evil-normal-define-key "r" 'evil-forward-paragraph)
(my/evil-visual-define-key "r" 'evil-forward-paragraph)
(my/evil-normal-define-key "R" 'evil-backward-paragraph)
(my/evil-visual-define-key "R" 'evil-backward-paragraph)

(my/evil-normal-define-key "j" 'evil-replace)
(my/evil-visual-define-key "j" 'evil-replace)
(my/evil-normal-define-key "J" 'evil-replace-state)
(my/evil-visual-define-key "J" 'evil-replace-state)

;; *** Don't save chars deleted with x to clipboard
(my/evil-normal-define-key "x" 'delete-char)
(my/evil-normal-define-key "X"
			   '(lambda () (interactive)
			      (backward-char)
			      (call-interactively #'delete-char)))

;; * Backups
;; Stop emacs from creating backup files on every save
(setq make-backup-files nil)
;; Max amount of characters, 200 000 ~200kb
(defvar my/per-session-backup-limit 200000)

(defvar my/backup-directory (concat (expand-file-name user-emacs-directory) "backups/"))
(defvar my/backup-per-session-directory (concat my/backup-directory "per-session/"))
(defvar my/auto-saves-directory (concat (expand-file-name user-emacs-directory) "auto-saves/"))

(my/create-dir-if-not-exist my/backup-directory)
(my/create-dir-if-not-exist my/backup-per-session-directory)
(my/create-dir-if-not-exist my/auto-saves-directory)
(defun my/backup-buffer-mode-ok ()
  (pcase (file-name-extension (buffer-name))
    ("gpg" nil)
    (_ (pcase major-mode
	 ('image-mode nil)
	 (_ t)))))

(defun my/should-backup-buffer ()
  (and (buffer-modified-p) buffer-file-name (my/backup-buffer-mode-ok) (< (point-max) my/per-session-backup-limit)))

(defun my/backup-format-file-path (path)
  (replace-regexp-in-string "/" "!" path))

(defun my/backup-buffer (backup-path)
  (interactive)
  (if (my/should-backup-buffer)
      (save-restriction (widen) (write-region (point-min) (point-max) (concat backup-path (number-to-string (floor (float-time))) (my/backup-format-file-path (buffer-file-name)))))))

;; ** Make backup on first save
(defvar my/first-save t)

(defun my/backup-original-buffer ()
  (interactive)
  (if my/first-save
      (progn
	(my/backup-buffer my/backup-directory)
	(setq-local my/first-save nil))))

;; ** Make backup on every save
(defun my/backup-buffer-per-session ()
  (interactive)
  (if (not my/first-save)
      (my/backup-buffer my/backup-per-session-directory)))

;; ** Delete old backups
;; Automatically delete old backup files older than a week
(message "Deleting old backup files...")
(let ((week (* 60 60 24 7))
      (current (float-time (current-time))))
  (dolist (file (directory-files my/backup-directory t))
    (when (and (backup-file-name-p file)
	       (> (- current (float-time (nth 5 (file-attributes file))))
		  week))
      (message "%s" file)
      (delete-file file))))

;; ** Delete per-session backups on startup
(async-shell-command (concat "rm " my/backup-per-session-directory "*" ))

;; ** Undo tree
(straight-use-package 'undo-tree)

(setq global-undo-tree-mode t)

;; Fixes errors
(setq undo-tree-enable-undo-in-region nil)
(setq-default undo-tree-enable-undo-in-region nil)

;; (setq undo-tree-auto-save-history t)
;; (setq-default undo-tree-auto-save-history t)

(setq undo-tree-visualizer-lazy-drawing nil)
(setq-default undo-tree-visualizer-lazy-drawing nil)

(setq undo-tree-visualizer-timestamps t)
(setq undo-tree-visualizer-diff t)

;; (setq undo-tree-auto-save-history t)

;; (setq undo-tree-history-directory-alist '(("." . "~/.emacs.d/saves")))
;; (make-directory (concat spacemacs-cache-directory "undo"))

;; *** Keys
(add-hook 'undo-tree-visualizer-mode-hook '(lambda () (interactive) (run-with-timer 0.1 nil 'evil-force-normal-state)))

(setq undo-tree-visualizer-mode-map (make-sparse-keymap))

(evil-define-key 'insert undo-tree-visualizer-mode-map (kbd "p") #'undo-tree-visualize-undo)
(evil-define-key 'insert undo-tree-visualizer-mode-map (kbd "n") #'undo-tree-visualize-redo)
(evil-define-key 'insert undo-tree-visualizer-mode-map (kbd "l") #'undo-tree-visualize-switch-branch-right)
(evil-define-key 'insert undo-tree-visualizer-mode-map (kbd "h") #'undo-tree-visualize-switch-branch-left)
(evil-define-key 'insert undo-tree-visualizer-mode-map (kbd "d") #'undo-tree-visualizer-toggle-diff)

;; * Leader
;; When changing leader, change =my/leader-map-key=
(define-prefix-command 'my/leader-map)

(defvar my/leader-map-key "SPC")
(defvar my/mod-leader-map-key "C-SPC")

(defvar my/window-leader-key "C-=")
(defvar my/mod-window-leader-key "M-C-=")

(my/evil-normal-define-key "U" 'undo-tree-visualize)

(my/evil-normal-define-key my/leader-map-key my/leader-map)
(my/evil-visual-define-key my/leader-map-key my/leader-map)

(my/evil-universal-define-key my/mod-leader-map-key my/leader-map)

;; * Alert
(defvar my/past-alerts (list))

(defun my/alert (&optional str severity flash-once)
  (let ((color
	 (pcase severity
	   ('low "green")
	   ('med  "yellow")
	   ('high  "red")
	   (_   "blue"))))
    
    (if flash-once
	(my/alert-blink-fringe-once color)
      (my/alert-blink-fringe color))
    
    (if str
	(progn
	  (push " " my/past-alerts)
	  (push (propertize (concat "[" str "]") 'face `(:background ,color)) my/past-alerts)
	  (message str)))))

(defvar my/alert-blink-fringe-color "red")

(defun my/alert-blink-fringe-once (color)
  (setq my/alert-blink-fringe-color color)
  (my/alert-fringe-set-color)
  (run-with-timer 0.25 nil 'my/alert-fringe-restore))

(defun my/alert-blink-fringe (color)
  (setq my/alert-blink-fringe-color color)
  (my/alert-fringe-set-color)
  (run-with-timer 0.25 nil 'my/alert-fringe-restore)
  (run-with-timer 0.5 nil 'my/alert-fringe-set-color)
  (run-with-timer 0.75 nil 'my/alert-fringe-restore)
  (run-with-timer 1.0 nil 'my/alert-fringe-set-color)
  (run-with-timer 1.25 nil 'my/alert-fringe-restore))

(defun my/alert-fringe-set-color ()
  (set-face-attribute 'fringe nil :foreground my/alert-blink-fringe-color :background my/alert-blink-fringe-color))

(defun my/alert-fringe-restore ()
  (set-face-attribute 'fringe nil :foreground nil :background nil))

(defun my/alert-reset ()
  (interactive)
  (setq my/past-alerts (list))
  (my/lv-line-update))

(defun my/alert-remove ()
  (interactive)
  (setq my/past-alerts (remove (completing-read "Remove entry" my/past-alerts) my/past-alerts))
  (my/lv-line-update))

(define-key my/leader-map (kbd "DEL") 'my/alert-reset)

;; * Package management
;; ** Guix
(straight-use-package 'guix)

;; *** Keys
(define-prefix-command 'my/guix-map)
(define-key my/leader-map (kbd "G") 'my/guix-map)

(define-key my/guix-map (kbd "v") 'guix)
(define-key my/guix-map (kbd "P") 'guix-profiles)
(define-key my/guix-map (kbd "g") 'guix-generation)
(define-key my/guix-map (kbd "G") 'guix-system-generations)
(define-key my/guix-map (kbd "i") 'guix-installed-user-packages)
(define-key my/guix-map (kbd "I") 'guix-installed-system-packages)

(define-prefix-command 'my/guix-services-map)
(define-key my/guix-map (kbd "s") 'my/guix-services-map)

(define-key my/guix-services-map (kbd "a") 'guix-all-services)
(define-key my/guix-services-map (kbd "n") 'guix-services-by-name)
(define-key my/guix-services-map (kbd "l") 'guix-services-by-location)
(define-key my/guix-services-map (kbd "d") 'guix-find-service-definition)

(define-prefix-command 'my/guix-package-map)
(define-key my/guix-map (kbd "p") 'my/guix-package-map)

(define-key my/guix-package-map (kbd "a") 'guix-all-packages)
(define-key my/guix-package-map (kbd "n") 'guix-packages-by-name)
(define-key my/guix-package-map (kbd "l") 'guix-packages-by-location)
(define-key my/guix-package-map (kbd "c") 'guix-packages-from-system-config-file)
(define-key my/guix-package-map (kbd "d") 'guix-find-package-definition)

(define-prefix-command 'my/guix-store-map)
(define-key my/guix-map (kbd "S") 'my/guix-store-map)

(define-key my/guix-store-map (kbd "l") 'guix-store-live-items)
(define-key my/guix-store-map (kbd "d") 'guix-store-dead-items)
(define-key my/guix-store-map (kbd "D") 'guix-store-item-derivers)
(define-key my/guix-store-map (kbd "f") 'guix-store-failures)
(define-key my/guix-store-map (kbd "r") 'guix-store-item-references)
(define-key my/guix-store-map (kbd "R") 'guix-store-item-referrers)
(define-key my/guix-store-map (kbd "C-r") 'guix-store-item-requisites)

;; ** Local packages
(add-to-list 'load-path (expand-file-name (concat user-emacs-directory "local-packages")))

;; * Write configs
(defun my/write-configs ()
  (interactive)
  (pcase (completing-read "Which config to write"
			  '("gnus" "xdefaults" "xinit" "xmodmap" "mpd" "gpg-agent" "cabal"))
    ("gnus" (my/write-gnus))
    ("xdefaults" (my/write-xdefaults))
    ("xinit" (my/write-xinitrc))
    ("xmodmap" (my/write-xmodmap))
    ("mpd" (my/write-mpd-config))
    ("gpg-agent" (my/write-gpg-agent-config))
    ("cabal" (my/write-cabal-config))))

(define-key my/leader-map (kbd "C-c") 'my/write-configs)

;; ** Write .gnus.el
;; Create =.gnus.el=, which gnus reads from
(defconst my/gnus-config-text "
   AddYourEmailHereThenDeleteThis
   (setq mail-host-address \"MyAdress\")
   ")

(defun my/write-gnus ()
  (my/create-file-with-content-if-not-exist
   "~/.gnus.el" my/gnus-config-text))

;; ** Write .Xdefaults
;; emacs. commands to disable scrollbar, etc before launching emacs, improving startup time
(defconst my/xdefaults-config-text "
   emacs.toolBar: 0
   emacs.menuBar: 0
   emacs.verticalScrollBars: off")

(defun my/write-xdefaults ()
  (my/create-file-with-content-if-not-exist "~/.Xdefaults" my/xdefaults-config-text))

;; ** Write .xinitrc
;; =xset s= disables screen saver
;; setxkbmap to select keyboard layout

(defconst my/xinit-config-text "
   xset s off
   xset s noblank
   xset s off
   xset s off -dpms
 
   setxkbmap -layout us -variant altgr-intl
   # setxkbmap -layout carpalx -variant qgmlwy
 
   # xmodmap ~./xmodmap
 
   # Fix java windows in exwm
   export _JAVA_AWT_WM_NONREPARENTING=1
 
   exec emacs")

(defun my/write-xinitrc ()
  (my/create-file-with-content-if-not-exist "~/.xinitrc" my/xinit-config-text))

;; ** Write .xmodmap
;; This swaps capslock and ctrl
(defconst my/xmodmap-config-text "
   ! Swap Caps_Lock and Control_L
   remove Lock = Caps_Lock
   remove Control = Control_L
   keysym Control_L = Caps_Lock
   keysym Caps_Lock = Control_L
   add Lock = Caps_Lock
   add Control = Control_L
   ")

(defun my/write-xmodmap ()
  (my/create-file-with-content-if-not-exist "~/.xmodmap" my/xmodmap-config-text))

;; ** Write mpd
(defconst my/mpd-config-text "
   music_directory \"~/Music\"
   playlist_directory  \"~/.config/mpd/playlists\"
   db_file \"~/.config/mpd/mpd.db\"
   log_file \"~/.config/mpd/mpd.log\"
   bind_to_address \"127.0.0.1\"
   port \"6600\"
 
   # For pulse audio
   audio_output {
   type \"pulse\"
   name \"pulse audio\"
   }")

(defun my/write-mpd-config ()
  (let* ((config-dir "~/.config/")
	 (mpd-dir (concat config-dir "mpd/"))
	 (mpd-config (concat mpd-dir "mpd.conf")))
    (my/create-dir-if-not-exist config-dir)
    
    (my/create-dir-if-not-exist mpd-dir)
    
    (my/create-file-with-content-if-not-exist mpd-config my/mpd-config-text)
    
    (my/create-file-if-not-exist (concat mpd-dir "mpd.log"))
    (my/create-file-if-not-exist (concat mpd-dir "mpd.db"))
    (my/create-dir-if-not-exist (concat mpd-dir "playlists/"))))

;; ** Write GPG pinentry
(defun my/write-gpg-agent-config ()
  (let* ((gpg-dir "~/.gnupg/")
	 (gpg-file (concat gpg-dir "gpg-agent.conf")))
    (my/create-dir-if-not-exist gpg-dir)
    (my/create-file-with-content-if-not-exist gpg-file "allow-emacs-pinentry")
    (shell-command "gpgconf --reload gpg-agent")))

;; ** Write cabal config
(defconst my/nix-config-text "nix: true
   documentation: True")

(defun my/write-cabal-config ()
  (let* ((cabal-dir "~/.cabal/")
	 (cabal-file (concat cabal-dir "config")))
    (my/create-dir-if-not-exist cabal-dir)
    (my/create-file-with-content-if-not-exist cabal-file my/nix-config-text)))

;; * Minor
;; ** Startup
;; Disable startup message
(setq inhibit-startup-message t)

;; ** Scratch buffer
;; *** Disable scratch buffer on startup
;; We need to do this because the scratch buffer created by emacs is temporary, the one in this config is a file
;;(kill-buffer "*scratch*")

;; *** Disable initial scratch buffer message
(setq initial-scratch-message nil)

;; *** Set default mode
(setq initial-major-mode 'org-mode)

;; ** Encoding
;; (setq locale-coding-system 'utf-8)
;; (set-terminal-coding-system 'utf-8)
;; (set-keyboard-coding-system 'utf-8)
;; (set-selection-coding-system 'utf-8)
;; (prefer-coding-system 'utf-8)

;; ** Line wrapping
;; *** Enable truncate lines mode
(setq-default truncate-lines nil)
(setq truncate-lines nil)

;; Always truncate lines
(setq truncate-partial-width-windows nil)
(setq-default truncate-partial-width-windows nil)

;; **** Toggle truncate lines
(define-key my/leader-map (kbd "C-v")
  '(lambda () (interactive)
     (setq truncate-lines (not truncate-lines))))

;; *** Fringe indicators of wrapped line
(setq visual-line-fringe-indicators '(right-triangle nil))

;; ** Disable useless functionallity
(tooltip-mode -1)

;; ** 1 letter prompts
;; Convert yes or no prompt to y or n prompt
(defalias 'yes-or-no-p 'y-or-n-p)

;; ** Smooth scroll
;; Scroll 1 line at a time when cursor goes outside screen
(setq scroll-conservatively 100)

;; ** Bell
;; Disable bell
(setq ring-bell-function 'ignore)

;; ** Subword (camel case movement)
;;  #+begin_src emacs-lisp
;; (global-subword-mode 1)
;;  #+end_src

;; ** Change max killring size
(setq kill-ring-max 500)

;; ** Pixel scroll mode
;; In org mode when displaying images pixel scroll mode can be useful maybe
;; (add-hook 'org-mode-hook 'pixel-scroll-mode)

;; ** Increase and decrease brightness
(defun my/increase-brightness ()
  (interactive)
  (shell-command "xbacklight +5"))

(defun my/decrease-brightness ()
  (interactive)
  (shell-command "xbacklight -5"))

(global-set-key (kbd "<XF86MonBrightnessUp>") 'my/increase-brightness)
(global-set-key (kbd "<XF86MonBrightnessDown>") 'my/decrease-brightness)

;; ** Update packages
(define-key my/leader-map (kbd "C-u") 'list-packages)

;; ** Sudo edit
(straight-use-package 'sudo-edit)

(define-key my/leader-map (kbd "M-s") 'sudo-edit)

;; *** Dired fix
(defun my/sudo-edit-is-on ()
  (string-equal
   (file-remote-p (or buffer-file-name default-directory) 'user)
   "root"))

(defun my/dired-sudo-edit-setup ()
  ;; If file is edited with sudo (in this case only works on dired due to hook)
  (if (my/sudo-edit-is-on)
      (dired-sort-other "-alh")))

(add-hook 'dired-mode-hook 'my/dired-sudo-edit-setup)

;; ** Enable disabled commands
(put 'narrow-to-region 'disabled nil)
(put 'narrow-to-page 'disabled nil)
(put 'narrow-to-defun 'disabled nil)

;; ** Async
(straight-use-package 'async)

(require 'async)
(require 'dired-async)
(autoload 'dired-async-mode "dired-async.el" nil t)
(dired-async-mode 1)

;; ** Zoom
;; (defun my/increase-volume ()
;; (interactive)
;; (text-scale-set 0))
;; (define-key my/leader-map (kbd "+") ')
;; (define-key my/leader-map (kbd "_") '(lambda () (interactive) (text-scale-set 0)))

(define-key my/leader-map (kbd "-") '(lambda () (interactive) (text-scale-decrease 4)))
(define-key my/leader-map (kbd "=") '(lambda () (interactive) (text-scale-increase 4)))

(define-key my/leader-map (kbd "C--") '(lambda () (interactive) (text-scale-decrease 1)))
(define-key my/leader-map (kbd "C-=") '(lambda () (interactive) (text-scale-increase 1)))


(define-key my/leader-map (kbd "+") '(lambda () (interactive) (text-scale-mode 0)))
(define-key my/leader-map (kbd "_") '(lambda () (interactive) (text-scale-mode 0)))

;; ** Toggle mono-font
;; (font-get "opensans" :spacing)
(defun my/toggle-mono-font(&optional arg)
  "If ARG is non-nil enable monofont, otherwise toggle it."
  (interactive)
  (if window-system
      (if (not my/is-font-mono)
	  (if  (or arg (string= (face-attribute 'default :family) my/font-raw))
	      (my/set-default-font my/mono-font)
	    (my/set-default-font my/font)))))

(define-key my/leader-map (kbd "C-f") 'my/toggle-mono-font)

;; *** Toggle local mono font
(defun my/toggle-local-mono-font(&optional arg)
  "If ARG is non-nil enable monofont, otherwise toggle it."
  (interactive)
  (if window-system
      (if (not my/is-font-mono)
	  (if  (or arg (string= (face-attribute 'default :family) my/font-raw))
	      (face-remap-add-relative 'default :family my/mono-font)
	    (face-remap-add-relative 'default :family my/font-raw)))))

(define-key my/leader-map (kbd "M-f") 'my/toggle-local-mono-font)

;; ** Exit emacs
(define-key my/leader-map (kbd "C-z") 'save-buffers-kill-emacs)

;; ** Bind help key
(define-key my/leader-map (kbd "h") help-map)

;; ** Help mode
(define-prefix-command 'my/help-map)
(define-key my/leader-map (kbd "H") 'my/help-map)

(define-key my/help-map (kbd "C-c") 'counsel-colors-emacs)
(define-key my/help-map (kbd "C") 'counsel-colors-web)

(define-key my/help-map (kbd "m") 'which-key-show-major-mode)

(define-key my/help-map (kbd "c") 'rainbow-mode)

(define-key my/help-map (kbd "y") 'yas-describe-tables)

;; *** Disable help mode binds
(setq help-mode-map (make-sparse-keymap))
(setq-default help-mode-map (make-sparse-keymap))

;; (evil-define-key 'normal help-mode-map (kbd "H") 'help-go-back)
;; (evil-define-key 'normal help-mode-map (kbd "L") 'help-go-forward)
;; (evil-define-key 'normal help-mode-map (kbd "<escape>") 'keyboard-quit)

(setq help-mode-map
      (let ((map (make-sparse-keymap)))
	(define-key map "H" 'help-go-back)
	(define-key map "L" 'help-go-forward)
	(define-key map (kbd "<escape>") 'keyboard-quit)
	map))

;; ** Compilation mode
;; (setq compilation-mode-map (make-sparse-keymap))
;; (setq-default compilation-mode-map (make-sparse-keymap))

;; (setq compilation-minor-mode-map (make-sparse-keymap))
;; (setq-default compilation-minor-mode-map (make-sparse-keymap))

;; (setq compilation-shell-minor-mode-map (make-sparse-keymap))
;; (setq-default compilation-shell-minor-mode-map (make-sparse-keymap))

;; (setq compilation-mode-tool-bar-map (make-sparse-keymap))
;; (setq-default compilation-mode-tool-bar-map (make-sparse-keymap))

(advice-add 'compilation-mode :after (lambda () (interactive) (evil-force-normal-state)))

;; ** Prefer loading newest lisp source file
(setq load-prefer-newer t)

;; ** Revert buffer bind
(define-key my/leader-map (kbd "r") 'revert-buffer)

;; ** Hotkey to hide cursor
(define-key my/leader-map (kbd "M-h") (lambda () (interactive) (setq cursor-type nil)))

;; ** Tetris
(evil-define-key 'insert tetris-mode-map (kbd "p") #'tetris-rotate-next)
(evil-define-key 'insert tetris-mode-map (kbd "P") #'tetris-rotate-prev)
(evil-define-key 'insert tetris-mode-map (kbd "n") #'tetris-move-down)
(evil-define-key 'insert tetris-mode-map (kbd "N") #'tetris-move-bottom)
(evil-define-key 'insert tetris-mode-map (kbd "h") #'tetris-move-left)
(evil-define-key 'insert tetris-mode-map (kbd "l") #'tetris-move-right)

(evil-define-key 'insert tetris-mode-map (kbd "SPC") #'tetris-move-bottom)

;; ** Redefine keyboard-escape-quit
(defun keyboard-escape-quit ()
  "Exit the current \"mode\" (in a generalized sense of the word).
   This command can exit an interactive command such as `query-replace',
   can clear out a prefix argument or a region,
   can get out of the minibuffer or other recursive edit,
   cancel the use of the current buffer (for special-purpose buffers),
   or go back to just one window (by deleting all but the selected window)."
  (interactive)
  (cond ((eq last-command 'mode-exited) nil)
	((region-active-p)
	 (deactivate-mark))
	((> (minibuffer-depth) 0)
	 (abort-recursive-edit))
	(current-prefix-arg
	 nil)
	((> (recursion-depth) 0)
	 (exit-recursive-edit))
	(buffer-quit-function
	 (funcall buffer-quit-function))))

;; ** lisp-ls
;; ls for systems without ls installed (like windows). Gets used automatically on those systems
(setq-default ls-lisp-format-time-list  '("%d-%m-%Y %H:%M" "%d-%m-%Y %H:%M")
	      ls-lisp-use-localized-time-format t)

;; ** Fix backward-sexp
(defun my/backward-sexp (&optional arg)
  "Fixed backward sexp so you don't have to place cursor 1 char in front of paren you want to go backward on"
  (interactive "^p")
  (or arg (setq arg 1))
  (my/forward-sexp (- arg) 1))

(defun my/forward-sexp (&optional arg extra-move)
  (interactive "^p")
  (or arg (setq arg 1))
  (if forward-sexp-function
      (funcall forward-sexp-function arg)
    (goto-char (or (scan-sexps (+ (point) extra-move) arg) (buffer-end arg)))
    (if (< arg 0) (backward-prefix-chars))))

;; ** Switch to last buffer
(defun my/switch-to-last-buffer ()
  (interactive)
  (switch-to-buffer (car (cdr (ivy--buffer-list "")))))

;; ** Echo keypresses instantly
(setq echo-keystrokes 0.01)

;; ** Disable mouse features
(define-key minibuffer-inactive-mode-map [mouse-1] #'ignore)

;; ** Minibuffer-depth
;; Enable and show minibuffer recursive depth
(setq enable-recursive-minibuffers t)
(minibuffer-depth-indicate-mode 1)

;; ** Clone indirect buffer name
;; *** Clone indirect buffer this window
(defun my/clone-indirect-buffer-name ()
  (interactive)
  (clone-indirect-buffer
   (concat
    (buffer-name)
    " | "
    (completing-read "Buffer name: " nil))
   t
   ))

;; *** Clone indirect buffer other window
(defun my/clone-indirect-buffer-name-other-window ()
  (interactive)
  (clone-indirect-buffer-other-window
   (concat
    (buffer-name)
    " | "
    (completing-read "Buffer name: " nil))
   t
   ))

;; ** Build config
(defun my/build-config-docs ()
  (interactive)
  (my/config-visit)
  (my/outorg-export-to-org-file "~/.emacs.d/readme.org"))

;; * File options
(define-prefix-command 'my/file-options-map)
(define-key my/leader-map (kbd "`") 'my/file-options-map)

;; ** Revert
(define-key my/file-options-map (kbd "r") 'revert-buffer)

;; ** Statistics
(define-prefix-command 'my/statistics-map)
(define-key my/file-options-map (kbd "s") 'my/statistics-map)

(define-key my/statistics-map (kbd "w") 'count-words)
(define-key my/statistics-map (kbd "r") 'count-words-region)

;; ** Indentation
(define-prefix-command 'my/indentation-map)
(define-key my/file-options-map (kbd "i") 'my/indentation-map)

(defun my/change-tab-width ()
  (interactive)
  (setq-default tab-width (string-to-number (completing-read "Enter tab width" nil))))

;; Applies only to region
(define-key my/indentation-map (kbd "t") 'tabify)
(define-key my/indentation-map (kbd "SPC") 'untabify)

(define-key my/indentation-map (kbd "w") 'my/change-tab-width)

;; * Open
(define-prefix-command 'my/open-map)
(define-key my/leader-map (kbd "o") 'my/open-map)

(defvar my/open-map-hook nil
  "Hook called after a buffer is visited through my/open-map")

;; ** Scratch
;; Kill the initial scratch buffer
(kill-buffer "*scratch*")

(defun my/switch-to-scratch()
  (interactive)
  (find-file (concat user-emacs-directory"*scratch*"))
  (run-hooks 'my/open-map-hook))

(define-key my/open-map (kbd "s") 'my/switch-to-scratch)

;; ** Backup
(defun my/backups-visit ()
  (interactive)
  (find-file (expand-file-name (concat user-emacs-directory "backups")))
  (run-hooks 'my/open-map-hook))

(defun my/backups-per-session-visit ()
  (interactive)
  (find-file (expand-file-name (concat user-emacs-directory "backups/per-session")))
  (run-hooks 'my/open-map-hook))


(define-key my/open-map (kbd "b") 'my/backups-per-session-visit)
(define-key my/open-map (kbd "B") 'my/backups-visit)

;; ** Visit nixos config
(defun my/nixos-config-visit ()
  (interactive)
  (find-file "/etc/nixos/configuration.nix")
  (run-hooks 'my/open-map-hook))

(define-key my/open-map (kbd "n") 'my/nixos-config-visit)

;; ** Visit config
(defun my/config-visit ()
  (interactive)
  (find-file (expand-file-name (concat user-emacs-directory "config.el")))
  ;; Emacs lags if flycheck runs on config
  (flycheck-mode -1)
  (run-hooks 'my/open-map-hook))

(define-key my/open-map (kbd "c") 'my/config-visit)

;; ** Reload config
(defun my/config-reload ()
  (interactive)
  (org-babel-load-file (expand-file-name (concat user-emacs-directory "config.org")))
  (run-hooks 'my/open-map-hook))
(define-key my/open-map (kbd "C-r") 'my/config-reload)


;; ** Open trash
(defun my/trash-visit ()
  (interactive)
  (find-file "~/.local/share/Trash/files/")
  (run-hooks 'my/open-map-hook))
(define-key my/open-map (kbd "t") 'my/trash-visit)


;; ** Open agenda
(defun my/org-agenda-show-agenda-and-todo (&optional arg)
  (interactive "P")
  (org-agenda arg "a")
  (run-hooks 'my/open-map-hook))

(define-key my/open-map (kbd "a") 'my/org-agenda-show-agenda-and-todo)

;; ** Open downloads
(defun my/open-downloads ()
  (interactive)
  (find-file "~/Downloads")
  (run-hooks 'my/open-map-hook))

(define-key my/open-map (kbd "d") 'my/open-downloads)

;; ** Open home
(defun my/open-home ()
  (interactive)
  (find-file "~")
  (run-hooks 'my/open-map-hook))

(define-key my/open-map (kbd "r") 'my/open-home)

;; ** Open password file
(defun my/open-passwords ()
  (interactive)
  (find-file espy-password-file)
  (run-hooks 'my/open-map-hook))

(define-key my/open-map (kbd "p") 'my/open-passwords)

;; ** Visit agenda file
(defun my/agenda-file-visit ()
  (interactive)
  (find-file "~/Notes/Agenda.org")
  (run-hooks 'my/open-map-hook))

(define-key my/open-map (kbd "A") 'my/agenda-file-visit)

;; ** Open firefox
(defvar my/gui-browser
  (if (my/is-system-package-installed 'icecat)
      "icecat"
    (if (my/is-system-package-installed 'firefox-nightly)
	"firefox-nightly"
      (if (my/is-system-package-installed 'iceweasel)
	  "iceweasel"
	"firefox"))))

(defvar my/temp-firefox-title-name "")
(defvar my/browser-bookmarks '(
			       "youtube.com"
			       "discordapp.com/channels/@me"
			       "github.com"
			       "steamcommunity.com/chat"
			       ))
(defun my/launch-firefox ()
  (interactive)
  (start-process my/gui-browser nil my/gui-browser "--new-window"))

;; (defun my/launch-firefox ()
;;   (interactive)
;;   (let* (
;;          (search (completing-read "url " my/browser-bookmarks))
;;          (adress
;;           (if (cl-member search my/browser-bookmarks :test #'string=)
;;               search
;;             (concat "https://www.google.com/search?q=" search))))
;;     (start-process (concat my/gui-browser my/temp-firefox-title-name) nil my/gui-browser "--new-window" adress)))

(define-key my/leader-map (kbd "C-b") 'my/launch-firefox)

;; ** Open eww
(defun my/launch-eww ()
  (interactive)
  (eww-browse-url (concat "https://www.google.com/search?q=" (completing-read "search: " nil))))

(define-key my/leader-map (kbd "b") 'my/launch-eww)

;; ** Suggest
(define-key my/leader-map (kbd "s") 'suggest)

;; * Org
(straight-use-package 'org)
(require 'org)
(require 'org-agenda)

;; Set org src indent to be 0
(setq org-edit-src-content-indentation 0)

(define-prefix-command 'my/org-mode-map)
(evil-define-key 'normal org-mode-map (kbd (concat my/leader-map-key " a")) #'my/org-mode-map)

;; ** Babel
;; *** Supported runnable languages
;;   ;; (org-babel-do-load-languages
;;    ;; 'org-babel-load-languages
;;    ;; '((R . t)
;;      ;; (ditaa . t)
;;      ;; (dot . t)
;;      ;; (emacs-lisp . t)
;;      ;; (gnuplot . t)
;;      ;; (haskell . nil)
;;      ;; (ocaml . nil)
;;      ;; (python . t)
;;      ;; (ruby . t)
;;      ;; (screen . nil)
;;      ;; (sh . t)
;;      ;; (sql . nil)
;;      ;; (sqlite . t)))

;; *** Disable warnings in org mode before evaluating source block
(setq org-confirm-babel-evaluate nil)

;; ** Bullets
(straight-use-package 'org-bullets)
(require 'org-bullets)

(when window-system
  (if (eq system-type 'windows-nt)
      (setq inhibit-compacting-font-caches t))
  (add-hook 'org-mode-hook (lambda () (interactive) (org-bullets-mode))))

;; ** Visuals
;; *** Hide emphasis markers
;; The equal signs =here= to make it bold should not be visible
(setq org-hide-emphasis-markers t)

;; *** Disable edit-src help header
(setq org-edit-src-persistent-message nil)

;; *** Disable code block indent
;; Should I change this??
;; (setq org-edit-src-content-indentation 0)

;; *** Change face of levels
(defvar my/org-level-1-height 1.9)
(defvar my/org-level-2-height 1.6)
(defvar my/org-level-3-height 1.4)
(defvar my/org-level-4-height 1.3)
(defvar my/org-level-5-height 1.25)
(defvar my/org-level-6-height 1.2)
(defvar my/org-level-7-height 1.15)
(defvar my/org-level-8-height 1.10)

(set-face-attribute 'org-level-1 nil :inherit 'outline-1) ;;:height my/org-level-1-height)
(set-face-attribute 'org-level-2 nil :inherit 'outline-2) ;;:height my/org-level-2-height)
(set-face-attribute 'org-level-3 nil :inherit 'outline-3) ;;:height my/org-level-3-height)
(set-face-attribute 'org-level-4 nil :inherit 'outline-4) ;;:height my/org-level-4-height)
(set-face-attribute 'org-level-5 nil :inherit 'outline-5) ;;:height my/org-level-5-height)
(set-face-attribute 'org-level-6 nil :inherit 'outline-6) ;;:height my/org-level-6-height)
(set-face-attribute 'org-level-7 nil :inherit 'outline-7) ;;:height my/org-level-7-height)
(set-face-attribute 'org-level-8 nil :inherit 'outline-8) ;;:height my/org-level-8-height)

;; :weight 'semi-bold

;; *** Ellipsis face
(setq org-ellipsis my/fold-ellipsis)

;; *** Always truncate lines
(setq org-startup-truncated nil)

;; ** Indent mode
(add-hook 'org-mode-hook 'org-indent-mode)

;; ** Org SRC
;; *** Make c-' open in current window
(setq org-src-window-setup 'current-window)

;; *** Don't save window layout
(add-hook 'org-src-mode-hook '(lambda () (interactive) (setq org-src--saved-temp-window-config nil)))

;; *** Rebind key
(define-key my/leader-map (kbd "'") 'my/toggle-org-src)

(defun my/toggle-org-src ()
  (interactive)
  (if (string= major-mode 'org-mode)
      (org-edit-special)
    (org-edit-src-exit)))

;; ** Agenda
;; Give agenda file to use
(if (file-exists-p "~/Notes/Agenda.org")
    (setq org-agenda-files (quote ("~/Notes/Agenda.org"))))

(setq org-agenda-window-setup 'current-window)

;; *** Display at startup
;; Spawn agenda buffer
;; (org-agenda-list)

;; **** Declare switch function
;; Because just giving "*Org Agenda*" to "initial-buffer-choice" doesn't work
(defun my/switch-to-agenda()
  (interactive)
  (switch-to-buffer "*Org Agenda*"))

;; **** Run switch function as initial buffer choice
(setq initial-buffer-choice 'my/switch-to-agenda)

;; **** Close all other open windows at start
(delete-other-windows)

;; ** Clock
;; (setq org-clock-mode-line-total today)

;; *** Keys
;; (define-prefix-command 'my/clock-map)
;; (define-key my/leader-map (kbd "c") 'my/clock-map)

;; (define-key my/clock-map (kbd "s") 'org-clock-in)
;; (define-key my/clock-map (kbd "S") 'org-clock-out)
;; (define-key my/clock-map (kbd "C-s") 'org-clock-in-last)

;; (define-key my/clock-map (kbd "e") 'org-clock-modify-effort-estimate)

;; ** Export
(define-prefix-command 'my/org-export-map)
(define-key my/org-mode-map (kbd "E") 'my/org-export-map)

;; *** Syntax highlighting for HTML export
(straight-use-package 'htmlize)

;; *** Twitter bootstrap
(straight-use-package 'ox-twbs)

;; *** ASCII
(define-prefix-command 'my/org-export-ascii-map)
(define-key my/org-export-map (kbd "a") 'my/org-export-ascii-map)

(define-key my/org-export-ascii-map (kbd "a") 'org-ascii-export-to-ascii)

;; *** HTML
(define-prefix-command 'my/org-export-html-map)
(define-key my/org-export-map (kbd "h") 'my/org-export-html-map)

(define-key my/org-export-html-map (kbd "h") 'org-html-export-to-html)
(define-key my/org-export-html-map (kbd "t") 'org-twbs-export-to-html)

;; *** PDF
(define-prefix-command 'my/org-export-pdf-map)
(define-key my/org-export-map (kbd "p") 'my/org-export-pdf-map)

(define-key my/org-export-pdf-map (kbd "p") 'org-latex-export-to-pdf)

;; *** Beamer presentation
(define-prefix-command 'my/org-export-slides-map)
(define-key my/org-export-map (kbd "s") 'my/org-export-slides-map)

(define-key my/org-export-slides-map (kbd "b") 'org-beamer-export-to-pdf)

;; *** Markdown
(define-prefix-command 'my/org-export-markdown-map)
(define-key my/org-export-map (kbd "m") 'my/org-export-markdown-map)

(define-key my/org-export-markdown-map (kbd "m") 'org-md-export-to-markdown)

;; *** ODT
(define-prefix-command 'my/org-export-odt-map)
(define-key my/org-export-map (kbd "o") 'my/org-export-odt-map)

(define-key my/org-export-odt-map (kbd "o") 'org-odt-export-to-odt)

;; *** Latex
(define-prefix-command 'my/org-export-latex-map)
(define-key my/org-export-map (kbd "l") 'my/org-export-latex-map)

(define-key my/org-export-latex-map (kbd "l") 'org-latex-export-to-latex)

;; ** Present
(defun my/org-present-next ()
  (interactive)
  (widen)
  (if (string= (string (char-after)) "*")
      (forward-line))
  (narrow-to-region
   (- (re-search-forward "^*") 1)
   (- (re-search-forward "^*") 1))
  (evil-open-fold)
  (goto-char (point-min)))

(defun my/org-present-prev ()
  (interactive)
  (widen)
  (if (string= (string (char-after)) "*")
      (forward-line))
  (narrow-to-region
   (re-search-backward "^*")
   (+ (re-search-backward "^*") 1))
  (evil-open-fold)
  (goto-char (point-min)))

;; ** Eldoc
;;  (straight-use-package 'org-plus-contrib)
;;  (require 'org-eldoc)
;;  (require 'org-src)
;;  (add-hook 'org-mode-hook #'org-eldoc-load)

;; *** Fix error
;; The function =org-src-get-lang-mode= doesn't exist, but the function =org-src--get-lang-mode= does
;;  (defun org-src-get-lang-mode (LANG)
;;    (org-src--get-lang-mode LANG))

;; ** Key
;; (evil-define-key 'normal org-mode-map (kbd "TAB") 'org-cycle)
;; (evil-define-key 'normal org-mode-map (kbd "C-s") 'swiper)

(define-key my/org-mode-map (kbd "i") 'org-toggle-inline-images)
(define-key my/org-mode-map (kbd "e") 'org-insert-link)

(define-key my/org-mode-map (kbd "p") 'org-shiftup)
(define-key my/org-mode-map (kbd "n") 'org-shiftdown)
(define-key my/org-mode-map (kbd "l") 'org-shiftright)
(define-key my/org-mode-map (kbd "h") 'org-shiftleft)

(define-key my/org-mode-map (kbd "P") 'org-shiftmetaup)
(define-key my/org-mode-map (kbd "N") 'org-shiftmetadown)
(define-key my/org-mode-map (kbd "L") 'org-shiftmetaright)
(define-key my/org-mode-map (kbd "H") 'org-shiftmetaleft)

(define-key my/org-mode-map (kbd "|") 'org-table-create-or-convert-from-region)

(define-key my/org-mode-map (kbd "z") 'org-shifttab)

(define-key my/org-mode-map (kbd "f") 'my/org-present-next)
(define-key my/org-mode-map (kbd "b") 'my/org-present-prev)

(define-key my/org-mode-map (kbd "i") 'org-toggle-inline-images)

(define-key my/org-mode-map (kbd "d") 'org-deadline)

;; Rebind tab to be yas-expand due to bugs with org-cycle when expanding snippets
(define-key org-mode-map "\t" 'yas-expand)

;; *** Show map
(define-prefix-command 'my/org-show-mode-map)
;; (define-key my/org-mode-map (kbd "s") 'my/org-show-mode-map)

(define-key my/org-mode-map (kbd "s") 'org-toggle-link-display)

;; *** Disable syntax highlighting in source code blocks
(setq org-src-fontify-natively nil)

;; * Outline
;; Must be set before outline is loaded
;; Required by outorg
(defvar outline-minor-mode-prefix "\M-#")

(straight-use-package 'outline)
;; (require 'outorg)

(add-hook 'prog-mode-hook 'outline-minor-mode)

;; ** Imenu
(define-key my/leader-map (kbd "I") 'counsel-imenu)

;; ** Counsel-outline
(define-key my/leader-map (kbd "TAB") 'counsel-outline)

;; ***  Fix so that counsel-outline can unfold to the line it needs to go to
(defun counsel-outline-action (x)
  "Go to outline X."
  (goto-char (cdr x))
  (outline-show-entry))

;; *** Fix counsel-outline in elisp mode
;; Elisp mode uses the classic lisp outline syntax
(setq counsel-outline-settings
      '((emacs-lisp-mode
	 :outline-regexp ";; [*]\\{1,8\\} "
	 :outline-level counsel-outline-level-emacs-lisp)
	(org-mode
	 :outline-title counsel-outline-title-org
	 :action counsel-org-goto-action
	 :history counsel-org-goto-history
	 :caller counsel-org-goto)
	;; markdown-mode package
	(markdown-mode
	 :outline-title counsel-outline-title-markdown)
	;; Built-in mode or AUCTeX package
	(latex-mode
	 :outline-title counsel-outline-title-latex)))

;; ** Outshine
(straight-use-package 'outshine)
;; (straight-use-package '(outshine :type git :host github :repo "alphapapa/outshine"))
(require 'outshine)

(add-hook 'outline-minor-mode-hook 'outshine-mode)

(setq outshine-startup-folded-p nil)

;; ** Outorg
(require 'outorg)
(setq outorg-edit-buffer-persistent-message nil)
(setq outorg-unindent-active-source-blocks-p nil)

;; *** Toggle current heading
(define-key my/leader-map (kbd "f") 'my/outorg-toggle-heading)

(defun my/outorg-toggle-heading ()
  (interactive)
  (if (string= major-mode 'org-mode)
      (outorg-copy-edits-and-exit)
    (outorg-edit-as-org)))

;; *** Toggle entire buffer
(define-key my/leader-map (kbd "F") 'my/outorg-toggle)

(defun my/outorg-toggle ()
  (interactive)
  (if (string= major-mode 'org-mode)
      (outorg-copy-edits-and-exit)
    (outorg-edit-as-org '(4))))

;; *** Export
(defun my/outorg-export-to-org-file (&optional name)
  (interactive)
  (let ((buffer (generate-new-buffer "outorg-org-output"))
	(mode major-mode))
    (copy-to-buffer buffer (point-min) (point-max))
    (switch-to-buffer buffer)
    (funcall mode)
    (outorg-convert-to-org)
    (if name
	(write-file name)
      (save-buffer))
    (kill-buffer)))

;; ** Visuals
(setq counsel-outline-face-style nil)

(set-face-attribute 'outshine-level-1 nil :inherit 'outline-1) ;;:height my/org-level-1-height)
(set-face-attribute 'outshine-level-2 nil :inherit 'outline-2) ;;:height my/org-level-2-height)
(set-face-attribute 'outshine-level-3 nil :inherit 'outline-3) ;;:height my/org-level-3-height)
(set-face-attribute 'outshine-level-4 nil :inherit 'outline-4) ;;:height my/org-level-4-height)
(set-face-attribute 'outshine-level-5 nil :inherit 'outline-5) ;;:height my/org-level-5-height)
(set-face-attribute 'outshine-level-6 nil :inherit 'outline-6) ;;:height my/org-level-6-height)
(set-face-attribute 'outshine-level-7 nil :inherit 'outline-7) ;;:height my/org-level-7-height)
(set-face-attribute 'outshine-level-8 nil :inherit 'outline-8) ;;:height my/org-level-8-height)

;; ** Java outline
;; (setq outline-regexp "\\(?:\\([ \t]*.*\\(class\\|interface\\)[ \t]+[a-zA-Z0-9_]+[ \t\n]*\\({\\|extends\\|implements\\)\\)\\|[ \t]*\\(public\\|private\\|static\\|final\\|native\\|synchronized\\|transient\\|volatile\\|strictfp\\| \\|\t\\)*[ \t]+\\(\\([a-zA-Z0-9_]\\|\\( *\t*< *\t*\\)\\|\\( *\t*> *\t*\\)\\|\\( *\t*, *\t*\\)\\|\\( *\t*\\[ *\t*\\)\\|\\(]\\)\\)+\\)[ \t]+[a-zA-Z0-9_]+[ \t]*(\\(.*\\))[ \t]*\\(throws[ \t]+\\([a-zA-Z0-9_, \t\n]*\\)\\)?[ \t\n]*{\\)" )

;; ** Narrowing
(define-prefix-command 'my/narrow-map)
(define-key my/leader-map (kbd "n") 'my/narrow-map)

(defun my/narrow-widen ()
  (interactive)
  (if loccur-mode
      (loccur-mode -1)
    (widen)))

(define-key my/narrow-map (kbd "w") 'my/narrow-widen)
(define-key my/narrow-map (kbd "r") 'narrow-to-region)

(define-key my/narrow-map (kbd "p") 'narrow-to-page)
(define-key my/narrow-map (kbd "d") 'narrow-to-defun)

(define-key my/narrow-map (kbd "i") 'my/auto-narrow-to-subtree)

;; *** Narrow to subtree
(defun my/auto-narrow-to-subtree ()
  (interactive)
  (pcase major-mode
    ('org-mode (org-narrow-to-subtree))
    (_
     (outline-previous-visible-heading 1)
     (outshine-narrow-to-subtree))))

;; ** Outline ellipsis
(defvar outline-display-table (make-display-table))
(set-display-table-slot outline-display-table 'selective-display
			(vector (make-glyph-code my/fold-ellipsis-char 'escape-glyph)))
(defun set-outline-display-table ()
  (setf buffer-display-table outline-display-table))

(add-hook 'outline-mode-hook 'set-outline-display-table)
(add-hook 'outline-minor-mode-hook 'set-outline-display-table)

;; ** Keys
;; *** Outline fold
(my/evil-normal-define-key "g i" 'outline-previous-visible-heading)

(my/evil-normal-define-key "g o" 'outline-toggle-children)
(my/evil-normal-define-key "g O" 'outline-show-subtree)
(my/evil-normal-define-key "g h" 'my/outline-hide-all-headings)
(my/evil-normal-define-key "g H" 'my/outline-hide-all)

(my/evil-normal-define-key "g a" 'outline-show-all)

(defun my/outline-hide-all ()
  (interactive)
  (outline-hide-sublevels 1))

(defun my/outline-hide-all-headings ()
  (interactive)
  (outline-show-all)
  (outline-hide-body))

;; *** Code fold
(my/evil-normal-define-key "g C-o" 'my/code-fold-show)
(my/evil-normal-define-key "g RET" 'my/code-fold-show-all)
(my/evil-normal-define-key "g C-h" 'my/code-fold-hide-level)

(defun my/code-fold-show ()
  (interactive)
  (if hs-minor-mode
      (hs-toggle-hiding)
    (yafolding-toggle-element)))

(defun my/code-fold-show-all ()
  (interactive)
  (if hs-minor-mode
      (hs-show-all)
    (yafolding-show-all)))

(defun my/code-fold-hide-level ()
  (interactive)
  (if hs-minor-mode
      (call-interactively 'hs-hide-level)
    (yafolding-hide-all)))

;; ** Folding
;; *** Hideshow
(require 'hideshow)

(defvar my/hs-ignore-modes '(fsharp-mode))

(add-hook 'prog-mode-hook '(lambda () (interactive)
			     (if (not (member major-mode my/hs-ignore-modes))
				 (hs-minor-mode 1))))

;; *** Yafolding
;; Used for universal folding
(straight-use-package 'yafolding)

(define-globalized-minor-mode global-yafolding-mode
  yafolding-mode yafolding-mode)
(yafolding-mode)
(global-yafolding-mode 1)

(setq yafolding-ellipsis-content my/fold-ellipsis)
(setq yafolding-show-fringe-marks nil)

;; *** Origami
;; (straight-use-package 'origami)

;; (global-origami-mode)

;; (setq origami-show-fold-header t)

;; **** Visuals
;; (setq origami-fold-replacement my/fold-ellipsis)

;;   (my/evil-normal-define-key "g C-o" 'my/code-fold-show)

;;   (my/evil-normal-define-key "g C-a" 'my/code-fold-hide-level)
;;   (my/evil-normal-define-key "g C-A" 'my/code-fold-show-all)

;;   (my/evil-normal-define-key "g C-h" 'my/code-fold-hide)

;;   (defun my/code-fold-show ()
;;     (interactive)
;;     (if hs-minor-mode
;;         (hs-show-block)
;;       (yafolding-show-element)))

;;   (defun my/code-fold-show-all ()
;;     (interactive)
;;     (if hs-minor-mode
;;         (hs-show-all)
;;       (yafolding-show-all)))

;;   (defun my/code-fold-hide-level ()
;;     (interactive)
;;     (if hs-minor-mode
;;         (call-interactively 'hs-hide-level)
;;       (yafolding-hide-all)))

;;   (defun my/code-fold-hide ()
;;     (interactive)
;;     (if hs-minor-mode
;;         (hs-hide-block)
;;       (yafolding-hide-element)))
;; * Completion
;; ** Ivy
(straight-use-package 'ivy)
(ivy-mode 1)

(setq ivy-use-virtual-buffers nil)

;; Make user intput selectable
(setq ivy-use-selectable-prompt t)

;; *** Visuals
;;;;;; Ivy height
(setq ivy-height 20)

;; Make counsel-yank-pop use default height
(delete `(counsel-yank-pop . ,ivy-height) ivy-height-alist)

;; **** Highlight whole row in minibuffer
;; Change the default emacs formatter to highlight whole row in minibuffer
(delete '(t . ivy-format-function-default) ivy-format-functions-alist)
(add-to-list 'ivy-format-functions-alist '(t . ivy-format-function-line))

;; *** Wgrep
;; Needed by ivy-occur to edit buffers
(straight-use-package 'wgrep)
(require 'wgrep)

;; *** Keys
(defun my/ivy-top ()
  (interactive)
  (ivy-previous-line ivy--length))

(defun my/ivy-bot ()
  (interactive)
  (ivy-next-line ivy--length))

(setq ivy-minibuffer-map (make-sparse-keymap))

;; Enable avy movements in ivy buffer
(evil-define-key '(motion normal) ivy-minibuffer-map (kbd "M-n") 'ivy-avy)
(evil-define-key '(motion normal) ivy-minibuffer-map (kbd "M-p") 'ivy-avy)

(evil-define-key '(motion normal) ivy-minibuffer-map (kbd "G") 'my/ivy-bot)
(evil-define-key '(motion normal) ivy-minibuffer-map (kbd "g g") 'my/ivy-top)

(evil-define-key '(motion normal) ivy-minibuffer-map (kbd "n") 'ivy-next-line)
(evil-define-key '(motion normal) ivy-minibuffer-map (kbd "p") 'ivy-previous-line)

(evil-define-key 'insert ivy-minibuffer-map (kbd "C-n") 'ivy-next-line)
(evil-define-key 'insert ivy-minibuffer-map (kbd "C-p") 'ivy-previous-line)

(evil-define-key '(motion normal) ivy-minibuffer-map (kbd "<escape>") 'ivy-call)

(define-key ivy-minibuffer-map [remap evil-ret] 'ivy-done)
(define-key ivy-minibuffer-map [remap newline] 'ivy-done)

(evil-define-key '(motion normal insert) ivy-minibuffer-map (kbd "C-g") 'minibuffer-keyboard-quit)

(evil-define-key '(motion normal insert) ivy-minibuffer-map (kbd "C-u") 'ivy-scroll-down-command)
(evil-define-key '(motion normal insert) ivy-minibuffer-map (kbd "C-w") 'ivy-scroll-up-command)

(evil-define-key '(motion normal insert) ivy-minibuffer-map (kbd "C-o") 'ivy-occur)

;; Doesn't work with dedicated minibuffer window?
;; evil-define-key '(motion normal) ivy-minibuffer-map  (kbd "C-y") 'ivy-dispatching-done)
;; define-key 'insert ivy-minibuffer-map  (kbd "C-y") 'ivy-dispatching-done)

(evil-define-key '(motion normal insert) ivy-minibuffer-map (kbd "C-s") 'ivy-next-line-or-history)

(evil-define-key '(motion normal insert) ivy-minibuffer-map (kbd "TAB") 'ivy-partial-or-done)

(evil-define-key '(motion normal insert) ivy-minibuffer-map (kbd "DEL") 'ivy-backward-delete-char)

(evil-define-key '(motion normal insert) ivy-minibuffer-map (kbd "RET") 'ivy-done)

(evil-define-key '(motion normal insert) ivy-minibuffer-map (kbd "C-d") 'ivy-insert-current)

(evil-define-key '(motion normal insert) ivy-minibuffer-map (kbd "C-f") 'ivy-backward-delete-char)

;; Clear ivy input
(evil-define-key '(motion normal) ivy-minibuffer-map (kbd "D") '(lambda () (interactive) (beginning-of-line-text)
								  (evil-delete-char (+ 1 (point)) (point-max))
								  (delete-char 1)))

;; **** Ivy occur
;; Also ivy-occur-grep
(define-prefix-command 'my/ivy-occur-map)
(evil-define-key 'normal ivy-occur-mode-map (kbd (concat my/leader-map-key " a")) 'my/ivy-occur-map)
(evil-define-key 'normal ivy-occur-grep-mode-map (kbd (concat my/leader-map-key " a")) 'my/ivy-occur-map)

(define-key my/ivy-occur-map (kbd "w") 'ivy-wgrep-change-to-wgrep-mode)
(define-key my/ivy-occur-map (kbd "r") 'ivy-occur-revert-buffer)

(evil-define-key '(normal visual insert) ivy-occur-mode-map (kbd "RET") 'ivy-occur-press)
(evil-define-key '(normal visual) ivy-occur-mode-map (kbd "p") 'evil-previous-line)
(evil-define-key '(normal visual) ivy-occur-mode-map (kbd "n") 'evil-next-line)
(evil-define-key '(normal visual) ivy-occur-mode-map (kbd "C-y") 'ivy-occur-read-action)

(evil-define-key '(normal visual insert) ivy-occur-grep-mode-map (kbd "RET") 'ivy-occur-press)
(evil-define-key '(normal visual) ivy-occur-grep-mode-map (kbd "p") 'evil-previous-line)
(evil-define-key '(normal visual) ivy-occur-grep-mode-map (kbd "n") 'evil-next-line)
(evil-define-key '(normal visual) ivy-occur-grep-mode-map (kbd "C-y") 'ivy-occur-read-action)

;; (define-key map (kbd "a") 'ivy-occur-read-action)
;; (define-key map (kbd "o") 'ivy-occur-dispatch)
;; (define-key map (kbd "c") 'ivy-occur-toggle-calling)

;; **** wgrep
;; Used in ivy occur
(setq wgrep-mode-map (make-sparse-keymap))

(define-prefix-command 'my/wgrep-map)
(evil-define-key 'normal wgrep-mode-map (kbd (concat my/leader-map-key " a")) 'my/wgrep-map)

(define-key my/wgrep-map (kbd "s") 'wgrep-finish-edit)
(define-key my/wgrep-map (kbd "k") 'wgrep-abort-changes)

;; ** Counsel
(straight-use-package 'counsel)

(counsel-mode 1)

;; (setq-default counsel-grep-base-command "rg -i -M 120 --no-heading --line-number --color never '%s' %s")
(setq counsel-grep-base-command "grep -i -E -n -e %s %s")

;; *** Counsel-yank-pop
;; Delete text under selection when pasing just like with normal evil paste
(advice-add #'counsel-yank-pop :before (lambda (&optional arg) (if (string= evil-state 'visual)
								   (delete-region (point) (mark)))))

;; *** Always run counsel ag in defalut directory
(defun my/counsel-ag ()
  (interactive)
  (counsel-ag nil default-directory))

;; *** Keys
(global-set-key (kbd "M-x") 'counsel-M-x)
(global-set-key (kbd "<f1> f") 'counsel-describe-function)
(global-set-key (kbd "<f1> v") 'counsel-describe-variable)
(global-set-key (kbd "<f1> l") 'counsel-find-library)
(global-set-key (kbd "<f2> i") 'counsel-info-lookup-symbol)
(global-set-key (kbd "<f2> u") 'counsel-unicode-char)

(global-set-key (kbd "M-k") 'counsel-yank-pop)

;; ** Counsel flycheck
;;   https://github.com/nathankot/dotemacs/blob/master/init.el

(defvar my/counsel-flycheck-history nil
  "History for `counsel-flycheck'")

(defun my/counsel-flycheck ()
  (interactive)
  (if (not (bound-and-true-p flycheck-mode))
      (message "Flycheck mode is not available or enabled")
    (ivy-read "Error: "
	      (let ((source-buffer (current-buffer)))
		(with-current-buffer (or (get-buffer flycheck-error-list-buffer)
					 (progn
					   (with-current-buffer
					       (get-buffer-create flycheck-error-list-buffer)
					     (flycheck-error-list-mode)
					     (current-buffer))))
		  (flycheck-error-list-set-source source-buffer)
		  (flycheck-error-list-reset-filter)
		  (revert-buffer t t t)
		  (split-string (buffer-string) "\n" t " *")))
	      :action (lambda (s &rest _)
			(-when-let* ( (error (get-text-property 0 'tabulated-list-id s))
				      (pos (flycheck-error-pos error)) )
			  (goto-char (flycheck-error-pos error))))
	      :history 'my/counsel-flycheck-history)))

(define-key my/leader-map (kbd "J") 'my/counsel-flycheck)

;; ** Swiper
(straight-use-package 'swiper)

(defun my/use-swiper-or-grep(&optional input case-sensative)
  (interactive)
  (swiper input))
;; (if (and buffer-file-name (not (bound-and-true-p org-src-mode)) (not (string= "gz" (file-name-extension buffer-file-name))))
;; (counsel-grep input)
;; (swiper input)))

;; Checks for if case sensative search
;; (if case-sensative
;; (setq counsel-grep-base-command "grep -E -n -e %s %s")
;; (setq-default counsel-grep-base-command "grep -i -E -n -e %s %s"))

(global-set-key (kbd "C-s") 'my/use-swiper-or-grep)
(global-set-key (kbd "C-s") 'my/use-swiper-or-grep)
;; (global-set-key (kbd "M-s") (lambda () (interactive) (my/use-swiper-or-grep nil t)))

;;  (setq swiper-use-visual-line t)

;; *** Search for thing-at-point
(defun my/swiper-thing-at-point ()
  "jump to word under cursor"
  (interactive)
  (my/use-swiper-or-grep (thing-at-point 'symbol)))

(my/evil-normal-define-key "#" 'my/swiper-thing-at-point)
(my/evil-normal-define-key "*" 'my/swiper-thing-at-point)

;; ** Ivy rich
(straight-use-package 'ivy-rich)
(require 'ivy-rich)
(ivy-rich-mode 1)

;; (ivy-set-display-transformer 'ivy-switch-buffer 'ivy-switch-buffer)
;; (setq ivy-rich-path-style 'abbrev)
'(ivy-switch-buffer
  (:columns
   ((ivy-rich-candidate (:width 30))  ; return the candidate itself
    (ivy-rich-switch-buffer-size (:width 7))  ; return the buffer size
    (ivy-rich-switch-buffer-indicators (:width 4 :face error :align right)); return the buffer indicators
    (ivy-rich-switch-buffer-major-mode (:width 12 :face warning))          ; return the major mode info
    (ivy-rich-switch-buffer-project (:width 15 :face success))             ; return project name using `projectile'
    (ivy-rich-switch-buffer-path (:width (lambda (x) (ivy-rich-switch-buffer-shorten-path x (ivy-rich-minibuffer-width 0.3))))))  ; return file path relative to project root or `default-directory' if project is nil
   :predicate
   (lambda (cand) (get-buffer cand)))
  counsel-M-x
  (:columns
   ((counsel-M-x-transformer (:width 40))  ; thr original transfomer
    (ivy-rich-counsel-function-docstring (:face font-lock-doc-face))))  ; return the docstring of the command
  counsel-describe-function
  (:columns
   ((counsel-describe-function-transformer (:width 40))  ; the original transformer
    (ivy-rich-counsel-function-docstring (:face font-lock-doc-face))))  ; return the docstring of the function
  counsel-describe-variable
  (:columns
   ((counsel-describe-variable-transformer (:width 40))  ; the original transformer
    (ivy-rich-counsel-variable-docstring (:face font-lock-doc-face))))  ; return the docstring of the variable
  counsel-recentf
  (:columns
   ((ivy-rich-candidate (:width 0.8)) ; return the candidate itself
    (ivy-rich-file-last-modified-time (:face font-lock-comment-face))))) ; return the last modified time of the file

;; ** Company
(straight-use-package 'company)
(require 'company)

(setq company-idle-delay 0.1)
(setq company-echo-delay 0)

;; Don't downcase result
(setq company-dabbbrev-downcase nil)

;; Make tooltim margin minimal
(setq company-tooltip-margin 2)

;; Start searching for candidates when 2 letters has been written
(setq company-minimum-prefix-length 2)

(add-to-list 'company-transformers 'company-sort-prefer-same-case-prefix)

(setq company-show-numbers t)

;; Make sure only 10 candidates are shown at a time
(setq company-tooltip-limit 10)

;; Align annotations to right side
(setq company-tooltip-align-annotations t)

;; Makes it possible to exit company without a candidate selected
(setq company-require-match nil)

;; Enable scrollbar
(setq company-tooltip-offset-display 'scrollbar) ;;'line

(global-company-mode t)

;; Remove dabbrev because evil has a better alternative and dabbrev is slow with long files
(setq company-backends (delete 'company-dabbrev company-backends))

;; *** Company posframe
;; (straight-use-package 'company-posframe)
;; (company-posframe-mode)

;; *** Company doc buffer
;; Company doc mode disables visual line mode for whatever reason, enable it inside this redefinition of company-show-doc-buffer
(defun my/company-show-doc-buffer ()
  "Temporarily show the documentation buffer for the selection."
  (interactive)
  (let (other-window-scroll-buffer)
    (company--electric-do
      (let* ((selected (nth company-selection company-candidates))
	     (doc-buffer (or (company-call-backend 'doc-buffer selected)
			     (user-error "No documentation available")))
	     start)
	(setq-local truncate-lines nil)
	(when (consp doc-buffer)
	  (setq start (cdr doc-buffer)
		doc-buffer (car doc-buffer)))
	(setq other-window-scroll-buffer (get-buffer doc-buffer))
	(let ((win (display-buffer doc-buffer t)))
	  (set-window-start win (if start start (point-min))))))))

(define-key company-active-map (kbd "C-o") 'my/company-show-doc-buffer)

(defun my/company-show-doc-buffer-keep-open ()
  "Temporarily show the documentation buffer for the selection."
  (interactive)
  (let (other-window-scroll-buffer)
    (let* ((selected (nth company-selection company-candidates))
	   (doc-buffer (or (company-call-backend 'doc-buffer selected)
			   (user-error "No documentation available")))
	   start)
      (setq-local truncate-lines nil)
      (when (consp doc-buffer)
	(setq start (cdr doc-buffer)
	      doc-buffer (car doc-buffer)))
      (setq other-window-scroll-buffer (get-buffer doc-buffer))
      (let ((win (display-buffer doc-buffer t)))
	(set-window-start win (if start start (point-min)))))))

(define-key company-active-map (kbd "M-o") 'my/company-show-doc-buffer-keep-open)

;; *** Company-show-numbers but with letters
;; Need to implement
;; Letters, etc for autocomplete
;; line 2769, might also need to change more lines due to "company show numbers" being at a few more places
;;  (setq right (concat (format " %s" (nth numbered '(a s d f g h j k l i r))) right)))


;; **** Find function that gets hotkey
;; name "company-complete-number"

;; *** Disable quickhelp for good
;; fsharp mode auto-enables quickhelp by default, disable it
(setq company-quickhelp-delay nil)

;; *** Visuals
;; Make company mode inherit colors from theme, change later maybe
(require 'color)

;; Compatibility with 16 color terminals
;; (if (not (string= (face-attribute 'default :background) "unspecified-bg"))
;; (let* ((my/background-color (face-attribute 'default :background)))
;; (set-face-attribute 'company-scrollbar-bg nil :background (color-lighten-name my/background-color 10))
;; (set-face-attribute 'company-scrollbar-fg nil :background (color-lighten-name my/background-color 5))

;; ;; Selected entry
;; (set-face-attribute 'company-tooltip-selection nil :background (face-attribute 'font-lock-function-name-face :background) :foreground  (face-attribute 'font-lock-function-name-face :foreground))
;; ;; All unmatching text
;; (set-face-attribute 'company-tooltip nil :foreground (face-attribute 'default :foreground) :background (color-lighten-name my/background-color 10))
;; ;; All matching text
;; (set-face-attribute 'company-tooltip-common nil :foreground (face-attribute 'font-lock-constant-face :foreground) :background (face-attribute 'font-lock-constant-face :background)))
;; (set-face-attribute 'company-scrollbar-bg nil :background "black")
;; (set-face-attribute 'company-scrollbar-fg nil :background "white")

;; ;; Selected entry
;; (set-face-attribute 'company-tooltip-selection nil :background "black" :foreground "red")
;; ;; All unmatching text
;; (set-face-attribute 'company-tooltip nil :foreground "white" :background "black")
;; ;; All matching text
;; (set-face-attribute 'company-tooltip-common nil :foreground "orange" :background "black"))

;; *** Keys
(define-key company-active-map (kbd "C-f") nil)

(define-key company-active-map (kbd "M-n") nil)
(define-key company-active-map (kbd "M-p") nil)
(define-key company-active-map (kbd "C-n") 'company-select-next)
(define-key company-active-map (kbd "C-p") 'company-select-previous)

(define-key company-active-map (kbd "C-u") 'company-previous-page)
(define-key company-active-map (kbd "C-w") 'company-next-page)

;; using C-h is better in every way
(define-key company-active-map (kbd "<f1>") 'nil)

;; Force autocomplete
(my/evil-universal-define-key "C-." 'company-complete)


;; ** Company-box
;; Company with icons
;; Doesn't work with my setup right now
;; (when window-system
;;  (straight-use-package 'company-box)

;;  (add-hook 'company-mode-hook 'company-box-mode)

;;  ;;(setq company-box-minimum-width 100)
;;  ;;(setq company-box--height 500)
;;  ;;(setq company-tooltip-minimum 10)

;;  (remove-hook 'company-box-selection-hook 'company-box-doc)
;;  (remove-hook 'company-box-hide-hook 'company-box-doc--hide))

;; ** Flycheck
(setq flycheck-mode-map (make-sparse-keymap))
(straight-use-package 'flycheck)

;; Decrease delay
(setq flycheck-display-errors-delay 0.1)

(global-flycheck-mode)

;; *** Disable flycheck fringe
(setq flycheck-indication-mode nil)

;; *** Flycheck at cursor
;; **** Flycheck posframe
;; (straight-use-package 'flycheck-posframe)

;; (with-eval-after-load 'flycheck
;;  (require 'flycheck-posframe)
;;  (add-hook 'flycheck-mode-hook #'flycheck-posframe-mode))

;; (setq-default flycheck-posframe-prefix ">")
;; (setq-default flycheck-posframe-warning-prefix ">")
;; (setq-default flycheck-posframe-info-prefix ">")
;; (setq-default flycheck-posframe-error-prefix ">")

;; **** Flycheck inline
(straight-use-package 'flycheck-inline)

(require 'flycheck-inline)

(with-eval-after-load 'flycheck
  (add-hook 'flycheck-mode-hook #'turn-on-flycheck-inline))

;; *** Flycheck-package
;; Flycheck with extra correction for elisp packages
(straight-use-package 'flycheck-package)

(eval-after-load 'flycheck
  '(flycheck-package-setup))

;; ** Which-key
(straight-use-package 'which-key)

(which-key-mode)

(setq which-key-idle-delay 1)

;; ** Yasnippet
(setq yas-minor-mode-map (make-sparse-keymap))

(straight-use-package 'yasnippet)
(require 'yasnippet)

(straight-use-package 'yasnippet-snippets)

(yas-global-mode 1)

;; *** Keys
(defvar my/yas-init nil)

(defun my/yas-insert-snippet ()
  (interactive)
  (when (not my/yas-init)
    (yas-reload-all)
    (setq my/yas-init t))
  (call-interactively 'yas-insert-snippet))

(define-key my/leader-map (kbd "i") 'my/yas-insert-snippet)

;; * Movement
;; ** Loccur
(straight-use-package 'loccur)
(require 'loccur)

(defvar-local my/loccur-search-running nil)

(defun my/loccur-isearch ()
  (interactive)
  (setq-local my/loccur-search-running t)
  (my/isearch-forward-regexp)
  (setq-local my/loccur-search-running nil))

(add-hook 'isearch-update-post-hook 'my/loccur-isearch-update)

(defun my/loccur-isearch-update ()
  (when my/loccur-search-running
    (loccur-mode -1)
    (if (not (string= isearch-string ""))
	(loccur (ivy--regex-plus isearch-string)))))

(add-hook 'isearch-mode-end-hook 'my/loccur-isearch-quit)

(defun my/loccur-isearch-quit ()
  (if (and my/loccur-search-running isearch-mode-end-hook-quit)
      (loccur-mode -1)))

;; *** Keys
(my/evil-normal-define-key "M-s" 'my/loccur-isearch)

;; ** Isearch
(require 'isearch)
(define-key isearch-mode-map (kbd "C-n") 'my/isearch-repeat-forward)
(define-key isearch-mode-map (kbd "C-p") 'my/isearch-repeat-backward)

(setq isearch-lazy-highlight t)
(setq lazy-highlight-initial-delay 0)

;; *** Stop from having to press C-n two times after pressing C-p
(defvar my/last-isearch-dir nil)

(defun my/isearch-repeat-forward ()
  (interactive)
  (call-interactively 'isearch-repeat-forward)
  
  (if (string= my/last-isearch-dir 'backward)
      (call-interactively 'isearch-repeat-forward))
  (setq my/last-isearch-dir 'forward))

(defun my/isearch-repeat-backward ()
  (interactive)
  (call-interactively 'isearch-repeat-backward)
  
  (if (string= my/last-isearch-dir 'forward)
      (call-interactively 'isearch-repeat-backward))
  (setq my/last-isearch-dir 'backward))

(add-hook 'isearch-mode-end-hook (lambda () (setq my/last-isearch-dir nil)))

;; *** Make isearch end when only one match
;; (defun my/ret-if-one-isearch-match ()
;;   (interactive)
;;   (set-buffer my/isearch-current-buffer)
;;   (save-excursion
;;     (goto-char (point-min))
;;     (if (= (count-matches isearch-string) 1)
;;         (setq unread-command-events (listify-key-sequence "\C-m")))))

;; (add-hook 'isearch-update-post-hook 'my/ret-if-one-isearch-match)


;; *** Replace isearch regexp search with ivy-regex-plus
(defun my/isearch-forward-regexp (&optional not-regexp no-recursive-edit)
  (interactive "P\np")
  (isearch-mode t (null not-regexp) nil (not no-recursive-edit) 'my/ivy--regex-plus))

(defun my/isearch-backward-regexp (&optional not-regexp no-recursive-edit)
  (interactive "P\np")
  (isearch-mode nil (null not-regexp) nil (not no-recursive-edit) 'my/ivy--regex-plus))

(defun my/ivy--regex-plus (string &optional lax)
  (ivy--regex-plus string))

;; *** Keys
;; Disable custom C-f key
(define-key isearch-mode-map (kbd "C-f") 'isearch-delete-char)

;; ** Goto middle of line
(defun my/go-to-middle-of-line ()
  (interactive)
  (goto-char (/  (+ (line-end-position) (line-beginning-position)) 2)))

(my/evil-normal-define-key "^" 'my/go-to-middle-of-line)

;; ** Navigate indent
;; https://emacs.stackexchange.com/questions/20900/navigate-by-indentation
(defun indentation-get-next-good-line (direction skip good)
  "Moving in direction `direction', and skipping over blank lines and lines that
  satisfy relation `skip' between their indentation and the original indentation,
  finds the first line whose indentation satisfies predicate `good'."
  (let ((starting-indentation (current-indentation))
	(lines-moved direction))
    (save-excursion
      (while (and (zerop (forward-line direction))
		  (or (eolp)  ; Skip past blank lines and other skip lines
		      (funcall skip (current-indentation) starting-indentation)))
	(setq lines-moved (+ lines-moved direction)))
      ;; Now we can't go further. Which case is it?
      (if (and
	   (not (eobp))
	   (not (bobp))
	   (funcall good (current-indentation) starting-indentation))
	  lines-moved
	nil))))

(defun indentation-get-next-sibling-line ()
  "The line number of the next sibling, if any."
  (indentation-get-next-good-line 1 '> '=))

(defun indentation-get-previous-sibling-line ()
  "The line number of the previous sibling, if any"
  (indentation-get-next-good-line -1 '> '=))

(defun indentation-get-parent-line ()
  "The line number of the parent, if any."
  (indentation-get-next-good-line -1 '>= '<))

(defun indentation-get-child-line ()
  "The line number of the first child, if any."
  (indentation-get-next-good-line +1 'ignore '>))

(defun indentation-move-to-line (func preserve-column name)
  "Move the number of lines given by func. If not possible, use `name' to say so."
  (let ((saved-column (current-column))
	(lines-to-move-by (funcall func)))
    (if lines-to-move-by
	(progn
	  (forward-line lines-to-move-by)
	  (move-to-column (if preserve-column
			      saved-column
			    (current-indentation))))
      (message "No %s to move to." name))))

(defun indentation-forward-to-next-sibling ()
  "Move to the next sibling if any, retaining column position."
  (interactive "@")
  (indentation-move-to-line 'indentation-get-next-sibling-line t "next sibling"))

(defun indentation-backward-to-previous-sibling ()
  "Move to the previous sibling if any, retaining column position."
  (interactive "@")
  (indentation-move-to-line 'indentation-get-previous-sibling-line t "previous sibling"))

(defun indentation-up-to-parent ()
  "Move to the parent line if any."
  (interactive "@")
  (indentation-move-to-line 'indentation-get-parent-line nil "parent"))

(defun indentation-down-to-child ()
  "Move to the first child line if any."
  (interactive "@")
  (indentation-move-to-line 'indentation-get-child-line nil "child"))

;; *** Keys
(my/evil-normal-define-key "C-n" 'indentation-forward-to-next-sibling)
(my/evil-visual-define-key "C-n" 'indentation-forward-to-next-sibling)

(my/evil-normal-define-key "C-p" 'indentation-backward-to-previous-sibling)
(my/evil-visual-define-key "C-p" 'indentation-backward-to-previous-sibling)

(my/evil-normal-define-key "C-h" 'indentation-up-to-parent)
(my/evil-visual-define-key "C-h" 'indentation-up-to-parent)

(my/evil-normal-define-key "C-l" 'indentation-down-to-child)
(my/evil-visual-define-key "C-l" 'indentation-down-to-child)

;; ** Marks
(setq mark-ring-max 100)

;; *** Bind counsel-mark-ring
(my/evil-universal-define-key "C-o" 'counsel-mark-ring)
(my/evil-universal-define-key "C-b" 'evil-jump-backward)
(my/evil-universal-define-key "M-b" 'evil-jump-forward)

;; ** Relative line numbers
;; (when (version<= "26.0.50" emacs-version )
;; (global-display-line-numbers-mode)

;; (setq display-line-numbers-type 'relative)
;; (setq-default display-line-numbers-type 'relative)
;; (setq display-line-numbers-current-absolute nil)
;; (setq-default display-line-numbers-current-absolute nil)

;; ;; Fixes org headings not
;; ;;(add-hook 'org-mode-hook (lambda ()(interactive) (setq-local display-line-numbers-type 'visual)))
;; (add-hook 'outline-mode-hook (lambda ()(interactive) (setq-local display-line-numbers-type 'visual)))
;; (add-hook 'outline-minor-mode-hook (lambda ()(interactive) (setq-local display-line-numbers-type 'visual)))

;; (if window-system
;; (progn
;; (set-face-attribute 'line-number-current-line nil :family my/mono-font)
;; (set-face-attribute 'line-number nil :family my/mono-font))))

;; ** Avy
(straight-use-package 'avy)
(require 'avy)

(setq avy-all-windows nil)

(setq avy-keys '(
		 ;; Easy
		 ?a ?n ?e ?t ?o ?s ?h ?d ?i ?r
		 ;; Med
		 ?g ?m ?l ?w ?y ?f ?u ?b ?x ?c ?v ?k ?p ?, ?.
		 ;; Hard
		 ?q ?\; ?j ?\/ ?z
		 
		 ;; Shifted
		 
		 ;; Easy
		 ?A ?N ?E ?T ?O ?S ?H ?D
		 ;; Med
		 ?R ?I ?G ?M ?L ?W ?Y ?F ?U ?B ?X ?C ?V ?K ?P
		 ;; Hard
		 ?Q ?\: ?J ?\? ?Z
		 
		 ;; Digits
		 ?7 ?4 ?8 ?3 ?9 ?2 ?0 ?1
		 ))

;; Disable highlighting when avy is used. Doesn't work on 16 color terminals
(if window-system (setq avy-background t))

;; *** Avy-goto-line
;; **** Above
(defun my/avy-goto-line-above-keep-horizontal-position (&optional arg)
  (interactive "p")
  (setq cursor-horizontal-pos (current-column))
  
  ;; Fixes problem with goto-line and visual line mode
  (if (eq evil-state 'visual)
      (if (eq (evil-visual-type) 'line)
	  (progn
	    (setq was-visual-line t)
	    (evil-visual-char)))
    (progn
      (setq was-visual-line nil)
      (my/evil-normal-state arg)))
  
  (avy-goto-line-above 2 t)
  
  (if (eq was-visual-line t)
      (evil-visual-line))
  
  (move-to-column cursor-horizontal-pos))

;; **** Below
(defun my/avy-goto-line-below-keep-horizontal-position (&optional arg)
  (interactive "p")
  (setq cursor-horizontal-pos (current-column))
  
  ;; Fixes problem with goto-line and visual line mode
  (if (eq evil-state 'visual)
      (if (eq (evil-visual-type) 'line)
	  (progn
	    (setq was-visual-line t)
	    (evil-visual-char)))
    (progn
      (setq was-visual-line nil)
      (my/evil-normal-state arg)))
  
  (avy-goto-line-below 2)
  
  (if (eq was-visual-line t)
      (evil-visual-line))
  
  (move-to-column cursor-horizontal-pos))

;; *** Avy-goto-word
(defun my/avy-goto-word-0-in-line(&optional arg)
  (interactive "p")
  (if (not (eq evil-state 'visual))
      (my/evil-normal-state arg))
  
  (avy-goto-subword-0 t nil (line-beginning-position) (line-end-position)))

;; *** Avy-goto-subword-0
;; **** Below
(defun my/avy-goto-subword-0-below()
  (interactive)
  (avy-goto-subword-0 t nil (point) (window-end (selected-window) t)))

;; **** Above
(defun my/avy-goto-subword-0-above()
  (interactive)
  (avy-goto-subword-0 t nil (window-start (selected-window)) (point)))

;; ** Avy flycheck
(straight-use-package 'avy-flycheck)

(define-key my/leader-map (kbd "j") 'avy-flycheck-goto-error)

;; ** Link-hint
(straight-use-package 'link-hint)

;; ** Scroll
(my/evil-universal-define-key "C-u" 'evil-scroll-up)
(my/evil-universal-define-key "C-w" 'evil-scroll-down)

;; *** On-screen
;; (straight-use-package 'on-screen)
;; (on-screen-global-mode)
;; (setq on-screen-highlight-method 'fringe)

;; ** Jammer
;; (straight-use-package 'jammer)

;; (setq jammer-repeat-delay 0.5)
;; (setq jammer-repeat-window 1)

;; (setq jammer-type 'repeat)
;; (setq jammer-block-type 'blacklist)
;; (setq jammer-block-list '(
;; ;; Backward/forward
;; evil-backward-char evil-forward-char evil-previous-line evil-next-line previous-line next-line
;; ;; Dired
;; dired-next-line dired-previous-line



;; ;; word movements
;; evil-forward-word evil-forward-word-begin evil-forward-word-end evil-backward-word-begin

;; ;; WORD movements
;; evil-forward-WORD evil-forward-WORD-begin evil-forward-WORD-end evil-backward-WORD-begin

;; evil-backward-word-begin evil-backward-word-end))
;; (jammer-mode)

;; ** goto change
;; g-; and g-,
(straight-use-package 'goto-chg)

;; ** My find file
(defun my/find-file ()
  (interactive)
  (find-file (let ((dir (ignore-errors (dired-current-directory))))
	       (if dir
		   (read-file-name "Find file: " dir)
		 (read-file-name "Find file: " default-directory)))))

;; ** Change default directory
(defun my/change-default-directory ()
  (interactive)
  (let ((dir (read-file-name "Change default dir: ")))
    (if (f-dir-p dir)
	(setq default-directory dir))))

;; ** Keys
;; (my/evil-normal-define-key "M-f" 'avy-goto-char-in-line)
;; (define-key my/leader-map (kbd "f") 'avy-goto-char-in-line)

(my/evil-normal-define-key "M-w" 'my/avy-goto-word-0-in-line)
(define-key my/leader-map (kbd "w") 'my/avy-goto-word-0-in-line)

(my/evil-normal-define-key "M-g" 'avy-goto-char-2)
(define-key my/leader-map (kbd "g") 'avy-goto-char-2)

(my/evil-normal-define-key "M-g" 'avy-goto-char-2)
(define-key my/leader-map (kbd "g") 'avy-goto-char-2)

;; (my/evil-normal-define-key "M-n" 'avy-goto-word-0-below)
;;   (define-key evil-normal-state-map (kbd "M-p") 'avy-goto-word-0-above)

;; (define-key my/leader-map (kbd "n") 'my/avy-goto-line-below-keep-horizontal-position)
;; (define-key my/leader-map (kbd "p") 'my/avy-goto-line-above-keep-horizontal-position)

;; (define-key my/keys-mode-map (kbd "M-l") 'link-hint-open-link)
(define-key my/leader-map (kbd "l") 'link-hint-open-link)
;; (define-key my/keys-mode-map (kbd "M-???") 'link-hint-copy-link)

;; * Bookmark management
(defun my/select-bookmark (message)
  (ivy-read message (bookmark-all-names)))

(defun my/add-bookmark ()
  (interactive)
  (bookmark-set (my/select-bookmark "Add bookmark ")))

(defun my/delete-bookmark ()
  (interactive)
  (bookmark-delete (my/select-bookmark "Delete bookmark ")))

;; doesn't work
;; (defun my/load-bookmark-file ()
;; (interactive)
;; (bookmark-load (ivy-read "load bookmark file " nil)))

;; * Window management
(defvar my/window-config-name-changed-hook nil
  "Hook called after user has loaded a window configuration")

(defvar my/window-configurations nil)
(defvar my/current-window-configuration "None")

(defun my/get-window-config-names ()
  (mapcar #'car my/window-configurations))

(defun my/select-window-config (message)
  (ivy-read message (my/get-window-config-names)))

(defun my/get-selected-window-config-position (selected-config)
  (cl-position selected-config (my/get-window-config-names) :test 'string=))

(defun my/update-current-window-config ()
  (setq my/current-window-configuration my/selected-window-config)
  (run-hooks 'my/window-config-name-changed-hook))

(defun my/add-window-config ()
  (interactive)
  (setq my/selected-window-config (my/select-window-config "Add window config "))
  
  (setq my/selected-window-config-position (my/get-selected-window-config-position my/selected-window-config))
  
  (if (eq my/selected-window-config-position nil)
      (push (list my/selected-window-config (current-window-configuration)) my/window-configurations)
    (setf (nth my/selected-window-config-position my/window-configurations) (list my/selected-window-config (current-window-configuration))))
  
  (my/update-current-window-config))

(defun my/load-window-config ()
  (interactive)
  (setq my/selected-window-config (my/select-window-config "Load window config "))
  (set-window-configuration (nth 1 (nth (my/get-selected-window-config-position my/selected-window-config) my/window-configurations)))
  
  (my/update-current-window-config))

(defun my/delete-window-config ()
  (interactive)
  (setq my/window-configurations (delete (nth (my/get-selected-window-config-position (my/select-window-config "Delete window config ")) my/window-configurations) my/window-configurations)))

;; ** Winner-mode
;; Remove default keys
(setq winner-mode-map (make-sparse-keymap))

(require 'winner)

(winner-mode)

;; ** No more window resetting
;; (defmacro save-window-excursion (&rest body)
;;  )

;; ** Switch to minibuffer
(defun my/toggle-switch-to-minibuffer ()
  "Switch to minibuffer window."
  (interactive)
  (if (active-minibuffer-window)
      (progn
	(if (string= major-mode "minibuffer-inactive-mode")
	    (progn
	      (message "HERE")
	      (select-window (previous-window)))
	  (select-window (active-minibuffer-window))))
    (error "Minibuffer is not active")))

(define-key my/keys-mode-map (kbd "C-j") 'my/toggle-switch-to-minibuffer)
(my/evil-universal-define-key "C-j" 'my/toggle-switch-to-minibuffer)

;; * Window and buffer settings
;; ** Delete other windows
(defun my/delete-other-windows()
  (interactive)
  (delete-other-windows)
  (my/lv-line-create)
  (run-hooks 'my/switch-buffer-hook))

;; ** Switch window hook

;; ** Switch buffer hook
(defvar my/switch-buffer-hook nil
  "Hook called after user has switched buffer")
(add-hook 'window-configuration-change-hook (lambda () (interactive) (run-hooks 'my/switch-buffer-hook) t))
(add-hook 'minibuffer-exit-hook (lambda () (interactive) (run-with-timer 0.1 nil (lambda () (interactive) (run-hooks 'my/switch-buffer-hook)))))
(add-hook 'my/switch-window-hook (lambda () (interactive) (run-hooks 'my/switch-buffer-hook) t))

(defadvice evil-window-up (after evil-window-up-after activate) (run-hooks 'my/switch-buffer-hook))
(defadvice evil-window-down (after evil-window-down activate) (run-hooks 'my/switch-buffer-hook))
(defadvice evil-window-left (after evil-window-left activate) (run-hooks 'my/switch-buffer-hook))
(defadvice evil-window-right (after evil-window-right activate) (run-hooks 'my/switch-buffer-hook))

;; ** Window settings
;; *** Make cursor auto move to new split window
(defun my/split-and-follow-horozontally ()
  (interactive)
  (split-window-below)
  ;;(balance-windows)
  (other-window 1))

(defun my/split-and-follow-vertically()
  (interactive)
  (split-window-right)
  ;;(balance-windows)
  (other-window 1))

;; *** Don't ask for confirmation when killing window
(setq kill-buffer-query-functions (delq 'process-kill-buffer-query-function kill-buffer-query-functions))


;; *** Kill all buffers
(defun my/kill-all-buffers ()
  (interactive)
  (mapc 'kill-buffer (buffer-list)))
(global-set-key (kbd "C-M-s-k") 'my/kill-all-buffers)

;; *** Unique names for identical buffer names
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)
;; (setq uniquify-buffer-name-style 'post-forward-angle-brackets)

;; * Dired
(require 'dired)

(require 'wdired)

;; ** Open current dir
(defun my/dired-curr-dir ()
  (interactive)
  (dired default-directory))

;; ** Narrow
;; (straight-use-package 'dired-narrow)

;; ** Move file between windows
(setq dired-dwim-target t)

;; ** Change permissions in wdired
(setq wdired-allow-to-change-permissions 'advanced)

;; ** Put deleted files into trash folder
(setq delete-by-moving-to-trash t)

;; ** Dired async
;; *** Better async confirmation messages
(setq dired-async-message-function
      (lambda (text face &rest args)
	(shell-command (format "echo '%s'" (apply #'format text args)))))

;; ** Dired atool
(straight-use-package 'dired-atool)

(dired-atool-setup)

(define-key dired-mode-map "c" 'dired-atool-do-pack)
(define-key dired-mode-map "Z" 'dired-atool-do-unpack-with-subdirectory)

;; ** Wdired
(define-prefix-command 'my/wdired-mode-map)
(evil-define-key 'normal wdired-mode-map (kbd (concat my/leader-map-key " a")) 'my/wdired-mode-map)

(define-key my/wdired-mode-map (kbd "s") 'wdired-finish-edit)
(define-key my/wdired-mode-map (kbd "k") 'wdired-abort-changes)

;; ** Dired collapse
;; (straight-use-package 'dired-collapse)

;; (add-hook 'dired-mode-hook 'dired-collapse-mode)

;; ** Subtree
;; (straight-use-package 'dired-subtree)

;; ** Date format
(setq my/dired-base-ls-command "-alh --time-style \"+%d-%m-%Y %H:%M\"")
(setq dired-listing-switches my/dired-base-ls-command)

;; ** Sorting
(defun my/dired-sort-menu ()
  (interactive)
  (dired-sort-other
   (pcase (completing-read "Sort by: " '( "date" "size" "name" "dir" "du" "~compatibility"))
     ("name" (progn (my/dired-du-disable-quietly) my/dired-base-ls-command))
     ("date" (progn (my/dired-du-disable-quietly) (concat my/dired-base-ls-command "-t")))
     ("size" (progn (my/dired-du-disable-quietly) (concat my/dired-base-ls-command "--sort=size")))
     ("dir" (progn (my/dired-du-disable-quietly) (concat my/dired-base-ls-command "--group-directories-first")))
     ("~compatibility" (progn (my/dired-du-disable-quietly) "-alh"))
     ("du" (progn (dired-sort-other "-alh") (dired-du-mode 1) "-alh")))))

(defun my/dired-du-disable-quietly ()
  (if dired-du-mode
      (dired-du-mode -1)))

(define-key dired-mode-map (kbd "s") 'my/dired-sort-menu)

;; ** Recursive folder size
(straight-use-package 'dired-du)
(require 'dired-du)

;; *** Disable on new buffer
(add-hook 'dired-mode-hook 'my/dired-du-disable-quietly)

;; ** Dired-single
;; (straight-use-package 'dired-single)

;; ** Keys
;; Reset dired mode map
(setq dired-mode-map (make-sparse-keymap))

(defun my/toggle-delete-to-trash ()
  (interactive)
  (if (eq delete-by-moving-to-trash nil)
      (progn
	(setq delete-by-moving-to-trash t)
	(message "Delete to trash enabled"))
    (progn
      (setq delete-by-moving-to-trash nil)
      (message "Delete to trash disabled"))))

(define-prefix-command 'my/dired-mode-map)
(evil-define-key 'normal dired-mode-map (kbd (concat my/leader-map-key " a")) 'my/dired-mode-map)

(define-key my/dired-mode-map (kbd "t") 'my/toggle-delete-to-trash)
(define-key my/dired-mode-map (kbd "w") 'dired-toggle-read-only)
(defun my/image-dired ()
  (interactive)
  (image-dired default-directory))
(define-key my/dired-mode-map (kbd "i") 'my/image-dired)
(define-key my/dired-mode-map (kbd "h") 'dired-hide-details-mode)

;; *** Dired close buffer and go down level
(defun my/dired-kill-and-go-down ()
  (interactive)
  (let ((dired-buffer (buffer-name)))
    (dired-up-directory)
    (kill-buffer dired-buffer)))

;; *** Dired keys
;; Bind =Backspace= to go up one directory
;; (define-key dired-mode-map [?\d] 'dired-up-directory)
(evil-define-key '(normal insert) dired-mode-map (kbd "k") 'dired-up-directory)

(evil-define-key 'insert dired-mode-map (kbd "A") 'dired-do-find-regexp)
(evil-define-key 'insert dired-mode-map  (kbd "B") 'dired-do-byte-compile)

(evil-define-key '(normal insert) dired-mode-map (kbd "K") 'my/dired-kill-and-go-down)
(evil-define-key '(normal insert) dired-mode-map (kbd "RET") 'dired-find-file)
(evil-define-key '(normal insert) dired-mode-map(kbd "C") 'dired-do-copy)
(evil-define-key '(normal insert) dired-mode-map (kbd "D") 'dired-do-delete)
(evil-define-key '(normal insert) dired-mode-map  (kbd "R") 'dired-do-rename)
(evil-define-key '(normal insert) dired-mode-map  (kbd "!") 'dired-do-shell-command)
(evil-define-key '(normal insert) dired-mode-map  (kbd "&") 'dired-do-async-shell-command)
(evil-define-key '(normal insert) dired-mode-map  (kbd "Z") 'dired-do-compress)
(evil-define-key '(normal insert) dired-mode-map  (kbd "c") 'dired-do-compress-to)

(evil-define-key 'insert dired-mode-map  (kbd "G") 'dired-do-chgrp)
(evil-define-key 'insert dired-mode-map  (kbd "H") 'dired-do-hardlink)
(evil-define-key 'insert dired-mode-map  (kbd "L") 'dired-do-load)
(evil-define-key 'insert dired-mode-map  (kbd "P") 'dired-do-print)
(evil-define-key 'insert dired-mode-map  (kbd "Q") 'dired-do-find-regexp-and-replace)
(evil-define-key 'insert dired-mode-map  (kbd "S") 'dired-do-symlink)
(evil-define-key 'insert dired-mode-map  (kbd "T") 'dired-do-touch)
;; Comparison commands
(evil-define-key '(normal insert) dired-mode-map (kbd "=") 'my/ediff-dired)

;; Make all regexp commands share a `%' prefix:
;; We used to get to the submap via a symbol dired-regexp-prefix,
;; but that seems to serve little purpose, and copy-keymap
;; does a better job without it.
(evil-define-key 'insert dired-mode-map (kbd "%") nil)
(evil-define-key 'insert dired-mode-map  (kbd "%u") 'dired-upcase)
(evil-define-key 'insert dired-mode-map  (kbd "%l") 'dired-downcase)
(evil-define-key 'insert dired-mode-map  (kbd "%d") 'dired-flag-files-regexp)
(evil-define-key 'insert dired-mode-map  (kbd "%g") 'dired-mark-files-containing-regexp)
(evil-define-key 'insert dired-mode-map  (kbd "%m") 'dired-mark-files-regexp)
(evil-define-key 'insert dired-mode-map  (kbd "%r") 'dired-do-rename-regexp)
(evil-define-key 'insert dired-mode-map  (kbd "%C") 'dired-do-copy-regexp)
(evil-define-key 'insert dired-mode-map  (kbd "%H") 'dired-do-hardlink-regexp)
(evil-define-key 'insert dired-mode-map  (kbd "%R") 'dired-do-rename-regexp)
(evil-define-key 'insert dired-mode-map  (kbd "%S") 'dired-do-symlink-regexp)
(evil-define-key 'insert dired-mode-map  (kbd "%&") 'dired-flag-garbage-files)
;; Commands for marking and unmarking.
(evil-define-key 'normal dired-mode-map (kbd "*") nil)
(evil-define-key 'insert dired-mode-map (kbd "*") nil)
(evil-define-key 'normal dired-mode-map (kbd "**") 'dired-mark-executables)
(evil-define-key 'normal dired-mode-map (kbd "*/") 'dired-mark-directories)
(evil-define-key 'normal dired-mode-map (kbd "*@") 'dired-mark-symlinks)
(evil-define-key 'normal dired-mode-map (kbd "*%") 'dired-mark-files-regexp)

(evil-define-key '(normal insert) dired-mode-map (kbd "*c") 'dired-change-marks)
(evil-define-key '(normal insert) dired-mode-map (kbd "*s") 'dired-mark-subdir-files)
(evil-define-key '(normal insert) dired-mode-map (kbd "*m") 'dired-mark)
(evil-define-key '(normal insert) dired-mode-map (kbd "*u") 'dired-unmark)
(evil-define-key '(normal insert) dired-mode-map (kbd "*?") 'dired-unmark-all-files)
(evil-define-key '(normal insert) dired-mode-map (kbd "*!") 'dired-unmark-all-marks)
(evil-define-key '(normal insert) dired-mode-map (kbd "U") 'dired-unmark-all-marks)
(evil-define-key '(normal insert) dired-mode-map (kbd "*\177") 'dired-unmark-backward)
(evil-define-key '(normal insert) dired-mode-map (kbd "*\C-n") 'dired-next-marked-file)
(evil-define-key '(normal insert) dired-mode-map (kbd "*\C-p") 'dired-prev-marked-file)
(evil-define-key '(normal insert) dired-mode-map (kbd "*t") 'dired-toggle-marks)
;; Lower keys for commands not operating on all the marked files
(evil-define-key 'insert dired-mode-map (kbd "a") 'dired-find-alternate-file)
(evil-define-key '(normal insert) dired-mode-map (kbd "d") 'dired-flag-file-deletion)
(evil-define-key 'insert dired-mode-map (kbd "e") 'my/find-file)
;; (put 'dired-find-file :advertised-binding (kbd "\C-m"))
(evil-define-key 'insert dired-mode-map (kbd "g") 'revert-buffer)
(evil-define-key 'insert dired-mode-map (kbd "i") 'dired-maybe-insert-subdir)
(evil-define-key '(normal insert) dired-mode-map (kbd "j") 'dired-goto-file)
;; Maybe bind this??
(evil-define-key 'insert dired-mode-map (kbd "l") 'dired-do-redisplay)
(evil-define-key 'normal dired-mode-map (kbd "M-m") 'dired-mark-subdir-files)
(evil-define-key '(normal insert) dired-mode-map (kbd "m") 'dired-mark)
(evil-define-key 'insert dired-mode-map (kbd "n") 'dired-next-line)
(evil-define-key '(normal insert) dired-mode-map (kbd "o") 'dired-find-file-other-window)
(evil-define-key 'insert dired-mode-map (kbd "\C-o") 'dired-display-file)
(evil-define-key 'insert dired-mode-map (kbd "p") 'dired-previous-line)
(evil-define-key 'insert dired-mode-map (kbd "s") 'my/dired-sort-menu)
(evil-define-key 'insert dired-mode-map (kbd "t") 'dired-toggle-marks)
(evil-define-key '(normal insert) dired-mode-map (kbd "u") 'dired-unmark)
(evil-define-key 'insert dired-mode-map (kbd "v") 'dired-view-file)

(evil-define-key 'insert dired-mode-map (kbd "y") 'dired-copy-filename-as-kill)
(evil-define-key 'insert dired-mode-map (kbd "w") 'browse-url-of-dired-file)
(evil-define-key '(normal insert) dired-mode-map (kbd "x") 'dired-do-flagged-delete)

(evil-define-key '(normal insert) dired-mode-map (kbd "?") 'dired-show-file-type)
(evil-define-key '(normal insert) dired-mode-map (kbd "+") 'dired-create-directory)
;; moving
(evil-define-key '(normal insert) dired-mode-map (kbd "<") 'dired-prev-dirline)
(evil-define-key '(normal insert) dired-mode-map (kbd ">") 'dired-next-dirline)
(evil-define-key '(normal insert) dired-mode-map (kbd "^") 'dired-up-directory)
;; folding
(evil-define-key 'normal dired-mode-map (kbd "g") nil)
(define-key dired-mode-map  [remap evil-close-fold] 'dired-hide-subdir)
(define-key dired-mode-map  [remap evil-open-fold] 'dired-unhide-subdir)
(define-key dired-mode-map  [remap my/outline-hide-all-body] 'dired-hide-all)
(define-key dired-mode-map  [remap outline-show-all] 'dired-hide-all)
;; misc
;; (evil-define-key 'insert dired-mode-map [remap read-only-mode] 'dired-toggle-read-only)
;; ;; `toggle-read-only' is an obsolete alias for `read-only-mode'
;; (evil-define-key 'insert dired-mode-map [remap toggle-read-only] 'dired-toggle-read-only)
;; (evil-define-key 'insert dired-mode-map  (kbd "\177") 'dired-unmark-backward)

;; thumbnail manipulation (image-dired)
(evil-define-key 'insert dired-mode-map (kbd "\C-td") 'image-dired-display-thumbs)
(evil-define-key 'insert dired-mode-map (kbd "\C-tt") 'image-dired-tag-files)
(evil-define-key 'insert dired-mode-map (kbd "\C-tr") 'image-dired-delete-tag)
(evil-define-key 'insert dired-mode-map (kbd "\C-tj") 'image-dired-jump-thumbnail-buffer)
(evil-define-key 'insert dired-mode-map (kbd "\C-ti") 'image-dired-dired-display-image)
(evil-define-key 'insert dired-mode-map (kbd "\C-tx") 'image-dired-dired-display-external)
(evil-define-key 'insert dired-mode-map (kbd "\C-ta") 'image-dired-display-thumbs-append)
(evil-define-key 'insert dired-mode-map (kbd "\C-t.") 'image-dired-display-thumb)
(evil-define-key 'insert dired-mode-map (kbd "\C-tc") 'image-dired-dired-comment-files)
(evil-define-key 'insert dired-mode-map (kbd "\C-tf") 'image-dired-mark-tagged-files)
(evil-define-key 'insert dired-mode-map (kbd "\C-t\C-t") 'image-dired-dired-toggle-marked-thumbs)
(evil-define-key 'insert dired-mode-map (kbd "\C-te") 'image-dired-dired-edit-comment-and-tags)
;; encryption and decryption (epa-dired)
;; (evil-define-key 'insert (kbd ":d") 'epa-dired-do-decrypt)
;; (evil-define-key 'insert  (kbd ":v") 'epa-dired-do-verify)
;; (evil-define-key 'insert  (kbd ":s") 'epa-dired-do-sign)
;; (evil-define-key 'insert  (kbd ":e") 'epa-dired-do-encrypt)

;; * Gud
(straight-use-package 'realgud)
(setq gdb-many-windows 'nil)
;; (require 'realgud)

;; * Eldoc
;; Shows information in echo area
;; Needed??
;; (add-hook 'emacs-lisp-mode-hook 'eldoc-mode)
(setq eldoc-echo-area-use-multiline-p t)
(setq-default eldoc-echo-area-use-multiline-p t)
(setq eldoc-idle-delay 0)

(setq tooltip-resize-echo-area t)

;; Update function to only show documentation when not in insert mode
;; (el-patch-feature eldoc)
;; (el-patch-defun eldoc-print-current-symbol-info ()
;; "Print the text produced by `eldoc-documentation-function'."
;;  This is run from post-command-hook or some idle timer thing,
;;  so we need to be careful that errors aren't ignored.
;; (if (not (eq evil-state 'insert))
;; (with-demoted-errors "eldoc error: %s"
;; (and (or (eldoc-display-message-p)
;;  Erase the last message if we won't display a new one.
;; (when eldoc-last-message
;; (eldoc-message nil)
;; nil))
;; (eldoc-message (funcall eldoc-documentation-function))))))

;; ** Box
;; (straight-use-package '(eldoc-box :type git :host github :repo "casouri/eldoc-box"))

;; (define-globalized-minor-mode global-eldoc-box-mode
;; eldoc-box-hover-mode eldoc-box-hover-mode)
;; (global-eldoc-box-mode)

;;  eldoc-box-hover-at-point-mode needs to be enabled after eldoc-box-hover-mode is done, otherwise problems can appear
;; (defadvice eldoc-box-hover-mode (after eldoc-box-hover-mode activate) (eldoc-box-hover-at-point-mode 1))

;; ** Inline
;; (straight-use-package 'eldoc-overlay)

;; (global-eldoc-overlay-mode)

;; * Code
;; ** Generic
;; *** Ivy-xref
(straight-use-package 'ivy-xref)

(require 'ivy-xref)
(setq xref-show-xrefs-function #'ivy-xref-show-xrefs)

;; *** Smartparens
;; (straight-use-package 'smartparens)

;; (smartparens-global-mode)

;; *** Quick-peek
(straight-use-package 'quick-peek)
(require 'quick-peek)

(setq quick-peek-spacer nil)

;; *** Aggressive indent
(straight-use-package 'aggressive-indent)

(global-aggressive-indent-mode)
;; (add-hook 'prog-mode-hook 'aggressive-indent-mode)

;; *** Whitespace cleanup
(straight-use-package 'whitespace-cleanup-mode)

(global-whitespace-cleanup-mode)

;; *** indent guide
;; (straight-use-package 'highlight-indent-guides)

;; (add-hook 'prog-mode-hook 'highlight-indent-guides-mode)

;; (add-hook 'prog-mode-hook '(lambda () (highlight-indent-guides-mode (my/highlight-indent-guide-should-enable))))

;; ;;(defun my/highlight-indent-guide-should-enable ()
;; ;;  (pcase major-mode
;; ;;    ('emacs-lisp-mode -1)
;; ;;    ('lisp-interaction-mode -1)
;; ;;    ('clojure-mode -1)
;; ;;    ('scheme-mode -1)
;; ;;    ('common-lisp-mode -1)
;; ;;    (_ 1)))

;; (setq highlight-indent-guides-method 'column)
;; ;; (setq highlight-indent-guides-method 'fill)

;; (setq highlight-indent-guides-responsive 'top)
;; (setq highlight-indent-guides-delay 0)

;; *** Auto jump to definition
(straight-use-package 'dumb-jump)

(defun my/auto-jump-to-definition ()
  (interactive)
  (pcase major-mode
    ('fsharp-mode (fsharp-ac/gotodefn-at-point))
    ('clojure-mode (clojure-find-def))
    ('emacs-lisp-mode (call-interactively 'xref-find-definitions))
    ('c-mode (call-interactively 'xref-find-definitions))
    ('c++-mode (call-interactively 'xref-find-definitions))
    ('objc-mode (call-interactively 'xref-find-definitions))
    ('csharp-mode (omnisharp-go-to-definition))
    ;; No idea why but supplying a empty string makes it work
    ('haskell-mode (intero-goto-definition))
    (_
     (if lsp-mode
	 (lsp-find-definition)
       (dumb-jump-go)))))

(define-key my/leader-map (kbd "t") 'my/auto-jump-to-definition)

(setq dumb-jump-selector 'ivy)

;; *** Auto find usages
(defun my/auto-find-usages ()
  (interactive)
  (pcase major-mode
    ('csharp-mode (omnisharp-find-usages-with-ido))
    ('haskell-mode (intero-uses-at))
    (_ (call-interactively 'xref-find-references))))

(define-key my/leader-map (kbd "u") 'my/auto-find-usages)

;; *** Auto eval
(defun my/auto-eval ()
  (interactive)
  (if (eq evil-state 'visual)
      (my/auto-eval-region)
    (pcase major-mode
      ;; Silent result
      ('org-mode (org-babel-execute-src-block nil nil '((:result-params . ("none")))))
      ('scheme-mode (geiser-eval-definition nil))
      ('clojure-mode (cider-eval-last-sexp))
      ('racket-mode (racket-eval-last-sexp))
      ('plantuml-mode (plantuml-preview-region 0 (line-beginning-position) (line-end-position)))
      ('fsharp-mode (fsharp-eval-phrase))
      ('c-mode (cling-send-region (line-beginning-position) (line-end-position)))
      ('c++-mode (cling-send-region (line-beginning-position) (line-end-position)))
      ('csharp-mode (my/csharp-run-repl))
      ('haskell-mode (intero-repl-eval-region (line-end-position) (save-excursion (re-search-backward (rx bol (or (any alpha) (any lower))))) ))
      (_ (call-interactively 'eros-eval-last-sexp)))))

(defun my/auto-eval-region ()
  (interactive)
  (pcase major-mode
    ('clojure-mode (cider-eval-region (point) (mark)))
    ('plantuml-mode (plantuml-preview-region 0 (point) (mark)))
    ('fsharp-mode (fsharp-eval-region (point) (mark)))
    ('c-mode (cling-send-region (point) (mark)))
    ('c++-mode (cling-send-region (point) (mark)))
    ('csharp-mode (my/csharp-run-repl))
    ('haskell-mode (intero-repl-eval-region (point) (mark)))
    (_ (eros--eval-overlay
	(eval-region (point) (mark) t)
	(point)))))

(defun my/auto-eval-buffer ()
  (interactive)
  (pcase major-mode
    ('scheme-mode (geiser-eval-buffer nil))
    ('clojure-mode (cider-eval-buffer))
    ('plantuml-mode (plantuml-preview-buffer 0))
    ('fsharp-mode (fsharp-eval-region (point-min) (point-max)))
    ('c-mode (cling-send-buffer))
    ('c++-mode (cling-send-buffer))
    ('csharp-mode (my/csharp-run-repl))
    ('haskell-mode (intero-repl-load))
    (_ (eval-buffer nil))))

(defun my/auto-eval-print ()
  (interactive)
  (pcase major-mode
    ('org-mode (call-interactively #'org-babel-execute-src-block))
    ('scheme-mode (geiser-eval-last-sexp t))
    ('clojure-mode (cider-eval-print-last-sexp))
    ('csharp-mode (my/csharp-run-repl))
    (_ (eval-print-last-sexp nil))))

(define-key my/leader-map (kbd "e") 'my/auto-eval)
(define-key my/leader-map (kbd "E") 'my/auto-eval-buffer)
(define-key my/leader-map (kbd "M-e") 'my/auto-eval-print)

;; *** Auto debug
(defun my/auto-debug ()
  (interactive)
  ;;(load-library "realgud")
  (if (eq evil-state 'visual)
      (my/auto-debug-region)
    (pcase major-mode
      ('emacs-lisp-mode (progn (require 'edebug) (call-interactively #'edebug-set-breakpoint)))
      ('c-mode (call-interactively #'gud-break))
      ('c++-mode (call-interactively #'gud-break))
      (_ (eval-last-sexp nil)))))

(defun my/auto-remove-debug ()
  (interactive)
  ;;(load-library "realgud")
  (if (eq evil-state 'visual)
      (my/auto-debug-region)
    (pcase major-mode
      ('emacs-lisp-mode (call-interactively #'edebug-unset-breakpoint))
      ('c-mode (call-interactively #'gud-remove))
      ('c++-mode (call-interactively #'gud-remove))
      (_ (eval-last-sexp nil)))))

;; (defun my/auto-debug-region ()
;; (interactive)
;; )

(defun my/start-gdb()
  (split-window-below)
  (call-interactively #'gdb))

(defun my/auto-start-debugger ()
  (interactive)
  ;;(load-library "realgud")
  (pcase major-mode
    ('c-mode (my/start-gdb))
    ('c++-mode (my/start-gdb))
    (_ (eval-last-sexp nil))))

(define-key my/leader-map (kbd "D") 'my/auto-debug)
(define-key my/leader-map (kbd "C-D") 'my/auto-remove-debug)
;; (define-key my/leader-map (kbd "D") 'my/auto-debug-buffer)
(define-key my/leader-map (kbd "M-D") 'my/auto-start-debugger)

;; *** Auto compile
(defun my/auto-compile ()
  (interactive)
  (pcase major-mode
    ('emacs-lisp-mode (emacs-lisp-byte-compile))
    ('clojure-mode (cider-eval-last-sexp))
    ('plantuml-mode (plantuml-preview-buffer 0))
    (_ (recompile))))

(define-key my/leader-map (kbd "C") 'my/auto-compile)

;; ** Documentation
;; *** Compact-docstrings
;; (straight-use-package 'compact-docstrings)

;; *** Zeal
(straight-use-package 'zeal-at-point)

(define-key my/leader-map (kbd "T") 'zeal-at-point)

;; ** Plantuml
(straight-use-package 'plantuml-mode)

;; Org src compatibility
(add-to-list 'auto-mode-alist '("\\.plantuml\\'" . plantuml-mode))

(defun my/plantuml-mode ()
  (interactive)
  (aggressive-indent-mode -1))

(add-hook 'plantuml-mode-hook 'my/plantuml-mode)

;; ** LSP
(straight-use-package 'lsp-mode)

;; Normally lsp-mode starts up flymake mode automaticall which breaks ccls
(setq lsp-prefer-flymake nil)

(setq lsp-document-highlight-delay nil)

;; *** Company LSP
(straight-use-package 'company-lsp)
(push 'company-lsp company-backends)

;; *** LSP-ui
(straight-use-package 'lsp-ui)
;; TODO I have to load the package fully here to set the fonts later
(require 'lsp-ui)

(setq lsp-ui-doc-enable nil
      lsp-ui-peek-enable t
      lsp-ui-sideline-enable t
      lsp-ui-imenu-enable t
      lsp-ui-flycheck-enable nil)
;; (setq lsp-ui-sideline-ignore-duplicate t)

(setq lsp-ui-sideline-show-code-actions t
      ;; Errors i think
      lsp-ui-sideline-show-diagnostics nil
      
      ;; someFunc :: IO ()
      lsp-ui-sideline-show-hover t
      
      ;; [someFunc]
      lsp-ui-sideline-show-symbol t)

(setq lsp-ui-sideline-delay 0)

;; *** Keys
(define-key my/leader-map (kbd "RET") 'lsp-ui-sideline-apply-code-actions)

;; ** Elgot
;; (straight-use-package 'eglot)

;; ** Lisps
(straight-use-package 'lispy)
(require 'lispy)

;; *** Common lisp
;; **** Slime
(straight-use-package 'slime)

(setq inferior-lisp-program "/usr/bin/sbcl")
(setq slime-contribs '(slime-fancy))

;; ***** Slime comany
(straight-use-package 'slime-company)

(slime-setup '(slime-fancy slime-company))

;; **** Keys
(define-prefix-command 'my/common-lisp-mode-map)
(evil-define-key 'normal lisp-mode-map (kbd (concat my/leader-map-key " a")) 'my/common-lisp-mode-map)

(define-key my/common-lisp-mode-map (kbd "d") 'slime-inspect-definition)


;; *** Scheme
;; (straight-use-package 'geiser)

;; (define-prefix-command 'my/scheme-mode-map)
;; (evil-define-key 'normal scheme-mode-map (kbd (concat my/leader-map-key " a")) 'my/scheme-mode-map)

;; (define-key my/scheme-mode-map (kbd "s") 'geiser-set-scheme)
;; (define-key my/scheme-mode-map (kbd "C-s") 'run-geiser)

;; *** Racket
(straight-use-package 'racket-mode)

;; **** Fix eval
(defun racket-eval-last-sexp ()
  "Eval the previous sexp asynchronously and `message' the result."
  (interactive)
  (racket--cmd/async
   `(eval
     ,(buffer-substring-no-properties (racket--repl-last-sexp-start)
				      (+ 1(point))))
   (lambda (v)
     (message "%s" v))))

(defun racket--repl-last-sexp-start ()
  (save-excursion
    (condition-case ()
	(progn
	  (my/backward-sexp)
	  (if (save-match-data (looking-at "#;"))
	      (+ (point) 2)
	    (point)))
      (scan-error (user-error "There isn't a complete s-expression before point")))))

;; *** Emacs-lisp
;; **** Eros
(straight-use-package 'eros)

(eros-mode 1)

;; **** Litable
;; ;;(straight-use-package '(litable :type git :host github :repo "Fuco1/blablabla"))
;; (straight-use-package 'litable)
;; (require 'litable)

;; ;; Eval everything
;; (defun litable--safe-eval (form)
;;   (eval form))

;; ;; Make it only eval the parens around cursor
;; (defun litable-update-defs (&optional a b c)
;;   (litable-remove-overlays)
;;   (when a
;;     (ignore-errors
;;       (let ((form (save-excursion
;;                     (backward-up-list)
;;                     (sexp-at-point))))
;;         (litable-find-function-subs-arguments form)))))

;; ***** Don't safe check
;; (defun litable--safe-eval (form)
;; (eval form))

;; **** Enable debugging on error
(setq debug-on-error nil)

;; **** Debugging

;; **** Suggest
(straight-use-package 'suggest)


;; **** Formatting
(straight-use-package 'elisp-format)

;; **** Keys
(define-prefix-command 'my/emacs-lisp-mode-map)
(evil-define-key 'normal emacs-lisp-mode-map (kbd (concat my/leader-map-key " a")) 'my/emacs-lisp-mode-map)

(define-key my/emacs-lisp-mode-map (kbd "d") 'find-function)
(define-key my/emacs-lisp-mode-map (kbd "D") 'find-variable)

(define-key my/emacs-lisp-mode-map (kbd "c") 'emacs-lisp-byte-compile)

(define-key my/emacs-lisp-mode-map (kbd "s") 'suggest)

(define-prefix-command 'my/emacs-lisp-formatting-map)
(define-key my/emacs-lisp-mode-map (kbd "f") 'my/emacs-lisp-formatting-map)

(define-key my/emacs-lisp-formatting-map (kbd "b") 'elisp-format-buffer)
(define-key my/emacs-lisp-formatting-map (kbd "r") 'elisp-format-region)
(define-key my/emacs-lisp-formatting-map (kbd "f") 'elisp-format-file)
(define-key my/emacs-lisp-formatting-map (kbd "C-d") 'elisp-format-directory)
;; Format marked files in dired
;; elisp-format-dired-mark-files


;; control
(define-key my/emacs-lisp-mode-map "n" 'edebug-step-mode)
;; (define-key my/emacs-lisp-mode-map "n" 'edebug-next-mode)

(define-key my/emacs-lisp-mode-map (kbd "s") 'edebug-go-mode)
(define-key my/emacs-lisp-mode-map "S" 'edebug-Go-nonstop-mode)
(define-key my/emacs-lisp-mode-map "t" 'edebug-trace-mode)
(define-key my/emacs-lisp-mode-map "T" 'edebug-Trace-fast-mode)

(define-key my/emacs-lisp-mode-map "g" 'edebug-goto-here)

(define-key my/emacs-lisp-mode-map "i" 'edebug-step-in)
(define-key my/emacs-lisp-mode-map "o" 'edebug-step-out)

;; *** Clojure
(straight-use-package 'clojure-mode)
(require 'clojure-mode)

;; **** Cider
(straight-use-package 'cider)

;; ***** Enlighten
(add-hook 'clojure-mode-hook 'cider-enlighten-mode)

;; **** Keys
(define-prefix-command 'my/clojure-mode-map)
(evil-define-key 'normal clojure-mode-map (kbd (concat my/leader-map-key " a")) 'my/clojure-mode-map)

(define-key my/clojure-mode-map (kbd "C-s") 'cider-connect)

;; ** Java
;; Try
;; https://github.com/mopemope/meghanada-emacs
;; or
;; =ENSIME=
(straight-use-package 'lsp-java)
(require 'lsp-java)

(defun my/java-mode ()
  (aggressive-indent-mode -1)
  (lsp)
  (lsp-lens-mode))

(add-hook 'java-mode-hook 'my/java-mode)

;; *** Keys
(define-prefix-command 'my/java-mode-map)
(evil-define-key 'normal java-mode-map (kbd (concat my/leader-map-key " a")) 'my/java-mode-map)

(define-key my/java-mode-map (kbd "r") 'lsp-rename)
(define-key my/java-mode-map (kbd "C-r") 'lsp-workspace-restart)
(define-key my/java-mode-map (kbd "f") 'lsp-format-buffer)
(define-key my/java-mode-map (kbd "i") 'lsp-java-organize-imports)
(define-key my/java-mode-map (kbd "C-b") 'lsp-java-build-project)

(define-key my/java-mode-map (kbd "f") 'xref-find-references)
(define-key my/java-mode-map (kbd "a") 'xref-find-apropos)

(define-prefix-command 'my/java-refractor-map)
(define-key my/java-mode-map (kbd "R") 'my/java-refractor-map)

(define-key my/java-refractor-map (kbd "c") 'lsp-java-extract-to-constant)
(define-key my/java-refractor-map (kbd "u") 'lsp-java-add-unimplemented-methods)
(define-key my/java-refractor-map (kbd "p") 'lsp-java-create-parameter)
(define-key my/java-refractor-map (kbd "f") 'lsp-java-create-field)
(define-key my/java-refractor-map (kbd "l") 'lsp-java-create-local)
(define-key my/java-refractor-map (kbd "m") 'lsp-java-extract-method)
(define-key my/java-refractor-map (kbd "i") 'lsp-java-add-import)

;; ** Python
;; *** Jedi
(straight-use-package 'company-jedi)

(add-to-list 'company-backends 'company-jedi)

;; ** Haskell
(straight-use-package '(haskell-mode :type git :host github :repo "walseb/haskell-mode"))

;; *** Project management
;; **** Stack
(straight-use-package 'hasky-stack)

;; **** Cabal
;; (straight-use-package 'hasky-cabal)

;; **** Nix cabal
;; Use nix-haskell-mode for automatic project management
;; (straight-use-package 'nix-haskell-mode)

;; *** Indentation
;; Haskell-indentation-mode seems better?? or hi2 or hindent
;; (straight-use-package 'hyai)
;; (straight-use-package 'hindent)

;; *** Extension management
(straight-use-package 'hasky-extensions)

;; *** Haskell-cabal
(straight-use-package 'company-cabal)

;; *** intero
(straight-use-package 'intero)
(require 'intero)

(add-hook 'haskell-mode-hook 'intero-mode)

;; Enabble hlint
(flycheck-add-next-checker 'intero '(warning . haskell-hlint))

(setq intero-pop-to-repl nil)

;; Start repl mode in insert mode
(add-to-list 'evil-insert-state-modes 'intero-repl-mode)

;; **** Redefine eval-region
;; The current one ignores any beg and end if you are not in visual line mode
(defun intero-repl-eval-region (begin end &optional prompt-options)
  "Evaluate the code in region from BEGIN to END in the REPL.
   If the region is unset, the current line will be used.
   PROMPT-OPTIONS are passed to `intero-repl-buffer' if supplied."
  (interactive "r")
  (let ((text (buffer-substring-no-properties begin end)))
    (intero-with-repl-buffer prompt-options
      (comint-simple-send
       (get-buffer-process (current-buffer))
       text))))

;; *** lsp-haskell
;; (straight-use-package 'lsp-haskell)
;; (require 'lsp-haskell)

;; (add-hook 'haskell-mode-hook 'lsp)

;; ** C/CPP
;; *** Irony
;; (straight-use-package 'irony)

;; (add-hook 'c++-mode-hook 'irony-mode)
;; (add-hook 'c-mode-hook 'irony-mode)
;; (add-hook 'objc-mode-hook 'irony-mode)

;; (add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)

;; **** Windows tweaks
;; Windows performance tweaks
;; (when (boundp 'w32-pipe-read-delay)
;; (setq w32-pipe-read-delay 0))
;; ;; Set the buffer size to 64K on Windows (from the original 4K)
;; (when (boundp 'w32-pipe-buffer-size)
;; (setq irony-server-w32-pipe-buffer-size (* 64 1024)))

;; **** Flycheck-irony
;; (straight-use-package 'flycheck-irony)

;; (eval-after-load 'flycheck
;; '(add-hook 'flycheck-mode-hook #'flycheck-irony-setup))

;; **** Company-irony
;; (straight-use-package 'company-irony)

;; (eval-after-load 'company
;; '(add-to-list 'company-backends 'company-irony))

;; **** Eldoc-irony
;; (straight-use-package 'irony-eldoc)

;; (add-hook 'irony-mode-hook #'irony-eldoc)

;; *** Elgot
;; (add-hook 'c-mode-hook 'eglot-ensure)
;; (add-hook 'c++-mode-hook 'eglot-ensure)
;; (add-hook 'objc-mode-hook 'eglot-ensure)

;; *** LSP CCLS
(straight-use-package 'ccls)
(require 'ccls)

(setq ccls-executable "/bin/ccls")

(defun my/c-mode ()
  (aggressive-indent-mode 0)
  (lsp)
  (lsp-lens-mode)
  (push 'company-lsp company-backends)
  
  (setq indent-tabs-mode t)
  (setq tab-width 4)
  (setq c-basic-offset 4)
  (setq evil-shift-width 4))

(add-hook 'c-mode-hook 'my/c-mode)
(add-hook 'c++-mode-hook 'my/c-mode)
(add-hook 'objc-mode-hook 'my/c-mode)

;; *** Cling
;; https://github.com/brianqq/inferior-cling
(defun my/cling (&optional flags)
  "Move to the buffer containing Cling, or create one if it does not exist. Defaults to C++11"
  (interactive)
  (let ((flags (or flags "-std=c++11")))
    (make-comint "inferior-cling" "cling" nil flags)
    (switch-to-buffer-other-window "*inferior-cling*")))

(defun my/cling-send-string (string &optional process)
  "Send a string terminated with a newline to the inferior-cling buffer. Has the effect of executing a command"
  (let ((process (or process (get-process "inferior-cling"))))
    (comint-send-string process string)
    (comint-send-string process "\n")))

(defun my/cling-send-region (start end)
  "Sends the region in the current buffer between `start` and `end` to the inferior-cling buffer. Sends the currently selected region when called interactively."
  (interactive "r")
  (my/cling-send-string (buffer-substring start end)))

(defun my/cling-send-buffer ()
  "Sends the current buffer to the inferior-cling buffer."
  (interactive)
  (my/cling-send-region (point-min) (point-max))) ;;do i want to wrap-raw this?

(defun my/cling-wrap-raw (string)
  "Wraps `string` in \".rawInput\", which tells Cling to accept function definitions"
  (format ".rawInput\n%s\n.rawInput" string))

(defun my/cling-wrap-region-and-send (start end)
  "Sends the region between start and end (currently selected when called interactively) to cling in raw input mode "
  (interactive "r")
  (my/cling-send-string (my/cling-wrap-raw (buffer-substring start end))))

(defun my/flatten-function-def ()
  "Flattens a function definition into a single line. This makes it easier to send to the inferior-cling buffer"
  (interactive)
  (replace-regexp "
   " "" nil (mark) (point))) ;;;Why did I do this again?

(defun my/select-defun ()
  "Selects the defun containing the point. Currently only works when point is on the line where the function's name is declared."
  (interactive)
  (move-beginning-of-line nil)
  (push-mark (point))
  (re-search-forward "{")
  (save-excursion
    (my/flatten-function-def))
  (backward-char)
  (forward-sexp))

(defun my/cling-wrap-defun-and-send ()
  "Sends the current defun to cling in raw input mode. Currently only works when point is on the first line of function definition."
  (interactive)
  (save-excursion
    (my/select-defun)
    (my/cling-wrap-region-and-send (mark) (point))
    (undo)
    (undo)));;;this is a rather leaky way of doing temporary changes. there should be some way to save buffer contents or something
   ;;;probably uses with-temp-buffer

;; *** Etags
;; (straight-use-package 'etags)
;; (straight-use-package 'counsel-etags)

;; *** Rtags


;; *** Debugging
;; (require 'gdb-mi)

;; *** Keys
(define-prefix-command 'my/c-mode-map)

(evil-define-key 'normal c-mode-map (kbd (concat my/leader-map-key " a")) 'my/c-mode-map)
(evil-define-key 'normal c++-mode-map (kbd (concat my/leader-map-key " a")) 'my/c-mode-map)

(define-key my/c-mode-map (kbd "e") 'cling)

(define-key my/c-mode-map (kbd "n") 'gud-step)
(define-key my/c-mode-map (kbd "N") 'gud-next)

(define-key my/c-mode-map (kbd "p") 'gud-print)
(define-key my/c-mode-map (kbd "P") 'gud-pstar)

(define-key my/c-mode-map (kbd "s") 'gud-go)

;; Run to selected line
(define-key my/c-mode-map (kbd "g") 'gud-until)

;; Run out of function
(define-key my/c-mode-map (kbd "f") 'gud-finish)

(define-key my/c-mode-map (kbd "v") 'gdb-display-locals-buffer)
(define-key my/c-mode-map (kbd "V") 'gdb-display-locals-for-thread)

(define-key my/c-mode-map (kbd "b") 'gdb-display-breakpoints-buffer)

;; *** Auto view documentation
(defun my/auto-view-docs ()
  (interactive)
  (pcase major-mode
    ('csharp-mode (omnisharp-current-type-documentation))
    (_
     (if lsp-mode
	 (lsp-describe-thing-at-point)
       (find-function-at-point)))))

(define-key my/leader-map (kbd "d") 'my/auto-view-docs)

;; *** Auto rename
(defun my/auto-rename ()
  (interactive)
  (pcase major-mode
    ('csharp-mode (omnisharp-rename))
    (_ (find-function-at-point))))

(define-key my/leader-map (kbd "R") 'my/auto-rename)

;; *** Auto format
(defun my/auto-format-buffer ()
  (interactive)
  (pcase major-mode
    ('csharp-mode (omnisharp-code-format-entire-file))
    (_ ())))

(defun my/auto-format-region ()
  (interactive)
  (pcase major-mode
    ('csharp-mode (omnisharp-code-format-region))
    (_ ())))

(define-key my/leader-map (kbd "<") 'my/auto-format-buffer)
(define-key my/leader-map (kbd ">") 'my/auto-format-buffer)
(define-key my/leader-map (kbd ",") 'my/auto-format-region)
(define-key my/leader-map (kbd ".") 'my/auto-format-region)


;; ** C#
(straight-use-package 'csharp-mode)

;; csharp-maybe-insert-codedoc
(setq csharp-mode-map (make-sparse-keymap))

;; *** REPL
(defun my/csharp-run-repl()
  (interactive)
  (eshell) (insert "csharp") (eshell-send-input))

;; *** Omnisharp-emacs
(straight-use-package 'omnisharp)

;; (add-hook 'csharp-mode-hook (lambda () (push 'company-omnisharp company-backends)))

(eval-after-load 'company '(add-to-list 'company-backends #'company-omnisharp))

;; This hack fixes omnisharp on arch linux
;; Read https://github.com/OmniSharp/omnisharp-emacs/issues/459
(if (eq system-type 'gnu/linux)
    (let ((dotnet-version (string-trim (shell-command-to-string "dotnet --version")))) (setenv "MSBuildSDKsPath" (format "/opt/dotnet/sdk/%s/Sdks" dotnet-version))))

;; **** Keys
(define-prefix-command 'my/csharp-mode-map)
(evil-define-key 'normal csharp-mode-map (kbd (concat my/leader-map-key " a")) 'my/csharp-mode-map)

(define-key my/csharp-mode-map (kbd "r") 'omnisharp-run-code-action-refactoring)
(define-key my/csharp-mode-map (kbd "S") 'omnisharp-reload-solution)
(define-key my/csharp-mode-map (kbd "u") 'omnisharp-fix-usings)
(define-key my/csharp-mode-map (kbd "i") 'omnisharp-find-implementations)
(define-key my/csharp-mode-map (kbd "e") 'omnisharp-solution-errors)
(define-key my/csharp-mode-map (kbd "o") 'omnisharp-show-overloads-at-point)

(define-key my/csharp-mode-map (kbd "g") 'omnisharp-navigate-to-solution-file)

;; **** Write formatting settings to omnisharp server config
;; omnisharp.json should be in ~/.omnisharp on all OSs
;; if(not(file-directory-p "~/.omnisharp")
;;     (make-directory "~/.omnisharp"))

;; (if(not(file-exists-p "~/.omnisharp/omnisharp.json"))
;;     (progn
;;       (write-region "
;;         {
;;             \"formattingOptions\": {
;;                 PUT OPTIONS HERE
;;             }
;;         }
;;        " nil "~/.omnisharp/omnisharp.json")

;;       (message "~/.omnisharp/omnisharp.json created")
;;       )
;;   )

;; *** My csharp mode
(defun my/csharp-mode ()
  (omnisharp-mode)
  (company-mode)
  (flycheck-mode)
  
  (setq indent-tabs-mode nil)
  (setq c-syntactic-indentation t)
  (c-set-style "ellemtel")
  (setq c-basic-offset 4)
  (setq tab-width 4)
  (setq evil-shift-width 4))

(add-hook 'csharp-mode-hook 'my/csharp-mode)

;; ** F#
(straight-use-package 'fsharp-mode)

;; :mode ("\\.fs\\'" . fsharp-mode)
(setq fsharp-doc-idle-delay 0)

;; (setq-default fsharp-indent-offset 2)

;; *** Keys
(add-hook 'fsharp-mode-hook 'my/fsharp-keys-init)

(define-prefix-command 'my/fsharp-mode-map)

(defun my/fsharp-keys-init()
  (interactive)
  (evil-define-key 'normal fsharp-mode-map (kbd (concat my/leader-map-key " a")) 'my/fsharp-mode-map))

(define-key my/fsharp-mode-map (kbd "v") 'fsharp-mark-phrase)
(define-key my/fsharp-mode-map (kbd "b") 'fsharp-goto-block-up)
(define-key my/fsharp-mode-map (kbd "C-r") 'fsharp-ac-status)
(define-key my/fsharp-mode-map (kbd "C-k") 'fsharp-ac/stop-process)
(define-key my/fsharp-mode-map (kbd "C-s") 'fsharp-ac/start-process)

;; *** Settings
(defun my/fsharp-mode()
  ;; Disable not so helpful modes
  ;;
  (aggressive-indent-mode 0)
  ;; Fsharp has built in intellisense highlight thing at point
  (symbol-overlay-mode -1)
  ;; Visual line mode in fsharp mode is broken, makes swiper take years to start, use truncate lines mode instead
  (visual-line-mode 0))

;; Autostart
(add-hook 'fsharp-mode-hook 'my/fsharp-mode)

;; ** Markdown
(straight-use-package 'markdown-mode)

;; ** Web mode
;; TODO: Fix settings, grab them from package site
(straight-use-package 'web-mode)

(add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.[agj]sp\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
;; Messes with .cs files???
;; (add-to-list 'auto-mode-alist '("\\.css?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.xml?\\'" . web-mode))

;; *** Debugger
(straight-use-package 'dap-mode)

;; * Macros
;; (define-prefix-command 'my/macro-map)

;; (define-key my/leader-map (kbd "q") 'my/macro-map)

;; ** Macro manager
(defvar my/current-macro-number 0)
(defvar my/macro-store '())
(defvar my/macro-last-name nil)

(defun my/macro-record-toggle ()
  (interactive)
  (if defining-kbd-macro
      (my/macro-record-stop)
    (call-interactively #'start-kbd-macro)))

(defun my/macro-record-stop ()
  (interactive)
  (call-interactively #'end-kbd-macro)
  (let
      ((macro-name (completing-read "Name macro: " nil nil nil "macro-")))
    (name-last-kbd-macro (intern macro-name))
    (add-to-list 'my/macro-store macro-name)))

(defun my/macro-run (count)
  (interactive "P")
  (let ((to-run (completing-read "Run macro: " my/macro-store)))
    (if count
	(dotimes (i count)
	  (execute-extended-command nil to-run))
      (execute-extended-command nil to-run))))

(defun my/macro-modify (&optional prefix)
  (interactive "P")
  (let
      ((chosen-macro (completing-read "Modify macro: " my/macro-store)))
    ;; Just simulate the keypresses collected
    (setq unread-command-events (listify-key-sequence (concat chosen-macro "\C-a")))
    ;;            Anything other than this and nil is not accepted it seems
    (edit-kbd-macro 'execute-extended-command prefix)))

(my/evil-normal-define-key "q" 'my/macro-record-toggle)
(my/evil-visual-define-key "q" 'my/macro-record-toggle)

(my/evil-normal-define-key "Q" 'my/macro-run)
(my/evil-visual-define-key "Q" 'my/macro-run)

(my/evil-normal-define-key "C-q" 'my/macro-modify)
(my/evil-visual-define-key "C-q" 'my/macro-modify)

;; ** Simple search
;; Search useful for making macros because it's fast
;; But is it really faster than isearch?
;; (defvar my/last-search)
;; (defun my/search-forward ()
;;  (interactive)
;;  (setq my/last-search (completing-read "Search: " nil))
;;  (search-forward-regexp my/last-search))

;; (defun my/search-backward ()
;;  (interactive)
;;  (setq my/last-search (completing-read "Search: " nil))
;;  (search-backward-regexp my/last-search))

;; (defun my/repeat-search-forward ()
;;  (interactive)
;;  (search-forward-regexp my/last-search))

;; (defun my/repeat-search-backward ()
;;  (interactive)
;;  (search-backward-regexp my/last-search))

;; (my/evil-normal-define-key "/" 'my/search-forward)
;; (my/evil-visual-define-key "/" 'my/search-forward)

;; (my/evil-normal-define-key "?" 'my/search-backward)
;; (my/evil-visual-define-key "?" 'my/search-backward)

;; (my/evil-normal-define-key "j" 'my/repeat-search-forward)
;; (my/evil-visual-define-key "j" 'my/repeat-search-forward)

;; (my/evil-normal-define-key "J" 'my/repeat-search-backward)
;; (my/evil-visual-define-key "J" 'my/repeat-search-backward)

;; ** Elmacro
;; Elmacro doesn't work with evil
;; (straight-use-package 'elmacro)
;; (require 'elmacro)

;; (define-globalized-minor-mode my/elmacro-global-mode elmacro-mode
;; (lambda ()
;; (elmacro-mode 1)))

;; (my/elmacro-global-mode 1)

;; *** Macro mechanisms
;; (defun my/get-interactive-commands ()
;; (let ((cmds  ()))
;; (mapatoms (lambda (s) (when (commandp s) (push s cmds))))
;; cmds))

;; (defvar my/current-macro-name "")
;; (defvar my/current-macro-number 0)

;; (defun my/macro-record-toggle ()
;; (interactive)
;; (if elmacro-mode
;; (my/macro-record-stop)
;; (my/macro-record)))

;; (defun my/macro-record ()
;; (setq my/current-macro-number (+ my/current-macro-number 1))
;; (setq my/current-macro-name (concat "my/rec-macro-" (number-to-string my/current-macro-number) "-" (completing-read "Macro name: " nil)))
;; (elmacro-mode 1)
;; (call-interactively 'kmacro-start-macro-or-insert-counter))

;; (defun my/macro-record-stop ()
;; (call-interactively 'kmacro-end-or-call-macro)
;; (elmacro-show-last-macro my/current-macro-name)
;; (elmacro-mode -1)
;; (eval-buffer)
;; )

;; (defun my/macro-run()
;; (interactive)
;; (call-interactively
;; (intern (ivy-read "Macros: " (reverse (sort (my/get-interactive-commands) 'string-lessp))
;; :initial-input "^my/rec-macro- "))))

;; Get thing
;; (call-interactively  (intern "counsel-M-x"))

;; (my/evil-normal-define-key "q" 'my/macro-record-toggle)
;; (my/evil-normal-define-key "Q" 'my/macro-run)

;; (my/evil-visual-define-key "q" 'my/macro-record-toggle)
;; (my/evil-visual-define-key "Q" 'my/macro-run)

;; * Encryption
;; ** GPG
;; *** Pinentry
(straight-use-package 'pinentry)

(if window-system
    (add-hook 'exwm-init-hook 'pinentry-start)
  (pinentry-start))

;; *** Reset GPG agent
(defun my/reset-gpg-agent ()
  (interactive)
  (shell-command "gpgconf --kill gpg-agent")
  (pinentry-stop)
  (pinentry-start))

;; ** Passwords
;; Enable org mode for .org.gpg files
(add-to-list 'auto-mode-alist '("\\.org.gpg\\'" . org-mode))

;; *** Espy
(straight-use-package 'espy)

(setq espy-password-file "~/pass.org.gpg")

(define-prefix-command 'my/password-map)
(define-key my/leader-map (kbd "p") 'my/password-map)

(define-key my/password-map (kbd "r") 'my/reset-gpg-agent)
(define-key my/password-map (kbd "u") 'espy-get-user)
(define-key my/password-map (kbd "p") 'espy-get-pass)

;; *** Auto-clean kill ring
;; (defvar my/pass-in-killring nil)

;; (defun my/ivy-pass ()
;; (interactive)
;; (setq my/pass-in-killring t)
;; (ivy-pass))

;; (defun my/pass-pop-killring ()
;; (if (eq my/pass-in-killring t)
;; (progn
;; (progn (pop kill-ring)
;; (message "Password removed"))
;; (setq my/pass-in-killring nil))))

(defun my/pop-killring ()
  (pop kill-ring)
  (setq my/pass-in-killring nil))

(define-key my/leader-map (kbd "C-k") 'my/pop-killring)
;; (advice-add 'evil-goggles--paste-advice :before (lambda () (interactive) (my/pass-pop-killring)))
;; (advice-add 'evil-goggles--paste-advice :before
;; (advice-add 'evil-paste-after :after (lambda (&rest r) (interactive) (my/pass-pop-killring)))
;; (advice-add 'evil-paste-before :after (lambda (&rest r) (interactive) (my/pass-pop-killring)))

;; * Terms
;; ** Set max lines to a lot
(setq term-buffer-maximum-size 10000)

;; ** Ansi term
(add-hook 'term-mode-hook (lambda () (interactive) (setq truncate-lines t)))

;; *** Keys
;; (my/evil-universal-define-key-in-mode 'term-raw-map "C-," 'term-char-mode)

;; * Eshell
;;  https://github.com/howardabrams/dot-files/blob/master/emacs-eshell.org
(require 'eshell)
;; Change to temporary name before renaming
(setq eshell-buffer-name "eshell")

(setq eshell-highlight-prompt t)
(setq eshell-hist-ignoredups t)
(setq eshell-history-size 10000)

(setq-default eshell-status-in-mode-line nil)

(defun my/eshell ()
  (interactive)
  (eshell)
  (my/give-buffer-unique-name "*eshell*"))

;; ** Prefer lisp to bash
(setq eshell-prefer-lisp-functions t)
(setq eshell-prefer-lisp-variables t)

;; ** Use tramp for sudo
(require 'em-tramp)
(defalias 'sudo 'eshell/sudo)

;; ** Autocompletion
;; (defun company-eshell-history (command &optional arg &rest ignored)
;; (interactive (list 'interactive))
;; (cl-case command
;; (interactive (company-begin-backend 'company-eshell-history))
;; (prefix (and (eq major-mode 'eshell-mode)
;; (let ((word (company-grab-word)))
;; (save-excursion
;; (eshell-bol)
;; (and (looking-at-p (s-concat word "$")) word)))))
;; (candidates (remove-duplicates
;; (->> (ring-elements eshell-history-ring)
;; (remove-if-not (lambda (item) (s-prefix-p arg item)))
;; (mapcar 's-trim))
;; :test 'string=))
;; (sorted t)))

;; (add-to-list 'company-backends 'company-eshell-history)

;; ** Eldoc
;; (straight-use-package 'esh-help)

;; (require 'esh-help)
;; (setup-esh-help-eldoc)

;; ** Did you mean
;; (straight-use-package 'eshell-did-you-mean)

;; (require 'eshell-did-you-mean)
;; (eshell-did-you-mean-setup)

;; ** Aliases
(defun eshell/f (file)
  (find-file file))

;; ** Clear
;; Default eshell/clear only spams newlines
(defun eshell/clear ()
  "Clear the eshell buffer."
  (let ((inhibit-read-only t))
    (erase-buffer)
    ;; Probably not needed
    ;;(eshell-send-input)
    ))

;; ** Term
(define-key my/leader-map (kbd "{") 'ansi-term)

;; ** Use ansi term for certain applications
(require 'em-term)
(add-to-list 'eshell-visual-commands "vim")
(add-to-list 'eshell-visual-commands "wifi-menu")
(add-to-list 'eshell-visual-commands "htop")

(setq eshell-visual-subcommands
      '(("git" "log" "diff" "show")
	("sudo" "wifi-menu")
	("sudo" "htop")
	("sudo" "vi" "visudo")))

;; ** Remove banner
(setq eshell-banner-message "")

;; ** Custom prompt
(defvar my/eshell-prompt-symbol)
(if window-system
    (setq my/eshell-prompt-symbol "λ")
  (setq my/eshell-prompt-symbol "$"))

(setq eshell-prompt-function
      (lambda ()
	(concat (abbreviate-file-name (eshell/pwd))
		(if (= (user-uid) 0) " # " (concat " " my/eshell-prompt-symbol " ")))))

(setq eshell-prompt-regexp
      (concat "^[^#$\n]* [#" my/eshell-prompt-symbol "] "))

;; ** Alert when task is done
(add-hook 'eshell-post-command-hook (lambda () (interactive)
				      (if (not (= 1 (line-number-at-pos (point))))
					  (progn
					    (my/alert nil 'low)
					    (message "Eshell command done!")))))

;; ** Custom Goto beg of line
(defun my/eshell-goto-beg-of-line ()
  (interactive)
  (let ((start-point (point)))
    (if (and
	 (re-search-backward my/eshell-prompt-symbol nil t) (= (line-number-at-pos) (line-number-at-pos start-point)))
	(evil-forward-char 2)
      (goto-char start-point)
      (evil-digit-argument-or-evil-beginning-of-line))))

;; ** With-editor
(straight-use-package 'with-editor)

(add-hook 'eshell-mode 'with-editor-export-editor)

;; ** Keys
(define-key my/leader-map (kbd "[") 'my/eshell)

;; Eshell keys are buffer local... Bind them on startup
(defun my/bind-eshell-keys ()
  (define-prefix-command 'my/eshell-mode-map)
  (evil-define-key 'normal eshell-mode-map (kbd (concat my/leader-map-key " a")) 'my/eshell-mode-map)
  
  (evil-define-key 'normal eshell-mode-map (kbd "RET") 'eshell-send-input)
  (define-key eshell-mode-map [remap evil-ret] 'eshell-send-input)
  
  (define-key my/eshell-mode-map (kbd "k") 'counsel-esh-history)
  
  (evil-define-key 'normal eshell-mode-map (kbd "0") 'my/eshell-goto-beg-of-line)

  (evil-define-key '(normal insert visual replace) eshell-mode-map (kbd "C-c") 'eshell-kill-process)
  
  (evil-define-key '(normal insert) eshell-mode-map (kbd "C-p") 'eshell-previous-matching-input-from-input)
  (evil-define-key '(normal insert) eshell-mode-map (kbd "C-n") 'eshell-next-matching-input-from-input))

(add-hook 'eshell-mode-hook 'my/bind-eshell-keys)

;; * Keys
;; ** Clean default maps
;; *** Clean global keys

;; ** Key rebinds
(require 'evil-maps)

;; *** General
(straight-use-package 'general)

(general-evil-setup)

;; *** Language specific symbols
;; **** Lower
(my/evil-emacs-define-key "M-a" '(lambda () (interactive) (my/exwm-fake-key "å")))
(my/evil-emacs-define-key "M-e" '(lambda () (interactive) (my/exwm-fake-key "ä")))
(my/evil-emacs-define-key "M-o" '(lambda () (interactive) (my/exwm-fake-key "ö")))

(my/evil-universal-define-key "M-a" '(lambda () (interactive) (my/fake-key "å" ?\å)))
(my/evil-universal-define-key "M-e" '(lambda () (interactive) (my/fake-key "ä" ?\ä)))
(my/evil-universal-define-key "M-o" '(lambda () (interactive) (my/fake-key "ö" ?\ö)))

;; **** Capital
(my/evil-emacs-define-key "M-A" '(lambda () (interactive) (my/exwm-fake-key "Å")))
(my/evil-emacs-define-key "M-E" '(lambda () (interactive) (my/exwm-fake-key "Ä")))
(my/evil-emacs-define-key "M-O" '(lambda () (interactive) (my/exwm-fake-key "Ö")))

(my/evil-universal-define-key "M-A" '(lambda () (interactive) (my/fake-key "Å" ?\Å)))
(my/evil-universal-define-key "M-E" '(lambda () (interactive) (my/fake-key "Ä" ?\Ä)))
(my/evil-universal-define-key "M-O" '(lambda () (interactive) (my/fake-key "Ö" ?\Ö)))

;; *** Backspace/delete C-h, C-l
(define-key evil-insert-state-map (kbd "C-f") 'backward-delete-char-untabify)
(define-key evil-insert-state-map (kbd "C-l") 'delete-char)

(define-key evil-replace-state-map (kbd "C-f") 'backward-delete-char-untabify)
(define-key evil-replace-state-map (kbd "C-l") 'delete-char)

(define-key evil-normal-state-map (kbd "<backspace>") 'my/alert)
(define-key evil-insert-state-map (kbd "<backspace>") 'my/alert)

(define-key evil-replace-state-map (kbd "DEL") 'my/alert)

;; *** k(Move up) <--> p(Paste)
;; **** k
(my/evil-normal-define-key "k" 'evil-paste-after)
(my/evil-normal-define-key "K" 'evil-paste-before)

(define-key evil-visual-state-map "k" 'evil-visual-paste)

;; Universal paste key
(global-set-key (kbd "C-k") 'evil-paste-after)
(global-set-key (kbd "C-K") 'evil-paste-before)
(define-key evil-insert-state-map (kbd "C-k") 'evil-paste-after)
(define-key evil-insert-state-map (kbd "C-K") 'evil-paste-before)

(define-key evil-window-map "k" 'evil-window-mru)

;; **** p
(define-key evil-window-map "p" 'evil-window-up)
(define-key evil-window-map "P" 'evil-window-move-very-top)

(my/evil-normal-define-key "p" 'evil-previous-line)

;; Rebind to make consistent with N
(my/evil-normal-define-key "P" 'delete-indentation)

(define-key evil-window-map (kbd "C-S-p") 'evil-window-move-very-top)

;; *** n(search-next) <--> j(Move up)
;; **** n
(my/evil-normal-define-key "n" 'evil-next-line)

(my/evil-normal-define-key "N" 'evil-join)

;; ex
;;  (evil-ex-define-cmd "j[oin]" 'evil-ex-join)
;;  (evil-ex-define-cmd "ju[mps]" 'evil-show-jumps)

(define-key evil-window-map (kbd "C-S-n") 'evil-window-move-very-bottom)

;; **** j
;; (my/evil-normal-define-key "j" 'evil-search-next)
;; (my/evil-normal-define-key "J" 'evil-search-previous)

;; (my/evil-normal-define-key "j" 'isearch-repeat-forward)
;; (my/evil-normal-define-key "J" 'isearch-repeat-backward)

;; ex
;; (evil-ex-define-cmd "new" 'evil-window-new)
;; (evil-ex-define-cmd "norm[al]" 'evil-ex-normal)
;; (evil-ex-define-cmd "noh[lsearch]" 'evil-ex-nohighlight)

(my/evil-normal-define-key "gj" 'evil-next-match)
(my/evil-normal-define-key "gJ" 'evil-previous-match)

;; *** Rebind search key
;; (my/evil-normal-define-key "s" 'my/avy-goto-subword-0-below)
;; (my/evil-normal-define-key "S" 'my/avy-goto-subword-0-above)

;; (my/evil-visual-define-key "s" 'my/avy-goto-subword-0-below)
;; (my/evil-visual-define-key "S" 'my/avy-goto-subword-0-above)

(my/evil-normal-define-key "s" 'my/isearch-forward-regexp)
(my/evil-normal-define-key "S" 'my/isearch-backward-regexp)

(my/evil-visual-define-key "s" 'my/isearch-forward-regexp)
(my/evil-visual-define-key "S" 'my/isearch-backward-regexp)

(my/evil-normal-define-key "g n" 'isearch-repeat-forward)
(my/evil-normal-define-key "g p" 'isearch-repeat-backward)

(my/evil-visual-define-key "g n" 'isearch-repeat-forward)
(my/evil-visual-define-key "g p" 'isearch-repeat-backward)

;; (my/evil-normal-define-key "/" 'evil-substitute)
;; (my/evil-normal-define-key "?" 'evil-change-whole-line)

;; (define-key evil-visual-state-map "/" 'evil-substitute)
;; (define-key evil-visual-state-map "?" 'evil-change-whole-line)

;; *** Rebind save key
;; (defun my/save-and-backup-buffer()
;; (interactive)
;; (my/backup-buffer)
;; (my/fake-open-keymap "C-x")
;; (my/fake-key (kbd "C-s") ?\C-s)
;; )

(general-simulate-key "C-x C-s")

(defun my/save-and-backup-buffer()
  (interactive)
  (my/backup-buffer-per-session)
  (my/backup-original-buffer)
  (general-simulate-C-x_C-s))

(define-key my/leader-map (kbd "s") 'my/save-and-backup-buffer)
(define-key my/leader-map (kbd "C-s") 'write-file)

;; *** Rebind C-d
(my/evil-normal-define-key "C-d" nil)

;; *** Rebind esc
(define-key key-translation-map (kbd "<escape>") (kbd "C-e"))
(define-key key-translation-map (kbd "C-e") (kbd "<escape>"))

;; *** Rebind enter
;;  (define-key key-translation-map (kbd "RET") (kbd "C-a"))
(define-key key-translation-map (kbd "C-a") (kbd "RET"))

;; *** Rebind tab
;; (define-key my/keys-mode-map (kbd "C-e") 'my/simulate-esc)
;; (define-key key-translation-map (kbd "?\\t") (kbd "C-="))

;; If window system, unbind tab key and not C-=

(define-key key-translation-map (kbd "TAB") (kbd "C-="))
(define-key key-translation-map (kbd "<tab>") (kbd "C-="))
(define-key key-translation-map (kbd "C-t") (kbd "TAB"))
(define-key key-translation-map (kbd "M-C-t") (kbd "C-TAB"))

;; (when window-system
;;  (define-key key-translation-map (kbd "TAB") (kbd "C--"))
;;  (define-key key-translation-map (kbd "<tab>") (kbd "C--"))

;;  (define-key key-translation-map (kbd "C-i") (kbd "C-~")))

;; *** Disable backspace
;; (define-key key-translation-map (kbd "C-e") (kbd "TAB"))
;; (define-key key-translation-map (kbd "M-C-i") (kbd "C-TAB"))

;; * nix
;; ** Direnv
(straight-use-package 'direnv)
(direnv-mode)

;; ** Nix-mode
(straight-use-package 'nix-mode)

;; ** Nix-options company
(straight-use-package 'company-nixos-options)
;; I can't find a pure add-to-list so i have to copy it so that company-backends isn't modified
(add-hook 'nix-mode-hook '(lambda () (interactive)
			    (let ((list company-backends))
			      (add-to-list 'list 'company-nixos-options)
			      (setq-local company-backends list))))

;; ** Pretty sha paths
;;  (straight-use-package 'pretty-sha-path)

;;  (add-hook 'eshell-mode-hook 'pretty-sha-path-mode)

;; * exwm
;; ** Keys before exwm init
;; Reset exwm-mode map
(setq exwm-mode-map (make-sparse-keymap))

(define-key evil-emacs-state-map (kbd "TAB") nil)
(global-unset-key (kbd "TAB"))

;; Rebind keys in exwm bufffers
(setq exwm-input-simulation-keys
      '(
	;; Delete char
	([?\C-f] . [delete])
	([?\C-l] . [backspace])
	;; movement
	([?\C-p] . [up])
	([?\C-n] . [down])
	
	;; ([?\C-u] . [prior])
	;; ([?\C-w] . [next])
	([?\C-w] . [?\C-d])
	
	([?\C-s] . [?\C-f])
	
	([?\C-a] . [return])
	([?\r] . [return])
	
	;;([?\C-e] . [?\C-[])
	;; ([?\C-e] . [escape])
	;; ([?\e] . [escape])
	
	([?\C-t] . [tab])
	([?\t] . [tab])
	
	([?\C-g] . [escape])
	;;([?\e] . [escape])
	
	;; Firefox hard-coded open url hotkey
	;;([?\C-o] . [f6])
	
	;; Redo
	([?\C-r] . [?\C-y])
	;; Undo
	([?\M-u] . [?\C-z])
	
	;; cut/paste.
	([?\C-y] . [?\C-c])
	([?\C-k] . [?\C-v])
	
	([?\M-a] . [?\C-å])
	([?\M-e] . [?\C-ä])
	([?\M-o] . [?\C-ö])
	
	([?\M-A] . [?\C-Å])
	([?\M-E] . [?\C-Ä])
	([?\M-O] . [?\C-Ö])
	
	([?\C-c] . [?\C-c])))

(setq exwm-input-prefix-keys nil)
;; Exwm don't send back these keys
(dolist (k '(
	     XF86AudioLowerVolume
	     XF86AudioRaiseVolume
	     XF86PowerOff
	     XF86AudioMute
	     XF86AudioPlay
	     XF86AudioStop
	     XF86AudioPrev
	     XF86AudioNext
	     XF86ScreenSaver
	     XF68Back
	     XF86Forward
	     Scroll_Lock
	     print
	     ))
  (cl-pushnew k exwm-input-prefix-keys))

(setq exwm-input-global-keys nil)

;; ** Core
;; https://emacs.stackexchange.com/questions/33326/how-do-i-cut-and-paste-effectively-between-applications-while-using-exwm
(straight-use-package 'exwm)

(require 'exwm)
;; (require 'exwm-config)

;; enable exwm
(exwm-enable)

;; (add-hook 'exwm-manage-finish-hook 'my/exwm-mode)
;; (defun my/exwm-mode ()
;;  (interactive)
;;  ())

;; ** Exwm-edit
(setq exwm-edit-bind-default-keys nil)
(straight-use-package '(exwm-edit :type git :host github :repo "walseb/exwm-edit"))
(require 'exwm-edit)

;; ** Set exwm buffer name
;; *** Manually set buffer name
(defun my/exwm-set-window-name ()
  (interactive)
  (exwm-workspace-rename-buffer (completing-read "set title " nil)))

(define-key my/file-options-map (kbd "r") 'my/exwm-set-window-name)

;; *** Auto set buffer name
;; We use class names for all windows expect for Java applications and GIMP (because of problems with those).
(defun my/exwm-should-use-title-for-buffer-name ()
  (or
   (not exwm-instance-name)
   (string-prefix-p "sun-awt-X11-" exwm-instance-name)
   (string= "gimp" exwm-instance-name)
   ;; Firefox calls itself navigator?
   (string-prefix-p "Navigator" exwm-instance-name t)
   ;;(string-prefix-p "Firefox" exwm-instance-name t)
   ))

(add-hook 'exwm-update-class-hook 'my/exwm-buffer-give-name-class)

(defun my/exwm-buffer-give-name-class ()
  (when (not (my/exwm-should-use-title-for-buffer-name))
    (exwm-workspace-rename-buffer exwm-class-name)))

(add-hook 'exwm-update-title-hook 'my/exwm-buffer-give-name-title)

(defun my/exwm-buffer-give-name-title ()
  (when (my/exwm-should-use-title-for-buffer-name)
    (exwm-workspace-rename-buffer exwm-title)))

;; ** Multi-screen
(if my/enable-randr
    (require 'exwm-randr))

;; *** Get monitor setup
(defun my/exwm-randr-auto-get-monitor ()
  (let* ((result)
	 (monitors (nth 1 (exwm-randr--get-monitors))))
    (dotimes (i (/ (length monitors) 2))
      (push (nth (* i 2) monitors) result)
      (push i result))
    result))

;; Get monitor setup
(if my/enable-randr
    (if my/device/monitor-setup
	(progn
	  (setq exwm-workspace-number (/ (length my/device/monitor-setup) 2))
	  (setq exwm-randr-workspace-monitor-plist my/device/monitor-setup))))
;; (let ((monitor-setup (my/exwm-randr-auto-get-monitor)))
;; (setq exwm-workspace-number (/ (length monitor-setup) 2))
;; (setq exwm-randr-workspace-monitor-plist monitor-setup)))

;; *** Enable
(if (and my/enable-randr (> exwm-workspace-number 1))
    (progn
      (exwm-randr-enable)))

;; *** Switch monitor (workspace) functions
;; `exwm-workspace-number' is equal to monitor count
;; (add-hook 'focus-out-hook 'exwm-layout--refresh)
(defun my/switch-monitor-left ()
  (interactive)
  (if (>= exwm-workspace-current-index (- exwm-workspace-number 1))
      (exwm-workspace-switch-create 0)
    (exwm-workspace-switch-create (+ exwm-workspace-current-index 1))))

(defun my/switch-monitor-right ()
  (interactive)
  (if (= exwm-workspace-current-index 0)
      (exwm-workspace-switch-create (- exwm-workspace-number 1))
    (exwm-workspace-switch-create (- exwm-workspace-current-index 1))))

;; (define-key my/keys-mode-map (kbd "M-l") 'my/switch-monitor-right)
;; (define-key my/keys-mode-map (kbd "M-h") 'my/switch-monitor-left)

;; ** Setting
;; Garbage collect when entering x window (because x is not in sync with emacs)
;; (add-hook 'exwm-mode-hook 'garbage-collect)

(setq exwm-workspace-show-all-buffers t)

(setq exwm-workspace-minibuffer-position 'top)

(add-hook 'exwm-init-hook (lambda () (interactive) (exwm-workspace-attach-minibuffer)))

;; ** Keys
(exwm-input-set-key (kbd my/mod-leader-map-key) 'my/leader-map)

(exwm-input-set-key (kbd "M-<tab>") 'my/toggle-switch-to-minibuffer)

(exwm-input-set-key (kbd "C-e") 'keyboard-quit)
(exwm-input-set-key (kbd "<tab>") 'my/window-hydra/body)
(exwm-input-set-key (kbd "C-=") 'my/window-hydra/body)

(exwm-input-set-key (kbd "M-x") 'counsel-M-x)

;;(exwm-input-set-key (kbd "M-w") '(lambda () (interactive) (exwm-input--fake-key ?\å)))
;;(exwm-input-set-key (kbd "M-r") '(lambda () (interactive) (exwm-input--fake-key ?\ä)))
;;(exwm-input-set-key (kbd "M-j") '(lambda () (interactive) (exwm-input--fake-key ?\ö)))

;; * Shr
(require 'shr)

;; *** Fix background colors shr
;; Try fixing colors
;; (setq shr-color-visible-luminance-min 80)
;; (setq shr-color-visible-distance-min 5)

;; Fully disables colors
(advice-add #'shr-colorize-region :around (defun shr-no-colourise-region (&rest ignore)))

;; *** Auto-open image at point
;; Redefine function to attempt to open image if link at point wasn't found
(el-patch-feature shr)
(el-patch-defun shr-browse-url (&optional external mouse-event)
  "Browse the URL at point using `browse-url'.
   If EXTERNAL is non-nil (interactively, the prefix argument), browse
   the URL using `shr-external-browser'.
   If this function is invoked by a mouse click, it will browse the URL
   at the position of the click.  Optional argument MOUSE-EVENT describes
   the mouse click event."
  (interactive (list current-prefix-arg last-nonmenu-event))
  (mouse-set-point mouse-event)
  (let ((url (get-text-property (point) 'shr-url)))
    (cond
     ((not url)
      ;; Was unsuccessful in opening link, attempt to open image
      (shr-browse-image))
     ((string-match "^mailto:" url)
      (browse-url-mail url))
     (t
      (if external
	  (funcall shr-external-browser url)
	(browse-url url))))))

;; * Browser
;; ** Eww/shr
(require 'eww)


;; *** Add URL to buffer name
(add-hook 'eww-after-render-hook '(lambda () (interactive) (my/give-buffer-unique-name (concat "eww - " (plist-get eww-data :title)))))

;; *** Keys
;; (define-key eww-mode-map [?\d] 'eww-back-url)
;; (evil-define-key 'normal eww-mode-map [?\d] 'eww-back-url)
;; (evil-define-key 'visual eww-mode-map [?\d] 'eww-back-url)

(evil-define-key 'normal eww-mode-map (kbd "w") 'evil-forward-word-begin)
(setq shr-map (make-sparse-keymap))

(evil-define-key 'normal eww-mode-map (kbd "H") 'eww-back-url)
(evil-define-key 'normal eww-mode-map (kbd "L") 'eww-forward-url)

(define-prefix-command 'my/eww-mode-map)
(evil-define-key 'normal eww-mode-map (kbd (concat my/leader-map-key " a")) 'my/eww-mode-map)

(define-key my/eww-mode-map (kbd "d") 'eww-download)
(define-key my/eww-mode-map (kbd "h") 'eww-history-browse)
(define-key my/eww-mode-map (kbd "o") 'eww-open-in-new-buffer)
(define-key my/eww-mode-map (kbd "r") 'eww-reload)
(define-key my/eww-mode-map (kbd "f") 'eww-open-file)
;; (define-key my/eww-mode-map (kbd "C-c") 'my/eww-toggle-code-highlighting)

;; ** Firefox exwm integration
(eval-after-load 'exwm
  (progn
    (straight-use-package 'exwm-firefox-core)
    (straight-use-package 'exwm-firefox-evil)
    (require 'exwm-firefox-evil)
    
    ;; Auto enable exwm-firefox-evil-mode on all firefox buffers
    (add-hook 'exwm-manage-finish-hook 'exwm-firefox-evil-activate-if-firefox)
    
    ;; Run firefox buffers in normal mode
    (add-hook 'exwm-firefox-evil-mode-hook 'exwm-firefox-evil-normal)))

;; *** Open new window macro
(defun my/exwm-firefox-core-window-new ()
  (interactive)
  (exwm-firefox-core-window-new)
  (run-with-timer 0.5 nil 'exwm-firefox-core-focus-search-bar))

;; *** Keys
(eval-after-load 'exwm
  (progn
    (defun my/exwm-firefox-evil-link-hint ()
      (interactive)
      (exwm-input--fake-key ?f)
      (exwm-input-send-next-key 2))
    
       ;;; Normal
    (evil-define-key '(normal motion) exwm-firefox-evil-mode-map (kbd "f") 'my/exwm-firefox-evil-link-hint)
    
    (evil-define-key '(normal motion) exwm-firefox-evil-mode-map (kbd "C-p") 'exwm-firefox-core-up)
    (evil-define-key '(normal motion) exwm-firefox-evil-mode-map (kbd "C-n") 'exwm-firefox-core-down)
    (evil-define-key '(normal motion) exwm-firefox-evil-mode-map (kbd "p") 'exwm-firefox-core-up)
    (evil-define-key '(normal motion) exwm-firefox-evil-mode-map (kbd "n") 'exwm-firefox-core-down)
    (evil-define-key '(normal motion) exwm-firefox-evil-mode-map (kbd "C-l") 'exwm-firefox-core-right)
    (evil-define-key '(normal motion) exwm-firefox-evil-mode-map (kbd "C-f") 'exwm-firefox-core-left)
    (evil-define-key '(normal motion) exwm-firefox-evil-mode-map (kbd "p") 'exwm-firefox-core-up)
    (evil-define-key '(normal motion) exwm-firefox-evil-mode-map (kbd "n") 'exwm-firefox-core-down)
    (evil-define-key '(normal motion) exwm-firefox-evil-mode-map (kbd "C-p") 'exwm-firefox-core-up)
    (evil-define-key '(normal motion) exwm-firefox-evil-mode-map (kbd "C-n") 'exwm-firefox-core-down)
    (evil-define-key '(normal motion) exwm-firefox-evil-mode-map (kbd "C-w") 'exwm-firefox-core-half-page-down)
    (evil-define-key '(normal motion) exwm-firefox-evil-mode-map (kbd "N") 'exwm-firefox-core-tab-next)
    (evil-define-key '(normal motion) exwm-firefox-evil-mode-map (kbd "P") 'exwm-firefox-core-tab-previous)
    (evil-define-key '(normal motion) exwm-firefox-evil-mode-map (kbd "s") 'exwm-firefox-core-tab-close)
    (evil-define-key '(normal motion) exwm-firefox-evil-mode-map (kbd "j") 'exwm-firefox-core-find-next)
    (evil-define-key '(normal motion) exwm-firefox-evil-mode-map (kbd "J") 'exwm-firefox-core-find-previous)
    (evil-define-key '(normal motion) exwm-firefox-evil-mode-map (kbd "C-s") 'exwm-firefox-core-quick-find)
    (evil-define-key '(normal motion) exwm-firefox-evil-mode-map (kbd "C-y") 'exwm-firefox-core-copy)
    (evil-define-key '(normal motion) exwm-firefox-evil-mode-map (kbd "C-k") 'exwm-firefox-core-paste)
    (evil-define-key '(normal motion) exwm-firefox-evil-mode-map (kbd "t") 'exwm-firefox-core-tab-new)
    ;;(evil-define-key '(normal motion) exwm-firefox-evil-mode-map (kbd "t") 'my/exwm-firefox-core-window-new)
    
       ;;; Visual
    (evil-define-key 'visual exwm-firefox-evil-mode-map (kbd "p") 'exwm-firefox-core-up-select)
    (evil-define-key 'visual exwm-firefox-evil-mode-map (kbd "n") 'exwm-firefox-core-down-select)
    
    (evil-define-key 'visual exwm-firefox-evil-mode-map (kbd "C-p") 'exwm-firefox-core-up-select)
    (evil-define-key 'visual exwm-firefox-evil-mode-map (kbd "C-n") 'exwm-firefox-core-down-select)
    
    (evil-define-key 'visual exwm-firefox-evil-mode-map (kbd "C-w") 'exwm-firefox-core-half-page-down-select)
    
    (evil-define-key 'visual exwm-firefox-evil-mode-map (kbd "j") 'exwm-firefox-core-find-next)
    (evil-define-key 'visual exwm-firefox-evil-mode-map (kbd "J") 'exwm-firefox-core-find-previous)
    
    (evil-define-key 'visual exwm-firefox-evil-mode-map (kbd "C-y") 'exwm-firefox-core-copy)
    (evil-define-key 'visual exwm-firefox-evil-mode-map (kbd "C-k") 'exwm-firefox-core-paste)
    
       ;;; Insert
    (evil-define-key 'insert exwm-firefox-evil-mode-map (kbd "C-p") 'exwm-firefox-core-up-select)
    (evil-define-key 'insert exwm-firefox-evil-mode-map (kbd "C-n") 'exwm-firefox-core-down-select)
    
    (evil-define-key 'insert exwm-firefox-evil-mode-map (kbd "C-u") 'exwm-firefox-core-half-page-up)
    (evil-define-key 'insert exwm-firefox-evil-mode-map (kbd "C-w") 'exwm-firefox-core-half-page-down)
    
    (evil-define-key 'insert exwm-firefox-evil-mode-map (kbd "M-w") '(lambda () (interactive) (my/exwm-fake-key "å")))
    (evil-define-key 'insert exwm-firefox-evil-mode-map (kbd "M-r") '(lambda () (interactive) (my/exwm-fake-key "ä")))
    (evil-define-key 'insert exwm-firefox-evil-mode-map (kbd "M-j") '(lambda () (interactive) (my/exwm-fake-key "ö")))

    (evil-define-key 'insert exwm-firefox-evil-mode-map (kbd "M-W") '(lambda () (interactive) (my/exwm-fake-key "Å")))
    (evil-define-key 'insert exwm-firefox-evil-mode-map (kbd "M-R") '(lambda () (interactive) (my/exwm-fake-key "Ä")))
    (evil-define-key 'insert exwm-firefox-evil-mode-map (kbd "M-J") '(lambda () (interactive) (my/exwm-fake-key "Ö")))
    
    (evil-define-key 'insert exwm-firefox-evil-mode-map (kbd "C-y") 'exwm-firefox-core-copy)
    (evil-define-key 'insert exwm-firefox-evil-mode-map (kbd "C-k") 'exwm-firefox-core-paste)
    (evil-define-key 'insert exwm-firefox-evil-mode-map (kbd "C-l") '(lambda () (interactive) (exwm-input--fake-key 'delete)))
    (evil-define-key 'insert exwm-firefox-evil-mode-map (kbd "C-f") '(lambda () (interactive) (exwm-input--fake-key 'backspace)))))


;; ** Next browser
;; (defun my/write-next-config ()
;; (my/create-dir-if-not-exist "~/.config")
;; (my/create-dir-if-not-exist "~/.config/next")
;; (my/create-file-with-content-if-not-exist "~/.config/next/init.lisp")

;; )

;; ** Set default browser
(setq-default browse-url-browser-function 'eww-browse-url)

;; * Version control
;; ** Ediff
(require 'ediff)
(setq-default ediff-forward-word-function 'forward-char)

;; Fixes exwm bug too?
(setq ediff-window-setup-function 'ediff-setup-windows-plain)

(setq ediff-split-window-function 'split-window-horizontally)

;; *** A and B to Ancestor
(defun ediff-copy-both-to-C ()
  (interactive)
  (ediff-copy-diff ediff-current-difference nil 'C nil
		   (concat
		    (ediff-get-region-contents ediff-current-difference 'A ediff-control-buffer)
		    (ediff-get-region-contents ediff-current-difference 'B ediff-control-buffer))))

;; *** Ediff-dired
;; https://oremacs.com/2017/03/18/dired-ediff/
(defun my/ediff-dired ()
  (interactive)
  (let ((files (dired-get-marked-files))
	(wnd (current-window-configuration)))
    (if (<= (length files) 2)
	(let ((file1 (car files))
	      (file2 (if (cdr files)
			 (cadr files)
		       (read-file-name
			"file: "
			(dired-dwim-target-directory)))))
	  (if (file-newer-than-file-p file1 file2)
	      (ediff-files file2 file1)
	    (ediff-files file1 file2)))
      (error "no more than 2 files should be marked"))))

;; *** Keys
(setq-default ediff-mode-map (make-sparse-keymap))

(define-prefix-command 'my/ediff-mode-map)
(evil-define-key 'normal ediff-mode-map (kbd (concat my/leader-map-key " a")) 'my/ediff-mode-map)
(define-key ediff-mode-map "p" 'ediff-previous-difference)
(define-key ediff-mode-map "n" 'ediff-next-difference)

;; ** Projectile
(straight-use-package 'projectile)

;; Disable projectile mode so that CPU isn't taken by projectile wating to refresh git project directory all the time
(projectile-mode -1)

;; ** Counsel projectile
;; If enabled it auto enables projectile, which has high CPU usage
(straight-use-package 'counsel-projectile)

;; ** Magit
(straight-use-package 'magit)

(setq git-commit-summary-max-length 50)

;; *** Performance
;; Disable magit in commit view, makes it possible to do large commits
(setq magit-commit-show-diff nil)

;; *** Diff
(require 'magit-diff)
(setq-default magit-diff-refine-hunk 'all)
;; (setq-default magit-diff-refine-ignore-whitespace nil)

;; *** Forge
(straight-use-package '(forge :type git :host github :repo "magit/forge"))

;; *** Keys
;; **** General
(require 'magit)
(require 'magit-mode)

(evil-define-key '(normal motion) magit-mode-map (kbd "0") #'magit-diff-default-context)
(evil-define-key '(normal motion) magit-mode-map (kbd "1") #'magit-section-show-level-1)
(evil-define-key '(normal motion) magit-mode-map  (kbd "2") #'magit-section-show-level-2)
(evil-define-key '(normal motion) magit-mode-map  (kbd "3") #'magit-section-show-level-3)
(evil-define-key '(normal motion) magit-mode-map  (kbd "4") #'magit-section-show-level-4)

;; Can't unbind "s"?
;; (my/evil-normal-define-key-in-mode magit-mode-map  "s" 'isearch-forward)
;; (define-key magit-status-mode-map "s" 'isearch-forward)
;; (my/evil-normal-define-key-in-mode magit-status-mode-map  "s" 'isearch-forward)
;; (my/evil-normal-define-key-in-mode magit-untracked-section-map  "s" 'isearch-forward)

;; ** diff-hl
(straight-use-package 'diff-hl)

(global-diff-hl-mode)

;; If there is no fringe (terminal), use margin instead
(unless (display-graphic-p) (diff-hl-margin-mode))

(add-hook 'dired-mode-hook 'diff-hl-dired-mode)

(add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh)

(setq diff-hl-draw-borders nil)

;; ** Keys
(evil-define-key 'insert magit-mode-map (kbd "a") 'evil-append)

(define-prefix-command 'my/vc-map)
(define-key my/leader-map (kbd "v") 'my/vc-map)

(define-key my/vc-map (kbd "n") 'diff-hl-next-hunk)
(define-key my/vc-map (kbd "p") 'diff-hl-previous-hunk)

(define-key my/vc-map (kbd "k") 'diff-hl-revert-hunk)
(define-key my/vc-map (kbd "=") 'diff-hl-diff-goto-hunk)

(define-key my/vc-map (kbd "e") 'counsel-projectile-find-file)
(define-key my/vc-map (kbd "s") 'counsel-git-grep)
;; (define-key my/vc-map (kbd "s") 'counsel-projectile-ag)
(define-key my/vc-map (kbd "d") 'projectile-dired)
(define-key my/vc-map (kbd "D") 'counsel-projectile-find-dir)

(define-key my/vc-map (kbd "K") 'projectile-kill-buffers)
(define-key my/vc-map (kbd "f") 'counsel-projectile-switch-to-buffer)
(define-key my/vc-map (kbd "F") 'projectile-ibuffer)

(define-key my/vc-map (kbd "O") 'projectile-save-project-buffers)
(define-key my/vc-map (kbd "C") 'projectile-compile-project)

(define-key my/vc-map (kbd "!") 'projectile-run-shell-command-in-root)
(define-key my/vc-map (kbd "&") 'projectile-run-async-shell-command-in-root)

(define-key my/vc-map (kbd "o") 'magit-status)

;; * Media
;; ** Volume keys
(defvar my/audio-sink nil)
;; Pulse sometimes first starts when a video, etc starts
(defun my/pulse-update-audio-sink ()
  (interactive)
  (setq my/audio-sink (substring (shell-command-to-string "pacmd list-sinks | grep \"\* index\"") (string-match "[0-9]" (shell-command-to-string "pacmd list-sinks | grep \"index\"")) -1)))

(defun my/pulse-mute-toggle ()
  (interactive)
  (my/pulse-update-audio-sink)
  (shell-command (concat "pactl set-sink-mute " my/audio-sink " toggle")))

(global-set-key (kbd "<XF86AudioMute>") 'my/pulse-mute-toggle)
(global-set-key (kbd "s-`") 'my/pulse-mute-toggle)

(defun my/pulse-raise-volume ()
  (interactive)
  (my/pulse-update-audio-sink)
  ;; Unmute
  (shell-command (concat "pactl set-sink-mute " my/audio-sink " 0"))
  (shell-command (concat "pactl set-sink-volume " my/audio-sink " +2.5%")))

(global-set-key (kbd "<XF86AudioRaiseVolume>") 'my/pulse-raise-volume)
(global-set-key (kbd "s-=") 'my/pulse-raise-volume)

(defun my/pulse-lower-volume ()
  (interactive)
  (my/pulse-update-audio-sink)
  ;; Unmute
  (shell-command (concat "pactl set-sink-mute " my/audio-sink " 0"))
  (shell-command (concat "pactl set-sink-volume " my/audio-sink " -2.5%")))

(global-set-key (kbd "<XF86AudioLowerVolume>") 'my/pulse-lower-volume)

(global-set-key (kbd "s--") 'my/pulse-lower-volume)

;; #+RESULTS:
;; : my/pulse-lower-volume

;; ** Music
(define-prefix-command 'my/music-map)
(define-key my/leader-map (kbd "m") 'my/music-map)

;; *** EMMS
;; Setup emms
(defvar my/emms-has-init nil)

(defvar my/emms-init-hook nil
  "Hook called when emms has to init fully")

(straight-use-package 'emms)


(add-hook 'my/emms-init-hook (lambda () (interactive)
			       (unless my/emms-has-init
				 (setq my/emms-has-init t)
				 (require 'emms-setup)
				 (require 'emms-player-mpd)
				 
				 (emms-all)
				 ;; Disable name of playing track in modeline (time is kept though)
				 (emms-mode-line-disable))))

(setq emms-mode-line-format nil)

(setq emms-seek-seconds 5)
(setq emms-player-list '(emms-player-mpd))
(setq emms-info-functions '(emms-info-mpd))

(setq emms-player-mpd-server-name "localhost")
(setq emms-player-mpd-server-port "6600")

;; (setq mpc-host "localhost:6600")

;; **** Sort by directory name instead of metadata
(setq emms-browser-get-track-field-function 'emms-browser-get-track-field-use-directory-name)

;; **** Open playlist
;; emms doesn't automatically connect to mpd when loading playlist, results in empty playlist
(defun my/open-emms-and-connect()
  "Reconnect to MPD and open emms playlist"
  (interactive)
  (run-hooks 'my/emms-init-hook)
  (emms-player-mpd-connect)
  (emms-smart-browse))

;; **** Sync MPD and emms
(defun my/sync-mpd-and-emms ()
  "Updates the MPD and emms database synchronously."
  (interactive)
  (call-process "mpc" nil nil nil "update")
  (emms-player-mpd-update-all-reset-cache)
  (emms-cache-set-from-mpd-all)
  (emms-player-mpd-connect)
  (message "MPD database and emms updated!"))

;; **** Keys
(require 'emms-browser)
(define-key my/music-map (kbd "u") 'my/sync-mpd-and-emms)

(define-key my/music-map (kbd "o") 'my/open-emms-and-connect)
(define-key my/music-map (kbd "g") 'emms-seek-to)
(define-key my/music-map (kbd "s") 'emms-pause)

(define-key emms-browser-mode-map (kbd "s") 'emms-pause)

(evil-define-key 'normal emms-browser-mode-map (kbd "RET") 'emms-browser-add-tracks)

(evil-define-key 'normal emms-playlist-mode-map (kbd "RET") 'emms-playlist-mode-play-smart)

(global-set-key (kbd "<XF86AudioPlay>") 'emms-pause)
(global-set-key (kbd "<XF86AudioStop>") 'emms-stop)

;; *** MPD
;; **** Start MPD
(defun my/start-mpd ()
  "Start MPD, connect to it and sync the metadata cache."
  (interactive)
  (start-process "mpd" nil "mpd")
  (message "MPD started and synced!"))

;; **** Kill daemon
(defun my/kill-music-daemon ()
  "Stops playback and kill the music daemon."
  (interactive)
  (emms-stop)
  (call-process "killall" nil nil nil "mpd")
  (message "MPD killed!"))

;; **** View MPD info
(defun my/mpd-info ()
  "Runs mpc, showing info in message field"
  (interactive)
  (shell-command "mpc"))

;; **** Shuffle playlist random
;; ***** Random on
(defun my/mpd-random-on ()
  "Turns on MPD random play"
  (interactive)
  (shell-command "mpc random on"))

;; ***** Random off
(defun my/mpd-random-off ()
  "Turns off MPD random play"
  (interactive)
  (shell-command "mpc random off"))

;; **** Volume control
;; ***** Raise volume
(defun my/mpd-raise-volume()
  (interactive)
  (shell-command "mpc volume +4"))

;; ***** Lower volume
(defun my/mpd-lower-volume ()
  (interactive)
  (shell-command "mpc volume -4"))

;; **** Change song
;; ***** Next song
(defun my/mpd-next-song()
  (interactive)
  (shell-command "mpc next"))

;; ***** Previous song
(defun my/mpd-previous-song()
  (interactive)
  (shell-command "mpc prev"))

;; **** Change time on track
;; ***** Forward
(defun my/mpd-wind-forward()
  (interactive)
  (shell-command "mpc seek +10"))

;; ***** Forward far
(defun my/mpd-wind-far-forward()
  (interactive)
  (shell-command "mpc seek +60"))

;; ***** Backwards
(defun my/mpd-wind-backward()
  (interactive)
  (shell-command "mpc seek -10"))

;; ***** Backwards far
(defun my/mpd-wind-far-backward()
  (interactive)
  (shell-command "mpc seek -60"))

;; **** Keys
(define-key my/music-map (kbd "C-s") 'my/start-mpd)
(define-key my/music-map (kbd "C-k") 'my/kill-music-daemon)
(define-key my/music-map (kbd "i") 'my/mpd-info)

(define-key my/music-map (kbd "r") 'my/mpd-random-on)
(define-key my/music-map (kbd "C-r") 'my/mpd-random-off)

(define-key my/music-map (kbd "=") 'my/mpd-raise-volume)
(define-key my/music-map (kbd "-") 'my/mpd-lower-volume)

(define-key my/music-map (kbd "n") 'my/mpd-next-song)
(define-key my/music-map (kbd "p") 'my/mpd-previous-song)

(define-key my/music-map (kbd "l") 'my/mpd-wind-forward)
(define-key my/music-map (kbd "h") 'my/mpd-wind-backward)
(define-key my/music-map (kbd "L") 'my/mpd-wind-far-forward)
(define-key my/music-map (kbd "H") 'my/mpd-wind-far-backward)

(global-set-key (kbd "<XF86AudioNext>") 'my/mpd-next-song)
(global-set-key (kbd "<XF86AudioPrev>") 'my/mpd-previous-song)

;; * Screenshots
;; ** Functions
;; *** Entire screen
(defun my/take-screenshot ()
  "Takes a fullscreen screenshot of the current workspace"
  (interactive)
  (when window-system
    (sit-for 1)
    (start-process "screenshot" nil "import" "-window" "root"
		   (concat (getenv "HOME") "/Pictures/Screenshots/" (subseq (number-to-string (float-time)) 0 10) ".png"))))

;; *** Region
(defun my/take-screenshot-region ()
  "Takes a screenshot of a region selected by the user."
  (interactive)
  (when window-system
    (call-process "import" nil nil nil ".newScreen.png")
    (call-process "convert" nil nil nil ".newScreen.png" "-shave" "1x1"
		  (concat (getenv "HOME") "/Pictures/Screenshots/" (subseq (number-to-string (float-time)) 0 10) ".png"))
    (call-process "rm" nil nil nil ".newScreen.png")))

;; *** Region ask for name
(defun my/take-screenshot-region-and-ask-for-name ()
  "Takes a screenshot of a region selected by the user and asks for file path"
  (interactive)
  (when window-system
    
    ;; Check if there is a directory called "images" in current dir, if so start read-file-name inside that directory
    (if(file-exists-p (concat default-directory "images/"))
	(setq screenshot-base-path (concat default-directory "images/"))
      (setq screenshot-base-path default-directory))
    
    ;; If screenshot path is not empty
    (call-process "import" nil nil nil ".newScreen.png")
    
    ;; Ask for path
    (setq screenshot-path (read-file-name "Screenshot file (.png already added) " screenshot-base-path))
    
    (call-process "convert" nil nil nil ".newScreen.png" "-shave" "1x1" (concat screenshot-path ".png"))
    (call-process "rm" nil nil nil ".newScreen.png")))

;; ** Keys
(global-set-key (kbd "<print>") 'my/take-screenshot-region-and-ask-for-name)

;;  (define-key my/leader-map (kbd "p r") 'my/take-screenshot-region)
;;  (define-key my/leader-map (kbd "p w") 'my/take-screenshot)

;; * Mail
;; ** Gnus
;; .gnus.el is written in =write config map=
;; https://github.com/gongzhitaao/GnusSolution
;; https://www.gnu.org/software/emacs/manual/html_node/gnus/Comparing-Mail-Back-Ends.html
(require 'gnus)
(define-key my/open-map (kbd "g") 'gnus)

;; Encrypt passwords
(setq netrc-file "~/.authinfo.gpg")

(setq gnus-use-full-window nil)

;; *** Sources
;; If dovecot server is setup
(if (my/is-system-package-installed 'dovecot)
    (setq gnus-select-method '(nnimap "Dovecot"
				      (nnimap-stream network)
				      (nnimap-address "localhost")
				      (nnimap-authenticator login)
				      (nnimap-user "admin"))))

(setq gnus-secondary-select-methods
      '((nntp "news.gmane.org")))

(setq mail-user-agent 'gnus-user-agent)
(setq read-mail-command 'gnus)
(setq send-mail-function 'message-send-mail-with-sendmail)
(setq sendmail-program "msmtp")

;; (setq smtpmail-smtp-server "smtp.gmail.com"
;; smtpmail-smtp-service 587
;; ;; Make Gnus NOT ignore [Gmail] mailboxes
;; gnus-ignored-newsgroups "^to\\.\\|^[0-9. ]+\\( \\|$\\)\\|^[\"]\"[#'()]")

;; *** Minor settings
;; Fix bug in gnus, Replace [ and ] with _ in ADAPT file names
;; (setq nnheader-file-name-translation-alist '((?[ . ?_) (?] . ?_)) )

;; Maybe disable later
;; (setq gnus-save-killed-list nil)

(setq gnus-user-date-format-alist '((t . "%Y-%m-%d %H:%M")))

;; '(gnus-always-force-window-configuration t)

;; Disable signatures
(setq message-signature nil)

;; never split messages
(setq message-send-mail-partially-limit nil)

;; Disable gnus expiration
(setq gnus-agent-enable-expiration 'DISABLE)

;; Create two connections to the server for faster fetching
(setq gnus-asynchronous t)

;; Disable .newsrc file (file can be read by other newsreaders)
(setq gnus-read-newsrc-file nil)
(setq gnus-save-newsrc-file nil)

(setq gnus-completing-read-function 'gnus-emacs-completing-read)

;; *** Group mode
;; Mode for choosing server
(defun my/gnus-group-mode ()
  ;; Tree view for groups.
  (gnus-topic-mode)
  ;; List all groups over level 5
  (gnus-group-list-all-groups 5))

(add-hook 'gnus-group-mode-hook 'my/gnus-group-mode)

;; Always show inbox
;; (setq gnus-permanently-visible-groups "INBOX")

;; Apparently only some servers support using 'some
;; (setq gnus-read-active-file 't)
;; (setq gnus-read-active-file 'some)
;; (setq gnus-check-new-newsgroups 'ask-server)

;; **** Keys
(evil-define-key 'normal gnus-group-mode-map (kbd "i") 'nil)
(evil-define-key 'normal gnus-group-mode-map (kbd "RET") (lambda () (interactive) (gnus-topic-select-group t)))
(evil-define-key '(normal insert) gnus-group-mode-map (kbd "TAB") 'gnus-topic-select-group)

(define-prefix-command 'my/gnus-group-map)
(evil-define-key 'normal gnus-group-mode-map (kbd (concat my/leader-map-key " a")) 'my/gnus-group-map)

(defun my/gnus-group-list-all-subscribed-groups ()
  "List all subscribed groups with or without un-read messages"
  (interactive)
  (gnus-group-list-all-groups 5))

(define-key 'my/gnus-group-map (kbd "s") 'my/gnus-group-list-all-subscribed-groups)

;; *** Topic mode
;; Adds headers to each server, tree view
(defun my/gnus-topic-mode ()
  (my/gnus-topic-setup)
  (my/gnus-topic-add-gmane-groups))

(add-hook 'gnus-topic-mode-hook 'my/gnus-topic-mode)

;; **** Subscribe to gmane groups
(defvar my/gnus-topic-gmane-prefix "nntp+news.gmane.org:")

(setq my/gnus-gmane-subscribed-emacs `(
				       ;; Gnus
				       ,(concat my/gnus-topic-gmane-prefix "gmane.emacs.help") ,(concat my/gnus-topic-gmane-prefix "gmane.emacs.gnus.general") ,(concat my/gnus-topic-gmane-prefix "gmane.emacs.gnus.announce") ,(concat my/gnus-topic-gmane-prefix "gmane.emacs.gnus.user")))

(setq my/gnus-gmane-subscribed-emacs-blogs `(
					     ,(concat my/gnus-topic-gmane-prefix "gwene.com.oremacs") ,(concat my/gnus-topic-gmane-prefix "gwene.me.emacsair")))

(setq my/gnus-gmane-subscribed-fsharp `(
					;; Gnus
					,(concat my/gnus-topic-gmane-prefix "gwene.com.reddit.pay.r.fsharp")))

(setq my/gnus-gmane-subscribed-guile `(
				       ;; Gnus
				       ,(concat my/gnus-topic-gmane-prefix "gmane.lisp.guile.user")))

(defun my/gnus-gmane-subscribed-get()
  (append
   my/gnus-gmane-subscribed-guile
   my/gnus-gmane-subscribed-fsharp
   my/gnus-gmane-subscribed-emacs-blogs
   my/gnus-gmane-subscribed-emacs))

(defun my/gnus-topic-add-gmane-groups ()
  (let ((list (my/gnus-gmane-subscribed-get)))
    (dotimes (i (+ 1 (length list)))
      (add-to-list 'gnus-newsrc-alist `(,(nth i list) 3 nil nil "nntp:news.gmane.org"))))
  
  ;; Move the dummy entry to the top
  (setq gnus-newsrc-alist (delete '("dummy.group" 0 nil) gnus-newsrc-alist))
  ;; We don't need the dummy group?
  ;;(add-to-list 'gnus-newsrc-alist '("dummy.group" 0 nil))
  )

;; **** Topic setup
(defun my/gnus-topic-setup ()
  "Hides non-relevant servers and puts them into categories. To show all servers, disable my/gnus-topic-mode"
  
  ;; "Gnus" is the root folder, and there are three mail accounts, "misc", "hotmail", "gmail"
  (setq gnus-topic-topology '
	(("Gnus" visible)
	 
	 ;; Mail
	 (("Mail" visible)
	  (("gmail" visible))
	  (("gmail-main" visible)))
	 
	 ;; News
	 (("News" visible)
	  (("Emacs" visible)
	   (("Emacs blogs" visible)))
	  (("Fsharp" visible))
	  (("Guile" visible))
	  )))
  
  (setq gnus-topic-alist `((("Gnus"))
			   ;; Mail
			   ("gmail-main"
			    "main-gmail/All"
			    "main-gmail/Sent"
			    "main-gmail/Starred"
			    "main-gmail/Trash")
			   
			   ;; News
			   ,(append '("Emacs") my/gnus-gmane-subscribed-emacs)
			   ,(append '("Emacs blogs") my/gnus-gmane-subscribed-emacs-blogs)
			   ,(append '("Fsharp") my/gnus-gmane-subscribed-fsharp)
			   ,(append '("Guile") my/gnus-gmane-subscribed-guile)
			   )))

;; **** Keys
(define-prefix-command 'my/gnus-topic-map)
(evil-define-key 'normal gnus-topic-mode-map (kbd (concat my/leader-map-key " a")) 'my/gnus-topic-map)

;; *** Summary mode
;; Mode for choosing which mail to open
(defun my/gnus-summary-mode ()
  (setq truncate-lines t)
  (my/toggle-local-mono-font t))

(add-hook 'gnus-summary-mode-hook 'my/gnus-summary-mode)

;; '(gnus-summary-mode-line-format "U%U %S" )
;; https://www.gnu.org/software/emacs/manual/html_node/gnus/Summary-Buffer-Lines.html
(setq-default gnus-summary-line-format
	      (concat
	       ;; Is unread
	       "%U"
	       ">"
	       ;; Total thread score
	       "%V"
	       ;; Has been replied to/cached/saved
	       "%R"
	       ;; Tab
	       "\t"
	       ;; Date as specified by `gnus-user-date-format-alist`
	       "%&user-date; \t"
	       ;; Linecount, leave -5,5 spacing
	       "%-5,5L"
	       ;; Sender taken from header, leave -20,20 spacing
	       "%-20,20n"
	       
	       "\t"
	       ;; Reply tree
	       "%B"
	       ;; Article subject string
	       "%-80,80S"
	       ;; End
	       "\n"))

(setq gnus-user-date-format-alist '((t . "%Y-%m-%d %H:%M")))
(setq gnus-thread-sort-functions '(gnus-thread-sort-by-date))

;; Supposed to be better
(setq-default gnus-summary-thread-gathering-function 'gnus-gather-threads-by-references)

(setq gnus-sum-thread-tree-false-root ""
      gnus-sum-thread-tree-single-indent ""
      gnus-sum-thread-tree-root ""
      gnus-sum-thread-tree-vertical"|"
      gnus-sum-thread-tree-leaf-with-other "+-> "
      gnus-sum-thread-tree-single-leaf"\\-> "
      gnus-sum-thread-tree-indent " ")

;; '(gnus-thread-hide-subtree t)
;; '(gnus-thread-sort-functions (quote gnus-thread-sort-by-most-recent-date))
;; '(gnus-treat-hide-citation t)
;; '(gnus-unread-mark 42)
;; '(gnus-ancient-mark 32)

;; **** Visuals
;; '(gnus-summary-high-unread ((t (:foreground "green"))))
;; '(gnus-summary-low-read ((t (:foreground "magenta"))))
;; '(gnus-summary-normal-read ((t (:foreground "red"))))
;; '(gnus-summary-selected ((t (:background "yellow"))))
;; '(gnus-summary-normal-unread ((t (:foreground "white"))))

;; **** Scoring
(setq gnus-parameters
      '(("nnimap.*"
	 (gnus-use-scoring nil)) ;; Enable later
	))

;; **** Keys
(define-prefix-command 'my/gnus-summary-map)
(evil-define-key 'normal gnus-summary-mode-map (kbd (concat my/leader-map-key "a")) 'my/gnus-summary-map)

(evil-define-key 'normal gnus-summary-mode-map (kbd "i") 'nil)
(evil-define-key 'normal gnus-summary-mode-map (kbd "RET") 'gnus-summary-scroll-up)

(defun my/gnus-summary-show-all-mail ()
  "Show all mail"
  (interactive)
  (gnus-summary-rescan-group 1))

(define-key 'my/gnus-summary-map (kbd "s") 'my/gnus-summary-show-all-mail)

;; *** Article mode
;; Mode for reading contents of mail
(defun my/gnus-article-mode ()
  ;; Font lock mode disables colors in html mail for whatever reason
  (font-lock-mode -1))

(add-hook 'gnus-article-mode-hook 'my/gnus-article-mode)

;; (defun my/gnus-article-display-mode ()
;;  (gnus-article-de-quoted-unreadable)
;;  (gnus-article-emphasize)
;;  (gnus-article-hide-boring-headers)
;;  (gnus-article-hide-headers-if-wanted)
;;  (gnus-article-hide-pgp)
;;  (gnus-article-highlight)
;;  (gnus-article-highlight-citation)
;;  (gnus-article-date-local)
;; )

(add-hook 'gnus-article-display-hook 'my/gnus-article-display-mode)

;; '(gnus-article-mode-line-format "U%U %S" )

;; **** Date headers
;; Make date headers better with timezone calculation and time passed

(setq gnus-article-date-headers '(user-defined)
      gnus-article-time-format
      (lambda (time)
	(let* ((date (format-time-string "%a, %d %b %Y %T %z" time))
	       (local (article-make-date-line date 'local))
	       (combined-lapsed (article-make-date-line date
							'combined-lapsed))
	       (lapsed (progn
			 (string-match " (.+" combined-lapsed)
			 (match-string 0 combined-lapsed))))
	  (concat local lapsed))))

;; **** Mail renderers, etc
;; html renderer
(setq mm-text-html-renderer 'shr)
;; Inline images?
(setq mm-attachment-override-types '("image/.*"))
;; No HTML mail
(setq mm-discouraged-alternatives '("text/html" "text/richtext"))

;; **** Keys
(define-prefix-command 'my/gnus-article-map)
(evil-define-key 'normal gnus-article-mode-map (kbd (concat my/leader-map-key " a")) 'my/gnus-article-map)

;; *** Browse server mode
;; **** Keys
(define-prefix-command 'my/gnus-browse-mode-map)
(evil-define-key 'normal gnus-browse-mode-map (kbd (concat my/leader-map-key " a")) 'my/gnus-browse-mode-map)

(evil-define-key 'normal gnus-browse-mode-map (kbd "RET") 'gnus-browse-select-group)

;; *** Server mode
;; **** Keys
(define-prefix-command 'my/gnus-server-mode-map)
(evil-define-key 'normal gnus-server-mode-map (kbd (concat my/leader-map-key " a")) 'my/gnus-server-mode-map)

(evil-define-key 'normal gnus-server-mode-map (kbd "RET") 'gnus-server-read-server)

;; *** Message mode
;; Mode for writing mail

;; **** Keys
(define-prefix-command 'my/gnus-message-map)
(evil-define-key 'normal gnus-group-mode-map (kbd (concat my/leader-map-key "a")) 'my/gnus-message-map)

;; *** Misc
;; **** Random color gnus logo
(random t) ; Randomize sequence of random numbers
(defun my/random-hex (&optional num)
  (interactive "P")
  (let (($n (if (numberp num) (abs num) 6 )))
    (format  (concat "%0" (number-to-string $n) "x" ) (random (1- (expt 16 $n))))))

(setq gnus-logo-colors (list (concat "#" (my/random-hex 6)) (concat "#" (my/random-hex 6))))

;; ** mbsync
(defvar my/sync-mail-hook nil)
(defvar my/sync-mail-has-begun nil)

(defun my/sync-mail ()
  (interactive)
  (async-shell-command "mbsync -a")
  (run-with-timer 0 nil (lambda () (interactive) (run-hooks 'my/sync-mail-hook))))


(add-hook 'gnus-topic-mode-hook 'my/sync-mail-begin)

(defun my/sync-mail-begin ()
  (if (my/is-system-package-installed 'mbsync)
      (run-with-timer 10 300 'my/sync-mail)))

;; ** Display unread mail count
(defun my/gnus-scan-unread ()
  (if (get-buffer "*Group*")
      (gnus-group-get-new-news)
    (gnus)))

(defun my/gnus-get-unread-mail-count ()
  (my/gnus-get-unread "Mail"))

(defun my/gnus-get-unread-news-count ()
  (my/gnus-get-unread "News"))

(defun my/gnus-get-unread (inbox)
  (let ((result ""))
    (dotimes (i (length gnus-topic-unreads))
      (if (string= inbox (car (nth i gnus-topic-unreads)))
	  (progn
	    (setq result (number-to-string (cdr (nth i gnus-topic-unreads))))
	    (setq i (length gnus-topic-unreads)))))
    result))

;; * System
(define-prefix-command 'my/system-commands-map)
(define-key my/leader-map (kbd "~") 'my/system-commands-map)

;; ** Suspend
;;   #+begin_src emacs-lisp
;; (define-prefix-command 'my/system-suspend-map)
;; (define-key my/system-commands-map (kbd "s") 'my/system-suspend-map)

;; (defun my/systemd-suspend-PC()
;;   (interactive)
;;   (shell-command "systemctl suspend"))
;; (define-key my/system-suspend-map (kbd "C-s") 'my/systemd-suspend-PC)

;; (defun my/systemd-hibernate-PC()
;;   (interactive)
;;   (shell-command "systemctl hibernate"))
;;   ;; Never used
;; ;;(define-key my/system-suspend-map (kbd "C-h") 'my/systemd-hibernate-PC)
;; #+end_src

;; ** Multi-monitor
(define-prefix-command 'my/system-monitor-map)
(define-key my/system-commands-map (kbd "m") 'my/system-monitor-map)

(defun my/x-suspend-monitor()
  (interactive)
  (shell-command "xset dpms force suspend"))
(define-key my/system-monitor-map (kbd "s") 'my/x-suspend-monitor)

(defun my/print-monitors ()
  (interactive)
  (shell-command "xrandr"))
(define-key my/system-monitor-map (kbd "p") 'my/print-monitors)

(defun my/monitor-home-setup ()
  (interactive)
  (shell-command "xrandr --output DP-1 --mode 2560x1440 --rate 60 --left-of DVI-D-1 --output DVI-D-1 --mode 1280x800 --rate 59.81"))
(define-key my/system-monitor-map (kbd "h") 'my/monitor-home-setup)

(defun my/auto-connect-screen ()
  (interactive)
  (with-temp-buffer
    (call-process "xrandr" nil t nil)
    (beginning-of-buffer)
    (if (search-forward "VGA1 connected" nil 'noerror)
	(start-process-shell-command
	 "xrandr" nil "xrandr --output VGA1 --primary --auto --output LVDS1 --off")
      (start-process-shell-command
       "xrandr" nil "xrandr --output LVDS1 --auto"))))

(define-key my/system-monitor-map (kbd "a") 'my/auto-connect-screen)

(if (window-system)
    (async-shell-command my/device/monitor-setup-command "xrandr setup buffer"))

;; ** Process monitors
(define-prefix-command 'my/processes-map)
(define-key my/system-commands-map (kbd "p") 'my/processes-map)

;; *** Top - proced
(define-key my/processes-map (kbd "t") 'proced)

;; **** Disable line wrapping
(defun my/proced-mode ()
  (interactive)
  (toggle-truncate-lines 1))

;; (add-hook 'proced-post-display-hook 'my/proced-mode)
(add-hook 'proced-mode-hook 'my/proced-mode)

;; *** Profiler
(define-prefix-command 'my/profiler-map)
(define-key my/processes-map (kbd "p") 'my/profiler-map)

(define-key my/profiler-map (kbd "s") 'profiler-start)
(define-key my/profiler-map (kbd "e") 'profiler-stop)
(define-key my/profiler-map (kbd "r") 'profiler-report)
(define-key my/profiler-map (kbd "R") 'profiler-reset)

;; ** Install software
(define-prefix-command 'my/software-install-map)
(define-key my/system-commands-map (kbd "i") 'my/software-install-map)

;; *** Install eclipse java language server
;; For use with lsp-java
(defun my/install-eclipse-java-language-server()
  (interactive)
  (shell-command "
   cd ~
   rm -rf ~/.emacs.d/eclipse.jdt.ls/server/
   mkdir -p ~/.emacs.d/eclipse.jdt.ls/server/
   wget http://download.eclipse.org/jdtls/snapshots/jdt-language-server-latest.tar.gz -O /tmp/jdt-latest.tar
   tar xf /tmp/jdt-latest.tar -C ~/.emacs.d/eclipse.jdt.ls/server/
   "))

(define-key my/software-install-map (kbd "j") 'my/install-eclipse-java-language-server)

;; *** Install pdf tools
(define-key my/software-install-map (kbd "p") 'pdf-tools-install)

;; *** Install omnisharp
(define-key my/software-install-map (kbd "o") 'omnisharp-install-server)

;; *** Install rtags
;; You need llvm
;; (defun my/install-rtags ()
;; (interactive)
;; (async-shell-command " cd ~
;; git clone --recursive https://github.com/Andersbakken/rtags.git
;; cd rtags
;; cmake -DCMAKE_EXPORT_COMPILE_COMMANDS=1 .
;; make"))

;; (define-key my/software-install-map (kbd "r") 'my/install-rtags)

;; *** Compile config
(defun my/compile-config ()
  (interactive)
  (byte-compile-file my/config-exported-location nil))

(define-key my/software-install-map (kbd "C-c") 'my/compile-config)

;; * Networking
(define-prefix-command 'my/net-utils-map)
(define-key my/system-commands-map (kbd "n") 'my/net-utils-map)

;; ** Tramp
;; (setq tramp-default-method "scpx")

;; ** Netstat
(defun my/net-utils-mode ()
  (interactive)
  (toggle-truncate-lines 1))

(add-hook 'net-utils-mode-hook 'my/net-utils-mode)

;; ** Keys
(define-key my/net-utils-map (kbd "s") 'netstat)
(define-key my/net-utils-map (kbd "p") 'ping)
(define-key my/net-utils-map (kbd "i") 'ifconfig)

;; * Hardware
(define-prefix-command 'my/hardware-info-map)
(define-key my/system-commands-map (kbd "h") 'my/hardware-info-map)

;; Memory
(defun my/unix-get-memory-available()
  (interactive)
  (shell-command "grep \"MemAvailable\" /proc/meminfo"))
(define-key my/hardware-info-map (kbd "m") 'my/unix-get-memory-available)

;; GPU
(defun my/unix-get-gpu()
  (interactive)
  (shell-command "lspci | grep ' VGA ' | cut -d\" \" -f 1 | xargs -i lspci -v -s {}"))
(define-key my/hardware-info-map (kbd "g") 'my/unix-get-gpu)

;; Blocked devices
(defun my/rfkill-get-blocked-devices()
  (interactive)
  (shell-command "rfkill list"))
(define-key my/hardware-info-map (kbd "b") 'my/rfkill-get-blocked-devices)

;; Get devices
(defun my/rfkill-get-devices()
  (interactive)
  (shell-command "cat /proc/devices"))
(define-key my/hardware-info-map (kbd "d") 'my/rfkill-get-devices)

;; ** CPU
;; Linux temps
(if (file-exists-p "/proc/cpuinfo") (progn
				      (define-prefix-command 'my/cpu-info-map)
				      (define-key my/hardware-info-map (kbd "c") 'my/cpu-info-map)
				      
				      (defun my/unix-cpu-get-clock()
					(interactive)
					(shell-command "grep \"cpu MHz\" /proc/cpuinfo"))
				      ;; Clock speed
				      (define-key my/cpu-info-map (kbd "f") 'my/unix-cpu-get-clock)
				      ;; Model name
				      (defun my/unix-cpu-get-name()
					(interactive)
					(shell-command "grep \"model name\" /proc/cpuinfo"))
				      (define-key my/cpu-info-map (kbd "n") 'my/unix-cpu-get-name)
				      ;; Core count
				      (defun my/unix-cpu-get-core-count()
					(interactive)
					;; Linux
					(shell-command "grep \"cores\" /proc/cpuinfo"))
				      
				      (define-key my/cpu-info-map (kbd "c") 'my/unix-cpu-get-core-count)
				      ;; Flags
				      (defun my/unix-cpu-get-flags()
					(interactive)
					(shell-command "grep \"flags\" /proc/cpuinfo"))
				      (define-key my/cpu-info-map (kbd "F") 'my/unix-cpu-get-flags)
				      ;; Vendor
				      (defun my/unix-cpu-get-vendor-id()
					(interactive)
					(shell-command "grep \"vendor_id\" /proc/cpuinfo"))
				      (define-key my/cpu-info-map (kbd "v") 'my/unix-cpu-get-vendor-id)
				      ;; Bugs (Bugs that has affected CPU model)
				      (defun my/unix-cpu-get-bugs()
					(interactive)
					(shell-command "grep \"bugs\" /proc/cpuinfo"))
				      (define-key my/cpu-info-map (kbd "b") 'my/unix-cpu-get-bugs)
				      ;; Cache size
				      (defun my/unix-cpu-get-cache-size()
					(interactive)
					(shell-command "grep \"cache size\" /proc/cpuinfo"))
				      (define-key my/cpu-info-map (kbd "C") 'my/unix-cpu-get-cache-size)))

;; Windows cpu core count
;; (if (or (eq system-type 'windows-nt) (eq system-type 'cygwin))
;; (let ((number-of-processors (getenv "NUMBER_OF_PROCESSORS")))
;; (if number-of-processors
;; (string-to-number number-of-processors))))

;; * Find
;; ** Custom locate
;; Locate only under current dir
(defvar my/locate-cache nil)
(defconst my/locate-database-dir "/home/admin/locate.db")

(defun my/locate-updatedb ()
  (interactive)
  (if my/using-gnu-mlocate
      (shell-command (concat "updatedb --localpaths=/home/admin --output=" my/locate-database-dir))
    (shell-command (concat "updatedb --database-root=/home/admin --output=" my/locate-database-dir)))
  
  (my/locate-update-cache))

(defun my/locate-update-cache ()
  (setq my/locate-cache (split-string
			 (shell-command-to-string
			  (concat "locate --database=" my/locate-database-dir " -r ."))
			 "\n")))

;; Would be nice if we could remove the initial file path, but it's too slow with pure map
;;  (map 'list (lambda (string) (substring string (length dir))))
;; And using mapc doesn't work because substring is a pure function
;;  (mapc (lambda (string) (substring string (length dir))))
(require 's)
(defun my/locate-list-remove-outside-curr-dir ()
  "Return locate cache with irrelevant entries removed."
  (let ((dir (expand-file-name default-directory)))
    (seq-filter
     (lambda (string) (s-starts-with-p dir string)) my/locate-cache)))

(defun my/locate ()
  (interactive)
  (let ((original-gc gc-cons-threshold))
    (setq gc-cons-threshold 80000000)
    ;; Check if cache is empty
    (when (not my/locate-cache)
      (my/locate-update-cache)
      ;; If still nothing in cache
      (when (not my/locate-cache)
	(message "Locate command returned nothing")))
    
    (ivy-read "Locate: " (my/locate-list-remove-outside-curr-dir)
	      :initial-input (concat "^" (expand-file-name default-directory))
	      :action (lambda (file)
			(when file
			  (with-ivy-window
			    (find-file
			     (concat (file-remote-p default-directory) file)))))
	      :unwind '(lambda () (setq gc-cons-threshold original-gc)))))

;; * Spelling
(define-prefix-command 'my/spell-map)
(define-key my/leader-map (kbd "S") 'my/spell-map)

(define-key my/spell-map (kbd "d") 'ispell-change-dictionary)
(define-key my/spell-map (kbd "s") 'flyspell-mode)

;; ** Company
(defun my/toggle-company-ispell ()
  (interactive)
  (cond
   ((memq 'company-ispell company-backends)
    (setq company-backends (delete 'company-ispell company-backends))
    (message "company-ispell disabled"))
   (t
    (add-to-list 'company-backends 'company-ispell)
    (message "company-ispell enabled!"))))

(define-key my/spell-map (kbd "c") 'my/toggle-company-ispell)

;; * Calc
;; (define-key my/leader-map (kbd "c") 'calc)

(defun my/calc-kill-current-line ()
  (interactive)
  (calc-kill-region (line-beginning-position) (line-end-position)))

(evil-define-key 'normal calc-mode-map (kbd "d d") 'my/calc-kill-current-line)
(evil-define-key 'visual calc-mode-map (kbd "d") 'calc-kill-region)

;; * Artist mode
;; ** Completing read
;; https://www.emacswiki.org/emacs/ArtistMode
(defun my/artist-select-operation (type)
  "Use ido to select a drawing operation in artist-mode"
  (interactive (list (completing-read "Drawing operation: "
				      (list "Pen" "Pen Line" "line" "straight line" "rectangle"
					    "square" "poly-line" "straight poly-line" "ellipse"
					    "circle" "text see-thru" "text-overwrite" "spray-can"
					    "erase char" "erase rectangle" "vaporize line" "vaporize lines"
					    "cut rectangle" "cut square" "copy rectangle" "copy square"
					    "paste" "flood-fill"))))
  (artist-select-operation type))

(defun my/artist-select-settings (type)
  "Use ido to select a setting to change in artist-mode"
  (interactive (list (completing-read "Setting: "
				      (list "Set Fill" "Set Line" "Set Erase" "Spray-size" "Spray-chars"
					    "Rubber-banding" "Trimming" "Borders"))))
  (if (equal type "Spray-size")
      (artist-select-operation "spray set size")
    (call-interactively (artist-fc-get-fn-from-symbol
			 (cdr (assoc type '(("Set Fill" . set-fill)
					    ("Set Line" . set-line)
					    ("Set Erase" . set-erase)
					    ("Rubber-banding" . rubber-band)
					    ("Trimming" . trimming)
					    ("Borders" . borders)
					    ("Spray-chars" . spray-chars))))))))

;; ** Picture mode keys
(setq picture-mode-map (make-sparse-keymap))
(setq-default picture-mode-map (make-sparse-keymap))

;; ** Keys
(define-key my/leader-map (kbd "k") 'artist-mode)

(define-prefix-command 'my/artist-mode-map)
(evil-define-key 'normal artist-mode-map (kbd (concat my/leader-map-key " a")) 'my/artist-mode-map)

(define-key my/artist-mode-map (kbd "o") 'my/artist-select-operation)
(define-key my/artist-mode-map (kbd "s") 'my/artist-select-settings)


;; (evil-define-key 'insert artist-mode-map (kbd "SPC") '(lambda () (interactive) (insert " ")))
;; (evil-define-key 'insert artist-mode-map (kbd "SPC") 'self-insert-command)

(setq artist-mode-map (make-sparse-keymap))
(setq-default artist-mode-map (make-sparse-keymap))
;; (evil-define-key 'insert artist-mode-map (kbd "DEL") 'picture-backward-clear-column)

;; (evil-define-key 'insert artist-mode-map (kbd "RET") 'newline)

(evil-define-key 'normal artist-mode-map (kbd "p") 'artist-previous-line)
(evil-define-key 'normal artist-mode-map (kbd "n") 'artist-next-line)


(evil-define-key 'normal artist-mode-map (kbd "n") 'artist-next-line)

(evil-define-key 'emacs artist-mode-map [down-mouse-1] 'artist-down-mouse-1)
(evil-define-key 'emacs artist-mode-map [S-down-mouse-1] 'artist-down-mouse-1)
(evil-define-key 'emacs artist-mode-map [down-mouse-2] 'artist-mouse-choose-operation)
(evil-define-key 'emacs artist-mode-map [S-down-mouse-2] 'artist-mouse-choose-operation)
(evil-define-key 'emacs artist-mode-map [down-mouse-3] 'artist-down-mouse-3)
(evil-define-key 'emacs artist-mode-map [S-down-mouse-3] 'artist-down-mouse-3)
(evil-define-key 'emacs artist-mode-map [C-mouse-4] 'artist-select-prev-op-in-list)
(evil-define-key 'emacs artist-mode-map [C-mouse-5] 'artist-select-next-op-in-list)

;; * Hydra
(straight-use-package 'hydra)

(setq hydra-hint-display-type 'message)

;; ** Window and buffer management
(defhydra my/window-hydra (:hint nil
				 :color red)
  ;; :pre (setq exwm-input-line-mode-passthrough t)
  ;; :post (setq exwm-input-line-mode-passthrough nil))
  "movement"
  
  ;; Move focus
  ("p" evil-window-up nil)
  ("n" evil-window-down nil)
  ("l" evil-window-right nil)
  ("h" evil-window-left nil)
  
  ;; Move focus to edge window
  ;; Frame border window
  ("|" evil-window-mru nil)
  
  ;; Move window
  ;; Move up
  ("P" evil-move-very-top nil)
  ;; Move down
  ("N" evil-move-very-bottom nil)
  ;; Move right
  ("L" evil-move-far-right nil)
  ;; Move left
  ("H" evil-move-far-left nil)
  
  ;; Switch monitor right
  ("M-l" my/switch-monitor-right nil)
  ;; Switch monitor left
  ("M-h" my/switch-monitor-left nil)
  
  ;; Resize window
  ;; Resize up
  ("C-p" (evil-window-increase-height 10) nil)
  ;; Resize down
  ("C-n" (evil-window-decrease-height 10) nil)
  ;; Resize right
  ("C-l" (evil-window-decrease-width 10) nil)
  ;; Resize left
  ("C-h" (evil-window-increase-width 10) nil)

  ;; Resize up
  ("C-S-p" (evil-window-increase-height 40) nil)
  ;; Resize down
  ("C-S-n" (evil-window-decrease-height 40) nil)
  ;; Resize right
  ("C-S-l" (evil-window-decrease-width 40) nil)
  ;; Resize left
  ("C-S-h" (evil-window-increase-width 40) nil)
  
  
  ;; Split
  ("o" split-window-right nil)
  ("v" split-window-below nil)
  
  ("i" my/clone-indirect-buffer-name nil)
  ("I" my/clone-indirect-buffer-name-other-window nil)
  
  ;; Search
  ("C-s" swiper-all nil)
  
  ;; Close window
  ("s" delete-window nil)
  ;; Focus on window
  ("d" my/delete-other-windows nil)
  
  ;; minimize window
  ("S" (lambda () (interactive) (evil-window-increase-height 1000) (evil-window-increase-width 1000)) nil)
  ;; maximize window
  ("D" (lambda () (interactive) (evil-window-decrease-height 1000) (evil-window-decrease-width 1000)) nil)
  
  ;; Buffer management
  ;; Find file
  ("e" my/find-file nil)
  ("E" my/dired-curr-dir nil)
  ("M-e" my/change-default-directory nil)
  
  ;; Find
  ("f" my/locate nil)
  ("F" my/counsel-ag nil)
  
  ;; Switch buffer
  ("a" ivy-switch-buffer nil)
  
  ("A" my/switch-to-last-buffer nil)
  
  ;; Kill buffer
  ("k" kill-current-buffer nil)
  
  ;; Move around in buffer
  ("C-u" evil-scroll-up nil)
  ("C-w" evil-scroll-down nil)
  
  ("y" counsel-linux-app nil)
  
  ;; Switch window configuration
  ("t" my/load-window-config nil)
  ("T" my/add-window-config nil)
  ("C-t" my/delete-window-config nil)
  
  ("b" counsel-bookmark nil)
  ("B" my/add-bookmark nil)
  ("C-b" my/delete-bookmark nil)
  
  ("u" winner-undo nil)
  ("C-r" winner-redo nil)
  
  ("R" rename-buffer nil))

;;  ("SPC" my/leader-map nil)

;; Add this to not auto exit insert mode after closing the hydra
;; ("<escape>" nil))

;; ** Evil-lispy
(defhydra my/lispy-hydra (:hint nil
				:color red)
  "lisp"
  
  ("H" (call-interactively #'lispy-backward nil))
  ("L" (call-interactively #'lispy-flow nil))
  
  ("C-l" (call-interactively #'lispy-knight-down) nil)
  ("C-h" (call-interactively #'lispy-knight-up) nil)
  
  ("l" (call-interactively #'lispy-right) nil)
  ("h" (call-interactively #'lispy-left) nil)
  
  ;;   ("l" (call-interactively #'down-list) nil)
  ;;   ("h" (call-interactively #'up-list) nil)
  
  ("N" (call-interactively #'lispy-raise) nil)
  ("P" (call-interactively #'lispy-convolute) nil)
  
  ("n" (call-interactively #'lispy-down) nil)
  ("p" (call-interactively #'lispy-up nil))
  
  ("u" (call-interactively #'undo nil))
  ;;("u" (call-interactively #'lispy-back nil))
  
  ("e" (call-interactively #'my/auto-eval nil))
  
  ("o" (call-interactively #'lispy-different nil))
  ("d" (call-interactively #'lispy-kill nil))
  
  ;;("y" (call-interactively #'lispy-occur nil))
  
  
  (">" (call-interactively #'lispy-slurp nil))
  ("<" (call-interactively #'lispy-barf nil))
  ("/" (call-interactively #'lispy-splice nil))
  
  ;;  ("r" (call-interactively #'lispy-raise nil))
  ;;  ("R" (call-interactively #'lispy-raise-some nil))
  
  ;;("+" (call-interactively #'lispy-join nil))
  
  ;;  ("C" (call-interactively #'lispy-splice nil))
  ;;  ("X" (call-interactively #'lispy-splice nil))
  ;;  ("w" (call-interactively #'lispy-splice nil))
  ;;  ("s" (call-interactively #'lispy-splice nil))
  ;;  ("/" (call-interactively #'lispy-splice nil))
  ;;  ("/" (call-interactively #'lispy-splice nil))
  ;;  ("/" (call-interactively #'lispy-splice nil))
  ;;  ("/" (call-interactively #'lispy-splice nil))
  ;;  ("/" (call-interactively #'lispy-splice nil))
  
  ("<escape>" nil nil))

;; ;; navigation
;; (lispy-define-key map "C" 'lispy-convolute)
;; (lispy-define-key map "X" 'lispy-convolute-left)
;; (lispy-define-key map "w" 'lispy-move-up)
;; (lispy-define-key map "s" 'lispy-move-down)
;; (lispy-define-key map "O" 'lispy-oneline)
;; (lispy-define-key map "M" 'lispy-alt-multiline)
;; (lispy-define-key map "S" 'lispy-stringify)
;; ;; marking
;; (lispy-define-key map "a" 'lispy-ace-symbol
;;   :override '(cond ((looking-at lispy-outline)
;;                     (lispy-meta-return))))
;; (lispy-define-key map "H" 'lispy-ace-symbol-replace)
;; (lispy-define-key map "m" 'lispy-mark-list)
;; ;; dialect-specific
;; (lispy-define-key map "e" 'lispy-eval)
;; (lispy-define-key map "E" 'lispy-eval-and-insert)
;; (lispy-define-key map "G" 'lispy-goto-local)
;; (lispy-define-key map "g" 'lispy-goto)
;; (lispy-define-key map "F" 'lispy-follow t)
;; (lispy-define-key map "D" 'pop-tag-mark)
;; (lispy-define-key map "A" 'lispy-beginning-of-defun)
;; (lispy-define-key map "_" 'lispy-underscore)
;; ;; miscellanea
;; (define-key map (kbd "SPC") 'lispy-space)
;; (lispy-define-key map "i" 'lispy-tab)
;; (lispy-define-key map "I" 'lispy-shifttab)
;; (lispy-define-key map "N" 'lispy-narrow)
;; (lispy-define-key map "W" 'lispy-widen)
;; (lispy-define-key map "c" 'lispy-clone)
;; (lispy-define-key map "u" 'lispy-undo)
;; (lispy-define-key map "q" 'lispy-ace-paren
;;   :override '(cond ((bound-and-true-p view-mode)
;;                     (View-quit))))
;; (lispy-define-key map "Q" 'lispy-ace-char)
;; (lispy-define-key map "v" 'lispy-view)
;; (lispy-define-key map "t" 'lispy-teleport
;;   :override '(cond ((looking-at lispy-outline)
;;                     (end-of-line))))
;; (lispy-define-key map "n" 'lispy-new-copy)
;; (lispy-define-key map "b" 'lispy-back)
;; (lispy-define-key map "B" 'lispy-ediff-regions)
;; (lispy-define-key map "x" 'lispy-x)
;; (lispy-define-key map "Z" 'lispy-edebug-stop)
;; (lispy-define-key map "V" 'lispy-visit)
;; (lispy-define-key map "-" 'lispy-ace-subword)
;; (lispy-define-key map "." 'lispy-repeat)
;; (lispy-define-key map "~" 'lispy-tilde)

;; Goto
;; ("d" lispy-goto)
;; ("l" lispy-goto-local)
;; ("r" lispy-goto-recursive)
;; ("p" lispy-goto-projectile)
;; ("f" lispy-follow)
;; ("b" pop-tag-mark)
;; ("q" lispy-quit)
;; ("j" lispy-goto-def-down)
;; ("a" lispy-goto-def-ace)
;; ("e" lispy-goto-elisp-commands)

;; Other
;; (("h" lispy-move-left)
;;  ("j" lispy-down-slurp)
;;  ("k" lispy-up-slurp)
;;  ("l" lispy-move-right)
;;  ("SPC" lispy-other-space)
;;  ("g" lispy-goto-mode)))

;; Knight
;;   ("j" lispy-knight-down)
;;   ("k" lispy-knight-up)
;;    ("z" nil))

;; *** Keys
(my/evil-universal-define-key my/mod-window-leader-key 'my/window-hydra/body)
(my/evil-universal-define-key my/window-leader-key 'my/window-hydra/body)

(my/evil-visual-define-key "z" 'my/lispy-hydra/body)
(my/evil-normal-define-key "z" 'my/lispy-hydra/body)

;; * Image modes
;; ** PDF view
(defun my/pdf-view-mode()
  (interactive)
  (display-line-numbers-mode -1))

(add-hook 'pdf-view-mode-hook 'my/pdf-view-mode t)

;; ** PDF tools
(straight-use-package 'pdf-tools)

(add-to-list 'auto-mode-alist '("\\.[pP][dD][fF]\\'" . my/init-pdf-tools))
;; (add-to-list 'auto-mode-alist '("\\.[pP][dD][fF]\\'" . pdf-view-mode))

(setq my/pdf-tools-installed nil)

(defun my/init-pdf-tools ()
  (interactive)
  (if (not my/pdf-tools-installed)
      (progn
	(require 'pdf-view)
	(pdf-tools-install)
	;; reload buffer with everything set
	(revert-buffer :ignore-auto :noconfirm)))
  (setq my/pdf-tools-installed t)
  (pdf-view-mode))

;; (add-hook 'pdf-view-mode-hook 'my/init-pdf-tools)

;; Enable pdf-links
(add-hook 'pdf-view-mode-hook 'pdf-links-minor-mode)

;; Remove default keys
(setq pdf-view-mode-map (make-sparse-keymap))

;; *** Keys
;; Disable insert mode
(define-key pdf-view-mode-map [remap evil-insert] 'evil-force-normal-state)

;; Scroll half page
(define-key pdf-view-mode-map [remap View-scroll-half-page-backward] 'pdf-view-scroll-down-or-previous-page)
(define-key pdf-view-mode-map [remap View-scroll-half-page-forward] 'pdf-view-scroll-up-or-next-page)

(define-key pdf-view-mode-map [remap evil-scroll-up] 'pdf-view-scroll-down-or-previous-page)
(define-key pdf-view-mode-map [remap evil-scroll-down] 'pdf-view-scroll-up-or-next-page)

;; goto
(define-key pdf-view-mode-map [remap evil-goto-first-line] 'pdf-view-first-page)

;; (kbd "G") = (evil-goto-line LAST-LINE)
(define-key pdf-view-mode-map [remap evil-goto-line] 'pdf-view-last-page)
;; search
(define-key pdf-view-mode-map [remap counsel-grep-or-swiper] 'isearch-forward)
(define-key pdf-view-mode-map [remap my/use-swiper-or-grep] 'isearch-forward)
(define-key pdf-view-mode-map [remap swiper] 'isearch-forward)
(define-key pdf-view-mode-map [remap counsel-grep] 'isearch-forward)

;; Movement
(define-key pdf-view-mode-map [remap evil-next-line] '(lambda () (interactive) (image-next-line 4)))
(define-key pdf-view-mode-map [remap evil-previous-line] '(lambda () (interactive) (image-previous-line 4)))

(define-key pdf-view-mode-map [remap evil-forward-char] '(lambda () (interactive) (image-forward-hscroll 8)))
(define-key pdf-view-mode-map [remap evil-backward-char] '(lambda () (interactive) (image-backward-hscroll 8)))

;; Disable other modes
(evil-define-key 'normal pdf-view-mode-map (kbd "i") 'nil)
(evil-define-key 'normal pdf-view-mode-map (kbd "v") 'nil)
(evil-define-key 'normal pdf-view-mode-map (kbd "R") 'nil)

;; Zoom
(evil-define-key 'normal pdf-view-mode-map (kbd "-") 'pdf-view-shrink)
(evil-define-key 'normal pdf-view-mode-map (kbd "=") 'pdf-view-enlarge)
(evil-define-key 'normal pdf-view-mode-map (kbd "_") 'pdf-view-scale-reset)
(evil-define-key 'normal pdf-view-mode-map (kbd "+") 'pdf-view-scale-reset)

;; Add to leader map
(define-prefix-command 'my/pdf-view-mode-map)
(evil-define-key 'normal pdf-view-mode-map (kbd (concat my/leader-map-key " a")) 'my/pdf-view-mode-map)

(define-key my/pdf-view-mode-map (kbd "o") 'pdf-occur)
(define-key my/pdf-view-mode-map (kbd "t") 'doc-view-open-text)
(define-key my/pdf-view-mode-map (kbd "n") 'pdf-view-midnight-minor-mode)
(define-key my/pdf-view-mode-map (kbd "g") 'pdf-view-goto-label)
(define-key my/pdf-view-mode-map (kbd "i") 'pdf-view-extract-region-image)

;; ** Image mode
(require 'image-mode)

(add-hook 'image-mode-hook '(lambda () (interactive) (display-line-numbers-mode -1)))

;; *** Open otf fonts with image mode
(add-to-list 'auto-mode-alist '("\\.otf\\'" . image-mode))

;; *** Blimp
(straight-use-package 'blimp)

(with-eval-after-load 'eimp
  (setq eimp-enable-undo t))

(add-hook 'image-mode-hook 'blimp-mode)

;; **** Recolor
(defun my/blimp-annotate-middle()
  (interactive)
  (blimp-add-to-command-stack (list "-gravity" "Center" "-fill" "red" "-pointsize" "25" "-annotate" "0,0" (completing-read "enter text " nil)))
  (blimp-execute-command-stack)
  (sleep-for 0.2))

;; *** Keys
(evil-define-key 'normal image-mode-map (kbd "-") 'image-decrease-size)
(evil-define-key 'normal image-mode-map (kbd "=") 'image-increase-size)
(evil-define-key 'normal image-mode-map (kbd "_") 'image-transform-fit-to-height)
(evil-define-key 'normal image-mode-map (kbd "+") 'image-transform-fit-to-width)

(evil-define-key 'normal image-mode-map (kbd "C-u") 'image-scroll-down)
(evil-define-key 'normal image-mode-map (kbd "C-w") 'image-scroll-up)

(evil-define-key 'normal image-mode-map (kbd "n") '(lambda () (interactive) (image-next-line 8)))
(evil-define-key 'normal image-mode-map (kbd "p") '(lambda () (interactive) (image-previous-line 8)))
(evil-define-key 'normal image-mode-map (kbd "h") '(lambda () (interactive) (image-backward-hscroll 8)))
(evil-define-key 'normal image-mode-map (kbd "l") '(lambda () (interactive) (image-forward-hscroll 8)))

(evil-define-key 'normal image-mode-map (kbd "G") '(lambda () (interactive) (image-next-line 100)))
(evil-define-key 'normal image-mode-map (kbd "g g") '(lambda () (interactive) (image-previous-line 100)))

(evil-define-key 'normal image-mode-map (kbd "$") '(lambda () (interactive) (image-forward-hscroll 100)))
(evil-define-key 'normal image-mode-map (kbd "0") '(lambda () (interactive) (image-backward-hscroll 100)))

(define-prefix-command 'my/image-mode-map)
(evil-define-key 'normal image-mode-map (kbd (concat my/leader-map-key " a")) 'my/image-mode-map)

(define-key my/image-mode-map (kbd "i") 'blimp-interface)
(define-key my/image-mode-map (kbd "I") 'blimp-interface-execute)

(define-key my/image-mode-map (kbd "r") 'blimp-clear-command-stack)
(define-key my/image-mode-map (kbd "e") 'blimp-execute-command-stack)
(define-key my/image-mode-map (kbd "p") 'blimp-toggle-prefix)
(define-key my/image-mode-map (kbd "p") 'blimp-toggle-prefix)

(define-key my/image-mode-map (kbd "a") 'my/blimp-annotate-middle)

;; * Spray
(straight-use-package 'spray)
(require 'spray)

(setq spray-wpm 500)

(define-key spray-mode-map (kbd "p") 'spray-slower)
(define-key spray-mode-map (kbd "n") 'spray-faster)

(define-key my/leader-map (kbd "M-v") 'spray-mode)

;; * Ligatures
(if window-system
    (global-prettify-symbols-mode 1))
(setq prettify-symbols-unprettify-at-point 'right-edge)

;; Redefine so that prettify mode is enabled even if a buffer local symbols alist isn't defined
;; (defun turn-on-prettify-symbols-mode ()
;; (when (not prettify-symbols-mode)
;; (prettify-symbols-mode 1)))

;; Replace comments with symbol
;; "^[\s\\|\t]*;+"
;; \\(^ *;; \\*\\)

;; ** Magit
;; Prettify symbols doesn't work with magit
(add-hook 'magit-mode-hook '(lambda () (interactive) (prettify-symbols-mode -1)))

;; ** Symbols
;; Read =reference-point-alist= to understand how to merge characters and add spaces to characters

;; *** Generic
(defconst my/generic-equal-symbols
  '(
    ("==" . ?≡)
    ("/=" . ?≢)
    ("!=" . ?≠)
    (">=" . ?≥)
    ("<=" . ?≤)
    ))

(defconst my/generic-arrow-symbols
  '(
    ("-<" . ?↢)
    (">-" . ?↣)
    ("~>" . ?⇝)
    ("<~" . ?⇜)
    ("->" . ?→)
    ("<-" . ?←)
    ("=>" . ?⇒)
    ("<=" . ?⇐)
    ("->>" . ?↠)
    ("<<-" . ?↞)
    ("|>" . ?⊳)
    ("<|" . ?⊲)
    ("<<" . ?≪)
    (">>" . ?≫)))

(defconst my/generic-greek-symbols
  '(("lambda" . ?λ)))

(defconst my/generic-logic-symbols
  '(("&&" . ?∧)
    ("||" . ?∨)))

(defconst my/pretty-comment-symbol ?|)

(defun my/prettify-comment ()
  `((,(string-trim comment-start) . ,my/pretty-comment-symbol)))

(defun my/prettify-comment-lisp ()
  `((,(concat (string-trim comment-start) (string-trim comment-start)) . ,my/pretty-comment-symbol)))

(defun my/prettify-outline-heading ()
  `(
    (,(concat (string-trim comment-start) " *") . ?◉)
    (,(concat (string-trim comment-start) " **") . (?\s (Br . Bl) ?○))
    (,(concat (string-trim comment-start) " ***") . (?\s (Br . Bl) ?\s (Br . Bl) ?✸))
    (,(concat (string-trim comment-start) " ****") . (?\s (Br . Bl) ?\s (Br . Bl) ?\s (Br . Bl) ?✿))
    (,(concat (string-trim comment-start) " *****") . (?\s (Br . Bl) ?\s (Br . Bl) ?\s (Br . Bl) ?\s (Br . Bl) ?◉))
    (,(concat (string-trim comment-start) " ******") . (?\s (Br . Bl) ?\s (Br . Bl) ?\s (Br . Bl) ?\s (Br . Bl) ?\s (Br . Bl) ?○))
    (,(concat (string-trim comment-start) " *******") . (?\s (Br . Bl) ?\s (Br . Bl) ?\s (Br . Bl) ?\s (Br . Bl) ?\s (Br . Bl) ?\s (Br . Bl) ?✸))
    (,(concat (string-trim comment-start) " ********") . (?\s (Br . Bl) ?\s (Br . Bl) ?\s (Br . Bl) ?\s (Br . Bl) ?\s (Br . Bl) ?\s (Br . Bl) ?\s (Br . Bl) ?✿))))

(defun my/prettify-outline-heading-lisp ()
  `((,(concat (string-trim comment-start) (string-trim comment-start) " *") . ?◉)
    (,(concat (string-trim comment-start) (string-trim comment-start) " **") . (?\s (Br . Bl) ?○))
    (,(concat (string-trim comment-start) (string-trim comment-start) " ***") . (?\s (Br . Bl) ?\s (Br . Bl) ?✸))
    (,(concat (string-trim comment-start) (string-trim comment-start) " ****") . (?\s (Br . Bl) ?\s (Br . Bl) ?\s (Br . Bl) ?✿))
    (,(concat (string-trim comment-start) (string-trim comment-start) " *****") . (?\s (Br . Bl) ?\s (Br . Bl) ?\s (Br . Bl) ?\s (Br . Bl) ?◉))
    (,(concat (string-trim comment-start) (string-trim comment-start) " ******") . (?\s (Br . Bl) ?\s (Br . Bl) ?\s (Br . Bl) ?\s (Br . Bl) ?\s (Br . Bl) ?○))
    (,(concat (string-trim comment-start) (string-trim comment-start) " *******") . (?\s (Br . Bl) ?\s (Br . Bl) ?\s (Br . Bl) ?\s (Br . Bl) ?\s (Br . Bl) ?\s (Br . Bl) ?✸))
    (,(concat (string-trim comment-start) (string-trim comment-start) " ********") . (?\s (Br . Bl) ?\s (Br . Bl) ?\s (Br . Bl) ?\s (Br . Bl) ?\s (Br . Bl) ?\s (Br . Bl) ?\s (Br . Bl) ?✿))))

(defun my/prettify-outline-heading-lisp-classic ()
  `((,(my/repeat-char (string-trim comment-start) "" 3) . ?◉)
    (,(my/repeat-char (string-trim comment-start) "" 4) . (?\s (Br . Bl) ?○))
    (,(my/repeat-char (string-trim comment-start) "" 5) . (?\s (Br . Bl) ?\s (Br . Bl) ?✸))
    (,(my/repeat-char (string-trim comment-start) "" 6) . (?\s (Br . Bl) ?\s (Br . Bl) ?\s (Br . Bl) ?✿))
    (,(my/repeat-char (string-trim comment-start) "" 7) . (?\s (Br . Bl) ?\s (Br . Bl) ?\s (Br . Bl) ?\s (Br . Bl) ?◉))
    (,(my/repeat-char (string-trim comment-start) "" 8) . (?\s (Br . Bl) ?\s (Br . Bl) ?\s (Br . Bl) ?\s (Br . Bl) ?\s (Br . Bl) ?○))
    (,(my/repeat-char (string-trim comment-start) "" 9) . (?\s (Br . Bl) ?\s (Br . Bl) ?\s (Br . Bl) ?\s (Br . Bl) ?\s (Br . Bl) ?\s (Br . Bl) ?✸))
    (,(my/repeat-char (string-trim comment-start) "" 10) . (?\s (Br . Bl) ?\s (Br . Bl) ?\s (Br . Bl) ?\s (Br . Bl) ?\s (Br . Bl) ?\s (Br . Bl) ?\s (Br . Bl) ?✿))))

;; *** Fsharp
(defconst my/fsharp-symbols
  (list))

;; *** Haskell
(defconst my/haskell-symbols
  '(("\\" . ?λ)
    ("()" . ?∅)
    ("!!" . ?‼)
    ("sqrt" . ?√)
    ("undefined" . ?⊥)
    ("pi" . ?π)
    ;;("::" . ?∷)
    ;; Here we construct a custom symbol that has the spaces that are removed when replacing " . " with a single char
    (" . " . (?\s (Br . Bl) ?\s (Bc . Bc) ?\s (Br . Bl) ?\s (Bc . Bc) ?∘)) ; "○"
    ;; Doesn't work?
    ;;haskell-font-lock-dot-is-not-composition)
    ("forall" . ?∀)))

;; *** Elisp
(defconst my/elisp-symbols
  '(("defun" . ?λ)
    ("not" . ?¬)))

;; *** Set symbol by mode
(defun my/get-pretty-symbols-by-mode (mode)
  (pcase mode
    ('haskell-mode (append
		    (my/prettify-comment)
		    my/haskell-symbols
		    my/generic-greek-symbols
		    my/generic-equal-symbols
		    my/generic-arrow-symbols
		    my/generic-logic-symbols
		    (my/prettify-outline-heading)
		    ))
    ('fsharp-mode (append
		   (my/prettify-comment)
		   my/fsharp-symbols
		   my/generic-greek-symbols
		   my/generic-equal-symbols
		   my/generic-arrow-symbols
		   (my/prettify-outline-heading)
		   ))
    ('emacs-lisp-mode (append
		       (my/prettify-comment-lisp)
		       ;;(my/prettify-comment-lisp)
		       my/elisp-symbols
		       my/generic-greek-symbols
		       my/generic-equal-symbols
		       my/generic-arrow-symbols
		       (my/prettify-outline-heading-lisp)
		       (my/prettify-outline-heading-lisp-classic)
		       ))
    ('lisp-interaction-mode (append
			     (my/prettify-comment)
			     ;;(my/prettify-comment-lisp)
			     my/elisp-symbols
			     my/generic-greek-symbols
			     my/generic-equal-symbols
			     my/generic-arrow-symbols
			     (my/prettify-outline-heading-lisp)
			     (my/prettify-outline-heading-lisp-classic)
			     ))
    
    (_ (append
	(my/prettify-comment)
	my/generic-greek-symbols
	my/generic-equal-symbols
	my/generic-arrow-symbols
	my/generic-logic-symbols
	(my/prettify-outline-heading)
	))))

(add-hook 'prog-mode-hook '(lambda () (interactive)
			     (setq-local prettify-symbols-alist
					 (my/get-pretty-symbols-by-mode major-mode))))

;; ** Enable modify symbols inside comment blocks
(defun my/prettify-symbols-default-compose-p (start end _match)
  "Return true iff the symbol MATCH should be composed.
   The symbol starts at position START and ends at position END.
   This is the default for `prettify-symbols-compose-predicate'
   which is suitable for most programming languages such as C or Lisp."
  ;; Check that the chars should really be composed into a symbol.
  (let* ((syntaxes-beg (if (memq (char-syntax (char-after start)) '(?w ?_))
			   '(?w ?_) '(?. ?\\)))
	 (syntaxes-end (if (memq (char-syntax (char-before end)) '(?w ?_))
			   '(?w ?_) '(?. ?\\))))
    (not (or (memq (char-syntax (or (char-before start) ?\s)) syntaxes-beg)
	     (memq (char-syntax (or (char-after end) ?\s)) syntaxes-end)
	     ;; It looks like this part makes it ignore comments, remove it
	     ;;(nth 8 (syntax-ppss))
	     ))))

(setq-default prettify-symbols-compose-predicate #'my/prettify-symbols-default-compose-p)

;; * Visuals
;; ** Indicate empty lines
(setq-default indicate-empty-lines t)

;; ** Center text
(straight-use-package 'olivetti)

(setq-default olivetti-body-width 150)

(define-key my/leader-map (kbd "V") 'olivetti-mode)

;; ** Fringe size
;; Used by diff-hl and flycheck
;; Fringe only on the left side
(fringe-mode '(5 . 0))

;; ** Beacon
;; (straight-use-package 'beacon)

;; (beacon-mode 1)

;; ** Rainbow
;; Changes background of eg. #FF00FF to represent color
(straight-use-package 'rainbow-mode)

;; ** Highlight current line
(global-hl-line-mode t)

;; ** Symbol overlay
;; Supposed to be faster thang highlight-thing
(straight-use-package '(symbol-overlay :type git :host github :repo "walseb/symbol-overlay"))

(setq symbol-overlay-idle-time nil)

(define-globalized-minor-mode global-symbol-overlay
  symbol-overlay-mode symbol-overlay-mode)
(symbol-overlay-mode)
(global-symbol-overlay)

;; *** Disable in insert mode
(defun symbol-overlay-post-command ()
  "Installed on `post-command-hook'."
  (unless (string= (symbol-overlay-get-symbol nil t) symbol-overlay-temp-symbol)
    (symbol-overlay-remove-temp)
    (when (and (eq evil-state 'normal) (not symbol-overlay-idle-time))
      (symbol-overlay-maybe-put-temp))))

;; ** Show paren
;; Highlights matching paren under cursor

;; *** Delay
(setq show-paren-delay 0)

;; *** Set paren style
(setq show-paren-style 'paren)

;; *** Show offscreen expression
;; https://with-emacs.com/posts/editing/show-matching-lines-when-parentheses-go-off-screen/
;; Not implemented yet
;; Define face to use
(defface my/show-paren-offscreen-face
  '((t :inherit highlight))
  "Face for showing function names offscreen")

(setq show-paren-highlight-openparen t)
(setq show-paren-when-point-inside-paren nil)
(setq show-paren-when-point-in-periphery t)

(show-paren-mode 1)

;; ** Highlight parens
;; Highlights surrounding parens
(straight-use-package 'highlight-parentheses)
(global-highlight-parentheses-mode)

;; *** Set delay
(setq hl-paren-delay 0)

;; ** Highlight changes
(define-key my/leader-map (kbd "q") 'highlight-changes-mode)

;; ** Scrollbar
(straight-use-package 'yascroll)
(global-yascroll-bar-mode)

;; ** Hl-Todo
(straight-use-package 'hl-todo)

(global-hl-todo-mode)

;; ** Hl-anything
(straight-use-package 'hl-anything)

(define-globalized-minor-mode global-hl-highlight-mode
  hl-highlight-mode hl-highlight-mode)
(hl-highlight-mode)
(global-hl-highlight-mode 1)

(define-key my/leader-map (kbd "M") 'hl-highlight-thingatpt-local)

;; ** Disable blinking cursor
(blink-cursor-mode 0)

;; ** Disable GUI
(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)

;; ** Disable comments with toggle
(straight-use-package 'hide-comnt)
(require 'hide-comnt)
(define-key my/leader-map (kbd "c") 'hide/show-comments-toggle)

;; ** Font lock
;; *** Font lock profiler
;; Use font-lock-profiler-buffer
(straight-use-package 'font-lock-profiler)

;; *** Remove unnecessary font-locks
;; **** Haskell
(setq haskell-font-lock-keywords '())

(defun haskell-font-lock-keywords ()
  '())

;; **** Elisp
(setq lisp-el-font-lock-keywords '())
(setq lisp-el-font-lock-keywords-1 '())
(setq lisp-el-font-lock-keywords-2 '())

;; **** Csharp
;; Doesn't seem to work fully, there are still lots of keywords left
(setq csharp-font-lock-keywords '())
(setq csharp-font-lock-keywords-1 '())
(setq csharp-font-lock-keywords-2 '())
(setq csharp-font-lock-keywords-3 '())

;; ** Modeline
;; Make mode line appear in echo area instead of in the mode line area. This saves space and makes it so that the mode line can't be split

;; *** Disable mode line
(setq mode-line-format nil)
(setq-default mode-line-format nil)

;; *** Mode line contents
;; Using header line to display
;; Set mode line height
(setq-default header-line-format
	      (quote
	       (
		;; Print if recursive editing
		"%["
		
		;; Information bar
		mode-line-mule-info
		mode-line-client
		
		;; If buffer is modified
		mode-line-modified
		
		;; Turns into @ when remote
		mode-line-remote
		
		" "
		
		;; Print current line number
		;;"%l"
		;;"%p"
		;;(:eval (format "%d" (/ (window-start) 0.01 (point-max))))
		;;"%p"
		
		;;"@"
		;; Print total line number and buffer position
		;; (:eval
		;; (let
		;; ((line-number-count (+ (count-lines (point-min) (point-max)) 1))
		;; (point-pos (count-lines (point) (point-min))))
		;; (let
		;; ((point-in-buffer-percentage (floor (* (/ (float point-pos) line-number-count) 100))))
		;; (concat (int-to-string point-in-buffer-percentage) "% ~" (int-to-string line-number-count)))))
		
		(:eval
		 (int-to-string (+ (count-lines (point-min) (point-max)) 1)))
		
		;;"%I"
		
		;; is narrowed
		"%n"
		
		" | "
		
		;; Print error if any
		"%e"
		
		;; Print mode
		(:eval (if defining-kbd-macro
			   "[MACRO] "))
		
		
		;; Print buffer name
		"%b > "
		
		;; Print mode
		"%m"
		
		;; Git branch and project name
		(:eval
		 (if (and (string= my/projectile-project-curr-buffer buffer-file-name) (not (string= my/projectile-project-name "-")))
		     (progn
		       (setq-local my/projectile-project-last-name-cache
				   (concat
				    " > "
				    my/buffer-git-branch
				    "@"
				    "["
				    my/projectile-project-name
				    "]"))
		       my/projectile-project-last-name-cache)
		   my/projectile-project-last-name-cache))
		
		
		;;which-func-current
		
		;; (:eval
		;; (let ((which-func (which-function)))
		;; (if which-func
		;; (concat
		;; " "
		;; which-func))))
		)))

;; *** LV-line (top modeline)
;; Use lv-line to create a mode line on the top of the screen
(defvar my/lv-line-format "")
(defconst my/lv-line--buffer " *LV-line*")
(defvar my/lv-line-window nil)

;; **** Allocate lv line update timings
(defvar my/lv-line-update-offset 8)
(defvar my/lv-line-allocated-update-limit my/lv-line-update-offset)
(defvar my/lv-line-allocated-update-current 0)

(defun my/lv-line-allocate-update-time (task)
  (if (> my/lv-line-allocated-update-current my/lv-line-allocated-update-limit)
      (setq my/lv-line-allocated-update-current 0)
    (setq my/lv-line-allocated-update-current (+ my/lv-line-allocated-update-current 1))
    (run-with-timer my/lv-line-allocated-update-current 60 task)))

;; **** LV-line update
(defun my/lv-line-update ()
  (interactive)
  (let* ((buffer (get-buffer my/lv-line--buffer)))
    (if (not buffer)
	(progn
	  (message "LV-line buffer not found - creating new one")
	  (my/lv-line-create)))
    (with-current-buffer buffer
      (erase-buffer)
      (insert (format-mode-line my/lv-line-format)))))

;; **** Create LV-line at top
(defun my/lv-line-set-buffer ()
  (setq truncate-lines nil)
  (setq-local mode-line-format nil)
  (setq indicate-empty-lines nil)
  (set-window-hscroll my/lv-line-window 0)
  (setq window-size-fixed t)
  (setq truncate-lines t)
  ;;(setq mode-line-format nil)
  
  (if window-system
      ;; Change to mono face
      (face-remap-add-relative 'default :family my/mono-font)) ;;:height my/default-face-height))
  
  ;; Offset by 10 pixels to make text fit
  (set-window-fringes (selected-window) 10 0)
  
  ;; Disable char at end of line
  (set-display-table-slot standard-display-table 0 ?\ )
  
  ;; Disable cursor
  (setq cursor-type nil)
  (setq cursor-in-non-selected-windows nil)
  
  (set-window-dedicated-p my/lv-line-window t)
  (set-window-parameter my/lv-line-window 'no-other-window t))

(defun my/lv-line-create ()
  (interactive)
  (if (not (get-buffer my/lv-line--buffer))
      (generate-new-buffer my/lv-line--buffer))
  (if (not (window-live-p my/lv-line-window))
      (let* ((original-window (selected-window)))
	(setq my/lv-line-window
	      (select-window
	       (let ((ignore-window-parameters t))
		 (split-window
		  (frame-root-window) -1 'above))))
	(switch-to-buffer my/lv-line--buffer)
	(my/lv-line-set-buffer)
	(select-window original-window))))

;; (defun my/lv-line-create ()
;; "Ensure that LV window is live and return it."
;; (if (window-live-p my/lv-line-window)
;; my/lv-line-window
;; (let ((ori (selected-window)) buf)
;; (prog1 (setq my/lv-line-window
;; (select-window
;; (let ((ignore-window-parameters t))
;; (split-window
;; (frame-root-window) -1 'above))))
;; (my/lv-line-create-buffer)
;; (select-window ori)))))

;; **** Update it
(defun my/lv-line-start()
  (my/lv-line-create)
  (my/lv-line-update)
  (run-with-timer my/lv-line-update-offset 60 'my/lv-line-update))

(if window-system
    (add-hook 'exwm-init-hook 'my/lv-line-start)
  (my/lv-line-start))

;; *** Keys
(define-prefix-command 'my/mode-line-map)
(define-key my/leader-map (kbd "M-m") 'my/mode-line-map)

;; **** Garbage Collection
(defvar my/mode-line-show-GC-stats nil)
(defun my/mode-line-toggle-show-GC-stats ()
  (interactive)
  (setq my/mode-line-show-GC-stats (not my/mode-line-show-GC-stats)))

(define-key my/mode-line-map (kbd "G") 'my/mode-line-toggle-show-GC-stats)

;; *** Mode line modules
;; http://www.holgerschurig.de/en/emacs-tayloring-the-built-in-mode-line/
;; **** Cursor position
(setq mode-line-position
      '(;; %p print percent of buffer above top of window, o Top, Bot or All
	;; (-3 "%p")
	;; %I print the size of the buffer, with kmG etc
	;; (size-indication-mode ("/" (-4 "%I")))
	;; " "
	;; %l print the current line number
	;; %c print the current column
	(line-number-mode ("%l" (column-number-mode ":%c")))))

;; **** Buffer name
;; (defvar my/buffer-name "")
;; (defvar my/max-buffer-name-length 10)

;; (defun my/update-max-buffer-name-length()
;; (interactive)
;; (setq my/max-buffer-name-length (floor (/ (frame-width) 10))))

;; (defun my/update-buffer-name-string (BUFFER)
;; (interactive)
;; (setq my/buffer-name
;; (if (> (string-width BUFFER) my/max-buffer-name-length)
;; (concat (string-trim-right (substring BUFFER 0 my/max-buffer-name-length)) "...")
;; BUFFER)))

;; (if window-system
;; ;; At this point in the code, exwm hasn't had time to maximize the emacs frame
;; (add-hook 'exwm-init-hook 'my/update-max-buffer-name-length)
;; ;; If on terminal, just run it now since it's always maximized
;; (my/update-max-buffer-name-length))

;; ;;(add-hook 'buffer-list-update-hook (lambda () (interactive) (my/update-buffer-name-string (buffer-name)) t) t)
;; (add-hook 'my/switch-buffer-hook (lambda () (interactive) (my/update-buffer-name-string (buffer-name)) t) t)

;; ;;(add-hook 'window-configuration-change-hook (lambda () (interactive) (my/update-buffer-name-string (buffer-name)) t) t)

;; **** Which function
(require 'which-func)

(setq which-func-unknown "")

(setq which-func-current
      '(:eval
	(let ((result (gethash (selected-window) which-func-table)))
	  (if result
	      (concat
	       " | < "
	       (replace-regexp-in-string "%" "%%" result)
	       " >"
	       )))))

;; (which-function-mode 1)

;; (remove-hook 'my/switch-buffer-hook 'which-func-update)

;; (replace-regexp-in-string "%" "%%"
;; (or
;; (gethash
;; (selected-window)
;; which-func-table)
;; which-func-unknown)))
;; Could be used if doing func mode manually
;; (setq my/which-function-modes '(c-mode emacs-lisp-mode))

;; (defun my/enable-which-function ()
;; (if (member major-mode my/which-function-modes)
;; (which-function-mode 1)))

;; (add-hook 'prog-mode-hook 'my/enable-which-function)

;; **** CPU heat
(defvar my/mode-line-enable-cpu-temp nil)

(if (and
     ;; If lm_sensors is not installed
     (my/is-system-package-installed 'sensors)
     ;; If there aren't any cpu heat sensors (eg. virtual machine)
     (= 0 (string-match-p ""
			  (shell-command-to-string "sensors | grep \"Core 0:\"")))
     ;; If it returns "no sensors found"
     (not (= 0 (string-match-p "No sensors found"
			       (shell-command-to-string "sensors | grep \"Core 0:\"")))))
    (setq my/mode-line-enable-cpu-temp t))

(defvar my/cpu-temp "")

(defun my/update-cpu-temp ()
  (interactive)
  ;; FIXME emacs regexes are wierd, use position of temp in print insead
  (string-match "\+.*C\s" (shell-command-to-string "sensors | grep \"Core 0:\""))
  (setq my/cpu-temp (substring (match-string 0 (shell-command-to-string "sensors | grep \"Core 0:\"")) 0 -3)))

(if my/mode-line-enable-cpu-temp
    (my/lv-line-allocate-update-time 'my/update-cpu-temp))

;; **** Disk space
(defvar my/disk-space nil)
(defun my/update-disk-space ()
  (interactive)
  (setq my/disk-space (my/file-size-human-readable (floor (* 1000 (string-to-number (get-free-disk-space user-emacs-directory)))))))

;; **** Network traffic
;; ***** Linux
(defvar my/mode-line-enable-network-traffic nil)

(if (file-exists-p "/proc/net/dev")
    (setq my/mode-line-enable-network-traffic t))

;; ****** RX
;; Received
(defvar my/rx 0)
(defvar my/rx-delta-formatted "0")

(defun my/linux-get-network-rx ()
  (with-temp-buffer
    (insert-file-contents "/proc/net/dev")
    (goto-char 1)
    (let ((rx 0))
      (while (search-forward-regexp "^[\s\t]*\\(.*\\):" nil t)
	(unless (string= (match-string 1) "lo")
	  (setq rx (+ rx (read (current-buffer))))))
      rx)))

(defun my/linux-update-network-rx-delta ()
  (interactive)
  (setq my/rx-new (my/linux-get-network-rx))
  (setq my/rx-delta-formatted (my/file-size-human-readable (- my/rx-new my/rx)))
  (setq my/rx my/rx-new))

(if my/mode-line-enable-network-traffic
    (my/lv-line-allocate-update-time 'my/linux-update-network-rx-delta))

(my/linux-update-network-rx-delta)

;; ****** TX
;; Transmitted
(setq my/tx 0)
(defvar my/tx-delta-formatted "0")

(defun my/linux-get-network-tx ()
  (with-temp-buffer
    (insert-file-contents "/proc/net/dev")
    (goto-char 1)
    (let ((tx 0))
      (while (search-forward-regexp "^[\s\t]*\\(.*\\):" nil t)
	(unless (string= (match-string 1) "lo")
	  (forward-word 8)
	  (setq tx (+ tx (read (current-buffer))))))
      tx)))

(defun my/linux-update-network-tx-delta ()
  (interactive)
  (setq my/tx-new (my/linux-get-network-tx))
  (setq my/tx-delta-formatted  (my/file-size-human-readable (- my/tx-new my/tx)))
  (setq my/tx my/tx-new))

(if my/mode-line-enable-network-traffic
    (my/lv-line-allocate-update-time 'my/linux-update-network-tx-delta))

(my/linux-update-network-tx-delta)

;; **** Mail
(defvar my/gnus-unread-string "")

(defun my/gnus-update-unread()
  (my/gnus-scan-unread)
  (setq my/gnus-unread-string
	(concat
	 "M:"
	 (my/gnus-get-unread-mail-count)
	 " > N:"
	 (my/gnus-get-unread-news-count))))

(add-hook 'my/sync-mail-hook 'my/gnus-update-unread)
(add-hook 'gnus-summary-exit-map 'my/gnus-update-unread)

;; **** Battery
;; If there is a battery, display it in the mode line
(require 'battery)

(display-battery-mode 1)
(setq battery-mode-line-format "%th - %p")

;; **** Date and time
;; Display time and date in good format (also displays CPU load)
(defvar my/date "")
(defvar my/time "")

(defun my/update-date ()
  (interactive)
  (setq my/date (format-time-string "%d-%m-%Y")))

(defun my/update-time ()
  (interactive)
  (setq my/time (format-time-string "%H:%M")))

(my/lv-line-allocate-update-time 'my/update-time)
(run-with-timer 0 3600 'my/update-date)

;; Update date now
(my/update-time)
(my/update-date)

;; **** Git branch name
(require 'vc-git)

(defvar my/buffer-git-branch "")

(defun my/update-buffer-git-branch ()
  (interactive)
  ;; Fixes tramp
  (if (not (string= major-mode "minibuffer-inactive-mode"))
      (setq my/buffer-git-branch (car (vc-git-branches)))))

(add-hook 'my/switch-buffer-hook 'my/update-buffer-git-branch)

;; **** Git project name
;; When projectile-mode is on, project name is updated on every keypress, here it is fixed
(defvar my/projectile-project-name "")
(defvar my/projectile-project-curr-buffer "")

;; Used by mode line to remember lasts git repo it had
(defvar-local my/projectile-project-last-name-cache "")

(defun my/update-projectile-project-name()
  (interactive)
  ;; Some virtual buffers don't work, but dired-mode does
  (when (or (string= major-mode 'dired-mode) (and buffer-file-name (file-exists-p buffer-file-name)))
    (setq my/projectile-project-name (projectile-project-name))
    (setq my/projectile-project-curr-buffer buffer-file-name)))

(add-hook 'my/switch-buffer-hook 'my/update-projectile-project-name)

;; **** Load average
(defvar my/load-average 0)
(defvar my/high-load-average 2)

(defun my/update-load-average ()
  (interactive)
  (setq my/load-average (/ (nth 0 (load-average)) 100.0)))

(my/lv-line-allocate-update-time 'my/update-load-average)

(my/update-load-average)

;; **** Ram usage
(defvar my/mode-line-enable-available-mem nil)

(if (and (file-exists-p "/proc/meminfo")
	 (progn
	   (with-temp-buffer
	     
	     (insert-file-contents "/proc/meminfo")
	     (setq my/mem-string (buffer-string))
	     (string-match "MemAvailable:.*\s" my/mem-string))))
    (setq my/mode-line-enable-available-mem t))

(defvar my/available-mem-formatted "nil")
(defvar my/available-mem 0)

(defun my/linux-update-available-mem ()
  (interactive)
  (with-temp-buffer
    (insert-file-contents "/proc/meminfo")
    (setq my/mem-string (buffer-string))
    
    (string-match "MemAvailable:.*\s" my/mem-string)
    (setq my/mem-string (match-string 0 my/mem-string))
    
    ;; Default returns kb, *1000 to get it to bytes
    (setq my/available-mem (* 1000(string-to-number (substring my/mem-string (string-match "[0-9]" my/mem-string) -1))))
    (setq my/available-mem-formatted (my/file-size-human-readable my/available-mem nil t))))

(if my/mode-line-enable-available-mem
    (my/lv-line-allocate-update-time 'my/linux-update-available-mem))

;; Update available mem on startup
(my/linux-update-available-mem)

;; **** Uptime
(defvar my/uptime-start-time (float-time))
(defvar my/uptime-total-time-formated "0M")

(defun my/get-uptime-formated-time ()
  (let* ((total-time  (- (float-time) my/uptime-start-time)))
    (if (> total-time (* 60 60))
	(concat (format "%0.1f"(/ total-time 3600.0)) "H")
      (concat (int-to-string (/ (floor total-time) 60)) "M"))))

(defun my/update-uptime-timer ()
  (interactive)
  (setq my/uptime-total-time-formated (my/get-uptime-formated-time)))

(my/lv-line-allocate-update-time 'my/update-uptime-timer)

;; **** Eye timer
(defvar my/eye-timer-last-break (float-time))

;; In seconds
(defvar my/eye-timer-break-every (* 21 60))

(defun my/eye-timer-break-screen ()
  (switch-to-buffer "Break")
  (insert "Break!"))

(defun my/eye-timer-add ()
  (message "$$$$$$$$$$$$$BREAK 20 sec$$$$$$$$$$$$$$")
  (my/eye-timer-break-screen)
  ;;(my/alert "Eye timer" 'high)
  (setq my/eye-timer-last-break (float-time)))

(defun my/eye-timer-update ()
  (interactive)
  (if (> (- (float-time) my/eye-timer-last-break) my/eye-timer-break-every)
      (my/eye-timer-add)))

(my/lv-line-allocate-update-time 'my/eye-timer-update)

;; *** Mode line format
(defvar my/frame-width (frame-width))

(defun my/frame-width-update()
  (interactive)
  (setq my/frame-width (frame-width)))

;; Only applicable to X since terminal never stretches, etc
(add-hook 'exwm-workspace-switch-hook 'my/frame-width-update)
(add-hook 'exwm-init-hook (lambda () (interactive) (run-with-timer 1 nil '(lambda () (interactive) (my/frame-width-update) (my/lv-line-update)))) t)

(defun my/mode-line-align (left right)
  "Return a string of `window-width' length containing LEFT, and RIGHT aligned respectively."
  (let* ((available-width (- my/frame-width (length left) 2)))
    (format (format "%%s %%%ds" available-width) left right)))

;; mode-line-format
(setq-default my/lv-line-format
	      '(:eval
		(my/mode-line-align
		 (format-mode-line
		  (quote
		   (
		    (:eval my/past-alerts)
		    )))
		 
		 (format-mode-line
		  (quote
		   (
		    "| "
		    
		    (:eval (if my/mode-line-show-GC-stats
			       (concat
				" GC: " (number-to-string (truncate gc-elapsed))
				"(" (number-to-string gcs-done) ")"
				" |"
				)))
		    
		    (:eval (if my/mode-line-enable-network-traffic
			       (concat
				my/tx-delta-formatted " ↑ "
				my/rx-delta-formatted " ↓ | ")))
		    
		    (:eval
		     (when my/mode-line-enable-available-mem
		       (concat
			"MEM: "
			;; If < 100 mb mem, make text red
			(if (< my/available-mem 1000000000)
			    (propertize my/available-mem-formatted 'face `(:background "red"))
			  my/available-mem-formatted)
			" | ")))
		    
		    ;;(:eval (concat "Org:" org-mode-line-string))
		    (:eval (if (boundp 'org-mode-line-string)
			       (concat "Org:" org-mode-line-string " | ")))
		    (:eval (if (not (eq battery-mode-line-string ""))
			       (concat "BAT: " battery-mode-line-string "%%%   | ")))
		    
		    (:eval (if my/mode-line-enable-cpu-temp
			       (concat " - " my/cpu-temp)))
		    
		    "C: "
		    (:eval (number-to-string my/load-average))
		    
		    " |"
		    
		    (:eval (concat " Up: " my/uptime-total-time-formated))
		    
		    (:eval (if (not (string= my/gnus-unread-string ""))
			       (concat " | "
				       my/gnus-unread-string)))
		    
		    
		    " | "
		    
		    (:eval my/time)
		    
		    " - "
		    
		    (:eval my/date)
		    ))))))

;; **** Csharp
(setq csharp-font-lock-keywords '())
(setq csharp-font-lock-keywords-1 '())
(setq csharp-font-lock-keywords-2 '())
(setq csharp-font-lock-keywords-3 '())

;; **** C
(setq cpp-font-lock-keywords '())
(setq c++-font-lock-keywords '())
(setq c++-font-lock-keywords-1 '())
(setq c++-font-lock-keywords-2 '())
(setq c++-font-lock-keywords-3 '())
(setq c-font-lock-keywords '())
(setq c-font-lock-keywords-1 '())
(setq c-font-lock-keywords-2 '())
(setq c-font-lock-keywords-3 '())


;; * Set theme
(defvar my/default-face-list '())

(defun my/set-face-to-default (face-name is-syntax)
  (add-to-list 'my/default-face-list face-name)
  ;; Reset face
  (set-face-attribute face-name nil :family 'unspecified :foundry 'unspecified :width 'unspecified :height 'unspecified :weight 'unspecified :slant 'unspecified :foreground 'unspecified :background 'unspecified :underline 'unspecified :overline 'unspecified :strike-through 'unspecified :box 'unspecified :stipple 'unspecified :font 'unspecified :inherit 'default))

;; (defun my/clean-font-lock-keywords ()
;;  (interactive)
;;  (mapc 'my/count font-lock-keywords-alist)
;;  )
;; (setq my/counter 0)
;; (defun my/count (&optional rest)
;;  (setq my/counter (+ 1 my/counter)))

(defun my/theme ()
  (interactive)
  (cl-loop for face in (face-list) do
	   ;; Don't change magit faces
	   (if (not (string-match "magit" (symbol-name face)))
	       (set-face-attribute face nil :foreground nil :background nil)))
  
  (setq my/diff-added-color "#335533")
  (setq my/diff-added-hl-color (color-lighten-name "#335533" 20))
  
  (setq my/diff-changed-color "#aaaa22")
  (setq my/diff-changed-hl-color (color-lighten-name "#aaaa22" 20))
  
  (setq my/diff-removed-color "#553333")
  (setq my/diff-removed-hl-color (color-lighten-name "#553333" 20))
  
  (setq my/diff-ancestor-color "#5f06b26ccd93")
  (setq my/diff-ancestor-hl-color (color-lighten-name "#5f06b26ccd93" 20))
  
  (if window-system
      (progn
	(setq my/mark-color my/diff-changed-color)
	(setq my/mark-color-1 (color-darken-name my/diff-changed-color 5))
	(setq my/mark-color-2 (color-darken-name my/diff-changed-color 10))
	(setq my/mark-color-3 (color-darken-name my/diff-changed-color 15))
	(setq my/mark-color-4 (color-darken-name my/diff-changed-color 20))
	(setq my/mark-color-5 (color-darken-name my/diff-changed-color 25))
	(setq my/mark-color-6 (color-darken-name my/diff-changed-color 30))
	
	(setq my/foreground-color "#E6E1DC")
	(setq my/foreground-color-1 (color-darken-name my/foreground-color 5))
	(setq my/foreground-color-2 (color-darken-name my/foreground-color 10))
	(setq my/foreground-color-3 (color-darken-name my/foreground-color 15))
	(setq my/foreground-color-4 (color-darken-name my/foreground-color 20))
	(setq my/foreground-color-5 (color-darken-name my/foreground-color 25))
	(setq my/foreground-color-6 (color-darken-name my/foreground-color 30))
	
	(setq my/background-color "#232323")
	(setq my/background-color-1 (color-lighten-name my/background-color 5))
	(setq my/background-color-2 (color-lighten-name my/background-color 10))
	(setq my/background-color-3 (color-lighten-name my/background-color 15))
	(setq my/background-color-4 (color-lighten-name my/background-color 20)))
    
    (setq my/mark-color "yellow")
    
    (setq my/foreground-color "white")
    (setq my/foreground-color-1 "white")
    (setq my/foreground-color-2 "white")
    (setq my/foreground-color-3 "white")
    (setq my/foreground-color-4 "white")
    (setq my/foreground-color-5 "white")
    (setq my/foreground-color-6 "white")
    
    (setq my/background-color "black")
    (setq my/background-color-1 "black")
    (setq my/background-color-2 "black")
    (setq my/background-color-3 "black")
    (setq my/background-color-4 "black")
    
    (setq my/diff-added-color "green")
    (setq my/diff-changed-color "yellow")
    (setq my/diff-removed-color "red"))
  
	 ;;; Emacs
  (set-face-attribute 'default nil :foreground my/foreground-color :background my/background-color)
  (set-face-attribute 'link nil :foreground my/background-color :background my/foreground-color)
  (set-face-attribute 'highlight nil :foreground my/foreground-color :background my/mark-color)
  (set-face-attribute 'region nil :foreground my/foreground-color :background my/mark-color)
  (set-face-attribute 'error nil :foreground "#c6350b" :background)
  (set-face-attribute 'warning nil :foreground "DarkOrange" :background)
  
  ;; Syntax
  
  ;;font-lock-builtin-face
  ;;  font-lock-comment-delimiter-face
  ;;  font-lock-comment-face
  ;;  font-lock-constant-face
  ;;  font-lock-doc-face
  ;;  font-lock-function-name-face
  ;;  font-lock-keyword-face
  ;;  font-lock-negation-char-face
  ;;  font-lock-preprocessor-face
  ;;  font-lock-regexp-grouping-backslash
  ;;  font-lock-regexp-grouping-construct
  ;;  font-lock-string-face
  ;;  font-lock-type-face
  ;;  font-lock-variable-name-face
  ;;  font-lock-warning-face
  
  
  (set-face-attribute 'font-lock-doc-face nil :foreground my/foreground-color :background my/background-color-4)
  
  (set-face-attribute 'font-lock-comment-face nil :foreground (color-lighten-name my/background-color 30) :background my/background-color) ;;:height my/comment-face-height)
  
  (my/set-face-to-default 'font-lock-string-face t)
  
  (my/set-face-to-default 'font-lock-function-name-face t)
  
  ;; Required by other face
  (my/set-face-to-default 'outline-4 t)
  
  ;; Evil
  (setq evil-emacs-state-cursor '("purple" box))
  (setq evil-normal-state-cursor '("red" box))
  (setq evil-visual-state-cursor '("yellow" box))
  (setq evil-insert-state-cursor '("orange" box))
  (setq evil-replace-state-cursor '("green" box))
  (setq evil-operator-state-cursor '("white" hollow))
  
  ;; On-screen
  ;;(set-face-attribute 'on-screen-shadow nil :foreground nil :background (color-lighten-name my/background-color 2))
  ;;(set-face-attribute 'on-screen-fringe nil :foreground my/foreground-color :background my/background-color)
  
  ;; Hl current line
  ;; Underlines part of current line
  ;;(set-face-attribute 'hl-line nil :foreground my/foreground-color :background nil :underline t)
  (require 'hl-line)
  (set-face-attribute 'hl-line nil :foreground my/foreground-color :background my/background-color-2 :underline nil)
  
	 ;;;  Org
  ;; =make this bold=
  (set-face-attribute 'org-verbatim nil :weight 'bold)
  (set-face-attribute 'org-code nil :family my/mono-font)
  
  (set-face-attribute 'org-quote nil :slant 'italic)
  
  (set-face-attribute 'org-mode-line-clock nil :foreground my/foreground-color :background my/foreground-color :height 'unspecified)
  
  (set-face-attribute 'org-mode-line-clock-overrun nil :foreground my/foreground-color :background "red" :height 'unspecified)
  
  (set-face-attribute 'org-agenda-filter-effort nil :foreground my/foreground-color :background my/background-color :height 'unspecified)
  
  (set-face-attribute 'org-agenda-filter-regexp nil :foreground my/foreground-color :background my/background-color :height 'unspecified)
  
  (set-face-attribute 'org-agenda-filter-tags nil :foreground my/foreground-color :background my/background-color :height 'unspecified)
  
  (set-face-attribute 'org-agenda-filter-category nil :foreground my/foreground-color :background my/background-color :height 'unspecified)
  
	 ;;; Diff
  (set-face-attribute 'diff-added nil  :background my/diff-added-color)
  (set-face-attribute 'diff-changed nil :background my/diff-changed-color)
  (set-face-attribute 'diff-removed nil :background my/diff-removed-color)
  
  (set-face-attribute 'diff-refine-added nil  :background my/diff-added-hl-color)
  (set-face-attribute 'diff-refine-changed nil :background my/diff-changed-hl-color)
  (set-face-attribute 'diff-refine-removed nil :background my/diff-removed-hl-color)
  
       ;;; Ediff
  (set-face-attribute 'ediff-current-diff-A nil :background my/diff-removed-color)
  (set-face-attribute 'ediff-current-diff-Ancestor nil :background my/diff-ancestor-color)
  (set-face-attribute 'ediff-current-diff-B nil :background my/diff-added-color)
  (set-face-attribute 'ediff-current-diff-C nil :background my/diff-changed-color)
  (set-face-attribute 'ediff-even-diff-A nil :background (color-darken-name my/diff-removed-color 18))
  (set-face-attribute 'ediff-even-diff-Ancestor nil :background (color-darken-name my/diff-ancestor-color 30))
  (set-face-attribute 'ediff-even-diff-B nil :background (color-darken-name my/diff-added-color 18))
  (set-face-attribute 'ediff-even-diff-C nil :background (color-darken-name my/diff-changed-color 18))
  (set-face-attribute 'ediff-fine-diff-A nil :background my/diff-removed-hl-color)
  (set-face-attribute 'ediff-fine-diff-Ancestor nil :background my/diff-ancestor-hl-color)
  (set-face-attribute 'ediff-fine-diff-B nil :background my/diff-added-hl-color)
  (set-face-attribute 'ediff-fine-diff-C nil :background my/diff-changed-hl-color)
  (set-face-attribute 'ediff-odd-diff-A nil :background (color-darken-name my/diff-removed-color 20))
  (set-face-attribute 'ediff-odd-diff-Ancestor nil :background (color-darken-name my/diff-ancestor-color 50))
  (set-face-attribute 'ediff-odd-diff-B nil :background (color-darken-name my/diff-added-color 20))
  (set-face-attribute 'ediff-odd-diff-C nil :background (color-darken-name my/diff-changed-color 20))
  
     ;;;  Show-paren
  (set-face-attribute 'show-paren-match nil :background my/background-color :foreground my/foreground-color)
  (set-face-attribute 'show-paren-match-expression nil :background my/foreground-color :foreground my/background-color)
  (set-face-attribute 'my/show-paren-offscreen-face nil :inherit 'highlight)
  
     ;;; Wgrep
  (set-face-attribute 'wgrep-file-face nil :background my/foreground-color-6 :foreground my/background-color)
  
     ;;; Ivy grep
  (set-face-attribute 'ivy-grep-info nil :background my/foreground-color-6 :foreground my/background-color)
  
	 ;;; Symbol overlay
  (if window-system
      (set-face-attribute 'symbol-overlay-default-face nil :foreground my/foreground-color :background my/mark-color-5))
  
	 ;;; Dired
  (set-face-attribute 'dired-directory nil :foreground my/background-color :background my/foreground-color)
  
	 ;;; Spray
  ;;  (set-face-attribute 'spray-accent-face nil :foreground "red" :background my/background-color)
  (set-face-attribute 'spray-accent-face nil :foreground my/foreground-color :background my/background-color :underline t)
  
	 ;;; Isearch
  (set-face-attribute 'isearch nil :foreground my/background-color :background my/foreground-color)
  (set-face-attribute 'lazy-highlight nil :foreground my/background-color :background my/foreground-color)
  
	 ;;; Highlight thing
  ;;(set-face-attribute 'symbol-overlay nil :foreground my/foreground-color :background my/mark-color)
  
	 ;;; Company
  (set-face-attribute 'company-scrollbar-bg nil :background my/background-color :family my/mono-font)
  (set-face-attribute 'company-scrollbar-fg nil :background my/foreground-color :family my/mono-font)
  
  ;; Selected entry
  (set-face-attribute 'company-tooltip-selection nil :background my/foreground-color :foreground my/background-color :family my/mono-font)
  ;; All unmatching text
  (set-face-attribute 'company-tooltip nil :foreground my/foreground-color :background my/background-color-1 :family my/mono-font)
  ;; All matching text
  (set-face-attribute 'company-tooltip-common nil :foreground my/background-color :background my/foreground-color :family my/mono-font)
  
  ;;      ;;; Company box
  ;;  (if (require 'company-box nil 'noerror)
  ;;      (progn
  ;;        (set-face-attribute 'company-box-annotation nil :family my/mono-font)
  ;;        (set-face-attribute 'company-box-background nil :family my/mono-font)
  ;;        (set-face-attribute 'company-box-candidate nil :family my/mono-font)
  ;;        (set-face-attribute 'company-box-scrollbar nil :family my/mono-font)
  ;;        (set-face-attribute 'company-box-selection nil :family my/mono-font)))
  
	 ;;; Popup menu
  ;; Selected entry
  (require 'popup)
  (set-face-attribute 'popup-menu-selection-face nil :foreground my/background-color :background my/foreground-color)
  ;; All unmatching text
  (set-face-attribute 'popup-menu-face nil :foreground my/foreground-color :background my/background-color-1)
  
	 ;;; Ivy
  (set-face-attribute 'ivy-current-match nil :foreground my/background-color :background my/foreground-color)
  (set-face-attribute 'ivy-cursor nil :foreground my/background-color :background my/foreground-color)
  (set-face-attribute 'ivy-minibuffer-match-highlight nil :foreground my/background-color :background my/foreground-color)
  
  (set-face-attribute 'ivy-minibuffer-match-face-1 nil :foreground my/background-color :background my/foreground-color)
  (set-face-attribute 'ivy-minibuffer-match-face-2 nil :foreground my/background-color :background my/foreground-color-2)
  (set-face-attribute 'ivy-minibuffer-match-face-3 nil :foreground my/background-color :background my/foreground-color-4)
  (set-face-attribute 'ivy-minibuffer-match-face-4 nil :foreground my/background-color :background my/foreground-color-6)
  
	 ;;; Swiper
  (set-face-attribute 'swiper-match-face-1 nil :foreground my/background-color :background my/foreground-color)
  (set-face-attribute 'swiper-match-face-2 nil :foreground my/background-color :background my/foreground-color-2)
  (set-face-attribute 'swiper-match-face-3 nil :foreground my/background-color :background my/foreground-color-4)
  (set-face-attribute 'swiper-match-face-4 nil :foreground my/background-color :background my/foreground-color-6)
  
	 ;;; Avy
  (set-face-attribute 'avy-lead-face nil :foreground my/background-color :background my/foreground-color-6)
  (set-face-attribute 'avy-lead-face-0 nil :foreground my/background-color :background my/foreground-color-2)
  (set-face-attribute 'avy-lead-face-1 nil :foreground my/background-color :background my/foreground-color-4)
  (set-face-attribute 'avy-lead-face-2 nil :foreground my/background-color :background my/foreground-color-6)
  
	 ;;; Eshell
  (require 'em-prompt)
  (if window-system
      (set-face-attribute 'eshell-prompt nil :foreground "purple" :background my/background-color)
    (set-face-attribute 'eshell-prompt nil :foreground "magenta" :background my/background-color))
  
	 ;;; Yascroll
  (set-face-attribute 'yascroll:thumb-fringe nil :background "slateblue" :foreground "slateblue")
  (set-face-attribute 'yascroll:thumb-text-area nil :background "slateblue")
  
	 ;;; Term
  (set-face-attribute 'term nil :family my/mono-font)
  (set-face-attribute 'term-bold nil :family my/mono-font)
  (set-face-attribute 'term-underline nil :family my/mono-font)
  
  (set-face-attribute 'term-color-black nil :foreground "black" :background "black" :family my/mono-font)
  (set-face-attribute 'term-color-blue nil :foreground "blue" :background "blue" :family my/mono-font)
  (set-face-attribute 'term-color-cyan nil :foreground "cyan" :background "cyan" :family my/mono-font)
  (set-face-attribute 'term-color-green nil :foreground "green" :background "green" :family my/mono-font)
  (set-face-attribute 'term-color-magenta nil :foreground "magenta" :background "magenta" :family my/mono-font)
  (set-face-attribute 'term-color-red nil :foreground "red" :background "red" :family my/mono-font)
  (set-face-attribute 'term-color-white nil :foreground "white" :background "white" :family my/mono-font)
  (set-face-attribute 'term-color-yellow nil :foreground "yellow" :background "yellow" :family my/mono-font)
  
	 ;;; Litable
  ;;  (set-face-attribute 'litable-result-face nil :foreground my/foreground-color :background my/background-color :weight 'bold)
  ;;  (set-face-attribute 'litable-substitution-face nil :foreground my/foreground-color :background my/background-color :weight 'bold)
  
	 ;;; Evil-goggles
	 ;;;; 2 color approach
  ;; (set-face-attribute 'evil-goggles-change-face nil :foreground my/background-color :background my/foreground-color)
  ;; (set-face-attribute 'evil-goggles-commentary-face nil :foreground my/background-color :background my/foreground-color)
  ;; (set-face-attribute 'evil-goggles-delete-face nil :foreground my/background-color :background my/foreground-color)
  ;; (set-face-attribute 'evil-goggles-fill-and-move-face nil :foreground my/background-color :background my/foreground-color)
  ;; (set-face-attribute 'evil-goggles-indent-face nil :foreground my/background-color :background my/foreground-color)
  ;; (set-face-attribute 'evil-goggles-join-face nil :foreground my/background-color :background my/foreground-color)
  ;; (set-face-attribute 'evil-goggles-paste-face nil :foreground my/background-color :background my/foreground-color)
  ;; (set-face-attribute 'evil-goggles-record-macro-face nil :foreground my/background-color :background my/foreground-color)
  ;; (set-face-attribute 'evil-goggles-replace-with-register-face nil :foreground my/background-color :background my/foreground-color)
  ;; (set-face-attribute 'evil-goggles-set-marker-face nil :foreground my/background-color :background my/foreground-color)
  ;; (set-face-attribute 'evil-goggles-shift-face nil :foreground my/background-color :background my/foreground-color)
  ;; (set-face-attribute 'evil-goggles-surround-face nil :foreground my/background-color :background my/foreground-color)
  ;; (set-face-attribute 'evil-goggles-yank-face nil :foreground my/background-color :background my/foreground-color)
  
  
  ;; (set-face-attribute 'diff-added nil  :background "green")
  ;; (set-face-attribute 'diff-changed nil :background "yellow")
  ;; (set-face-attribute 'diff-removed nil :background "red")
  
  ;; Diff-hl
  (set-face-attribute 'diff-hl-change nil :background (face-attribute 'diff-changed :background))
  
  ;; Paren highlight
  ;;(set-face-attribute 'show-paren-match nil :foreground my/foreground-color :background my/mark-color-5)
  (set-face-attribute 'show-paren-match nil :foreground my/background-color :background my/foreground-color)
  (set-face-attribute 'show-paren-mismatch nil :background "red")
  
   ;;; Mode line
  (set-face-attribute 'header-line nil
		      :foreground my/foreground-color
		      :background "#063000")
  
  ;; Mode line separator
  ;; Set mode line height
  ;;  (set-face-attribute 'mode-line nil
  ;;                      :foreground my/foreground-color
  ;;                      :background my/background-color-1)
  ;;
  ;;  (set-face-attribute 'mode-line-inactive nil
  ;;                      :foreground my/foreground-color
  ;;                      :background my/background-color)
  
  ;; Highlight faces
  ;;(highlight-indent-guides-auto-set-faces)
  
     ;;; lsp
  (my/set-face-to-default 'lsp-ui-doc-background nil)
  ;;(set-face-attribute 'lsp-ui-doc-background nil :foreground my/foreground-color :background my/background-color)
     ;;;; Doc
  (set-face-attribute 'lsp-ui-doc-header nil :foreground my/foreground-color :background my/background-color-4)
  (set-face-attribute 'lsp-ui-doc-url nil :foreground my/background-color :background my/foreground-color)
  
     ;;;; Sideline
  (my/set-face-to-default 'lsp-ui-peek-filename nil)
  (my/set-face-to-default 'lsp-ui-peek-footer nil)
  (my/set-face-to-default 'lsp-ui-peek-header nil)
  (my/set-face-to-default 'lsp-ui-peek-highlight nil)
  (my/set-face-to-default 'lsp-ui-peek-line-number nil)
  (my/set-face-to-default 'lsp-ui-peek-list nil)
  (my/set-face-to-default 'lsp-ui-peek-peek nil)
  (my/set-face-to-default 'lsp-ui-peek-selection nil)
  (my/set-face-to-default 'lsp-ui-sideline-code-action nil)
  (my/set-face-to-default 'lsp-ui-sideline-current-symbol nil)
  
  ;;  (set-face-attribute 'lsp-ui-peek-filename nil :foreground my/foreground-color :background my/background-color)
  ;;  (set-face-attribute 'lsp-ui-peek-footer nil :foreground my/foreground-color :background my/background-color)
  ;;  (set-face-attribute 'lsp-ui-peek-header nil :foreground my/foreground-color :background my/background-color)
  ;;  (set-face-attribute 'lsp-ui-peek-highlight nil :foreground my/foreground-color :background my/background-color)
  ;;  (set-face-attribute 'lsp-ui-peek-line-number nil :foreground my/foreground-color :background my/background-color)
  ;;  (set-face-attribute 'lsp-ui-peek-list nil :foreground my/foreground-color :background my/background-color)
  ;;  (set-face-attribute 'lsp-ui-peek-peek nil :foreground my/foreground-color :background my/background-color)
  ;;  (set-face-attribute 'lsp-ui-peek-selection nil :foreground my/foreground-color :background my/background-color)
  ;;  (set-face-attribute 'lsp-ui-sideline-code-action nil :foreground my/foreground-color :background my/background-color)
  ;;  (set-face-attribute 'lsp-ui-sideline-current-symbol nil :foreground my/foreground-color :background my/background-color)
  
  (set-face-attribute 'lsp-ui-sideline-global nil :foreground nil :background nil)
  (set-face-attribute 'lsp-ui-sideline-symbol nil :foreground my/background-color :background my/foreground-color)
  (set-face-attribute 'lsp-ui-sideline-symbol-info nil :foreground my/mark-color :background my/background-color)
  
     ;;;; Lens
  (set-face-attribute 'lsp-lens-face nil :foreground my/foreground-color :background my/mark-color-4 :height 0.8)
  
   ;;; Which-key
  (set-face-attribute 'which-key-command-description-face nil :family my/mono-font)
  (set-face-attribute 'which-key-docstring-face nil :family my/mono-font)
  (set-face-attribute 'which-key-group-description-face nil :family my/mono-font)
  (set-face-attribute 'which-key-highlighted-command-face nil :family my/mono-font)
  (set-face-attribute 'which-key-key-face nil :family my/mono-font)
  (set-face-attribute 'which-key-local-map-description-face nil :family my/mono-font)
  (set-face-attribute 'which-key-note-face nil :family my/mono-font)
  (set-face-attribute 'which-key-separator-face nil :family my/mono-font)
  (set-face-attribute 'which-key-special-key-face nil :family my/mono-font))

(if window-system
    (add-hook 'exwm-init-hook 'my/theme)
  (add-hook 'after-init-hook 'my/theme))

;; (counsel-faces)

(define-key my/leader-map (kbd "M-c") 'my/theme)

;; * Run command on boot
(if my/on-boot-run
    (async-shell-command my/on-boot-run))

;; * Restore gc mem
(setq gc-cons-threshold my/after-gc-mem)

;; * Report start time
(run-with-timer 4 nil (lambda () (interactive) (message (concat "Booted in " (emacs-init-time)))))


;; * Byte-compile the config
;; Byte compilation doesn't work before loading everything because of some reason, so do it now
(unless (and (file-exists-p my/config-compiled-location) (my/is-file-more-up-to-date my/config-compiled-location my/config-location))
  (byte-compile-file "~/.emacs.d/config.el")
  (message "Config is now byte compiled, restart to run it"))
