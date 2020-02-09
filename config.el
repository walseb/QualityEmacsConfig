;; -*- lexical-binding:t -*-
;; * Docs
;; ** Requirements
;; *** All
;; =xorg-server xorg-xrandr xorg-xinit xorg-setxkbmap mlocate atool unzip mono pulseaudio pavucontrol firefox the_silver_searcher imagemagick ttf-inconsolata ttf-opensans ttf-dejavu aspell aspell-en aspell-sv mpd mpc poppler poppler-glib=
;; *** All optional
;; =msbuild dotnet-sdk godot-mono guile fsharp ghc ghc-static=

;; *** Firefox
;; **** Plugins
;; https://addons.mozilla.org/en-US/firefox/addon/plasma-integration/

;; https://addons.mozilla.org/en-US/firefox/addon/yet-another-hints-extension/
;; OR
;; https://addons.mozilla.org/en-US/firefox/addon/vim-vixen/

;; ***** Vim vixen config
;; #+begin_src example
;; {
;;   "keymaps": {
;;     "<C-m>": { "type": "follow.start", "newTab": false },
;;     "<C-j>": { "type": "follow.start", "newTab": true }
;;   },
;;   "search": {
;;     "default": "google",
;;     "engines": {
;;       "google": "https://google.com/search?q={}",
;;       "yahoo": "https://search.yahoo.com/search?p={}",
;;       "bing": "https://www.bing.com/search?q={}",
;;       "duckduckgo": "https://duckduckgo.com/?q={}",
;;       "twitter": "https://twitter.com/search?q={}",
;;       "wikipedia": "https://en.wikipedia.org/w/index.php?search={}"
;;     }
;;   },
;;   "properties": {
;;     "hintchars": "anetoshdirgmlwyfubxcvkp,.q;j/z",
;;     "smoothscroll": false,
;;     "complete": "sbh"
;;   },
;;   "blacklist": [
;;   ]
;; }
;; #+end_src

;; **** System packages
;; plasma-browser-integration

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

;; ** Emacs events
;; *** Key to number
;; Run:
;; (read-event)
;; Then press a key

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

;; ** Can't find package error in package.el
;; run
;; =package-refresh-contents=
;; or restart emacs

;; ** Gnus
;; *** Setup mail with dovecot
;; 1. Use nixos config
;; 2. run my/write-mail-configs
;; 3. Change permissions =chmod 600 ~/.dovecot-pass; chmod 600 ~/.msmtprc; chmod 600 ~/.mbsyncrc;=
;; 4. Enter google app password, etc into ~/.msmtprc, ~/.mbsyncrc Don't modify .dovecot-pass
;; 5.
;; 6. Enable mail in device.el
;; 7. Restart emacs, restart dovecot, etc

;; *** How to setup name and password without dovecot
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

;; ** Keybinds
;; To find what keycode is fired when you press a key run (read-event)
;; This can be used to figure what keycode to bind with (keyboard-translate

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

;; ** Haskell IDE engine / HIE
;; *** Error on first line of file
;; **** Cabal V2 related problem
;; It could be that HIE still doesn't support cabal V2, just run =cabal configure= to fix this

;; ** Nix
;; *** Create project with cabal2nix
;; cabal init

;; Setup cabal2nix
;; #+BEGIN_SRC shell
;; # Not really necessary
;; cabal2nix . > default.nix

;; cabal2nix --shell . > shell.nix
;; # beware that HIE might not work with new-configure, run normal configure instead for V1 project
;; *nix-shell --command 'cabal new-configure'
;; #+END_SRC

;; **** Automatically create default.nix and shell.nix from the cabal file
;; Add to default.nix
;; #+BEGIN_SRC
;; # default.nix
;; { pkgs ? import <nixpkgs> {} }:

;; pkgs.haskellPackages.callCabal2nix "name" ./. {}
;; #+END_SRC

;; ***** optional
;; And lastly add to shell.nix
;; #+BEGIN_SRC
;; # shell.nix
;; (import ./. {}).env
;; #+END_SRC
;; and just run *nix-shell then cabal build or whatver
;; Edit dependencies in cabal project file
;; Nix will always be used

;; OR
;; just run
;; #+BEGIN_SRC
;; *nix-shell -A env
;; #+END_SRC
;; always instead

;; ** Java LSP with gradle doesn't start
;; This could be because of permissions. LSP builds it's own gradle build and for example .gradle dir in the project root needs to have the correct permissions to allow LSP to build everything

;; * Todo
;; ** Packages to try
;; nix-buffer

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
;; *** Bookmarks
;; https://www.reddit.com/r/emacs/comments/9bly3d/linkmarksel_use_orgmode_links_for_bookmarks/
;; https://www.emacswiki.org/emacs/BookMarks

;; ** Annotations
;; https://github.com/bastibe/annotate.el

;; ** Make magit-status faster during huge edits, or create new magit-status-fast command
;; ** Should save-window-excursion be disabled?
;; Steps to reproduce: open two window split, do M-x, close the window you focused when doing M-x, cancel the M-x with C-g

;; ** GPG doesn't remember last password when saving a symmetrically encryped buffer
;; ** Automate gnus
;; *** Notmuch gnus integration
;; ** Fix ivy grep/occur
;; Colors change when you put your cursor over custom faces
;; ** Fix change defalut directory to change save dir
;; ** Customize ivy more
;; ** Make macros faster
;; Temporarily disable "global-hl-line-mode" while running macro (takes like 70% cpu in worst cases)
;; Disable symbol-overlay while in macro (takes little cpu, but you can still gain speed)

;; ** Make macros work both ways
;; Right now macros only work if they go from the top down

;; ** Read large files package
;; There is one for dired too

;; ** Refractor config
;; Maybe split up because of bad performance?
;; Should probably create some sort of guideline for where to write down prefix keys, where to write visual headers
;; I should probably not just have a header named just "visuals"

;; ** Track down more performance problems
;; Especially when using highlight-indent-guides on large files

;; ** Org-noter
;; Great for commenting pdfs

;; ** Org-capture
;; Great for referencing to source code

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

;; ** Try to fix performance of yascrollbar

;; ** Fix unicode fonts
;; Right now unicode fonts are most of the time taller than the normal fonts

;; ** Outlines
;; Fix the heading font-lock so that it also covers the comment part
;; *** Highlight to end of window
;; Maybe the outlines should be highlighted to the end of the window

;; ** Optimize number key symbol placement
;; ** Fix meta keys
;; *** my/switch-monitor

;; ** Ivy menu for suspend-map
;; Also rename it to something better

;; ** Heading text-object

;; ** Fix my/gnus-topic-add-gmane-groups
;; It doesn't work and it's badly written

;; ** Automate email setup
;; You could easily create prompts when creating the config files that modify the password and user fields

;; ** Fix so that you can use counsel-yank in minibuffer again

;; ** Make evil-commentary also comment multiple empty lines

;; ** Haskell
;; *** Get ghc-imported-from
;; Currently broken

;; *** Get hlive
;; Currently broken

;; *** Direnv binds
;; Maybe add direnv bind for creating a .envrc with content "use nix"

;; ** Prettify symbol sometimes not working
;; This is because ~prettify-symbols--current-symbol-bounds~ sometimes gets set to a value outside the range of the current buffer like ~(2689 2691)~
;; This can be solved by setting it to nil
;; This is the full error:
;; Error in post-command-hook (prettify-symbols--post-command-hook): (args-out-of-range 2689 2691)

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

;; *** Append to end of file
(defun my/append-to-file (file string)
  ;; The 1 here is used to stop write-region from printing when it writes something
  (write-region string nil file t 1))

;; ** Is external package installed
;; Checks variable =exec-path= for package
(defun my/is-system-package-installed (package)
  (if (executable-find (symbol-name package))
      (symbol-name package)
    (message (concat "Package: " (symbol-name package) " not installed"))
    nil))

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
  (exwm-input--fake-key
   ;; (string-to-char
   key
   ;; )
   ))

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
			  ;;:family font
			  :font font
			  ;;:fontset "fontset-default"
			  :height my/default-face-height)))

;; *** Set size
(defun my/set-default-font-size ()
  (if window-system
      (set-face-attribute 'default nil
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

;; ** Repeat char
(defun my/repeat-char (char initial-string n)
  (setq initial-string (concat char initial-string))
  (if (> n 1)
      (my/repeat-char char initial-string (- n 1))
    initial-string))

;; ** Delete old functions
(defun my/delete-everything-older-than (folder time)
  (message (concat "Deleting all files older than " (number-to-string time) " seconds"))
  (let ((time time)
	(current (float-time (current-time))))
    (dolist (file (directory-files folder t))
      (when (and (backup-file-name-p file)
		 (> (- current (float-time (nth 5 (file-attributes file))))
		    time))
	(message "%s" file)
	(delete-file file)))
    (message "Deletion completed!")))

;; ** Is there any line longer than
;; From mm-bodies.el
(defun my/line-longer-than (length)
  "Say whether any of the lines in the buffer is longer than LENGTH."
  (save-excursion
    (goto-char (point-min))
    (end-of-line)
    (while (and (not (eobp))
		(not (> (current-column) length)))
      (forward-line 1)
      (end-of-line))
    (and (> (current-column) length)
	 (current-column))))

;; ** Get position of beginning of next line
(defun my/next-line-pos ()
  (save-excursion
    (forward-line)
    (beginning-of-line)
    (point)))

;; * Fonts
(defun my/get-best-font ()
  (if (my/font-installed "Liga Inconsolata LGC")
      "Liga Inconsolata LGC"
    (if (my/font-installed "Inconsolata LGC")
	"Inconsolata LGC"
      (if (my/font-installed "Inconsolata")
	  "Inconsolata"
	(if (my/font-installed "DejaVu Sans Mono")
	    "DejaVu Sans Mono"
	  (if (my/font-installed "Fira Mono")
	      "Fira Mono"
	    (if (my/font-installed "dejavu sans mono")
		"DejaVuSansMono"
	      (if (my/font-installed "Noto Sans Mono")
		  "NotoSansMono"
		(if (my/font-installed "Perfect DOS VGA 437")
		    "Perfect DOS VGA 437")))))))))

(defun my/get-best-symbol-font ()
  (if (my/font-installed "Liga Inconsolata LGC")
      "Liga Inconsolata LGC"
    (if (my/font-installed "DejaVu Sans Mono")
	"DejaVu Sans Mono"
      (if (my/font-installed "dejavu sans mono")
	  "DejaVuSansMono"
	(if (my/font-installed "Noto Sans Mono")
	    "NotoSansMono")))))

(setq my/font (my/get-best-font))
(setq my/symbol-font (my/get-best-symbol-font))

(when my/font
  ;; Set default font
  (add-to-list 'default-frame-alist (cons 'font my/font))

  (my/set-default-font-size)
  ;; (my/set-default-font my/font)

  ;; Set symbol font
  (set-fontset-font t 'symbol my/symbol-font))


;; * Startup processes
;; ** Prevent async command from opening new window
;; Buffers that I don't want popping up by default
(add-to-list 'display-buffer-alist
	     '("\\*Async Shell Command\\*.*" display-buffer-no-window))

;; ** Check if OS is fully compatible
(defvar fully-compatible-system (or (eq system-type 'gnu/linux)(eq system-type 'gnu)(eq system-type 'gnu/kfreebsd)))

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

;; ** Evil-goggles support
(straight-use-package 'evil-goggles)
(require 'evil-goggles)

(add-hook 'after-init-hook 'evil-goggles-mode)

;; Disable pulse which both fixes so that you can set foreground color on the pulse font and saves on performance
(setq evil-goggles-pulse nil)
(setq evil-goggles-duration 60)

(evil-goggles-use-diff-faces)

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

;; *** Clear default keys
(setq evil-mc-key-map nil)

;; *** Start mc-mode in this buffer
(evil-mc-mode)

;; *** Add unsupported commands
(add-to-list 'evil-mc-custom-known-commands
	     '(delete-char . ((:default . evil-mc-execute-default-call-with-count))))

(add-to-list 'evil-mc-custom-known-commands
	     '(org-delete-char . ((:default . evil-mc-execute-default-call-with-count))))

(add-to-list 'evil-mc-custom-known-commands
	     '(csharp-maybe-insert-codedoc . ((:default . evil-mc-execute-default-call-with-count))))

;; *** Enable globally
;; Enable evil-mc in all modes, including fundamental-mode
(add-hook 'evil-local-mode-hook 'evil-mc-mode)

;; *** Disable on keybord-quit (C-g)
(setq evil-mc-undo-cursors-on-keyboard-quit t)

;; *** Keys
(define-key evil-visual-state-map (kbd "A") 'evil-mc-make-cursor-in-visual-selection-end)
(define-key evil-visual-state-map (kbd "I") 'evil-mc-make-cursor-in-visual-selection-beg)

(define-key evil-normal-state-map (kbd "C-S-n") 'evil-mc-make-and-goto-next-match)
(define-key evil-normal-state-map (kbd "C-S-p") 'evil-mc-make-and-goto-prev-match)

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

(cl-loop for (mode . state) in '(
				 ;; So i C-leader works for exwm windows
				 (exwm-mode . emacs)
				 (eshell-mode . insert)
				 (interactive-haskell-mode . insert)
				 (term-mode . insert)
				 ;;(org-agenda-mode . insert)
				 (magit-popup-mode . insert)
				 (proced-mode . insert)
				 (emms-playlist-mode . insert))
	 do (evil-set-initial-state mode state))

;; *** Disable motion state
;; Motion state is like normal mode but you can't go into insert-mode. Some modes start up with this restrictive mode, disable it here
(setq evil-motion-state-modes nil)

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

(setq evil-textobj-anyblock-blocks
      '(("(" . ")")
	("{" . "}")
	("\\[" . "\\]")
	("<" . ">")
	("\"" . "\"")
	("“" . "”")))

;; *** Evil commentary
(straight-use-package 'evil-commentary)

(evil-define-key 'normal evil-commentary-mode-map ":" 'evil-commentary-line)
(evil-define-key '(normal visual) evil-commentary-mode-map ";" 'evil-commentary)

(evil-define-key 'normal evil-commentary-mode-map "gY" 'evil-commentary-yank-line)

;; **** Allow commenting empty line
;; Because of some reason emacs crashes with undo tree error if this isn't run late
(add-hook 'after-init-hook
	  (lambda ()
	    (evil-commentary-mode)

	    (evil-define-operator evil-commentary-line (beg end type)
	      "Comment or uncomment [count] lines."
	      :motion evil-line
	      :move-point nil
	      (interactive "<R>")
	      (when (evil-visual-state-p)
		(unless (memq type '(line block))
		  (let ((range (evil-expand beg end 'line)))
		    (setq beg (evil-range-beginning range)
			  end (evil-range-end range)
			  type (evil-type range))))
		(evil-exit-visual-state))
	      ;; If current line is blank
	      (if (save-excursion
		    (beginning-of-line)
		    (looking-at "[[:space:]]*$"))
		  (insert comment-start)
		(evil-commentary beg end type)))))

;; *** Evil-eval operator
(evil-define-operator evil-eval (beg end type)
  "Run eval on BEG to END."
  (interactive "<R>")
  (my/auto-eval-region beg end))

(my/evil-normal-define-key "/" 'evil-eval)
(my/evil-normal-define-key "?" 'my/auto-eval)

;; **** Add evil-goggle command
(add-to-list 'evil-goggles--commands '(evil-eval :face evil-goggles-yank-face :switch evil-goggles-enable-yank :advice evil-goggles--generic-async-advice))

;; *** Evil-surround
(straight-use-package 'evil-surround)
(add-hook 'after-init-hook (lambda () (global-evil-surround-mode 1)))

;; **** Setup pair binds
(setq-default evil-surround-pairs-alist
	      '(
		;; Default
		(?\( . ("(" . ")"))
		(?\[ . ("[ " . "]"))
		(?\{ . ("{" . "}"))

		;; Default reversed
		(?\) . ("(" . ")"))
		(?\] . ("[" . "]"))
		(?\} . ("{" . "}"))

		;; My meta keys
		(134217841 . ("!" . "!"))
		(134217831 . ("@" . "@"))
		(134217837 . ("#" . "#"))
		(134217836 . ("$" . "$"))
		(134217847 . ("%" . "%"))
		(134217849 . ("*" . "*"))
		(134217830 . ("(" . ")"))
		(134217845 . ("(" . ")"))
		(134217826 . ("&" . "&"))
		(134217787 . ("^" . "^"))

		(134217828 . ("1" . "1"))
		(134217843 . ("2" . "2"))
		(134217844 . ("3" . "3"))
		(134217838 . ("4" . "4"))
		(134217842 . ("5" . "5"))
		(134217833 . ("6" . "6"))
		(134217825 . ("7" . "7"))
		(134217829 . ("8" . "8"))
		(134217839 . ("9" . "9"))
		(134217832 . ("0" . "0"))

		;; Blocks, etc
		(?# . ("#{" . "}"))
		(?b . ("(" . ")"))
		(?B . ("{" . "}"))
		(?> . ("<" . ">"))
		(?t . evil-surround-read-tag)
		(?< . evil-surround-read-tag)
		(?f . evil-surround-function)))

;; **** Keys
(evil-define-key 'normal evil-surround-mode-map (kbd ",") 'evil-surround-edit)
(evil-define-key 'normal evil-surround-mode-map (kbd "C-,") 'evil-Surround-edit)
(evil-define-key 'visual evil-surround-mode-map (kbd ",") 'evil-surround-region)
(evil-define-key 'visual evil-surround-mode-map (kbd "C-,") 'evil-Surround-region)

;; *** Evil-args
(straight-use-package 'evil-args)

;; bind evil-args text objects
(define-key evil-inner-text-objects-map "a" 'evil-inner-arg)
(define-key evil-outer-text-objects-map "a" 'evil-outer-arg)

;; ** Evil-lion
(straight-use-package 'evil-lion)
(evil-lion-mode)

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

;; ** Fix evil scroll
;; https://github.com/emacs-evil/evil/pull/1154/files
(evil-define-command evil-scroll-up (count)
  "Scrolls the window and the cursor COUNT lines upwards.
If COUNT is not specified the function scrolls down
`evil-scroll-count', which is the last used count.
If the scroll count is zero the command scrolls half the screen."
  :repeat nil
  :keep-visual t
  (interactive "<c>")
  (evil-save-column
    (setq count (or count (max 0 evil-scroll-count)))
    (setq evil-scroll-count count)
    (when (= (point-min) (line-beginning-position))
      (signal 'beginning-of-buffer nil))
    (when (zerop count)
      (setq count (/ (window-body-height) 2)))
    (let ((xy (evil-posn-x-y (posn-at-point))))
      (condition-case nil
	  (progn
	    (scroll-down count)
	    (goto-char (posn-point (posn-at-x-y (car xy) (cdr xy)))))
	(beginning-of-buffer
	 (condition-case nil
	     (with-no-warnings (previous-line count))
	   (beginning-of-buffer)))))))

(evil-define-command evil-scroll-down (count)
  "Scrolls the window and the cursor COUNT lines downwards.
If COUNT is not specified the function scrolls down
`evil-scroll-count', which is the last used count.
If the scroll count is zero the command scrolls half the screen."
  :repeat nil
  :keep-visual t
  (interactive "<c>")
  (evil-save-column
    (setq count (or count (max 0 evil-scroll-count)))
    (setq evil-scroll-count count)
    (when (eobp) (signal 'end-of-buffer nil))
    (when (zerop count)
      (setq count (/ (window-body-height) 2)))
    ;; BUG #660: First check whether the eob is visible.
    ;; In that case we do not scroll but merely move point.
    (if (<= (point-max) (window-end))
	(with-no-warnings (next-line count nil))
      (let ((xy (evil-posn-x-y (posn-at-point))))
	(condition-case nil
	    (progn
	      (scroll-up count)
	      (let* ((wend (window-end nil t))
		     (p (posn-at-x-y (car xy) (cdr xy)))
		     (margin (max 0 (- scroll-margin
				       (cdr (posn-col-row p))))))
		(goto-char (posn-point p))
		;; ensure point is not within the scroll-margin
		(when (> margin 0)
		  (with-no-warnings (next-line margin))
		  (recenter scroll-margin))
		(when (<= (point-max) wend)
		  (save-excursion
		    (goto-char (point-max))
		    (recenter (- (max 1 scroll-margin)))))))
	  (end-of-buffer
	   (goto-char (point-max))
	   (recenter (- (max 1 scroll-margin)))))))))

(defvar evil-cached-header-line-height nil
  "Cached height of the header line.")

(defun evil-header-line-height ()
  "Return the height of the header line.
If there is no header line, return nil."
  (let ((posn (posn-at-x-y 0 0)))
    (when (eq (posn-area posn) 'header-line)
      (cdr (posn-object-width-height posn)))))

(defun evil-posn-x-y (position)
  "Return the x and y coordinates in POSITION.
This function returns y offset from the top of the buffer area including
the header line.  This definition could be changed in future.
Note: On Emacs 22 and 23, y offset, returned by `posn-at-point' and taken
by `posn-at-x-y', is relative to the top of the buffer area including
the header line.
However, on Emacs 24, y offset returned by `posn-at-point' is relative to
the text area excluding the header line, while y offset taken by
`posn-at-x-y' is relative to the buffer area including the header line.
This asymmetry is by design according to GNU Emacs team.
This function fixes the asymmetry between them on Emacs 24 and later versions.
Borrowed from mozc.el."
  (let ((xy (posn-x-y position)))
    (when (and (> emacs-major-version 24) header-line-format)
      (setcdr xy (+ (cdr xy)
		    (or evil-cached-header-line-height
			(setq evil-cached-header-line-height (evil-header-line-height))
			0))))
    xy))

;; ** Fix evil open line
(setq evil-auto-indent nil)

;; ** Keys
;; Prevent emacs state from being exited with esc, fixes exwm since it uses emacs state and to exit hydra you have to do esc
(define-key evil-emacs-state-map (kbd "<escape>") 'keyboard-quit)

;; Couldn't bother to create custom evil-join
;; P is normally bound to manual, make this key useful
(my/evil-normal-define-key "P" 'delete-indentation)

(my/evil-normal-define-key "DEL" 'backward-delete-char-untabify)

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

;; *** Move by paragraph
(defun my/move-paragraph (forward)
  (let ((regex-forward "[[:graph:]].*\n[[:blank:]]*\n")
	(regex-backward "^[[:blank:]]*\n.*[[:graph:]]"))
    (if forward
	(if (ignore-errors (re-search-forward regex-forward)) (previous-line) (end-of-buffer))
      (unless (ignore-errors (re-search-backward regex-backward)) (beginning-of-buffer)))
    (beginning-of-line)))

(evil-define-motion my/backward-paragraph (count)
  "Move to the end of the COUNT-th next paragraph."
  :jump t
  :type exclusive
  (if count
      (dotimes (i count)
	(my/move-paragraph nil))
    (my/move-paragraph nil)))

(evil-define-motion my/forward-paragraph (count)
  "Move to the end of the COUNT-th next paragraph."
  :jump t
  :type exclusive
  (if count
      (dotimes (i count)
	(my/move-paragraph t))
    (my/move-paragraph t)))

(my/evil-normal-define-key "r" 'my/forward-paragraph)
(my/evil-visual-define-key "r" 'my/forward-paragraph)
(my/evil-normal-define-key "R" 'my/backward-paragraph)
(my/evil-visual-define-key "R" 'my/backward-paragraph)

(my/evil-normal-define-key "j" 'evil-replace)
(my/evil-visual-define-key "j" 'evil-replace)
(my/evil-normal-define-key "J" 'evil-replace-state)
(my/evil-visual-define-key "J" 'evil-replace-state)

;; *** Don't save chars deleted with x to clipboard
(my/evil-normal-define-key "x" 'delete-char)
(my/evil-normal-define-key "X"
			   (lambda () (interactive)
			     (backward-char)
			     (call-interactively #'delete-char)))

;; * Hydra
(straight-use-package 'hydra)
(setq hydra-hint-display-type 'message)

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

;; * Compatibility
;; ** Windows host clipboard crash
;; Emacs crashes from time to time when it's run in linux but the clipboard contents are from windows.
(setq x-select-request-type 'STRING)

(when my/windows-host
  (with-eval-after-load 'select
    (defun gui--selection-value-internal (type)
      (let ((request-type (if (eq window-system 'x)
			      (or x-select-request-type
				  '(UTF8_STRING COMPOUND_TEXT STRING))
			    'STRING))
	    text)
	(with-demoted-errors "gui-get-selection: %S"
	  (if (consp request-type)
	      (while (and request-type (not text))
		(setq text (gui-get-selection type (car request-type)))
		(setq request-type (cdr request-type)))
	    (setq text (gui-get-selection type request-type))))
	;; This seems to be the problem
	;; (if text
	;; (remove-text-properties 0 (length text) '(foreign-selection nil) text))
	text))))

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

(defvar my/alert-updated-hook nil)

(defun my/alert-reset ()
  (interactive)
  (setq my/past-alerts (list))
  (run-hooks 'my/alert-updated-hook))

(defun my/alert-remove ()
  (interactive)
  (setq my/past-alerts (remove (completing-read "Remove entry" my/past-alerts) my/past-alerts))
  (run-hooks 'my/alert-updated-hook))

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
;; (add-to-list 'load-path (expand-file-name (concat user-emacs-directory "local-packages")))

;; * Write configs
(defun my/write-configs ()
  (interactive)
  (pcase (completing-read "Which config to write: "
			  '("xdefaults" "xinit" "xmodmap" "mpd" "gpg-agent" "cabal" "mbsync" "msmtp" "dovecot" "direnv") nil t)
    ("xdefaults"
     ;; With emacs 27 gui is disabled in early-init.el instead of xdefaults
     (if (string< emacs-version "27")
	 (my/write-xdefaults)
       (message "Not writing xdefault file, emacs version is > 27 so you shouldn't need a xdefaults file")))
    ("xinit" (my/write-xinitrc))
    ("xmodmap" (my/write-xmodmap))
    ("mpd" (my/write-mpd-config))
    ("gpg-agent" (my/write-gpg-agent-config))
    ("cabal" (my/write-cabal-config))
    ("mbsync" (my/write-mbsync-config))
    ("msmtp" (my/write-msmtp-config))
    ("dovecot" (my/write-dovecot-config))
    ("direnv" (my/write-direnv-config))))

(define-key my/leader-map (kbd "C-c") 'my/write-configs)

;; ** Write .gnus.el
;; I thinks this is no longer needed
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
    (my/create-file-with-content-if-not-exist gpg-file (concat "allow-emacs-pinentry\n" "pinentry-program " (shell-command-to-string "which pinentry-emacs")))
    (shell-command "gpgconf --reload gpg-agent")))

;; ** Write cabal config
(defconst my/nix-config-text "nix: true
documentation: True")

(defun my/write-cabal-config ()
  (let* ((cabal-dir "~/.cabal/")
	 (cabal-file (concat cabal-dir "config")))
    (my/create-dir-if-not-exist cabal-dir)
    (my/create-file-with-content-if-not-exist cabal-file my/nix-config-text)))

;; ** Write mail configs
(defun my/write-mail-configs ()
  (interactive)
  (my/write-mbsync-config)
  (my/write-msmtp-config)
  (my/write-dovecot-config))

(defun my/write-mbsync-config ()
  (let* ((source-dir (concat user-emacs-directory "configs/mail/mbsync/.mbsyncrc"))
	 (target-dir "~/.mbsyncrc"))
    (copy-file source-dir target-dir))
  (make-directory "~/Maildir")
  (make-directory "~/Maildir/main-gmail"))

(defun my/write-msmtp-config ()
  (let* ((source-dir (concat user-emacs-directory "configs/mail/msmtp/.msmtprc"))
	 (target-dir "~/.msmtprc"))
    (copy-file source-dir target-dir)))

(defun my/write-dovecot-config ()
  (let ((config-dir  "~/.dovecot-pass"))
    (my/create-file-with-content-if-not-exist config-dir "admin:{PLAIN}")))

;; ** Write direnv config
(defun my/write-direnv-config ()
  (let ((config-target  "~/.direnvrc")
	(config-source (concat user-emacs-directory "configs/direnv/.direnvrc")))
    (copy-file config-source config-target)))

;; * Minor
;; ** Startup
;; Disable startup message
(setq inhibit-startup-message t)
(setq inhibit-startup-echo-area-message t)

;; ** Scratch buffer
;; *** Disable scratch buffer on startup
;; We need to do this because the scratch buffer created by emacs is temporary, the one in this config is a file
;; (kill-buffer "*scratch*")

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
  (lambda () (interactive)
    (setq truncate-lines (not truncate-lines))))

;; ** Visual line mode
(global-visual-line-mode 1)

;; *** Fringe indicators of wrapped line
(setq visual-line-fringe-indicators '(left-bracket nil))
;; (setq visual-line-fringe-indicators '(top-left-angle nil))
;; (setq visual-line-fringe-indicators '(empty-line nil))

;; ** Disable useless functionallity
(tooltip-mode -1)

;; ** 1 letter prompts
;; Convert yes or no prompt to y or n prompt
(defalias 'yes-or-no-p 'y-or-n-p)

;; ** Smooth scroll
;; Scroll 1 line at a time when cursor goes outside screen
(setq scroll-conservatively 200)

;; ** Bell
;; Disable bell
(setq ring-bell-function 'ignore)

;; ** Subword (camel case movement)
(global-subword-mode 1)

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

;; ** Sudo edit
(straight-use-package 'sudo-edit)

(define-key my/leader-map (kbd "C-S-s") 'sudo-edit)

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
;; (define-key my/leader-map (kbd "_") (lambda () (interactive) (text-scale-set 0)))

(define-key my/leader-map (kbd "-") (lambda () (interactive) (text-scale-decrease 1)))
(define-key my/leader-map (kbd "=") (lambda () (interactive) (text-scale-increase 1)))

(define-key my/leader-map (kbd "C--") (lambda () (interactive) (text-scale-decrease 4)))
(define-key my/leader-map (kbd "C-=") (lambda () (interactive) (text-scale-increase 4)))


(define-key my/leader-map (kbd "+") (lambda () (interactive) (text-scale-mode 0)))
(define-key my/leader-map (kbd "_") (lambda () (interactive) (text-scale-mode 0)))

;; ** Exit emacs
(define-key my/leader-map (kbd "C-z") 'save-buffers-kill-emacs)

;; ** Help mode
(setq help-mode-map (make-sparse-keymap))

;; ** Compilation mode
(setq compilation-mode-map (make-sparse-keymap))
(setq compilation-minor-mode-map (make-sparse-keymap))
(setq compilation-shell-minor-mode-map (make-sparse-keymap))
(setq compilation-mode-tool-bar-map (make-sparse-keymap))

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
;; (setq echo-keystrokes 0.001)
(setq echo-keystrokes 0)

;; ** Configure mouse
(define-key minibuffer-inactive-mode-map [mouse-1] #'ignore)

;; *** Disable middleclick and right click
(define-key global-map [mouse-2] #'ignore)
(my/evil-universal-define-key "<mouse-2>" #'ignore)

(define-key global-map [mouse-3] #'ignore)

;; *** Disable mouse wheel acceleration
(setq mouse-wheel-progressive-speed nil)

;; ** Minibuffer-depth
;; Enable and show minibuffer recursive depth
(setq enable-recursive-minibuffers t)
(minibuffer-depth-indicate-mode 1)

;; ** Clone indirect buffer name
;; *** Clone indirect buffer this window
(defun my/clone-indirect-buffer ()
  (interactive)
  (when (not (string= major-mode 'exwm-mode))
    (clone-indirect-buffer
     (concat
      "I: "
      (buffer-name)
      )
     t)))

;; *** Clone indirect buffer other window
(defun my/clone-indirect-buffer-other-window ()
  (interactive)
  (when (not (string= major-mode 'exwm-mode))
    (clone-indirect-buffer-other-window
     (concat
      "I: "
      (buffer-name)
      )
     t
     )))

;; ** Build config
(defun my/build-config-docs ()
  (interactive)
  (my/config-visit)
  (my/outorg-export-to-org-file "~/.emacs.d/readme.org"))

;; ** Man mode
;; *** Disable keys
(setq Man-mode-map (make-sparse-keymap))

;; ** Timer
;; Set timer to only run expired repeating hooks once after sleep
(setq timer-max-repeats 1)

;; ** Auto kill buffer
(defun my/auto-kill-buffer ()
  (interactive)
  (pcase major-mode
    ('gnus-summary-mode (gnus-summary-exit))
    ('ediff-mode (call-interactively #'ediff-quit))
    (_ (kill-current-buffer))))

;; ** Copy minibuffer contents
(define-key my/leader-map (kbd "Y") (lambda ()
				      (interactive)
				      ;; (kill-buffer " *Minibuf-0*")
				      (let ((curr-buf (buffer-name)))
					(switch-to-buffer " *Minibuf-0*")
					(copy-region-as-kill (point-min) (point-max))
					(switch-to-buffer curr-buf))))

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
  (find-file (concat user-emacs-directory "scratch.org")))

;; *** A persistent scratch buffer without being a file
;; (defun my/switch-to-scratch()
;;   (interactive)
;;   (let ((scratch-buffer (get-buffer "*scratch*")))
;;     (if scratch-buffer
;;	(switch-to-buffer scratch-buffer)
;;       (switch-to-buffer "*scratch*")
;;       (when (not (file-exists-p (concat user-emacs-directory "scratch")))
;;	(write-region "" nil (concat user-emacs-directory "scratch")))
;;       (insert-file-contents (concat user-emacs-directory "scratch"))
;;       ;; This generates a new mode map and uses it. This makes it possible to modify the current mode map without modifying the org mode map.
;;       (org-mode)
;;       (use-local-map (copy-keymap org-mode-map))
;;       (local-set-key [remap my/save-and-backup-buffer] (lambda () (interactive)
;;							 ;; Using write-region instead of write-file here makes it so that the scratch buffer doesn't get assigned to a file, which means it can be used without any problems in a direnv buffer
;;							 (save-restriction
;;							   (widen)
;;							   (write-region (point-min) (point-max) (concat user-emacs-directory "scratch")))))))
;;   (run-hooks 'my/open-map-hook))

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

(define-key my/open-map (kbd "N") 'my/nixos-config-visit)

;; ** Visit notes
(defun my/nixos-notes-visit ()
  (interactive)
  (find-file "~/Notes/")
  (run-hooks 'my/open-map-hook))

(define-key my/open-map (kbd "n") 'my/nixos-notes-visit)

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

;; ** Open messages
(defun my/open-messages ()
  (interactive)
  (switch-to-buffer "*Messages*")
  (run-hooks 'my/open-map-hook))

(define-key my/open-map (kbd "m") 'my/open-messages)

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

;; ** Open mail
;; (define-key my/open-map (kbd "M") 'gnus)
(define-key my/open-map (kbd "M") 'mu4e)

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
  (eww-browse-url (my/get-search-url) t))

(when (not my/use-w3m)
  (define-key my/leader-map (kbd "b") 'my/launch-eww))

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
(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp . t)
   (gnuplot . t)))

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
;; *** Highlight whole heading line
(setq org-fontify-whole-heading-line t)

;; *** Hide emphasis markers
;; The equal signs =here= to make it bold should not be visible
(setq org-hide-emphasis-markers nil)

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
(add-hook 'org-src-mode-hook (lambda () (interactive) (setq org-src--saved-temp-window-config nil)))

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
(straight-use-package 'org-plus-contrib)
(require 'org-eldoc)
(require 'org-src)
(add-hook 'org-mode-hook #'org-eldoc-load)

;; *** Fix error
;; The function =org-src-get-lang-mode= doesn't exist, but the function =org-src--get-lang-mode= does
;;  (defun org-src-get-lang-mode (LANG)
;;    (org-src--get-lang-mode LANG))

;; ** Key
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

(define-key my/org-mode-map (kbd "d") 'org-deadline)

;; (define-key org-mode-map "\t" 'nil)

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

;; ** Outline evil text object
(evil-define-text-object evil-around-heading (count &optional beg end type)
  "Select heading"
  (let ((top nil)
	(bot nil))
    (save-restriction
      (save-excursion
	(my/auto-narrow-to-subtree)
	(setq top (point-min))
	(setq bot (point-max))))
    (evil-range top bot)))

(evil-define-text-object evil-inside-heading (count &optional beg end type)
  "Select heading"
  (let ((top nil)
	(bot nil))
    (save-restriction
      (save-excursion
	(my/auto-narrow-to-subtree)
	(beginning-of-buffer)
	(ignore-errors
	  (next-line)
	  (beginning-of-line))
	(setq top (point))
	(setq bot (point-max))))
    (evil-range top bot)))

(define-key evil-outer-text-objects-map "h" 'evil-around-heading)
(define-key evil-inner-text-objects-map "h" 'evil-inside-heading)

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

;; Clean outshine-mode-map
(setq outshine-mode-map (make-sparse-keymap))

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

;; *** Set outshine fonts to inherit from outline
(set-face-attribute 'outshine-level-1 nil :inherit 'outline-1) ;;:height my/org-level-1-height)
(set-face-attribute 'outshine-level-2 nil :inherit 'outline-2) ;;:height my/org-level-2-height)
(set-face-attribute 'outshine-level-3 nil :inherit 'outline-3) ;;:height my/org-level-3-height)
(set-face-attribute 'outshine-level-4 nil :inherit 'outline-4) ;;:height my/org-level-4-height)
(set-face-attribute 'outshine-level-5 nil :inherit 'outline-5) ;;:height my/org-level-5-height)
(set-face-attribute 'outshine-level-6 nil :inherit 'outline-6) ;;:height my/org-level-6-height)
(set-face-attribute 'outshine-level-7 nil :inherit 'outline-7) ;;:height my/org-level-7-height)
(set-face-attribute 'outshine-level-8 nil :inherit 'outline-8) ;;:height my/org-level-8-height)

;; *** Fontify whole line
;; This makes it so the whole line the heading is on has the heading background color
(setq outshine-fontify-whole-heading-line t)

;; ** Narrowing
;; (define-prefix-command 'my/narrow-map)
;; (define-key my/leader-map (kbd "n") 'my/narrow-map)

;; *** Narrow indirect
(defun my/narrow-indirect (beg end)
  (my/clone-indirect-buffer)
  (narrow-to-region beg end))

;; **** Evil operator
(evil-define-operator my/evil-narrow-indirect (beg end type)
  "Indirectly narrow the region from BEG to END."
  (interactive "<R>")
  (evil-normal-state)
  (my/narrow-indirect beg end))

(define-key evil-normal-state-map "M" 'my/evil-narrow-indirect)
(define-key evil-visual-state-map "M" 'my/evil-narrow-indirect)

;; **** Evil-goggle support
(add-to-list 'evil-goggles--commands '(my/evil-narrow-indirect :face evil-goggles-yank-face :switch evil-goggles-enable-yank :advice evil-goggles--generic-async-advice))

;; *** Narrow
;; **** Evil operator
(evil-define-operator my/evil-narrow (beg end type)
  "Indirectly narrow the region from BEG to END."
  (interactive "<R>")
  (evil-normal-state)
  (narrow-to-region beg end))

(define-key evil-normal-state-map "m" 'my/evil-narrow)
(define-key evil-visual-state-map "m" 'my/evil-narrow)

;; *** Universal narrow function
(defun my/narrow-widen ()
  (interactive)
  (if loccur-mode
      (loccur-mode -1)
    (widen)))

;; (define-key my/narrow-map (kbd "w") 'my/narrow-widen)
(define-key my/leader-map (kbd "n") 'my/narrow-widen)

;; *** Narrow to subtree
(defun my/auto-narrow-to-subtree ()
  (interactive)
  (pcase major-mode
    ('org-mode (org-narrow-to-subtree))
    (_
     ;; Fixes a bug where if cursor is at heading, the one above gets narrowed
     (next-line)

     (outline-previous-visible-heading 1)
     (outshine-narrow-to-subtree))))

;; (define-key my/narrow-map (kbd "i") 'my/auto-narrow-to-subtree)

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

(add-hook 'prog-mode-hook (lambda () (interactive)
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

;; * Completion
;; ** Ivy
(straight-use-package 'ivy)
(ivy-mode 1)

(setq ivy-use-virtual-buffers nil)

;; Make user intput selectable
(setq ivy-use-selectable-prompt t)

;; *** Fix ivy dispatching
;; So when using exwm with minibuffer at the top and trying to use the ivy menu you get an error like
;; window-resize: Cannot resize the root window of a frame
;; This fixes that, it might cause bugs though
(defun ivy-shrink-after-dispatching ()
  "Shrink the window after dispatching when action list is too large."
  nil)

;; *** Visuals
;; Ivy height
(add-hook 'exwm-init-hook (lambda () (run-with-timer 1 nil (lambda () (setq ivy-height (+ (window-height) 1))))))

;; Make counsel-yank-pop use default height
;; (delete `(counsel-yank-pop . 5) ivy-height-alist)
;; Disable set height depending on command
(add-hook 'after-init-hook (lambda ()
			     (setq ivy-height-alist nil)
			     (setq-default ivy-height-alist nil)
			     (add-to-list 'ivy-height-alist '(swiper . 10))
			     (add-to-list 'ivy-height-alist '(swiper-isearch . 10))
			     (add-to-list 'ivy-height-alist '(counsel-switch-buffer . 10))))

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

(define-prefix-command 'my/ivy-mode-map)
(evil-define-key 'normal ivy-minibuffer-map (kbd (concat my/leader-map-key " a")) 'my/ivy-mode-map)

(define-key my/ivy-mode-map (kbd "k") 'counsel-minibuffer-history)

;; Enable avy movements in ivy buffer
(evil-define-key '(motion normal) ivy-minibuffer-map (kbd "M-n") 'ivy-avy)
(evil-define-key '(motion normal) ivy-minibuffer-map (kbd "M-p") 'ivy-avy)

(evil-define-key '(motion normal) ivy-minibuffer-map (kbd "G") 'my/ivy-bot)
(evil-define-key '(motion normal) ivy-minibuffer-map (kbd "g g") 'my/ivy-top)

(evil-define-key '(motion normal) ivy-minibuffer-map (kbd "n") 'ivy-next-line)
(evil-define-key '(motion normal) ivy-minibuffer-map (kbd "p") 'ivy-previous-line)

(evil-define-key '(motion normal insert) ivy-minibuffer-map (kbd "C-n") 'ivy-next-line)
(evil-define-key '(motion normal insert) ivy-minibuffer-map (kbd "C-p") 'ivy-previous-line)

(evil-define-key '(motion normal insert) ivy-minibuffer-map (kbd "C-d") 'ivy-call)

(evil-define-key '(motion normal insert) ivy-minibuffer-map (kbd "C-c") 'ivy-dispatching-done)

(define-key ivy-minibuffer-map [remap evil-ret] 'ivy-done)
(define-key ivy-minibuffer-map [remap newline] 'ivy-done)

(evil-define-key '(motion normal insert) ivy-minibuffer-map (kbd "C-g") 'minibuffer-keyboard-quit)
(evil-define-key '(motion normal insert) minibuffer-inactive-mode-map (kbd "C-g") 'minibuffer-keyboard-quit)

(evil-define-key '(motion normal insert) ivy-minibuffer-map (kbd "C-u") 'ivy-scroll-down-command)
(evil-define-key '(motion normal insert) ivy-minibuffer-map (kbd "C-w") 'ivy-scroll-up-command)

(evil-define-key '(motion normal insert) ivy-minibuffer-map (kbd "C-o") 'ivy-occur)

;; Doesn't work with dedicated minibuffer window?
;; evil-define-key '(motion normal) ivy-minibuffer-map  (kbd "C-y") 'ivy-dispatching-done)
;; define-key 'insert ivy-minibuffer-map  (kbd "C-y") 'ivy-dispatching-done)

(evil-define-key '(motion normal insert) ivy-minibuffer-map (kbd "C-s") 'ivy-next-line-or-history)

(evil-define-key '(motion normal insert) ivy-minibuffer-map (kbd "TAB") 'ivy-partial-or-done)

(evil-define-key '(motion normal insert) ivy-minibuffer-map (kbd "RET") 'ivy-done)

(evil-define-key '(motion normal insert) ivy-minibuffer-map (kbd "C-y") 'ivy-insert-current)

;; Clear ivy input
(evil-define-key '(motion normal) ivy-minibuffer-map (kbd "D") (lambda () (interactive) (beginning-of-line-text)
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

;; *** Remove dups
;; **** Remove duplicate entries in kill ring
(advice-add #'counsel-yank-pop :before (lambda (&optional arg) (delete-dups kill-ring)))

;; **** Remove duplicate entries in yank ring
(advice-add #'counsel-mark-ring :before (lambda (&optional arg) (delete-dups mark-ring)))

;; *** Find file
;; **** Filter out .. and .
(setq counsel-find-file-ignore-regexp "^\\./$\\|^\\.\\./$")
;; In ivy you can't press ~/~ on the first entry to enter that directory because there is supposed to be ~.~ there
(setq ivy-extra-directories '("./"))

;; *** Keys
;; (define-key my/leader-map (kbd "g") 'counsel-M-x)
(global-set-key (kbd "M-c") 'counsel-M-x)

(global-set-key (kbd "M-k") 'counsel-yank-pop)

(define-key ivy-minibuffer-map (kbd "DEL") 'ivy-backward-delete-char)

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

		  ;; Finally, refresh the error list to show the most recent errors
		  (flycheck-error-list-refresh)

		  (revert-buffer t t t)
		  (split-string (buffer-string) "\n" t)))
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
(define-key swiper-map (kbd "M-j") 'swiper-query-replace)


;;  (setq swiper-use-visual-line t)

;; *** Disable visual line search
;; When using visual line mode swiper also searches every visual line, not just every line. This is really slow
(setq swiper-use-visual-line nil)
(setq swiper-use-visual-line-p (lambda (a) nil))

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

(defvar my/ivy-rich-docstring-spacing 40)

;; *** Faces
(defface my/ivy-rich-doc-face
  '((t :inherit font-lock-doc-face))
  "Face used for the doc face in ivy rich buffers")

(defface my/ivy-rich-switch-buffer-size-face
  '((t :inherit default))
  "Face used by ivy rich")

(defface my/ivy-rich-switch-buffer-path-face
  '((t :inherit default))
  "Face used by ivy rich")

(defface my/ivy-rich-switch-buffer-project-face
  '((t :inherit default))
  "Face used by ivy rich")

(defface my/ivy-rich-switch-buffer-indicator-face
  '((t :inherit default))
  "Face used by ivy rich")

(defface my/ivy-rich-switch-buffer-major-mode-face
  '((t :inherit default))
  "Face used by ivy rich")

(defface my/ivy-rich-find-file-symlink-face
  '((t :inherit default))
  "Face used by ivy rich")

;; *** Transformers
;; **** Don't shorten path
(defun my/ivy-rich-path (file)
  (buffer-file-name
   (get-buffer file)))

;; *** Set transformers list
;; (ivy-set-display-transformer 'ivy-switch-buffer 'ivy-switch-buffer)
;; (setq ivy-rich-path-style 'abbrev)

(setq ivy-rich-display-transformers-list
      `(ivy-switch-buffer
	(:columns
	 ((ivy-rich-candidate (:width 30))
	  ;;(ivy-rich-switch-buffer-size (:width 7 :face my/ivy-rich-switch-buffer-size-face))
	  (ivy-rich-switch-buffer-indicators (:width 4 :face my/ivy-rich-switch-buffer-indicator-face :align right))
	  (ivy-rich-switch-buffer-major-mode (:width 12 :face my/ivy-rich-switch-buffer-major-mode-face))
	  (ivy-rich-switch-buffer-project (:width 15 :face my/ivy-rich-switch-buffer-project-face))
	  (my/ivy-rich-path)

	  ;; These two takes a lot of memory and cpu
	  ;;(ivy-rich-switch-buffer-path (:width (lambda (x) (ivy-rich-switch-buffer-shorten-path x (ivy-rich-minibuffer-width 0.3))) :face my/ivy-rich-switch-buffer-path-face))
	  )
	 :predicate
	 (lambda (cand) (get-buffer cand)))
	counsel-find-file
	(:columns
	 ((ivy-read-file-transformer)
	  (ivy-rich-counsel-find-file-truename (:face my/ivy-rich-find-file-symlink-face))))
	counsel-M-x
	(:columns
	 ((counsel-M-x-transformer (:width ,my/ivy-rich-docstring-spacing))
	  (ivy-rich-counsel-function-docstring (:face my/ivy-rich-doc-face))))
	counsel-describe-function
	(:columns
	 ((counsel-describe-function-transformer (:width ,my/ivy-rich-docstring-spacing))
	  (ivy-rich-counsel-function-docstring (:face my/ivy-rich-doc-face))))
	counsel-describe-variable
	(:columns
	 ((counsel-describe-variable-transformer (:width ,my/ivy-rich-docstring-spacing))
	  (ivy-rich-counsel-variable-docstring (:face my/ivy-rich-doc-face))))
	counsel-recentf
	(:columns
	 ((ivy-rich-candidate (:width 0.8))
	  (ivy-rich-file-last-modified-time (:face font-lock-comment-face))))))

(ivy-rich-mode 1)

;; ** Company
(straight-use-package 'company)
(require 'company)

(setq company-idle-delay 0)
;; Sets how long before company echoes tooltips in the minibuffer. Normally company and eldocs fights eachother if this is 0. This is fixed using hooks in "Fix company and eldoc"
(setq company-echo-delay 0)

;; Don't downcase result
(setq company-dabbbrev-downcase nil)

;; Make tooltim margin minimal
(setq company-tooltip-margin 2)

;; Start searching for candidates when 2 letters has been written
(setq company-minimum-prefix-length 2)

(add-to-list 'company-transformers 'company-sort-prefer-same-case-prefix)

(setq company-show-numbers t)

;; Only show 10 candidates at a time
(setq company-tooltip-limit 10)

;; Align annotations to right side
(setq company-tooltip-align-annotations t)

;; Makes it possible to exit company without a candidate selected
(setq company-require-match nil)

;; Enable scrollbar
(setq company-tooltip-offset-display 'scrollbar) ;;'line

(global-company-mode t)

;; Remove dabbrev because it's slow in long files
(setq company-backends (delete 'company-dabbrev company-backends))

;; *** Fix company and eldoc
;; Where the company menu is up, eldoc shouldn't write to the minibuffer because company is already writing documentation there
(add-hook 'company-completion-started-hook (lambda (a) (setq-local eldoc-idle-delay 100)))
(add-hook 'company-after-completion-hook (lambda (a) (setq-local eldoc-idle-delay my/eldoc-idle-delay)))

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
	(visual-line-mode 1)
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
      (visual-line-mode 1)
      (when (consp doc-buffer)
	(setq start (cdr doc-buffer)
	      doc-buffer (car doc-buffer)))
      (setq other-window-scroll-buffer (get-buffer doc-buffer))
      (let ((win (display-buffer doc-buffer t)))
	(set-window-start win (if start start (point-min)))))))

(define-key company-active-map (kbd "C-S-h") 'my/company-show-doc-buffer-keep-open)

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
;; **** Lighter
(defun my/company--group-lighter (candidate base)
  (let ((backend (or (get-text-property 0 'company-backend candidate)
		     (cl-some (lambda (x) (and (not (keywordp x)) x))
			      company-backend))))
    (when (and backend (symbolp backend))
      (let ((name (replace-regexp-in-string "company-\\|-company" ""
					    (symbol-name backend))))
	(format "%s-%s | " base name)))))

;; ***** Set base
(setq company-lighter-base "company")

;; *** Keys
(define-key company-active-map (kbd "M-n") nil)
(define-key company-active-map (kbd "M-p") nil)
(define-key company-active-map (kbd "C-n") 'company-select-next)
(define-key company-active-map (kbd "C-p") 'company-select-previous)

(define-key company-active-map (kbd "C-u") 'company-previous-page)
(define-key company-active-map (kbd "C-w") 'company-next-page)

;; Complete on tab
(define-key company-active-map (kbd "TAB") 'company-complete-selection)

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

;; *** Disable switch buffer delay
(setq flycheck-idle-buffer-switch-delay nil)

;; *** Disable flycheck fringe
(setq flycheck-indication-mode nil)

;; *** Flycheck at cursor
;; **** Flycheck-posframe
(straight-use-package 'flycheck-posframe)
(require 'flycheck-posframe)

(with-eval-after-load 'flycheck
  (add-hook 'flycheck-mode-hook #'flycheck-posframe-mode))

(setq my/flycheck-posframe-symbol "→ ")

;; (setq flycheck-posframe-error-prefix my/flycheck-posframe-symbol)
;; (setq flycheck-posframe-info-prefix my/flycheck-posframe-symbol)
;; (setq flycheck-posframe-prefix my/flycheck-posframe-symbol)
;; (setq flycheck-posframe-warning-prefix my/flycheck-posframe-symbol)

(setq flycheck-posframe-error-prefix nil)
(setq flycheck-posframe-info-prefix nil)
(setq flycheck-posframe-prefix nil)
(setq flycheck-posframe-warning-prefix nil)

;; **** Flycheck inline
;; (straight-use-package 'flycheck-inline)

;; (require 'flycheck-inline)

;; (with-eval-after-load 'flycheck
;;   '(add-hook 'flycheck-mode-hook #'turn-on-flycheck-inline))

;; *** Flycheck-package
;; Flycheck with extra correction for elisp packages
(straight-use-package 'flycheck-package)

(with-eval-after-load 'flycheck
  (flycheck-package-setup))

;; *** Copy flycheck errors contents
(define-key my/leader-map (kbd "y") 'flycheck-copy-errors-as-kill)

;; ** Which-key
;; (straight-use-package 'which-key)

;; (which-key-mode)

;; (setq which-key-idle-delay 1)

;; ** Yasnippet
(setq yas-minor-mode-map (make-sparse-keymap))

(straight-use-package 'yasnippet)
(require 'yasnippet)

(straight-use-package 'yasnippet-snippets)

(yas-global-mode 1)

;; *** Company integration
;; https://emacs.stackexchange.com/questions/10431/get-company-to-show-suggestions-for-yasnippet-names

(defvar company-mode/enable-yas t
  "Enable yasnippet for all backends.")

(defun company-mode/backend-with-yas (backend)
  (if (or (not company-mode/enable-yas) (and (listp backend) (member 'company-yasnippet backend)))
      backend
    (append (if (consp backend) backend (list backend))
	    '(:with company-yasnippet))))

(setq company-backends (mapcar #'company-mode/backend-with-yas company-backends))

;; *** Ivy integration
(straight-use-package 'ivy-yasnippet)
;; Needed because its font isn't loaded on install, but is needed in theme
(require 'ivy-yasnippet)
(define-key my/leader-map (kbd "i") 'ivy-yasnippet)

;; *** Org-mode fix
;; https://orgmode.org/manual/Conflicts.html
;; https://emacs.stackexchange.com/questions/29758/yasnippets-and-org-mode-yas-next-field-or-maybe-expand-does-not-expand
;; Yasnippet tab-key doesn't work with org-mode. This fixes that
(defun yas-org-very-safe-expand ()
  (let ((yas-fallback-behavior 'return-nil)) (yas-expand)))
(add-hook 'org-mode-hook
	  (lambda ()
	    (add-to-list 'org-tab-first-hook 'yas-org-very-safe-expand)
	    (define-key yas-keymap [tab] 'yas-next-field)))

;; *** Keys
;; Maybe unbind yas-expand in normal mode, since you only really do it in insert mode
;; (my/evil-normal-define-key "TAB" #'yas-expand)
;; (my/evil-insert-define-key "TAB" #'yas-expand)

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
(my/evil-normal-define-key "C-S-s" 'my/loccur-isearch)

;; ** Isearch
(require 'isearch)
(define-key isearch-mode-map (kbd "C-n") 'my/isearch-repeat-forward)
(define-key isearch-mode-map (kbd "C-p") 'my/isearch-repeat-backward)

(define-key isearch-mode-map (kbd "C-w") (lambda () (interactive) () (my/isearch-repeat-times t 5)))
(define-key isearch-mode-map (kbd "C-u") (lambda () (interactive) () (my/isearch-repeat-times nil 5)))

(setq isearch-lazy-highlight t)
(setq lazy-highlight-initial-delay 0)

;; *** Stop from having to press C-n two times after pressing C-p
(defvar my/last-isearch-dir nil)

(defun my/isearch-repeat-times (forward &optional count)
  (dotimes (i count)
    (my/isearch-repeat forward)))

(defun my/isearch-repeat (forward)
  (interactive)
  (if forward
      (my/isearch-repeat-forward)
    (my/isearch-repeat-backward)))

(defun my/isearch-repeat-forward ()
  (interactive)
  (call-interactively 'isearch-repeat-forward)

  (when (string= my/last-isearch-dir 'backward)
    (call-interactively 'isearch-repeat-forward))
  (setq my/last-isearch-dir 'forward))

(defun my/isearch-repeat-backward ()
  (interactive)
  (call-interactively 'isearch-repeat-backward)

  (when (string= my/last-isearch-dir 'forward)
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

(my/evil-normal-define-key "<deletechar>" 'indentation-down-to-child)
(my/evil-visual-define-key "<deletechar>" 'indentation-down-to-child)

;; ** Marks
(setq mark-ring-max 100)

;; *** Bind counsel-mark-ring
(my/evil-universal-define-key "C-o" 'counsel-mark-ring)
(my/evil-universal-define-key "C-b" 'evil-jump-backward)
(my/evil-universal-define-key "M-b" 'evil-jump-forward)

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
(straight-use-package 'jammer)
(setq jammer-repeat-delay 0.5)
(setq jammer-repeat-window 1)
(setq jammer-type 'repeat)
(setq jammer-block-type 'blacklist)
(setq jammer-block-list '(
			  backward-delete-char
			  delete-char
			  ;;ivy-backward-delete-char
			  ;;			  ;; Backward/forward
			  ;;			  evil-backward-char evil-forward-char evil-previous-line evil-next-line previous-line next-line
			  ;;			  ;; Dired
			  ;;			  dired-next-line dired-previous-line
			  ;;			  word movements
			  ;;			  evil-forward-word evil-forward-word-begin evil-forward-word-end evil-backward-word-begin
			  ;;			  ;; WORD movements
			  ;;			  evil-forward-WORD evil-forward-WORD-begin evil-forward-WORD-end evil-backward-WORD-begin
			  ;;			  evil-backward-word-begin evil-backward-word-end
			  ))
;; (jammer-mode)

;; ** goto change
;; g-; and g-,
(straight-use-package 'goto-chg)

;; ** Change default directory
(defun my/change-default-directory ()
  (interactive)
  (let ((dir (read-file-name "Change default dir: ")))
    (if (f-dir-p dir)
	(setq default-directory dir))))

;; ** Keys
;; (my/evil-normal-define-key "M-f" 'avy-goto-char-in-line)
;; (define-key my/leader-map (kbd "f") 'avy-goto-char-in-line)

;; (my/evil-normal-define-key "M-w" 'my/avy-goto-word-0-in-line)
;; (define-key my/leader-map (kbd "w") 'my/avy-goto-word-0-in-line)

;; (my/evil-normal-define-key "M-g" 'avy-goto-char-2)
;; (define-key my/leader-map (kbd "g") 'avy-goto-char-2)

;; (my/evil-normal-define-key "M-g" 'avy-goto-char-2)
;; (define-key my/leader-map (kbd "g") 'avy-goto-char-2)

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
;; ** Window split functions
(defun my/window-split-up ()
  (interactive)
  (split-window nil nil 'above))

(defun my/window-split-down ()
  (interactive)
  (split-window nil nil 'below))

(defun my/window-split-left ()
  (interactive)
  (split-window nil nil 'left))

(defun my/window-split-right ()
  (interactive)
  (split-window nil nil 'right))

;; ** Window config manager
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
  (setq my/selected-window-config (my/select-window-config "Add window config: "))

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
	    (select-window (previous-window))
	  (select-window (active-minibuffer-window))))
    (error "Minibuffer is not active")))

(define-key my/keys-mode-map (kbd "C-j") 'my/toggle-switch-to-minibuffer)
(my/evil-universal-define-key "C-j" 'my/toggle-switch-to-minibuffer)

;; ** Window and buffer management hydra
(defhydra my/window-hydra (:hint nil
				 :color red
				 :pre
				 (progn
				   (setq hydra-hint-display-type 'message)
				   (setq my/window-hydra/hint
					 (concat "next: "
						 (let ((list (ivy--buffer-list "")))
						   (if (and (string= (car list) (buffer-name))
							    ;; If there is only 1 buffer in emacs
							    (> (length list) 1))
						       (substring-no-properties
							(nth 1 list))
						     (substring-no-properties
						      (car list))))))))
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
  ("$" my/switch-monitor-right nil)
  ;; Switch monitor left
  ("0" my/switch-monitor-left nil)

  ;; Resize window
  ;; Resize up
  ("C-p" (evil-window-increase-height 10) nil)
  ;; Resize down
  ("C-n" (evil-window-decrease-height 10) nil)
  ;; Resize right
  ;;("<delete>" (evil-window-decrease-width 10) nil)
  ;; ("<deletechar>" (evil-window-decrease-width 10) nil)

  ("<deletechar>" (evil-window-decrease-width 10) nil)
  ("DEL" (evil-window-decrease-width 10) nil)
  ;; Resize left
  ;;("\b" (evil-window-increase-width 10) nil)
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
  ("o" my/window-split-right nil)
  ("O" my/window-split-left nil)
  ("v" my/window-split-down nil)
  ("V" my/window-split-up nil)

  ;; ("i" my/clone-indirect-buffer-other-window nil)
  ("I" my/clone-indirect-buffer nil)

  ;; Search
  ("C-s" swiper-all nil)

  ;; Close window
  ("s" delete-window nil)
  ;; Focus on window
  ("d" delete-other-windows nil)

  ;; minimize window
  ("S" (lambda () (interactive) (evil-window-increase-height 1000) (evil-window-increase-width 1000)) nil)
  ;; maximize window
  ("D" (lambda () (interactive) (evil-window-decrease-height 1000) (evil-window-decrease-width 1000)) nil)

  ;; Buffer management
  ;; Find file
  ("e" counsel-find-file nil)
  ("E" my/dired-curr-dir nil)
  ("M-e" my/change-default-directory nil)

  ;; Find
  ("f" my/auto-find nil)
  ("F" my/counsel-ag nil)

  ;; Browser
  ("b" my/switch-w3m-buffer nil)
  ("B" my/browser-activate-tab nil)

  ;; Switch buffer
  ("a" counsel-switch-buffer nil)
  ("A" my/switch-to-last-buffer nil)

  ;; Same as M-e
  ("8" my/counsel-switch-buffer-ediff nil)

  ;; Kill buffer
  ("k" my/auto-kill-buffer nil)

  ;; Move around in buffer
  ("C-u" evil-scroll-up nil)
  ("C-w" evil-scroll-down nil)

  ("," counsel-linux-app nil)

  ;; Switch window configuration
  ("t" my/load-window-config nil)
  ("T" my/add-window-config nil)
  ("C-t" my/delete-window-config nil)

  (";" counsel-bookmark nil)
  (":" my/add-bookmark nil)
  ("C-;" my/delete-bookmark nil)

  ;; Projectile
  ("y" counsel-projectile-switch-to-buffer nil)
  ("Y" counsel-projectile-find-file nil)
  ("*" counsel-projectile-ag nil)
  ("C-y" counsel-projectile-switch-project nil)
  ("C-Y" projectile-kill-buffers nil)

  ("u" winner-undo nil)
  ("C-r" winner-redo nil)

  ("R" rename-buffer nil)

  ;; Add this to not auto exit insert mode after closing the hydra
  ;; ("<escape>" nil)
  )

;; *** Keys
(my/evil-universal-define-key my/mod-window-leader-key 'my/window-hydra/body)
(my/evil-universal-define-key my/window-leader-key 'my/window-hydra/body)

;; * Window and buffer settings
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

;; ** Disable cluttered major mode
;; Dired normally puts the sorting string in the major mode name, this disables that
(defun dired-sort-set-mode-line ())

;; ** Open current dir
(defun my/dired-curr-dir ()
  (interactive)
  (dired
   ;; If this file isn't temporary
   (if (buffer-file-name)
       ;; Default directory isn't updated when entering symlinks, this fixes that
       (file-name-directory (file-truename (buffer-file-name)))
     default-directory)))

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
	(let ((formatted-text (concat "Dired-async completed at: " (current-time-string) " with message: " (apply #'format text args) "\n")))
	  (message formatted-text)
	  (save-excursion
	    (switch-to-buffer "dired-async-messages")
	    (insert formatted-text)))))

;; ** rsync
(straight-use-package 'dired-rsync)

;; ** Dired atool
(straight-use-package 'dired-atool)

(dired-atool-setup)

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

;; *** Tramp compatibility
;; When dired is used over tramp sorting doesn't work
(defun my/dired-tramp-compatibility ()
  (when (file-remote-p default-directory)
    (dired-sort-other
     (progn (my/dired-du-disable-quietly) "-alh"))))

(add-hook 'dired-mode-hook 'my/dired-tramp-compatibility)

;; ** Recursive folder size
(straight-use-package 'dired-du)
(require 'dired-du)

(setq dired-du-size-format t)

;; *** Disable on new buffer
(add-hook 'dired-mode-hook 'my/dired-du-disable-quietly)

;; ** Dired-single
;; (straight-use-package 'dired-single)

;; ** Dired omit-mode
;; This hides the this directory and previous directory folders in dired (. and ..)
(require 'dired-x)

(setq dired-omit-extensions nil)
(setq dired-omit-files "^\\.$\\|^\\.\\.$")

(add-hook 'dired-mode-hook 'dired-omit-mode)

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
(evil-define-key '(normal) dired-mode-map (kbd "k") 'dired-up-directory)

(evil-define-key 'insert dired-mode-map (kbd "A") 'dired-do-find-regexp)
(evil-define-key 'insert dired-mode-map  (kbd "B") 'dired-do-byte-compile)

(evil-define-key '(normal insert) dired-mode-map (kbd "K") 'my/dired-kill-and-go-down)
(evil-define-key '(normal insert) dired-mode-map (kbd "RET") 'dired-find-file)
(evil-define-key '(normal insert) dired-mode-map(kbd "C") 'dired-do-copy)
(evil-define-key '(normal insert) dired-mode-map (kbd "D") 'dired-do-delete)
(evil-define-key '(normal insert) dired-mode-map  (kbd "j") 'dired-do-rename)
(evil-define-key '(normal insert) dired-mode-map  (kbd "J") 'dired-do-rename)
(evil-define-key '(normal insert) dired-mode-map  (kbd "!") 'dired-do-shell-command)
(evil-define-key '(normal insert) dired-mode-map  (kbd "&") 'dired-do-async-shell-command)
(evil-define-key '(normal insert) dired-mode-map  (kbd "Z") 'dired-atool-do-unpack-with-subdirectory)
(evil-define-key '(normal insert) dired-mode-map  (kbd "c") 'dired-atool-do-pack)

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
(evil-define-key '(normal insert) dired-mode-map (kbd "d") 'dired-flag-file-deletion)
(evil-define-key 'insert dired-mode-map (kbd "e") 'counsel-find-file)
;; (put 'dired-find-file :advertised-binding (kbd "\C-m"))
(evil-define-key 'insert dired-mode-map (kbd "g") 'revert-buffer)
;; (evil-define-key 'insert dired-mode-map (kbd "RET") 'dired-subtree-insert)
;; (evil-define-key 'insert dired-mode-map (kbd "i") 'dired-subtree-insert)
;; (evil-define-key 'insert dired-mode-map (kbd "k") 'dired-subtree-remove)
(evil-define-key 'insert dired-mode-map (kbd "RET") 'dired-maybe-insert-subdir)
(evil-define-key 'insert dired-mode-map (kbd "i") 'dired-maybe-insert-subdir)
(evil-define-key 'insert dired-mode-map (kbd "k") 'dired-kill-subdir)
;; Maybe bind this??
(evil-define-key 'insert dired-mode-map (kbd "l") 'dired-do-redisplay)
(evil-define-key 'normal dired-mode-map (kbd "M-m") 'dired-mark-subdir-files)
(evil-define-key '(normal insert) dired-mode-map (kbd "m") 'dired-mark)
(evil-define-key 'insert dired-mode-map (kbd "M") 'dired-toggle-marks)
(evil-define-key 'insert dired-mode-map (kbd "n") 'dired-next-line)
(evil-define-key '(normal insert) dired-mode-map (kbd "o") 'dired-find-file-other-window)
(evil-define-key '(normal insert) dired-mode-map (kbd "O") 'dired-insert-subdir)
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
(setq my/eldoc-idle-delay 0)
(setq eldoc-idle-delay my/eldoc-idle-delay)

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
;; (straight-use-package 'eldoc-box)

;; (add-hook 'eldoc-mode-hook 'eldoc-box-hover-mode)

;; eldoc-box-hover-at-point-mode needs to be enabled after eldoc-box-hover-mode is done, otherwise problems can appear
;; (defadvice eldoc-box-hover-mode (after eldoc-box-hover-mode activate) (eldoc-box-hover-at-point-mode 1))

;; *** Force remove header
;; (defun eldoc-box--display (str)
;;   "Display STR in childframe."
;;   (unless (equal str "") ; WORKAROUND lsp returns empty string from time to time
;;     (let ((doc-buffer (get-buffer-create eldoc-box--buffer)))
;;       (with-current-buffer doc-buffer
;;	(setq mode-line-format nil)
;;	(setq header-line-format nil)
;;	;; without this, clicking childframe will make doc buffer the current buffer
;;	;; and `eldoc-box--maybe-cleanup' in `eldoc-box--cleanup-timer' will clear the childframe
;;	(setq eldoc-box-hover-mode t)
;;	(erase-buffer)
;;	(insert str)
;;	(goto-char (point-min)))
;;       (eldoc-box--get-frame doc-buffer))))

;; ** Inline
;; (straight-use-package 'eldoc-overlay)

;; (global-eldoc-overlay-mode)

;; * Code
;; ** Generic
;; *** Xref
;; By default xref always prompts with certain commands
(setq xref-prompt-for-identifier nil)

;; **** Ivy-xref
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
(add-to-list 'aggressive-indent-excluded-modes 'plantuml-mode)
(add-to-list 'aggressive-indent-excluded-modes 'java-mode)
(add-to-list 'aggressive-indent-excluded-modes 'c-mode)
(add-to-list 'aggressive-indent-excluded-modes 'fsharp-mode)

;; *** Whitespace cleanup
(straight-use-package 'whitespace-cleanup-mode)

(global-whitespace-cleanup-mode)

;; *** indent guide
;; (straight-use-package 'highlight-indent-guides)

;; (add-hook 'prog-mode-hook 'highlight-indent-guides-mode)

;; (add-hook 'prog-mode-hook (lambda () (highlight-indent-guides-mode (my/highlight-indent-guide-should-enable))))

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

;; *** Auto fix suggested
(defun my/auto-fix-suggested ()
  (interactive)
  (pcase major-mode
    ('haskell-mode
     (if my/haskell-hie-enable
	 (call-interactively 'lsp-execute-code-action)
       (call-interactively 'attrap-attrap)))
    (_
     (if lsp-mode
	 (call-interactively 'lsp-execute-code-action)
       (call-interactively 'attrap-attrap)))))

(define-key my/leader-map (kbd "RET") 'my/auto-fix-suggested)

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
    ('haskell-mode
     (if my/haskell-hie-enable
	 (lsp-find-definition)
       (call-interactively 'xref-find-definitions)))
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
    (_ (call-interactively 'xref-find-references))))

(define-key my/leader-map (kbd "u") 'my/auto-find-usages)

;; *** Auto eval
(defun my/auto-eval ()
  (interactive)
  (if (string= evil-state 'visual)
      (my/auto-eval-region)
    (pcase major-mode
      ;; Silent result
      ;; ('org-mode (org-babel-execute-src-block nil nil '((:result-params . ("none")))))
      ;; ('org-mode (org-babel-execute-src-block nil nil '((:result-params . ("silent")))))
      ;; ('org-mode (org-babel-execute-src-block))
      ;; Without pretty print it formats results of "[[[ ... ]]]" as "| ... |"
      ('org-mode (org-babel-execute-src-block nil nil '((:result-params . ("pp")))))
      ('scheme-mode (geiser-eval-definition nil))
      ('clojure-mode (cider-eval-last-sexp))
      ;; ('racket-mode (geiser-eval-definition nil))
      ;; ('racket-mode (racket-eval-last-sexp))
      ('racket-mode (my/racket-send-last-sexp))
      ('plantuml-mode (plantuml-preview-region 0 (line-beginning-position) (line-end-position)))
      ('fsharp-mode (fsharp-eval-phrase))
      ('c-mode (cling-send-region (line-beginning-position) (line-end-position)))
      ('c++-mode (cling-send-region (line-beginning-position) (line-end-position)))
      ('csharp-mode (my/csharp-run-repl))
      ('haskell-mode (save-excursion (my/auto-eval-region (progn (beginning-of-line) (point)) (progn (end-of-line) (point)))))
      (_ (call-interactively 'eros-eval-last-sexp)))))

(defun my/auto-eval-region (beg end)
  (interactive)
  (pcase major-mode
    ('clojure-mode (cider-eval-region beg end))
    ('plantuml-mode (plantuml-preview-region 0 beg end))
    ('fsharp-mode (fsharp-eval-region beg end))
    ('c-mode (cling-send-region beg end))
    ('c++-mode (cling-send-region beg end))
    ('csharp-mode (my/csharp-run-repl))
    ('racket-mode (racket--send-region-to-repl beg end))
    ;; ('haskell-mode (save-selected-window (my/haskell-interactive-copy-string-to-prompt (buffer-substring-no-properties beg end)) (haskell-interactive-bring) (goto-char (point-max)) (re-search-backward (haskell-interactive-prompt-regex)) (end-of-line) (recenter) ))
    ('haskell-mode (save-excursion (my/haskell-interactive-mode-run-expr (buffer-substring-no-properties beg end))))

    ;; (haskell-interactive-mode-prompt-previous)
    (_
     ;; eval-region doesn't return anything, just prints to the minibuffer so eros can't be used here
     (eros--eval-overlay
      (eval-region beg end t)
      end)
     )))

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
    ('racket-mode (racket--send-region-to-repl (point-min) (point-max)))
    ('haskell-mode (progn (haskell-process-load-file) (haskell-interactive-bring) (end-of-buffer) (recenter)))
    ;; For now disable elisp evaluation
    (_ (when (not (string= (buffer-name) "config.el"))
	 (eval-buffer nil)))))

(defun my/auto-eval-print ()
  (interactive)
  (pcase major-mode
    ('org-mode (call-interactively #'org-babel-execute-src-block))
    ('scheme-mode (geiser-eval-last-sexp t))
    ('clojure-mode (cider-eval-print-last-sexp))
    ('csharp-mode (haskell-interactive-copy-to-prompt))
    (_ (eval-print-last-sexp nil))))

;; (define-key my/leader-map (kbd "e") 'my/auto-eval)
(define-key my/leader-map (kbd "e") 'my/auto-eval-buffer)
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

(define-key my/leader-map (kbd "C-D") 'my/auto-debug)
;; (define-key my/leader-map (kbd "C-D") 'my/auto-remove-debug)
;; (define-key my/leader-map (kbd "D") 'my/auto-debug-buffer)
;; (define-key my/leader-map (kbd "M-D") 'my/auto-start-debugger)

;; *** Auto compile
(defun my/auto-compile ()
  (interactive)
  (pcase major-mode
    ('emacs-lisp-mode (emacs-lisp-byte-compile))
    ('clojure-mode (cider-eval-last-sexp))
    ('plantuml-mode (plantuml-preview-buffer 0))
    (_ (recompile))))

(define-key my/leader-map (kbd "C") 'my/auto-compile)

;; *** Auto docs
(defun my/auto-docs ()
  (interactive)
  (pcase major-mode
    ('haskell-mode
     ;; (let ((browse-url-browser-function 'eww-browse-url))
     (my/ivy-hoogle))
    ('haskell-interactive-mode (my/ivy-hoogle))
    ('nix-mode (my/nixos-options-ivy))))

(define-key my/leader-map (kbd "h") help-map)
(define-key my/leader-map (kbd "H") 'my/auto-docs)

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

(add-hook 'plantuml-mode-hook 'my/plantuml-mode)

;; ** LSP
(straight-use-package 'lsp-mode)

;; Normally lsp-mode starts up flymake mode automaticall which breaks ccls
(setq lsp-prefer-flymake nil)

(setq lsp-document-highlight-delay nil)

;; *** Company LSP
(straight-use-package 'company-lsp)
(push 'company-lsp company-backends)

;; Increases performance
(setq company-lsp-cache-candidates 'auto)

;; *** LSP-ui
(straight-use-package 'lsp-ui)
;; TODO I have to load the package fully here to set the fonts later
(require 'lsp-ui)

(add-hook 'lsp-mode-hook 'lsp-ui-mode)

(setq lsp-ui-doc-enable nil
      lsp-ui-peek-enable t
      lsp-ui-sideline-enable t
      lsp-ui-imenu-enable t
      lsp-ui-flycheck-enable t)
;; (setq lsp-ui-sideline-ignore-duplicate t)

(setq lsp-ui-sideline-show-code-actions nil
      ;; Errors i think
      lsp-ui-sideline-show-diagnostics nil

      ;; someFunc :: IO ()
      lsp-ui-sideline-show-hover t

      ;; [someFunc]
      lsp-ui-sideline-show-symbol t)

(setq lsp-ui-sideline-delay 0)

;; *** Flycheck keys
(define-key lsp-ui-flycheck-list-mode-map [remap evil-ret] 'lsp-ui-flycheck-list--view)
(define-key lsp-ui-flycheck-list-mode-map [remap newline] 'lsp-ui-flycheck-list--view)

;; ** DAP
;; I have to clear the mode map before it's defined otherwise I can't unbind it
(setq-default dap-mode-map nil)

(straight-use-package 'dap-mode)
(require 'dap-mode)
(dap-mode 1)
(dap-ui-mode 1)

;; ** Elgot
;; (straight-use-package 'eglot)

;; ** Lisps
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
(add-hook 'racket-mode-hook (lambda () (setq-local flycheck-check-syntax-automatically '(save))))

(defun my/racket-send-last-sexp ()
  (save-excursion
    (goto-char (+ 1 (point)))
    (racket-send-last-sexp)))

;; **** Fix eval
;; (with-eval-after-load 'racket-mode
;;   (defun racket-eval-last-sexp ()
;;     "Eval the previous sexp asynchronously and `message' the result."
;;     (interactive)
;;     (racket--cmd/async
;;      `(eval
;;        ,(buffer-substring-no-properties (racket--repl-last-sexp-start)
;;					(+ 1(point))))
;;      (lambda (v)
;;        (message "%s" v))))

;;   (defun racket--repl-last-sexp-start ()
;;     (save-excursion
;;       (condition-case ()
;;	  (progn
;;	    (my/backward-sexp)
;;	    (if (save-match-data (looking-at "#;"))
;;		(+ (point) 2)
;;	      (point)))
;;	(scan-error (user-error "There isn't a complete s-expression before point"))))))

;; **** Keys
(setq racket-repl-mode-map (make-sparse-keymap))

;; *** Emacs-lisp
;; **** Eros
(straight-use-package 'eros)

(eros-mode 1)

;; ***** Make it work inside run with timer and such
;; Eros doesn't work inside run-with-timers because then ~this-command~ is set. In this patch I just remove the ~this-command~ check
(cl-defun eros--make-result-overlay (value &rest props &key where duration (type 'result)
					   (format (concat " " eros-eval-result-prefix "%s "))
					   (prepend-face 'eros-result-overlay-face)
					   &allow-other-keys)
  "Place an overlay displaying VALUE at the end of line.

VALUE is used as the overlay's after-string property, meaning it
is displayed at the end of the overlay.  The overlay itself is
placed from beginning to end of current line.

Return nil if the overlay was not placed or if it might not be
visible, and return the overlay otherwise.

Return the overlay if it was placed successfully, and nil if it
failed.

This function takes some optional keyword arguments:

- If WHERE is a number or a marker, apply the overlay over the
  entire line at that place (defaulting to `point').  If it is a
  cons cell, the car and cdr determine the start and end of the
  overlay.

- DURATION takes the same possible values as the
  `eros-eval-result-duration' variable.

- TYPE is passed to `eros--make-overlay' (defaults to `result').

- FORMAT is a string passed to `format'.  It should have exactly
  one %s construct (for VALUE).

All arguments beyond these (PROPS) are properties to be used on
the overlay."
  (declare (indent 1))
  (while (keywordp (car props))
    (setq props (cddr props)))
  ;; If the marker points to a dead buffer, don't do anything.
  (let ((buffer (cond
		 ((markerp where) (marker-buffer where))
		 ((markerp (car-safe where)) (marker-buffer (car where)))
		 (t (current-buffer)))))
    (with-current-buffer buffer
      (save-excursion
	(when (number-or-marker-p where)
	  (goto-char where))
	;; Make sure the overlay is actually at the end of the sexp.
	(skip-chars-backward "\r\n[:blank:]")
	(let* ((beg (if (consp where)
			(car where)
		      (save-excursion
			(backward-sexp 1)
			(point))))
	       (end (if (consp where)
			(cdr where)
		      (line-end-position)))
	       (display-string (format format value))
	       (o nil))
	  (remove-overlays beg end 'category type)
	  (funcall (if eros-overlays-use-font-lock
		       #'font-lock-prepend-text-property
		     #'put-text-property)
		   0 (length display-string)
		   'face prepend-face
		   display-string)
	  ;; If the display spans multiple lines or is very long, display it at
	  ;; the beginning of the next line.
	  (when (or (string-match "\n." display-string)
		    (> (string-width display-string)
		       (- (window-width) (current-column))))
	    (setq display-string (concat " \n" display-string)))
	  ;; Put the cursor property only once we're done manipulating the
	  ;; string, since we want it to be at the first char.
	  (put-text-property 0 1 'cursor 0 display-string)
	  (when (> (string-width display-string) (* 3 (window-width)))
	    (setq display-string
		  (concat (substring display-string 0 (* 3 (window-width)))
			  "...\nResult truncated.")))
	  ;; Create the result overlay.
	  (setq o (apply #'eros--make-overlay
			 beg end type
			 'after-string display-string
			 props))
	  (pcase duration
	    ((pred numberp) (run-at-time duration nil #'eros--delete-overlay o))
	    (`command
	     ;; (if this-command
	     (add-hook 'pre-command-hook
		       #'eros--remove-result-overlay
		       nil 'local)
	     ;; (eros--remove-result-overlay))
	     ))
	  (let ((win (get-buffer-window buffer)))
	    ;; Left edge is visible.
	    (when (and win
		       (<= (window-start win) (point))
		       ;; In 24.3 `<=' is still a binary predicate.
		       (<= (point) (window-end win))
		       ;; Right edge is visible. This is a little conservative
		       ;; if the overlay contains line breaks.
		       (or (< (+ (current-column) (string-width value))
			      (window-width win))
			   (not truncate-lines)))
	      o)))))))

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
(straight-use-package 'lsp-java)
(require 'lsp-java)

(defun my/java-mode ()
  ;; Add configurations for java dap-mode
  (require 'dap-java)

  ;; (require 'lsp-java-boot)

  (lsp)
  (lsp-lens-mode)
  ;; (lsp-java-boot-lens-mode)

  (lsp-ui-sideline-mode -1))

;; Enable java lens
(setq lsp-java-references-code-lens-enabled t)
(setq lsp-java-implementations-code-lens-enabled t)

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
;; (straight-use-package '(haskell-mode :type git :host github :repo "walseb/haskell-mode"))
(straight-use-package 'haskell-mode)

(defun my/haskell-mode ()
  (interactive)
  (setq-local evil-shift-width 2)

  ;; Fix incorrect comment formatting
  (setq-local comment-start "--")
  (setq-local comment-padding 1))

(add-hook 'haskell-mode-hook 'my/haskell-mode)

;; *** org-mode (ob) support
(require 'ob-haskell)

;; *** haskell-process
(setq haskell-process-auto-import-loaded-modules t)
;; Disable ghci error overlay
(setq haskell-process-show-overlays nil)

;; **** Start using nix-shell
;; (setq haskell-process-wrapper-function (lambda (argv) (append (list "nix-shell" "--pure" "-I" "." "--command" )
;;							 (list (mapconcat ’identity argv " ")))))

;; *** haskell-interactive-mode
;; (add-hook 'haskell-mode-hook (lambda () (add-hook 'after-save-hook 'haskell-process-load-file nil t)))

(setq haskell-interactive-mode-read-only t)
(setq haskell-interactive-popup-errors nil)

;; **** Max memory
;; (require 'haskell-customize)
;; (add-to-list 'haskell-process-args-ghci "+RTS")
;; (add-to-list 'haskell-process-args-ghci "-M1m")

;; (add-to-list 'haskell-process-args-cabal-new-repl "--ghc-option=+RTS")
;; (add-to-list 'haskell-process-args-cabal-new-repl "--ghc-option=-M1m")

;; This would work but the quotes can't be escaped cause then they are never formatted back to normal quotes again
;; (add-to-list 'haskell-process-args-cabal-new-repl "--ghc-options="+RTS -M100m"")
;; (add-to-list 'haskell-process-args-cabal-repl "--ghc-options="+RTS -M100m"")

;; *** Run expr
;; Just patch the :complete to also run eros overlay
;; Also removed the (insert "\n") cause it was causing everything that I evaled to add one newline
(defun my/haskell-interactive-mode-run-expr (expr)
  "Run the given expression."
  (let ((session (haskell-interactive-session))
	(process (haskell-interactive-process)))
    (haskell-process-queue-command
     process
     (make-haskell-command
      :state (list session process expr 0)
      :go (lambda (state)
	    ;; I also commented out these because they are just used when in repl-mode I think
	    ;; (goto-char (point-max))
	    ;; (insert "\n")
	    ;; (setq haskell-interactive-mode-result-end
	    ;;	  (point-max))
	    (haskell-process-send-string (cadr state)
					 (haskell-interactive-mode-multi-line (cl-caddr state)))
	    (haskell-process-set-evaluating (cadr state) t))
      :live (lambda (state buffer)
	      (unless (and (string-prefix-p ":q" (cl-caddr state))
			   (string-prefix-p (cl-caddr state) ":quit"))
		(let* ((cursor (cl-cadddr state))
		       (next (replace-regexp-in-string
			      haskell-process-prompt-regex
			      ""
			      (substring buffer cursor))))
		  (haskell-interactive-mode-eval-result (car state) next)
		  (setf (cl-cdddr state) (list (length buffer)))
		  nil)))
      :complete
      (lambda (state response)
	(haskell-process-set-evaluating (cadr state) nil)
	(unless (haskell-interactive-mode-trigger-compile-error state response)
	  (haskell-interactive-mode-expr-result state response)
	  (eros--eval-overlay response (my/next-line-pos))))))))

;; *** Patch multi-line
;; Turn 'prompt2' into prompt-cont. 'prompt2' might be deprecated
(with-eval-after-load 'haskell-interactive-mode
  (defun haskell-interactive-mode-multi-line (expr)
    "If a multi-line expression EXPR has been entered, then reformat it to be:

:{
do the
   multi-liner
   expr
:}"
    (if (not (string-match-p "\n" expr))
	expr
      (let ((pre (format "^%s" (regexp-quote haskell-interactive-prompt)))
	    (lines (split-string expr "\n")))
	(cl-loop for elt on (cdr lines) do
		 (setcar elt (replace-regexp-in-string pre "" (car elt))))
	;; Temporarily set prompt2 to be empty to avoid unwanted output
	(concat ":set prompt-cont \"\"\n"
		":{\n"
		(mapconcat #'identity lines "\n")
		"\n:}\n"
		(format ":set prompt-cont \"%s\"" haskell-interactive-prompt-cont))))))

;; **** Better copy to prompt
(defun my/haskell-interactive-copy-string-to-prompt (string)
  "Copy the current line to the prompt, overwriting the current prompt."
  (let ((l (substring-no-properties string)))
    ;; If it looks like the prompt is at the start of the line, chop
    ;; it off.
    (when (and (>= (length l) (length haskell-interactive-prompt))
	       (string= (substring l 0 (length haskell-interactive-prompt))
			haskell-interactive-prompt))
      (setq l (substring l (length haskell-interactive-prompt))))

    (haskell-interactive-mode-set-prompt l)))


;; **** Keys
(add-hook 'haskell-interactive-mode-hook
	  (lambda ()
	    (evil-define-key '(normal insert visual replace) haskell-interactive-mode-map (kbd "C-c") 'haskell-process-interrupt)
	    (evil-define-key '(normal insert) haskell-interactive-mode-map (kbd "C-n") 'haskell-interactive-mode-history-next)
	    (evil-define-key '(normal insert) haskell-interactive-mode-map (kbd "C-p") 'haskell-interactive-mode-history-previous)))

;; *** nix-haskell-mode
;; It's buggy for me
;; (straight-use-package 'nix-haskell-mode)
;; (require 'nix-haskell-mode)
;; (add-hook 'haskell-mode-hook 'nix-haskell-mode)

;; *** GHC flags
;; https://downloads.haskell.org/~ghc/master/users-guide/using-warnings.html?source=post_page---------------------------
;; https://medium.com/mercury-bank/enable-all-the-warnings-a0517bc081c3
(setq my/ghc-flags
      '(
	"-Weverything"
	"-Wincomplete-uni-patterns"
	"-Wincomplete-record-updates"

	;; Don't warn if prelude is implicitly imported
	;; "-Wimplicit-prelude"
	"-Wno-implicit-prelude"

	;; Dante disables this by default
	"-Wmissing-home-modules"

	"-Widentities"
	"-Wredundant-constraints"
	"-Wpartial-fields"

	;; Normally there is a warning on every non-annotated top-level function https://gitlab.haskell.org/ghc/ghc/issues/14794?source=post_page---------------------------#ticket
	"-Wno-missing-exported-signatures"

	;; Warns that you haven't defined an export list. Without an export list all functions in the file are accessible
	;; "-Wmissing-export-lists"
	"-Wno-missing-export-lists"

	;; Don't give warning when imports arent either qualified or imported using import lists
	"-Wno-missing-import-lists"

	;; "Don’t use Safe Haskell warnings"
	"-Wno-unsafe"
	;; "Don’t use Safe Haskell warnings"
	"-Wno-safe"
	;; "Warning for polymorphic local bindings; nothing wrong with those"
	"-Wno-missing-local-signatures"
	;; "Warn if the monomorphism restriction is used"
	"-Wmonomorphism-restriction"

	;; Warns if haskell has problems inlining
	"-Wall-missed-specialisations"

	"-Wcpp-undef"

	;; By default you get a warning when you don't include type signature when writing function, disable that
	"-Wno-missing-signatures"
	))

(defun my/cabal-ghc-flags-insert ()
  (interactive)
  (insert "ghc-options: ")
  (insert (mapconcat 'identity my/ghc-flags " ")))

;; *** Hoogle
;; **** Ivy
;; (straight-use-package '(ivy-hoogle :type git :host github :repo "sjsch/ivy-hoogle"))

(defvar my/ivy-hoogle-max-entries 100)

(defun my/ivy-hoogle--do-search (str)
  (let* ((args (concat
		"search "
		"-l "
		(and my/ivy-hoogle-max-entries (concat "-n " (int-to-string my/ivy-hoogle-max-entries))))))
    (message args)
    (counsel--async-command (format "hoogle %s \"%s\""
				    args
				    str))
    '("working...")))

(defun my/ivy-hoogle ()
  "Perform a hoogle search."
  (interactive)
  (ivy-read "Hoogle: "
	    #'my/ivy-hoogle--do-search
	    :dynamic-collection t
	    :preselect (symbol-at-point)
	    ;; :preselect (counsel-symbol-at-point)
	    :re-builder #'regexp-quote
	    :action (lambda (str)
		      (browse-url
		       (substring-no-properties str
						;; + 3 is to remove the "-- " from the string
						(+ (string-match "-- .*$" str) 3)
						(length str))))
	    :caller 'my/ivy-hoogle))

;; *** Disable haskell-doc
;; Haskell-doc is loaded in as the eldoc-documentation-function.
(add-hook 'haskell-mode-hook (lambda ()
			       (setq-local eldoc-documentation-function nil)))

;; *** Project management
;; **** Stack
(straight-use-package 'hasky-stack)

;; **** Cabal
;; (straight-use-package 'hasky-cabal)

;; **** Nix cabal
;; Use nix-haskell-mode for automatic project management
;; (straight-use-package 'nix-haskell-mode)

;; *** Formatting
;; (setq haskell-stylish-on-save t)
;; (setq haskell-mode-stylish-haskell-path "brittany")
(setq haskell-mode-stylish-haskell-path "ormolu")

;; *** Extension management
(straight-use-package 'hasky-extensions)

;; *** Haskell-cabal
(straight-use-package 'company-cabal)

;; *** Fix lockup
;; This fixes a lockup that sometimes happens. I think this has to do with flycheck-mode
(add-hook 'haskell-mode-hook (lambda ()
			       ;; Fixes lockups due to prettify-symbol I think
			       (setq-local syntax-propertize-function nil)))

;; *** lsp-haskell
(when my/haskell-hie-enable
  ;; (straight-use-package 'lsp-haskell)
  (straight-use-package '(lsp-haskell :type git :host github :repo "walseb/lsp-haskell"))

  (require 'lsp-haskell)

  (defun my/haskell-lsp-mode ()
    ;; haskll-doc-mode is buggy if eldoc is on
    (setq-local lsp-eldoc-enable-hover nil)

    ;; (setq-local lsp-ui-flycheck-enable nil)

    ;; lsp-haskell doesn't work with native json
    ;; (setq-local lsp-use-native-json nil)

    (lsp)

    ;; Disable flycheck because errors are generated by haskell-interactive-mode anyways
    ;; (flycheck-disable-checker 'lsp-ui)
    (setq-local flycheck-checker 'haskell-ghc)

    ;; (setq-local flycheck-checkers (remove 'lsp-ui flycheck-checkers))
    )

  (add-hook 'haskell-mode-hook 'my/haskell-lsp-mode))

;; **** Make it start in nix-shell
;; (setq lsp-haskell-process-wrapper-function (lambda (argv)
;;					     (append
;;					      (append (list "nix-shell" "-I" "." "--command" )
;;						      (list (mapconcat 'identity argv " ")))
;;					      (list (concat (lsp-haskell--get-root) "/shell.nix")))))

;; **** Hack in eldoc support
(when my/haskell-hie-enable
  (setq my/haskell-lsp-eldoc-entries '())

  ;; This function modifies what's displayed in lsp-ui-sideline. Here it is redefined so that it takes what's supposed to be displayed in the sideline, and instead sends it to an eldoc cache
  (defun lsp-ui-sideline--push-info (symbol tag bounds info bol eol)
    (when (and (= tag (lsp-ui-sideline--calculate-tag))
	       (not (lsp-ui-sideline--stop-p)))
      (let* ((info (concat (thread-first (gethash "contents" info)
			     lsp-ui-sideline--extract-info
			     lsp-ui-sideline--format-info)))
	     (current (and (>= (point) (car bounds)) (<= (point) (cdr bounds)))))
	(when (and (> (length info) 0)
		   (lsp-ui-sideline--check-duplicate symbol info))
	  (let* ((final-string (lsp-ui-sideline--make-display-string info symbol current))
		 (pos-ov (lsp-ui-sideline--find-line (length final-string) bol eol))
		 (ov (when pos-ov (make-overlay (car pos-ov) (car pos-ov)))))

	    ;; My changes:
	    (let ((final-string-formatted (substring-no-properties final-string)))
	      (add-to-list 'my/haskell-lsp-eldoc-entries final-string-formatted))

	    (when pos-ov
	      ;; (overlay-put ov 'info info)
	      ;; (overlay-put ov 'symbol symbol)
	      (overlay-put ov 'bounds bounds)
	      (overlay-put ov 'current current)
	      ;;(overlay-put ov 'after-string final-string)
	      (overlay-put ov 'window (get-buffer-window))
	      (overlay-put ov 'kind 'info)
	      (push ov lsp-ui-sideline--ovs)))))))

  (defun my/haskell-lsp-eldoc-print ()
    (interactive)
    (when my/haskell-lsp-eldoc-entries
      (let ((at-point (thing-at-point 'symbol t)))
	(when at-point
	  (let ((str (seq-find
		      (lambda (candidate)
			(let ((candidate-last-word (string-match (rx (not whitespace) (regexp "*") space eol) candidate)))
			  (if candidate-last-word
			      (progn
				(string=
				 (substring candidate candidate-last-word (- (length candidate) 1))
				 at-point))
			    nil)))
		      my/haskell-lsp-eldoc-entries)))

	    (if str
		(s-trim str)
	      nil))))))

  ;; No idea why but eldoc doesn't run the documentation function unless i press escape, this fixes that
  (add-hook 'haskell-mode-hook (lambda ()
				 (eldoc-mode -1)
				 (setq-local eldoc-documentation-function 'my/haskell-lsp-eldoc-print)

				 (add-hook 'post-command-hook
					   'eldoc-print-current-symbol-info nil t))))

;; *** Dante
(when (not my/haskell-hie-enable)
  (setq dante-tap-type-time 0)

  (straight-use-package 'dante)

  (require 'dante)
  (add-hook 'haskell-mode-hook 'dante-mode)

  (defun my/dante-mode ()
    (my/dante-fix-flycheck))

  (add-hook 'dante-mode-hook 'my/dante-mode))

;; **** Set flycheck settings
(defun my/dante-fix-flycheck ()
  ;; Settings good for both dante and the haskell repl
  (setq-local flymake-no-changes-timeout nil)
  (setq-local flymake-start-syntax-check-on-newline nil)
  (setq-local flycheck-check-syntax-automatically '(save mode-enabled idle-buffer-switch))) ;; idle-change

;; **** Add more warnings
(when (not my/haskell-hie-enable)
  (setq my/ghc-warning-parameters
	;; Dante disables this by default, so remove it
	(remove "-Wmissing-home-modules" my/ghc-flags))

  (setq dante-load-flags (append dante-load-flags my/ghc-warning-parameters)))

;; Remove duplicates if any
;; (setq dante-load-flags (remove-duplicates dante-load-flags :test 'string=))

;; **** Add hlint to dante
(when (not my/haskell-hie-enable)
  (add-hook 'haskell-mode-hook (lambda ()
				 (flycheck-add-next-checker 'haskell-dante '(warning . haskell-hlint))
				 ;; Remove dante since the haskell repl is a lot faster at detecting errors anyways
				 ;; But turns out this leads to some packages being labled hidden?
				 ;; (add-to-list 'flycheck-disabled-checkers 'haskell-dante)

				 ;; Dante runs a lot faster now? Also problem with haskell-ghc is that it doesn't care about cabal files, it just runs ghc on the current file
				 (add-to-list 'flycheck-disabled-checkers 'haskell-ghc))))

;; **** Apply GHC hints
(straight-use-package 'attrap)

;; **** Disable nix boot
(setq dante-methods '(bare-cabal))

;; **** Fix
;; Dante wants to use the cabal dist folder instead of the dist-newstyle folder which makes it crash on newstyle projects
;; Here I just changed dist to dist-newstyle
(setq dante-methods-alist
      `((styx "styx.yaml" ("styx" "repl" dante-target))
	(snack ,(lambda (d) (directory-files d t "package\\.\\(yaml\\|nix\\)")) ("snack" "ghci" dante-target))
	(new-impure-nix dante-cabal-new-nix ("nix-shell" "--run" (concat "cabal new-repl " (or dante-target (dante-package-name) "") " --builddir=dist-newstyle/dante")))
	(new-nix dante-cabal-new-nix ("nix-shell" "--pure" "--run" (concat "cabal new-repl " (or dante-target (dante-package-name) "") " --builddir=dist-newstyle/dante")))
	(nix dante-cabal-nix ("nix-shell" "--pure" "--run" (concat "cabal repl " (or dante-target "") " --builddir=dist-newstyle/dante")))
	(impure-nix dante-cabal-nix ("nix-shell" "--run" (concat "cabal repl " (or dante-target "") " --builddir=dist-newstyle/dante")))
	(new-build "cabal.project.local" ("cabal" "new-repl" (or dante-target (dante-package-name) nil) "--builddir=dist-newstyle/dante"))
	(nix-ghci ,(lambda (d) (directory-files d t "shell.nix\\|default.nix")) ("nix-shell" "--pure" "--run" "ghci"))
	(stack "stack.yaml" ("stack" "repl" dante-target))
	(mafia "mafia" ("mafia" "repl" dante-target))
	(bare-cabal ,(lambda (d) (directory-files d t "..cabal$")) ("cabal" "repl" dante-target "--builddir=dist-newstyle/dante"))
	(bare-ghci ,(lambda (_) t) ("ghci"))))

;; *** Flycheck
;; Remove flycheck stack-ghc since it freezes emacs without stack. Don't remove the standard ghc checker though, because it works fine if I don't have HIE. If I have HIE emacs should use that instead
(add-hook 'haskell-mode-hook (lambda ()
			       (add-to-list 'flycheck-disabled-checkers 'haskell-stack-ghc)))

;; ** C/CPP
;; *** LSP CCLS
(straight-use-package 'ccls)
(require 'ccls)

(setq ccls-executable "/bin/ccls")

(defun my/c-mode ()
  (lsp)
  (lsp-lens-mode)
  (push 'company-lsp company-backends)

  (setq indent-tabs-mode t)
  (setq tab-width 4)
  (setq c-basic-offset 4)
  (setq-local evil-shift-width 4))

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
    ('haskell-mode
     (if my/haskell-hie-enable
	 (lsp-describe-thing-at-point)
       (call-interactively 'dante-info)))
    (_
     (if lsp-mode
	 (lsp-describe-thing-at-point)
       (call-interactively 'describe-function)))))

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
  (if (string= evil-state 'visual)
      (my/auto-format-region)
    (pcase major-mode
      ('csharp-mode (omnisharp-code-format-entire-file))
      ('nix-mode (nix-mode-format))
      ('haskell-mode (haskell-mode-stylish-buffer))
      (_ ()))))

(defun my/auto-format-region ()
  (interactive)
  (pcase major-mode
    ('csharp-mode (omnisharp-code-format-region))
    ('emacs-lisp-mode (elisp-format-region))
    (_ ())))

(define-key my/leader-map (kbd "<") 'my/auto-format-buffer)
(define-key my/leader-map (kbd ">") 'my/auto-format-buffer)

;; ** C#
(straight-use-package 'csharp-mode)

;; csharp-maybe-insert-codedoc
(setq csharp-mode-map (make-sparse-keymap))

;; *** REPL
(defun my/csharp-run-repl()
  (interactive)
  (eshell)
  (insert "csharp")
  (eshell-send-input))

;; *** Omnisharp-emacs
(straight-use-package 'omnisharp)

;; (add-hook 'csharp-mode-hook (lambda () (push 'company-omnisharp company-backends)))

(with-eval-after-load 'company
  (add-to-list 'company-backends #'company-omnisharp))

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
  (setq evil-shift-width-local 4))

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
  ;; Fsharp has built in intellisense highlight thing at point
  (symbol-overlay-mode -1)
  ;; Visual line mode in fsharp mode is broken, makes swiper take years to start, use truncate lines mode instead
  ;;(visual-line-mode 0)
  )

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

;; ** Gnuplot
(straight-use-package 'gnuplot)

;; ** Structural editing
;; *** Lispy
(straight-use-package 'lispy)
(require 'lispy)

(defhydra my/lispy-hydra (:hint nil
				:color red
				:pre (setq hydra-hint-display-type 'message))
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
  ("p" (call-interactively #'lispy-up) nil)

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

;; *** Structured haskell mode
(straight-use-package 'shm)
(require 'shm-case-split)

(defhydra my/structured-haskell-hydra (:hint nil
					     :color red
					     :pre (setq hydra-hint-display-type 'message))
  "haskell"
  ("U" (call-interactively (lambda () (interactive) (insert "undefined"))) nil)

  ;; Also check forward/backward node
  ("l" shm/goto-parent-end nil)
  ("h" shm/goto-parent nil)
  ("RET" (call-interactively #'shm/newline-indent) nil)

  ("N" (call-interactively #'shm/raise) nil)

  ("u" (call-interactively #'undo) nil)

  ("e" (call-interactively #'my/auto-eval) nil)

  ("k" (call-interactively #'shm/yank) nil)
  ("d" (call-interactively #'shm/kill) nil)
  ("D" (call-interactively #'shm/kill-line) nil)

  ("c" (call-interactively #'shm/case-split) nil)

  ("<escape>" nil))

;; *** Keys
(my/evil-visual-define-key "z" 'my/lispy-hydra/body)
(my/evil-normal-define-key "z" 'my/lispy-hydra/body)

;; (defun my/structural-navigation-state ()
;;  (interactive)
;;  (pcase major-mode
;;    ('haskell-mode (my/structured-haskell-hydra/body))
;;    (_ (my/lispy-hydra/body))))

;; (my/evil-visual-define-key "z" 'my/structural-navigation-state)
;; (my/evil-normal-define-key "z" 'my/structural-navigation-state)

;; * Wakatime
;; ** Custom wakatime-save mechanism
(with-eval-after-load 'wakatime-mode
  ;; Undefine wakatime-bind-hooks
  (defun wakatime-bind-hooks ())

  (setq my/wakatime-idle-time (* 60 4))
  (setq my/wakatime-report-time (/ my/wakatime-idle-time 2))
  (setq my/wakatime-user-present nil)
  (run-with-idle-timer my/wakatime-idle-time t (lambda () (message (concat "User no longer present at " (current-time-string))) (setq my/wakatime-user-present nil)))
  (add-hook 'post-command-hook (lambda () (when (not my/wakatime-user-present) (message (concat "User present again at: " (current-time-string)))) (setq my/wakatime-user-present t)))
  ;; Make the first report after the idle timer has been run
  (run-with-timer my/wakatime-report-time my/wakatime-report-time
		  (lambda () (when my/wakatime-user-present
			  (message (concat "Reporting to wakatime at: " (current-time-string)))
			  (wakatime-save))))
  (message "custom wakatime save mechanism loaded"))

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

(defun my/macro-run (&optional count)
  "If COUNT is a number repeat that amount of times, otherwise if it's nil run the macro until an error is thrown."
  (let ((to-run-string (completing-read "Run macro: " my/macro-store)))
    (unwind-protect
	(progn
	  (my/macro-run-optimize)
	  (if (>= emacs-major-version 27)
	      (my/macro-run-new to-run-string count)
	    (my/macro-run-legacy to-run-string count)))
      (my/macro-run-reset))))

(defun my/macro-run-optimize ()
  (font-lock-mode -1)
  (flyspell-mode -1)
  (symbol-overlay-mode -1)
  (global-hl-line-mode -1)
  (yascroll-bar-mode -1)
  (highlight-parentheses-mode -1))

(defun my/macro-run-reset ()
  (font-lock-mode 1)
  (my/flyspell-mode-auto-select)
  (symbol-overlay-mode)
  (global-hl-line-mode 1)
  (yascroll-bar-mode 1)
  (highlight-parentheses-mode 1))

(defun my/macro-run-new (to-run-string count)
  "Emacs >= 27"
  (let ((to-run (symbol-function (intern to-run-string))))
    (if count
	(dotimes (i count)
	  (funcall to-run))
      (while (ignore-errors (funcall to-run))))))

(defun my/macro-run-legacy (to-run-string count)
  "Emacs <= 26"
  (let ((to-run (intern to-run-string)))
    (if count
	(execute-kbd-macro to-run count)
      (execute-kbd-macro to-run 0))))

(defun my/macro-modify (&optional prefix)
  (interactive "P")
  (let
      ((chosen-macro (completing-read "Modify macro: " my/macro-store)))
    ;; Just simulate the keypresses collected
    (setq unread-command-events (listify-key-sequence (concat chosen-macro "\C-a")))
    ;;            Anything other than this and nil is not accepted it seems
    (edit-kbd-macro 'execute-extended-command prefix)))

;; *** Evil operator
(evil-define-operator evil-macro-run (beg end type)
  "Run macro on BEG to END."
  (interactive "<R>")
  (evil-normal-state)
  (save-restriction
    (goto-char beg)
    (narrow-to-region beg end)
    ;; current-prefix-arg here is used because I don't know how to access the universal argument in this function
    (my/macro-run current-prefix-arg)))

;; *** Keys
(my/evil-normal-define-key "q" 'my/macro-record-toggle)
(my/evil-visual-define-key "q" 'my/macro-record-toggle)

(my/evil-normal-define-key "Q" 'evil-macro-run)
(my/evil-visual-define-key "Q" 'evil-macro-run)

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

(setq espy-password-file "~/pass/pass.org.gpg")

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

;; * Terminal
;; ** Set max lines to a lot
(setq term-buffer-maximum-size 10000)

;; ** Disable line wrapping
(add-hook 'term-mode-hook (lambda () (interactive) (visual-line-mode -1)
			    (setq truncate-lines t)))

;; *** Keys
;; (my/evil-universal-define-key-in-mode 'term-raw-map "C-," 'term-char-mode)

;; * Eshell
;;  https://github.com/howardabrams/dot-files/blob/master/emacs-eshell.org
(require 'eshell)
;; Change to temporary name before renaming. This has to be unique. If it isn't the buffer with the same name will get its major mode changed to eshell
(setq eshell-buffer-name "*eshell-temp-name*")

(setq-default eshell-status-in-mode-line nil)

(defun my/eshell ()
  (interactive)
  (eshell)
  (my/give-buffer-unique-name "*eshell*"))

;; ** Allow to delete prompt
(add-hook 'eshell-mode-hook (lambda () (setq-local inhibit-read-only t)))

;; ** History
(setq eshell-highlight-prompt t)
(setq eshell-hist-ignoredups t)
(setq eshell-history-size 10000)

;; *** Clean history
;; This cryptic code removes all duplicate lines in the eshell history file without sorting
;; The two tac here are here so that if a new command is a duplicate, the new command is kept while the old duplicate is removed
(with-eval-after-load 'em-hist
  (let ((content (shell-command-to-string (concat "tac " eshell-history-file-name " | awk '!seen[$0]++' | tac"))))
    (write-region content nil eshell-history-file-name)))

;; *** Append history
;; https://emacs.stackexchange.com/questions/18564/merge-history-from-multiple-eshells
(setq eshell-save-history-on-exit nil)

(defun my/eshell-append-history ()
  "Call `eshell-write-history' with the `append' parameter set to `t'."
  (when eshell-history-ring
    (let ((newest-cmd-ring (make-ring 1)))
      (ring-insert newest-cmd-ring (car (ring-elements eshell-history-ring)))
      (let ((eshell-history-ring newest-cmd-ring))
	(my/append-to-file eshell-history-file-name (concat (car (ring-elements eshell-history-ring)) "\n"))))))

(add-hook 'eshell-pre-command-hook #'my/eshell-append-history)

(add-hook 'eshell-mode-hook (lambda () (interactive) (setq eshell-exit-hook (remove 'eshell-write-history eshell-exit-hook))))

;; ** Prefer lisp to bash
(setq eshell-prefer-lisp-functions nil)
(setq eshell-prefer-lisp-variables nil)

;; ** Use tramp for sudo
;; (require 'em-tramp)
;; (defalias 'sudo 'eshell/sudo)

;; ** Completion
;; *** Bash-completion
;; https://github.com/szermatt/emacs-bash-completion/issues/24
;; (straight-use-package 'bash-completion)

;; (add-hook 'eshell-mode-hook (lambda () (setq-local bash-completion-nospace t)))

;; (setq eshell-default-completion-function 'eshell-bash-completion)

;; (defun eshell-bash-completion ()
;;   (while (pcomplete-here
;;	  (nth 2 (bash-completion-dynamic-complete-nocomint (save-excursion (eshell-bol) (point)) (point))))))

;; *** Fish-completion
;; Uses bash-completion if fish wasn't found
;; (straight-use-package 'fish-completion)
;; (fish-completion-mode)

;; *** Fix inserting random tabs
;; https://github.com/company-mode/company-mode/issues/409#issuecomment-434820576
;; Also disables command completions
;; (add-hook 'eshell-mode-hook
;;	  (lambda () (setq-local completion-at-point-functions (remove 'pcomplete-completions-at-point completion-at-point-functions))))

;; Commented out what's changed
(with-eval-after-load 'em-cmpl
  (defun eshell-complete-parse-arguments ()
    "Parse the command line arguments for `pcomplete-argument'."
    (when (and eshell-no-completion-during-jobs
	       (eshell-interactive-process))
      ;; (insert-and-inherit "\t")
      (throw 'pcompleted t))
    (let ((end (point-marker))
	  (begin (save-excursion (eshell-bol) (point)))
	  (posns (list t))
	  args delim)
      (when (memq this-command '(pcomplete-expand
				 pcomplete-expand-and-complete))
	(run-hook-with-args 'eshell-expand-input-functions begin end)
	(if (= begin end)
	    (end-of-line))
	(setq end (point-marker)))
      (if (setq delim
		(catch 'eshell-incomplete
		  (ignore
		   (setq args (eshell-parse-arguments begin end)))))
	  (cond ((memq (car delim) '(?\{ ?\<))
		 (setq begin (1+ (cadr delim))
		       args (eshell-parse-arguments begin end)))
		((eq (car delim) ?\()
		 (eshell-complete-lisp-symbol)
		 (throw 'pcompleted t))
		(t
		 ;; (insert-and-inherit "\t")
		 (throw 'pcompleted t))))
      (when (get-text-property (1- end) 'comment)
	;; (insert-and-inherit "\t")
	(throw 'pcompleted t))
      (let ((pos begin))
	(while (< pos end)
	  (if (get-text-property pos 'arg-begin)
	      (nconc posns (list pos)))
	  (setq pos (1+ pos))))
      (setq posns (cdr posns))
      (cl-assert (= (length args) (length posns)))
      (let ((a args)
	    (i 0)
	    l)
	(while a
	  (if (and (consp (car a))
		   (eq (caar a) 'eshell-operator))
	      (setq l i))
	  (setq a (cdr a) i (1+ i)))
	(and l
	     (setq args (nthcdr (1+ l) args)
		   posns (nthcdr (1+ l) posns))))
      (cl-assert (= (length args) (length posns)))
      (when (and args (eq (char-syntax (char-before end)) ? )
		 (not (eq (char-before (1- end)) ?\\)))
	(nconc args (list ""))
	(nconc posns (list (point))))
      (cons (mapcar
	     (function
	      (lambda (arg)
		(let ((val
		       (if (listp arg)
			   (let ((result
				  (eshell-do-eval
				   (list 'eshell-commands arg) t)))
			     (cl-assert (eq (car result) 'quote))
			     (cadr result))
			 arg)))
		  (if (numberp val)
		      (setq val (number-to-string val)))
		  (or val ""))))
	     args)
	    posns))))

;; ** Eldoc
;; *** Eshell-eldoc
;; (straight-use-package '(eshell-eldoc :type git :host github :repo "defaultxr/eshell-eldoc"))
;; (add-hook 'eshell-mode-hook 'eshell-eldoc-enable-for-buffer)

;; *** Eshell-help
(straight-use-package 'esh-help)

(require 'esh-help)
(setup-esh-help-eldoc)

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
		(if (= (user-uid) 0) " # " ;;(concat " " my/eshell-prompt-symbol " ")
		  "\n"
		  ))))

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

(add-hook 'eshell-mode-hook (lambda ()
			      (with-editor-export-editor)
			      ;; Clear echo area to remove annoying messages. A problem with this is that it also hides error messages
			      (message nil)))

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

  (evil-define-key '(normal insert visual replace) eshell-mode-map (kbd "C-c") (lambda () (interactive) (insert "") (call-interactively 'eshell-send-input)))
  ;;(evil-define-key '(normal insert visual replace) eshell-mode-map (kbd "C-x") 'eshell-interrupt-process)
  (evil-define-key '(normal insert visual replace) eshell-mode-map (kbd "C-z") 'eshell-kill-process)

  (evil-define-key '(normal insert) eshell-mode-map (kbd "C-p") 'eshell-previous-matching-input-from-input)
  (evil-define-key '(normal insert) eshell-mode-map (kbd "C-n") 'eshell-next-matching-input-from-input)

  (evil-define-key '(normal insert) eshell-mode-map (kbd "TAB") 'completion-at-point))

(add-hook 'eshell-mode-hook 'my/bind-eshell-keys)

;; * Keyboard layouts
;; ** Carpalx
(defun my/carpalx-enable ()
  (interactive)
  (async-shell-command "setxkbmap -I ~/.emacs.d/configs/kbd-layouts/ carpalx.xkb -print | xkbcomp -I/home/admin/.emacs.d/configs/kbd-layouts/ - $DISPLAY"))

(when my/carpalx-enable
  (my/carpalx-enable))

;; * Keys
;; ** Key rebinds
(require 'evil-maps)

;; *** General
(straight-use-package 'general)

(general-evil-setup)

;; *** Language specific symbols
;; **** Lower
(define-key input-decode-map (kbd "M-p") (kbd "å"))
(define-key input-decode-map (kbd "M-,") (kbd "ä"))
(define-key input-decode-map (kbd "M-.") (kbd "ö"))

;; **** Capital
(define-key input-decode-map (kbd "M-P") (kbd "Å"))
(define-key input-decode-map (kbd "M-<") (kbd "Ä"))
(define-key input-decode-map (kbd "M->") (kbd "Ö"))

;; *** Rebind backspace with C-f
(keyboard-translate ?\C-f ?\C-?)

;; 127 is backspace
;; (define-key input-decode-map (kbd "C-f") [127])
;; There are 2 unbinds here for compatibility
;; (define-key input-decode-map (kbd "<backspace>") (kbd "C-="))

;; Don't split up tabs on delete
;; (global-set-key (kbd "DEL") 'backward-delete-char)

;; *** Rebind delete with
(keyboard-translate ?\C-l 'delete)
;; (define-key input-decode-map (kbd "C-l") (kbd "<deletechar>"))
;; There are 2 unbinds here for compatibility
;; (define-key input-decode-map (kbd "<deletechar>") (kbd "C-="))
;; (define-key input-decode-map (kbd "<delete>") (kbd "C-="))

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
(my/evil-normal-define-key "s" 'my/isearch-forward-regexp)
(my/evil-normal-define-key "S" 'my/isearch-backward-regexp)

(my/evil-visual-define-key "s" 'my/isearch-forward-regexp)
(my/evil-visual-define-key "S" 'my/isearch-backward-regexp)

(defun my/isearch-repeat-forward-with-cleanup ()
  (interactive)
  (call-interactively 'isearch-repeat-forward)
  ;;(run-with-idle-timer 1 nil 'lazy-highlight-cleanup)
  (run-with-idle-timer 1 nil 'isearch-done))

(defun my/isearch-repeat-backward-with-cleanup ()
  (interactive)
  (call-interactively 'isearch-repeat-backward)
  (run-with-idle-timer 1 nil 'isearch-done))

(my/evil-normal-define-key "g n" 'my/isearch-repeat-forward-with-cleanup)
(my/evil-normal-define-key "g p" 'my/isearch-repeat-backward-with-cleanup)

(my/evil-visual-define-key "g n" 'my/isearch-repeat-forward-with-cleanup)
(my/evil-visual-define-key "g p" 'my/isearch-repeat-backward-with-cleanup)

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

(defun my/save-and-backup-buffer ()
  (interactive)
  ;; (my/backup-buffer-per-session)
  (my/backup-original-buffer)
  (general-simulate-C-x_C-s))

(define-key my/leader-map (kbd "s") 'my/save-and-backup-buffer)
(define-key my/leader-map (kbd "C-s") 'write-file)

;; *** Rebind C-d
(my/evil-normal-define-key "C-d" nil)

;; *** Rebind esc
(define-key input-decode-map (kbd "<escape>") (kbd "C-e"))
(define-key input-decode-map (kbd "C-e") (kbd "<escape>"))
;; (keyboard-translate ?\C-e ?\C-\[)

;; *** Rebind enter
;;  (define-key input-decode-map (kbd "RET") (kbd "C-a"))
;; (define-key input-decode-map (kbd "C-a") (kbd "RET"))
(keyboard-translate ?\C-a ?\C-m)

;; *** Rebind tab
;; (define-key my/keys-mode-map (kbd "C-e") 'my/simulate-esc)
;; (define-key input-decode-map (kbd "?\\t") (kbd "C-="))

;; If window system, unbind tab key and not C-=

(define-key input-decode-map (kbd "TAB") (kbd "C-="))
(define-key input-decode-map (kbd "<tab>") (kbd "C-="))
(define-key input-decode-map (kbd "C-t") (kbd "TAB"))
(define-key input-decode-map (kbd "M-C-t") (kbd "C-TAB"))
;; This doesn't work here because you can't cross-bind like above. All keys involved would point to the same output key
;; (keyboard-translate ?\C-t ?\C-i)

;; (when window-system
;;  (define-key input-decode-map (kbd "TAB") (kbd "C--"))
;;  (define-key input-decode-map (kbd "<tab>") (kbd "C--"))

;;  (define-key input-decode-map (kbd "C-i") (kbd "C-~")))

;; *** Disable backspace
;; (define-key input-decode-map (kbd "C-e") (kbd "TAB"))
;; (define-key input-decode-map (kbd "M-C-i") (kbd "C-TAB"))

;; *** Rebind number row
;; **** Numbers
;; ***** Disable number row
;; (define-key input-decode-map (kbd "C-[") [control-bracketleft])

(define-key input-decode-map (kbd "1") (kbd "C-="))
(define-key input-decode-map (kbd "2") (kbd "C-="))
(define-key input-decode-map (kbd "3") (kbd "C-="))
(define-key input-decode-map (kbd "4") (kbd "C-="))
(define-key input-decode-map (kbd "5") (kbd "C-="))
(define-key input-decode-map (kbd "6") (kbd "C-="))
(define-key input-decode-map (kbd "7") (kbd "C-="))
(define-key input-decode-map (kbd "8") (kbd "C-="))
(define-key input-decode-map (kbd "9") (kbd "C-="))
(define-key input-decode-map (kbd "0") (kbd "C-="))

;; ***** Set new number row
(define-key input-decode-map (kbd "M-d") (kbd "1"))
(define-key input-decode-map (kbd "M-s") (kbd "2"))
(define-key input-decode-map (kbd "M-t") (kbd "3"))
(define-key input-decode-map (kbd "M-n") (kbd "4"))
(define-key input-decode-map (kbd "M-r") (kbd "5"))
(define-key input-decode-map (kbd "M-i") (kbd "6"))
(define-key input-decode-map (kbd "M-a") (kbd "7"))
(define-key input-decode-map (kbd "M-e") (kbd "8"))
(define-key input-decode-map (kbd "M-o") (kbd "9"))
(define-key input-decode-map (kbd "M-h") (kbd "0"))

;; **** Symbols
;; ***** Disable symbol keys
(define-key input-decode-map (kbd "!") (kbd "C-="))
(define-key input-decode-map (kbd "@") (kbd "C-="))
(define-key input-decode-map (kbd "#") (kbd "C-="))
(define-key input-decode-map (kbd "$") (kbd "C-="))
(define-key input-decode-map (kbd "%") (kbd "C-="))
(define-key input-decode-map (kbd "^") (kbd "C-="))
(define-key input-decode-map (kbd "&") (kbd "C-="))
(define-key input-decode-map (kbd "*") (kbd "C-="))
(define-key input-decode-map (kbd "(") (kbd "C-="))
(define-key input-decode-map (kbd ")") (kbd "C-="))

;; ***** Set new keys
(define-key input-decode-map (kbd "M-q") (kbd "!"))
(define-key input-decode-map (kbd "M-g") (kbd "@"))
(define-key input-decode-map (kbd "M-m") (kbd "#"))
(define-key input-decode-map (kbd "M-l") (kbd "$"))
(define-key input-decode-map (kbd "M-w") (kbd "%"))
(define-key input-decode-map (kbd "M-;") (kbd "^"))
(define-key input-decode-map (kbd "M-b") (kbd "&"))
(define-key input-decode-map (kbd "M-y") (kbd "*"))
(define-key input-decode-map (kbd "M-f") (kbd "("))
(define-key input-decode-map (kbd "M-u") (kbd ")"))

;; * nix
;; ** Direnv
(straight-use-package 'direnv)
(direnv-mode)

;; ** Nix-mode
(straight-use-package 'nix-mode)

;; ** Nix-options
;; *** Company
(straight-use-package 'company-nixos-options)

;; Run nixos-options on a hook because otherwise it's run on every startup which means increased startup time even if you aren't going to edit nix files
(add-hook 'nix-mode-hook (lambda () (interactive)
			   (setq-local company-backends (pushnew 'company-nixos-options company-backends))))

;; *** Ivy
(defun my/nixos-options-ivy ()
  (interactive)
  (require 'nixos-options)
  (switch-to-buffer
   (nixos-options-doc-buffer
    (nixos-options-get-documentation-for-option
     (nixos-options-get-option-by-name
      (completing-read "nix-options: " nixos-options))))))

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
	([?\C-l] . [delete])
	([?\C-f] . [backspace])

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

	;; ([escape] . [escape])
	([?\C-g] . [escape])
	([?\C-e] . [escape])

	;; Firefox hard-coded open url hotkey
	;;([?\C-o] . [f6])

	;; Redo
	([?\C-r] . [?\C-y])
	;; Undo
	([?\M-u] . [?\C-z])

	;; cut/paste.
	([?\C-y] . [?\C-c])
	([?\C-k] . [?\C-v])

	([?\M-f] . [?\C-å])
	([?\M-u] . [?\C-ä])
	([?\M-b] . [?\C-ö])

	([?\M-F] . [?\C-Å])
	([?\M-U] . [?\C-Ä])
	([?\M-B] . [?\C-Ö])

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

;; ** load exwm
;; https://emacs.stackexchange.com/questions/33326/how-do-i-cut-and-paste-effectively-between-applications-while-using-exwm
(straight-use-package 'exwm)

(require 'exwm)

;; enable exwm
(exwm-enable)

;; ** exwm keys
(exwm-input-set-key (kbd my/mod-leader-map-key) 'my/leader-map)

(exwm-input-set-key (kbd "M-<tab>") 'my/toggle-switch-to-minibuffer)

(exwm-input-set-key (kbd "<escape>") 'keyboard-quit)

;; (exwm-input-set-key (kbd "C-?") (lambda () (my/exwm-fake-key 'backspace)))

(exwm-input-set-key (kbd "<tab>") 'my/window-hydra/body)
(exwm-input-set-key (kbd "C-=") 'my/window-hydra/body)

(exwm-input-set-key (kbd "M-x") 'counsel-M-x)

;; (exwm-input-set-key (kbd "DEL") (lambda () (interactive) (exwm-input--fake-key 'backspace)))
;; (exwm-input-set-key (kbd "<deletechar>") (lambda () (interactive) (exwm-input--fake-key 'delete)))

;; (exwm-input-set-key (kbd "M-w") (lambda () (interactive) (exwm-input--fake-key ?\å)))
;; (exwm-input-set-key (kbd "M-r") (lambda () (interactive) (exwm-input--fake-key ?\ä)))
;; (exwm-input-set-key (kbd "M-j") (lambda () (interactive) (exwm-input--fake-key ?\ö)))

;; ** Exwm-edit
(setq exwm-edit-bind-default-keys nil)
(straight-use-package '(exwm-edit :type git :host github :repo "walseb/exwm-edit" :branch "AllFixes"))
(require 'exwm-edit)
(global-exwm-edit-mode 1)

;; *** Remove header
(add-hook 'exwm-edit-mode-hook (lambda () (kill-local-variable 'header-line-format)))

;; *** Keys
(exwm-input-set-key (kbd "C-d") #'exwm-edit--compose)
;; (exwm-input-set-key (kbd "M-j") #'exwm-edit--compose)

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

;; ** Fix modeline in exwm buffers
(add-hook 'exwm-floating-exit-hook (lambda ()
				     (kill-local-variable 'header-line-format)))

;; ** Disable floating windows
(setq exwm-manage-force-tiling t)

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

;; * Shr
(require 'shr)

;; ** Fix background colors shr
;; Try fixing colors
;; (setq shr-color-visible-luminance-min 80)
;; (setq shr-color-visible-distance-min 5)

;; Fully disables colors
(advice-add #'shr-colorize-region :around (defun shr-no-colourise-region (&rest ignore)))

;; ** Auto-open image at point
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
(defun my/get-search-url ()
  (interactive)
  (let ((search (counsel-google)))
    ;; Don't do a google search for anything that has a dot then a letter
    ;; There are two (not whitespace) here because otherwise the * wildcard would accept strings without any char after a dot
    (if (or
	 (string-match-p (rx whitespace) search)
	 (not (string-match-p (rx (regexp "\\.") (not whitespace) (not whitespace) (regexp "*") eol) search)))
	(concat "https://www.google.com/search?q=" search)
      search)))

;; ** w3m
(straight-use-package 'w3m)
(when (and (my/is-system-package-installed 'w3m) my/use-w3m)
  (require 'w3m)
  (w3m-display-mode 'plain))

(setq w3m-use-title-buffer-name t)

(setq w3m-session-crash-recovery nil)

(setq w3m-search-word-at-point nil)

;; *** Images
;; Make images load instantly
(setq w3m-default-display-inline-images t)
(setq w3m-idle-images-show-interval 0)

;; *** Launch w3m
(defun my/w3m-get-search-url ()
  "Custom w3m search function"
  (interactive)
  (let ((search (completing-read "search: " nil)))
    ;; Don't do a google search for anything that has a dot then a letter
    ;; There are two (not whitespace) here because otherwise the * wildcard would accept strings without any char after a dot
    (if (or
	 (string-match-p (rx whitespace) search)
	 (not (string-match-p (rx (regexp "\\.") (not whitespace) (not whitespace) (regexp "*") eol) search)))
	(w3m-search "google" "test")
      (w3m search t))))

(defun my/launch-w3m ()
  (interactive)
  (my/w3m-get-search-url))

(when my/use-w3m
  (define-key my/leader-map (kbd "b") 'my/launch-w3m))

;; *** Switch w3m buffer
(defun my/switch-w3m-buffer ()
  "Switch w3m buffer"
  (interactive)
  (setq this-command #'my/switch-w3m-buffer)
  (ivy-read "Switch to buffer: " #'internal-complete-buffer
	    :keymap ivy-switch-buffer-map
	    :preselect (buffer-name (other-buffer (current-buffer)))
	    :action #'ivy--switch-buffer-action
	    :matcher #'ivy--switch-buffer-matcher
	    :caller 'ivy-switch-buffer
	    :initial-input "*w3m*"))

;; *** Keys
(evil-define-key 'normal w3m-mode-map (kbd "RET") 'w3m-view-this-url)
(evil-define-key 'normal w3m-mode-map (kbd "o") 'w3m-search)
(evil-define-key 'insert w3m-mode-map (kbd "q") 'undefined)

(evil-define-key 'normal w3m-mode-map (kbd "u") 'w3m-history)
(evil-define-key 'normal w3m-mode-map (kbd "U") 'w3m-db-history)

;; ** Eww/shr
(require 'eww)

;; *** Add URL to buffer name
(add-hook 'eww-after-render-hook (lambda () (interactive) (my/give-buffer-unique-name (concat "eww - " (plist-get eww-data :title)))))

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
(with-eval-after-load 'exwm
  (progn
    (straight-use-package '(exwm-firefox-core :type git :host github :repo "walseb/exwm-firefox-core"  :branch "new-exwm-edit-version"))
    (straight-use-package '(exwm-firefox-evil :type git :host github :repo "walseb/exwm-firefox-evil"  :branch "new-exwm-edit-version"))
    ;;    (straight-use-package 'exwm-firefox-core)
    ;;    (straight-use-package 'exwm-firefox-evil)
    (require 'exwm-firefox-evil)

    ;; Auto enable exwm-firefox-evil-mode on all firefox buffers
    (add-hook 'exwm-manage-finish-hook 'exwm-firefox-evil-activate-if-firefox)

    ;; Run firefox buffers in normal mode
    (add-hook 'exwm-firefox-evil-mode-hook 'exwm-firefox-evil-normal)))

(setq exwm-firefox-core-search-bookmarks '(("google.com")
					   ("youtube.com")
					   ("github.com")
					   ("gmail.com")))

(setq exwm-firefox-evil-link-hint-end-key nil)

;; *** Tabs
;; http://doc.rix.si/cce/cce-browsers.html

(require 'dbus)

(defun my/browser-activate-tabs-cb (dbus ivy-hash choice)
  (funcall dbus "Activate" :int32 (truncate (string-to-number (gethash choice ivy-hash)))))

(defun my/browser-activate-tab ()
  "Activate a browser tab using Ivy. Requires plasma-browser integration"
  (interactive)
  (let ((ivy-hash (make-hash-table :test 'equal))
	(dbus (apply-partially 'dbus-call-method :session
			       "org.kde.plasma.browser_integration" "/TabsRunner"
			       "org.kde.plasma.browser_integration.TabsRunner")))
    (let ((cb (-partial #'my/browser-activate-tabs-cb dbus ivy-hash))
	  (res (funcall dbus "GetTabs")))
      (mapc
       (lambda (obj)
	 (let ((id (number-to-string (car (car (alist-get "id" (car obj) nil nil #'equal)))))
	       (title (car (car (alist-get "title" (car obj) nil nil #'equal)))))
	   (puthash title id ivy-hash)))
       res)
      (ivy-read "Activate tab: " ivy-hash :action cb))))

;; *** Keys
(with-eval-after-load 'exwm
  (progn
       ;;; Normal
    (evil-define-key '(insert visual normal motion) exwm-firefox-evil-mode-map (kbd "C-d") 'exwm-edit--compose)

    (evil-define-key '(normal motion) exwm-firefox-evil-mode-map (kbd "p") 'exwm-firefox-core-up)
    (evil-define-key '(normal motion) exwm-firefox-evil-mode-map (kbd "n") 'exwm-firefox-core-down)
    (evil-define-key '(normal motion) exwm-firefox-evil-mode-map (kbd "C-p") 'exwm-firefox-core-up)
    (evil-define-key '(normal motion) exwm-firefox-evil-mode-map (kbd "C-n") 'exwm-firefox-core-down)

    (evil-define-key '(normal motion) exwm-firefox-evil-mode-map (kbd "l") 'exwm-firefox-core-right)
    (evil-define-key '(normal motion) exwm-firefox-evil-mode-map (kbd "h") 'exwm-firefox-core-left)
    (evil-define-key '(normal motion) exwm-firefox-evil-mode-map (kbd "<deletechar>") 'exwm-firefox-core-right)
    (evil-define-key '(normal motion) exwm-firefox-evil-mode-map (kbd "C-h") 'exwm-firefox-core-left)

    (evil-define-key '(normal motion) exwm-firefox-evil-mode-map (kbd "C-w") 'exwm-firefox-core-half-page-down)
    (evil-define-key '(normal motion) exwm-firefox-evil-mode-map (kbd "e") 'my/browser-activate-tab)
    (evil-define-key '(normal motion) exwm-firefox-evil-mode-map (kbd "N") 'exwm-firefox-core-tab-next)
    (evil-define-key '(normal motion) exwm-firefox-evil-mode-map (kbd "P") 'exwm-firefox-core-tab-previous)
    (evil-define-key '(normal motion) exwm-firefox-evil-mode-map (kbd "k") 'exwm-firefox-core-tab-close)
    (evil-define-key '(normal motion) exwm-firefox-evil-mode-map (kbd "g n") 'exwm-firefox-core-find-next)
    (evil-define-key '(normal motion) exwm-firefox-evil-mode-map (kbd "g p") 'exwm-firefox-core-find-previous)
    (evil-define-key '(normal motion) exwm-firefox-evil-mode-map (kbd "C-s") 'exwm-firefox-core-quick-find)
    (evil-define-key '(normal motion) exwm-firefox-evil-mode-map (kbd "C-y") 'exwm-firefox-core-copy)
    (evil-define-key '(normal motion) exwm-firefox-evil-mode-map (kbd "C-k") 'exwm-firefox-core-paste)
    (evil-define-key '(normal motion) exwm-firefox-evil-mode-map (kbd "t") 'exwm-firefox-core-tab-new)

    (evil-define-key '(normal motion) exwm-firefox-evil-mode-map (kbd "s") 'exwm-firefox-core-find)
    (evil-define-key '(normal motion) exwm-firefox-evil-mode-map (kbd "S") 'exwm-firefox-core-find)

    (evil-define-key '(normal motion) exwm-firefox-evil-mode-map (kbd "<prior>") 'exwm-firefox-core-page-up)
    (evil-define-key '(normal motion) exwm-firefox-evil-mode-map (kbd "<next>") 'exwm-firefox-core-page-down)


    (evil-define-key '(normal motion visual insert) exwm-firefox-evil-mode-map (kbd "C-n") 'exwm-firefox-core-find-next)
    (evil-define-key '(normal motion visual insert) exwm-firefox-evil-mode-map (kbd "C-p") 'exwm-firefox-core-find-previous)

    ;; Bind tab
    (evil-define-key '(normal motion visual insert) exwm-firefox-evil-mode-map (kbd "TAB") (lambda () (interactive)
											     (exwm-input--fake-key 'tab)))

    (evil-define-key '(normal motion) exwm-firefox-evil-mode-map (kbd "f") 'exwm-firefox-evil-link-hint)
    (evil-define-key '(normal motion) exwm-firefox-evil-mode-map (kbd "F") 'exwm-firefox-evil-link-hint-new-tab)

       ;;; Visual
    (evil-define-key 'visual exwm-firefox-evil-mode-map (kbd "p") 'exwm-firefox-core-up-select)
    (evil-define-key 'visual exwm-firefox-evil-mode-map (kbd "n") 'exwm-firefox-core-down-select)

    (evil-define-key 'visual exwm-firefox-evil-mode-map (kbd "C-w") 'exwm-firefox-core-half-page-down-select)

    (evil-define-key 'visual exwm-firefox-evil-mode-map (kbd "j") 'exwm-firefox-core-find-next)
    (evil-define-key 'visual exwm-firefox-evil-mode-map (kbd "J") 'exwm-firefox-core-find-previous)

    (evil-define-key 'visual exwm-firefox-evil-mode-map (kbd "C-y") 'exwm-firefox-core-copy)
    (evil-define-key 'visual exwm-firefox-evil-mode-map (kbd "C-k") 'exwm-firefox-core-paste)

       ;;; Insert
    (evil-define-key 'insert exwm-firefox-evil-mode-map (kbd "C-u") (lambda () (interactive) (exwm-firefox-evil-normal) (exwm-firefox-core-half-page-up)))
    (evil-define-key 'insert exwm-firefox-evil-mode-map (kbd "C-w") (lambda () (interactive) (exwm-firefox-evil-normal) (exwm-firefox-core-half-page-down)))
    ;;
    ;;    (evil-define-key 'insert exwm-firefox-evil-mode-map (kbd "M-f") (lambda () (interactive) (my/exwm-fake-key "å")))
    ;;    (evil-define-key 'insert exwm-firefox-evil-mode-map (kbd "M-u") (lambda () (interactive) (my/exwm-fake-key "ä")))
    ;;    (evil-define-key 'insert exwm-firefox-evil-mode-map (kbd "M-b") (lambda () (interactive) (my/exwm-fake-key "ö")))
    ;;
    ;;    (evil-define-key 'insert exwm-firefox-evil-mode-map (kbd "M-F") (lambda () (interactive) (my/exwm-fake-key "Å")))
    ;;    (evil-define-key 'insert exwm-firefox-evil-mode-map (kbd "M-U") (lambda () (interactive) (my/exwm-fake-key "Ä")))
    ;;    (evil-define-key 'insert exwm-firefox-evil-mode-map (kbd "M-B") (lambda () (interactive) (my/exwm-fake-key "Ö")))

    ;;    (evil-define-key 'insert exwm-firefox-evil-mode-map (kbd "M-,") (lambda () (interactive) (exwm-input--fake-key ?ä)))
    ;;    (evil-define-key 'insert exwm-firefox-evil-mode-map (kbd "ä") (lambda () (interactive) (exwm-input--fake-key ?ä)))

    (evil-define-key 'insert exwm-firefox-evil-mode-map (kbd "C-y") 'exwm-firefox-core-copy)
    (evil-define-key 'insert exwm-firefox-evil-mode-map (kbd "C-k") 'exwm-firefox-core-paste)
    (evil-define-key 'insert exwm-firefox-evil-mode-map (kbd "C-l") (lambda () (interactive) (exwm-input--fake-key 'delete)))
    (evil-define-key 'insert exwm-firefox-evil-mode-map (kbd "DEL") (lambda () (interactive) (exwm-input--fake-key 'backspace)))))

;; ** Next browser
;; (defun my/write-next-config ()
;; (my/create-dir-if-not-exist "~/.config")
;; (my/create-dir-if-not-exist "~/.config/next")
;; (my/create-file-with-content-if-not-exist "~/.config/next/init.lisp")

;; )

;; ** Set default browser
(if my/use-w3m
    (setq-default browse-url-browser-function 'w3m-browse-url)
  (setq-default browse-url-browser-function 'eww-browse-url))

;; * Version control
;; ** Ediff
(require 'ediff)
(setq-default ediff-forward-word-function 'forward-char)

;; Stops ediff from creating a new frame dedicated to the control panel
;; This also fixes exwm from crashing or something
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

;; *** Hydra
(defhydra hydra-ediff (:color blue
			      :hint nil
			      :pre (progn
				     (setq hydra-hint-display-type 'posframe)))
  "
^Buffers           Files           VC                     Ediff regions
----------------------------------------------------------------------
_b_uffers           _f_iles (_=_)       _r_evisions              _l_inewise
_B_uffers (3-way)   _F_iles (3-way)                          _w_ordwise
		  _c_urrent file
"
  ("b" ediff-buffers)
  ("B" ediff-buffers3)
  ("=" ediff-files)
  ("f" ediff-files)
  ("F" ediff-files3)
  ("c" ediff-current-file)
  ("r" ediff-revision)
  ("l" ediff-regions-linewise)
  ("w" ediff-regions-wordwise))

(define-key my/leader-map (kbd "D") 'hydra-ediff/body)

;; *** Counsel ediff control
;; Only shows ediff control buffers
(defun my/counsel-switch-buffer-ediff ()
  (interactive)
  (ivy-read "Switch to buffer: " 'internal-complete-buffer
	    :preselect (buffer-name (other-buffer (current-buffer)))
	    :keymap ivy-switch-buffer-map
	    :action #'ivy--switch-buffer-action
	    :matcher #'ivy--switch-buffer-matcher
	    :caller 'counsel-switch-buffer
	    :unwind #'counsel--switch-buffer-unwind
	    :update-fn 'counsel--switch-buffer-update-fn
	    :initial-input "\*Ediff  Control  Panel"))

;; *** Keys
(define-prefix-command 'my/ediff-mode-map)
(evil-define-key 'normal ediff-mode-map (kbd (concat my/leader-map-key " a")) 'my/ediff-mode-map)

;; ** Projectile
(straight-use-package 'projectile)

;; *** Set completion system
(setq projectile-completion-system 'ivy)

;; ** Counsel projectile
;; If enabled it auto enables projectile, which has high CPU usage
(straight-use-package 'counsel-projectile)

(projectile-mode 1)
(counsel-projectile-mode 1)

;; ** Compile project
;; *** Gradlew
;; The default gradlew compile command doesn't do a clean build
(projectile-register-project-type 'gradlew '("gradlew")
				  :compile "./gradlew clean build"
				  :test "./gradlew test"
				  :test-suffix "Spec")
;; *** Keys
(define-key my/leader-map (kbd "C-y") 'projectile-compile-project)

;; *** Disable mode line
(defun projectile-update-mode-line()
  ())

;; ** Diff
;; Puts + and - in the diff buffer on the fringe so that text from there can be more easily copied.
;; Only works on emacs 27
(setq diff-font-lock-prettify t)

;; ** Magit
(straight-use-package 'magit)

(setq git-commit-summary-max-length 50)

;; *** Performance
;; Disable magit in commit view, makes it possible to do large commits
(setq magit-commit-show-diff nil)

;; *** Diff
;; (require 'magit-diff)
(setq-default magit-diff-refine-hunk 'all)
;; (setq-default magit-diff-refine-ignore-whitespace nil)

;; *** Forge
(straight-use-package '(forge :type git :host github :repo "magit/forge"))

;; *** Keys
(require 'magit)
(require 'magit-mode)

(evil-define-key '(normal motion) magit-mode-map (kbd "0") 'magit-diff-default-context)
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

(setq diff-hl-side 'right)

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

(define-key my/vc-map (kbd "a") 'counsel-projectile-switch-to-buffer)
(define-key my/vc-map (kbd "A") 'counsel-projectile-switch-project)

(define-key my/vc-map (kbd "f") 'counsel-projectile-find-file)
(define-key my/vc-map (kbd "F") 'counsel-projectile-ag)

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

;; ** Music
(define-prefix-command 'my/music-map)
(define-key my/leader-map (kbd "M") 'my/music-map)

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
    (if (file-exists-p (concat default-directory "images/"))
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
;; ** mu4e
;; *** Find nixos install location
;; https://www.reddit.com/r/NixOS/comments/6duud4/adding_mu4e_to_emacs_loadpath/
(setq my/mu4epath
      (ignore-errors
	(concat
	 (f-dirname
	  (file-truename
	   (executable-find "mu")))
	 "/../share/emacs/site-lisp/mu4e")))

(when (and my/mu4epath (string-prefix-p "/nix/store/" my/mu4epath) (file-directory-p my/mu4epath))
  (add-to-list 'load-path my/mu4epath))

;; *** Settings
;; https://www.reddit.com/r/emacs/comments/bfsck6/mu4e_for_dummies/
(when my/mu4epath
  (require 'mu4e))

(setq mu4e-get-mail-command "mbsync -c ~/.mbsyncrc -a"
      ;; mu4e-html2text-command "w3m -T text/html" ;;using the default mu4e-shr2text
      mu4e-view-use-gnus t
      mu4e-view-prefer-html t
      mu4e-update-interval 300
      mu4e-headers-auto-update t
      mu4e-compose-signature-auto-include nil
      mu4e-compose-format-flowed t)

;; to view selected message in the browser, no signin, just html mail
(when my/mu4epath
  (add-to-list 'mu4e-view-actions
	       '("ViewInBrowser" . mu4e-action-view-in-browser) t))

;; enable inline images
(setq mu4e-view-show-images t)

;; don't save message to Sent Messages, IMAP takes care of this
(setq mu4e-sent-messages-behavior 'delete)

;; **** Modification
;; I want mu4e to display images in w3m and not switch to the mail buffer whenever you open a mail

;; ***** Display email in w3m duffer
(defun mu4e-view (msg)
  (mu4e~view-define-mode)
  (unless (mu4e~view-mark-as-read-maybe msg)
    (my/mu4e-w3m-display msg)))

(defun my/mu4e-w3m-display (msg)
  (when (get-buffer mu4e~view-buffer-name)
    (progn
      (switch-to-buffer mu4e~view-buffer-name)
      (kill-buffer)))
  (w3m-browse-url (concat "file://" (mu4e~write-body-to-html msg)))
  ;; (mu4e~view-mode-body)
  ;; (mu4e-view-mode)

  (rename-buffer mu4e~view-buffer-name)
  (select-window (get-buffer-window (get-buffer "*mu4e-headers*"))))

;; ***** Don't autoselect new buffers
(defun mu4e-headers-view-message ()
  (interactive)
  (unless (eq major-mode 'mu4e-headers-mode)
    (mu4e-error "Must be in mu4e-headers-mode (%S)" major-mode))
  (let* ((msg (mu4e-message-at-point))
	 (docid (or (mu4e-message-field msg :docid)
		    (mu4e-warn "No message at point")))
	 (decrypt (mu4e~decrypt-p msg))
	 (viewwin (mu4e~headers-redraw-get-view-window)))
    (unless (window-live-p viewwin)
      (mu4e-error "Cannot get a message view"))
    (let ((curr-window (selected-window)))
      (select-window viewwin)
      (switch-to-buffer (mu4e~headers-get-loading-buf))
      (mu4e~proc-view docid mu4e-view-show-images decrypt)
      ;; (select-window curr-window)
      )))

;; ***** Fix windows splitting
(defun mu4e~headers-redraw-get-view-window ()
  (if (eq mu4e-split-view 'single-window)
      (or (and (buffer-live-p (mu4e-get-view-buffer))
	       (get-buffer-window (mu4e-get-view-buffer)))
	  (selected-window))
    ;; (mu4e-hide-other-mu4e-buffers)
    (unless (buffer-live-p (mu4e-get-headers-buffer))
      (mu4e-error "No headers buffer available"))
    (switch-to-buffer (mu4e-get-headers-buffer))
    ;; kill the existing view buffer
    (when (buffer-live-p (mu4e-get-view-buffer))
      (if (get-buffer-window (mu4e-get-view-buffer))
	  (progn
	    (select-window (get-buffer-window (mu4e-get-view-buffer)))
	    (kill-buffer-and-window))
	(kill-buffer (mu4e-get-view-buffer))))
    ;; get a new view window
    (setq mu4e~headers-view-win
	  (let* ((new-win-func
		  (cond
		   ((eq mu4e-split-view 'horizontal) ;; split horizontally
		    '(split-window-vertically mu4e-headers-visible-lines))
		   ((eq mu4e-split-view 'vertical) ;; split vertically
		    '(split-window-horizontally mu4e-headers-visible-columns)))))
	    (cond ((with-demoted-errors "Unable to split window: %S"
		     (eval new-win-func)))
		  (t ;; no splitting; just use the currently selected one
		   (selected-window)))))))

;; **** Send messages
;; ***** org-mime
(straight-use-package 'org-mime)
(require 'org-mime)
(setq org-mime-library 'mml)

;; **** Dynamically setting the width of the columns so it takes up the whole width
;; from https://www.reddit.com/r/emacs/comments/bfsck6/mu4e_for_dummies/elgoumx
(add-hook 'mu4e-headers-mode-hook
	  (defun my/mu4e-change-headers ()
	    (interactive)
	    (setq mu4e-headers-fields
		  `((:human-date . 25) ;; alternatively, use :date
		    (:flags . 6)
		    (:from . 22)
		    (:thread-subject . ,(- (window-body-width) 70)) ;; alternatively, use :subject
		    (:size . 7)))))

;; ** Gnus
;; .gnus.el is written in =write config map=
;; https://github.com/gongzhitaao/GnusSolution
;; https://www.gnu.org/software/emacs/manual/html_node/gnus/Comparing-Mail-Back-Ends.html
(require 'gnus)

;; Encrypt passwords
(setq netrc-file "~/.authinfo.gpg")

(setq gnus-use-full-window nil)

;; *** Disable state
;; Gnus normally stores random state inside =~/.newsrc-dribble=, this prevents that from happening
(setq gnus-use-dribble-file nil)

;; *** Sources
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
(evil-define-key 'normal gnus-group-mode-map (kbd "o") (lambda () (interactive) (gnus-topic-select-group t)))
(evil-define-key 'normal gnus-group-mode-map (kbd "RET") 'gnus-topic-select-group)
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
  ;; This doesn't work
  ;;(my/gnus-topic-add-gmane-groups)
  )

(add-hook 'gnus-topic-mode-hook 'my/gnus-topic-mode)

;; **** Subscribe to gmane groups
(defvar my/gnus-topic-gmane-prefix "nntp+news.gmane.org:")

(setq my/gnus-gmane-subscribed-emacs `(
				       ,(concat my/gnus-topic-gmane-prefix "gmane.emacs.help")
				       ,(concat my/gnus-topic-gmane-prefix "gmane.emacs.gnus.general")
				       ,(concat my/gnus-topic-gmane-prefix "gmane.emacs.gnus.announce")
				       ,(concat my/gnus-topic-gmane-prefix "gmane.emacs.gnus.user")))

(setq my/gnus-gmane-subscribed-emacs-blogs `(
					     ,(concat my/gnus-topic-gmane-prefix "gwene.com.oremacs")
					     ,(concat my/gnus-topic-gmane-prefix "gwene.me.emacsair")))

(setq my/gnus-gmane-subscribed-fsharp `(
					,(concat my/gnus-topic-gmane-prefix "gwene.com.reddit.pay.r.fsharp")))

(setq my/gnus-gmane-subscribed-guile `(
				       ,(concat my/gnus-topic-gmane-prefix "gmane.lisp.guile.user")))

(defun my/gnus-gmane-subscribed-get ()
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
  (visual-line-mode -1)
  (setq truncate-lines t))

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

(evil-define-key 'insert gnus-summary-mode-map (kbd "n") 'gnus-summary-next-article)
(evil-define-key 'insert gnus-summary-mode-map (kbd "p") 'gnus-summary-prev-article)

(evil-define-key 'insert gnus-summary-mode-map (kbd "N") 'gnus-summary-next-unread-article)
(evil-define-key 'insert gnus-summary-mode-map (kbd "P") 'gnus-summary-prev-unread-article)

(defun my/gnus-summary-show-all-mail ()
  "Show all mail"
  (interactive)
  (gnus-summary-rescan-group 1))

(define-key 'my/gnus-summary-map (kbd "s") 'my/gnus-summary-show-all-mail)

;; *** Article mode
;; Mode for reading contents of mail
;; (defun my/gnus-article-mode ()
;; Font lock mode disables colors in html mail for whatever reason
;; (font-lock-mode -1))
;; )

;; (add-hook 'gnus-article-mode-hook 'my/gnus-article-mode)

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
;; (setq mm-text-html-renderer 'shr)
(setq mm-text-html-renderer 'w3m)
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
(evil-define-key 'normal gnus-group-mode-map (kbd (concat my/leader-map-key " a")) 'my/gnus-message-map)

;; *** Misc
;; **** Random color gnus logo
(random t) ; Randomize sequence of random numbers
(defun my/random-hex (&optional num)
  (interactive "P")
  (let (($n (if (numberp num) (abs num) 6 )))
    (format  (concat "%0" (number-to-string $n) "x" ) (random (1- (expt 16 $n))))))

(setq gnus-logo-colors (list (concat "#" (my/random-hex 6)) (concat "#" (my/random-hex 6))))

;; ** mbsync
(defvar my/sync-gnus-hook nil)
(defvar my/sync-gnus-has-begun nil)
(defconst my/mbsync-config "~/.mbsyncrc")

(defun my/sync-gnus ()
  (interactive)
  (message (concat "Syncing mail at: " (current-time-string)))
  (if (file-exists-p my/mbsync-config)
      (let ((mbsync-config my/mbsync-config))
	(async-start
	 (lambda ()
	   (shell-command (concat
			   "mbsync -a "
			   "--config "
			   mbsync-config)))
	 (lambda (result)
	   (run-hooks 'my/sync-gnus-hook))))
    (message "mbsync config not created")))

(defvar my/is-syncing nil)

(defun my/sync-gnus-begin ()
  (when (and (my/is-system-package-installed 'mbsync) (file-exists-p my/mbsync-config) (not my/is-syncing))
    (setq my/is-syncing t)
    (run-with-timer 0 300 'my/sync-gnus)))

(if my/run-mail-on-boot
    (add-hook 'exwm-init-hook 'my/sync-gnus-begin)
  ;;(my/sync-gnus-begin)
  (add-hook 'gnus-topic-mode-hook 'my/sync-gnus-begin))

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
(define-key my/leader-map (kbd "S") 'my/system-commands-map)

;; ** Suspend
(define-prefix-command 'my/system-suspend-map)
(define-key my/system-commands-map (kbd "s") 'my/system-suspend-map)

(defun my/systemd-suspend-PC()
  (interactive)
  (shell-command "systemctl suspend"))
(define-key my/system-suspend-map (kbd "C-s") 'my/systemd-suspend-PC)

;; (defun my/systemd-hibernate-PC()
;;  (interactive)
;;  (shell-command "systemctl hibernate"))
;; Never used
;; (define-key my/system-suspend-map (kbd "C-h") 'my/systemd-hibernate-PC)

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

;; *** Proced
(setq proced-tree-flag t)
(define-key my/processes-map (kbd "t") 'proced)

;; **** Disable line wrapping
(defun my/proced-mode ()
  (interactive)
  (visual-line-mode -1)
  ;; I need to delay this because of some reason
  (run-with-timer 0.000001 nil (lambda ()
				 (setq truncate-lines t))))

;; (add-hook 'proced-post-display-hook 'my/proced-mode)
(add-hook 'proced-mode-hook 'my/proced-mode)

;; **** Auto update interval
(setq proced-auto-update-interval 0.5)

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
(define-prefix-command 'my/network-map)
(define-key my/system-commands-map (kbd "n") 'my/network-map)

;; ** Network manager
;; Right now enwc seems to only be able to switch wifi networks and display network status in modeline
;; Also check out https://github.com/Kodkollektivet/emacs-nm
(straight-use-package 'enwc)
(setq enwc-default-backend 'nm)

(define-key my/network-map (kbd "e") 'enwc)

;; *** Connect to wifi networks
(defun my/nm-connect-to-wifi-network ()
  (interactive)
  (shell-command
   (concat "nmcli device wifi connect "
	   (completing-read "Select network: "
			    (progn
			      (setq enwc-scan-interactive t)
			      (map 'list
				   (lambda (net) (enwc-value-from-scan 'essid net))
				   (enwc-get-networks))))
	   " password "
	   (read-passwd "Enter password: "))))

(define-key my/network-map (kbd "c") 'my/nm-connect-to-wifi-network)

;; *** Delete wifi networks
(defun my/nm-delete-wifi-network ()
  (interactive)
  (shell-command
   (concat "nmcli con delete "
	   (completing-read "Select network: "
			    (progn
			      (setq enwc-scan-interactive t)
			      (map 'list
				   (lambda (net) (enwc-value-from-scan 'essid net))
				   (enwc-get-networks)))))))

(define-key my/network-map (kbd "d") 'my/nm-delete-wifi-network)

;; ** Tramp
;; (setq tramp-default-method "scpx")

;; *** Buffer naming
;; Tramp buffers aren't prefixed with server name by default
;; https://emacs.stackexchange.com/questions/26444/include-host-in-buffer-name-for-all-files-opened-with-tramp
;; (defun my/tramp-add-server-prefix ()
;;   "Add the name of the connection type and server to the buffer name"
;;   (let* ((file (or buffer-file-name default-directory))
;;	 (is-file-remote (file-remote-p file)))
;;     (when is-file-remote
;;       (rename-buffer file))))

;; (add-hook 'find-file-hook #'my/tramp-add-server-prefix)
;; (add-hook 'dired-mode-hook #'my/tramp-add-server-prefix)

;; *** Performance
;; https://gist.github.com/ralt/a36288cd748ce185b26237e6b85b27bb

;; ** Netstat
(defun my/net-utils-mode ()
  (interactive)
  (toggle-truncate-lines 1)
  (visual-line-mode -1))

(add-hook 'net-utils-mode-hook 'my/net-utils-mode)

;; ** Keys
(define-key my/network-map (kbd "s") 'netstat)
(define-key my/network-map (kbd "p") 'ping)
(define-key my/network-map (kbd "P") (lambda () (interactive) (ping "8.8.8.8")))
(define-key my/network-map (kbd "i") 'ifconfig)

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
;; ** Ellocate
(straight-use-package '(ellocate :type git :host github :repo "walseb/ellocate"))

;; ** Auto find
(defun my/auto-find ()
  (interactive)
  (if (projectile-project-p)
      (projectile-find-file)
    (ellocate)))

;; * Spelling
(define-prefix-command 'my/spell-map)
;; (define-key my/leader-map (kbd "S") 'my/spell-map)

;; http://aspell.net/0.50-doc/man-html/4_Customizing.html#suggestion
;; Allow 5 words to be connected without spaces. Default is 2
;; Run-together causes a performance loss while typing but bad-spellers only
(setq ispell-extra-args (list "--sug-mode=bad-spellers" "--run-together" "--run-together-limit=5"))

;; ** Flyspell
(define-key my/spell-map (kbd "d") 'ispell-change-dictionary)
(define-key my/spell-map (kbd "s") 'flyspell-mode)

;; List of major modes not to check
(setq my/flyspell-do-not-check '(
				 minibuffer-inactive-mode
				 eshell-mode
				 shell-mode
				 term-mode

				 wdired-mode
				 ))

(defun my/flyspell-mode-auto-select ()
  ;; Don't run this right when flyspell mode is on, the mode might not have changed yet. Instead wait a millisecond until the mode has been decided and then check for prog-mode
  (run-with-timer 0.5 nil (lambda ()
			    (if (derived-mode-p 'prog-mode)
				(flyspell-prog-mode)
			      ;; It has to be both writable and not a part of the do not check list for spell checking to activate
			      (when (and (not buffer-read-only) (not (member major-mode my/flyspell-do-not-check)))
				(flyspell-mode 1))))))

(define-globalized-minor-mode global-my/flyspell-mode
  flyspell-mode my/flyspell-mode-auto-select)
(global-my/flyspell-mode 1)

;; *** Personal directory
(setq ispell-personal-dictionary (concat user-emacs-directory ".aspell.en.pws"))

;; *** Clean mode map
(add-hook 'flyspell-mode-hook
	  (lambda ()
	    ;; This should remove binds like ~C-c $~ but doesn't. No idea why
	    (setq flyspell-mode-map (make-sparse-keymap))))

;; *** Flyspell-prog enable only for certain faces
;; Don't auto correct strings
(setq flyspell-prog-text-faces
      '(
	;; font-lock-string-face
	font-lock-comment-face
	font-lock-doc-face))

;; *** Flyspell-Correct
(straight-use-package 'flyspell-correct)

;; **** Key
(my/evil-insert-define-key (kbd "C-d") 'flyspell-correct-at-point)
(my/evil-normal-define-key (kbd "C-d") 'flyspell-correct-at-point)
(my/evil-visual-define-key (kbd "C-d") 'flyspell-correct-at-point)

;; *** Company
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

;; ** Langtool
(straight-use-package 'langtool)

(setq langtool-language-tool-jar
      (ignore-errors
	(concat
	 (f-dirname
	  (file-truename
	   (executable-find "languagetool")))
	 "/../share/languagetool-commandline.jar")))

(require 'langtool)

(setq langtool-autoshow-idle-delay 0)
(setq langtool-mother-tongue "en-US")

(define-key my/spell-map (kbd "l") 'langtool-check)
(define-key my/spell-map (kbd "L") 'langtool-check-done)

;; * Calc
(define-key my/leader-map (kbd "m") 'calc)

(defun my/calc-kill-current-line ()
  (interactive)
  (calc-kill-region (line-beginning-position) (line-end-position)))

(evil-define-key 'normal calc-mode-map [remap evil-delete-whole-line] 'my/calc-kill-current-line)
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
(define-key my/leader-map (kbd "A") 'artist-mode)

(define-prefix-command 'my/artist-mode-map)
(evil-define-key 'normal artist-mode-map (kbd (concat my/leader-map-key " a")) 'my/artist-mode-map)

(define-key my/artist-mode-map (kbd "o") 'my/artist-select-operation)
(define-key my/artist-mode-map (kbd "s") 'my/artist-select-settings)

;; (evil-define-key 'insert artist-mode-map (kbd "SPC") (lambda () (interactive) (insert " ")))
;; (evil-define-key 'insert artist-mode-map (kbd "SPC") 'self-insert-command)

(setq artist-mode-map (make-sparse-keymap))
(setq-default artist-mode-map (make-sparse-keymap))
;; (evil-define-key 'insert artist-mode-map (kbd "<delete>") 'picture-backward-clear-column)

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
(define-key pdf-view-mode-map [remap evil-next-line] (lambda () (interactive) (image-next-line 4)))
(define-key pdf-view-mode-map [remap evil-previous-line] (lambda () (interactive) (image-previous-line 4)))

(define-key pdf-view-mode-map [remap evil-forward-char] (lambda () (interactive) (image-forward-hscroll 8)))
(define-key pdf-view-mode-map [remap evil-backward-char] (lambda () (interactive) (image-backward-hscroll 8)))

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

(add-hook 'image-mode-hook (lambda () (display-line-numbers-mode -1)))
(add-hook 'image-mode-hook (lambda () (auto-revert-mode 1)))

;; Make animated images loop
(setq image-animate-loop t)

;; *** Open otf fonts with image mode
(add-to-list 'auto-mode-alist '("\\.otf\\'" . image-mode))

;; *** Blimp
(straight-use-package 'blimp)

(setq eimp-enable-undo t)

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

(evil-define-key 'normal image-mode-map (kbd "n") (lambda () (interactive) (image-next-line 8)))
(evil-define-key 'normal image-mode-map (kbd "p") (lambda () (interactive) (image-previous-line 8)))
(evil-define-key 'normal image-mode-map (kbd "h") (lambda () (interactive) (image-backward-hscroll 8)))
(evil-define-key 'normal image-mode-map (kbd "l") (lambda () (interactive) (image-forward-hscroll 8)))

(evil-define-key 'normal image-mode-map (kbd "G") (lambda () (interactive) (image-next-line 1000)))
(evil-define-key 'normal image-mode-map (kbd "g g") (lambda () (interactive) (image-previous-line 1000)))

(evil-define-key 'normal image-mode-map (kbd "$") (lambda () (interactive) (image-forward-hscroll 1000)))
(evil-define-key 'normal image-mode-map (kbd "0") (lambda () (interactive) (image-backward-hscroll 1000)))

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
;; Check out prettify-utils
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
(add-hook 'magit-mode-hook (lambda () (interactive) (prettify-symbols-mode -1)))

;; ** Symbols
;; Read =reference-point-alist= to understand how to merge characters and add spaces to characters

;; *** Generic
(defconst my/generic-equality-symbols
  '(
    ("==" . ?≡)
    ("/=" . ?≢)
    ("!=" . ?≠)
    (">=" . ?≥)
    ("<=" . ?≤)
    ))

(defconst my/generic-arrow-symbols
  '(
    ;; Fish here is a bit wrong but there isn't a proper double arrowed one in the utf spec that I could find
    ;; ("<=<" . ?↢)
    ;; (">=>" . ?↣)

    ("<-<" . ?↢)
    (">->" . ?↣)

    ;; (">=>" . (?\s (Br . Bl) ?\s (Br . Bl) #Xe146))

    ("-<" . ?⤙)
    (">-" . ?⤚)

    ("~>" . ?⇝)
    ("<~" . ?⇜)

    ("->" . ?→)
    ("<-" . ?←)

    ("=>" . ?⇒)
    ;; Conflicting with equality symbols
    ;; ("<=" . ?⇐)

    ("->>" . ?↠)
    ("<<-" . ?↞)

    ("|>" . ?⊳)
    ("<|" . ?⊲)

    ("<<" . ?≪)
    (">>" . ?≫)

    ("<<<" . ?⋘)
    (">>>" . ?⋙)

    ("><" . ?⋈)
    ))

(defconst my/generic-greek-symbols
  '(("lambda" . ?λ)))

(defconst my/generic-logic-symbols
  '(("&&" . ?∧)
    ("||" . ?∨)))

;; *** Org
;; Doesn't work without regexps because there isn't any space between org emphasis markers and the contents. For example this ~code~
;; (defconst my/org-hide-emphasis
;;   '(
;;     ("*" . ? )
;;     ("/" . ? )
;;     ("_" . ? )
;;     ("=" . ? )
;;     ("~" . ? )
;;     ("+" . ? )))

;; *** Comment delimiter
;; Font lock automatically handles comment highlighting through the function font-lock-fontify-syntactically-region

;; https://www.w3schools.com/charsets/ref_utf_block.asp
;; https://en.wikipedia.org/wiki/Block_Elements
;; █ comment
;; ▉ comment
;; ▊ comment
;; ▋ comment
;; ▌ comment
;; ▌ comment
;; ▍ comment
;; ▎ comment
;; ▏ comment
;; ▐ comment
;; Here we just use prettify symbols mode to hide the comment so that font-lock can highlight its background
;; Using prettify-symbol to display the comment box leaves gaps between them, using font lock doesn't
(setq my/pretty-comment-symbol ? )

;; Use font lock to
(add-hook 'prog-mode-hook (lambda ()
			    (setq-local font-lock-comment-start-skip (concat (s-trim-right comment-start) "+"))))

(defun my/prettify-comment ()
  `((,(string-trim comment-start) . ,my/pretty-comment-symbol)))

(defun my/prettify-comment-lisp ()
  `((,(concat (string-trim comment-start) (string-trim comment-start)) . ,my/pretty-comment-symbol)))

;; *** Outline headings
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
;; https://github.com/roelvandijk/base-unicode-symbols
;; https://github.com/enomsg/vim-haskellConcealPlus/blob/master/after/syntax/haskell.vim
;; http://haskell.github.io/haskell-mode/manual/latest/Unicode-support.html#Unicode-support
;; https://github.com/roelvandijk/emacs-haskell-unicode-input-method/blob/master/haskell-unicode-input-method.el
;; https://emacs.nasy.moe/#orgf407c8c
(defconst my/haskell-symbols
  '(("\\" . ?λ)
    ("()" . ?∅)
    ("!!" . ?‼)
    ("sqrt" . ?√)
    ("undefined" . ?⊥)
    ("pi" . ?π)
    ("not" . ?¬)
    ;;("::" . ?∷)
    ("exists" . ?∃)

    ;; Here we construct a custom symbol that has the spaces that are removed when replacing " . " with a single char
    (" . " . (?\s (Br . Bl) ?\s (Bc . Bc) ?\s (Br . Bl) ?\s (Bc . Bc) ?∘)) ; "○"
    ;; Doesn't work?
    ;;haskell-font-lock-dot-is-not-composition)
    ("forall" . ?∀)

    ;; ("(*)" . ?×)

    ;; Foldable
    ("elem" . ?∈)
    ("notElem" . ?∉)

    ;; List
    ("[]" . ?ε)
    ("++" . ?⧺)
    ("union" . ?∪)
    ("intersect" . ?∩)
    ("isSubsetOf" . ?⊆)
    ("isProperSubsetOf" . ?⊂)

    ("theta" . ?θ)

    ;; ("\\" . ?∖)

    ;; Monoid
    ("mempty" . ?∅)
    ("mappend" . ?⊕)

    ;; Arrows
    ;; ("***" . ?⁂)
    ;; ("|||" . ?⫴)
    ;; ("+++" . ?⧻)
    ))

;; https://en.wikipedia.org/wiki/Mathematical_operators_and_symbols_in_Unicode
(defconst my/haskell-type-symbols
  '(
    ("Bool" . ?𝔹)
    ("Real" . ?ℝ)
    ("Integer" . ?ℤ)
    ("Natural" . ?ℕ)
    ("Rational" . ?ℚ)
    ("Irrational" . ?ℙ)
    ("False" . ?𝔽)
    ("True" . ?𝕋)
    ))

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
		    my/haskell-type-symbols
		    my/generic-greek-symbols
		    my/generic-equality-symbols
		    ;; my/generic-arrow-symbols
		    my/generic-logic-symbols
		    (my/prettify-outline-heading)))
    ('fsharp-mode (append
		   (my/prettify-comment)
		   my/fsharp-symbols
		   my/generic-greek-symbols
		   my/generic-equality-symbols
		   ;; my/generic-arrow-symbols
		   (my/prettify-outline-heading)
		   ))
    ('emacs-lisp-mode (append
		       (my/prettify-comment-lisp)
		       my/elisp-symbols
		       my/generic-greek-symbols
		       my/generic-equality-symbols
		       ;; my/generic-arrow-symbols
		       (my/prettify-outline-heading-lisp)
		       ;; (my/prettify-outline-heading-lisp-classic)
		       ))
    ('lisp-interaction-mode (append
			     (my/prettify-comment-lisp)
			     my/elisp-symbols
			     my/generic-greek-symbols
			     my/generic-equality-symbols
			     ;; my/generic-arrow-symbols
			     (my/prettify-outline-heading-lisp)
			     ;; (my/prettify-outline-heading-lisp-classic)
			     ))
    (_ (append
	(my/prettify-comment)
	my/generic-greek-symbols
	my/generic-equality-symbols
	;; my/generic-arrow-symbols
	my/generic-logic-symbols
	(my/prettify-outline-heading)
	))))

(add-hook 'prog-mode-hook (lambda () (interactive)
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

(define-globalized-minor-mode global-olivetti-mode
  nil (lambda ()
	;; (when (not (my/line-longer-than olivetti-body-width))
	(pcase major-mode
	  ('minibuffer-inactive-mode)
	  ('exwm-mode)
	  ('mu4e-headers-mode)
	  ('pdf-view-mode)
	  (_ (olivetti-mode)))))

(global-olivetti-mode 1)

(define-key my/leader-map (kbd "V") 'olivetti-mode)

;; ** Fringe size
;; Used by diff-hl and flycheck
;; Fringe only on the left side
(fringe-mode '(5 . 5))

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
  (unless (or (not (eq evil-state 'normal)) (string= (symbol-overlay-get-symbol nil t) symbol-overlay-temp-symbol))
    (symbol-overlay-remove-temp)
    (when (not symbol-overlay-idle-time)
      (symbol-overlay-maybe-put-temp))))

;; ** Put lv at top
(require 'lv)
(defun lv-window ()
  "Ensure that LV window is live and return it."
  (if (window-live-p lv-wnd)
      lv-wnd
    (let ((ori (selected-window))
	  buf)
      (prog1 (setq lv-wnd
		   (select-window
		    (let ((ignore-window-parameters t))
		      (split-window
		       ;; Change is here
		       (frame-root-window) -1 'above))))
	(if (setq buf (get-buffer " *LV*"))
	    (switch-to-buffer buf)
	  (switch-to-buffer " *LV*")
	  (set-window-hscroll lv-wnd 0)
	  (setq window-size-fixed t)
	  (setq mode-line-format nil)
	  (setq header-line-format nil)
	  (setq cursor-type nil)
	  (setq display-line-numbers nil)
	  (setq display-fill-column-indicator nil)
	  (set-window-dedicated-p lv-wnd t)
	  (set-window-parameter lv-wnd 'no-other-window t))
	(select-window ori)))))

;; ** Show paren
;; Highlights matching paren under cursor

;; *** Delay
(setq show-paren-delay 0)

;; *** Set paren style
(setq show-paren-style 'paren)

(setq show-paren-highlight-openparen t)
(setq show-paren-when-point-inside-paren nil)
(setq show-paren-when-point-in-periphery t)

(show-paren-mode 1)

;; *** Show offscreen expression
;; https://with-emacs.com/posts/editing/show-matching-lines-when-parentheses-go-off-screen/
;; Not implemented yet
;; Define face to use
(defface my/show-paren-offscreen-face
  '((t :inherit highlight))
  "Face for showing function names offscreen")

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
(setq yascroll:scroll-bar '(left-fringe))

;; *** Fix for emacs 27
;; The function ~window-fringes~ returns a list of 4 results on some versions of emacs because of some reason. This fixes that
(when (>= emacs-major-version 27)
  (defun yascroll:choose-scroll-bar ()
    (when (memq window-system yascroll:enabled-window-systems)
      (cl-destructuring-bind (left-width right-width outside-margins pers)
	  (window-fringes)
	(cl-loop for scroll-bar in (yascroll:listify yascroll:scroll-bar)
		 if (or (eq scroll-bar 'text-area)
			(and (eq scroll-bar 'left-fringe)
			     (> left-width 0))
			(and (eq scroll-bar 'right-fringe)
			     (> right-width 0)))
		 return scroll-bar)))))

;; ** Hl-Todo
(straight-use-package 'hl-todo)

(global-hl-todo-mode)

;; ** Hl-anything
;; Really buggy and makes buffer switching slow
;; (straight-use-package 'hl-anything)

;; (define-globalized-minor-mode global-hl-highlight-mode
;;   hl-highlight-mode hl-highlight-mode)
;; (hl-highlight-mode)
;; (global-hl-highlight-mode 1)

;; (define-key my/leader-map (kbd "M") 'hl-highlight-thingatpt-local)

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

;; **** Racket
(setq racket-font-lock-keywords '())
(setq racket-font-lock-keywords-0 '())
(setq racket-font-lock-keywords-1 '())
(setq racket-font-lock-keywords-2 '())
(setq racket-font-lock-keywords-3 '())
(setq racket-font-lock-keywords-level-0 '())
(setq racket-font-lock-keywords-level-1 '())
(setq racket-font-lock-keywords-level-2 '())
(setq racket-font-lock-keywords-level-3 '())

;; ** Modeline
;; Make mode line appear in echo area instead of in the mode line area. This saves space and makes it so that the mode line can't be split

;; *** Calculate frame width
(defvar my/frame-width (frame-width))

(defun my/frame-width-update()
  (interactive)
  (setq my/frame-width (frame-width)))

;; *** Disable mode line
(setq mode-line-format nil)
(setq-default mode-line-format nil)

;; *** Mode line highlight face
(defface my/mode-line-highlight
  '((t :inherit highlight))
  "Face for highlighting something in mode line")

;; *** Mode line contents
;; Don't set it directly here, because the variable is needed to fix exwm
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
		 (int-to-string (count-lines (point-min) (point-max))))

		;;"%I"

		;; is narrowed
		"%n"

		;; Is loccur
		(:eval (when loccur-mode
			 " Loccur"))

		" | "

		;; Print error if any
		"%e"

		;; Print mode
		(:eval (when defining-kbd-macro
			 (concat
			  (propertize
			   "[MACRO]"
			   'face 'my/mode-line-highlight)
			  " ")))

		;; Print buffer name
		"%b > "

		;; Print mode
		"%m"

		;; Git
		(:eval
		 (if (and my/projectile-project-name my/buffer-git-branch (not (string= my/projectile-project-name "-")))
		     (concat
		      " > "
		      my/buffer-git-branch
		      "@"
		      my/projectile-project-name
		      (when my/git-changes-string
			(concat
			 "["
			 my/git-changes-string
			 "]"
			 )))))

		(" "
		 (company-candidates
		  (:eval
		   (if (consp company-backend)
		       (my/company--group-lighter (nth company-selection
						       company-candidates)
						  company-lighter-base)
		     (concat
		      "| "
		      (symbol-name company-backend)
		      )))
		  ;; Symbol when company is not in use
		  ""))
		)))

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

;; **** Allocate status line update timings
(defvar my/status-line-update-offset 8)
(defvar my/status-line-allocated-update-current 0)

(defun my/status-bar-allocate-update-time (task)
  (if (> my/status-line-allocated-update-current my/status-line-update-offset)
      (setq my/status-line-allocated-update-current 0)
    (setq my/status-line-allocated-update-current (+ my/status-line-allocated-update-current 1))
    (run-with-timer my/status-line-allocated-update-current 60 task)))
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
    (my/status-bar-allocate-update-time 'my/update-cpu-temp))

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
    (my/status-bar-allocate-update-time 'my/linux-update-network-rx-delta))

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
    (my/status-bar-allocate-update-time 'my/linux-update-network-tx-delta))

(my/linux-update-network-tx-delta)

;; **** Mail
;; ***** Gnus mail counter
(defvar my/gnus-unread-string nil)

(defvar my/gnus-mail-counter-update-hook nil)

(defun my/gnus-update-unread()
  (my/gnus-scan-unread)
  (setq my/gnus-unread-string
	(concat
	 "M:"
	 (my/gnus-get-unread-mail-count)
	 " > N:"
	 (my/gnus-get-unread-news-count)))
  (run-hooks 'my/gnus-mail-counter-update-hook))

(add-hook 'my/sync-gnus-hook 'my/gnus-update-unread)
(add-hook 'gnus-summary-exit-hook 'my/gnus-update-unread)

;; ***** mu4e mail counter
(straight-use-package 'mu4e-alert)

(defvar my/mu4e-unread-mail-count nil)

(setq mu4e-alert-modeline-formatter (lambda (count)
				      (setq my/mu4e-unread-mail-count (number-to-string count))))

;; ****** Enable
(when my/mu4epath
  (mu4e-alert-enable-mode-line-display))

;; **** Battery
;; If there is a battery, display it in the mode line
(require 'battery)

(display-battery-mode 1)
(setq battery-mode-line-format "%th - %p")

;; ***** Reload battery display mode
(defun my/battery-display-mode-reload ()
  (interactive)
  (display-battery-mode -1)
  (setq battery-status-function
	(cond ((and (eq system-type 'gnu/linux)
		    (file-readable-p "/proc/apm"))
	       #'battery-linux-proc-apm)
	      ((and (eq system-type 'gnu/linux)
		    (file-directory-p "/proc/acpi/battery"))
	       #'battery-linux-proc-acpi)
	      ((and (eq system-type 'gnu/linux)
		    (file-directory-p "/sys/class/power_supply/")
		    (directory-files "/sys/class/power_supply/" nil
				     battery-linux-sysfs-regexp))
	       #'battery-linux-sysfs)
	      ((and (eq system-type 'berkeley-unix)
		    (file-executable-p "/usr/sbin/apm"))
	       #'battery-bsd-apm)
	      ((and (eq system-type 'darwin)
		    (condition-case nil
			(with-temp-buffer
			  (and (eq (call-process "pmset" nil t nil "-g" "ps") 0)
			       (> (buffer-size) 0)))
		      (error nil)))
	       #'battery-pmset)
	      ((fboundp 'w32-battery-status)
	       #'w32-battery-status)))
  (display-battery-mode 1))

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

(my/status-bar-allocate-update-time 'my/update-time)
(run-with-timer 0 3600 'my/update-date)

;; Update date now
(my/update-time)
(my/update-date)

;; **** Git project and branch name
(require 'vc-git)

;; When projectile-mode is on, project name is updated on every keypress, here it is fixed
(defvar-local my/projectile-project-name nil)
(defvar-local my/buffer-git-branch nil)
;; Make sure every buffer is only scanned once
(defvar-local my/projectile-project-buffer-already-scanned nil)

(defun my/update-projectile-project-name()
  (interactive)
  ;; Some virtual buffers don't work, but dired-mode does
  (when (or (string= major-mode 'dired-mode) (and buffer-file-name (file-exists-p buffer-file-name) (not my/projectile-project-buffer-already-scanned)))
    (setq my/projectile-project-buffer-already-scanned t)
    (setq my/projectile-project-name (projectile-project-name))
    (setq my/buffer-git-branch (car (vc-git-branches)))))

(if (>= emacs-major-version 27)
    (add-hook 'window-state-change-hook 'my/update-projectile-project-name)
  (add-hook 'window-configuration-change-hook 'my/update-projectile-project-name))

;; **** Git changes
(require 'diff-hl)
(defvar-local my/git-changes-string nil)

(defvar-local my/vc-insert-count 0)
(defvar-local my/vc-change-count 0)
(defvar-local my/vc-delete-count 0)

(defun my/mode-line-update-git-changes-string ()
  (setq my/git-changes-string (format "+%d ~%d -%d"
				      my/vc-insert-count
				      my/vc-change-count
				      my/vc-delete-count)))

(defun my/mode-line-update-git-changes-string-reset ()
  (setq my/vc-insert-count 0)
  (setq my/vc-change-count 0)
  (setq my/vc-delete-count 0)
  (setq my/git-changes-string nil))

(defun my/modeline-update-git-changes (changes)
  "CHANGES is generated by `(diff-hl-changes)'"
  (my/mode-line-update-git-changes-string-reset)
  (mapc (lambda (entry)
	  (pcase (nth 2 entry)
	    ('insert (setq my/vc-insert-count (+ my/vc-insert-count (nth 1 entry))))
	    ('change (setq my/vc-change-count (+ my/vc-change-count (nth 1 entry))))
	    ('delete (setq my/vc-delete-count (+ my/vc-delete-count (nth 1 entry))))))
	changes)
  (my/mode-line-update-git-changes-string))

;; ***** Override old function
(defun diff-hl-changes ()
  (my/mode-line-update-git-changes-string-reset)
  (let* ((file buffer-file-name)
	 (backend (vc-backend file)))
    (when backend
      (let ((state (vc-state file backend)))
	(cond
	 ((diff-hl-modified-p state)
	  (let* (diff-auto-refine-mode res)
	    (with-current-buffer (diff-hl-changes-buffer file backend)
	      (goto-char (point-min))
	      (unless (eobp)
		(ignore-errors
		  (diff-beginning-of-hunk t))
		(while (looking-at diff-hunk-header-re-unified)
		  (let ((line (string-to-number (match-string 3)))
			(len (let ((m (match-string 4)))
			       (if m (string-to-number m) 1)))
			(beg (point)))
		    (diff-end-of-hunk)
		    (let* ((inserts (diff-count-matches "^\\+" beg (point)))
			   (deletes (diff-count-matches "^-" beg (point)))
			   (type (cond ((zerop deletes) 'insert)
				       ((zerop inserts) 'delete)
				       (t 'change))))
		      (when (eq type 'delete)
			(setq len 1)
			(cl-incf line))
		      (push (list line len type) res))))))
	    (my/modeline-update-git-changes res)
	    (nreverse res)))
	 ((eq state 'added)
	  `((1 ,(line-number-at-pos (point-max)) insert)))
	 ((eq state 'removed)
	  `((1 ,(line-number-at-pos (point-max)) delete))))))))

;; **** Load average
(defvar my/load-average 0)
(defvar my/high-load-average 2)

(defun my/update-load-average ()
  (interactive)
  (setq my/load-average (/ (nth 0 (load-average)) 100.0)))

(my/status-bar-allocate-update-time 'my/update-load-average)

(my/update-load-average)

;; **** Ram usage
(defvar my/mode-line-enable-available-mem nil)

(if (and (file-exists-p "/proc/meminfo")
	 (progn
	   (with-temp-buffer

	     (insert-file-contents "/proc/meminfo")
	     (setq my/mem-string (buffer-string))
	     (ignore-errors
	       (string-match "MemAvailable:.*\s" my/mem-string)))))
    (setq my/mode-line-enable-available-mem t))

(defvar my/available-mem-formatted "nil")
(defvar my/available-mem 0)

(defun my/linux-update-available-mem ()
  (interactive)
  (with-temp-buffer
    (insert-file-contents "/proc/meminfo")
    (setq my/mem-string (buffer-string))

    (when (ignore-errors
	    (string-match "MemAvailable:.*\s" my/mem-string))
      (setq my/mem-string (match-string 0 my/mem-string))

      ;; Default returns kb, *1000 to get it to bytes
      (setq my/available-mem
	    (* 1000 (string-to-number
		     (substring my/mem-string (string-match "[0-9]" my/mem-string) -1))))

      (setq my/available-mem-formatted (my/file-size-human-readable my/available-mem nil t)))))

(if my/mode-line-enable-available-mem
    (my/status-bar-allocate-update-time 'my/linux-update-available-mem))

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

(my/status-bar-allocate-update-time 'my/update-uptime-timer)

;; **** Break timer
;; In seconds
(defvar my/break-time (* 21 60))
(defvar my/enable-breaks t)

(defun my/break-screen ()
  (when my/enable-breaks
    ;; Restart timer
    (my/break-timer-run)

    ;; Show break buffer
    (switch-to-buffer "Break")
    (insert "Break")
    (message (concat "Break at " (format-time-string "%H:%M")))))

(defun my/break-timer-run ()
  (interactive)
  (run-with-timer my/break-time nil #'my/break-screen))

(when my/enable-breaks
  (my/break-timer-run))

;; *** Status line
(setq my/status-bar 'mini-modeline)

;; **** Status line format
;; Only applicable to X since terminal never stretches, etc
(add-hook 'exwm-workspace-switch-hook 'my/frame-width-update)

(defun my/mode-line-align (left right)
  "Return a string of `window-width' length containing LEFT, and RIGHT aligned respectively."
  (let* ((available-width (- my/frame-width (length left) 2)))
    (format (format "%%s %%%ds" available-width) left right)))

;; mode-line-format
(setq-default my/status-line-format
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
				my/rx-delta-formatted " ↓ "
				"| "
				)))

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

		    ;; (:eval (if (and my/gnus-unread-string (not (string= my/gnus-unread-string "")))
		    ;;	       (concat " | "
		    ;;		       my/gnus-unread-string)))

		    (:eval (if (and my/mu4e-unread-mail-count)
			       (concat " | Mail: "
				       my/mu4e-unread-mail-count)))


		    " | "

		    (:eval my/time)

		    " - "

		    (:eval my/date)
		    ))))))

;; **** Posframe status line
;; https://github.com/dakra/statusbar.el

;; (posframe-delete-frame (get-buffer-create "*window*"))

;; (posframe-show "*window*"
;;	       :string "test"
;;	       :x-pixel-offset 100
;;	       :poshandler 'my/posframe-poshandler-right-side-minibuffer
;;	       ;; :left-fringe 100
;;	       :right-fringe 200
;;	       )

;; (defun my/posframe-poshandler-right-side-minibuffer (info)
;;   (cons (- -1 (plist-get info :minibuffer-height) ) 0))

;; (setq buf "*window*")

;; (defun statusbar--line-length (buf)
;;   "Return current line length of the statusbar text.
;; BUF is the statusbar buffer."
;;   (with-current-buffer buf
;;     (point-max)))

;; (defun statusbar--position-handler (info)
;;   "Posframe position handler.
;; INFO is the childframe plist from `posframe'.
;; Position the statusbar in the bottom right over the minibuffer."
;;   (let* ((font-width (plist-get info :font-width))
;;	 (buf (plist-get info :posframe-buffer))
;;	 (buf-width (* font-width (statusbar--line-length buf)))
;;	 (parent-frame (plist-get info :parent-frame))
;;	 (parent-frame-width (frame-pixel-width parent-frame))
;;	 (exwm-systemtray-offset
;;	  (if-let* ((tray-list (and (boundp 'exwm-systemtray--list) exwm-systemtray--list))
;;		    (icon-size (+ exwm-systemtray--icon-min-size exwm-systemtray-icon-gap))
;;		    (tray-width (* (length exwm-systemtray--list) icon-size)))
;;	      tray-width
;;	    0))
;;	 (x-offset (plist-get info :x-pixel-offset))
;;	 (x-pos (- parent-frame-width buf-width x-offset exwm-systemtray-offset))
;;	 ;; (y-pos (+ exwm-floating--cursor-top-right))
;;	 (font-height (plist-get info :font-width))
;;     ;; (y-pos exwm-floating--cursor-top-right))
;;     ;; (y-pos 2097070))
;;     (cons x-pos y-pos)))

;; **** mini-modeline
(when (string= my/status-bar 'mini-modeline)
  (straight-use-package 'mini-modeline)

  (setq mini-modeline-enhance-visual nil)
  ;; Mini-modeline flashes during GC if this is t
  (setq garbage-collection-messages nil)

  ;; This fixes a bug with ~counsel-describe-function~
  (setq mini-modeline-echo-duration 99999)

  (setq-default mini-modeline-r-format my/status-line-format)
  (mini-modeline-mode 1))

;; **** LV-line (top modeline)
;; Use lv-line to create a mode line on the top of the screen
(defvar my/lv-line-format "")
(defconst my/lv-line--buffer " *LV-line*")
(defvar my/lv-line-window nil)

(when (string= my/status-bar 'lv-line)
  (setq my/lv-line-format my/status-line-format))

;; ***** Init
(when (string= my/status-bar 'lv-line)
  (add-hook 'exwm-init-hook (lambda () (interactive) (run-with-timer 1 nil (lambda () (interactive) (my/frame-width-update) (my/lv-line-update)))) t))

;; ***** Create LV-line at top
(defun my/lv-line-set-buffer ()
  (setq-local mode-line-format nil)
  (setq-local header-line-format nil)
  (setq indicate-empty-lines nil)
  (set-window-hscroll my/lv-line-window 0)
  (setq window-size-fixed t)
  (setq truncate-lines t)
  (visual-line-mode -1)

  ;; Offset by 10 pixels to make text fit
  ;;(set-window-fringes (selected-window) 10 0)

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
	(select-window original-window)))
  (my/frame-width-update))

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

;; ***** Update it
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

(defun my/lv-line-start ()
  (my/lv-line-create)
  (my/lv-line-update)
  (run-with-timer my/status-line-update-offset 60 'my/lv-line-update))

(when (string= my/status-bar 'lv-line)
  (if window-system
      (add-hook 'exwm-init-hook 'my/lv-line-start)
    (my/lv-line-start)))

;; ****** Update on new events
(when (string= my/status-bar 'lv-line)
  ;; Update on alert
  (add-hook 'my/alert-updated-hook 'my/lv-line-update)

  ;; Update on mail counter change
  (add-hook 'my/gnus-mail-counter-update-hook 'my/lv-line-update))

;; * Theme
(defvar my/default-face-list '())

(defun my/set-face-to-default (face-name is-syntax)
  (add-to-list 'my/default-face-list face-name)
  ;; Reset face
  (set-face-attribute face-name nil :family 'unspecified :foundry 'unspecified :width 'unspecified :height 'unspecified :weight 'unspecified :slant 'unspecified :foreground 'unspecified :background 'unspecified :underline 'unspecified :overline 'unspecified :strike-through 'unspecified :box 'unspecified :stipple 'unspecified :font 'unspecified :inherit 'default))

;; ** Define colors
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
      ;; (setq my/foreground-color (color-darken-name my/foreground-color 10))
      (setq my/foreground-color-1 (color-darken-name my/foreground-color 5))
      (setq my/foreground-color-2 (color-darken-name my/foreground-color 10))
      (setq my/foreground-color-3 (color-darken-name my/foreground-color 15))
      (setq my/foreground-color-4 (color-darken-name my/foreground-color 20))
      (setq my/foreground-color-5 (color-darken-name my/foreground-color 25))
      (setq my/foreground-color-6 (color-darken-name my/foreground-color 30))

      (setq my/background-color (color-darken-name "#292b2e" 10))
      ;; (setq my/background-color "#121212")
      ;; (setq my/background-color "#212121")
      ;; (setq my/background-color "#232323")
      ;; (setq my/background-color "#000000")
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

;; ** Remove color
(defun my/theme-remove-color ()
  (cl-loop for face in (face-list) do
	   ;; Don't change magit faces
	   (if (and (not (string-match "magit" (symbol-name face))) (not (string-match "w3m" (symbol-name face))))
	       (set-face-attribute face nil :foreground nil :background nil))))

;; ** Set colors
;; *** Default colors
(defun my/theme-default-colors ()
  (set-face-attribute 'default nil :foreground my/foreground-color :background my/background-color)
  (set-face-attribute 'link nil :foreground my/background-color :background my/foreground-color)
  (set-face-attribute 'highlight nil :foreground my/foreground-color :background my/mark-color)
  (set-face-attribute 'region nil :foreground my/foreground-color :background my/mark-color)
  (set-face-attribute 'error nil :foreground "#c6350b" :background)
  (set-face-attribute 'warning nil :foreground "DarkOrange" :background)

  (set-face-attribute 'font-lock-doc-face nil :foreground my/foreground-color :background my/background-color-4)
  ;; (set-face-attribute 'font-lock-comment-face nil :foreground (color-lighten-name my/background-color 30) :background my/background-color)
  (set-face-attribute 'font-lock-comment-face nil :foreground (color-lighten-name my/background-color 30) :background (color-lighten-name my/background-color 2))
  ;; (set-face-attribute 'font-lock-comment-delimiter-face nil :foreground (color-lighten-name my/background-color 15) :background my/background-color)
  (set-face-attribute 'font-lock-comment-delimiter-face nil :foreground my/background-color-4 :background my/background-color-2)
  (my/set-face-to-default 'font-lock-string-face t)
  (my/set-face-to-default 'font-lock-function-name-face t))

(defun my/theme-outline-colors ()
  (set-face-attribute 'outline-1 nil :foreground (color-lighten-name my/background-color 2) :background (color-darken-name my/foreground-color 50))
  (set-face-attribute 'outline-2 nil :foreground (color-lighten-name my/background-color 2) :background (color-darken-name my/foreground-color 50))
  (set-face-attribute 'outline-3 nil :foreground (color-lighten-name my/background-color 2) :background (color-darken-name my/foreground-color 50))
  (set-face-attribute 'outline-4 nil :foreground (color-lighten-name my/background-color 2) :background (color-darken-name my/foreground-color 50))
  (set-face-attribute 'outline-5 nil :foreground (color-lighten-name my/background-color 2) :background (color-darken-name my/foreground-color 50))
  (set-face-attribute 'outline-6 nil :foreground (color-lighten-name my/background-color 2) :background (color-darken-name my/foreground-color 50))
  (set-face-attribute 'outline-7 nil :foreground (color-lighten-name my/background-color 2) :background (color-darken-name my/foreground-color 50))
  (set-face-attribute 'outline-8 nil :foreground (color-lighten-name my/background-color 2) :background (color-darken-name my/foreground-color 50)))

;; (defun my/theme-outline-colors ()
;;   (set-face-attribute 'outline-1 nil :foreground (color-darken-name my/foreground-color 50) :background (color-lighten-name my/background-color 10))
;;   (set-face-attribute 'outline-2 nil :foreground (color-darken-name my/foreground-color 50) :background (color-lighten-name my/background-color 10))
;;   (set-face-attribute 'outline-3 nil :foreground (color-darken-name my/foreground-color 50) :background (color-lighten-name my/background-color 10))
;;   (set-face-attribute 'outline-4 nil :foreground (color-darken-name my/foreground-color 50) :background (color-lighten-name my/background-color 10))
;;   (set-face-attribute 'outline-5 nil :foreground (color-darken-name my/foreground-color 50) :background (color-lighten-name my/background-color 10))
;;   (set-face-attribute 'outline-6 nil :foreground (color-darken-name my/foreground-color 50) :background (color-lighten-name my/background-color 10))
;;   (set-face-attribute 'outline-7 nil :foreground (color-darken-name my/foreground-color 50) :background (color-lighten-name my/background-color 10))
;;   (set-face-attribute 'outline-8 nil :foreground (color-darken-name my/foreground-color 50) :background (color-lighten-name my/background-color 10)))

(defun my/theme-header-line-color ()
  (set-face-attribute 'header-line nil
		      ;; Green mode line
		      :foreground my/foreground-color
		      ;; :background "#052000"
		      :background (color-darken-name "#5d4d7a" 10)
		      ;; :foreground my/background-color
		      ;; :background my/foreground-color
		      )

  (set-face-attribute 'my/mode-line-highlight nil
		      :foreground "#063000"
		      :background my/foreground-color)

  ;; Mode line separator
  ;; Set mode line height
  ;;  (set-face-attribute 'mode-line nil
  ;;                      :foreground my/foreground-color
  ;;                      :background my/background-color-1)
  ;;
  ;;  (set-face-attribute 'mode-line-inactive nil
  ;;                      :foreground my/foreground-color
  ;;                      :background my/background-color)
  )

(defun my/theme-evil-colors ()
  ;; Evil
  (setq evil-emacs-state-cursor '("purple" box))
  (setq evil-normal-state-cursor '("red" box))
  (setq evil-visual-state-cursor '("yellow" box))
  (setq evil-insert-state-cursor '("orange" box))
  (setq evil-replace-state-cursor '("green" box))
  (setq evil-operator-state-cursor '("white" hollow))
  (setq evil-operator-state-cursor '("white" hollow)))

(defun my/theme-diff-colors ()
  ;; Diff
  (set-face-attribute 'diff-added nil  :background my/diff-added-color)
  (set-face-attribute 'diff-changed nil :background my/diff-changed-color)
  (set-face-attribute 'diff-removed nil :background my/diff-removed-color)

  (set-face-attribute 'diff-refine-added nil  :background my/diff-added-hl-color)
  (set-face-attribute 'diff-refine-changed nil :background my/diff-changed-hl-color)
  (set-face-attribute 'diff-refine-removed nil :background my/diff-removed-hl-color)

  ;; Ediff
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
  (set-face-attribute 'ediff-odd-diff-C nil :background (color-darken-name my/diff-changed-color 20)))

(defun my/theme-org-colors ()
  ;; =affects this text=
  (set-face-attribute 'org-verbatim nil :weight 'bold)
  (set-face-attribute 'org-quote nil :slant 'italic)
  (set-face-attribute 'org-mode-line-clock nil :foreground my/foreground-color :background my/foreground-color :height 'unspecified)
  (set-face-attribute 'org-mode-line-clock-overrun nil :foreground my/foreground-color :background "red" :height 'unspecified)
  (set-face-attribute 'org-agenda-filter-effort nil :foreground my/foreground-color :background my/background-color :height 'unspecified)
  (set-face-attribute 'org-agenda-filter-regexp nil :foreground my/foreground-color :background my/background-color :height 'unspecified)
  (set-face-attribute 'org-agenda-filter-tags nil :foreground my/foreground-color :background my/background-color :height 'unspecified) (set-face-attribute 'org-agenda-filter-category nil :foreground my/foreground-color :background my/background-color :height 'unspecified)

  (set-face-attribute 'org-code nil :background my/background-color-3)
  (set-face-attribute 'org-block nil :background my/background-color-1)

  ;; Disable right of header background coloring
  (set-face-attribute 'org-meta-line nil :background nil)

  ;; Used by org-src block borders, by default it uses the comment face
  ;;  (set-face-attribute 'org-block-begin-line nil :background my/background-color-3)
  ;;  (set-face-attribute 'org-block-end-line nil :background my/background-color-3)

  ;; Used by org src-blocks when in use, might also be used for other things
  (set-face-attribute 'secondary-selection nil :background (color-darken-name my/background-color 5)))

;; *** Package colors
(defun my/theme-package-colors ()
  (when (require 'hl-line nil 'noerror)
    ;; (set-face-attribute 'hl-line nil :foreground my/foreground-color :background my/background-color-2 :underline nil)
    ;; (set-face-attribute 'hl-line nil :foreground my/foreground-color :background (color-darken-name "#00008b" 20) :underline nil)
    (set-face-attribute 'hl-line nil :foreground my/foreground-color :background "#212026" :underline nil)
    )

  ;;  Show-paren
  (set-face-attribute 'show-paren-match nil :background my/background-color :foreground my/foreground-color)
  (set-face-attribute 'show-paren-match-expression nil :background my/foreground-color :foreground my/background-color)
  (set-face-attribute 'my/show-paren-offscreen-face nil :inherit 'highlight)

  ;; Wgrep
  (set-face-attribute 'wgrep-file-face nil :background my/foreground-color-6 :foreground my/background-color)

  ;; Ivy grep
  (set-face-attribute 'ivy-grep-info nil :background my/foreground-color-6 :foreground my/background-color)

  ;; Symbol overlay
  (if window-system
      (set-face-attribute 'symbol-overlay-default-face nil :foreground my/foreground-color :background my/mark-color-5))

  ;; Dired
  (set-face-attribute 'dired-directory nil :foreground my/background-color :background my/foreground-color)
  (my/set-face-to-default 'dired-perm-write 't)
  (set-face-attribute 'dired-symlink nil :foreground 'unspecified :background 'unspecified :inherit font-lock-comment-face)

  ;; Spray
  ;;  (set-face-attribute 'spray-accent-face nil :foreground "red" :background my/background-color)
  (set-face-attribute 'spray-accent-face nil :foreground my/foreground-color :background my/background-color :underline t)

  ;; Isearch
  (set-face-attribute 'isearch nil :foreground my/background-color :background my/foreground-color)
  (set-face-attribute 'lazy-highlight nil :foreground my/background-color :background my/foreground-color)
  ;; Haskell
  (set-face-attribute 'haskell-literate-comment-face nil :foreground 'unspecified :background 'unspecified :inherit font-lock-comment-face)

  ;; Highlight thing
  ;;(set-face-attribute 'symbol-overlay nil :foreground my/foreground-color :background my/mark-color)

  ;; Company
  (set-face-attribute 'company-scrollbar-bg nil :background my/background-color)
  (set-face-attribute 'company-scrollbar-fg nil :background my/foreground-color)
  ;; Selected entry
  (set-face-attribute 'company-tooltip-selection nil :background my/foreground-color :foreground my/background-color)
  ;; All unmatching text
  (set-face-attribute 'company-tooltip nil :foreground my/foreground-color :background my/background-color-1)
  ;; All matching text
  (set-face-attribute 'company-tooltip-common nil :foreground my/background-color :background my/foreground-color)

  ;; Popup menu
  ;; Selected entry
  (when (require 'popup nil 'noerror)
    (set-face-attribute 'popup-menu-selection-face nil :foreground my/background-color :background my/foreground-color)
    ;; All unmatching text
    (set-face-attribute 'popup-menu-face nil :foreground my/foreground-color :background my/background-color-1))

  ;; Ivy
  ;; Ivy also uses "font-lock-doc-face" for the documentation
  (set-face-attribute 'ivy-current-match nil :foreground my/background-color :background my/mark-color-3)
  (set-face-attribute 'ivy-cursor nil :foreground my/background-color :background my/foreground-color)
  (set-face-attribute 'ivy-minibuffer-match-highlight nil :foreground my/background-color :background my/foreground-color)
  ;;(set-face-attribute 'ivy-separator nil :foreground 'unspecified :background 'unspecified :inherit font-lock-comment-face)

  (set-face-attribute 'ivy-minibuffer-match-face-1 nil :foreground my/background-color :background my/foreground-color)
  (set-face-attribute 'ivy-minibuffer-match-face-2 nil :foreground my/background-color :background my/foreground-color-2)
  (set-face-attribute 'ivy-minibuffer-match-face-3 nil :foreground my/background-color :background my/foreground-color-4)
  (set-face-attribute 'ivy-minibuffer-match-face-4 nil :foreground my/background-color :background my/foreground-color-6)

  ;; Ivy yasnippet
  (set-face-attribute 'ivy-yasnippet-key nil :foreground 'unspecified :background 'unspecified :inherit font-lock-comment-face)

  ;; Ivy rich
  (set-face-attribute 'my/ivy-rich-doc-face nil :foreground 'unspecified :background 'unspecified :inherit font-lock-comment-face)
  (set-face-attribute 'my/ivy-rich-switch-buffer-indicator-face nil :foreground 'unspecified :background 'unspecified :inherit font-lock-comment-face)
  (set-face-attribute 'my/ivy-rich-switch-buffer-major-mode-face nil :foreground 'unspecified :background 'unspecified :inherit font-lock-comment-face)
  (set-face-attribute 'my/ivy-rich-switch-buffer-size-face nil :foreground 'unspecified :background 'unspecified :inherit font-lock-comment-face)
  (set-face-attribute 'my/ivy-rich-switch-buffer-path-face nil :foreground 'unspecified :background 'unspecified :inherit font-lock-comment-face)
  (set-face-attribute 'my/ivy-rich-switch-buffer-project-face nil :foreground 'unspecified :background 'unspecified :inherit font-lock-comment-face)
  (set-face-attribute 'my/ivy-rich-find-file-symlink-face nil :foreground 'unspecified :background 'unspecified :inherit font-lock-comment-face)

  ;; Swiper
  (set-face-attribute 'swiper-match-face-1 nil :foreground my/background-color :background my/foreground-color)
  (set-face-attribute 'swiper-match-face-2 nil :foreground my/background-color :background my/foreground-color-2)
  (set-face-attribute 'swiper-match-face-3 nil :foreground my/background-color :background my/foreground-color-4)
  (set-face-attribute 'swiper-match-face-4 nil :foreground my/background-color :background my/foreground-color-6)

  ;; Avy
  (set-face-attribute 'avy-lead-face nil :foreground my/background-color :background my/foreground-color-6)
  (set-face-attribute 'avy-lead-face-0 nil :foreground my/background-color :background my/foreground-color-2)
  (set-face-attribute 'avy-lead-face-1 nil :foreground my/background-color :background my/foreground-color-4)
  (set-face-attribute 'avy-lead-face-2 nil :foreground my/background-color :background my/foreground-color-6)

  ;; Eshell
  (require 'em-prompt)
  (if window-system
      (set-face-attribute 'eshell-prompt nil :foreground "purple" :background my/background-color)
    (set-face-attribute 'eshell-prompt nil :foreground "magenta" :background my/background-color))

  ;; Yascroll
  (set-face-attribute 'yascroll:thumb-fringe nil :background "slateblue" :foreground "slateblue")
  (set-face-attribute 'yascroll:thumb-text-area nil :background "slateblue")

  ;; Term
  (set-face-attribute 'term-color-black nil :foreground "black" :background "black")
  (set-face-attribute 'term-color-blue nil :foreground "blue" :background "blue")
  (set-face-attribute 'term-color-cyan nil :foreground "cyan" :background "cyan")
  (set-face-attribute 'term-color-green nil :foreground "green" :background "green")
  (set-face-attribute 'term-color-magenta nil :foreground "magenta" :background "magenta")
  (set-face-attribute 'term-color-red nil :foreground "red" :background "red")
  (set-face-attribute 'term-color-white nil :foreground "white" :background "white")
  (set-face-attribute 'term-color-yellow nil :foreground "yellow" :background "yellow")

  ;; Flyspell
  (when window-system
    (set-face-attribute 'flyspell-incorrect nil :underline '(:style wave :color "Blue"))
    (set-face-attribute 'flyspell-duplicate nil :underline '(:style wave :color "LightBlue")))

  ;; Litable
  (when (require 'litable nil 'noerror)
    (set-face-attribute 'litable-result-face nil :foreground my/foreground-color :background my/background-color :weight 'bold)
    (set-face-attribute 'litable-substitution-face nil :foreground my/foreground-color :background my/background-color :weight 'bold))

  ;; Diff-hl
  (set-face-attribute 'diff-hl-change nil :background (face-attribute 'diff-changed :background))

  ;; Paren highlight
  ;;(set-face-attribute 'show-paren-match nil :foreground my/foreground-color :background my/mark-color-5)
  (set-face-attribute 'show-paren-match nil :foreground my/background-color :background my/foreground-color)
  (set-face-attribute 'show-paren-mismatch nil :background "red")

  ;; Flycheck-posframe
  (set-face-attribute 'flycheck-posframe-background-face nil :foreground my/foreground-color :background "#000000")

  ;; lsp
  (my/set-face-to-default 'lsp-ui-doc-background nil)
  ;;(set-face-attribute 'lsp-ui-doc-background nil :foreground my/foreground-color :background my/background-color)

  ;; lsp Doc
  (set-face-attribute 'lsp-ui-doc-header nil :foreground my/foreground-color :background my/background-color-4)
  (set-face-attribute 'lsp-ui-doc-url nil :foreground my/background-color :background my/foreground-color)

  ;; lsp Sideline
  (my/set-face-to-default 'lsp-ui-peek-filename nil)
  (my/set-face-to-default 'lsp-ui-peek-footer nil)
  (my/set-face-to-default 'lsp-ui-peek-header nil)
  (my/set-face-to-default 'lsp-ui-peek-highlight nil)
  (my/set-face-to-default 'lsp-ui-peek-line-number nil)
  (my/set-face-to-default 'lsp-ui-peek-list nil)
  (my/set-face-to-default 'lsp-ui-peek-peek nil)
  (my/set-face-to-default 'lsp-ui-peek-selection nil)
  (my/set-face-to-default 'lsp-ui-sideline-current-symbol nil)
  (my/set-face-to-default 'lsp-ui-sideline-code-action nil)

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

  ;; lsp lens
  ;; (set-face-attribute 'lsp-lens-face nil :foreground my/foreground-color :background my/mark-color-4 :height 0.8)
  (set-face-attribute 'lsp-lens-face nil :foreground 'unspecified :background my/background-color :inherit font-lock-comment-face)

  ;; Highlight faces
  (when (require 'highlight-indent-guides nil 'noerror)
    (highlight-indent-guides-auto-set-faces)))

(defun my/theme ()
  (interactive)
  (my/theme-remove-color)
  (my/theme-default-colors)
  (my/theme-outline-colors)
  (my/theme-header-line-color)
  (my/theme-evil-colors)
  (my/theme-diff-colors)
  (my/theme-org-colors)
  (my/theme-package-colors))

(if window-system
    (add-hook 'exwm-init-hook 'my/theme)
  (add-hook 'after-init-hook 'my/theme))

(define-key my/leader-map (kbd "M-c") 'my/theme)

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
;; *** git-backup
(straight-use-package '(git-backup :type git :host github :repo "antham/git-backup"))
(straight-use-package '(git-backup-ivy :type git :host github :repo "walseb/git-backup-ivy"))
(require 'git-backup-ivy)

(add-hook 'after-save-hook (lambda () (git-backup-version-file git-backup-ivy-git-path git-backup-ivy-backup-path nil (buffer-file-name))))

(define-key my/leader-map (kbd "C-u") 'git-backup-ivy)

;; *** Manual way
;; (defun my/backup-buffer-per-session ()
;;   (interactive)
;;   (if (not my/first-save)
;;       (my/backup-buffer my/backup-per-session-directory)))

;; **** Delete old backups
;; Automatically delete old backup files older than a week
;; (message "Deleting old long term backup files...")
;; (my/delete-everything-older-than my/backup-directory (* 60 60 24 7))

;; ** Delete per-session backups on startup
(ignore-errors
  ;; Delete anything older than a day
  (my/delete-everything-older-than my/backup-directory (* 60 60 24 1)))

;; ** Undo
;; *** Disable undo warning buffer
;; There is a warning window that pops up if you have made too many changes to a buffer, this might stop long macros, so stop that window from popping up
(require 'warnings)
(add-to-list 'warning-suppress-types '(undo discard-info))

;; ** Undo tree
(straight-use-package 'undo-tree)

(setq global-undo-tree-mode t)

;; Fixes errors
(setq undo-tree-enable-undo-in-region nil)
(setq-default undo-tree-enable-undo-in-region nil)

(setq-default undo-tree-visualizer-lazy-drawing nil)

(setq undo-tree-visualizer-timestamps t)
(setq undo-tree-visualizer-diff t)

;; *** Persistent history
(setq my/undo-tree-history-dir (concat user-emacs-directory "undo-tree"))

(ignore-errors
  (make-directory my/undo-tree-history-dir))

(setq-default undo-tree-auto-save-history t)
(setq undo-tree-history-directory-alist `(("." . ,my/undo-tree-history-dir)))

;; *** Disable modes in visualizer
(add-hook 'undo-tree-visualizer-mode-hook (lambda () (add-hook 'visual-line-mode-hook (lambda () (when visual-line-mode (visual-line-mode -1))) nil t)))

;; *** Keys
(with-eval-after-load 'undo-tree
  (setq undo-tree-visualizer-mode-map (make-sparse-keymap))

  (evil-define-key 'insert undo-tree-visualizer-mode-map (kbd "p") #'undo-tree-visualize-undo)

  (evil-define-key 'insert undo-tree-visualizer-mode-map (kbd "p") #'undo-tree-visualize-undo)
  (evil-define-key 'insert undo-tree-visualizer-mode-map (kbd "n") #'undo-tree-visualize-redo)
  (evil-define-key 'insert undo-tree-visualizer-mode-map (kbd "l") #'undo-tree-visualize-switch-branch-right)
  (evil-define-key 'insert undo-tree-visualizer-mode-map (kbd "h") #'undo-tree-visualize-switch-branch-left)
  (evil-define-key 'insert undo-tree-visualizer-mode-map (kbd "d") #'undo-tree-visualizer-toggle-diff))

;; * Run command on boot
(if my/run-command-on-boot
    (async-shell-command my/run-command-on-boot))

;; * Restore gc mem
(setq gc-cons-threshold my/after-gc-mem)
(garbage-collect)

;; * Report start time
(run-with-timer 4 nil (lambda () (interactive) (message (concat "Booted in " (emacs-init-time)))))

;; * Byte-compile the config
;; Byte compilation doesn't work before loading everything because of some reason, so do it now
(unless (and (file-exists-p my/config-compiled-location) (my/is-file-more-up-to-date my/config-compiled-location my/config-location))
  (byte-compile-file "~/.emacs.d/config.el" nil)
  (message "Config is now byte compiled, restart to run it"))
