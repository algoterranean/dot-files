;; zenburn
;; ample-zen
;; afternoon
;; dakrone
;; deep-thought
;; espresso
;; flatland
;; gruvbox
;; heroku
;; leuven
;; nifelheim
;; purple-haze
;; solarized-light
;; subatomic
;; subatomic256
;; tronesque
;; underwater
;; misterioso


;;======================
;; SETUP
;;======================

(server-start)

(add-to-list 'load-path "~/.emacs.d/")

;; support for home/work profiles that need to be kept separate for security reasons
(defvar work-emacs-file "~/.emacs.work")
(defvar home-emacs-file "~/.emacs.home")
(cond ((file-exists-p work-emacs-file)
       (load-file work-emacs-file))
      ((file-exists-p home-emacs-file)
       (load-file home-emacs-file)))


(require 'cl)
(require 'package)
(setq package-archives
      '(("gnu" . "http://elpa.gnu.org/packages/")
	("marmalade" . "http://marmalade-repo.org/packages/")
	("melpa" . "http://melpa.milkbox.net/packages/")))
(package-initialize)


(defvar my-package-list '(anzu
			  ace-jump-mode
			  adaptive-wrap
			  apples-mode
			  buffer-stack
			  crosshairs
			  csharp-mode
			  django-mode
			  dired+
			  dired-details
			  dired-details+
			  display-theme
			  edit-server
			  exec-path-from-shell
			  erc-terminal-notifier
			  ercn
			  flycheck
			  flyspell
			  fringe-current-line
			  gist
			  helm
			  helm-chrome
			  helm-c-yasnippet
			  helm-dash
			  helm-flycheck
			  helm-ls-git
			  helm-google
			  helm-proc
			  helm-dictionary
			  helm-themes
			  helm-swoop
			  helm-make
			  helm-mode-manager
			  helm-package
			  helm-proc
			  helm-projectile
			  helm-projectile-all
			  ;; helm-spaces ;; TODO make something similar for workgroups2
			  hide-comnt ;; locks up emacs?
			  htmlfontify
			  htmlize
			  itail
			  js2-mode
			  magit
			  markdown-mode
			  multiple-cursors
			  ;; mark-multiple
			  nav
			  omnisharp
			  org-magit
			  org-screenshot
			  pianobar
			  powerline
			  ;; popwin
			  python-mode
			  projectile
			  rainbow-mode
			  scratch-persist
			  slime
			  ;; session
			  server
			  sqlite
			  weblogger
			  w3m
			  workgroups2   ;; origional workgroups.el is buggy
			  yasnippet))



(defvar my-require-list '(ace-jump-mode
			  adaptive-wrap
			  algo-mode
			  apples-mode
			  buffer-stack
			  edmacro
			  erc-nicklist
			  display-theme
			  dired+
			  dired-details+
			  helm
			  helm-files
			  helm-ring
			  flycheck
			  magit
			  multiple-cursors
			  nav
			  org
			  powerline
			  sqlite
			  ;; popwin

			  ;; session
			  workgroups2
			  ;; spaces
			  scratch-persist
			  vc
			  ;; non-packages
			  boo-mode
			  cg-mode
			  ;; nxhtml-mumamo-mode
			  yasnippet
			  ))

(defvar my-theme-list-pos 0)
(defvar my-theme-list '(afternoon-theme
			ample-theme
			ample-zen-theme
			anti-zenburn-theme
			assemblage-theme
			bubbleberry-theme
			busybee-theme
			clues-theme
			cyberpunk-theme
			dakrone-theme
			darkburn-theme
			deep-thought-theme
			django-theme
			espresso-theme
			flatland-theme
			flatui-theme
			gandalf-theme
			grandshell-theme
			gruvbox-theme
			heroku-theme
			inkpot-theme
			ir-black-theme
			jujube-theme
			late-night-theme
			leuven-theme
			molokai-theme
			monochrome-theme
			monokai-theme
			mustang-theme
			naquadah-theme
			niflheim-theme
			noctilux-theme
			nzenburn-theme
			obsidian-theme
			occidental-theme
			planet-theme
			purple-haze-theme
			qsimpleq-theme
			remember-theme
			reverse-theme
			soft-morning-theme
			soft-stone-theme
			solarized-theme
			soothe-theme
			spacegray-theme
			steady-theme
			subatomic-theme
			subatomic256-theme
			sunny-day-theme
			tango-2-theme
			tango-plus-theme
			tangotango-theme
			tommyh-theme
			toxi-theme
			tron-theme
			tronesque-theme
			twilight-theme
			ujelly-theme
			underwater-theme
			waher-theme
			zen-and-art-theme
			zenburn-theme))




(defun my-packages-installed-p ()
  (loop for p in (append my-package-list my-theme-list)
	when (not (package-installed-p p)) do (return nil)
	finally (return t)))


(defun update-my-packages ()
  ;; update and install all packages if necessary
  (unless (my-packages-installed-p)
    (message "%s" "Emacs is not refreshing its package database..")
    (package-refresh-contents)
    (message "%s" " done.")
    ;; install the missing packages
    (dolist (p (append my-package-list my-theme-list))
      (when(not (package-installed-p p))
	(package-install p)))))

(update-my-packages)

;; require anything needed below, ignoring any that don't exist
(mapc (lambda (x) (require x nil t)) 
      my-require-list)



;;======================
;; PREFERENCES
;;======================


;; display and visuals
(load-theme 'zenburn t)
(defvar my-current-theme 'zenburn)
(setq theme-load-from-file nil)

(menu-bar-mode -1)
(scroll-bar-mode -1)
(tool-bar-mode -1)
(global-font-lock-mode t)
(show-paren-mode t)
(column-number-mode t)
(linum-mode t)
(setq transient-mark-mode t ;; enable visual feedback on selections
      frame-title-format "%b - emacs"
      initial-frame-alist '((fullscreen . maximized))
      ns-pop-up-frames nil)


(setq echo-keystrokes 0.1)
(setq font-lock-maximum-decoration t)
(setq font-lock-verbose nil)


;; indentation
(setq tab-width 4
      python-indent 4
      c-default-style "linux" c-basic-offset 4) ;; set the default indentation style for c-mode

;; powerline
(set-face-attribute
 'mode-line nil
 :foreground "#030303"
 :box nil)
(set-face-attribute
 'mode-line-inactive nil
 :background "#666666"
 :foreground "grey22"
 :box nil)
(setq header-line-format nil)

;; themes
(add-to-list 'custom-theme-load-path "~/.emacs.d/elpa")
(setq custom-safe-themes t)


;; init
(setq inhibit-default-init t
      default-directory "~/"
      inhibit-startup-screen t
      initial-scratch-message nil
      ecb-tip-of-the-day nil)

;; quitting
(setq confirm-kill-emacs nil) ;;'yes-or-no-p)

;; backups and autosaves
(setq backup-inhibited t
      make-backup-files -1
      auto-save-default nil)
(auto-save-mode nil)


;; debugging
(setq stack-trace-on-error t
      debug-on-error t)

;; misc
(put 'overwrite-mode 'disabled t)
(if (load "mwheel" t) (mwheel-install))
(add-hook 'comint-output-filter-functions
	  'comint-watch-for-password-prompt) ;; don't echo passwords
(when window-system
  (global-unset-key "\C-z")) ; iconify-or-deiconify-frame (C-x C-z)
(put 'narrow-to-region 'disabled nil)

(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

(cua-mode 0) ;; disable cua mode
(setq magic-mode-alist nil) ;; auto-mode-alist is the only list used for file associations
(setq confirm-nonexistent-file-or-buffer nil) ;; do not confirm a new file or buffer
(setq blink-cursor t) ;; blink the cursor when stationary
(display-time-mode t)

(setq delete-by-moving-to-trash t)
(setq shift-select-mode nil)
(setq truncate-partial-width-windows nil)

(setq auto-compression-mode t) ;; transparently open compressed files

;; don't show these buffers when trying to switch
(setq buffer-stack-untracked '("*Help*" "*Completions*" "*scratch*"
			       "*Messages*" "*Backtrace*" "*Warnings*"))


(setq helm-dash-common-docsets '("Mono" "Unity 3D"))
(setq browse-url-browser-function 'w3m-browse-url)

;; (nav-disable-overeager-window-splitting) ;; nav


;;======================
;; KEYBINDINGS
;;======================

(kbd "C-c SPC")

;; TODO: create function to pick one of these keybindings at random and quiz
;; user about what it does or what the keybinding is for that functions
(defvar my-global-keybindings
  '(
    ;; useful buffer commands
    ("\C-xB" bury-buffer)
    ("\C-xE" apply-macro-to-region-lines)
    ("\C-xI" insert-buffer)
    ("\C-cg" goto-line)
    ("\C-cG" goto-char)
    ("\C-cw" delete-region) ; ala C-w and M-C-w
    ([?\C-,] previous-buffer)
    ([?\C-.] next-buffer)
    ;; window and frame navigation
    ([C-return] other-window)
    ([C-M-return] other-frame)
    ;; comment/uncomment
    ("\C-cc" comment-region)
    ("\C-cu" uncomment-region)
    ;; ace-jump
    ("\C-c " ace-jump-mode) ;; C-c SPC
    ;; helm
    ("\C-c\C-h\C-f" helm-for-files)
    ("\C-c\C-h\C-k" helm-bookmark-helper)
    ("\C-c\C-h\C-d" helm-dash)

    ("\C-c\C-h\C-b" helm-buffers-list)
    ("\C-x\C-b" helm-buffers-list)
    ("\C-xb" helm-buffers-list)
    ("\M-y" helm-show-kill-ring)

    ;;("\C-c\C-h\C-g" helm-ls-git) ;; TODO customize this so that it ignores .meta files
    ("\C-c\C-h\C-s" helm-swoop)
    ("\C-c\C-h\C-h" helm-mini)
    ("\C-c\C-h\C-o" helm-org-headlines)
    ("\C-c\C-h\C-t" helm-themes)
    ("\C-c\C-h\C-y" helm-flycheck)
    ("\M-x" helm-M-x)
    ("\C-c\C-h\C-g" helm-do-grep)
    ("\C-c\C-h\C-u" helm-unity-execute)
    ("\C-c\C-h\C-p" helm-projectile)
    ([C-f9] helm-ls-git)
    ;; ("\C-xb" helm-buffers-list)
    ("\C-c\C-h\C-r" helm-regexp)
    ;; helm-help
    ;; helm-keybindings
    ;; helm-websearch
    ;; helm-yank-kill
    ;; helm-
    ("\C-x\C-c" revert-workgroups-first)
    ;; irc
    ("\C-ci" start-my-erc)
    ;; TODO add command to toggle erc-nicklist
    ;; TOOD figure out notification window system, particularly for private chat

    ;; TODO create helm support for workgroups2
    ;; ("\C-cs" helm-spaces)
    ;; pianobar
    ("\C-cp" pianobar)
    ;; eshell
    ([f11] eshell)
    ;; TODO add toggle for "desktop" saving, logging, and switching
    ;; native fullscreen for win32 and osx
    ([f7] toggle-full-screen-cross-platform)
    ;; grep
    ;; ([C-f10] search-proj-recursively)
    ;; ([f10] search-proj-directory)
    ;; navigate to the matching paren/brace
    ("%" goto-match-paren)
    ;; join line shortcut
    ("\C-j" join-line)
    ;; google-code nav mode (like "directory" view in IDEs)
    ([f8] toggle-nav-or-nicklist)
    ;; magit toggle
    ([f9] magit-status)
    ;; alternative way to use Meta-X (with C thrown for fat-fingering)
    ("\C-x\C-m" execute-extended-command)
    ("\C-c\C-m" execute-extended-command)
    ;; recenter text on screen (vertically)
    ([C-l] '(recenter 1))
    ;; insert today's date in MM.DD.YYYY format
    ("\C-cd" insert-date)
    ;; org stuff
    ;; ("\C-cl" org-store-link) ;; TODO remap this
    ("\C-ca" org-agenda)
    ;; cycle through kill ring in reverse
    ("\M-Y" yank-pop-forwards) ; M-Y (Meta-Shift-Y)
    ;; select all
    ("\C-c\C-a" mark-whole-buffer)
    ;; rotate through my color theme list
    ;; ("\C-ct" rotate-through-themes)

    ;; unity logs
    ([f5] (lambda () (interactive) (watch-log-file
				    "~/Library/Logs/Unity/Editor.log")))
    ([C-f5] (lambda () (interactive) (watch-log-file
				      "~/Library/Logs/Unity/Player.log")))
    ;; unity/algoterranean interpreter
    ([C-f11] (lambda () (interactive) (watch-log-file unity-interpreter-log)))
    
    ;; weblogger
    ([C-f12] htmlize-for-blog)
    ([f12] weblogger-start-entry)
    ;; shortcuts to mainipulate window sizes
    ([?\C-c ?\C--] shrink-window)
    ([?\C-c ?\C-=] enlarge-window)
    ([?\C-c ?\M--] shrink-window-horizontally)
    ([?\C-c ?\M-+] enlarge-window-horizontally)
    ;; expand/contract region
    ([?\C-=] er/expand-region)
    ([?\C-+] er/contract-region)
    ;; move the current line of text up or down
    ("\C-c\C-p" move-line-up)
    ("\C-c\C-n" move-line-down)

    ;; notes:
    ;; C-u C-x =  (
    ;; C-c x is reserved for users (where x is a letter)
    ;; C-c followed by a control character is reserved for major modes
    ;;
    ))

;; ;; better buffer movement
;; (global-set-key (kbd "S-<right>") 'buffer-stack-up)
;; (global-set-key (kbd "S-<left>") 'buffer-stack-down)

;; disable help/manuals as F1
(global-unset-key [f1])
(global-unset-key [f11])

;; set all global keybindings
(mapcar (lambda (x) (global-set-key (car x) (car (cdr x))))
	my-global-keybindings)



;;======================
;; CUSTOM FUNCTIONS
;;======================

(defun toggle-full-screen-cross-platform ()
  (interactive)
  (cond ((eq system-type 'darwin) (toggle-frame-fullscreen))
	((eq system-type 'windows-nt) (shell-command "emacs_fullscreen.exe"))))



;; (defun starter-kit-pretty-lambdas ()
;;   (font-lock-add-keywords
;;    nil `(("(\\(lambda\\>\\)"
;;           (0 (progn (compose-region (match-beginning 1) (match-end 1)
;;                                     ,(make-char 'greek-iso8859-7 107))
;;                     nil))))))
;; (starter-kit-pretty-lambdas)




;; awesome. NOTE: modes need to be added here manually
;; auto-indent pasted code
(defadvice yank (after indent-region activate)
  (if (member major-mode
              '(emacs-lisp-mode scheme-mode lisp-mode c-mode c++-mode
				objc-mode latex-mode plain-tex-mode python-mode boo-mode))
      (indent-region (region-beginning) (region-end) nil)))

(defadvice yank-pop (after indent-region activate)
  (if (member major-mode
              '(emacs-lisp-mode scheme-mode lisp-mode c-mode c++-mode
				objc-mode latex-mode plain-tex-mode python-mode boo-mode))
      (indent-region (region-beginning) (region-end) nil)))



;; from http://www.mygooglest.com/fni/dot-emacs.html
;; move (shift) a line of text up or down like you would do in Eclipse
;; pressing `M-up' (or `M-down')
(defun move-line (n)
  "Move the current line up or down by N lines."
  (interactive "p")
  (let ((col (current-column))
        start
        end)
    (beginning-of-line)
    (setq start (point))
    (end-of-line)
    (forward-char)
    (setq end (point))
    (let ((line-text (delete-and-extract-region start end)))
      (forward-line n)
      (insert line-text)
      ;; restore point to original column in moved line
      (forward-line -1)
      (forward-char col))))

(defun move-line-up (n)
  "Move the current line up by N lines."
  (interactive "p")
  (move-line (if (null n) -1 (- n))))

(defun move-line-down (n)
  "Move the current line down by N lines."
  (interactive "p")
  (move-line (if (null n) 1 n)))




(defun watch-log-file (fn)
  (interactive)
  ;;(split-window-horizontally 50)
  (find-file fn)
  (auto-revert-mode t))

(defun htmlize-for-blog  ()
  (interactive)
  (let ((htmlize-output-type 'inline-css))
    (my-htmlize-region (region-beginning) (region-end))))


;; Mostly from http://ruslanspivak.com/2007/08/18/htmlize-your-erlang-code-buffer/
;; The output of this can be copied in its entirety and pasted directly
;; to a blog post, for example.
(defun my-htmlize-region (beg end)
  "Htmlize region and put into <pre> tag style that is left in <body>
tag, change the font-size to 10pt, and copy the results to the
clipboard (kill ring)."
  (interactive "r")
  (let* ((buffer-faces (htmlize-faces-in-buffer))
         (face-map (htmlize-make-face-map (adjoin 'default buffer-faces)))
         (pre-tag (format
                   "<pre style=\"%s font-size: 10pt; \">"
                   (mapconcat #'identity (htmlize-css-specs
                                          (gethash 'default face-map)) " ")))
         (htmlized-reg (htmlize-region-for-paste beg end)))
    (switch-to-buffer-other-window "*htmlized output*")
					; clear buffer
    (kill-region (point-min) (point-max))
					; set mode to have syntax highlighting
    (nxml-mode)
    (save-excursion
      (insert htmlized-reg))
    (while (re-search-forward "<pre>" nil t)
      (replace-match pre-tag nil nil))
    (goto-char (point-min))
    ;; copy results to clipboard and kill the working buffer
    (clipboard-kill-ring-save (point-min) (point-max))
    (kill-buffer)
    ))



;; only works on osx
(defun get-number-of-monitors ()
  (interactive)
  (do-applescript (format "
tell application \"System Events\"
  set _desktops to a reference to every desktop
end tell
set x to count of _desktops
")))

;; (global-set-key [f11]
;; (lambda ()
;;  (interactive)
;;  (toggle-frame-fullscreen)
;;  (if (> (get-number-of-monitors) 1)
;;      (progn
;; (modify-frame-parameters (make-frame) '((top + 10) (left + 1920) (width . 144) (height . 80) (vertical-scroll-bars . nil)))
;; (modify-frame-parameters (make-frame) '((top + -210) (left + -1051) (width . 148) (height . 105) (vertical-scroll-bars . nil))))
;;    )))



(defun rotate-through-themes ()
  (interactive)
  (setq my-theme-list-pos (% (+ 1 my-theme-list-pos) (length my-theme-list)))
  (let ((next-theme (car (read-from-string
			  (car (split-string (symbol-name (nth (% (+ 1 my-theme-list-pos)
								  (length my-theme-list)) my-theme-list)) "-theme"))))))
    (progn
      (disable-theme my-current-theme)
      (load-theme next-theme t)
      (message "Loaded theme %s." next-theme)
      (setq my-current-theme next-theme)
      )))


(defun what-face (pos)
  (interactive "d")
  (let ((face (or (get-char-property (point) 'read-face-name)
                  (get-char-property (point) 'face))))
    (if face (message "Face: %s" face) (message "No face at %d" pos))))




(defconst animate-n-steps 3)
(defun emacs-reloaded ()
  (animate-string (concat ";; Initialization successful, welcome to "
			  (substring (emacs-version) 0 16)
			  ".")
		  0 0)
  (newline-and-indent)  (newline-and-indent))
;; (add-hook 'after-init-hook 'emacs-reloaded)


(defun search-proj-recursively ()
  (interactive)
  (rgrep (read-from-minibuffer "grep: ")
	 (read-from-minibuffer "filter: " "*.boo")
	 (read-from-minibuffer "dir: " (file-name-directory (or load-file-name
								buffer-file-name)))
	 ))

(defun search-proj-directory ()
  (interactive)
  (grep (concat "grep -nH -e "
		(read-from-minibuffer "grep: ")
		" "
		(read-from-minibuffer "dir: " (file-name-directory (or load-file-name
								       buffer-file-name)))
		(read-from-minibuffer "filter: " "*.boo")
		)))

(defun goto-match-paren (arg)
  "Go to the matching parenthesis if on parenthesis AND last command
is a movement command, otherwise insert %.
vi style of % jumping to matching brace."
  (interactive "p")
  (message "%s" last-command)
  (if (not (memq last-command '(set-mark cua-set-mark goto-match-paren
					 down-list up-list end-of-defun
					 beginning-of-defun backward-sexp forward-sexp backward-up-list forward-paragraph
					 backward-paragraph end-of-buffer beginning-of-buffer backward-word forward-word
					 mwheel-scroll backward-word forward-word mouse-start-secondary
					 mouse-yank-secondary
					 mouse-secondary-save-then-kill move-end-of-line move-beginning-of-line
					 backward-char
					 forward-char scroll-up scroll-down scroll-left scroll-right
					 mouse-set-point next-buffer previous-buffer next-line previous-line)))
      (self-insert-command (or arg 1))
    (cond ((looking-at "\\s\(") (forward-list 1) (backward-char 1))
          ((looking-at "\\s\)") (forward-char 1) (backward-list 1))
          (t (self-insert-command (or arg 1))))))

(defun insert-date (prefix)
  "Insert the current date. With prefix-argument, use ISO format. With
   two prefix arguments, write out the day and month name."
  (interactive "P")
  (let ((format (cond
		 ((not prefix) "%m.%d.%Y")
		 ((equal prefix '(4)) "%Y-%m-%d")
		 ((equal prefix '(16)) "%A, %d. %B %Y")))
	(system-time-locale "en_US"))
    (insert (format-time-string format)))) ;; test 01.15.2013

(defun yank-pop-forwards (arg)
  (interactive "p")
  (yank-pop (- arg)))

;; always compile .emacs
(defun autocompile nil
  "compile itself if ~/.emacs"
  (interactive)
  (require 'bytecomp)
  (if (string= (buffer-file-name) (expand-file-name (concat
						     default-directory ".emacs")))
      (byte-compile-file (buffer-file-name))))
;; (add-hook 'after-save-hook 'autocompile) ;; disable for now... random end of file problems...



;;======================
;; OSX-SPECIFIC
;;======================
(cond ((eq system-type 'darwin)
       ;; (set-frame-font "Consolas for Powerline FixedD-12")
       (set-frame-font "Source Code Pro-12")
       (setq mac-option-modifier 'super
	     mac-command-modifier 'meta
	     ns-use-native-fullscreen nil    ;; so nice
	     )))




;;======================
;; WIN-SPECIFIC
;;======================
(cond ((eq system-type 'windows-nt)
       (setenv "GS_LIB" "c:/gs/gs8.15/lib;c:/gs/fonts")
       ;; (set-frame-font "Consolas for Powerline FixedD-10")
       (set-frame-font "Source Code Pro-9")
       (add-to-list 'exec-path "c:\\Program Files (x86)\\Mozilla Firefox/")
       (setq ps-lpr-command "c:/gs/gs8.15/bin/gswin32c.exe"
	     ps-lpr-switches '("-q" "-dNOPAUSE" "-dBATCH" "-sDEVICE=mswinpr2")
	     ps-printer-name t)
       (server-start)))

;; ;; fixes issue with python.el not loading
;; (add-to-list 'load-path "~/.emacs.d/")
;; (let ((default-directory "~/.emacs.d/"))
;;   (setq load-path
;; (append
;; (let ((load-path (copy-sequence load-path)))
;;   (normal-top-level-add-subdirs-to-load-path))
;; load-path)))
;; (let ((default-directory "~/bin/emacs-24.2/"))
;;   (setq load-path
;; (append
;; (let ((load-path (copy-sequence load-path)))
;;   (normal-top-level-add-subdirs-to-load-path))
;; load-path)))


;;======================
;; MODES
;;======================
;; (exec-path-from-shell-copy-env "PYTHONPATH")

;; eshell
;; sync up various path and other env variables from the actual shell
(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize))
(exec-path-from-shell-copy-env "DYLD_FALLBACK_LIBRARY_PATH")
(exec-path-from-shell-copy-env "MANPATH")
(exec-path-from-shell-copy-env "INFOPATH")
(exec-path-from-shell-copy-env "MONO_FRAMEWORK_PATH")
(exec-path-from-shell-copy-env "BOOC_EXEC")


;; helm pcomplete mode
;; (add-hook 'eshell-mode-hook
;;           #'(lambda ()
;;               (define-key eshell-mode-map
;;                 [remap eshell-pcomplete]
;;                 'helm-esh-pcomplete)))

;; enable history in helm
(add-hook 'eshell-mode-hook
	  #'(lambda ()
	      (define-key eshell-mode-map
		(kbd "M-p")
		'helm-eshell-history)))


;; c-mode
(defun my-c-mode-common-hook ()
  ;; my customizations for all of c-mode, c++-mode, objc-mode, java-mode
  ;; (c-set-offset 'substatement-open 0)
  (setq c++-tab-always-indent t
	c-basic-offset 4                  ;; Default is 2
	c-indent-level 4                  ;; Default is 2
	;; (setq c-auto-newline t)
	tab-stop-list '(4 8 12 16 20 24 28 32 36 40 44 48 52 56 60)
	tab-width 4
	indent-tabs-mode t)  ; use spaces only if nil
  )

(add-hook 'c-mode-common-hook 'my-c-mode-common-hook)



;; org-mode
(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))
(add-to-list 'auto-mode-alist '("\\TODO\\'" . org-mode))
(setq org-todo-keywords
      '((sequence "TODO(!)" "STARTED(!)" "|" "DONE(!)" "CANCELED(!)")))
(add-hook 'org-mode-hook (lambda ()
			   (local-set-key [C-return] 'next-multiframe-window)))
(setq org-src-fontify-natively t
      org-log-done t
      org-src-lang-modes '(("ocaml" . tuareg)
			   ("elisp" . emacs-lisp)
			   ("ditaa" . artist)
			   ("asymptote" . asy)
			   ("dot" . fundamental)
			   ("boo" . boo)))



;; nxml-mode
(setq auto-mode-alist (cons '("\.xaml$" . nxml-mode) auto-mode-alist))
;; (define-key nxml-mode-map (kbd "\C-c \C-e") 'weblogger-toggle-edit-body)


;; iswitchb-mode
;; use arrow keys to navigate iswitchb minibuffer
(defun iswitchb-local-keys ()
  (mapc (lambda (K)
	  (let* ((key (car K)) (fun (cdr K)))
	    (define-key iswitchb-mode-map (edmacro-parse-keys key) fun)))
	'(("<right>" . iswitchb-next-match)
	  ("<left>"  . iswitchb-prev-match)
	  ("<up>"    . ignore             )
	  ("<down>"  . ignore             ))))
(iswitchb-mode t)
(icomplete-mode t)
(add-hook 'iswitchb-define-mode-map-hook 'iswitchb-local-keys)



;; powershell-moded
(add-to-list 'auto-mode-alist '("\\.ps1\\'" . powershell-mode)) ;; mode for editing powershell scripts


;; magit and git
(add-hook 'magit-mode-hook
	  (lambda ()
	    (define-key magit-mode-map [f9] 'delete-window)
	    (define-key magit-mode-map [C-return] 'other-window)
	    ))

(if (eq system-type 'windows-nt)
    (progn
      (setq magit-git-executable "C:\\Program Files (x86)\\Git\\bin\\git.exe")
      (add-to-list 'exec-path "C:\\Program Files (x86)\\Git\\bin")
      (setenv "PATH" (concat "C:\\Program Files (x86)\\Git\\bin;"
			     (getenv "PATH")))))

;; disable vc-git dues to slowness
(remove-hook 'find-file-hooks 'vc-find-file-hook)
(setq vc-handled-backends nil)
;; (setq magit-emacscli


;; csharp-mode
(setq auto-mode-alist (append '(("\\.cs$" . csharp-mode)) auto-mode-alist))
(add-hook 'csharp-mode-hook
	  (lambda ()
	    (c-set-style "linux")
	    ;; (c-set-offset substatement-open 0)
	    (setq c-basic-offset 4
		  c-auto-newline t)
	    (local-set-key (kbd "{") 'c-electric-brace)))



;; sgml-mode
(setq sgml-warn-about-undefined-elements nil)
(setq sgml-warn-about-undefined-entities nil)


;; gnus
;; sort so that newest entries in an RSS feed show up at the top
(setq gnus-thread-sort-functions
      '((not gnus-thread-sort-by-number)))
;; gnus-thread-sort-by-subject
;; (not gnus-thread-sort-by-total-score)))
(if (eq system-type 'windows-nt)
    (setq gnus-button-url 'browse-url-generic
	  browse-url-generic-program "firefox.exe"
	  browse-url-browser-function gnus-button-url))
(global-set-key (kbd "C-c C-o") 'browse-url)


;; always kill gnus before quitting emacs
(add-hook 'kill-emacs-hook
	  (lambda ()
	    (if (get-buffer "*Group*")
		(gnus-group-exit))))




;; dired

(defun sof/dired-sort ()
  "Dired sort hook to list directories first."
  (save-excursion
    (let (buffer-read-only)
      (forward-line 2) ;; beyond dir. header
      (sort-regexp-fields t "^.*$" "[ ]*." (point) (point-max))))
  (and (featurep 'xemacs)
       (fboundp 'dired-insert-set-properties)
       (dired-insert-set-properties (point-min) (point-max)))
  (set-buffer-modified-p nil))

(add-hook 'dired-after-readin-hook 'sof/dired-sort)

(setq dired-details-hidden-string "")

;; ignore .meta files with dired+
(setq-default dired-omit-files-p t) ;; this is buffer-local
(setq dired-omit-files "\\.meta$")
;; (setq dired-omit-files (concat dired-omit-files "\\|\\.meta$"))


;; python-mode
(add-to-list 'auto-mode-alist '("\\.py\\'" . python-mode))


;; js2-mode
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))
(setq js2-use-font-lock-faces t)

;; enables undo/redo with window setups. Use C-c left and C-c right
(winner-mode 1)

;; cg mode
;; (autoload 'cg-mode "cg" nil t)
;; (load "~/.emacs.d/cg-mode.el")
;; (add-to-list 'auto-mode-alist '("\\.shader$" . cg-mode))
;; (add-to-list 'auto-mode-alist '("\\.cginc$" . cg-mode))

;; nxhtml mode
;; (load "~/.emacs.d/nxhtml/autostart.el")
;; (add-to-list 'auto-mode-alist '("\\.html$" . nxhtml-mumamo-mode))

;; boo mode
(setq boo-custom-macros '("client" "server" "standalone" "not_server"
			  "not_client" "not_standalone"))
(add-to-list 'auto-mode-alist '("\\.boo$" . boo-mode))
(add-to-list 'auto-mode-alist '("\\.boo.macro$" . boo-mode))


(require 'boo-mode)

;; weblogger
;; (weblogger-select-configuration)


;; popwin-mode
;; (popwin-mode 1)

;; session
;; (add-hook 'after-init-hook 'session-initialize)

;; workgroups
(setq wg-morph-on nil)
(setq wg-default-session-file "~/.emacs.d/.emacs_workgroups")
(setq wg-prefix-key (kbd "C-z")) ;; TODO move to keybindings up top?
(setq wg-emacs-exit-save-behaviour 'ask)
(setq wg-workgroups-mode-exit-save-behavior 'ask)

(workgroups-mode 1)

;; TODO add support to workgroups2 for handling multiple sessions
;; ask if we want to save the workgroup session first. bind this to C-x C-c
(defun revert-workgroups-first ()
  (interactive)
  (progn
    (wg-revert-all-workgroups)
    (save-buffers-kill-terminal)))

;; (add-hook 'kill-emacs-hook
;;  (lambda ()
;;    (if (not (yes-or-no-p "Save current workgroup session?"))
;; (wg-revert-all-workgroups))))



;; nav mode
(nav-disable-overeager-window-splitting)

;; applescript mode
(add-to-list 'auto-mode-alist '("\\.\\(applescri\\|sc\\)pt\\'" . apples-mode))

;; pianobar (command line player for Pandora)
(add-to-list 'exec-path "/usr/local/bin")
(autoload 'pianobar "pianobar" nil t)

;;
(add-to-list 'auto-mode-alist '("\\.emacs.home" . emacs-lisp-mode))
(add-to-list 'auto-mode-alist '("\\.emacs.work" . emacs-lisp-mode))

;; eshell
(setq eshell-path-env (getenv "PATH"))


;; helm
(setq helm-idle-delay 0.1)
(setq helm-input-idle-delay 0.1)
(if (eq system-type 'darwin)
    (setq helm-locate-command "mdfind -name %.0s %s")) ;; osx only
(loop for ext in '("\\.swf$" "\\.elc$" "\\.pyc$" "\\.meta$")
      do (add-to-list 'helm-boring-file-regexp-list ext))

(defun helm-bookmark-helper ()
  (interactive)
  (if (eq at-work t)
      (helm-firefox-bookmarks)
    (helm-chrome-bookmarks)))


;;
(defun toggle-nav-or-nicklist ()
  (interactive)
  (if (string= (message "%s" major-mode) "erc-mode")
      (erc-nicklist)
    (nav-toggle)))


;; flycheck
;; (add-hook 'after-init-hook 'global-flycheck-mode)
;; (load-file "~/.emacs.d/unity-csharp.el")

;; from https://github.com/cantsin/dotemacs/blob/master/setup-unity.el
;; (defun unity-find-project-sln-from-dir (dir-path)
;;   "Walks up the tree from DIR-PATH, trying to find an sln.
;; If it finds one, returns it, else nil."
;;   (if (and (file-exists-p dir-path) (not (string= dir-path "/")))
;;       (let* (
;; 	     (dir-name (file-name-base (directory-file-name dir-path)))
;; 	     (sln-path (concat (file-name-as-directory dir-path)
;; 			       (concat dir-name ".sln"))))
;;         (if (file-exists-p sln-path)
;;             sln-path
;;           (unity-find-project-sln-from-dir (expand-file-name (concat
;; 							      dir-path "/.."))) ;; keep going down stack
;;           ))
;;     nil))

;; (defun unity-find-project-dir-from-file (project-file)
;;   (file-name-directory (unity-find-project-sln-from-dir
;;                         (file-name-directory buffer-file-name))))

;; (defun buffer-has-unity-sln-parent ()
;;   "Find the sln file that defines this project."
;;   (unity-find-project-sln-from-dir (file-name-directory buffer-file-name)))

;; (defun restore-root-if-necessary (project-root err)
;;   (let* (
;;          (error-filename (flycheck-error-filename err))
;;          (is-rooted (string-match (concat "^" project-root ".*")
;; 				  error-filename)))
;;     (if (not is-rooted)
;;         (flycheck-error-new
;;          :filename (concat project-root error-filename)
;;          :line (flycheck-error-line err)
;;          :column (flycheck-error-column err)
;;          :message (flycheck-error-message err)
;;          :level (flycheck-error-level err))
;;       err)))

;; (defun parse-patterns-and-restore-path (output checker _buffer)
;;   "the file names listed in the error output from unity doesn't include
;; the project root. flycheck can't find the files if it's not an absolute path."
;;   (let* (
;;          (trimmed-output
;;           (mapconcat 'identity
;;                      (-map
;;                       (lambda (line) (substring line 0 (min (length
;; 							     line) 1000)))
;;                       (split-string output "\n"))
;;                      "\n"))
;;          (raw-parse-results (flycheck-parse-with-patterns
;; 			     trimmed-output checker _buffer))
;;          (project-root (unity-find-project-dir-from-file buffer-file-name)))
;;     (-map
;;      (lambda (err) (restore-root-if-necessary project-root err))
;;      raw-parse-results)))





(defun algo-flycheck-find-makefile (dir-path)
  "Follow the directory structure upwards until we find a makefile. If we do not, return nil."
  (let ((potential-makefile (concat dir-path "makefile"))
	(potential-makefile2 (concat dir-path "Makefile"))
	(parent-dir (expand-file-name (concat dir-path "/.."))))

    (cond ((file-exists-p potential-makefile) potential-makefile)
	  ((file-exists-p potential-makefile2) potential-makefile2)
	  ((and (file-directory-p parent-dir)
		(not (string= parent-dir "/")))
	   (algo-flycheck-find-makefile parent-dir))
	  (t nil))))


(flycheck-define-checker algo-project-flychecker
  "Flychecker setup for boo files that also have a makefile associated with them."
  :command ("make" "-f" (eval (algo-flycheck-find-makefile (file-name-directory buffer-file-name))) "FLYCHECK=yes")
  :error-patterns
  ((error line-start (file-name) "(" line "," column "): error "
	  (message) line-end)
   (error line-start (file-name) "(" line "," column "): BCE"
	  (message) line-end)
   (warning line-start (file-name) "(" line "," column "): warning "
	    (message) line-end)   
   (warning line-start (file-name) "(" line "," column "): BCW"
	    (message) line-end))
  :modes (csharp-mode boo-mode)
  :predicate (lambda () (algo-flycheck-find-makefile (file-name-directory buffer-file-name)))
  (setq flycheck-check-syntax-automatically '(save mode-enabled)))
(add-to-list 'flycheck-checkers 'algo-project-flychecker 'append)
(setq flycheck-check-syntax-automatically '(save mode-enabled))
(global-flycheck-mode)


(defun helm-unity-action (candidate)
  (let ((action (symbol-name candidate))
	(unity-command (concat "/Applications/Unity/Unity.app/Contents/MacOS/Unity "
			       "-batchmode " 
			       "-logFile " 
			       "-projectPath ~/Cloud/projects/algoterranean/unity/ "
			       "-quit " 
			       "-executeMethod Algoterranean.Networking.NetworkMenus.")))
    (cond ((string= action "Flycheck") 
	   (flycheck-buffer))
	  ((string= action "Build Library") 
	   (shell-command "make -f ~/Cloud/projects/algoterranean/src/Algoterranean/makefile&"))
	  ((string= action "Build Standalone") 
	   (shell-command (concat unity-command "BuildSinglePlayer&")))
	  ((string= action "Build Server") 
	   (shell-command (concat unity-command "BuildServer&")))
	  ((string= action "Build Client") 
	   (shell-command (concat unity-command "BuildClient&"))))))




(defun helm-unity-execute ()
  (interactive)
  (helm :sources '(((name . "Commands")
		   (candidates lambda nil
			       '("Flycheck" "Build Library" "Build Standalone" "Build Client" "Build Server"))
		   (match . helm-files-match-only-basename)
		   (type . command)
		   (adjust)
		   (recenter)
		   (action . (("Execute" . helm-unity-action)))
		   ;; (default-action . (("Execute" . (lambda (candidate) (message candidate)))))
		   ;; (persistent-action . (("Execute" . (lambda (candidate) (message candidate)))))
		   )
		   ((name . "Project Files")
		    (candidates lambda nil 
				(append (files-in-below-directory "/Users/p/Cloud/projects/algoterranean/src/Algoterranean/" "\\.boo$\\|\\.cs$")
					(files-in-below-directory "/Users/p/Cloud/projects/algoterranean/unity/" "\\.boo$\\|\\.cs$")))
		    (match . helm-files-match-only-basename)
		    (type . file)
		    (recenter)))
	
	:buffer "*helm-unity*"))


(defun files-in-below-directory (directory fregexp)
  "List the files in DIRECTORY and in its sub-directories that match the regexp FREGEXP"
  (let (el-files-list
	(current-directory-list
	 (directory-files-and-attributes directory t)))
    ;; while we are in the current directory
    (while current-directory-list
      (cond
       ;; check to see whether filename matches the regexp
       ;; and if so, append its name to a list.
	 ((string-match fregexp (car (car current-directory-list)))
	  ;; ((equal ".boo" (substring (car (car current-directory-list)) -4))
	  (setq el-files-list
		(cons (car (car current-directory-list)) el-files-list)))
       ;; check whether filename is that of a directory
	 ((eq t (car (cdr (car current-directory-list))))
	;; decide whether to skip or recurse
	(if
	    (equal "."
		   (substring (car (car current-directory-list)) -1))
	    ;; then do nothing since filename is that of
	    ;;   current directory or parent, "." or ".."
	    ()
	  ;; else descend into the directory and repeat the process
	  (setq el-files-list
		(append
		 (files-in-below-directory
		  (car (car current-directory-list)) fregexp)
		 el-files-list)))))
      ;; move to the next filename in the list; this also
      ;; shortens the list so the while loop eventually comes to an end
      (setq current-directory-list (cdr current-directory-list)))
    ;; return the filenames
    el-files-list))

;; (add-to-list 'auto-mode-alist '(".algo.boo$" . boo-mode))
;; (add-to-list 'auto-mode-alist '(".algo.py$" . python-mode))
;; (add-to-list 'auto-mode-alist '(".algo.scheme$" . scheme-mode))

;; SWANK:*COMMUNICATION-STYLE*

;; (slime-connect "127.0.0.1" "13000"

;; mono stuff
(setq auto-mode-alist (cons '("\.exe.config$" . nxml-mode) auto-mode-alist))
(setq tags-file-name ".tags")

;; projectile mode
(projectile-global-mode)
(setq projectile-enable-caching t)


(auto-revert-mode t)
