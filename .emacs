;; ~/.emacs, ~/.emacs.el, or ~/.emacs.d/init.el

;; emacs-major-version
;; emacs-minor-version
;; system-name
;; user-emacs-directory
;; user-init-file


;;======================
;; SETUP
;;======================

(require 'cl)

(if (file-exists-p "~/.emacs.d/.emacs.work")
    (load-file "~/.emacs.d/.emacs.work")
  (progn
    (setq at-work nil)
    (setq at-home t)))


(require 'package)
(setq package-archives
      '(("gnu" . "http://elpa.gnu.org/packages/")
	("marmalade" . "http://marmalade-repo.org/packages/")
	("melpa" . "http://melpa.milkbox.net/packages/")))
(package-initialize)


(defvar my-package-list '(anzu
			  adaptive-wrap
			  crosshairs
			  csharp-mode 
			  django-mode
			  display-theme
			  gist 
			  htmlfontify 
			  htmlize 
			  fringe-current-line
			  itail 
			  magit 
			  markdown-mode
			  powerline
			  python-mode
			  rainbow-mode))

(defvar my-require-list '(adaptive-wrap
			  edmacro
			  display-theme
			  magit
			  org
			  powerline
			  python-mode
			  vc))

(defvar my-theme-list-pos 0)
(defvar my-theme-list '(afternoon-theme
			ample-theme
			ample-zen-theme
			anti-zenburn-theme
			assemblage-theme
			;; base16-theme
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
			;;github-theme
			grandshell-theme
			gruvbox-theme
			hemisu-theme
			heroku-theme
			inkpot-theme
			ir-black-theme
			jujube-theme
			late-night-theme
			leuven-theme
			moe-theme
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
			;;sublime-theme
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

;; require anything needed below
(mapc 'require my-require-list)



;;======================
;; PREFERENCES
;;======================

;; display and visuals
(setq theme-load-from-file nil)
(load-theme 'zenburn t)
(defvar my-current-theme 'zenburn)
(set-frame-font "Consolas for Powerline FixedD-10")
(menu-bar-mode -1)
(scroll-bar-mode -1)
(tool-bar-mode -1)
(global-font-lock-mode t)
(show-paren-mode t)
(setq tab-width 4)
(setq transient-mark-mode t) ;; enable visual feedback on selections
(column-number-mode t)
(linum-mode t)
(setq frame-title-format "%b - emacs")


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
(setq inhibit-default-init t)
(setq default-directory "~/")
(setq inhibit-startup-screen t)
(setq initial-scratch-message nil)
(setq ecb-tip-of-the-day nil)

;; quitting
(setq confirm-kill-emacs 'yes-or-no-p)

;; backups and autosaves
(setq backup-inhibited t)
(setq make-backup-files -1)
(auto-save-mode nil)
(setq auto-save-default nil)

;; debugging
(setq stack-trace-on-error t)
(setq debug-on-error t)

;; misc
(put 'overwrite-mode 'disabled t)
(if (load "mwheel" t) (mwheel-install))
(add-hook 'comint-output-filter-functions 'comint-watch-for-password-prompt) ;; don't echo passwords
(when window-system
  (global-unset-key "\C-z")) ; iconify-or-deiconify-frame (C-x C-z)
(put 'narrow-to-region 'disabled nil)

(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(prefer-coding-system 'utf-8)



;;======================
;; KEYBINDINGS
;;======================
(defvar my-global-keybindings '()) ;; TODO: write a function to define keybindings based on this list and then another function to test the user to see if they remember the keybindings.
      
;; useful buffer commands
(global-set-key "\C-xB" 'bury-buffer)
(global-set-key "\C-xE" 'apply-macro-to-region-lines)
(global-set-key "\C-xI" 'insert-buffer)

(global-set-key "\C-cg" 'goto-line)
(global-set-key "\C-cG" 'goto-char)
(global-set-key "\C-cw" 'delete-region) ; ala C-w and M-C-w

;; window and frame navigation
(global-set-key [C-return] 'other-window)
(global-set-key [C-M-return] 'other-frame)
;; comment/uncomment
(global-set-key (kbd "C-c c") 'comment-region)
(global-set-key (kbd "C-c u") 'uncomment-region)
;; shortcuts to mainipulate window sizes
(global-set-key (kbd "C-c C--") 'shrink-window)
(global-set-key (kbd "C-c C-=") 'enlarge-window)
(global-set-key (kbd "C-c M--") 'shrink-window-horizontally)
(global-set-key (kbd "C-c M-=") 'enlarge-window-horizontally)
;; global recursive grep from a directory
(global-set-key [C-f10] 'search-proj-recursively)
;; grep the current working directory
(global-set-key [f10] 'search-proj-directory)
;; navigate to the matching paren/brace
(global-set-key (kbd "%") 'goto-match-paren)
;; join line shortcut
(global-set-key (kbd "C-j") 'join-line)
;; google-code nav mode (like "directory" view in IDEs)
(global-set-key [f8] 'nav-toggle)
;; 
(global-set-key (kbd "C-=") 'er/expand-region)
(global-set-key (kbd "C-+") 'er/contract-region)
;; magit toggle
(global-set-key [f9] 'magit-status)
;; alternative way to use Meta-X (with C thrown for fat-fingering)
(global-set-key "\C-x\C-m" 'execute-extended-command)
(global-set-key "\C-c\C-m" 'execute-extended-command)
;; recenter text on screen (vertically)
(global-set-key '[C-l] '(recenter 1))
;; insert today's date in MM.DD.YYYY format
(global-set-key (kbd "C-c d") 'insert-date)
;; 
(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)
;; cycle through kill ring in reverse
(global-set-key "\M-Y" 'yank-pop-forwards) ; M-Y (Meta-Shift-Y)
;; select all
(global-set-key "\C-c\C-a" 'mark-whole-buffer)
;; rotate through my color theme list
(global-set-key "\C-ct" 'rotate-through-themes)



;;======================
;; CUSTOM FUNCTIONS
;;======================
(defun rotate-through-themes ()
  (interactive)
  (setq my-theme-list-pos (% (+ 1 my-theme-list-pos) (length my-theme-list)))
  (let ((next-theme (car (read-from-string
			 (car (split-string (symbol-name (nth (% (+ 1 my-theme-list-pos) 
								 (length my-theme-list)) my-theme-list)) "-theme"))))))
    (progn
      (disable-theme my-current-theme)
      (load-theme next-theme t)
      (setq my-current-theme next-theme)
      )))







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
	 (read-from-minibuffer "dir: " (file-name-directory (or load-file-name buffer-file-name)))
	 ))

(defun search-proj-directory ()
  (interactive)
  (grep (concat "grep -nH -e " 
		(read-from-minibuffer "grep: ") 
		" " 
		(read-from-minibuffer "dir: " (file-name-directory (or load-file-name buffer-file-name)))
		(read-from-minibuffer "filter: " "*.boo")
		)))

(defun goto-match-paren (arg)
  "Go to the matching parenthesis if on parenthesis AND last command is a movement command, otherwise insert %.
vi style of % jumping to matching brace."
  (interactive "p")
  (message "%s" last-command)
  (if (not (memq last-command '(set-mark cua-set-mark goto-match-paren down-list up-list end-of-defun
                                beginning-of-defun backward-sexp forward-sexp backward-up-list forward-paragraph
                                backward-paragraph end-of-buffer beginning-of-buffer backward-word forward-word
                                mwheel-scroll backward-word forward-word mouse-start-secondary mouse-yank-secondary
                                mouse-secondary-save-then-kill move-end-of-line move-beginning-of-line backward-char
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
  (if (string= (buffer-file-name) (expand-file-name (concat default-directory ".emacs")))
      (byte-compile-file (buffer-file-name))))
(add-hook 'after-save-hook 'autocompile)




;;======================
;; OSX-SPECIFIC
;;======================
(if (eq system-type 'darwin)
    (progn
      (setq mac-option-modifier 'super)
      (setq mac-command-modifier 'meta)))


;;======================
;; WIN-SPECIFIC
;;======================
(cond ((eq system-type 'windows-nt)
       (setenv "GS_LIB" "c:/gs/gs8.15/lib;c:/gs/fonts")
       (setq ps-lpr-command "c:/gs/gs8.15/bin/gswin32c.exe")
       (setq ps-lpr-switches '("-q" "-dNOPAUSE" "-dBATCH" "-sDEVICE=mswinpr2"))
       (setq ps-printer-name t)

       (add-to-list 'exec-path "c:\\Program Files (x86)\\Mozilla Firefox/")
       (server-start)))

;; ;; fixes issue with python.el not loading
;; (add-to-list 'load-path "~/.emacs.d/")
;; (let ((default-directory "~/.emacs.d/"))
;;   (setq load-path
;; 	(append
;; 	 (let ((load-path (copy-sequence load-path)))
;; 	   (normal-top-level-add-subdirs-to-load-path))
;; 	 load-path)))
;; (let ((default-directory "~/bin/emacs-24.2/"))
;;   (setq load-path
;; 	(append
;; 	 (let ((load-path (copy-sequence load-path)))
;; 	   (normal-top-level-add-subdirs-to-load-path))
;; 	 load-path)))


;;======================
;; WORK-SPECIFC
;;======================



;;======================
;; HOME-SPECIFIC
;;======================

(if at-home
    ())

;;======================
;; MODES
;;======================

;; c-mode
(defun my-c-mode-common-hook ()
  ;; my customizations for all of c-mode, c++-mode, objc-mode, java-mode
  (c-set-offset 'substatement-open 0)
  (setq c++-tab-always-indent t)
  (setq c-basic-offset 4)                  ;; Default is 2
  (setq c-indent-level 4)                  ;; Default is 2
  ;; (setq c-auto-newline t)
  (setq tab-stop-list '(4 8 12 16 20 24 28 32 36 40 44 48 52 56 60))
  (setq tab-width 4)
  (setq indent-tabs-mode t)  ; use spaces only if nil
)

(add-hook 'c-mode-common-hook 'my-c-mode-common-hook)



;; org-mode
(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))
(add-to-list 'auto-mode-alist '("\\TODO\\'" . org-mode))
(setq org-todo-keywords
      '((sequence "TODO(!)" "STARTED(!)" "|" "DONE(!)" "CANCELED(!)")))
(add-hook 'org-mode-hook (lambda ()
			   (local-set-key [C-return] 'next-multiframe-window)))
(setq org-src-fontify-natively t)
(setq org-log-done t)
(add-to-list 'org-src-lang-modes '("boo" . boo))

;; nxml-mode
(setq auto-mode-alist (cons '("\.xaml$" . nxml-mode) auto-mode-alist))


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
(iswitchb-mode 1)
(add-hook 'iswitchb-define-mode-map-hook 'iswitchb-local-keys)



;; powershell-mode
(add-to-list 'auto-mode-alist '("\\.ps1\\'" . powershell-mode)) ;; mode for editing powershell scripts


;; magit and git
(add-hook 'magit-mode-hook
	  (lambda ()
	    (define-key magit-mode-map [f9] 'delete-window)
	    ))
(if (eq system-type 'windows-nt)
    (progn
      (setq magit-git-executable "C:/Program Files (x86)/Git/bin/git.exe")
      (setq exec-path (add-to-list 'exec-path "C:/Program Files (x86)/Git/bin"))
      (setenv "PATH" (concat "C:\\Program Files (x86)\\Git\\bin;" (getenv "PATH")))))
;; disable vc-git dues to slowness
(remove-hook 'find-file-hooks 'vc-find-file-hook)
(setq vc-handled-backends nil)


;; csharp-mode
(setq auto-mode-alist (append '(("\\.cs$" . csharp-mode)) auto-mode-alist))
(add-hook 'csharp-mode-hook 
	 (lambda ()
	   (c-set-style "linux")
	   (setq c-basic-offset 4)
	   (c-set-offset substatement-open 0)
	   (setq c-auto-newline t)
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




;; dired
(setq ls-lisp-ignore-case t)

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
;; (require 'dired-single)



;; python-mode
(add-to-list 'auto-mode-alist '("\\.py\\'" . python-mode))


