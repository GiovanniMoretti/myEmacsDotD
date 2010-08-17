;================================================================================
;; Image support in orgmode using iimage
;; from http://orgmode.org/worg/org-configs/org-config-examples.php#sec-2_2
;(load-file "~/Documents/emacs/xyz.el")
; (load "iimage")

; (autoload 'iimage-mode         "iimage" "Support Inline image minor mode." t)
; (autoload 'turn-on-iimage-mode "iimage" "Turn on Inline image minor mode." t)

; (add-to-list 'iimage-mode-image-regex-alist
;              (cons (concat "\\[\\[file:\\(~?" iimage-mode-image-filename-regex
;                           "\\)\\]")  1))

;(defun org-toggle-iimage-in-org ()
;  "display images in your org file"
;  (interactive)
;  (if (face-underline-p 'org-link)
;      (set-face-underline-p 'org-link nil)
;      (set-face-underline-p 'org-link t))
;  (iimage-mode))
;;===============================================================================
;(setq iimage-mode-image-search-path (cons "~/Dropbox/org/" 
;                                          iimage-mode-image-search-path))

;; Add in additional downloaded packages
(setq load-path (cons "~/Dropbox/Emacs-config/" load-path))

; And the latest version of orgmode
(setq load-path (cons "~/Dropbox/Emacs-config/orgmode/lisp/" load-path))

; And Color-Theme
;(setq load-path (cons "~/Dropbox/Emacs-config/color-theme-6.6.0/" load-path))

;;(add-to-list 'load-path "/path/to/color-theme.el/file")
;(require 'color-theme)
;(eval-after-load "color-theme"
;  '(progn
;     (color-theme-initialize)
;     (color-theme-hober)))


(custom-set-variables
      ;; custom-set-variables was added by Custom.
      ;; If you edit it by hand, you could mess it up, so be careful.
      ;; Your init file should contain only one such instance.
      ;; If there is more than one, they won't work right.
     '(inhibit-startup-screen t)
     '(org-default-notes-file "~/Dropbox/org/notes.org")
     '(org-fontify-whole-heading-line t)
     '(org-hide-leading-stars t)
     '(org-mobile-directory "~/Dropbox/MobileOrg")

     ; Set to the name of the file where new notes will be stored
     '(org-mobile-inbox-for-pull "~/Dropbox/org/flagged.org")

     '(savehist-mode t nil (savehist))
     '(weblogger-config-alist (quote (("default" "http://www.reflections.co.nanni" "" "1")) 
					z/wordpress/xmlrpc\.php " " Giov))
)

(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(org-level-1 ((t (:inherit outline-1 :foreground "#008800" :weight bold :height 1.3))))
 '(org-level-2 ((t (:inherit outline-2 :foreground "#883333" :weight bold :height 1.15))))
 '(org-level-3 ((t (:inherit outline-3 :foreground "black" :weight bold))))
)

 ; ORGMODE ACTIVATION
     ;; The following lines are always needed.  Choose your own keys.
     (add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))
     (global-set-key "\C-cl" 'org-store-link)
     (global-set-key "\C-ca" 'org-agenda)
     (global-set-key "\C-cb" 'org-iswitchb)

     (setq org-directory "~/Dropbox/org/")

;; What's the difference between remember mode and org-remember?
;;   - see p20 of the short Orgmode guide: http://orgmode.org/orgguide.pdf 
;; REMEMBER-mode configuration

;   (setq remember-data-file "~/Dropbox/org/notes.org")

    (org-remember-insinuate)
    (setq org-default-notes-file (concat org-directory "/notes.org"))
    (define-key global-map "\C-cr" 'org-remember)

    (defun org-display-remember-file ()
       "open and display the default remember file"
       (interactive)
       (find-file org-default-notes-file)
    )

    ; GSM
    (define-key global-map "\C-cn" 'org-display-remember-file)

;; from http://orgmode.org/worg/org-tutorials/orgtutorial_dto.php

(require 'org-install)
(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
(setq org-log-done t)

; (setq org-agenda-files (file-expand-wildcards "~/Dropbox/org/*.org"))

(setq org-agenda-files (list "~/Dropbox/org/work.org"
                             "~/Dropbox/org/home.org"
                             "~/Dropbox/org/todo.org"
                             "~/Dropbox/org/TeachingNotes.org")
)

;--------------------------------------------------------------------
;; from http://www.linuxjournal.com/article/9116
(global-font-lock-mode 1)


(setq org-mobile-files (list "~/Dropbox/org/todo.org"
                             "~/Dropbox/org/home.org"
                             "~/Dropbox/org/home.org"
                             "~/Dropbox/org/giovanni.org"
                             "~/Dropbox/org/electronics.org"
                             "~/Dropbox/org/TeachingNotes.org"
))

;; from http://comments.gmane.org/gmane.emacs.orgmode/20974
     (setq org-agenda-custom-commands
           '(("w" todo "WAITING")
             ("W" todo-tree "WAITING")
             ("u" tags "+boss-urgent"
             ("v" tags-todo "+boss-urgent")
             ("f" occur-tree "\\<FIXME\\>")
             ("h" . "HOME+Name tags searches") ; description for "h" prefix
             ("hl" tags "+home+Lisa")
             ("hp" tags "+home+Peter")
             ("hk" tags "+home+Kim"))))


;; From http://doc.norang.ca/org-mode.html#Refiling

    ; Use IDO for target completion
    (setq org-completion-use-ido t)

  ; Targets include this file and any file contributing to the agenda - up to 5 levels deep
  (setq org-refile-targets (quote ((org-agenda-files :maxlevel . 5) (nil :maxlevel . 5))))

  ; Targets start with the file name - allows creating level 1 tasks
  (setq org-refile-use-outline-path (quote file))

  ; Targets complete in steps so we start with filename, TAB shows the next level of targets etc
  (setq org-outline-path-complete-in-steps t)

  ; Allow refile to create parent tasks with confirmation
  (setq org-refile-allow-creating-parent-nodes (quote confirm))


;;; This was installed by package-install.el.
;;; This provides support for the package system and
;;; interfacing with ELPA, the package archive.
;;; Move this code earlier if you want to reference
;;; packages in your .emacs.
;(when
;    (load
;     (expand-file-name "~/.emacs.d/elpa/package.el"))
;  (package-initialize))

;(require 'org-babel-init)

(load-file "~/Dropbox/Emacs-config/orgmode/lisp/org-exp-blocks.el")
(require 'org-exp-blocks)

;===========================================================================
; Define Giovanni's overriding KEYBOARD SHORTCUTS as global commands
; Using tabs is tricky - see 
;     http://stackoverflow.com/questions/916797/emacs-global-set-key-to-c-tab

   ; EASY Show Next/Prev BUFFER 
   (global-set-key (read-kbd-macro "<C-tab>")           'next-buffer)
   (global-set-key (read-kbd-macro "<C-S-iso-lefttab>") 'previous-buffer)

   ; Define Ctrl/Shift-C as copy (sortof like Windows) - 
   ; as Ctrl/C is too overloaded in Emacs/Orgmode to change.
   (global-set-key (read-kbd-macro "<C-S-c>") 'org-w3m-copy-for-org-mode)

   ; Kill buffer (close it and make screen region vanish)
   (global-set-key (read-kbd-macro "<C-f4>") 'kill-buffer-and-window)

   ; CTRL/Z as undo instead of  (suspend-frame) which minimizes the window
   ;   (global-set-key (read-kbd-macro "<C-z>") 'undo) DOESN'T WORK
   ;   (global-set-key  "\C-z" 'undo)                  THIS IS OK
   (define-key global-map "\C-z" 'undo)           ; AND SO IS THIS


;------------------------------------------------------------------------------------------------

;; Make Ctrl-F4 close the current frame - from http://weitz.de/win/
(global-set-key [\C-S-f4] 'kill-this-buffer)

;; How to I open the Agenda when starting Emacs?	
; (add-hook 'after-init-hook '(lambda () (org-agenda-list 1)))

; I have a bash alias to start emacs with the Agenda open:
; alias org='/usr/bin/emacs --funcall org-agenda-list &'

;; Activate a RECENT FILES list (from http://www.emacswiki.org/emacs/EmacsNiftyTricks)
;; Use Alt-F12 to recentf-open-files
   (require 'recentf)
   (recentf-mode 1)
   (setq recentf-max-saved-items 500)
   (setq recentf-max-menu-items 60)
   (global-set-key [(meta f12)] 'recentf-open-files)

;To make header lines more apparently visible in the buffer, set them
;a background color and change this part of
;‘org-set-font-lock-defaults’, so that the header line is drawn across
;the screen:
;; Headlines
;'("^\\(\\**\\)\\(\\* \\)\\(.*\xa\\)" (1 (org-get-level-face 1))
;	     (2 (org-get-level-face 2)) (3 (org-get-level-face 3)))

; Dragging URLs
; This function uses org-mode support for plain list to facilitate
; dragging URLs from a webbrowser (or other apps) to an org-mode
; buffer:

(defadvice dnd-insert-text (around org-mouse-dnd-insert-text activate)
  (if (eq major-mode 'org-mode)
      (progn
	(cond
	 ;; if this is the end of the line then just insert text here
	 ((eolp)
	  (skip-chars-backward " \t")
	  (kill-region (point) (point-at-eol))
	  (unless (looking-back ":") (insert ":"))
	  (insert " "))

	 ;; if this is the beginning of the line then insert before
	 ((and (looking-at " \\|\t")
	       (save-excursion
		 (skip-chars-backward " \t") (bolp)))
	  (beginning-of-line)
	  (looking-at "[ \t]*")
	  (open-line 1)
	  (indent-to (- (match-end 0) (match-beginning 0)))
	  (insert "+ "))

	 ;; if this is a middle of the line, then insert after
	 (t
	  (end-of-line)
	  (newline t)
	  (indent-relative)
	  (insert "+ ")))
	(insert text)
	(beginning-of-line))
    ad-do-it))

(global-set-key "\C-v" 'scroll-up)