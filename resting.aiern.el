;; The TODO-planner payoff
;; :PROPERTIES:...
;; So TODOs are all well and good, but what is a really neat feature is when you
;; can easily capture new TODOs and display them easily.

;; In order to do this, let's configure a couple of Emacs options in your emacs init:


;; [[file:~/.emacs.d/resting.aiern.org::*The TODO-planner payoff][The TODO-planner payoff:1]]
(require 'org)
;; Setup C-c c to capture new TODOs
(global-set-key (kbd "C-c c") 'org-capture)
;; Setup a key bind for the agenda
(global-set-key (kbd "C-c a") 'org-agenda)
;; Set up agenda to know about our file, you can use a list of files or
;; directories here
(setq org-agenda-files '("~/todo.org"))
;; A new template
(setq org-capture-templates
      '(("t" "Todo" entry (file "~/todo.org")
         "* TODO %?\n%U\n")))
;; The TODO-planner payoff:1 ends here

;; Show off other features of org-mode if we have more time
;; Maybe not in excruciating detail, but we can show off the power and cover it in
;; more detail at a later time:

;; - refiling (=org-refile=)
;; - source code blocks
;; - org-babel
;; - clocking in/out
;; - table formulas
;; - custom agenda views
;; - capturing notes (not just TODOs)
;; - publishing projects remotely via TRAMP

;; #+BEGIN_LaTeX
;; $a + b$
;; #+END_LaTeX


;; [[file:~/.emacs.d/resting.aiern.org::*Show off other features of org-mode if we have more time][Show off other features of org-mode if we have more time:1]]
(defun my/function ()
  "docstring"
  (interactive)
  (progn
    (+ 1 1)
    (message "Hi")))
;; Show off other features of org-mode if we have more time:1 ends here

;; [[file:~/.emacs.d/resting.aiern.org::*python][python:2]]
(message (concat "Hello" "Europe!")) ;; inline comment
;; python:2 ends here

;; activating Org-mode

;; - http://orgmode.org/org.html#Activation
;; - http://orgmode.org/worg/org-configs/org-customization-guide.html

;; [[http://orgmode.org/worg/org-tutorials/orgtutorial_dto.html#sec-1-2][Activation of Org-mode (external Tutorial)]]:

;; [[file:~/.emacs.d/resting.aiern.org::*activating Org-mode][activating Org-mode:1]]
(add-to-list 'auto-mode-alist '("\\.\\(org\\|org_archive\\|txt\\)$" . org-mode))
(define-key global-map "\C-ca" 'org-agenda)     ;; by convention: "C-c a" opens agenda
(define-key global-map "\C-cc" 'org-capture)    ;; by convention: "C-c c" captures stuff
(define-key global-map "\C-cl" 'org-store-link) ;; by convention: "C-c l" stores a link to this heading
(setq org-log-done t) ;; if you want to log DONE-states to the :LOGBOOK:-drawer
;; activating Org-mode:1 ends here

;; capture, refile, archive ([[http://orgmode.org/org.html#Capture-_002d-Refile-_002d-Archive][docu]])

;; - ~C-c c~          *capture*
;; - ~C-c C-w~        *refile*
;; - ~C-c C-x C-a~    *archive*
;; - ~C-u C-u C-c c~  *goto last capture*

;; - ~#+ARCHIVE: %s_done::~   per-file archive
;; - ~C-c C-x a~       toggle ARCHIVE tag
;; - ~C-u C-c C-x a~   check direct children for archiving


;; [[file:~/.emacs.d/resting.aiern.org::*capture, refile, archive ([[http://orgmode.org/org.html#Capture-_002d-Refile-_002d-Archive\][docu\]])][capture, refile, archive ([[http://orgmode.org/org.html#Capture-_002d-Refile-_002d-Archive][docu]]):1]]
;; ######################################################
;; templates for capturing C-c c
;; http://orgmode.org/org.html#Capture-templates
(setq org-capture-templates
      '(
	("s" "shorts-todo" entry (file+headline "~/share/all/org-mode/misc.org" "shorts")
	 "* NEXT %?\n:PROPERTIES:\n:CREATED: %U\n:END:\n\n" :empty-lines 1)
	("e" "Event" entry (file+headline "~/share/all/org-mode/misc.org" "Events")
	 "* %?\n:PROPERTIES:\n:CREATED: %U\n:END:\n\n" :empty-lines 1)
	("i" "IST Templates")
	("is" "IST shorts" entry (file+headline "~/share/all/org-mode/IST.org" "shorts")
	 "* NEXT %?\n:PROPERTIES:\n:CREATED: %U\n:END:\n\n" :empty-lines 1)
	("ie" "IST event" entry (file+headline "~/share/all/org-mode/IST.org" "Events")
	 "* %?\n:PROPERTIES:\n:CREATED: %U\n:END:\n\n" :empty-lines 1)
	("ii" "IST isst" entry (file+headline "~/share/all/org-mode/IST.org" "Events")
	 "* %? IST isst: \n:PROPERTIES:\n:CREATED: %U\n:END:\n\n- [[contact:Ingo Pill][Ingo Pill]]\n- [[contact:Thomas Quartisch][Thomas Quartisch]]\n\n" :empty-lines 1)
	("b" "Besorgung" entry (file+headline "~/share/all/org-mode/hardware.org" "Besorgungen")
	 "* TODO %?\n:PROPERTIES:\n:CREATED: %U\n:END:\n\n" :empty-lines 1)
	("C" "Clipboard" entry (file+headline "~/share/all/org-mode/misc.org" "shorts")
	 "* TODO %?\n:PROPERTIES:\n:CREATED: %U\n:END:\n%x\n\n" :empty-lines 1)
	("c" "capture to inbox, refile later" entry (file "~/share/all/org-mode/inbox.org")
	 "\n* %?\n:PROPERTIES:\n:CREATED: %U\n:END:\n\n" :empty-lines 1)
	("m" "movie" entry (file+headline "~/share/all/org-mode/movies.org" "inbox")
	 "* TODO %?\n:PROPERTIES:\n:CREATED: %U\n:END:\n\n" :empty-lines 1)
	("x" "xlog")
	("xh" "xlog hometrainer" table-line (id "xlog-hometrainer") "| %T |  |  |  |")
	("xk" "Keyboard Akkus leer" table-line (id "3407c9b7-1b41-443b-9254-32c4af3a54e8") "| %T |")
      )
)
;; capture, refile, archive ([[http://orgmode.org/org.html#Capture-_002d-Refile-_002d-Archive][docu]]):1 ends here

;; MobileOrg ([[http://orgmode.org/org.html#MobileOrg][docu]])

;; - great [[http://mobileorg.ncogni.to/][iOS app]]
;;   - no iOS on my side
;;   - does not seem to be maintained any more :-(
;; - [[http://wiki.github.com/matburt/mobileorg-android/][Android app]]
;;   - sync via Dropbox, ssh, scp, WEBDAV, gpg encryption, ...


;; [[file:~/.emacs.d/resting.aiern.org::*MobileOrg ([[http://orgmode.org/org.html#MobileOrg\][docu\]])][MobileOrg ([[http://orgmode.org/org.html#MobileOrg][docu]]):1]]
;;; http://orgmode.org/org.html#MobileOrg
;;; directory where to store MobileOrg-files
(setq org-mobile-directory "~/share/all/org-mode/mobile-org/")
(setq org-directory "~/share/all/org-mode")
(setq org-mobile-inbox-for-pull "~/share/all/org-mode/inbox.org")
(setq org-mobile-force-id-on-agenda-items nil)
'(org-mobile-files (quote ("~/share/all/org-mode/contacts.org" "~/data/share/all/org-mode/hardware.org" )))
;; MobileOrg ([[http://orgmode.org/org.html#MobileOrg][docu]]):1 ends here
