;;; init.el --- Spacemacs Initialization File
;;
;; Copyright (c) 2012-2018 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
;; Comment/uncomment this line to enable MELPA Stable if desired.  See `package-archive-priorities`
;; and `package-pinned-packages`. Most users will not need or want to do this.
;;(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(package-initialize)

;; Sourced from: https://orgmode.org/elpa.html
(require 'package)
(add-to-list 'package-archives '("org" . "https://orgmode.org/elpa/") t)

;; Avoid garbage collection during startup.
;; see `SPC h . dotspacemacs-gc-cons' for more info
(defconst emacs-start-time (current-time))
(setq gc-cons-threshold 402653184 gc-cons-percentage 0.6)
(load (concat (file-name-directory load-file-name)
              "core/core-versions.el")
      nil (not init-file-debug))
(load (concat (file-name-directory load-file-name)
              "core/core-load-paths.el")
      nil (not init-file-debug))
(load (concat spacemacs-core-directory "core-dumper.el")
      nil (not init-file-debug))

(if (not (version<= spacemacs-emacs-min-version emacs-version))
    (error (concat "Your version of Emacs (%s) is too old. "
                   "Spacemacs requires Emacs version %s or above.")
           emacs-version spacemacs-emacs-min-version)
  ;; Disable file-name-handlers for a speed boost during init
  (let ((file-name-handler-alist nil))
    (require 'core-spacemacs)
    (spacemacs/dump-restore-load-path)
    (configuration-layer/load-lock-file)
    (spacemacs/init)
    (configuration-layer/stable-elpa-init)
    (configuration-layer/load)
    (spacemacs-buffer/display-startup-note)
    (spacemacs/setup-startup-hook)
    (spacemacs/dump-eval-delayed-functions)
    (when (and dotspacemacs-enable-server (not (spacemacs-is-dumping-p)))
      (require 'server)
      (when dotspacemacs-server-socket-dir
        (setq server-socket-dir dotspacemacs-server-socket-dir))
      (unless (server-running-p)
        (message "Starting a server...")
        (server-start)))))
(org-babel-do-load-languages
 'org-babel-load-languages
 '((python . t) (emacs-lisp . t) (R . t)))

;; C-c c is for capturing any kind of task, thought, or code snippet on a whim
(define-key global-map "\C-cc" 'org-capture)
;; Force UTF-8
(setq org-export-coding-system 'utf-8)

;; Sourced from: https://orgmode.org/worg/org-contrib/org-protocol.html
(require 'org-protocol)
(server-start)
(add-to-list 'load-path "/usr/share/applications/org-protocol-emacsclient.desktop")

;; Sourced from: https://blog.jethro.dev/posts/capturing_inbox/
(require 'org-capture)
(setq amorehead/org-agenda-directory "~/Dropbox/Documents/Org/")
(setq org-capture-templates
      `(("i" "Inbox" entry (file ,(concat amorehead/org-agenda-directory "Inbox.org"))
         "* TODO %? :INBOX:")
        ("e" "Email" entry (file+headline ,(concat amorehead/org-agenda-directory "Emails.org") "Emails")
         "* TODO [#A] Reply: %a :@HOME:@SCHOOL:" :immediate-finish t)
        ("l" "Link" entry (file ,(concat amorehead/org-agenda-directory "Inbox.org"))
         "* TODO %(org-cliplink-capture)" :immediate-finish t)
        ("c" "Capture with org-protocol" entry (file ,(concat amorehead/org-agenda-directory "Inbox.org"))
         "* TODO [[%:link][%:description]]\n\n %i" :immediate-finish t)))

(setq org-refile-targets '(("~/Dropbox/Documents/Org/Projects.org" :maxlevel . 5)
									("~/Dropbox/Documents/Org/Next.org" :maxlevel . 2)
									("~/Dropbox/Documents/Org/Emails.org" :maxlevel . 2)
									("~/Dropbox/Documents/Org/Reading.org" :maxlevel . 2)
									("~/Dropbox/Documents/Org/Someday.org" :level . 1)))

;; C-c C-w is for refiling any tasks into a more appropriate location
(define-key global-map "\C-cw" 'org-refile)

(setq org-todo-keywords '((sequence "TODO(t)" "NEXT(n)" "WAITING(w)" "|" "DONE(d)" "CANCELLED(c)")))

(setq org-agenda-text-search-extra-files '(agenda-archives))
(setq org-blank-before-new-entry (quote ((heading) (plain-list-item))))
(setq org-enforce-todo-dependencies t)
(setq org-log-done (quote time))
(setq org-log-redeadline (quote time))
(setq org-log-reschedule (quote time))

(setq org-agenda-custom-commands
      '(("c" "Simple agenda view" agenda "")))

(setq amorehead/org-agenda-todo-view
      `("A" "Agenda"
        ((agenda ""
                 ((org-agenda-span 'day)
                  (org-deadline-warning-days 365)))
         (todo "TODO"
               ((org-agenda-overriding-header "To Refile")
                (org-agenda-files '(,(concat amorehead/org-agenda-directory "Inbox.org")))))
         (todo "TODO"
               ((org-agenda-overriding-header "Emails")
                (org-agenda-files '(,(concat amorehead/org-agenda-directory "Emails.org")))))
         (todo "TODO"
               ((org-agenda-overriding-header "One-Off Tasks")
                (org-agenda-files '(,(concat amorehead/org-agenda-directory "Next.org")))
                (org-agenda-skip-function '(org-agenda-skip-entry-if 'deadline 'scheduled))))             
         (todo "NEXT"
               ((org-agenda-overriding-header "In Progress")
                (org-agenda-files '(,(concat amorehead/org-agenda-directory "Someday.org")
                                    ,(concat amorehead/org-agenda-directory "Projects.org")
                                    ,(concat amorehead/org-agenda-directory "Next.org")))
                ))
         (todo "TODO"
               ((org-agenda-overriding-header "Today's Tasks")
                (org-agenda-files '(,(concat amorehead/org-agenda-directory "Next.org")
                					,(concat amorehead/org-agenda-directory "Projects.org")))
                (org-agenda-skip-function '(org-agenda-skip-scheduled-task-if-not-today))
                (org-agenda-entry-types '(:schedule))))
         (todo "TODO"
               ((org-agenda-overriding-header "All Project Tasks")
                (org-agenda-files '(,(concat amorehead/org-agenda-directory "Projects.org")))
                ))
         nil)))

(defun jethro/org-agenda-process-inbox-item ()
  "Process a single item in the org-agenda."
  (org-with-wide-buffer
   (org-agenda-set-tags)
   (org-agenda-priority)
   (call-interactively 'jethro/my-org-agenda-set-effort)
   (org-agenda-refile nil nil t)))
   
;; Sourced from: https://emacs.stackexchange.com/questions/30188/show-only-todays-tasks-in-org-agenda
(defun org-agenda-skip-deadline-if-not-today ()
"If this function returns nil, the current match should not be skipped.
Otherwise, the function must return a position from where the search
should be continued."
  (ignore-errors
    (let ((subtree-end (save-excursion (org-end-of-subtree t)))
          (deadline-day
            (time-to-days
              (org-time-string-to-time
                (org-entry-get nil "DEADLINE"))))
          (now (time-to-days (current-time))))
       (and deadline-day
            (not (= deadline-day now))
            subtree-end))))
            
;; Sourced from: https://emacs.stackexchange.com/questions/30188/show-only-todays-tasks-in-org-agenda
(defun org-agenda-skip-scheduled-task-if-not-today ()
"If this function returns nil, the current match should not be skipped.
Otherwise, the function must return a position from where the search
should be continued."
  (ignore-errors
    (let ((subtree-end (save-excursion (org-end-of-subtree t)))
          (schedule-day
            (time-to-days
              (org-time-string-to-time
                (org-entry-get nil "SCHEDULED"))))
          (now (time-to-days (current-time))))
       (and schedule-day
            (not (= schedule-day now))
            subtree-end))))

(add-to-list 'org-agenda-custom-commands
             `("r" "Reading" todo ""
               ((org-agenda-files '(,(concat amorehead/org-agenda-directory "Reading.org"))))))

(add-to-list 'org-agenda-custom-commands `,amorehead/org-agenda-todo-view)

;; Reveal.js + Org mode - Sourced from: https://opensource.com/article/18/2/how-create-slides-emacs-org-mode-and-revealjs
(setq Org-Reveal-root "file:///home/alexm/Slides/reveal.js-4.0.2")
(setq Org-Reveal-title-slide nil)

;; Notmuch email integration
(autoload 'notmuch "notmuch" "notmuch mail" t)

;; Sourced from: https://orgmode.org/manual/Clocking-Work-Time.html
(setq org-clock-persist 'history)
(org-clock-persistence-insinuate)
