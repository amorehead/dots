;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; C-c n r is for refiling any tasks into a more appropriate location
(define-key global-map "\C-cnr" 'org-refile)

(setq user-full-name "Alex Morehead"
      user-mail-address "alex.morehead@gmail.com"
      doom-scratch-intial-major-mode 'lisp-interaction-mode
      doom-font (font-spec :family "Iosevka" :size 16)
      doom-variable-pitch-font (font-spec :family "Libre Baskerville")
      doom-serif-font (font-spec :family "Libre Baskerville")
      display-line-numbers-type nil

      company-idle-delay nil
      lsp-ui-sideline-enable nil
      lsp-enable-symbol-highlighting nil)

(setq org-directory "~/Dropbox/Documents/Org/GTD/Org"
      org-ellipsis " ▼ "
      org-adapt-indentation nil
      org-habit-show-habits-only-for-today t)

(setq search-highlight t
      search-whitespace-regexp ".*?"
      isearch-lax-whitespace t
      isearch-regexp-lax-whitespace nil
      isearch-lazy-highlight t
      isearch-lazy-count t
      lazy-count-prefix-format " (%s/%s) "
      lazy-count-suffix-format nil
      isearch-yank-on-move 'shift
      isearch-allow-scroll 'unlimited)

(setq direnv-always-show-summary nil)

(remove-hook '+doom-dashboard-functions #'doom-dashboard-widget-shortmenu)

(use-package! notmuch
  :commands (notmuch)
  :init
  (map! :desc "notmuch" "<f2>" #'notmuch)
  (map! :map notmuch-search-mode-map
        :desc "toggle read" "t" #'+notmuch/toggle-read
        :desc "Reply to thread" "r" #'notmuch-search-reply-to-thread
        :desc "Reply to thread sender" "R" #'notmuch-search-reply-to-thread-sender
        :desc "Filter" "/" #'notmuch-search-filter
        :desc "Archive All" "A" #'+notmuch-archive-all
        :desc "Delete All" "D" #'+notmuch-delete-all)
  (map! :map notmuch-show-mode-map
        :desc "Next link" "<tab>" #'org-next-link
        :desc "Previous link" "<backtab>" #'org-previous-link
        :desc "URL at point" "C-<return>" #'browse-url-at-point)
  (defun +notmuch/toggle-read ()
    "toggle read status of message"
    (interactive)
    (if (member "unread" (notmuch-search-get-tags))
        (notmuch-search-tag (list "-unread"))
      (notmuch-search-tag (list "+unread"))))
  :config
  (setq message-auto-save-directory "~/.mail/Drafts/"
        message-send-mail-function 'message-send-mail-with-sendmail
        sendmail-program (executable-find "msmtp")
        message-sendmail-envelope-from 'header
        mail-envelope-from 'header
        mail-specify-envelope-from t
        message-sendmail-f-is-evil nil
        message-kill-buffer-on-exit t
        notmuch-always-prompt-for-sender t
        notmuch-archive-tags '("-unread")
        notmuch-crypto-process-mime t
        notmuch-hello-sections '(notmuch-hello-insert-saved-searches)
        notmuch-labeler-hide-known-labels t
        notmuch-search-oldest-first nil
        notmuch-archive-tags '("-inbox" "-unread")
        notmuch-message-headers '("To" "Cc" "Subject" "Bcc")
        notmuch-saved-searches '((:name "inbox" :query "tag:inbox")
                                 (:name "unread" :query "tag:inbox and tag:unread")
                                 (:name "to-me" :query "tag:inbox and tag:to-me")
                                 (:name "personal" :query "tag:inbox and tag:personal")
                                 (:name "org-roam" :query "tag:inbox and tag:roam")
                                 (:name "nus" :query "tag:inbox and tag:nus")
                                 (:name "drafts" :query "tag:draft")))

  (defun +notmuch-archive-all ()
    "Archive all the emails in the current view."
    (interactive)
    (notmuch-search-archive-thread nil (point-min) (point-max)))


  (defun +notmuch-delete-all ()
    "Archive all the emails in the current view.
Mark them for deletion by cron job."
    (interactive)
    (notmuch-search-tag-all '("+deleted"))
    (+notmuch-archive-all)))

(after! dired
  (setq dired-listing-switches "-aBhl  --group-directories-first"
        dired-dwim-target t
        dired-recursive-copies (quote always)
        dired-recursive-deletes (quote top)))

(use-package! dired-narrow
  :commands (dired-narrow-fuzzy)
  :init
  (map! :map dired-mode-map
        :desc "narrow" "/" #'dired-narrow-fuzzy))

(use-package! deadgrep
  :if (executable-find "rg")
  :init
  (map! "M-s" #'deadgrep))

(use-package! smerge-mode
  :bind (("C-c h s" . amorehead/hydra-smerge/body))
  :init
  (defun amorehead/enable-smerge-maybe ()
    "Auto-enable `smerge-mode' when merge conflict is detected."
    (save-excursion
      (goto-char (point-min))
      (when (re-search-forward "^<<<<<<< " nil :noerror)
        (smerge-mode 1))))
  (add-hook 'find-file-hook #'amorehead/enable-smerge-maybe :append)
  :config
  (defhydra amorehead/hydra-smerge (:color pink
                                        :hint nil
                                        :pre (smerge-mode 1)
                                        ;; Disable `smerge-mode' when quitting hydra if
                                        ;; no merge conflicts remain.
                                        :post (smerge-auto-leave))
    "
   ^Move^       ^Keep^               ^Diff^                 ^Other^
   ^^-----------^^-------------------^^---------------------^^-------
   _n_ext       _b_ase               _<_: upper/base        _C_ombine
   _p_rev       _u_pper           g   _=_: upper/lower       _r_esolve
   ^^           _l_ower              _>_: base/lower        _k_ill current
   ^^           _a_ll                _R_efine
   ^^           _RET_: current       _E_diff
   "
    ("n" smerge-next)
    ("p" smerge-prev)
    ("b" smerge-keep-base)
    ("u" smerge-keep-upper)
    ("l" smerge-keep-lower)
    ("a" smerge-keep-all)
    ("RET" smerge-keep-current)
    ("\C-m" smerge-keep-current)
    ("<" smerge-diff-base-upper)
    ("=" smerge-diff-upper-lower)
    (">" smerge-diff-base-lower)
    ("R" smerge-refine)
    ("E" smerge-ediff)
    ("C" smerge-combine-with-next)
    ("r" smerge-resolve)
    ("k" smerge-kill-current)
    ("q" nil "cancel" :color blue)))

(use-package! magit
  :init
  (map! "s-g" #'magit-status
        "C-c g" #'magit-status
        "s-G" #'magit-blame-addition
        "C-c G" #'magit-blame-addition)
  :config
  (transient-append-suffix 'magit-log "a"
    '("w" "Wip" magit-wip-log-current))
  (magit-define-popup-switch 'magit-log-popup
                             ?m "Omit merge commits" "--no-merges")
  (transient-append-suffix 'magit-log "-A"
    '("-m" "Omit merge commits" "--no-merges")))

(use-package! git-link
  :commands
  (git-link git-link-commit git-link-homepage)
  :custom
  (git-link-use-commit t))

(use-package! easy-kill
  :bind*
  (([remap kill-ring-save] . easy-kill)))

(use-package! smartparens
  :init
  (map! :map smartparens-mode-map
        "C-M-f" #'sp-forward-sexp
        "C-M-b" #'sp-backward-sexp
        "C-M-u" #'sp-backward-up-sexp
        "C-M-d" #'sp-down-sexp
        "C-M-p" #'sp-backward-down-sexp
        "C-M-n" #'sp-up-sexp
        "C-M-s" #'sp-splice-sexp
        "C-)" #'sp-forward-slurp-sexp
        "C-}" #'sp-forward-barf-sexp
        "C-(" #'sp-backward-slurp-sexp
        "C-M-)" #'sp-backward-slurp-sexp
        "C-M-)" #'sp-backward-barf-sexp))

;; Sourced from http://cachestocaches.com/2018/6/org-literate-programming/#org-babel-basics--remote-code-execution
;; Run/highlight code using babel in org-mode
(org-babel-do-load-languages
'org-babel-load-languages
'(
    (emacs-lisp . t)
    (ipython . t)
    (python . t)
    (dot . t)
    (R . t)
    (sh . t)
    (shell . t)
  ;; Include other languages here...
  ))
;; Syntax highlight in #+BEGIN_SRC blocks
(setq org-src-fontify-natively t)
;; Don't prompt before running code in org
(setq org-confirm-babel-evaluate nil)
;; Fix an incompatibility between the ob-async and ob-ipython packages
(setq ob-async-no-async-languages-alist '("ipython"))

(after! org
  (use-package! ol-notmuch
  :init
  (map! :map notmuch-show-mode-map "C" #'amorehead/org-capture-email)
  (defun amorehead/org-capture-email ()
    (interactive)
    (org-capture nil "e")))
  (require 'org-habit)

  (with-eval-after-load 'flycheck
    (flycheck-add-mode 'proselint 'org-mode))

  (map! :leader
        :prefix "n"
        "c" #'org-capture)
  (map! :map org-mode-map
        "M-n" #'outline-next-visible-heading
        "M-p" #'outline-previous-visible-heading)
  (setq org-src-window-setup 'current-window
        org-return-follows-link t
        org-use-speed-commands t
        org-catch-invisible-edits 'show
        org-preview-latex-image-directory "/tmp/ltximg/"
        org-structure-template-alist '(("a" . "export ascii")
                                       ("c" . "center")
                                       ("C" . "comment")
                                       ("e" . "example")
                                       ("E" . "export")
                                       ("h" . "export html")
                                       ("l" . "export latex")
                                       ("q" . "quote")
                                       ("s" . "src")
                                       ("v" . "verse")
                                       ("el" . "src emacs-lisp")
                                       ("d" . "definition")
                                       ("t" . "theorem")))


  (defun amorehead/org-archive-done-tasks ()
    "Archive all done tasks."
    (interactive)
    (org-map-entries 'org-archive-subtree "/DONE" 'file))
  (require 'find-lisp)
  (setq amorehead/org-agenda-directory (file-truename "~/Dropbox/Documents/Org/GTD/Org/"))
  (setq org-agenda-files
        (find-lisp-find-files amorehead/org-agenda-directory "\.org$")))

(setq org-capture-templates
      `(("i" "Inbox" entry (file ,(concat amorehead/org-agenda-directory "Inbox.org"))
         "* TODO %? :INBOX:")
        ("e" "Email" entry (file+headline ,(concat amorehead/org-agenda-directory "Emails.org") "Emails")
         "* TODO [#A] Reply: %a :@HOME:@SCHOOL:" :immediate-finish t)
        ("l" "Link" entry (file ,(concat amorehead/org-agenda-directory "Inbox.org"))
         "* TODO %(org-cliplink-capture)" :immediate-finish t)
        ("c" "Capture with org-protocol" entry (file ,(concat amorehead/org-agenda-directory "Inbox.org"))
         "* TODO [[%:link][%:description]]\n\n %i" :immediate-finish t)))

(setq org-todo-keywords
      '((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d)")
        (sequence "WAITING(w@/!)" "HOLD(h@/!)" "|" "CANCELLED(c@/!)")))

(setq org-log-done 'time
      org-log-into-drawer t
      org-log-state-notes-insert-after-drawers nil)

(setq org-tag-alist '(("@errand" . ?e)
                      ("@office" . ?o)
                      ("@home" . ?h)
                      (:newline)
                      ("CANCELLED" . ?c)))

(setq org-fast-tag-selection-single-key nil)
(setq org-refile-use-outline-path 'file
      org-outline-path-complete-in-steps nil)
(setq org-refile-allow-creating-parent-nodes 'confirm
      org-refile-targets '((org-agenda-files . (:level . 1))))

(defvar amorehead/org-agenda-bulk-process-key ?f
  "Default key for bulk processing inbox items.")

(defun amorehead/org-process-inbox ()
  "Called in org-agenda-mode, processes all inbox items."
  (interactive)
  (org-agenda-bulk-mark-regexp "inbox:")
  (amorehead/bulk-process-entries))

(defvar amorehead/org-current-effort "1:00"
  "Current effort for agenda items.")

(defun amorehead/my-org-agenda-set-effort (effort)
  "Set the effort property for the current headline."
  (interactive
   (list (read-string (format "Effort [%s]: " amorehead/org-current-effort) nil nil amorehead/org-current-effort)))
  (setq amorehead/org-current-effort effort)
  (org-agenda-check-no-diary)
  (let* ((hdmarker (or (org-get-at-bol 'org-hd-marker)
                       (org-agenda-error)))
         (buffer (marker-buffer hdmarker))
         (pos (marker-position hdmarker))
         (inhibit-read-only t)
         newhead)
    (org-with-remote-undo buffer
      (with-current-buffer buffer
        (widen)
        (goto-char pos)
        (org-show-context 'agenda)
        (funcall-interactively 'org-set-effort nil amorehead/org-current-effort)
        (end-of-line 1)
        (setq newhead (org-get-heading)))
      (org-agenda-change-all-lines newhead hdmarker))))

(defun amorehead/org-agenda-process-inbox-item ()
  "Process a single item in the org-agenda."
  (org-with-wide-buffer
   (org-agenda-set-tags)
   (org-agenda-priority)
   (call-interactively 'amorehead/my-org-agenda-set-effort)
   (org-agenda-refile nil nil t)))

(defun amorehead/bulk-process-entries ()
  (if (not (null org-agenda-bulk-marked-entries))
      (let ((entries (reverse org-agenda-bulk-marked-entries))
            (processed 0)
            (skipped 0))
        (dolist (e entries)
          (let ((pos (text-property-any (point-min) (point-max) 'org-hd-marker e)))
            (if (not pos)
                (progn (message "Skipping removed entry at %s" e)
                       (cl-incf skipped))
              (goto-char pos)
              (let (org-loop-over-headlines-in-active-region) (funcall 'amorehead/org-agenda-process-inbox-item))
              ;; `post-command-hook' is not run yet.  We make sure any
              ;; pending log note is processed.
              (when (or (memq 'org-add-log-note (default-value 'post-command-hook))
                        (memq 'org-add-log-note post-command-hook))
                (org-add-log-note))
              (cl-incf processed))))
        (org-agenda-redo)
        (unless org-agenda-persistent-marks (org-agenda-bulk-unmark-all))
        (message "Acted on %d entries%s%s"
                 processed
                 (if (= skipped 0)
                     ""
                   (format ", skipped %d (disappeared before their turn)"
                           skipped))
                 (if (not org-agenda-persistent-marks) "" " (kept marked)")))))

(defun amorehead/org-inbox-capture ()
  (interactive)
  "Capture a task in agenda mode."
  (org-capture nil "i"))

(setq org-agenda-bulk-custom-functions `((,amorehead/org-agenda-bulk-process-key amorehead/org-agenda-process-inbox-item)))

(map! :map org-agenda-mode-map
      "i" #'org-agenda-clock-in
      "r" #'amorehead/org-process-inbox
      "R" #'org-agenda-refile
      "c" #'amorehead/org-inbox-capture)

(defun amorehead/set-todo-state-next ()
  "Visit each parent task and change NEXT states to TODO"
  (org-todo "NEXT"))

(add-hook 'org-clock-in-hook 'amorehead/set-todo-state-next 'append)

(use-package! org-clock-convenience
  :bind (:map org-agenda-mode-map
              ("<S-up>" . org-clock-convenience-timestamp-up)
              ("<S-down>" . org-clock-convenience-timestamp-down)
              ("o" . org-clock-convenience-fill-gap)
              ("e" . org-clock-convenience-fill-gap-both)))

(use-package! org-agenda
  :init
  (map! "<f1>" #'amorehead/switch-to-agenda)
  (setq org-agenda-block-separator nil
        org-agenda-start-with-log-mode t)
  (defun amorehead/switch-to-agenda ()
    (interactive)
    (org-agenda nil " "))
  :config
  (defun amorehead/is-project-p ()
  "Any task with a todo keyword subtask"
  (save-restriction
    (widen)
    (let ((has-subtask)
          (subtree-end (save-excursion (org-end-of-subtree t)))
          (is-a-task (member (nth 2 (org-heading-components)) org-todo-keywords-1)))
      (save-excursion
        (forward-line 1)
        (while (and (not has-subtask)
                    (< (point) subtree-end)
                    (re-search-forward "^\*+ " subtree-end t))
          (when (member (org-get-todo-state) org-todo-keywords-1)
            (setq has-subtask t))))
      (and is-a-task has-subtask))))

  (defun amorehead/skip-projects ()
  "Skip trees that are projects"
  (save-restriction
    (widen)
    (let ((next-headline (save-excursion (or (outline-next-heading) (point-max)))))
      (cond
       ((org-is-habit-p)
        next-headline)
       ((amorehead/is-project-p)
        next-headline)
       (t
        nil)))))

  (setq org-columns-default-format "%40ITEM(Task) %Effort(EE){:} %CLOCKSUM(Time Spent) %SCHEDULED(Scheduled) %DEADLINE(Deadline)")
  (setq org-agenda-custom-commands `(("A" "Agenda"
                                      ((agenda ""
                                               ((org-agenda-span 'week)
                                                (org-deadline-warning-days 365)))
                                       (todo "TODO"
                                             ((org-agenda-overriding-header "Inbox")
                                              (org-agenda-files '(,(concat amorehead/org-agenda-directory "Inbox.org")))))
                                       (todo "TODO"
                                             ((org-agenda-overriding-header "Emails")
                                              (org-agenda-files '(,(concat amorehead/org-agenda-directory "Emails.org")))))
                                       (todo "NEXT"
                                             ((org-agenda-overriding-header "In Progress")
                                              (org-agenda-files '(,(concat amorehead/org-agenda-directory "Projects.org")))))
                                       (todo "TODO"
                                             ((org-agenda-overriding-header "Active Projects")
                                              (org-agenda-skip-function #'amorehead/skip-projects)
                                              (org-agenda-files '(,(concat amorehead/org-agenda-directory "Projects.org")))))
                                       (todo "TODO"
                                             ((org-agenda-overriding-header "One-Off Tasks")
                                              (org-agenda-files '(,(concat amorehead/org-agenda-directory "Next.org")))
                                              (org-agenda-skip-function '(org-agenda-skip-entry-if 'deadline 'scheduled)))))))))

(use-package! org-roam
  :commands (org-roam-insert org-roam-find-file org-roam-switch-to-buffer org-roam)
  :hook
  (after-init . org-roam-mode)
  :init
  (map! :leader
        :prefix "n"
        :desc "org-roam" "l" #'org-roam
        :desc "org-roam-insert" "i" #'org-roam-insert
        :desc "org-roam-switch-to-buffer" "b" #'org-roam-switch-to-buffer
        :desc "org-roam-find-file" "f" #'org-roam-find-file
        :desc "org-roam-show-graph" "g" #'org-roam-show-graph
        :desc "org-roam-insert" "i" #'org-roam-insert
        :desc "org-roam-capture" "c" #'org-roam-capture)
  (setq org-roam-directory (file-truename "~/Dropbox/Documents/Org/MindMeld/Org/")
        org-roam-db-location "~/Dropbox/Documents/Org/MindMeld/Org/org_roam.db"
        org-roam-db-gc-threshold most-positive-fixnum
        org-roam-graph-exclude-matcher (list "Private" "Lab")
        org-roam-tag-sources '(prop last-directory)
        org-id-link-to-org-use-id t)
  :config
  (setq org-roam-capture-templates
        '(("b" "Book" plain (function org-roam--capture-get-point)
           "%?"
           :file-name "Books/${slug}"
           :head "#+setupfile:../hugo_setup.org
#+hugo_slug: ${slug}
#+roam_tags: book
#+title: ${title}

** Source


** Summary
*** Abstract


*** Introduction


*** Related Work
Related to 

*** Methods


*** Results


*** Conclusions
"
           :unnarrowed t)
          ("c" "Concept" plain (function org-roam--capture-get-point)
           "%?"
           :file-name "Concepts/${slug}"
           :head "#+setupfile:../hugo_setup.org
#+hugo_slug: ${slug}
#+roam_tags: concept
#+title: ${title}

** Summary


** Related Work
Related to "
           :unnarrowed t)
          ("l" "Lab" plain (function org-roam--capture-get-point)
           "%?"
           :file-name "Lab/${slug}"
           :head "#+setupfile:../hugo_setup.org
#+hugo_slug: ${slug}
#+roam_tags: lab
#+title: ${title}

** Summary
"
           :unnarrowed t)
          ("p" "Paper" plain (function org-roam--capture-get-point)
           "%?"
           :file-name "Papers/${slug}"
           :head "#+setupfile:../hugo_setup.org
#+hugo_slug: ${slug}
#+roam_tags: paper
#+title: ${title}

** Source


** Summary
*** Abstract


*** Introduction


*** Related Work
Need [[file:../2021_research_list.org][to read]].

*** Methods


*** Results


*** Conclusions
"
           :unnarrowed t)
          ("t" "Talk" plain (function org-roam--capture-get-point)
           "%?"
           :file-name "Talks/${slug}"
           :head "#+setupfile:../hugo_setup.org
#+hugo_slug: ${slug}
#+roam_tags: talk
#+title: ${title}

** Source


** Summary
*** Abstract


*** Introduction


*** Related Work
Related to

*** Methods


*** Results


*** Conclusions
"
           :unnarrowed t)
          ("j" "Private" plain (function org-roam-capture--get-point)
           "%?"
           :file-name "Private/${slug}"
           :head "#+roam_tags: private
#+title: ${title}

** Summary
"
           :unnarrowed t)))
  (setq org-roam-capture-ref-templates
        '(("r" "ref" plain (function org-roam-capture--get-point)
           "%?"
           :file-name "Websites/${slug}"
           :head "#+setupfile:../hugo_setup.org
#+roam_key: ${ref}
#+hugo_slug: ${slug}
#+roam_tags: website
#+title: ${title}

** Source
${ref}

** Summary


Related to "
           :unnarrowed t)))
  (set-company-backend! 'org-mode '(company-capf)))

(use-package! org-roam-protocol
  :after org-protocol)

(after! company
  (map! "M-/" #'company-complete))

(use-package! company-posframe
  :hook (company-mode . company-posframe-mode))

(after! (org-roam)
  (winner-mode +1)
  (map! :map winner-mode-map
        "<M-right>" #'winner-redo
        "<M-left>" #'winner-undo))

(after! (org ox-hugo)
  (defun amorehead/conditional-hugo-enable ()
    (save-excursion
      (if (cdr (assoc "SETUPFILE" (org-roam--extract-global-props '("SETUPFILE"))))
          (org-hugo-auto-export-mode +1)
        (org-hugo-auto-export-mode -1))))
  (add-hook 'org-mode-hook #'amorehead/conditional-hugo-enable))

(use-package! org-download
  :commands
  org-download-dnd
  org-download-yank
  org-download-screenshot
  org-download-dnd-base64
  :init
  (map! :map org-mode-map
        "s-Y" #'org-download-screenshot
        "s-y" #'org-download-yank)
  (pushnew! dnd-protocol-alist
            '("^\\(?:https?\\|ftp\\|file\\|nfs\\):" . +org-dragndrop-download-dnd-fn)
            '("^data:" . org-download-dnd-base64))
  (advice-add #'org-download-enable :override #'ignore)
  :config
  (defun +org/org-download-method (link)
    (let* ((filename
            (file-name-nondirectory
             (car (url-path-and-query
                   (url-generic-parse-url link)))))
           ;; Create folder name with current buffer name, and place in root dir
           (dirname (concat "./images/"
                            (replace-regexp-in-string " " "_"
                                                      (downcase (file-name-base buffer-file-name)))))
           (filename-with-timestamp (format "%s%s.%s"
                                            (file-name-sans-extension filename)
                                            (format-time-string org-download-timestamp)
                                            (file-name-extension filename))))
      (make-directory dirname t)
      (expand-file-name filename-with-timestamp dirname)))
  :config
  (setq org-download-screenshot-method
        (cond (IS-MAC "screencapture -i %s")
              (IS-LINUX
               (cond ((executable-find "maim")  "maim -u -s %s")
                     ((executable-find "scrot") "scrot -s %s")))))
  (setq org-download-method '+org/org-download-method))

(after! org-journal
  (setq org-journal-date-prefix "#+TITLE: "
        org-journal-file-format "%Y-%m-%d.org"
        org-journal-dir (file-truename "~/Dropbox/Documents/Org/MindMeld/Org/Private/")
        org-journal-carryover-items nil))

(use-package! citeproc-org
  :after org
  :config
  (citeproc-org-setup))

(use-package! mathpix.el
  :commands (mathpix-screenshot)
  :init
  (map! "C-x m" #'mathpix-screenshot)
  :config
  (setq mathpix-screenshot-method "maim -u -s %s"
        mathpix-app-id (password-store-get "mathpix/app-id")
        mathpix-app-key (password-store-get "mathpix/app-key")))

(use-package! gif-screencast
  :bind
  ("<f12>" . gif-screencast-start-or-stop))

(defun insert-date ()
  "Insert a timestamp according to locale's date and time format."
  (interactive)
  (insert (format-time-string "%c" (current-time))))

(use-package! outshine
  :commands (outshine-mode))

(after! ivy
  (map! :map ivy-minibuffer-map
        "S-SPC" nil)
  (add-to-list 'ivy-re-builders-alist '(counsel-projectile-find-file . ivy--regex-plus)))

(after! mixed-pitch
  (dolist (f (-filter (lambda (sym)
                        (s-prefix? "company-" (symbol-name sym)))
                      (face-list)))
    (pushnew! mixed-pitch-fixed-pitch-faces f)))

(use-package! org-roam-bibtex
  :after (org-roam)
  :hook (org-roam-mode . org-roam-bibtex-mode)
  :config
  (setq org-roam-bibtex-preformat-keywords
   '("=key=" "title" "url" "file" "author-or-editor" "keywords"))
  (setq orb-templates
        `(("r" "ref" plain (function org-roam-capture--get-point)
           ""
           :file-name "Websites/${slug}"
           :head ,(concat
                   "#+setupfile: ../hugo_setup.org\n"
                   "#+title: ${=key=}: ${title}\n"
                   "#+roam_key: ${ref}\n\n"
                   "* ${title}\n"
                   "  :PROPERTIES:\n"
                   "  :Custom_ID: ${=key=}\n"
                   "  :URL: ${url}\n"
                   "  :AUTHOR: ${author-or-editor}\n"
                   "  :NOTER_DOCUMENT: %(orb-process-file-field \"${=key=}\")\n"
                   "  :NOTER_PAGE: \n"
                   "  :END:\n")
           :unnarrowed t))))

(use-package! bibtex-completion
  :config
  (setq bibtex-completion-notes-path "~/Dropbox/Documents/Org/MindMeld/Org"
        bibtex-completion-bibliography "~/Dropbox/Documents/Org/MindMeld/Org/Bibliographies/mindmeld.bib"
        bibtex-completion-pdf-field "file"
        bibtex-completion-notes-template-multiple-files
         (concat
          "#+title: ${title}\n"
          "#+roam_key: cite:${=key=}\n"
          "* TODO Notes\n"
          ":PROPERTIES:\n"
          ":Custom_ID: ${=key=}\n"
          ":NOTER_DOCUMENT: %(orb-process-file-field \"${=key=}\")\n"
          ":AUTHOR: ${author-abbrev}\n"
          ":JOURNAL: ${journaltitle}\n"
          ":DATE: ${date}\n"
          ":YEAR: ${year}\n"
          ":DOI: ${doi}\n"
          ":URL: ${url}\n"
          ":END:\n\n"
          )))

(use-package! nov
  :hook (nov-mode . variable-pitch-mode)
  :mode ("\\.\\(epub\\|mobi\\)\\'" . nov-mode))

(after! org-noter
  org-noter-doc-split-fraction '(0.57 0.43))

(use-package! yaml-mode
  :mode ("\\.yml\\'" . yaml-mode))

(use-package! org-roam-server)

(use-package! emmet-mode
  :hook
  ((sgml-mode . emmet-mode)
   (css-mode . emmet-mode)))


(defun amorehead/open-with (arg)
  "Open visited file in default external program.
When in dired mode, open file under the cursor.
With a prefix ARG always prompt for command to use."
  (interactive "P")
  (let* ((current-file-name
          (if (eq major-mode 'dired-mode)
              (dired-get-file-for-visit)
            buffer-file-name))
         (open (pcase system-type
                 (`darwin "open")
                 ((or `gnu `gnu/linux `gnu/kfreebsd) "xdg-open")))
         (program (if (or arg (not open))
                      (read-shell-command "Open current file with: ")
                    open)))
    (call-process program nil 0 nil current-file-name)))

;;(map! "C-c o o" 'amorehead/open-with)

(use-package! org-gcal
  :commands (org-gcal-sync)
  :config
  (setq org-gcal-client-id (password-store-get "Email/org-gcal-client")
        org-gcal-client-secret (password-store-get "Email/org-gcal-secret")
        org-gcal-file-alist `(("alex.morehead@gmail.com" . ,(concat amorehead/org-agenda-directory "Calendars/Personal.org")))))

(use-package prog-mode
  :config
  (add-hook 'prog-mode-hook 'outline-minor-mode)
  (add-hook 'prog-mode-hook 'hs-minor-mode))

;; Sourced from: https://jblevins.org/projects/deft/
(use-package deft
  :bind ("<f8>" . deft)
  :commands (deft)
  :config (setq deft-directory "~/Dropbox/Documents/Org/MindMeld/Org"))

(setq deft-recursive t)
(setq deft-use-filename-as-title t)

(defun amorehead/ledger-cleanup-csv ()
  (save-excursion
    (goto-char (point-min))
    (while (search-forward "\n" nil t)
      (beginning-of-line)
      (delete-horizontal-space)
      (end-of-line)
      (delete-horizontal-space)
      (when (string-equal (buffer-substring-no-properties (line-beginning-position) (line-end-position))
                          "")
        (delete-char -1)))))

(defun amorehead/ledger-import-sc (file)
  (interactive "f")
  (let* ((cleaned-content (with-temp-buffer
                            (insert-file-contents file)
                            (amorehead/ledger-cleanup-csv)
                            (buffer-string)))
         (lines (split-string cleaned-content "\n"))
         (account (completing-read "account: " (ledger-accounts-list-in-buffer))))
    (save-match-data
      (dolist (line lines)
        (let* ((items (split-string line ","))
               (date-maybe (car items)))
          (when (string-match "\\([0-9][0-9]\\)/\\([0-9][0-9]\\)/\\([0-9][0-9][0-9][0-9]\\)" date-maybe)
            (pcase-let ((`(_ ,info ,foreign ,sgd) items))
              (let ((date (format "%s/%s/%s" ;YYYY/MM/DD
                                  (match-string 3 date-maybe)
                                  (match-string 2 date-maybe)
                                  (match-string 1 date-maybe))))
                (when (string-match "SGD \\(.*\\) \\(DR\\|CR\\)" sgd)
                  (let ((cr-p (string-equal (match-string 2 sgd) "CR"))
                        (value (match-string 1 sgd)))
                    (insert date
                            " * " info "\n"
                            "    " account
                            "   " (if cr-p "-" "") "S$" value "\n"
                            "    " (completing-read info (ledger-accounts-list-in-buffer))
                            "\n\n\n")))))))))))

(defun kill-other-buffers ()
      "Kill all other buffers."
      (interactive)
      (mapc 'kill-buffer (delq (current-buffer) (buffer-list))))

(after! org-latex
  (setq org-latex-pdf-process (list "latexmk -f -xelatex %f")))

(setq org-babel-python-command "python3")

;; Source: https://stackoverflow.com/questions/55563546/emacs-org-mode-latex-simply-switch-between-pdflatex-xelatex-and-lualatex
(setq org-latex-pdf-process (list "latexmk -pdflatex='%latex -shell-escape -interaction nonstopmode' -pdf -output-directory=%o %f"))

;; Source: https://emacsredux.com/blog/2016/01/31/use-tab-to-indent-or-complete/
(setq tab-always-indent 'complete)

;; Source: https://stackoverflow.com/questions/47058372/in-org-mode-how-to-call-code-block-to-evaluate-from-other-org-file
(org-babel-lob-ingest "~/Dropbox/Documents/Org/MindMeld/Org/Concepts/org_babel_dot_graph_generation.org")

(map!
 [backtab] #'+fold/toggle
 [C-tab] #'+fold/open-all
 [C-iso-lefttab] #'+fold/close-all)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(fci-rule-color "#544863")
 '(git-link-use-commit t t)
 '(jdee-db-active-breakpoint-face-colors (cons "#222228" "#40B4C4"))
 '(jdee-db-requested-breakpoint-face-colors (cons "#222228" "#74DFC4"))
 '(jdee-db-spec-breakpoint-face-colors (cons "#222228" "#4E415C"))
 '(objed-cursor-color "#964C7B")
 '(pdf-view-midnight-colors (cons "#FFFFFF" "#27212E"))
 '(rustic-ansi-faces
   ["#27212E" "#964C7B" "#74DFC4" "#FFE261" "#40B4C4" "#EB64B9" "#B4DCE7" "#FFFFFF"])
 '(safe-local-variable-values
   '((eval require 'org-roam-dev)
     (eval amorehead/conditional-hugo-enable)
     (org-src-preserve-indentation)
     (eval require 'ol-info))))

;; Source: https://blog.jethro.dev/posts/migrating_to_doom_emacs/
(remove-hook 'text-mode-hook #'auto-fill-mode)
(add-hook 'message-mode-hook #'word-wrap-mode)

;; Source: https://github.com/Wilfred/deadgrep
(global-set-key (kbd "<f5>") #'deadgrep)

;; Found by interactively calling global-set-key (in GUI) followed by C-x ESC ESC (repeat-complex-command) to see how Emacs chose to formulate this command
(global-set-key [3 24 C-i] 'org-clock-in)

;; Found by interactively calling global-set-key (in GUI) followed by C-x ESC ESC (repeat-complex-command) to see how Emacs chose to formulate this command
(global-set-key [3 24 C-o] 'org-clock-out)

;; Source: https://orgmode.org/worg/org-contrib/org-protocol.html
(server-start)
(add-to-list 'load-path "~/path/to/org/protocol/")
(require 'org-protocol)

;; Source: https://emacs.stackexchange.com/questions/28037/org-mode-file-hyperlinks-always-use-doc-view-cant-force-it-to-use-external-pdf?rq=1
;; No code required for the enhancement above
