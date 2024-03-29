;; To set up Doom Emacs for Manjaro Linux, use the following procedure:
;;
;; 0. Remove any existing Emacs/Doom Emacs configuration files by running `pamac remove emacs-nativecomp emacs-emacsql emacs-emacsql-sqlite3 emacs-pg` followed by `rm -rf ~/.config/emacs/ ~/.config/doom/ ~/.emacs.d/ ~/.doom.d/`
;; 1. Install natively-compiled Emacs by running `pamac install emacs-nativecomp`
;; 2. Install Doom Emacs locally by following the instructions at https://github.com/doomemacs/doomemacs/blob/master/docs/getting_started.org#doom-emacs
;; 3. Append the command `export PATH="$HOME/.emacs.d/bin:$PATH"` to your `~/.zshrc` startup file and restart your current shell to ensure `doom` commands now work globally
;; 4. Run `doom doctor`, and then install any required or optional fonts or language compilers (e.g., `pamac install marked shellcheck`)
;; 5. Install Org Mode dependencies by running `pamac install ripgrep fd biber ttf-iosevka ttf-librebaskerville texlive-latex texlive-latexrecommended texlive-basic texlive-fontsrecommended texlive-fontsextra texlive-latexextra texlive-plaingeneric texlive-binextra texlive-fontsextra texlive-bibtexextra texlive-langenglish texlive-mathscience texlive-xetex texlive-luatex emacs-emacsql-sqlite3`
;; 6. Replace Doom Emacs' local config with your standard remote-synced config by running `cp -r ~/Dropbox/Repositories/Personal_Repositories/dots/doom_emacs/.doom.d/ ~/`
;; 7. Install all dependencies for custom Doom Emacs config by now running `doom sync`
;; 8. Restart your local machine to properly initialize `sqlite` for Doom Emacs + Org Mode support
;; 9. Test your new Doom Emacs installation by opening (and optionally bookmarking) the "Emacs" application launcher
;; 10. After all of these steps are complete, if e.g., you still run into issues regarding org-agenda not showing all your TODO items on your calendar, restart your machine (again) to have sqlite3 start properly for Org Roam + SQLite support

;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; C-c n r is for refiling any tasks into a more appropriate location
;; (define-key global-map "\C-cnr" 'org-refile)

(setq user-full-name "Alex Morehead"
      user-mail-address "alex.morehead@gmail.com"
      doom-scratch-intial-major-mode 'lisp-interaction-mode
      doom-font (font-spec :family "Iosevka" :size 16)
      doom-variable-pitch-font (font-spec :family "Libre Baskerville")
      doom-big-font (font-spec :family "Libre Baskerville")
      doom-serif-font (font-spec :family "Libre Baskerville")
      display-line-numbers-type t

      company-idle-delay nil
      lsp-ui-sideline-enable nil
      lsp-enable-symbol-highlighting nil)
      
;; Sourced from: https://jblevins.org/projects/deft/
(use-package deft
  :bind ("<f8>" . deft)
  :commands (deft)
  :config (setq deft-directory "~/Dropbox/Documents/Org/MindMeld/Org"
  			    deft-extensions '("org", "tex")))

(setq deft-recursive t)
(setq deft-use-filename-as-title t)

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

;; Fix citeproc-org installation
;;(require 'company-posframe)

;; Add calfw for calendar views with org-agenda
(require 'calfw)
(require 'calfw-org)
(setq cfw:org-agenda-schedule-args '(:timestamp))

(after! org
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
      `(
		  ("i" "Inbox" entry (file ,(concat amorehead/org-agenda-directory "Inbox.org"))
		     "* TODO %? :INBOX:")
		  ("l" "Link" entry (file ,(concat amorehead/org-agenda-directory "Inbox.org"))
		     "* TODO %(org-cliplink-capture)" :immediate-finish t)
		  ("c" "Capture with org-protocol" entry (file ,(concat amorehead/org-agenda-directory "Inbox.org"))
		     "* TODO [[%:link][%:description]]\n\n %i" :immediate-finish t)
      )
)

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
  :commands (org-capture org-roam-graph org-roam-capture)
  :init
  (map! :leader
        :prefix "n"
        :desc "org-capture" "i" #'org-capture
        :desc "org-roam-graph" "g" #'org-roam-graph
        :desc "org-roam-capture" "c" #'org-roam-capture)
  (setq org-roam-complete-everywhere t)
  (setq org-roam-database-connector 'sqlite3)
  (setq org-roam-db-location "~/Dropbox/Documents/Org/MindMeld/Org/org_roam.db")
  (setq org-roam-directory (file-truename "~/Dropbox/Documents/Org/MindMeld/Org/"))
  (setq org-roam-db-gc-threshold most-positive-fixnum
        ;;org-roam-graph-exclude-matcher (list "Private" "Lab")
        org-roam-tag-sources '(prop last-directory)
        org-id-link-to-org-use-id t)
)

(use-package! org-roam-protocol
  :after org-protocol)

(after! company
  (map! "M-/" #'company-complete))

;;(use-package! company-posframe
;;  :hook (company-mode . company-posframe-mode))

(after! org-roam
  :ensure t
  :init
  (setq org-roam-v2-ack t)
  :custom
  ;;(winner-mode +1)
  ;;(map! :map winner-mode-map
  ;;      "<M-right>" #'winner-redo
  ;;      "<M-left>" #'winner-undo)
  (setq org-roam-capture-templates
  	'(
  		("b" "Book" plain "%?" :target
  		        (file+head "Books/${slug}.org"
"#+setupfile:../hugo_setup.org
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


*** Conclusions\n"
				) :unnarrowed t)

      	("c" "Concept" plain "%?" :target
      	        (file+head "Concepts/${slug}.org"
"#+setupfile:../hugo_setup.org
#+hugo_slug: ${slug}
#+roam_tags: concept
#+title: ${title}

** Summary


** Related Work
Related to "
				) :unnarrowed t)

        ("l" "Lab" plain "%?" :target
                (file+head "Lab/${slug}.org"
"#+setupfile:../hugo_setup.org
#+hugo_slug: ${slug}
#+roam_tags: lab
#+title: ${title}

** Summary\n"
				) :unnarrowed t)
        ("p" "Paper" plain "%?" :target
                (file+head "Papers/${slug}.org"
"#+setupfile:../hugo_setup.org
#+hugo_slug: ${slug}
#+roam_tags: paper
#+title: ${title}

** Source


** Summary
*** Abstract


*** Introduction


*** Related Work
Need [[file:../2024_research_list.org][to read]].

*** Methods


*** Results


*** Conclusions\n"
				) :unnarrowed t)
        ("t" "Talk" plain "%?" :target
                (file+head "Talks/${slug}.org"
"#+setupfile:../hugo_setup.org
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


*** Conclusions\n"
				) :unnarrowed t)
		("j" "Private" plain "%?" :target
		        (file+head "Private/${slug}.org"
"#+roam_tags: private
#+title: ${title}

** Summary\n"
				) :unnarrowed t)
  	)
  )
  (setq org-roam-capture-ref-templates
    '(
	    ("r" "ref" plain "%?" :target
	            (file+head "Websites/${slug}.org"
"#+setupfile:../hugo_setup.org
#+roam_key: ${ref}
#+hugo_slug: ${slug}
#+roam_tags: website
#+title: ${title}

** Source
${ref}

** Summary


Related to "
				) :unnarrowed t)
  	)
  )
  (set-company-backend! 'org-mode '(company-capf))
  :config
  (org-roam-setup)
)

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

;;(use-package! citeproc-org
;;  :after org
;;  :config
;;  (citeproc-org-setup))

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
    
(after! org
  (use-package! ox-extra
    :config
    (ox-extras-activate '(latex-header-blocks ignore-headlines))))
  ;; Import ox-latex to get org-latex-classes and other functionality
  ;; for exporting to LaTeX from org
  ;; Source: https://github.com/aidanscannell/my-org-resume#global-configuration-for-org-modes-latex-exporter
  (use-package! ox-latex
    :init
    ;; code here will run immediately
    :config
    ;; code here will run after the package is loaded
    (setq org-latex-pdf-process
          '("pdflatex -interaction nonstopmode -output-directory %o %f"
            "biber %b"
            "pdflatex -interaction nonstopmode -output-directory %o %f"
            "pdflatex -interaction nonstopmode -output-directory %o %f"))
    (setq org-latex-with-hyperref nil) ;; stop org adding hypersetup{author..} to latex export
    ;; (setq org-latex-prefer-user-labels t)
    
    ;; deleted unwanted file extensions after latexMK
    (setq org-latex-logfiles-extensions
          (quote ("lof" "lot" "tex~" "aux" "idx" "log" "out" "toc" "nav" "snm" "vrb" "dvi" "fdb_latexmk" "blg" "brf" "fls" "entoc" "ps" "spl" "bbl" "xmpi" "run.xml" "bcf" "acn" "acr" "alg" "glg" "gls" "ist")))

    (unless (boundp 'org-latex-classes)
      (setq org-latex-classes nil)))

(use-package! org-roam-bibtex
  :after (org-roam)
  :hook (org-roam-mode . org-roam-bibtex-mode)
  :config
  (require 'org-ref)
  (setq org-roam-bibtex-preformat-keywords
   '("=key=" "title" "url" "file" "author-or-editor" "keywords"))
  (setq orb-templates
	`(
        ("r" "ref" plain
           ;;(function org-roam-capture--get-point)
           ""
           :target (file+head "Websites/${slug}"
					   ,(concat
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
"  :END:\n"
						) :unnarrowed t)
    	)
  	)
  )
)

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

(use-package! org-roam-ui)

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

(use-package prog-mode
  :config
  (add-hook 'prog-mode-hook 'outline-minor-mode)
  (add-hook 'prog-mode-hook 'hs-minor-mode))

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

;;(after! org-latex
;;  (setq org-latex-pdf-process (list "latexmk -f -xelatex %f")))

(setq org-babel-python-command "python3")

;; Source: https://stackoverflow.com/questions/55563546/emacs-org-mode-latex-simply-switch-between-pdflatex-xelatex-and-lualatex
;; (setq org-latex-pdf-process (list "latexmk -pdflatex='%latex -shell-escape -interaction nonstopmode' -pdf -output-directory=%o %f"))

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

;; Force pdf-tools to "install" at startup to become the default PDF reader in Emacs
;; (pdf-tools-install)

(require 'openwith)
(openwith-mode t)
(setq openwith-associations '(("\\.pdf\\'" "xournalpp" (file))))

