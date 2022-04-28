;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
;;(setq user-full-name "John Doe"
;;      user-mail-address "john@doe.com")

;;(make-directory "~/.org/gtd")
;;(make-directory "~/.org/braindump/org")

;;(require 'init-vulpea)

(load "~/.doom.d/vulpea.el")
(load "~/.doom.d/syscraft.el")

(setq user-full-name "Rishabh Sehgal"
      user-mail-address "sehgal.rish@utexas.edu"
      doom-scratch-initial-major-mode 'lisp-interaction-mode
      doom-font (font-spec :family "SF Mono" :size 15)
      doom-variable-pitch-font (font-spec :family "SF Pro" :size 15)
      doom-serif-font (font-spec :family "New York")
      doom-theme 'modus-operandi
      display-line-numbers-type nil
      load-prefer-newer t
      +zen-text-scale 1
      writeroom-extra-line-spacing 0.3

      company-idle-delay nil
      lsp-ui-sideline-enable nil
      lsp-enable-symbol-highlighting nil
      search-highlight t
      search-whitespace-regexp ".*?"
      org-directory "~/org/"
      org-ellipsis " ▼ "
      org-adapt-indentation nil
      org-habit-show-habits-only-for-today t)

(autoload 'ffap-guesser "ffap")
(setq minibuffer-default-add-function
      (defun minibuffer-default-add-function+ ()
        (with-selected-window (minibuffer-selected-window)
          (delete-dups
           (delq nil
                 (list (thing-at-point 'symbol)
                       (thing-at-point 'list)
                       (ffap-guesser)
                       (thing-at-point-url-at-point)))))))

(setq rish/default-bibliography `(,(expand-file-name "braindump/org/refs-rish.bib" org-directory)))

(use-package! modus-themes
  :ensure
  :init
  ;; Add all your customizations prior to loading the themes
  (setq modus-themes-italic-constructs t
        modus-themes-completions
        '((t . (extrabold intense)))
     ;; modus-themes-completions 'opinionated
        modus-themes-variable-pitch-headings t
        modus-themes-scale-headings t
        modus-themes-variable-pitch-ui t
        modus-themes-org-agenda
        '((header-block . (variable-pitch scale-title))
          (header-date . (grayscale bold-all)))
        modus-themes-org-blocks
        '(grayscale)
        modus-themes-mode-line
        '(borderless)
        modus-themes-region '(bg-only no-extend))

  ;; Load the theme files before enabling a theme
  (modus-themes-load-themes)
  :config
  (modus-themes-load-operandi)
  :bind ("<f5>" . modus-themes-toggle))

(use-package! ctrlf
  :hook
  (after-init . ctrlf-mode))

(use-package! dired-narrow
  :commands (dired-narrow-fuzzy)
  :init
  (map! :map dired-mode-map
        :desc "narrow" "/" #'dired-narrow-fuzzy))

(map! "C-c h s" #'+vc/smerge-hydra/body)

(use-package! git-link
  :commands
  (git-link git-link-commit git-link-homepage)
  :custom
  (git-link-use-commit t))

(map! "s-g" #'magit-status
      "C-c g" #'magit-status
      "s-G" #'magit-blame-addition
      "C-c G" #'magit-blame-addition)

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

(require 'org)
(require 'org-habit)

(after! org
  (setq org-attach-dir-relative t))

(with-eval-after-load 'flycheck
  (flycheck-add-mode 'proselint 'org-mode))

(map! :map org-mode-map
      "M-n" #'outline-next-visible-heading
      "M-p" #'outline-previous-visible-heading)

(setq org-src-window-setup 'current-window
      org-return-follows-link t
      org-babel-load-languages '((emacs-lisp . t)
                                 (python . t)
                                 (dot . t)
                                 (R . t))
      org-confirm-babel-evaluate nil
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

(defun rish/org-archive-done-tasks ()
  "Archive all done tasks."
  (interactive)
  (org-map-entries 'org-archive-subtree "/DONE" 'file))

(require 'find-lisp)
(setq rish/org-agenda-directory
      (expand-file-name "gtd/" org-directory))
(setq rish/org-roam-directory
      (expand-file-name "roam/" org-directory)) 
;; recursive search of org files. filter out certain directory from lookup by adding a array filter. Example, filtering out all org files in xxxx/xxx/daily/ directory
(setq org-agenda-files 
      (seq-filter (lambda(x) (not (string-match "/daily/"(file-name-directory x)))) 
       (directory-files-recursively rish/org-roam-directory "\\.org$")
       ))
;;(setq org-agenda-files 
;;      '((find-lisp-find-files rish/org-agenda-directory "\.org$") 
;;        (find-lisp-find-files "~/org/braindump/org/project/" "\.org$")))


;; javascript:location.href ='org-protocol://capture?template=c&url='+encodeURIComponent(location.href) +'&title='+encodeURIComponent(document.title)+'&body='+encodeURIComponent(window.getSelection())
(setq org-capture-templates
      `(("i" "Inbox" entry  (file "~/org/gtd/inbox.org")
         ,(concat "* TODO %?\n"
                  "/Entered on/ %U"))
        ("e" "Email" entry  (file+headline "gtd/emails.org" "Emails")
         ,(concat "* TODO [#A] Reply: %a :@study:@research:@work:\n"
                  "/Entered on/ %U") 
         :immediate-finish t)

        ("c" "org-protocol-capture" entry (file "~/org/gtd/inbox.org")
         ,(concat "* TODO [[%:link][%:description]]\n\n %i %?\n"
                   "/Entered on/ %U")
                  :immediate-finish t)
        ("w" 
         "Default Template" 
         entry  
         (file+headline "~/org/gtd/inbox.org" "Notes")
         ,(concat "* TODO %?\n"
                  "/Entered on/ %U")
         "* %^{Title}\n\n  Source: %u, %c\n\n  %i"
         :empty-lines 1)

        ("s" "Slipbox" entry  (file "braindump/org/inbox.org")
         "* %?\n")))

(defun rish/org-capture-inbox ()
  (interactive)
  (org-capture nil "i"))

(defun rish/org-capture-emails ()
  (interactive)
  (org-capture nil "e"))

(defun rish/org-protocol-capture ()
  (interactive)
  (org-capture nil "c"))

(defun rish/org-capture-default ()
  (interactive)
  (org-capture nil "w"))

(defun rish/org-capture-slipbox ()
  (interactive)
  (org-capture nil "s"))

(defun rish/org-agenda ()
  (interactive)
  (org-agenda nil " "))

(bind-key "C-c <tab>" #'rish/org-capture-inbox)
(bind-key "C-c SPC" #'rish/org-agenda)

;;(setq org-todo-keywords
;;      '((sequence "TODO(t)" "NEXT(n)" "HOLD(h)" "|" "DONE(d)")))
  ;; setup todo keywords

;; setup todo keywords
(setq org-todo-keywords
 '((sequence "TODO(t)" "PROJ(p)" "NEXT(n)" "|" "DONE(d!)")
   (sequence "WAITING(w@/!)"
             "HOLD(h@/!)"
             "|"
             "CANCELLED(c@/!)"
             "MEETING"))

 ;; use fast todo selection
 org-use-fast-todo-selection t
 ;; block parent until children are done
 org-enforce-todo-dependencies t
 ;; allo to fast fix todo state without triggering anything
 org-treat-S-cursor-todo-selection-as-state-change nil
 ;; setup state triggers
 org-todo-state-tags-triggers
 '(("CANCELLED" ("CANCELLED" . t))
   ("WAITING" ("WAITING" . t))
   ("HOLD" ("WAITING") ("HOLD" . t))
   (done ("WAITING") ("HOLD") ("FOCUS"))
   ("TODO" ("WAITING") ("CANCELLED") ("HOLD"))
   ("DONE" ("WAITING") ("CANCELLED") ("HOLD"))
   ("PROJ" ("WAITING") ("CANCELLED") ("HOLD"))
   ("NEXT" ("WAITING") ("CANCELLED") ("HOLD")))

 ;; use drawer for state changes
 org-log-into-drawer t)
;; The following are a collection of useful options for clocking, most taken from Matthew Lee Hinman, in his emacs blog series.
;; Resume clocking task when emacs is restarted
(org-clock-persistence-insinuate)
;; Save the running clock and all clock history when exiting Emacs, load it on startup
(setq org-clock-persist t)
;; Resume clocking task on clock-in if the clock is open
(setq org-clock-in-resume t)
;; prompt to resume an active clock
(setq org-clock-persist-query-resume t)
;; Change tasks to active when clocking in
;;(setq org-clock-in-switch-to-state "ACTV")

;; change tasks back to NEXT when clocking out, so it is marked in my agenda in its own area
(setq org-clock-out-switch-to-state "NEXT")
;; Save clock data and state changes and notes in the LOGBOOK drawer
(setq org-clock-into-drawer t)
;; Sometimes I change tasks I'm clocking quickly - this removes clocked tasks
;; with 0:00 duration
(setq org-clock-out-remove-zero-time-clocks t)
;; Clock out when moving task to a done state
(setq org-clock-out-when-done t)
;; Enable auto clock resolution for finding open clocks
(setq org-clock-auto-clock-resolution (quote when-no-clock-is-running))
;; Include current clocking task in clock reports
(setq org-clock-report-include-clocking-task t)
;; use pretty things for the clocktable
(setq org-pretty-entities t)


(defun log-todo-next-creation-date (&rest ignore)
  "Log NEXT creation time in the property drawer under the key 'ACTIVATED'"
  (when (and (string= (org-get-todo-state) "NEXT")
             (not (org-entry-get nil "ACTIVATED")))
    (org-entry-put nil "ACTIVATED" (format-time-string "[%Y-%m-%d]"))))
(add-hook 'org-after-todo-state-change-hook #'log-todo-next-creation-date)

(setq org-log-done 'time
      org-log-into-drawer t
      org-log-state-notes-insert-after-drawers nil)

(setq org-tag-alist '(("@errand" . ?e)
                      ("@research" . ?r)
                      ("@study" . ?s)
                      ("@work" . ?w)))

(setq org-fast-tag-selection-single-key nil)
(setq org-refile-use-outline-path 'file
      org-outline-path-complete-in-steps nil)
(setq org-refile-allow-creating-parent-nodes 'confirm
      org-refile-targets '(("projects.org" . (:level . 1))))

(defun rish/org-process-inbox ()
  "Called in org-agenda-mode, processes all inbox items."
  (interactive)
  (org-agenda-bulk-mark-regexp "inbox:")
  (rish/bulk-process-entries))

(defvar rish/org-current-effort "1:00"
  "Current effort for agenda items.")

(defun rish/my-org-agenda-set-effort (effort)
  "Set the effort property for the current headline."
  (interactive
   (list (read-string (format "Effort [%s]: " rish/org-current-effort) nil nil rish/org-current-effort)))
  (setq rish/org-current-effort effort)
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
        (funcall-interactively 'org-set-effort nil rish/org-current-effort)
        (end-of-line 1)
        (setq newhead (org-get-heading)))
      (org-agenda-change-all-lines newhead hdmarker))))

(defun rish/org-agenda-process-inbox-item ()
  "Process a single item in the org-agenda."
  (org-with-wide-buffer
   (org-agenda-set-tags)
   (org-agenda-priority)
   (call-interactively 'rish/my-org-agenda-set-effort)
   (org-agenda-refile nil nil t)))

(defun rish/bulk-process-entries ()
  (let ())
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
              (let (org-loop-over-headlines-in-active-region) (funcall 'rish/org-agenda-process-inbox-item))
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

(defun rish/org-inbox-capture ()
  (interactive)
  "Capture a task in agenda mode."
  (org-capture nil "i"))

(map! :map org-agenda-mode-map
      "i" #'org-agenda-clock-in
      "I" #'rish/clock-in-and-advance
      "r" #'rish/org-process-inbox
      "R" #'org-agenda-refile
      "c" #'rish/org-inbox-capture)

(defun rish/advance-todo ()
  (org-todo 'right)
  (remove-hook 'org-clock-in-hook #'rish/advance-todo))

(defun rish/clock-in-and-advance ()
  (interactive)
  (add-hook 'org-clock-in-hook 'rish/advance-todo)
  (org-agenda-clock-in))

(use-package! org-clock-convenience
  :bind (:map org-agenda-mode-map
         ("<S-up>" . org-clock-convenience-timestamp-up)
         ("<S-down>" . org-clock-convenience-timestamp-down)
         ("o" . org-clock-convenience-fill-gap)
         ("e" . org-clock-convenience-fill-gap-both)))

;; org Mac iCal
;; Enable org mac ical
(require 'org-mac-iCal)

;; Set calendar list:
(setq org-mac-iCal-range 10)

(use-package! org-agenda
  :init
  ;;(map! "<f1>" #'rish/switch-to-agenda)
  (setq org-agenda-block-separator nil
        org-agenda-start-with-log-mode t)
  ;;(defun rish/switch-to-agenda ()
  ;;  (interactive)
  ;;  (org-agenda nil " "))
  :config
  (defun rish/is-project-p ()
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

  (defun rish/skip-projects ()
    "Skip trees that are projects."
    (save-restriction
      (widen)
      (let ((next-headline (save-excursion (or (outline-next-heading) (point-max)))))
        (cond
         ((org-is-habit-p)
          next-headline)
         (t
          nil)))))

 )

 ;; (setq org-columns-default-format "%40ITEM(Task) %Effort(EE){:} %CLOCKSUM(Time Spent) %SCHEDULED(Scheduled) %DEADLINE(Deadline)")
 ;; (setq org-agenda-custom-commands `((" " "Agenda"
 ;;                                     ((alltodo ""
 ;;                                               ((org-agenda-overriding-header "Inbox")
 ;;                                                (org-agenda-files `(,(expand-file-name "gtd/inbox.org" org-directory)))))
 ;;                                      (agenda ""
 ;;                                              ((org-agenda-span 'week)
 ;;                                               (org-deadline-warning-days 365)))
 ;;                                      (todo "NEXT"
 ;;                                            ((org-agenda-overriding-header "In Progress")
 ;;                                             (org-agenda-files `(,(expand-file-name "gtd/projects.org" org-directory)))))
 ;;                                      (todo "TODO"
 ;;                                            ((org-agenda-overriding-header "Active Projects")
 ;;                                             (org-agenda-files `(,(expand-file-name "gtd/projects.org" org-directory)))
 ;;                                             (org-agenda-skip-function #'rish/skip-projects)))))))

(use-package! org-roam
  :init
  (map! :leader
        :prefix "n"
        :desc "org-roam" "l" #'org-roam-buffer-toggle
        :desc "org-roam-node-insert" "i" #'org-roam-node-insert
        :desc "org-roam-node-insert" "I" #'org-roam-node-insert-immediate
        :desc "org-roam-node-find" "f" #'org-roam-node-find
        :desc "org-roam-ref-find" "r" #'org-roam-ref-find
        :desc "org-roam-show-graph" "g" #'org-roam-show-graph
        :desc "rish/org-capture-slipbox" "<tab>" #'rish/org-capture-slipbox
        :desc "org-roam-capture" "c" #'org-roam-capture
        :desc "org-roam-capture-task" "t" #'my/org-roam-capture-task
        :desc "org-roam-capture-inbox" "b" #'my/org-roam-capture-inbox
        :desc "org-roam-find-project" "p" #'my/org-roam-find-project
        :desc "org-roam-dailies" "d" #'org-roam-dailies-map)
  (setq org-roam-directory (file-truename "~/org/roam")
        org-roam-db-gc-threshold most-positive-fixnum
        org-id-link-to-org-use-id t)
  :config
  (require 'org-roam-dailies) ;; Ensure the keymap is available
  (org-roam-db-autosync-mode +1)
  (org-roam-db-sync 'force)
  ;; (org-check-agenda-file org-link-set-parameters)
  (set-popup-rules!
    `((,(regexp-quote org-roam-buffer) ; persistent org-roam buffer
       :side right :width .33 :height .5 :ttl nil :modeline nil :quit nil :slot 1)
      ("^\\*org-roam: " ; node dedicated org-roam buffer
       :side right :width .33 :height .5 :ttl nil :modeline nil :quit nil :slot 2)))
  (add-hook 'org-roam-mode-hook #'turn-on-visual-line-mode)
  (setq org-roam-capture-templates
        '(
        ;; ("d" "default" plain "* TODO %?\n /Entered on/ %U"
        ;; :if-new (file+head "default/%<%Y%m%d%H%M%S>-${slug}.org"
        ;;                      "${title}\n")
        ;; :immediate-finish t
        ;; :unnarrowed t)
          ("d" "default" plain "%?"
           :if-new (file+head "default/%<%Y%m%d%H%M%S>-${slug}.org" 
                              "${title}\n#+date: %U\n")
           :unnarrowed t)
          ("m" "main" plain "%?"
           :if-new (file+head "main/%<%Y%m%d%H%M%S>-${slug}.org"
                              "${title}\n#+date: %U\n")
           :immediate-finish t
           :unnarrowed t)
          ("r" "reference" plain "%?"
           :if-new
           (file+head "reference/%<%Y%m%d%H%M%S>-${slug}.org" 
                      "${title}\n#+date: %U\n")
           :immediate-finish t
           :unnarrowed t)
          ;; the project template, used for projects WITH A DEADLINE
          ("p" "project" plain "* Overview\n\n* Tasks\n** TODO Set project name and deadline\n\n* Ideas\n\n* Notes\n\n* Meetings\n\n* Resources\n\n* PROJ projectname"
           :if-new (file+head "project/%<%Y%m%d%H%M%S>-${slug}.org" 
                              "${title}\n#+filetags: Project")
           :unnarrowed t)
           
          ;; the metaproject template, used for projects without a deadline
          ("P" "meta project" plain "* Overview\n\n* Tasks\n** TODO Add project name and set a work schedule\n\n* Thoughts\n\n* Notes\n\n* Meetings\n\n* Resources\n\n* PROJ projectname"
           :if-new (file+head "project/%<%Y%m%d%H%M%S>-${slug}.org" 
                              "${title}\n#+filetags: Metaproject")
           :unnarrowed t)
;;          ("p" "project" plain "%?"
;;           :if-new (file+head "project/%<%Y%m%d%H%M%S>-${slug}.org" 
;;                              "${title}\n#+filetags: Project\n#+date: %U\n")
;;           :unnarrowed t)

          ;; class note template, used for a class note for a class
          ("c" "class-note" plain "* Overview\n\n\n* Notes\n\n\n* References"
           :if-new (file+head "class-notes/%<%Y%m%d%H%M%S>-${slug}.org" 
                              "${title}\n#+filetags: classnote:classname:class")
           :unnarrowed t)

          ("w" "weekly goal setting" plain "* Goals\n\n* Action Items\n"
           :if-new (file+head "weekly/%<%Y%m%d%H%M%S>-${slug}.org" 
                              "${title}\n#+filetags: weeklygoals")
           :unnarrowed t)
          
          ("a" "article" plain "%?"
           :if-new
           (file+head "articles/%<%Y%m%d%H%M%S>-${slug}.org" 
                      "${title}\n#+filetags: :article:\n#+date: %U\n")
           :immediate-finish t
           :unnarrowed t)))

  (defun rish/tag-new-node-as-draft ()
    (org-roam-tag-add '("draft")))
  (add-hook 'org-roam-capture-new-node-hook #'rish/tag-new-node-as-draft)
  (set-company-backend! 'org-mode '(company-capf))
  (cl-defmethod org-roam-node-type ((node org-roam-node))
    "Return the TYPE of NODE."
    (condition-case nil
        (file-name-nondirectory
         (directory-file-name
          (file-name-directory
           (file-relative-name (org-roam-node-file node) org-roam-directory))))
      (error "")))
  (setq org-roam-node-display-template
        (concat "${type:15} ${title:*} " (propertize "${tags:10}" 'face 'org-tag)))
  (require 'citar)
  (defun rish/org-roam-node-from-cite (keys-entries)
    (interactive (list (citar-select-ref :multiple nil :rebuild-cache t)))
    (let ((title (citar--format-entry-no-widths (cdr keys-entries)
                                                "${author editor} :: ${title}")))
      (org-roam-capture- :templates
                         '(("r" "reference" plain "%?" :if-new
                            (file+head "reference/${citekey}.org"
                                       ":PROPERTIES: :ROAM_REFS: [cite:@${citekey}] 
                                       :END: #${title}\n")
                            :immediate-finish t
                            :unnarrowed t))
                         :info (list :citekey (car keys-entries))
                         :node (org-roam-node-create :title title)
                         :props '(:finalize find-file)))))

(use-package! vulpea
  :ensure t
  :after (org-agenda org-roam)
  :init
  (map! :leader
        :prefix "v"
        :desc "vulpea-insert" "i" #'vulpea-insert
        :desc "vulpea-tags-add" "t" #'vulpea-tags-add
        :desc "vulpea-tags-delete" "T" #'vulpea-tags-delete
        :desc "vulpea-agenda-main" "a" #'vulpea-agenda-main
        :desc "vulpea-agenda-person" "p" #'vulpea-agenda-person
        :desc "vulpea-find" "f" #'vulpea-find
        :desc "vulpea-capture-task" "c" #'vulpea-capture-task
        :desc "vulpea-capture-meeting" "m" #'vulpea-capture-meeting
        )
  (add-to-list 'window-buffer-change-functions 
                #'vulpea-setup-buffer)
  ;; pretty org files
  (setq
   org-adapt-indentation nil
   org-hidden-keywords nil
   org-hide-emphasis-markers nil
   org-hide-leading-stars nil
   org-image-actual-width '(512)
   org-imenu-depth 1
   org-pretty-entities nil
   org-startup-folded t)

  ;; do not allow invisible edits (...)
  (setq org-catch-invisible-edits 'error)

  ;; formatting for properties
  (setq org-property-format "%-24s %s")
  
  ;; hook into org-roam-db-autosync-mode you wish to enable
  ;; persistence of meta values (see respective section in README to
  ;; find out what meta means)
  :config
  (load! "vulpea")
  (load! "vulpea-agenda")
  (load! "vulpea-capture")
  ;; Disable vulpea's custom agenda commands
  (load! "lib-vulpea-agenda")
  ;; avoid noisy `org-check-agenda-file'
  (advice-add #'org-check-agenda-file :around 
              #'vulpea-check-agenda-file)
  (add-hook 'vulpea-insert-handle-functions 
            #'vulpea-insert-handle)

  (advice-add 'org-agenda :before #'vulpea-agenda-files-update)

  ;; prevent headings from clogging tag
  ;; tags
  (setq org-tag-persistent-alist '(("FOCUS" . ?f) 
                                   ("PROJECT" . ?p)))
  (setq org-use-tag-inheritance t)
  (setq org-tags-exclude-from-inheritance '("project" 
                                            "people"))
  (setq-default vulpea-find-default-filter 
                (lambda (note) 
                  (= (vulpea-note-level note) 0)) 
                vulpea-insert-default-filter 
                (lambda (note) 
                  (= (vulpea-note-level note) 0)))

  :hook ((org-roam-db-autosync-mode . vulpea-db-autosync-enable)))

(use-package! org-super-agenda 
  :commands org-super-agenda-mode
  ;; Taken from https://github.com/claykaufmann/dotfiles/blob/main/doom/.doom.d/config.org 
  ;; Set the org agenda prefix format. This removes roam date titles from the agenda view mainly. (again, from Boris Buliga in his Task Management with Org Roam series) For todo’s, I used this stack overflow post to add the deadline to the todo tag. Being able to view the deadline in task view was extremely important to me, and this accomplishes that.
  :config
  (setq org-agenda-prefix-format
        '((agenda . " %i %(vulpea-agenda-category 18)%?-14t% s")
          (todo . " %i %(vulpea-agenda-category 18) %-11(let ((deadline (org-get-deadline-time (point)))) (if deadline (format-time-string \"%Y-%m-%d\" deadline) \"\")) ")
          (tags . " %i %(vulpea-agenda-category 18) %t ")
          (search . " %i %(vaulpea-agenda-category 18) %t ")))

  (setq org-agenda-use-time-grid t)
  (setq org-agenda-time-grid
      (quote
       ((daily today remove-match)
        (0900 1000 1100 1200 1300 1400 1500 1600 1700 1800 1900 2000 2100 2200 2300 2359 0100 0200)
        "......" "----------------")))

  ;;(setq org-agenda-time-grid '((daily today require-timed) "----------------------" nil)
  (setq org-agenda-skip-scheduled-if-done t
        org-agenda-skip-deadline-if-done t
        org-agenda-include-deadlines t
        org-agenda-include-diary t
        org-agenda-block-separator nil
        org-agenda-compact-blocks t
        org-agenda-start-with-log-mode t
        org-deadline-warning-days 7  
        org-agenda-start-day nil) ;; i.e. today

  (setq org-agenda-sorting-strategy '((agenda deadline-up  habit-down time-up 
                                              priority-down timestamp-down category-keep)))
  ;; Agenda Styling
  ;; Add an extra line after each day for better spacing in the default agenda.
  (setq org-agenda-format-date
            (lambda (date)
              (concat "\n" (org-agenda-format-date-aligned date))))

  (let ((org-super-agenda-group '((:auto-category t)))) (org-agenda-list))
  
  ;; set the span of the default agenda to be a week
  (setq org-agenda-span 10)

  ;; show deadlines
  ;; Custom Command Agenda Views
  ;; Add custom views:
  (setq org-agenda-custom-commands
  
        ;; a refiling view
        '(("r" "Things to refile"
           ((tags
             "REFILE"
             ((org-agenda-overriding-header "To refile:")
              (org-tags-match-list-sublevels nil)))))
  
          ;; the day view (used most often)
          ("d" "Day View"
  
           ;; show the base agenda
           ((agenda "" ((org-agenda-span 'day)
                        ;; enable the diary in the daily view so I can see how classes fit into the day
                        (org-agenda-include-diary t) 
                        ;; add a hook to call org mac iCal
                        (org-agenda-mode-hook (lambda () (org-mac-iCal)))
                        ;; add 7 days of warning to get things due this week
                        (org-deadline-warning-days 7)
                        ;; set super agenda groups
                        (org-super-agenda-groups
                          ;; main group of today to show the time grid
                         '((:name "Today"
                            :time-grid t
                            :date today
                            :order 1
                            )
                           ;; second group to show all tasks due this week (using deadline-warning-days)
                           (:name "Due this week"
                            :todo t
                            :order 4)))))
  
            ;; show a bunch of different todo groups
            (alltodo "" ((org-agenda-overriding-header "")
                         (org-super-agenda-groups
                          ;; next up are all todos marked NEXT
                          '((:name "Next up"
                             :todo "NEXT"
                             :discard (:todo "PROJ")
                             :discard (:tag "REFILE")
                             :order 1)
                            ;; all taks with a priority of A
                            (:name "Important"
                             :priority "A"
                             :order 3)
                            ;; tasks that are estimated to be less than 30 minutes
                            (:name "Quick Picks"
                             :effort< "0:30"
                             :order 5)
                            ;; overdue tasks
                            (:name "Overdue"
                             :deadline past
                             :order 4)
                            (:name "Due Today"
                             :deadline today
                             :order 2)
                            (:name "Due Soon"
                             :deadline future
                             :order 8)
                            (:name "Overdue"
                             :deadline past
                             :face error
                             :order 7)
                            (:name "Issues"
                             :tag "Issue"
                             :order 11)
  
                            ;; tasks with no due date
                            (:name "No due date"
                             :deadline nil
                             :order 70)
  
                            ;; emacs related tasks (before projects to separate them)
                            (:name "Emacs"
                             :tag "emacs"
                             :order 12)
  
                            ;; all projects, hide the PROJ tag to avoid duplication (the tag will appear if the due date is coming up in the top week section)
                            (:name "Projects"
                             :todo "TODO"
                             :discard (:todo "PROJ")
                             :tag ("Project" "Metaproject")
                             :order 9)

                            (:name "Others"
                             :deadline t
                             :order 10) 
                            ;; discard all things with the REFILE tag, as they will appear in the next group
                            (:discard (:tag "REFILE")
                             :order 80)
                            ))))
  
            ;; refile section, to show anything that should be refiled
            (tags "REFILE" ((org-agenda-overriding-header "To Refile:")))))))

  ;; We now set a bunch of custom faces for different org agenda variables, to make the custom org agenda look much better.
  (custom-set-faces!
  ;; set the agenda structure font (heading) mainly used to change the color of super agenda group names
    '(org-agenda-structure :slant italic :foreground "green3" :width semi-expanded )
    ;; set the shceduled today font (for some reason it defaults to being dimmed, which was not nice)
    '(org-scheduled-today :foreground "MediumPurple1")
    ;; by default this is white, add some color to make it pop on the time grid
    '(org-agenda-diary :foreground "goldenrod1"))
              
)

;; Moved outside the use-package! agenda so it shows on the home screen
(defun rish/switch-to-agenda ()
    (interactive)
    (org-agenda nil "o"))

(after! org-agenda
   (org-super-agenda-mode))

(use-package! thrift)

(after! company
  (map! "M-/" #'company-complete))

(use-package! company-posframe
  :hook (company-mode . company-posframe-mode))

(use-package! ox-hugo
  :after org)

(use-package! mathpix.el
  :commands (mathpix-screenshot)
  :init
  (map! "C-x m" #'mathpix-screenshot)
  :config
  (setq mathpix-screenshot-method "xfce4-screenshooter -r -o cat > %s"
        mathpix-app-id (with-temp-buffer (insert-file-contents "./secrets/mathpix-app-id") (buffer-string))
        mathpix-app-key (with-temp-buffer (insert-file-contents "./secrets/mathpix-app-key") (buffer-string))))

(defun insert-date ()
  "Insert a timestamp according to locale's date and time format."
  (interactive)
  (insert (format-time-string "%c" (current-time))))

(use-package! outshine
  :commands (outshine-mode))

(after! bibtex-completion
  (setq! bibtex-completion-notes-path org-roam-directory
         bibtex-completion-bibliography rish/default-bibliography
         org-cite-global-bibliography rish/default-bibliography
         bibtex-completion-pdf-field "file"))

(after! bibtex-completion
  (after! org-roam
    (setq! bibtex-completion-notes-path org-roam-directory)))

;;(defun file-name-concat (&rest parts)
;;  (reduce (lambda (a b) (expand-file-name b a)) parts))

(after! citar
  (map! :map org-mode-map
        :desc "Insert citation" "C-c b" #'citar-insert-citation)
  (setq citar-bibliography rish/default-bibliography
        citar-at-point-function 'embark-act
        citar-symbol-separator "  "
        citar-format-reference-function 'citar-citeproc-format-reference
        org-cite-csl-styles-dir "~/Zotero/styles"
        citar-citeproc-csl-styles-dir org-cite-csl-styles-dir
        citar-citeproc-csl-locales-dir "~/Zotero/locales"
        citar-citeproc-csl-style (concat org-cite-csl-styles-dir "apa.csl")))

(use-package! nov
  :hook (nov-mode . variable-pitch-mode)
  :mode ("\\.\\(epub\\|mobi\\)\\'" . nov-mode))

(after! org-noter
  org-noter-doc-split-fraction '(0.57 0.43))

(use-package! yaml-mode
  :mode ("\\.yml\\'" . yaml-mode))

(use-package! emmet-mode
  :hook
  ((sgml-mode . emmet-mode)
   (css-mode . emmet-mode)))


(defun rish/open-with (arg)
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

(map! "C-c o o" 'rish/open-with)

(after! org-latex
  (setq org-latex-pdf-process (list "latexmk -f -xelatex %f")))

(map!
 [backtab] #'+fold/toggle
 [C-tab] #'+fold/open-all
 [C-iso-lefttab] #'+fold/close-all)

(global-set-key (kbd "s-<right>") 'move-end-of-line)
(global-set-key (kbd "s-z") 'undo)
(global-set-key (kbd "s-c") 'copy)
(global-set-key (kbd "s-v") 'paste)

;; (org-roam-db-sync 'force)

;;(use-package! abnormal
;;  :config
;;  (abnormal-mode))


;; Doom exposes five (optional) variables for controlling fonts in Doom. Here
;; are the three important ones:
;;
;; + `doom-font'
;; + `doom-variable-pitch-font'
;; + `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;;
;; They all accept either a font-spec, font string ("Input Mono-12"), or xlfd
;; font string. You generally only need these two:
;; (setq doom-font (font-spec :family "monospace" :size 15 :weight 'semi-light)
;;       doom-variable-pitch-font (font-spec :family "sans" :size 16))

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
;;(setq doom-theme 'doom-one)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
;;(setq org-directory "~/org/")

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)

(global-visual-line-mode t)

;;(use-package! emacqsql-sqlite3)

(use-package! mixed-pitch
  :hook
  ;; If you want it in all text modes:
  (text-mode . mixed-pitch-mode))

;; Here are some additional functions/macros that could help you configure Doom:
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.
