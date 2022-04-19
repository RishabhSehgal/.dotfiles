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
      `(("i" "Inbox" entry  (file "gtd/inbox.org")
         ,(concat "* TODO %?\n"
                  "/Entered on/ %U"))
        ("e" "Email" entry  (file+headline "gtd/emails.org" "Emails")
         ,(concat "* TODO [#A] Reply: %a :@study:@research:@work:\n"
                  "/Entered on/ %U") 
         :immediate-finish t)

        ("c" "org-protocol-capture" entry (file "gtd/inbox.org")
         ,(concat "* TODO [[%:link][%:description]]\n\n %i %?\n"
                   "/Entered on/ %U")
                  :immediate-finish t)
        ("w" 
         "Default Template" 
         entry  
         (file+headline "gtd/inbox.org" "Notes")
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
(setq
   org-todo-keywords
   '((sequence "TODO(t)" "|" "DONE(d!)")
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
     ("DONE" ("WAITING") ("CANCELLED") ("HOLD")))

   ;; use drawer for state changes
   org-log-into-drawer t)

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

(use-package! org-agenda
  :init
  (map! "<f1>" #'rish/switch-to-agenda)
  (setq org-agenda-block-separator nil
        org-agenda-start-with-log-mode t)
  (defun rish/switch-to-agenda ()
    (interactive)
    (org-agenda nil " "))
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

  (setq org-columns-default-format "%40ITEM(Task) %Effort(EE){:} %CLOCKSUM(Time Spent) %SCHEDULED(Scheduled) %DEADLINE(Deadline)")
  (setq org-agenda-custom-commands `((" " "Agenda"
                                      ((alltodo ""
                                                ((org-agenda-overriding-header "Inbox")
                                                 (org-agenda-files `(,(expand-file-name "gtd/inbox.org" org-directory)))))
                                       (agenda ""
                                               ((org-agenda-span 'week)
                                                (org-deadline-warning-days 365)))
                                       (todo "NEXT"
                                             ((org-agenda-overriding-header "In Progress")
                                              (org-agenda-files `(,(expand-file-name "gtd/projects.org" org-directory)))))
                                       (todo "TODO"
                                             ((org-agenda-overriding-header "Active Projects")
                                              (org-agenda-files `(,(expand-file-name "gtd/projects.org" org-directory)))
                                              (org-agenda-skip-function #'rish/skip-projects))))))))

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
          ("m" "main" plain
           "%?"
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
          ("p" "project" plain "%?"
           :if-new (file+head "project/%<%Y%m%d%H%M%S>-${slug}.org" 
                              "${title}\n#+filetags: Project\n#+date: %U\n")
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
                                       ":PROPERTIES:
:ROAM_REFS: [cite:@${citekey}]
:END:
#+title: ${title}\n")
                            :immediate-finish t
                            :unnarrowed t))
                         :info (list :citekey (car keys-entries))
                         :node (org-roam-node-create :title title)
                         :props '(:finalize find-file)))))

(use-package! org-super-agenda)
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

(use-package! vulpea
  :ensure t
  :after org-roam
  :init
  (map! :leader
        :prefix "v"
        :desc "vulpea-insert" "i" #'vulpea-insert
        :desc "vulpea-tags-add" "t" #'vulpea-tags-add
        :desc "vulpea-tags-delete" "T" #'vulpea-tags-delete
        :desc "vulpea-agenda-files" "a" #'vulpea-agenda-files
        )
  ;; hook into org-roam-db-autosync-mode you wish to enable
  ;; persistence of meta values (see respective section in README to
  ;; find out what meta means)
  :config
  (load! "vulpea-agenda")
  (add-hook 'vulpea-insert-handle-functions 
            #'vulpea-insert-handle)
  ;; prevent headings from clogging tag  
  (setq org-tags-exclude-from-inheritance '("project" 
                                            "people"))
  :hook ((org-roam-db-autosync-mode . vulpea-db-autosync-enable)))

(org-roam-db-sync 'force)

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
