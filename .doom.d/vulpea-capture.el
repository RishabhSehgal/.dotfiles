(defvar vulpea-capture-inbox-file
  (format "inbox-%s.org" system-name)
  "The path to the inbox file.
It is relative to `vulpea-directory', unless it is absolute.")

;;;###autoload
(defun vulpea-capture-setup ()
  "Wire all bits for capturing."
  (dolist (var '(vulpea-capture-inbox-file))
    (set var (expand-file-name (symbol-value var) vulpea-directory)))
  (unless org-default-notes-file
    (setq org-default-notes-file vulpea-capture-inbox-file)))

(setq org-roam-dailies-directory "~/org/roam/daily")
(setq
   org-capture-templates
   '(("t" "todo" plain (file vulpea-capture-inbox-file)
      "* TODO %?\n%U\n" :clock-in t :clock-resume t)

     ("m" "Meeting" entry
      (function vulpea-capture-meeting-target)
      (function vulpea-capture-meeting-template)
      :clock-in t
      :clock-resume t))
   org-roam-capture-templates
   '(("d" "default" plain "%?"
      :if-new (file+head
               "~/org/roam/default/%<%Y%m%d%H%M%S>-${slug}.org"
               "#+title: ${title}\n\n")
      :unnarrowed t))
   org-roam-dailies-capture-templates
   `(("d" "default" entry
      "* %<%H:%M>\n\n%?"
      :if-new (file+head
               ,(expand-file-name "%<%Y-%m-%d>.org"
                                  org-roam-dailies-directory)
               ,(string-join '("#+title: %<%A, %d %B %Y>"
                               "#+filetags: journal"
                               "\n")
                             "\n")))))

;;;###autoload
(defun vulpea-capture-task ()
  "Capture a task."
  (interactive)
  (org-capture nil "t"))

(defun vulpea-capture-meeting ()
  "Capture a meeting."
  (interactive)
  (org-capture nil "m"))

(defun vulpea-capture-meeting-target ()
  "Return a target for a meeting capture."
  (let ((person (org-capture-get :meeting-person)))
    ;; unfortunately, I could not find a way to reuse
    ;; `org-capture-set-target-location'
    (if (vulpea-note-id person)
        (let ((path (vulpea-note-path person))
              (headline "Meetings"))
          (set-buffer (org-capture-target-buffer path))
          ;; Org expects the target file to be in Org mode, otherwise
          ;; it throws an error. However, the default notes files
          ;; should work out of the box. In this case, we switch it to
          ;; Org mode.
          (unless (derived-mode-p 'org-mode)
            (org-display-warning
             (format
              "Capture requirement: switching buffer %S to Org mode"
              (current-buffer)))
            (org-mode))
          (org-capture-put-target-region-and-position)
          (widen)
          (goto-char (point-min))
          (if (re-search-forward
               (format org-complex-heading-regexp-format
                       (regexp-quote headline))
               nil t)
              (beginning-of-line)
            (goto-char (point-max))
            (unless (bolp) (insert "\n"))
            (insert "* " headline "\n")
            (beginning-of-line 0)))
      (let ((path vulpea-capture-inbox-file))
        (set-buffer (org-capture-target-buffer path))
        (org-capture-put-target-region-and-position)
        (widen)))))

(defun vulpea-capture-meeting-template ()
  "Return a template for a meeting capture."
  (let ((person (vulpea-select
                 "Person"
                 :filter-fn
                 (lambda (note)
                   (let ((tags (vulpea-note-tags note)))
                     (seq-contains-p tags "people"))))))
    (org-capture-put :meeting-person person)
    (if (vulpea-note-id person)
        "* MEETING [%<%Y-%m-%d %a>] :REFILE:MEETING:\n%U\n\n%?"
      (concat "* MEETING with "
              (vulpea-note-title person)
              " on [%<%Y-%m-%d %a>] :MEETING:\n%U\n\n%?"))))

(defvar vulpea-capture-person-template
  `("d" "default" plain
    #'org-roam-capture--get-point
    "%?"
    :file-name "people/%<%Y%m%d%H%M%S>-${slug}"
    :head ,(concat
            ":PROPERTIES:\n"
            ":ID:                     ${id}\n"
            ":END:\n"
            "#+TITLE: ${title}\n"
            "\n")
    :unnarrowed t
    :immediate-finish t)
  "Capture template for person.
Variables in the capture context are provided by
`vulpea-create'.")

(defvar vulpea-capture-article-template
  `("d" "default" plain
    #'org-roam-capture--get-point
    "%?"
    :file-name "litnotes/%<%Y%m%d%H%M%S>-${slug}"
    :head ,(concat
            ":PROPERTIES:\n"
            ":ID:                     ${id}\n"
            ":END:\n"
            "#+TITLE: ${title}\n"
            "#+ROAM_KEY: ${url}\n"
            "#+ROAM_TAGS: Content:Article Status:New\n"
            "\n")
    :unnarrowed t
    :immediate-finish t)
  "Capture template for article.
Variables in the capture context are provided by
`vulpea-capture-article'.")

;;;###autoload
(defun vulpea-capture-article ()
  "Capture an article using `vulpea-capture-article-template'.
User is asked to provide an URL, title and authors of the article
being captured.
Title is inferred from URL, but user may edit it.
Authors can be created on the fly. See
`vulpea-capture-person-template' for more information."
  (interactive)
  (when-let*
      ((url (read-string "URL: "))
       (title (org-cliplink-retrieve-title-synchronously url))
       (title (read-string "Title: " title))
       (people (fun-collect-while
                (lambda ()
                  (let ((person
                         (vulpea-select
                          "Person"
                          :filter-fn
                          (lambda (note)
                            (let ((tags (vulpea-note-tags note)))
                              (seq-contains-p tags "people"))))))
                    (if (vulpea-note-id person)
                        person
                      (vulpea-create
                       (vulpea-note-title person)
                       vulpea-capture-person-template))))
                nil))
       (note (vulpea-create title
                            vulpea-capture-article-template
                            (list (cons 'url url)))))
    (vulpea-utils-with-note note
      (vulpea-meta-set note "authors" people t))
    (find-file (vulpea-note-path note))))

(defun vulpea-capture-journal ()
  "Capture a journal entry.
By default it uses current date to find a daily. With
\\[universal-argument] user may select the date."
  (interactive)
  (cond
   ((equal current-prefix-arg '(4))     ; select date
    (org-roam-dailies-capture-date))
   (t
    (org-roam-dailies-capture-today))))
