;; System crafters functions

(defun org-roam-node-insert-immediate (arg &rest args)
  (interactive "P")
  (let ((args (push arg args))
        (org-roam-capture-templates (list (append (car org-roam-capture-templates)
                                                  '(:immediate-finish t)))))
    (apply #'org-roam-node-insert args)))

(defun my/org-roam-filter-by-tag (tag-name)
  (lambda (node)
    (member tag-name (org-roam-node-tags node))))

;;(defun my/org-roam-list-notes-by-tag (tag-name)
;;  (mapcar #'org-roam-node-file
;;          (seq-filter
;;           (my/org-roam-filter-by-tag tag-name)
;;           (org-roam-node-list))))

;;(defun my/org-roam-refresh-agenda-list ()
;;  (interactive)
;;  (setq org-agenda-files (my/org-roam-list-notes-by-tag "Project")))

;; Build the agenda list the first time for the session
;;(my/org-roam-refresh-agenda-list)

(defun my/org-roam-project-finalize-hook ()
  "Adds the captured project file to `org-agenda-files' if the
capture was not aborted."
  ;; Remove the hook since it was added temporarily
  (remove-hook 'org-capture-after-finalize-hook #'my/org-roam-project-finalize-hook)

  ;; Add project file to the agenda list if the capture was confirmed
  (unless org-note-abort
    (with-current-buffer (org-capture-get :buffer)
      (add-to-list 'org-agenda-files (buffer-file-name)))))

(defun my/org-roam-find-project ()
  (interactive)
  ;; Add the project file to the agenda after capture is finished
  (add-hook 'org-capture-after-finalize-hook #'my/org-roam-project-finalize-hook)

  ;; Select a project file to open, creating it if necessary
  (org-roam-node-find
   nil
   nil
   (my/org-roam-filter-by-tag "Project")
   :templates
   '(("p" "project" plain "* Goals\n\n%?\n\n* Tasks\n\n** TODO Add initial tasks\n\n* Dates\n\n"
      :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n#+category: ${title}\n#+filetags: Project")
      :unnarrowed t))))

(defun my/org-roam-capture-inbox ()
  (interactive)
  (org-roam-capture- :node (org-roam-node-create)
                     :templates '(("i" "inbox" plain "* %?"
                                  :if-new (file+head "Inbox.org" "#+title: Inbox\n")))))

(defun my/org-roam-capture-task ()
  (interactive)
  ;; Add the project file to the agenda after capture is finished
  (add-hook 'org-capture-after-finalize-hook #'my/org-roam-project-finalize-hook)

  ;; Capture the new task, creating the project file if necessary
  (org-roam-capture- :node (org-roam-node-read
                            nil
                            (my/org-roam-filter-by-tag "Project"))
                     :templates '(("p" "project" plain "** TODO %?"
                                   :if-new (file+head+olp "%<%Y%m%d%H%M%S>-${slug}.org"
                                                          "#+title: ${title}\n#+category: ${title}\n#+filetags: Project"
                                                          ("Tasks"))))))

(defun my/org-roam-copy-todo-to-today ()
  (interactive)
  (let ((org-refile-keep t) ;; Set this to nil to delete the original!
        (org-roam-dailies-capture-templates
          '(("t" "tasks" entry "%?"
             :if-new (file+head+olp "%<%Y-%m-%d>.org" "#+title: %<%Y-%m-%d>\n" ("Tasks")))))
        (org-after-refile-insert-hook #'save-buffer)
        today-file
        pos)
    (save-window-excursion
      (org-roam-dailies--capture (current-time) t)
      (setq today-file (buffer-file-name))
      (setq pos (point)))

    ;; Only refile if the target file is different than the current file
    (unless (equal (file-truename today-file)
                   (file-truename (buffer-file-name)))
      (org-refile nil nil (list "Tasks" today-file nil pos)))))

(defun org-roam-node-list ()
  "Return all nodes stored in the database as a list of `org-roam-node's."
  (let ((rows (org-roam-db-query
               "SELECT
  id,
  file,
  filetitle,
  \"level\",
  todo,
  pos,
  priority ,
  scheduled ,
  deadline ,
  title,
  properties ,
  olp,
  atime,
  mtime,
  '(' || group_concat(tags, ' ') || ')' as tags,
  aliases,
  refs
FROM
  (
  SELECT
    id,
    file,
    filetitle,
    \"level\",
    todo,
    pos,
    priority ,
    scheduled ,
    deadline ,
    title,
    properties ,
    olp,
    atime,
    mtime,
    tags,
    '(' || group_concat(aliases, ' ') || ')' as aliases,
    refs
  FROM
    (
    SELECT
      nodes.id as id,
      nodes.file as file,
      nodes.\"level\" as \"level\",
      nodes.todo as todo,
      nodes.pos as pos,
      nodes.priority as priority,
      nodes.scheduled as scheduled,
      nodes.deadline as deadline,
      nodes.title as title,
      nodes.properties as properties,
      nodes.olp as olp,
      files.atime as atime,
      files.mtime as mtime,
      files.title as filetitle,
      tags.tag as tags,
      aliases.alias as aliases,
      '(' || group_concat(RTRIM (refs.\"type\", '\"') || ':' || LTRIM(refs.ref, '\"'), ' ') || ')' as refs
    FROM nodes
    LEFT JOIN files ON files.file = nodes.file
    LEFT JOIN tags ON tags.node_id = nodes.id
    LEFT JOIN aliases ON aliases.node_id = nodes.id
    LEFT JOIN refs ON refs.node_id = nodes.id
    GROUP BY nodes.id, tags.tag, aliases.alias )
  GROUP BY id, tags )
GROUP BY id")))
    (cl-loop for row in rows
             append (pcase-let* ((`(,id ,file ,file-title ,level ,todo ,pos ,priority ,scheduled ,deadline
                                        ,title ,properties ,olp ,atime ,mtime ,tags ,aliases ,refs)
                                  row)
                                 (all-titles (cons title aliases)))
                      (mapcar (lambda (temp-title)
                                (org-roam-node-create :id id
                                                      :file file
                                                      :file-title file-title
                                                      :file-atime atime
                                                      :file-mtime mtime
                                                      :level level
                                                      :point pos
                                                      :todo todo
                                                      :priority priority
                                                      :scheduled scheduled
                                                      :deadline deadline
                                                      :title temp-title
                                                      :aliases aliases
                                                      :properties properties
                                                      :olp olp
                                                      :tags tags
                                                      :refs refs))
                              all-titles)))))


;;(add-to-list 'org-after-todo-state-change-hook
;;        (lambda ()
;;             (when (equal org-state "DONE")
;;                (my/org-roam-copy-todo-to-today))))
