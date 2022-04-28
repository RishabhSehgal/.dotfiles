(defvar vulpea-agenda-hide-scheduled-and-waiting-next-tasks t
  "Non-nil means to hide scheduled and waiting tasks.
Affects the following commands:
- `vulpea-agenda-cmd-focus'
- `vulpea-agenda-cmd-waiting'")

(defvar vulpea-agenda-main-buffer-name "*agenda:main*"
  "Name of the main agenda buffer.")


;;;###autoload
(defun vulpea-agenda-main ()
  "Show main `org-agenda' view."
  (interactive)
  (org-agenda nil " "))

;;;###autoload
(defun vulpea-agenda-person ()
  "Show main `org-agenda' view."
  (interactive)
  (let* ((person (vulpea-select-from
                  "Person"
                  (vulpea-db-query-by-tags-some '("people"))))
         (node (org-roam-node-from-id (vulpea-note-id person)))
         (names (cons (org-roam-node-title node)
                      (org-roam-node-aliases node)))
         (tags (seq-map #'vulpea--title-to-tag names))
         (query (string-join tags "|")))
    (dlet ((org-agenda-overriding-arguments (list t query)))
      (org-agenda nil "M"))))



;; Commands

;;;###autoload
(defconst vulpea-agenda-cmd-refile
  '(tags
    "REFILE"
    ((org-agenda-overriding-header "To refile")
     (org-tags-match-list-sublevels nil))))

;;;###autoload
(defconst vulpea-agenda-cmd-today
  '(agenda
    ""
    ((org-agenda-span 'day)
     (org-agenda-skip-deadline-prewarning-if-scheduled t)
     (org-agenda-sorting-strategy '(habit-down
                                    time-up
                                    category-keep
                                    todo-state-down
                                    priority-down)))))

;;;###autoload
(defconst vulpea-agenda-cmd-focus
  '(tags-todo
    "FOCUS"
    ((org-agenda-overriding-header
      (concat "To focus on"
              (if vulpea-agenda-hide-scheduled-and-waiting-next-tasks
                  ""
                " (including WAITING and SCHEDULED tasks)")))
     (org-agenda-skip-function 'vulpea-agenda-skip-habits)
     (org-tags-match-list-sublevels t)
     (org-agenda-todo-ignore-scheduled
      vulpea-agenda-hide-scheduled-and-waiting-next-tasks)
     (org-agenda-todo-ignore-deadlines
      vulpea-agenda-hide-scheduled-and-waiting-next-tasks)
     (org-agenda-todo-ignore-with-date
      vulpea-agenda-hide-scheduled-and-waiting-next-tasks)
     (org-agenda-tags-todo-honor-ignore-options t)
     (org-agenda-sorting-strategy
      '(todo-state-down priority-down effort-up category-keep)))))

;;;###autoload
(defconst vulpea-agenda-cmd-stuck-projects
  '(tags-todo
    "PROJECT-CANCELLED-HOLD/!"
    ((org-agenda-overriding-header "Stuck Projects")
     (org-agenda-skip-function 'vulpea-agenda-skip-non-stuck-projects)
     (org-agenda-sorting-strategy
      '(todo-state-down priority-down effort-up category-keep)))))

;;;###autoload
(defconst vulpea-agenda-cmd-projects
  '(tags-todo
    "PROJECT-HOLD"
    ((org-agenda-overriding-header (concat "Projects"))
     (org-tags-match-list-sublevels t)
     (org-agenda-skip-function 'vulpea-agenda-skip-non-projects)
     (org-agenda-tags-todo-honor-ignore-options t)
     (org-agenda-sorting-strategy
      '(todo-state-down priority-down effort-up category-keep)))))

;;;###autoload
(defconst vulpea-agenda-cmd-waiting
  '(tags-todo
    "-CANCELLED+WAITING-READING-FOCUS|+HOLD/!"
    ((org-agenda-overriding-header
      (concat "Waiting and Postponed Tasks"
              (if vulpea-agenda-hide-scheduled-and-waiting-next-tasks
                  ""
                " (including WAITING and SCHEDULED tasks)")))
     (org-agenda-skip-function 'vulpea-agenda-skip-non-tasks)
     (org-tags-match-list-sublevels nil)
     (org-agenda-todo-ignore-scheduled
      vulpea-agenda-hide-scheduled-and-waiting-next-tasks)
     (org-agenda-todo-ignore-deadlines
      vulpea-agenda-hide-scheduled-and-waiting-next-tasks))))

;; Utilities to build agenda commands -- skip

;;;###autoload
(defun vulpea-agenda-skip-habits ()
  "Skip tasks that are habits."
  (save-restriction
    (widen)
    (let ((subtree-end (save-excursion (org-end-of-subtree t))))
      (cond
       ((org-is-habit-p)
        subtree-end)
       (t
        nil)))))

;;;###autoload
(defun vulpea-agenda-skip-non-projects ()
  "Skip trees that are not projects."
  (if (save-excursion (vulpea-agenda-skip-non-stuck-projects))
      (save-restriction
        (widen)
        (let ((subtree-end (save-excursion (org-end-of-subtree t))))
          (cond
           ((vulpea-agenda-project-p)
            nil)
           ((and (vulpea-agenda-project-subtree-p)
                 (not (vulpea-agenda-task-p)))
            nil)
           (t
            subtree-end))))
    (save-excursion (org-end-of-subtree t))))

;;;###autoload
(defun vulpea-agenda-skip-non-tasks ()
  "Show non-project tasks.
Skip project and sub-project tasks, habits, and project related
tasks."
  (save-restriction
    (widen)
    (let ((next-headline (save-excursion
                           (or (outline-next-heading)
                               (point-max)))))
      (cond
       ((vulpea-agenda-task-p)
        nil)
       (t
        next-headline)))))

;;;###autoload
(defun vulpea-agenda-skip-non-stuck-projects ()
  "Skip trees that are not stuck projects."
  (save-restriction
    (widen)
    (let ((next-headline (save-excursion
                           (or (outline-next-heading)
                               (point-max)))))
      (if (vulpea-agenda-project-p)
          (let* ((subtree-end (save-excursion
                                (org-end-of-subtree t)))
                 (has-next ))
            (save-excursion
              (forward-line 1)
              (while (and (not has-next)
                          (< (point) subtree-end)
                          (re-search-forward "^\\*+ TODO "
                                             subtree-end t))
                (unless (member "WAITING" (org-get-tags))
                  (setq has-next t))))
            (if has-next
                next-headline
              nil)) ; a stuck project, has subtasks but no next task
        next-headline))))

;; Utilities to build agenda commands -- predicates

;;;###autoload
(defun vulpea-agenda-project-p ()
  "Return non-nil if the heading at point is a project.
Basically, it's any item with some todo keyword and tagged as
PROJECT."
  (let* ((comps (org-heading-components))
         (todo (nth 2 comps))
         (tags (split-string (or (nth 5 comps) "") ":")))
    (and (member todo org-todo-keywords-1)
         (member "PROJECT" tags))))

;;;###autoload
(defun vulpea-agenda-project-subtree-p ()
  "Any task with a todo keyword that is in a project subtree.
Callers of this function already widen the buffer view."
  (let ((task (save-excursion (org-back-to-heading 'invisible-ok)
                              (point))))
    (save-excursion
      (vulpea-agenda-find-project-task)
      (if (equal (point) task)
          nil
        t))))

;;;###autoload
(defun vulpea-agenda-task-p ()
  "Any task with a todo keyword and no subtask."
  (save-restriction
    (widen)
    (let ((has-subtask)
          (subtree-end (save-excursion (org-end-of-subtree t)))
          (is-a-task (member (nth 2 (org-heading-components))
                             org-todo-keywords-1)))
      (save-excursion
        (forward-line 1)
        (while (and (not has-subtask)
                    (< (point) subtree-end)
                    (re-search-forward "^\\*+ " subtree-end t))
          (when (member (org-get-todo-state) org-todo-keywords-1)
            (setq has-subtask t))))
      (and is-a-task (not has-subtask)))))

;; Utilities to build agenda commands -- search

;;;###autoload
(defun vulpea-agenda-find-project-task ()
  "Move point to the parent (project) task if any."
  (save-restriction
    (widen)
    (let ((parent-task (save-excursion
                         (org-back-to-heading 'invisible-ok)
                         (point))))
      (while (org-up-heading-safe)
        (when (member (nth 2 (org-heading-components))
                      org-todo-keywords-1)
          (setq parent-task (point))))
      (goto-char parent-task)
      parent-task)))

(setq
  org-agenda-dim-blocked-tasks nil
  ;; setting it to t speeds up agenda, but... initial visibility is
  ;; not honored, which for me is a bigger issue
  org-agenda-inhibit-startup nil

  ;; also show state change in log mode
  org-agenda-log-mode-items '(closed clock state)

  ;; tags
  org-agenda-show-inherited-tags nil

  ;; more structured view
  org-agenda-prefix-format
  '((agenda . " %(vulpea-agenda-category 24) %?-12t %12s")
    (todo . " %(vulpea-agenda-category 24) ")
    (tags . " %(vulpea-agenda-category 24) ")
    (search . " %(vulpea-agenda-category 24) "))
  org-agenda-todo-keyword-format "%-1s"
  org-agenda-tags-column 0

  ;; show agenda in current window
  org-agenda-window-setup 'current-window
  
  ;; Enabling a time grid and defining it
  org-agenda-use-time-grid t
  org-agenda-time-grid
      (quote
       ((daily today remove-match)
        (0900 1000 1100 1200 1300 1400 1500 1600 1700 1800 1900 2000 2100 2200 2300 2359 0100 0200)
        "......" "----------------"))


  ;; show deadlines ;; Custom Command Agenda Views 
  ;; Add custom views:
  org-agenda-custom-commands 
  '(("A" "Agenda"
     (,vulpea-agenda-cmd-refile
      ,vulpea-agenda-cmd-today
      ,vulpea-agenda-cmd-focus
      ,vulpea-agenda-cmd-projects
      ,vulpea-agenda-cmd-waiting)
     ((org-agenda-buffer-name vulpea-agenda-main-buffer-name)))
    ))

