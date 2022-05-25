;;; org-tracker.el --- Simple, unopinionated integration between org-mode and
;;; multiple issue trackers

;;; Copyright (C) 2018 Off Market Data, Inc. DBA Urbint
;;; Copyright (C) 2021 Griffin Smith
;;; Permission is hereby granted, free of charge, to any person obtaining a copy
;;; of this software and associated documentation files (the "Software"), to
;;; deal in the Software without restriction, including without limitation the
;;; rights to use, copy, modify, merge, publish, distribute, sublicense, and/or
;;; sell copies of the Software, and to permit persons to whom the Software is
;;; furnished to do so, subject to the following conditions:

;;; The above copyright notice and this permission notice shall be included in
;;; all copies or substantial portions of the Software.

;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
;;; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
;;; FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS
;;; IN THE SOFTWARE.

;;; Commentary:
;;; org-tracker provides simple, unopinionated integration between Emacs's
;;; org-mode and multiple issue trackers
;;;
;;; TODO document configuration
;;;

;;; Code:

(require 'cl-macs)
(require 'dash)
(require 's)
(require 'org)
(require 'org-element)
(require 'subr-x)
(require 'ivy)
(require 'json)
(require 'jiralib2)

(require 'org-tracker-utils)

;;;
;;; Backends
;;;

(cl-defgeneric org-tracker-backend/extract-issue-id
    (backend elt &optional property)
  "Query BACKEND for the ID of the issue referenced by ELT, or NIL if none.
Optional arg PROPERTY can specify the org property to extract from")

(cl-defgeneric org-tracker-backend/projects (backend)
  "Query BACKEND for a list of epics.
Can return `:UNSUPPORTED' if the backend does not support projects"
  :UNSUPPORTED)

(cl-defgeneric org-tracker-backend/epics (backend)
  "Query BACKEND for a list of epics.
Can return `:UNSUPPORTED' if the backend does not support epics"
  :UNSUPPORTED)

(cl-defgeneric org-tracker-backend/milestones (backend)
  "Query BACKEND for a list of milestones.
Can return `:UNSUPPORTED' if the backend does not support milestones"
  :UNSUPPORTED)

(cl-defgeneric org-tracker-backend/workflow-states (backend)
  "Query BACKEND for a list of workflow states.")

(cl-defgeneric org-tracker-backend/issue-types (backend)
  "Query BACKEND for a list of issue types.
Can return `:UNSUPPORTED' if the backend does not support issue types"
  :UNSUPPORTED)

(cl-defgeneric org-tracker-backend/labels (backend)
  "Query BACKEND for a list of labels.
Can return `:UNSUPPORTED' if the backend does not support milestones"
  :UNSUPPORTED)

(cl-defgeneric org-tracker-backend/whoami (backend)
  "Query BACKEND for the id of the currently authenticated user.")

(cl-defgeneric org-tracker-backend/iterations (backend)
  "Query BACKEND for a list of iterations.
Can return `:UNSUPPORTED' if the backend does not support iterations"
  :UNSUPPORTED)

(cl-defgeneric org-tracker-backend/search-issues (backend query &key detailed)
  "Query BACKEND for issues with text matching QUERY.
With DETAILED, use the detailed search endpoint (if applicable) for the
backend")

(cl-defgeneric org-tracker-backend/create-issue
    (backend
     &rest rest-keys
     &key title project-id epic-id issue-type description labels
     workflow-state-id
     &allow-other-keys))

(cl-defgeneric org-tracker-backend/update-issue
    (backend issue-id
             &key epic-id workflow-state-id assignee description title labels))

(cl-defgeneric org-tracker-backend/create-epic
    (backend
     &rest rest-keys
     &key title project-id milestone-id description labels
     &allow-other-keys))

(cl-defgeneric org-tracker-backend/issue-kv->prop-kv (backend key value)
  "For a particular BACKEND, convert VALUE, the value for KEY in an issue, to a
pair of (string . string) as property key and value for use in an org element
property.

If nil is returned, the value will not be set as a property of the element"
  nil)

(cl-defgeneric org-tracker-backend/populate-issue (backend issue)
  "Query BACKEND, if necessary, to populate all the fields of ISSUE."
  issue)

(require 'org-tracker-jira)
(require 'org-tracker-clubhouse)

(defvar-local org-tracker-local-backend :UNSET)

(defun org-tracker--configured-backend ()
  (save-mark-and-excursion
    (save-restriction
      (widen)
      (goto-char (point-min))
      (when (search-forward "#+TRACKER_BACKEND: " nil t)
        (intern
         (s-trim
          (buffer-substring-no-properties (point) (point-at-eol))))))))

(defun org-tracker--populate-local-backend ()
  (when-let ((local-backend (org-tracker--configured-backend)))
    (setq-local org-tracker-local-backend local-backend)))

(add-hook 'org-mode-hook #'org-tracker--populate-local-backend)

(defvar org-tracker-backends ())

(defun org-tracker-backend-name->backend (backend-name)
  (or
   (car (alist-get backend-name org-tracker-backends))
   (error "Could not find org-tracker backend named `%s'"
          backend-name)))

(defun org-tracker-current-backend ()
  "Return the current configured org-tracker backend."
  (when (eq org-tracker-local-backend :UNSET)
    (org-tracker--populate-local-backend))
  (if-let ((backend (or org-tracker-local-backend
                        org-tracker-default-backend)))
      (if (symbolp backend)
          (org-tracker-backend-name->backend backend)
        backend)
    (error "No currently configured org-tracker backend could be found")))

;;;
;;; Configuration
;;;

(defvar org-tracker-default-backend)

(defvar org-tracker-state-alist
  '(("LATER"  . "Unscheduled")
    ("[ ]"    . "Ready for Development")
    ("TODO"   . "Ready for Development")
    ("OPEN"   . "Ready for Development")
    ("ACTIVE" . "In Development")
    ("PR"     . "Review")
    ("DONE"   . "Merged")
    ("[X]"    . "Merged")
    ("CLOSED" . "Merged"))
  "Alist mapping org-mode todo keywords to their corresponding states in
  Clubhouse. In `org-tracker-mode', moving headlines to these todo keywords
  will update to the corresponding status in Clubhouse")

(defvar org-tracker-ticket-types
  '(("feature" . "Feature")
    ("bug"     . "Bug")
    ("chore"   . "Chore")))

(defvar org-tracker-default-ticket-types
  '(("feature" . "Feature")
    ("bug"     . "Bug")
    ("chore"   . "Chore")
    ("prompt"  . "**Prompt each time (do not set a default ticket type)**")))

(defvar org-tracker-default-state "Unscheduled"
  "Default state to create all new stories in.")

(defvar org-tracker-claim-ticket-on-status-update 't
  "Controls the assignee behavior of stories on status update.

If set to 't, will mark the current user as the owner of any clubhouse
stories on any update to the status.

If set to nil, will never automatically update the assignee of clubhouse
stories.

If set to a list of todo-state's, will mark the current user as the owner of
clubhouse stories whenever updating the status to one of those todo states.")

(defvar org-tracker-create-stories-with-labels nil
  "Controls the way org-tracker creates stories with labels based on org tags.

If set to 't, will create labels for all org tags on headlines when stories are
created.

If set to 'existing, will set labels on created stories only if the label
already exists in clubhouse

If set to nil, will never create stories with labels")

;;;
;;; Utilities
;;;
(defun org-tracker-collect-headlines (beg end)
  "Collects the headline at point or the headlines in a region. Returns a list."
  (if (and beg end)
      (org-tracker-get-headlines-in-region beg end)
    (list (org-element-find-headline))))


(defun org-tracker-get-headlines-in-region (beg end)
  "Collects the headlines from BEG to END"
  (save-excursion
    ;; This beg/end clean up pulled from `reverse-region`.
    ;; it expands the region to include the full lines from the selected region.

    ;; put beg at the start of a line and end and the end of one --
    ;; the largest possible region which fits this criteria
    (goto-char beg)
    (or (bolp) (forward-line 1))
    (setq beg (point))
    (goto-char end)
    ;; the test for bolp is for those times when end is on an empty line;
    ;; it is probably not the case that the line should be included in the
    ;; reversal; it isn't difficult to add it afterward.
    (or (and (eolp) (not (bolp))) (progn (forward-line -1) (end-of-line)))
    (setq end (point-marker))

    ;; move to the beginning
    (goto-char beg)
    ;; walk by line until past end
    (let ((headlines '())
          (before-end 't))
      (while before-end
        (add-to-list 'headlines (org-element-find-headline))
        (let ((before (point)))
          (org-forward-heading-same-level 1)
          (setq before-end (and (not (eq before (point))) (< (point) end)))))
      (reverse headlines))))

;;;
;;; Org-element interaction
;;;

(defun org-element-ticket-id (backend &optional property)
  (org-tracker-backend/extract-issue-id
   backend
   (org-element-find-headline)
   property))

(defun org-element-find-headline ()
  (save-mark-and-excursion
    (when (not (outline-on-heading-p)) (org-back-to-heading))
    (let ((current-elt (org-element-at-point)))
      (when (equal 'headline (car current-elt))
        (cadr current-elt)))))

(defun org-tracker--element-type (elt)
  "Return one of 'epic, 'ticket, or nil indicating the type of ELT."
  (case
   ((plist-get elt :CLUBHOUSE-EPIC-ID) 'epic)
   ((plist-get elt :CLUBHOUSE-ID) 'story)))

(defun org-tracker-clocked-in-issue-id (&optional backend)
  "Return the ticket-id of the currently clocked-in org entry, if any."
  (save-mark-and-excursion
    (save-current-buffer
      (when (org-clocking-p)
        (set-buffer (marker-buffer org-clock-marker))
        (save-restriction
          (when (or (< org-clock-marker (point-min))
                    (> org-clock-marker (point-max)))
            (widen))
          (goto-char org-clock-marker)
          (org-element-ticket-id
           (or backend (org-tracker-current-backend))))))))

(defun org-element-and-children-at-point ()
  (let* ((elt (org-element-find-headline))
         (contents-begin (or (plist-get elt :contents-begin)
                             (plist-get elt :begin)))
         (end   (plist-get elt :end))
         (level (plist-get elt :level))
         (children '()))
    (save-excursion
      (goto-char (+ contents-begin (length (plist-get elt :title))))
      (while (< (point) end)
        (let* ((next-elt (org-element-at-point))
               (elt-type (car next-elt))
               (elt      (cadr next-elt)))
          (when (and (eql 'headline elt-type)
                     (eql (+ 1 level) (plist-get elt :level)))
            (push elt children))
          (goto-char (plist-get elt :end)))))
    (append elt `(:children ,(reverse children)))))

(defun +org-element-contents (elt)
  (if-let ((begin (plist-get (cadr elt) :contents-begin))
           (end (plist-get (cadr elt) :contents-end)))
      (buffer-substring-no-properties begin end)
    ""))

(defun org-tracker-find-description-drawer ()
  "Try to find a DESCRIPTION drawer in the current element."
  (let ((elt (org-element-at-point)))
    (cl-case (car elt)
      ('drawer (+org-element-contents elt))
      ('headline
       (when-let ((drawer-pos (string-match
                               ":DESCRIPTION:"
                               (+org-element-contents elt))))
         (save-excursion
           (goto-char (+ (plist-get (cadr elt) :contents-begin)
                         drawer-pos))
           (org-tracker-find-description-drawer)))))))

(defun org-tracker--description-for-elt (elt)
  (save-mark-and-excursion
    (goto-char (plist-get elt :begin))
    (org-tracker-find-description-drawer)))

(defun org-tracker--labels-for-elt (backend elt)
  "Return the tracker labels based on the tags of ELT and the user's config."
  (unless (eq nil org-tracker-create-stories-with-labels)
    (let ((tags (org-get-tags (plist-get elt :contents-begin))))
      (cl-case org-tracker-create-stories-with-labels
        ('t tags)
        ('existing (-filter
                    (lambda (tag)
                      (-some
                       (lambda (l)
                         (string-equal tag (cdr l)))
                       (org-tracker-backend/labels backend)))
                    tags))))))

(defun org-tracker-workflow-state-id-to-todo-keyword (backend workflow-state-id)
  "Convert the named WORKFLOW-STATE-ID to an org todo keyword."
  (let* ((state-name (alist-get-equal
                      workflow-state-id
                      (org-tracker-backend/workflow-states backend)))
         (inv-state-name-alist
          (-map (lambda (cell) (cons (cdr cell) (car cell)))
                org-tracker-state-alist)))
    (or (alist-get-equal state-name inv-state-name-alist)
        (if state-name (s-upcase state-name) "UNKNOWN"))))

;;;
;;; Prompting
;;;

(defun org-tracker-prompt-for-project (backend cb)
  (let ((projects (org-tracker-backend/projects backend)))
    (if (or (eq :UNSUPPORTED projects)
            (null projects))
        (funcall cb nil)
      (ivy-read
       "Select a project: "
       (-map #'cdr projects)
       :require-match t
       :history 'org-tracker-project-history
       :action (lambda (selected)
                 (let ((project-id
                        (find-match-in-alist selected projects)))
                   (funcall cb project-id)))))))

(defun org-tracker-prompt-for-epic (backend cb)
  "Prompt the user for an epic using ivy and call CB with its ID."
  (let ((epics (org-tracker-backend/epics backend)))
    (if (or (eq :UNSUPPORTED epics)
            (null epics))
        (funcall cb nil)
      (ivy-read
       "Select an epic: "
       (-map #'cdr (append '((nil . "No Epic")) epics))
       :history 'org-tracker-epic-history
       :action (lambda (selected)
                 (let ((epic-id
                        (find-match-in-alist selected epics)))
                   (funcall cb epic-id)))))))

(defun org-tracker-prompt-for-milestone (backend cb)
  "Prompt the user for a milestone using ivy and call CB with its ID."
  (let ((milestones (org-tracker-backend/milestones backend)))
    (if (or (eq :UNSUPPORTED milestones)
            (null milestones))
        (funcall cb nil)
      (ivy-read
       "Select a milestone: "
       (-map #'cdr (append '((nil . "No Milestone")) milestones))
       :require-match t
       :history 'org-tracker-milestone-history
       :action (lambda (selected)
                 (let ((milestone-id
                        (find-match-in-alist selected milestones)))
                   (funcall cb milestone-id)))))))

(defun org-tracker-prompt-for-issue-type (backend cb)
  "Prompt the user for an issue type from BACKEND using ivy and call CB with its
ID."
  (let ((issue-types (org-tracker-backend/issue-types backend)))
    (if (or (eq :UNSUPPORTED issue-types)
            (null issue-types))
        (funcall cb nil)
      (ivy-read
       "Select a story type: "
       (-map #'cdr issue-types)
       :history 'org-tracker-issue-type-history
       :action (lambda (selected)
                 (let ((issue-type
                        (find-match-in-alist selected issue-types)))
                   (funcall cb issue-type)))))))

;;;
;;; Epic creation
;;;

(defun org-tracker-populate-created-epic (backend elt epic &key extra-properties)
  (let ((elt-start  (plist-get elt :begin))
        (epic-id    (alist-get 'id epic)))
    (save-excursion
      (goto-char elt-start)

      (loop for (key . value) in epic
            do (if-let ((prop-kv (org-tracker-backend/issue-kv->prop-kv
                                  backend
                                  key
                                  value)))
                   (org-set-property (car prop-kv) (cdr prop-kv))
                 (when (let ((-compare-fn #'eq)) (-contains-p extra-properties key))
                   (org-set-property (symbol-name key)
                                     (format "%s" value))))))))

(defun org-tracker-create-epic (&optional beg end)
  "Create a clubhouse epic using selected headlines.
Will pull the title from the headline at point, or create epics for all the
headlines in the selected region (between BEG and END).

All epics are added to the same milestone, as selected via a prompt.
If the epics already have a CLUBHOUSE-EPIC-ID, they are filtered and ignored."
  (interactive
   (when (use-region-p)
     (list (region-beginning) (region-end))))

  (let* ((backend (org-tracker-current-backend))
         (elts    (org-tracker-collect-headlines beg end))
         (elts    (-remove (lambda (elt) (org-tracker-backend/extract-issue-id backend elt :EPIC-ID)) elts)))
    (org-tracker-prompt-for-project
     backend
     (lambda (project-id)
       (when project-id
         (org-tracker-prompt-for-milestone
          backend
          (lambda (milestone-id)
            (dolist (elt elts)
              (let* ((title (substring-no-properties (plist-get elt :title)))
                     (description (org-tracker--description-for-elt elt))
                     (labels (org-tracker--labels-for-elt backend elt))
                     (epic  (org-tracker-backend/create-epic
                             backend
                             :title title
                             :milestone-id milestone-id
                             :project-id project-id
                             :labels labels
                             :description description)))
                (org-tracker-populate-created-epic backend elt epic))))))))))

;;;
;;; Story creation
;;;

(defun org-tracker-default-state-id (backend)
  (alist-get-equal
   org-tracker-default-state
   (org-tracker-backend/workflow-states backend)))

(cl-defun org-tracker-populate-created-issue (backend elt issue &key extra-properties)
  (save-excursion
    (goto-char (plist-get elt :begin))
    (org-todo "TODO")
    (loop for (key . value) in issue
          do (if-let ((prop-kv (org-tracker-backend/issue-kv->prop-kv
                                backend
                                key
                                value)))
                 (org-set-property (car prop-kv) (cdr prop-kv))
               (when (let ((-compare-fn #'eq))
                       (-contains-p extra-properties key))
                 (org-set-property (symbol-name key)
                                   (format "%s" value)))))))

(defun org-tracker-create-issue (&optional beg end &key then)
  "Creates a new issue in the configured backend using selected headlines.

Will pull the title from the headline at point,
or create cards for all the headlines in the selected region.

All stories are added to the same project and epic, as selected via two prompts.
If the stories already have a CLUBHOUSE-ID, they are filtered and ignored."
  (interactive
   (when (use-region-p)
     (list (region-beginning) (region-end))))

  (let* ((backend  (org-tracker-current-backend))
         (elts     (org-tracker-collect-headlines beg end))
         (new-elts (-remove (lambda (elt) (org-tracker-backend/extract-issue-id backend elt)) elts)))
    (org-tracker-prompt-for-project
     backend
     (lambda (project-id)
       (when project-id
         (org-tracker-prompt-for-epic
          backend
          (lambda (epic-id)
            (let ((create-issue
                   (lambda (story-type)
                     (-map
                      (lambda (elt)
                        (let* ((title (substring-no-properties (plist-get elt :title)))
                               (description
                                (org-tracker--description-for-elt elt))
                               (labels (org-tracker--labels-for-elt
                                        backend elt))
                               (workflow-state-id (org-tracker-default-state-id
                                                   backend))
                               (issue (org-tracker-backend/create-issue
                                       backend
                                       :title title
                                       :project-id project-id
                                       :epic-id epic-id
                                       :workflow-state-id workflow-state-id
                                       :issue-type issue-type
                                       :description description
                                       :labels labels)))
                          (org-tracker-populate-created-issue backend elt issue)
                          (when (functionp then)
                            (funcall then issue))))
                      new-elts))))
              (org-tracker-prompt-for-issue-type backend create-issue)))))))))

(defun org-tracker-create-story-with-task-list (&optional beg end)
  "Creates a clubhouse story using the selected headline, making all direct
children of that headline into tasks in the task list of the story."
  (interactive
   (when (use-region-p)
     (list (region-beginning) (region-end))))

  (let* ((elt (org-element-and-children-at-point)))
    (org-tracker-create-story nil nil
     :then (lambda (story)
             (pp story)
             (org-tracker-push-task-list
              (alist-get 'id story)
              (plist-get elt :children))))))

;;;
;;; Task creation
;;;

(cl-defun org-tracker-create-task (title &key story-id)
  (cl-assert (and (stringp title)
               (integerp story-id)))
  (org-tracker-request
   "POST"
   (format "/stories/%d/tasks" story-id)
   :data (json-encode `((description . ,title)))))

(defun org-tracker-push-task-list (&optional parent-clubhouse-id child-elts)
  "Writes each child of the element at point as a task list item.

When called as (org-tracker-push-task-list PARENT-CLUBHOUSE-ID CHILD-ELTS),
allows manually passing a clubhouse ID and list of org-element plists to write"
  (interactive)
  (let* ((elt (org-element-and-children-at-point))
         (parent-clubhouse-id (or parent-clubhouse-id
                                  (org-element-extract-clubhouse-id elt)))
         (child-elts (or child-elts (plist-get elt :children)))
         (story (org-tracker-get-story parent-clubhouse-id))
         (existing-tasks (alist-get 'tasks story))
         (task-exists
          (lambda (task-name)
            (cl-some (lambda (task)
                    (string-equal task-name (alist-get 'description task)))
                  existing-tasks)))
         (elts-with-starts
          (-map (lambda (e) (cons (set-marker (make-marker)
                                         (plist-get e :begin))
                             e))
                child-elts)))
    (dolist (child-elt-and-start elts-with-starts)
      (let* ((start (car child-elt-and-start))
             (child-elt (cdr child-elt-and-start))
             (task-name (substring-no-properties (plist-get child-elt :title))))
        (unless (funcall task-exists task-name)
          (let ((task (org-tracker-create-task
                       task-name
                       :story-id parent-clubhouse-id)))
            (org-tracker-populate-created-task child-elt task start)))))))

(defun org-tracker-populate-created-task (elt task &optional begin)
  (let ((elt-start (or begin (plist-get elt :begin)))
        (task-id   (alist-get 'id task))
        (story-id  (alist-get 'story_id task)))

    (save-excursion
      (goto-char elt-start)

      (org-set-property "clubhouse-task-id" (format "%d" task-id))

      (org-set-property "clubhouse-story-id"
                        (org-link-make-string
                         (org-tracker-link-to-story story-id)
                         (number-to-string story-id)))

      (org-todo "TODO"))))

;;;
;;; Task Updates
;;;

(cl-defun org-tracker-update-task-internal
    (story-id task-id &rest attrs)
  (cl-assert (and (integerp story-id)
                  (integerp task-id)
                  (listp attrs)))
  (org-tracker-request
   "PUT"
   (format "stories/%d/tasks/%d" story-id task-id)
   :data
   (json-encode attrs)))

;;;
;;; Story updates
;;;

(cl-defun org-tracker-update-story-internal
    (story-id &rest attrs)
  (cl-assert (and (integerp story-id)
                  (listp attrs)))
  (org-tracker-request
   "PUT"
   (format "stories/%d" story-id)
   :data
   (json-encode attrs)))

(cl-defun org-tracker-update-epic-internal
    (story-id &rest attrs)
  (cl-assert (and (integerp story-id)
                  (listp attrs)))
  (org-tracker-request
   "PUT"
   (format "epics/%d" epic-id)
   :data
   (json-encode attrs)))

(cl-defun org-tracker-update-issue-at-point (backend &rest attrs)
  (when-let* ((elt (org-element-find-headline))
              (issue-id (org-tracker-backend/extract-issue-id
                         backend
                         elt)))
    (apply
     #'org-tracker-backend/update-issue
     backend
     issue-id
     attrs)
    t))

(cl-defun org-tracker-update-epic-at-point (&rest attrs)
  (when-let* ((epic-id (org-element-clubhouse-id :CLUBHOUSE-EPIC-ID)))
    (apply
     #'org-tracker-update-epic-internal
     (cons epic-id attrs))
    t))

(defun org-tracker-update-issue-title (&optional beg end )
  "Update the title of the tracker issue linked to the current headline.

When called interactively with a region, operates on all elements between BEG
and END."
  (interactive
   (when (use-region-p)
     (list (region-beginning) (region-end))))

  (let ((backend (org-tracker-current-backend)))
    (dolist (elt (org-tracker-collect-headlines beg end))
      (let* ((title (substring-no-properties (plist-get elt :title))))
        (and
         (org-tracker-update-issue-at-point
          backend
          :title title)
         (message "Successfully updated issue title to \"%s\""
                  title))))))

(defun org-tracker-update-status ()
  "Update the status of the Clubhouse story linked to the current element.

Update the status of the Clubhouse story linked to the current element with the
entry in `org-tracker-state-alist' corresponding to the todo-keyword of the
element."
  (interactive)
  (let* ((elt (org-element-find-headline))
         (todo-keyword (-> elt
                           (plist-get :todo-keyword)
                           (substring-no-properties)))
         (backend (org-tracker-current-backend))
         (issue-id (org-tracker-backend/extract-issue-id backend elt)))
    (when-let* ((workflow-state
                 (alist-get-equal todo-keyword org-tracker-state-alist))
                (workflow-state-id
                 (alist-get-equal workflow-state
                                  (invert-alist
                                   (org-tracker-backend/workflow-states
                                    backend)))))
      (let ((update-assignee?
             (if (or (eq 't org-tracker-claim-ticket-on-status-update)
                     (member todo-keyword
                             org-tracker-claim-ticket-on-status-update))
                 (if org-tracker-username
                     't
                   (warn "Not claiming story since `org-tracker-username'
                       is not set")
                   nil))))

        (message "%s"
                 (org-tracker-backend/update-issue
                  backend
                  issue-id
                  :workflow-state-id workflow-state-id
                  :assignee (when update-assignee?
                              (org-tracker-backend/whoami backend))))
        (message
         (if update-assignee?
             "Successfully claimed issue and updated status to \"%s\""
           "Successfully updated status to \"%s\"")
         workflow-state)))))

(defun org-tracker-update-description ()
  "Update the description of the Clubhouse story linked to the current element.

Update the status of the Clubhouse story linked to the current element with the
contents of a drawer inside the element called DESCRIPTION, if any."
  (interactive)
  (when-let* ((new-description (org-tracker-find-description-drawer)))
    (and
     (org-tracker-update-story-at-point
      :description new-description)
     (message "Successfully updated story description"))))

(defun org-tracker-update-labels (&optional beg end)
  "Update the labels of the issue linked to the element at point.

When called interactively with a region, operates on all elements between BEG
and END.

Will use the value of `org-tracker-create-stories-with-labels' to determine
which labels to set."
  (interactive
   (when (use-region-p)
     (list (region-beginning) (region-end))))

  (let ((backend (org-tracker-current-backend)))
    (dolist (elt (org-tracker-collect-headlines beg end))
      (let* ((new-labels (org-tracker--labels-for-elt backend elt))
             (label-desc (->> new-labels (-map #'cdar) (s-join ":"))))
        (and
         (org-tracker-update-issue-at-point
          backend
          :labels new-labels)
         (message "Successfully updated issue labels to :%s:"
                  label-desc))))))


;;;
;;; Creating headlines from existing stories
;;;

(defun org-tracker--task-to-headline-text (level task)
  (format "%s %s %s
:PROPERTIES:
:clubhouse-task-id: %s
:clubhouse-story-id: %s
:END:"
          (make-string level ?*)
          (if (equal :json-false (alist-get 'complete task))
              "TODO" "DONE")
          (alist-get 'description task)
          (alist-get 'id task)
          (let ((story-id (alist-get 'story_id task)))
            (org-link-make-string
             (org-tracker-link-to-story story-id)
             story-id))))

(defun org-tracker--issue-to-headline-text (backend level issue)
  (let ((issue-id (alist-get 'id issue)))
    (format
     "%s %s %s %s
:PROPERTIES:
%s
:END:
%s
%s
"
     (make-string level ?*)
     (org-tracker-workflow-state-id-to-todo-keyword
      backend
      (alist-get 'workflow_state_id issue))
     (alist-get 'name issue)
     (if-let ((labels (->> issue
                        (alist-get 'labels)
                        ->list
                        (-map
                         (lambda (label)
                           (if (stringp label) label (alist-get 'name label)))))))
         (format ":%s:" (s-join ":" labels))
       "")
     (->> issue
       (-keep (lambda (kv) (org-tracker-backend/issue-kv->prop-kv
                       backend
                       (car kv)
                       (cdr kv))))
       (-map (lambda (kv) (format ":%s: %s"
                             (car kv)
                             (cdr kv))))
       (s-join "\n"))
     (let ((desc (alist-get 'description issue)))
       (if (= 0 (length desc)) ""
         (format ":DESCRIPTION:\n%s\n:END:" desc)))
     (if-let ((tasks (seq-sort-by
                      (apply-partially #'alist-get 'position)
                      #'<
                      (alist-get 'tasks issue))))
         (mapconcat (apply-partially #'org-tracker--task-to-headline-text
                                     (1+ level))
                    tasks
                    "\n")
       ""))))

(defun org-tracker-headline-from-my-tasks (level)
  "Prompt my active stories and create a single `org-mode' headline at LEVEL."
  (interactive "*nLevel: \n")
  (if org-tracker-username
      (let* ((story-list (org-tracker--search-stories
                          (format "owner:%s !is:done !is:archived"
                                  org-tracker-username)))
             (stories (to-id-name-pairs story-list)))
        (org-tracker-headline-from-story-id level
                                              (find-match-in-alist
                                               (ivy-read "Select Story: "
                                                         (-map #'cdr stories))
                                               stories)))
    (warn "Can't fetch my tasks if `org-tracker-username' is unset")))

(defun org-tracker-headline-from-story-id (level story-id)
  "Create a single `org-mode' headline at LEVEL based on the given clubhouse STORY-ID."
  (interactive "*nLevel: \nnStory ID: ")
  (let* ((story (org-tracker-get-story story-id)))
    (if (equal '((message . "Resource not found.")) story)
        (message "Story ID not found: %d" story-id)
      (save-mark-and-excursion
        (insert (org-tracker--issue-to-headline-text level story))
        (org-align-tags)))))

(defun org-tracker-prompt-for-iteration (cb)
  "Prompt for iteration and call CB with that iteration"
  (ivy-read
   "Select an interation: "
   (-map #'cdr (org-tracker-iterations))
   :require-match t
   :history 'org-tracker-iteration-history
   :action (lambda (selected)
             (let ((iteration-id
                    (find-match-in-alist selected (org-tracker-iterations))))
               (funcall cb iteration-id)))))

(defun org-tracker--get-iteration (iteration-id)
  (-> (org-tracker-request "GET" (format "iterations/%d/stories" iteration-id))
      (append nil)))

(defun org-tracker-headlines-from-iteration (level)
  "Create `org-mode' headlines from a clubhouse iteration.

Create `org-mode' headlines from all the resulting stories at headline level LEVEL."
  (interactive "*nLevel: ")
  (org-tracker-prompt-for-iteration
   (lambda (iteration-id)
     (let ((story-list (org-tracker--get-iteration iteration-id)))
       (if (null story-list)
           (message "Iteration id returned no stories: %d" iteration-id)
         (let ((text (mapconcat (apply-partially
                                 #'org-tracker--issue-to-headline-text
                                 level)
                                (reject-archived story-list) "\n")))
               (save-mark-and-excursion
                 (insert text)
                 (org-align-all-tags))
             text))))))

(defun org-tracker-headlines-from-search (level query)
  "Create `org-mode' headlines from a query to the tracker's search endpoint.

Submits QUERY to the configured backend's native search endpoint, and creates
`org-mode' headlines from all the resulting stories at headline level LEVEL."
  (interactive
   "*nLevel: \nMQuery: ")
  (let* ((backend (org-tracker-current-backend))
         (story-list (org-tracker-backend/search-issues backend query
                                                        :detailed t)))
    (if (null story-list)
        (message "Query returned no stories: %s" query)
      (let ((text (mapconcat (apply-partially
                              #'org-tracker--issue-to-headline-text
                              backend
                              level)
                             (reject-archived story-list) "\n")))
        (if (called-interactively-p)
            (save-mark-and-excursion
              (insert text)
              (org-align-all-tags))
          text)))))

(defun org-tracker-prompt-for-issue (backend cb)
  "Prompt the user for a clubhouse issue, then call CB with the full issue."
  (ivy-read "Issue title: "
            (lambda (search-term)
              (let* ((stories (org-tracker-backend/search-issues
                               backend
                               search-term)))
                (-map (lambda (issue)
                        (propertize (alist-get 'name issue) 'issue issue))
                      stories)))
            :dynamic-collection t
            :history 'org-tracker-issue-prompt
            :action (lambda (s) (funcall cb (get-text-property 0 'issue s)))
            :require-match t))

(defun org-tracker-headline-from-issue (level)
  "Prompt for a issue, and create an org headline at LEVEL from that issue."
  (interactive "*nLevel: ")
  (let ((backend (org-tracker-current-backend)))
    (org-tracker-prompt-for-issue
     backend
     (lambda (issue)
       (save-mark-and-excursion
         (insert (org-tracker--issue-to-headline-text
                  backend
                  level
                  (org-tracker-backend/populate-issue
                   (org-tracker-current-backend)
                   issue)))
         (org-align-tags))))))


(defun org-tracker-link ()
  "Link the current `org-mode' headline with an existing clubhouse story."
  (interactive)
  (org-tracker-prompt-for-story
   (lambda (story)
     (org-tracker-populate-created-story
      (org-element-find-headline)
      story
      :extra-properties '(("clubhouse-story-name" . name)))
     (org-todo
      (org-tracker-workflow-state-id-to-todo-keyword
       (alist-get 'workflow_state_id story))))))

(defun org-tracker-claim (&optional beg end)
  "Assign the clubhouse story associated with the headline at point to yourself."
  (interactive
   (when (use-region-p)
     (list (region-beginning) (region-end))))
  (let ((elts (org-tracker-collect-headlines beg end))
        (backend (org-tracker-current-backend)))
    (if org-tracker-username
        (dolist (elt elts)
          (when-let ((issue-id (org-tracker-backend/extract-issue-id
                                backend elt)))
            (org-tracker-backend/update-issue
             backend
             issue-id
             :assignee (org-tracker-backend/whoami backend))
            (message "Successfully claimed story")))
      (warn "Can't claim story if `org-tracker-username' is unset"))))

(defun org-tracker-sync-status (&optional beg end)
  "Pull the status(es) for the story(ies) in region and update the todo state.

Uses `org-tracker-state-alist'. Operates over stories from BEG to END"
  (interactive
   (when (use-region-p)
     (list (region-beginning) (region-end))))
  (let ((elts (-filter (lambda (e) (plist-get e :CLUBHOUSE-ID))
                       (org-tracker-collect-headlines beg end))))
    (save-mark-and-excursion
      (dolist (e elts)
        (goto-char (plist-get e :begin))
        (let* ((clubhouse-id (org-element-extract-clubhouse-id e))
               (story (org-tracker-get-story clubhouse-id))
               (workflow-state-id (alist-get 'workflow_state_id story))
               (todo-keyword (org-tracker-workflow-state-id-to-todo-keyword
                              workflow-state-id)))
          (let ((org-after-todo-state-change-hook
                 (remove 'org-tracker-update-status
                         org-after-todo-state-change-hook)))
            (org-todo todo-keyword)))))
    (message "Successfully synchronized status of %d stories from Clubhouse"
             (length elts))))

(cl-defun org-tracker-set-epic (&optional issue-id epic-id cb &key beg end)
  "Set the epic of the issue with ISSUE-ID to EPIC-ID, then call CB.

When called interactively, prompt for an epic and set the issue of the issue[s]
at point, or region identified by BEG and END"
  (interactive
   (when (use-region-p)
     (list nil nil nil
           :beg (region-beginning)
           :end (region-end))))
  (let ((backend (org-tracker-current-backend)))
    (if (and issue-id epic-id)
        (progn
          (org-tracker-backend/update-issue
           backend
           issue-id
           :epic-id epic-id)
          (when cb (funcall cb)))
      (let ((elts (org-tracker-collect-headlines beg end)))
        (org-tracker-prompt-for-epic
         backend
         (lambda (epic-id)
           (-map
            (lambda (elt)
              (when-let ((issue-id (org-tracker-backend/extract-issue-id
                                    backend elt)))
                (org-tracker-set-epic
                 issue-id
                 epic-id
                 (lambda ()
                   (let ((prop-kv (org-tracker-backend/issue-kv->prop-kv
                                   backend
                                   'epic-id
                                   epic-id)))
                     (org-set-property (car prop-kv) (cdr prop-kv)))
                   (message "Successfully set the epic on issue %s to %s"
                            issue-id epic-id)))))
            elts)))))))

;;;

(define-minor-mode org-tracker-mode
  "If enabled, updates to the todo keywords on org headlines will update the
linked ticket in Clubhouse."
  :group 'org
  :lighter "Org-Tracker"
  :keymap '()
  (add-hook 'org-after-todo-state-change-hook
            'org-tracker-update-status
            nil
            t))

(provide 'org-tracker)

;;; org-tracker.el ends here
