;;; org-tracker-jira.el --- JIRA backend for org-tracker -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2021 Griffin Smith
;;
;; Author: Griffin Smith <https://github.com/glittershark>
;; URL: https://github.com/glittershark/org-tracker
;; Maintainer: Griffin Smith <root@gws.fyi>
;; Created: May 19, 2021
;; Modified: May 19, 2021
;; Version: 0.0.1
;; Package-Requires: ((emacs "27.1"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  JIRA backend for org-tracker
;;
;;; Code:

(require 'jiralib2)


(defclass org-tracker-jira-backend ()
  ((jira-url
    :initarg :url
    :accessor jira-url)
   (jira-auth
    :initarg :auth
    :accessor jira-auth)
   (jira-user-login-name
    :initarg :user-login-name
    :accessor jira-user-login-name)
   (jira-token
    :initarg :token
    :accessor jira-token)
   (epic-custom-field
    :initarg :epic-custom-field
    :accessor epic-custom-field)))

(defun org-tracker--call-with-jira-backend (backend f)
  (let ((jiralib2-url (jira-url backend))
        (jiralib2-auth (jira-auth backend))
        (jiralib2-user-login-name (jira-user-login-name backend))
        (jiralib2-token (jira-token backend)))
    (funcall f)))

(defmacro org-tracker--with-jira-backend (backend &rest body)
  (declare (indent 1))
  `(org-tracker--call-with-jira-backend ,backend (lambda () ,@body)))

(cl-defmethod org-tracker-backend/extract-issue-id
  ((_backend org-tracker-jira-backend) elt &optional _property)
  (when-let* ((jira-id-link
               (plist-get elt
                          ;; we don't use a separate property for epic-id like
                          ;; clubhouse does
                          :JIRA-ID)))
    (cond
     ((string-match
       (rx "[[" (one-or-more anything) "]"
           "[" (group (one-or-more anything)) "]]")
       jira-id-link)
      (match-string 1 jira-id-link))
     ((string-match
       (rx "[[https://"
           (one-or-more anything)
           "/browse/"
           (group (one-or-more anything)))
       jira-id-link)
      (match-string 1 jira-id-link))
     (t jira-id-link))))

(cl-defmethod org-tracker-backend/projects
  ((backend org-tracker-jira-backend))
  (org-tracker--with-jira-backend backend
    (to-id-name-pairs (jiralib2-get-projects))))

(cl-defmethod org-tracker-backend/epics
  ((backend org-tracker-jira-backend))
  (org-tracker--with-jira-backend backend
    (->> (jiralib2-session-call
          "/rest/api/2/search"
          :params '((jql . "issuetype = Epic")))
      (alist-get 'issues)
      (-map (lambda (issue)
              (cons
               (alist-get 'key issue)
               (->> issue (alist-get 'fields) (alist-get 'summary))))))))

(cl-defmethod org-tracker-backend/workflow-states
  ((backend org-tracker-jira-backend))
  (org-tracker--with-jira-backend backend
    (to-id-name-pairs
     (jiralib2-session-call "/rest/api/2/status"))))

(cl-defmethod org-tracker-backend/issue-types
  ((backend org-tracker-jira-backend))
  (org-tracker--with-jira-backend backend
    (to-id-name-pairs
     (jiralib2-session-call "/rest/api/2/issuetype"))))

(cl-defmethod org-tracker-backend/labels
  ((backend org-tracker-jira-backend))
  (org-tracker--with-jira-backend backend
    (-map (lambda (x) (cons x x))
          (alist-get 'values
                     (jiralib2-session-call "/rest/api/2/label")))))

(cl-defmethod org-tracker-backend/search-issues
  ((backend org-tracker-jira-backend) query &key detailed)
  (org-tracker--with-jira-backend backend
    (->>
        (if detailed
            (->>
                (jiralib2-session-call
                 "/rest/api/2/search"
                 :params `((jql . ,query)))
              (alist-get 'issues)
              (-map #'jira->issue))
          (->>
              (jiralib2-session-call
               "/rest/api/2/issue/picker"
               :params `((query . ,query)
                         (currentJQL . "order by created DESC")))
            (alist-get 'sections)
            (-find (lambda (section) (equal "cs" (alist-get 'id section))))
            (alist-get 'issues)
            (-map (lambda (issue)
                    (cons `(name . ,(alist-get 'summaryText issue))
                          issue))))))))

(cl-defmethod org-tracker-backend/whoami
  ((backend org-tracker-jira-backend))
  (org-tracker--with-jira-backend backend
    (alist-get 'accountId
               (jiralib2-session-call "/rest/api/2/myself"))))

(cl-defmethod org-tracker-backend/issue-subtasks
  ((backend org-tracker-jira-backend) issue-id)
  (org-tracker-backend/search-issues
   backend
   (format "issuetype in subTaskIssueTypes() AND parent = %s order by created DESC"
           issue-id)
   :detailed t))

(cl-defmethod org-tracker-backend/fetch-issue
  ((backend org-tracker-jira-backend) issue-id)
  (org-tracker--with-jira-backend backend
    (jira->issue
     (jiralib2-session-call
      (format "/rest/api/2/issue/%s" issue-id)))))

(comment
 (org-tracker-backend/fetch-issue
  (org-tracker-backend-name->backend 'readyset-jira)
  "ENG-1327")
 )

(defun jiralib2--get-custom-field (backend name)
  "Query BACKEND for the 'id of the custom field with name NAME."
  (let ((fields (jiralib2-session-call "/rest/api/2/field")))
    (->> fields
         (-find (lambda (field) (equal name (alist-get 'name field))))
         (alist-get 'id))))

(defun jiralib2--get-epic-custom-field (backend)
  (unless (and (slot-boundp backend 'epic-custom-field)
               (not (null (slot-value backend 'epic-custom-field))))
    (let ((field (jiralib2--get-custom-field backend  "Epic Link")))
      (setf (slot-value backend 'epic-custom-field) field)))
  (slot-value backend 'epic-custom-field))

(defun jira->issue (issue)
  (let* ((id (alist-get 'id issue))
         (key (alist-get 'key issue))
         (fields (alist-get 'fields issue))
         (name (alist-get 'summary fields))
         (workflow_state_id
          (->> fields (alist-get 'status) (alist-get 'id))))
    (append `((id . ,id)
              (key . ,key)
              (name . ,name)
              (workflow_state_id . ,workflow_state_id))
            fields)))

(cl-defmethod org-tracker-backend/create-issue
  ((backend org-tracker-jira-backend)
   &key title project-id epic-id workflow-state-id issue-type description labels
   parent)
  (message "%s" issue-type)
  (org-tracker--with-jira-backend backend
    (let* ((epic-field (jiralib2--get-epic-custom-field backend))
           (fields `((summary . ,title)
                     (project . ((id . ,project-id)))
                     (,epic-field . ,epic-id)
                     ;; (status . ((id . ,workflow-state-id)))
                     (issuetype . ((id . ,issue-type)))
                     (description . ,description)
                     (labels . ,labels)
                     (parent . ,parent)))
           (params `((fields . ,fields)))
           (create-resp (jiralib2-session-call
                         "/rest/api/2/issue"
                         :type "POST"
                         :data (json-encode params)))
           (issue (jiralib2-session-call
                   (format "/rest/api/2/issue/%s"
                           (alist-get 'id create-resp)))))
      (jira->issue issue))))

(defun org-tracker-jira--epic-issue-type-id (backend)
  "Query BACKEND for the id of the issue type named \"Epic\"."
  (->> (org-tracker-backend/issue-types backend)
       (-find (lambda (it) (equal (cdr it) "Epic")))
       car))

(cl-defmethod org-tracker-backend/create-epic
  ((backend org-tracker-jira-backend)
   &key title project-id milestone-id description labels)
  (org-tracker--with-jira-backend backend
    (let* ((epic-name-field (jiralib2--get-custom-field backend "Epic Name"))
           (epic-issue-type (org-tracker-jira--epic-issue-type-id backend))
           (fields `((summary . ,title)
                     (project . ((id . ,project-id)))
                     (,epic-name-field . ,title)
                     (description . ,description)
                     (labels . ,labels)
                     (issuetype . ((id . ,epic-issue-type)))))
           (params `((fields . ,fields)))
           (create-resp (jiralib2-session-call
                         "/rest/api/2/issue"
                         :type "POST"
                         :data (json-encode params)))
           (issue (jiralib2-session-call
                   (format "/rest/api/2/issue/%s"
                           (alist-get 'id create-resp)))))
      (jira->issue issue))))

(defun org-tracker-jira--find-transition (backend issue-id to-status-id)
  (org-tracker--with-jira-backend backend
    (->>
        (jiralib2-session-call
         (format
          "/rest/api/2/issue/%s/transitions"
          issue-id))
      (alist-get 'transitions)
      (-find (lambda (tr)
               (equal to-status-id
                      (->> tr
                        (alist-get 'to)
                        (alist-get 'id)))))
      (alist-get 'id))))

(cl-defmethod org-tracker-backend/update-issue
  ((backend org-tracker-jira-backend)
   issue-id
   &key epic-id workflow-state-id assignee description title labels)
  (org-tracker--with-jira-backend backend
    (when workflow-state-id
      (if-let ((transition-id (org-tracker-jira--find-transition
                               backend
                               issue-id
                               workflow-state-id)))
          (jiralib2-session-call
           (format "/rest/api/2/issue/%s/transitions"
                   issue-id)
           :type "POST"
           :data (json-encode `((transition . ((id . ,transition-id))))))
        (error
         "Could not find transition to state %s for issue %s"
         workflow-state-id
         issue-id)))
    (when-let ((fields (alist-remove-nils
                        `((,(jiralib2--get-epic-custom-field backend)
                           .
                           ,epic-id)
                          (summary . ,title)
                          (description . ,description)
                          (assignee . ((id . ,assignee)))
                          (labels . ,labels)))))
      (jiralib2-session-call
       (concat "/rest/api/2/issue/" issue-id)
       :type "PUT"
       :data (json-encode `((fields . ,fields)))))))

(cl-defmethod org-tracker-backend/create-subtask
  ((backend org-tracker-jira-backend)
   &rest rest-keys
   &key parent-issue-id project-id
   &allow-other-keys)
  (org-plist-delete rest-keys :parent-issue-id)
  (let ((issue-type
         (->>
          (org-tracker--with-jira-backend backend
            (jiralib2-session-call
             "/rest/api/2/issuetype"))
          (-filter (lambda (i) (eq 't (alist-get 'subtask i))))
          (-filter (lambda (i)
                     (string-equal
                      project-id
                      (->> i
                           (alist-get 'scope)
                           (alist-get 'project)
                           (alist-get 'id)))))
          car
          (alist-get 'id))))
    (unless issue-type
      (error "Could not find Sub-Task issue type for project %s"
             project-id))
    (apply
     #'org-tracker-backend/create-issue
     backend
     :parent `((key . ,parent-issue-id))
     :issue-type issue-type
     rest-keys)))

(cl-defmethod org-tracker-backend/populate-issue
  ((backend org-tracker-jira-backend) story)
  (let ((key (alist-get 'key story)))
    (org-tracker--with-jira-backend backend
      (jira->issue
       (jiralib2-get-issue key)))))

(cl-defmethod org-tracker-backend/issue-kv->prop-kv
  ((backend org-tracker-jira-backend) key value)
  (when value
    (case key
      (key
       (cons
        "jira-id"
        (org-link-make-string
         (format "%s/browse/%s"
                 (jira-url backend)
                 value)
         value)))
      (issuetype
       (cons
        "issue-type"
        (if (stringp value)
            (alist-get-equal value
                             (org-tracker-backend/issue-types
                              backend))
          (alist-get 'name value))))
      (parent
       (cons
        "parent"
        (alist-get 'key value)))
      (otherwise
       (cond
        ((or
          (eq key 'epic-id)
          (equal (symbol-name key)
                 (org-tracker--with-jira-backend backend
                   (jiralib2--get-epic-custom-field backend))))
         (cons
          "epic-id"
          (org-link-make-string
           (format "%s/browse/%s"
                   (jira-url backend)
                   value)
           value))))))))


(provide 'org-tracker-jira)
;;; org-tracker-jira.el ends here
