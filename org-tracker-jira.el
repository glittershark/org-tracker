;;; org-tracker-jira.el --- JIRA backend for org-tracker -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2021 Griffin Smith
;;
;; Author: Griffin Smith <https://github.com/glittershark>
;; Maintainer: Griffin Smith <root@gws.fyi>
;; Created: May 19, 2021
;; Modified: May 19, 2021
;; Version: 0.0.1
;; Package-Requires: ((emacs "24.3"))
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
  ((backend org-tracker-jira-backend) elt &optional property)
  (when-let* ((jira-id-link (plist-get elt (or property :JIRA-ID))))
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
  ((backend org-tracker-jira-backend) query)
  (org-tracker--with-jira-backend backend
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
                    issue))))))

(cl-defmethod org-tracker-backend/whoami
  ((backend org-tracker-jira-backend))
  (org-tracker--with-jira-backend backend
    (alist-get 'accountId
               (jiralib2-session-call "/rest/api/2/myself"))))

(defun jiralib2--get-epic-custom-field (backend)
  (unless (and (slot-boundp backend 'epic-custom-field)
               (not (null (slot-value backend 'epic-custom-field))))
    (let ((fields (jiralib2-session-call "/rest/api/2/field")))
      (setf
       (slot-value backend 'epic-custom-field)
       (->> fields
         (-find (lambda (field) (equal "Epic Link" (alist-get 'name field))))
         (alist-get 'id)))))
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
   &key title project-id epic-id workflow-state-id issue-type description
   labels)
  (org-tracker--with-jira-backend backend
    (let* ((epic-field (jiralib2--get-epic-custom-field backend))
           (fields
            `((summary . ,title)
              (project . ((id . ,project-id)))
              (,epic-field . ,epic-id)
              ;; (status . ((id . ,workflow-state-id)))
              (issuetype . ((id . ,issue-type)))
              (description . ,description)
              (labels . ,labels)))
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
   &key epic-id workflow-state-id assignee description title)
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
                          (assignee . ((id . ,assignee)))))))
      (jiralib2-session-call
       (concat "/rest/api/2/issue/" issue-id)
       :type "PUT"
       :data (json-encode `((fields . ,fields)))))))

(cl-defmethod org-tracker-backend/populate-issue
  ((backend org-tracker-jira-backend) story)
  (let ((key (alist-get 'key story)))
    (org-tracker--with-jira-backend backend
      (jira->issue
       (jiralib2-get-issue key)))))

(cl-defmethod org-tracker-backend/issue-kv->prop-kv
  ((backend org-tracker-jira-backend) key value)
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
    (otherwise
     (cond
      ((and (equal (symbol-name key)
                   (org-tracker--with-jira-backend backend
                     (jiralib2--get-epic-custom-field backend)))
            value)
       (cons
        "epic-id"
        (org-link-make-string
         (format "%s/browse/%s"
                 (jira-url backend)
                 value)
         value)))))))

(provide 'org-tracker-jira)
;;; org-tracker-jira.el ends here
