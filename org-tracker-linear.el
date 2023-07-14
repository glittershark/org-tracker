;;; org-tracker-linear.el --- Linear backend for org-tracker -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2023 Griffin Smith
;;
;; Author: Griffin Smith <https://github.com/glittershark>
;; URL: https://github.com/glittershark/org-tracker
;; Maintainer: Griffin Smith <root@gws.fyi>
;; Created: May 31, 2023
;; Modified: May 31, 2023
;; Version: 0.0.1
;; Package-Requires: ((emacs "25.1") (org "9.3"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Linear backend for org-tracker
;;
;;; Code:

(defclass org-tracker-linear-backend ()
  ((auth-token
    :initarg :auth-token
    :accessor auth-token)
   (team
    :initarg :team
    :accessor org-tracker-linear-backend/team)
   (team-slug
    :initarg :team-slug
    :accessor org-tracker-linear-backend/team-slug)))

(defvar linear-auth-token nil
  "Authorization token for the Linear API.")


(defun org-tracker--call-with-linear-backend
  (backend f)
  (let ((linear-auth-token (auth-token backend)))
    (funcall f)))

(defmacro org-tracker--with-linear-backend (backend &rest body)
  (declare (indent 1))
  `(org-tracker--call-with-linear-backend ,backend (lambda () ,@body)))

(defvar linear-graphql-endpoint* "https://api.linear.app/graphql")
(cl-defun linear-request (query &optional (variables '()))
  (let* ((url-request-method "POST")
         (url-request-extra-headers
          `(("Content-Type" . "application/json")
            ("Authorization" . ,(concat "Bearer " linear-auth-token))))
         (url-request-data
          (json-encode `(("query" . ,query)
                         ("variables" . ,variables))))
         (buf))
    (setq buf (url-retrieve-synchronously linear-graphql-endpoint*))
    (with-current-buffer buf
      (goto-char url-http-end-of-headers)
      (if (>= url-http-response-status 400)
          (let ((resp (buffer-substring-no-properties
                       (point)
                       (point-max))))
            (kill-buffer)
            (error "Request failed: %s" resp))
        (alist-get 'data (prog1 (json-read)
                           (kill-buffer)))))))

(cl-defun linear-load-all-pages (field
                                 query-fragment
                                 &key
                                 (args "")
                                 (variable-declarations "")
                                 (variables '()))
  (let*
      ((field-sym (intern field))
       (query (format "query Page($after: String%s) {
                          %s(first: 100, after: $after%s) {
                            nodes {
                              %s
                            }
                            pageInfo {
                              hasNextPage
                              endCursor
                            }
                          }
                        }"
                      (if (string-empty-p variable-declarations)
                          ""
                        (concat ", " variable-declarations))
                      field
                      (if (string-empty-p args) ""
                        (concat ", " args))
                      query-fragment))
       (resp (linear-request query variables))
       (results (-> resp
                    (->> (alist-get field-sym))
                    (->> (alist-get 'nodes))
                    (coerce 'list))))
    (while (->> resp
                (alist-get field-sym)
                (alist-get 'pageInfo)
                (alist-get 'hasNextPage)
                (eq 't))
      (setq resp (linear-request
                  query
                  (append variables
                          `(("after"
                             .
                             ,(->> resp
                                   (alist-get field-sym)
                                   (alist-get 'pageInfo)
                                   (alist-get 'endCursor)))))))
      (appendq! results (-> resp
                            (->> (alist-get field-sym))
                            (->> (alist-get 'nodes))
                            (coerce 'list))))
    results))

(defun org-tracker-linear-backend/team-id (backend)
  (org-tracker--with-linear-backend backend
    (->
     (linear-request
      "
query TeamId($teamName: String) {
  teams(first: 1, filter: { name: { eq: $teamName } }) {
    nodes {
      id
    }
  }
}
"
      `((teamName . ,(org-tracker-linear-backend/team backend))))
     (->> (alist-get 'teams))
     (->> (alist-get 'nodes))
     (aref 0)
     (->> (alist-get 'id)))))

(cl-defmethod org-tracker-backend/projects
  ((backend org-tracker-linear-backend))
  (org-tracker--with-linear-backend backend
    (to-id-name-pairs
     (linear-load-all-pages
      "projects"
      "id name"))))

(cl-defmethod org-tracker-backend/workflow-states
  ((backend org-tracker-linear-backend))
  (org-tracker--with-linear-backend backend
    (to-id-name-pairs
     (linear-load-all-pages
      "workflowStates"
      "id name"
      :variable-declarations "$teamName: String"
      :args "filter: { team: { name: { eq: $teamName } } }"
      :variables `((teamName . ,(org-tracker-linear-backend/team backend)))))))


(cl-defmethod org-tracker-backend/labels
  ((backend org-tracker-linear-backend))
  (org-tracker--with-linear-backend backend
    (to-id-name-pairs
     (linear-load-all-pages
      "issueLabels"
      "id name"))))

(cl-defmethod org-tracker-backend/search-issues
  ((backend org-tracker-linear-backend) query &key detailed)
  (org-tracker--with-linear-backend backend
    (-map
     #'linear->issue
     (linear-load-all-pages
      "issueSearch"
      "id
       number
       team { key }
       name: title
       description
       state { id }"
      :variable-declarations "$query: String"
      :args "query: $query"
      :variables `((query . ,query))))))

(cl-defmethod org-tracker-backend/whoami
  ((backend org-tracker-linear-backend))
  (org-tracker--with-linear-backend backend
    (->>
     (linear-request "query Whoami { viewer { id } }")
     (alist-get 'viewer)
     (alist-get 'id))))

(defun linear->issue (issue)
  (setf (alist-get 'workflow_state_id issue)
        (->> issue
             (alist-get 'state)
             (alist-get 'id)))
  (setf (alist-get 'key issue)
        (format "%s-%s"
                (->> issue
                     (alist-get 'team)
                     (alist-get 'key))
                (alist-get 'number issue)))
  (setf (alist-get 'labels issue)
        (->> issue
             (alist-get 'raw_labels)
             (alist-get 'nodes)
             (--map (alist-get 'name it))))
  issue)

(cl-defmethod org-tracker-backend/fetch-issue
  ((backend org-tracker-linear-backend) issue-id)
  (org-tracker--with-linear-backend backend
    (let* ((resp
            (linear-request
             "query FetchIssue($id: String!) {
                issue(id: $id) {
                  id
                  number
                  team { key }
                  name: title
                  description
                  state { id }
                  raw_labels: labels {
                    nodes {
                      id
                      name
                    }
                  }
                }
              }"
             `((id . ,issue-id))))
           (issue (alist-get 'issue resp)))
      (linear->issue issue))))

(cl-defmethod org-tracker-backend/create-issue
  ((backend org-tracker-linear-backend)
   &key title project-id epic-id workflow-state-id issue-type description labels
   parent)
  (org-tracker--with-linear-backend backend
    (let* ((team-id (org-tracker-linear-backend/team-id backend))
           (label-ids (--map (alist-get 'id it)
                             (linear-load-all-pages
                              "issueLabels"
                              "id"
                              :variable-declarations "$labelNames: [String!]"
                              :args "filter: { name: { in: $labelNames } }"
                              :variables `((labelNames . ,labels)))))
           (resp
            (linear-request
             (format
              "
mutation CreateIssue(
  $teamId: String!
  $title: String!
  %s
  %s
  $description: String
  $labelIds: [String!]
  $parentId: String
) {
  issueCreate(
    input: {
      teamId: $teamId
      title: $title
      %s
      %s
      description: $description
      labelIds: $labelIds
      parentId: $parentId
    }
  ) {
    issue {
      id
      number
      team { key }
      name: title
      description
      state { id }
      raw_labels: labels {
        nodes {
          id
          name
        }
      }
    }
  }
}"
              (if project-id "$projectId: String" "")
              (if workflow-state-id "$stateId: String" "")
              (if project-id "projectId: $projectId" "")
              (if workflow-state-id "stateId: $stateId" ""))
             `((teamId . ,team-id)
               (title . ,title)
               (projectId . ,(or project-id :json-null))
               (stateId . ,(or workflow-state-id :json-null))
               (description . ,description)
               (labelIds . ,(or label-ids (make-vector 0 nil)))))))
      (->> resp
           (alist-get 'issueCreate)
           (alist-get 'issue)
           (linear->issue)))))

(comment
 (let ((compare-symbols (lambda (x y) (string-lessp
                                       (symbol-name x)
                                       (symbol-name y)))))
   (-sort compare-symbols
          '(id
            key
            name
            description
            workflow_state_id)))
 )

(cl-defmethod org-tracker-backend/populate-issue
  ((backend org-tracker-linear-backend) story)

  (if (let ((compare-symbols (lambda (x y) (string-lessp
                                            (symbol-name x)
                                            (symbol-name y)))))
        (equal (-sort compare-symbols
                      '(id
                        key
                        name
                        description
                        workflow_state_id))
               (-sort compare-symbols (-map #'car story))))
      story
    (let* ((key (alist-get 'key story))
           (number (string-to-number (cadr (split-string key "-")))))
      (org-tracker--with-linear-backend backend
        (->
         (linear-request
          "
query FetchIssue($number: Float) {
  issues(first: 1, filter: { number: { eq: $number } }) {
    nodes {
      id
      number
      team { key }
      name: title
      description
      state { id }
      raw_labels: labels {
        nodes {
          id
          name
        }
      }
    }
  }
}"
          `((number . ,number)))
         (->> (alist-get 'issues))
         (->> (alist-get 'nodes))
         (aref 0)
         (linear->issue))))))

(cl-defmethod org-tracker-backend/issue-kv->prop-kv
  ((backend org-tracker-linear-backend) key value)
  (when value
    (case key
      (id (cons "linear-id" value))
      (key
       (cons "linear-key"
             (org-link-make-string
              (format "https://linear.app/%s/issue/%s"
                      (org-tracker-linear-backend/team-slug backend)
                      value)
              value))))))

(cl-defmethod org-tracker-backend/extract-issue-id
  ((_backend org-tracker-linear-backend) elt &optional _property)
  (message "%s" elt)
  (plist-get elt :LINEAR-ID))

(cl-defmethod org-tracker-backend/update-issue
  ((backend org-tracker-linear-backend)
   issue-id
   &key epic-id workflow-state-id assignee description title labels)
  (org-tracker--with-linear-backend backend
    (linear-request
     "
mutation UpdateIssue($input: IssueUpdateInput!, $issueUpdateId: String!) {
  issueUpdate(input: $input, id: $issueUpdateId) {
    success
  }
}"
     `(("issueUpdateId" . ,issue-id)
       ("input" . ,(alist-remove-nils
                    `(("stateId" . ,workflow-state-id)
                      ("assigneeId" . ,assignee)
                      ("description" . ,description)
                      ("title" . ,title)
                      ("labelIds" . ,labels))))))))

(comment
 (split-string "REA-1234" "-")

 (linear-load-all-pages
  "workflowStates"
  "id name")

 (let ((backend
        (make-instance 'org-tracker-linear-backend
                       :auth-token linear-auth-token
                       :team "ReadySet")))
   ;; (org-tracker-backend/create-issue
   ;;  backend
   ;;  :title "Test"
   ;;  :description "Test"
   ;;  )

   )

 (let ((backend
        (make-instance 'org-tracker-linear-backend
                       :auth-token linear-auth-token
                       :team "ReadySet")))
   (org-tracker-backend/workflow-states backend))

 (let ((x (make-vector 3 'a))) (coerce x 'list))

 (let ((backend
        (make-instance 'org-tracker-linear-backend
                       :auth-token linear-auth-token
                       :team "ReadySet")))
   (setq issue
         (org-tracker-backend/populate-issue backend `((key . "REA-2229")))))

 ()
 (alist-get 'workflow_state_id issue)

 )

(provide 'org-tracker-linear)
;;; org-tracker-linear.el ends here
