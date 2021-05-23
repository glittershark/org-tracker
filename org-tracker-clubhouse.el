;;; org-tracker-clubhouse.el --- Description -*- lexical-binding: t; -*-
;;;
;;; Copyright (C) 2021 Griffin Smith
;;;
;;; Author: Griffin Smith <https://github.com/glittershark>
;;; Maintainer: Griffin Smith <root@gws.fyi>
;;; Created: May 11, 2021
;;; Modified: May 11, 2021
;;; Version: 0.0.1
;;; Package-Requires: ((emacs "24.3"))
;;;
;;; This file is not part of GNU Emacs.
;;;
;;; Copyright (C) 2018 Griffin Smith
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

;;;
;;;; Commentary:
;;;
;;;  Description
;;;
;;;; Code:

;;;
;;; Configuration
;;;

(defvar clubhouse-auth-token nil
  "Authorization token for the Clubhouse API.")

(defvar clubhouse-username nil
  "Username for the current Clubhouse user.

Unfortunately, the Clubhouse API doesn't seem to provide this via the API given
an API token, so we need to configure this for
`clubhouse-claim-story-on-status-updates' to work")

(defvar clubhouse-team-name nil
  "Team name to use in links to Clubhouse.
ie https://app.clubhouse.io/<TEAM_NAME>/stories")

(defvar clubhouse-workflow-name "Default")

(defvar clubhouse-story-types
  '(("feature" . "Feature")
    ("bug"     . "Bug")
    ("chore"   . "Chore")))

(defvar clubhouse-default-story-type nil
  "Sets the default story type. If set to 'nil', it will interactively prompt
the user each and every time a new story is created. If set to 'feature',
'bug', or 'chore', that value will be used as the default and the user will
not be prompted")


;;;
;;; Org interaction
;;;

(defun org-element-extract-clubhouse-id (elt &optional property)
  (when-let* ((clubhouse-id-link (plist-get elt (or property :CLUBHOUSE-ID))))
    (cond
     ((string-match
       (rx "[[" (one-or-more anything) "]"
           "[" (group (one-or-more digit)) "]]")
       clubhouse-id-link)
      (string-to-number (match-string 1 clubhouse-id-link)))
     ((string-match
       (rx "[[https://app.clubhouse.io/"
           (one-or-more anything)
           "/story/" (group (one-or-more digit)))
       clubhouse-id-link)
      (string-to-number (match-string 1 clubhouse-id-link)))
     ((string-match-p
       (rx buffer-start
           (one-or-more digit)
           buffer-end)
       clubhouse-id-link)
      (string-to-number clubhouse-id-link)))))

(comment
 (let ((strn "[[https://app.clubhouse.io/example/story/2330][2330]]"))
   (string-match
    (rx "[[" (one-or-more anything) "]"
        "[" (group (one-or-more digit)) "]]")
    strn)
   (string-to-number (match-string 1 strn)))
 )

;;;
;;; API interaction
;;;

(defvar clubhouse-base-url* "https://api.clubhouse.io/api/v3")

(defun clubhouse-auth-url (url &optional params)
  (concat url
          "?"
          (url-build-query-string
           (cons `("token" ,clubhouse-auth-token) params))))

(defun clubhouse-baseify-url (url)
 (if (s-starts-with? clubhouse-base-url* url) url
   (concat clubhouse-base-url*
           (if (s-starts-with? "/" url) url
             (concat "/" url)))))

(cl-defun clubhouse-request (method url &key data (params '()))
  (setq url (-> url
              clubhouse-baseify-url
              (clubhouse-auth-url params)))
  (let* ((url-request-method method)
         (url-request-extra-headers
          '(("Content-Type" . "application/json")))
         (url-request-data data)
         (buf))

   (setq buf (url-retrieve-synchronously url))

   (with-current-buffer buf
     (goto-char url-http-end-of-headers)
     (prog1 (json-read) (kill-buffer)))))

(cl-defun clubhouse-fetch-as-id-name-pairs
    (resource &optional
              (id-attr 'id)
              (name-attr 'name))
  "Returns the given resource from clubhouse as (id . name) pairs"
  (let ((resp-json (clubhouse-request "GET" resource)))
    (-> resp-json
        ->list
        reject-archived
        (to-id-name-pairs id-attr name-attr))))

(defun clubhouse-get-story
    (clubhouse-id)
  (clubhouse-request "GET" (format "/stories/%s" clubhouse-id)))

(defcache clubhouse-projects
  "Returns projects as (project-id . name)"
  (clubhouse-fetch-as-id-name-pairs "projects"))

(defcache clubhouse-epics
  "Returns epics as (epic-id . name)"
  (clubhouse-fetch-as-id-name-pairs "epics"))

(defcache clubhouse-milestones
  "Returns milestone-id . name)"
  (clubhouse-fetch-as-id-name-pairs "milestones"))

(defun clubhouse-fetch-workflow-states ()
  "Returns worflow states as (name . id) pairs"
  (let* ((resp-json (clubhouse-request "GET" "workflows"))
         (workflows (->list resp-json))
         ;; just assume it exists, for now
         (workflow  (-find (lambda (workflow)
                             (equal clubhouse-workflow-name
                                    (alist-get 'name workflow)))
                           workflows))
         (states    (->list (alist-get 'states workflow))))
    (to-id-name-pairs states
                      'name
                      'id)))

(defcache clubhouse-labels
  "Returns labels as (label-id . name)"
  (clubhouse-fetch-as-id-name-pairs "labels"))

(defcache clubhouse-whoami
  "Returns the ID of the logged in user"
  (->> (clubhouse-request
        "GET"
        "/members")
       ->list
       (find-if (lambda (m)
                  (->> m
                       (alist-get 'profile)
                       (alist-get 'mention_name)
                       (equal clubhouse-username))))
       (alist-get 'id)))

(defcache clubhouse-iterations
  "Returns iterations as (iteration-id . name)"
  (clubhouse-fetch-as-id-name-pairs "iterations"))

;;;

(defclass org-tracker-clubhouse-backend ()
  ((auth-token :initarg :auth-token :accessor auth-token)
   (username :initarg :username :accessor username)
   (team-name :initarg :team-name :accessor team-name)
   (workflow-name :initarg :workflow-name :accessor workflow-name)))

(defun org-tracker--call-with-clubhouse-backend (backend f)
  (let ((clubhouse-auth-token (auth-token backend))
        (clubhouse-username (username backend))
        (clubhouse-team-name (team-name backend))
        (clubhouse-workflow-name (workflow-name backend)))
    (funcall f)))

(defmacro org-tracker--with-clubhouse-backend (backend &rest body)
  (declare (indent 1))
  `(org-tracker--call-with-clubhouse-backend ,backend (lambda () ,@body)))

(cl-defmethod make-instance
  ((_class (subclass org-tracker-clubhouse-backend)) &rest initargs)
  (let ((new (cl-call-next-method)))
    (fill-defaults
     new
     `((auth-token . ,clubhouse-auth-token)
       (username . ,clubhouse-username)
       (team-name . ,clubhouse-team-name)
       (workflow-name . ,clubhouse-workflow-name)))
    new))

(cl-defmethod org-tracker-backend/extract-ticket-id
  ((_ org-tracker-clubhouse-backend) elt &optional property)
  (org-element-extract-clubhouse-id elt property))

(cl-defmethod org-tracker-backend/projects
  ((backend org-tracker-clubhouse-backend))
  (org-tracker--with-clubhouse-backend
   backend
   (clubhouse-fetch-as-id-name-pairs "projects")))

(cl-defmethod org-tracker-backend/epics
    ((backend org-tracker-clubhouse-backend))
  (org-tracker--with-clubhouse-backend
   backend
   (clubhouse-fetch-as-id-name-pairs "epics")))

(cl-defmethod org-tracker-backend/milestones
    ((backend org-tracker-clubhouse-backend))
  (org-tracker--with-clubhouse-backend
   backend
   (clubhouse-fetch-as-id-name-pairs "milestones")))

(cl-defmethod org-tracker-backend/workflow-states
    ((backend org-tracker-clubhouse-backend))
  (org-tracker--call-with-clubhouse-backend
   backend
   #'clubhouse-fetch-workflow-states))

(cl-defmethod org-tracker-backend/issue-types
    ((_ org-tracker-clubhouse-backend))
  clubhouse-story-types)

(cl-defmethod org-tracker-backend/labels
    ((backend org-tracker-clubhouse-backend))
  (org-tracker--with-clubhouse-backend
   backend
   (clubhouse-fetch-as-id-name-pairs "labels")))

(cl-defmethod org-tracker-backend/whoami
    ((backend org-tracker-clubhouse-backend))
  (org-tracker--with-clubhouse-backend
   backend
   (->> (clubhouse-request
         "GET"
         "/members")
     ->list
     (find-if (lambda (m)
                (->> m
                  (alist-get 'profile)
                  (alist-get 'mention_name)
                  (equal clubhouse-username)))))
   (alist-get 'id)))

(cl-defmethod org-tracker-backend/iterations
    ((backend org-tracker-clubhouse-backend))
  (org-tracker--with-clubhouse-backend
   backend
   (clubhouse-fetch-as-id-name-pairs "iterations")))

(cl-defmethod org-tracker-backend/search-stories
  ((backend org-tracker-clubhouse-backend) query)
  (org-tracker--with-clubhouse-backend backend
    (-> (org-tracker-request "GET" "search/stories"
      :params `((query ,(if query (format "\"%s\"" query) ""))))
      cdadr
      (append nil)
      reject-archived)))

(cl-defmethod org-tracker-backend/create-issue
    ((backend org-tracker-clubhouse-backend)
     &key title project-id epic-id workflow-state-id issue-type description
     labels)
  (cl-assert (and (stringp title)
                  (integerp project-id)
                  (or (null epic-id) (integerp epic-id))
                  (or (null description) (stringp description))))
  (let ((params `((name . ,title)
                  (project_id . ,project-id)
                  (epic_id . ,epic-id)
                  (story_type . ,issue-type)
                  (description . ,(or description ""))
                  (labels . ,labels))))

    (when workflow-state-id
      (push `(workflow_state_id . ,workflow-state-id) params))

    (org-tracker--with-clubhouse-backend
     backend
     (clubhouse-request
      "POST"
      "stories"
      :data
      (json-encode params)))))

(defun clubhouse-link-to-story (backend story-id)
  (format "https://app.clubhouse.io/%s/story/%d"
          (team-name backend)
          story-id))

(defun clubhouse-link-to-epic (backend epic-id)
  (format "https://app.clubhouse.io/%s/epic/%d"
          (team-name backend)
          epic-id))

(defun clubhouse-link-to-milestone (backend milestone-id)
  (format "https://app.clubhouse.io/%s/milestone/%d"
          (team-name backend)
          milestone-id))

(defun clubhouse-link-to-project (backend project-id)
  (format "https://app.clubhouse.io/%s/project/%d"
          (team-name backend)
          project-id))

(cl-defmethod org-tracker-backend/issue-kv->prop-kv
  ((backend org-tracker-clubhouse-backend) key value)
  (case key
    (id
     (cons
      "clubhouse-id"
      (org-link-make-string
       (clubhouse-link-to-story backend value)
       (number-to-string value))))
    (epic_id
     (when value
       (cons
        "clubhouse-epic"
        (org-link-make-string
         (clubhouse-link-to-epic backend value)
         (alist-get value (org-tracker-backend/epics backend))))))
    (project_id
     (cons
      "clubhouse-project"
      (org-link-make-string
       (clubhouse-link-to-project backend value)
       (alist-get value (org-tracker-backend/projects backend)))))
    (story_type
     (cons
      "story-type"
      (alist-get-equal value (org-tracker-backend/issue-types backend))))))

;;;
;;; Links
;;;


(provide 'org-tracker-clubhouse)
;;; org-tracker-clubhouse.el ends here
