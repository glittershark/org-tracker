;;; org-tracker-utils.el wh- -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2021 Griffin Smith
;;
;; Author: Griffin Smith <https://github.com/glittershark>
;; Maintainer: Griffin Smith <root@gws.fyi>
;; Created: May 11, 2021
;; Modified: May 11, 2021
;; Version: 0.0.1
;; Keywords: Symbol’s value as variable is void: finder-known-keywords
;; Homepage: https://github.com/grfn/org-tracker-utils
;; Package-Requires: ((emacs "24.3"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;
;;
;;; Code:


(defmacro comment (&rest _)
  "Comment out one or more s-expressions."
  nil)

(defun ->list (vec) (append vec nil))

(defun reject-archived (item-list)
  (-reject (lambda (item) (equal :json-true (alist-get 'archived item))) item-list))

(defun alist->plist (key-map alist)
  (->> key-map
       (-map (lambda (key-pair)
               (let ((alist-key (car key-pair))
                     (plist-key (cdr key-pair)))
                 (list plist-key (alist-get alist-key alist)))))
       (-flatten-n 1)))

(defun alist-get-equal (key alist)
  "Like `alist-get', but uses `equal' instead of `eq' for comparing keys"
  (->> alist
       (-find (lambda (pair) (equal key (car pair))))
       (cdr)))

(defun invert-alist (alist)
  "Invert the keys and values of ALIST."
  (-map (lambda (cell) (cons (cdr cell) (car cell))) alist))

(comment

 (alist->plist
  '((foo . :foo)
    (bar . :something))

  '((foo . "foo") (bar . "bar") (ignored . "ignoreme!")))
 ;; => (:foo "foo" :something "bar")

 )

(defun find-match-in-alist (target alist)
  (->> alist
       (-find (lambda (key-value)
                   (string-equal (cdr key-value) target)))
       car))

(defun fill-defaults (obj slot-defaults)
  "For each `(slot . default-value)' pair in SLOT-DEFAULTS, set the value of
slot in OBJ to slot-default if it's unbound."
  (loop for (slot . default) in slot-defaults
        unless (slot-boundp obj slot)
        do (set-slot-value obj slot default)))

(defvar org-tracker-cache-clear-functions ())

(defmacro defcache (name args &optional docstring &rest body)
  (let* ((doc (when docstring (list docstring)))
         (cache-var-name (intern (concat (symbol-name name)
                                         "-cache")))
         (clear-cache-function-name
          (intern (concat "clear-" (symbol-name cache-var-name)))))
    `(progn
       (defvar ,cache-var-name :no-cache)
       (defun ,name ()
         ,@doc
         (when (equal :no-cache ,cache-var-name)
           (setq ,cache-var-name (progn ,@body)))
         ,cache-var-name)
       (defun ,clear-cache-function-name ()
         (interactive)
         (setq ,cache-var-name :no-cache))

       (push (quote ,clear-cache-function-name)
             org-tracker-cache-clear-functions))))

(defun org-tracker-clear-cache ()
  (interactive)
  (-map #'funcall org-tracker-cache-clear-functions))

(cl-defun to-id-name-pairs
    (seq &optional (id-attr 'id) (name-attr 'name))
  (->> seq
       ->list
       (-map (lambda (resource)
          (cons (alist-get id-attr   resource)
                (alist-get name-attr resource))))))

(provide 'org-tracker-utils)
;;; org-tracker-utils.el ends here
