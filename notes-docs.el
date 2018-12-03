;; -*- no-byte-compile: t; -*-

;; - [X] delete doc
;; - [X] create doc
;; - [X] save doc
;; - [X] search throigh docs
;; - [X] list docs
;; - [X] list docs by date
;; - [ ] autoloads
;; - [ ] package
;; - [ ] tests


(require 'sofa)
(require 'cl-lib)

(defvar notes.el-mode-map
  '(("shell" . shell-script-mode)
    ("c_cpp" . c++-mode)
    ("clojure" . clojure-mode)))

(defvar notes.el--current-doc nil
  "Local variable set in buffers used to edit notes.")
(make-variable-buffer-local 'notes.el--current-doc)
(put 'notes.el--current-doc 'permanent-local t)

(defvar notes.el--database-cache nil
  "Cache for the documents of the default database.")

(defun notes.el-lookup-mode (mode-name)
  (or
   (assoc-value mode-name notes.el-mode-map)
   (assoc-default (concat "." mode-name) auto-mode-alist 'string-match)
   (symbol-function (intern-soft mode-name))
   'fundamental-mode))

(defmacro notes.el-get-doc-attr (doc &rest attrs)
  "retrieves the deep value specified by attrs from doc,
  e.g. (notes.el-get-doc-attr doc \"content\" \"mode\")"
  `(->> ,doc
	,@(seq-map (lambda (sym) `(assoc-value ,sym)) attrs)))

(defun notes.el--doc-id (doc)
  ""
  (or
   (assoc-value "_id" doc)
   (assoc-value "name" doc)
   (error "doc does not have an id")))

(defun notes.el-get-doc (doc-id &optional database)
  (or (notes.el--cache-get-doc doc-id database)
      (let* ((doc (sofa-get-document (or database notes.el-default-db) doc-id))
	     (error (assoc-value "error" doc))
	     (reason (assoc-value "reason" doc)))
	(when error (error "%s: %s" error reason))
	(unless doc (error "failed to retrieve document %s" doc-id))
	(notes.el--cache-add-doc doc database)
	doc)))

(cl-defun notes.el-create-doc (name &optional database
				    &key
				    (timestamp (rk/time-in-msecs))
				    (content "")
				    (mode 'fundamental-mode))
  (let ((new-doc `(("_id" . ,name)
		   ("name" . ,name)
		   ("timestamp" . ,timestamp)
		   ("createdAt" . ,timestamp)
		   ("content"
		    ("textAndAttributes" . [,content nil])
		    ("mode" . ,(if (stringp mode) mode (prin1-to-string mode)))))))
    (condition-case err
	(let ((rev (assoc-value "_rev" (notes.el-get-doc name database))))
	  (setq new-doc (append new-doc `(("_rev" . ,rev)))))
      (error))
    (notes.el--set-document new-doc database)))

(defun notes.el--set-document (doc &optional database)
  ;; 1. PUT document
  (let* ((db (or database notes.el-default-db))
	 (doc-id (notes.el--doc-id doc))
	 (old-rev (assoc-value "_rev" doc))
	 (new-rev (sofa-put-document db doc-id doc))
	 (new-doc (->> doc
		   (rk/alist-put "_rev" new-rev)
		   (rk/alist-put "timestamp" (rk/time-in-msecs)))))

    ;; 2. update cache
    (notes.el--cache-add-doc new-doc db)

    ;; 3. update any notes.el note list buffers
    (let ((buffers
	   (seq-filter (lambda (b) (equal "*notes.el*" (buffer-name b))) (buffer-list))))
      (dolist (buf (buffer-list))
	(when (equal "*notes.el*" (buffer-name buf))
	  (with-current-buffer buf
	    (seq-do
	     (lambda (item)
	       (when (->> item (gethash :doc) (notes.el--doc-id) (equal doc-id))
		 (puthash :doc new-doc item)))
	     notes.el-widgets-rendered-items)))))

    new-doc))

(defun notes.el--cache-get-doc (doc-id &optional database)
  (let ((cached-docs (assoc-value (or database notes.el-default-db) notes.el--database-cache)))
    (assoc-value `("_id" . ,doc-id) cached-docs)))

(defun notes.el--cache-add-doc (doc &optional database)
  (notes.el--cache-remove-doc doc database)
  (let* ((db (or database notes.el-default-db))
	 (cached-docs (assoc-value db notes.el--database-cache)))
    (when cached-docs
      (notes.el--change-cache! db (append (list doc) cached-docs)))))

(defun notes.el--cache-remove-doc (doc-or-id &optional database)
  (let* ((db (or database notes.el-default-db))
	 (cached-docs (assoc-value db notes.el--database-cache))
	 (doc-id (if (stringp doc-or-id) doc-or-id (notes.el--doc-id doc-or-id))))
    (when cached-docs
      (notes.el--change-cache!
       db
       (seq-remove (lambda (doc) (equal doc-id (notes.el--doc-id doc))) cached-docs)))))

(defun notes.el--cache-clear! ()
  (setq notes.el--database-cache nil))

(defun notes.el--change-cache! (db docs)
  (--> notes.el--database-cache
       (remove* db it :test 'equal :key 'car)
       (append `((,db . ,docs)) it)
       (setq notes.el--database-cache it))
  docs)

(defun notes.el-get-all-docs (&optional database)
  (let* ((database (or database notes.el-default-db))
	 (cached (assoc-value database notes.el--database-cache)))
    (if cached
	cached
      (let* ((json (sofa-get (sofa-view-endpoint database nil nil :include-docs t)))
	     (err (assoc-value "error" json nil))
	     (rows (assoc-value "rows" json nil)))
	(when err (error err))
	(let ((docs (seq-map (lambda (ea) (assoc-value "doc" ea)) rows)))
	  (notes.el--change-cache! database docs))))))

(defun notes.el-get-doc-content (doc)
  (--> doc
       (notes.el-get-doc-attr it "content" "textAndAttributes")
       (seq-filter (lambda (ea) (stringp ea)) it)
       -concat
       (s-join "" it)))

(defun notes.el--get-doc-timestamp-string (doc)
  (let ((ts (assoc-value "timestamp" doc)))
    (format-time-string "%Y-%m-%d %H:%M:%S" (rk/msecs-to-time ts))))

(defun notes.el-delete-doc (doc-or-id &optional database)
  (notes.el--cache-remove-doc doc-or-id database)
  (let ((doc-id (if (stringp doc-or-id) doc-or-id (notes.el--doc-id doc-or-id)))
	(database (or database notes.el-default-db)))
    (sofa-delete-document database doc-id)))

(defun notes.el-open-doc-named (doc-id &optional doc database)
  "docstring"
  (let* ((doc (or doc (notes.el-get-doc doc-id database)))
	 (content (notes.el-get-doc-content doc))
	 (mode (notes.el-lookup-mode (notes.el-get-doc-attr doc "content" "mode")))
	 (doc-buffer (rk/get-fresh-buffer (concat "*rk/notes \"" doc-id "\" "))))
    (with-current-buffer doc-buffer
      (condition-case err
       (funcall mode)
       (error (message "Could not activate mode %s b/c %s" mode err)))
      (notes.el-mode)
      (setq notes.el--current-doc doc)
      (save-excursion (insert content))
      (set-buffer-modified-p nil)
      (switch-to-buffer doc-buffer))))

(defun notes.el-mode-save-buffer ()
  (message "Saving notes of %s..." (buffer-name))
  (when (buffer-modified-p)
    (let* ((content
	    (save-restriction
	      (widen)
	      (buffer-substring-no-properties (point-min) (point-max))))
	   (mode (symbol-name major-mode))
	   (doc notes.el--current-doc)
	   (changed-doc (--> doc
			     (assoc-value "content" it)
			     (rk/alist-put "textAndAttributes" `[,content nil] it)
			     (rk/alist-put "mode" mode it)
			     (rk/alist-put "content" it doc)
			     (notes.el--set-document it))))
      (setq notes.el--current-doc changed-doc))
    (message "Saved!")
    (set-buffer-modified-p nil))
  t)
(put 'notes.el-mode-save-buffer 'permanent-local-hook t)

(defun notes.el-mode-really-kill-buffer ()
  (if (buffer-modified-p)
      (yes-or-no-p (format "Kill modified buffer %s? " (buffer-name)))
    t))
(put 'notes.el-mode-really-kill-buffer 'permanent-local-hook t)

(define-minor-mode notes.el-mode
  "Notes from my notes db, https://sofa.kra.hn/notes-from-lively"
  :init-value nil
  :lighter " notes.el"
  :keymap
  :group 'notes.el
  ;; note: all those vars and hooks are permanent-local, i.e. that they aren't
  ;; reset on mode changes and `kill-all-local-variables'
  (add-hook 'write-contents-functions 'notes.el-mode-save-buffer nil t)
  (add-hook 'kill-buffer-query-functions 'notes.el-mode-really-kill-buffer nil t))


(provide 'notes-docs)
