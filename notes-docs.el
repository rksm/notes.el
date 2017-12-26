;; -*- no-byte-compile: t; -*-

;; - [ ] delete doc
;; - [ ] create doc
;; - [ ] save doc
;; - [X] search throigh docs
;; - [X] list docs
;; - [X] list docs by date


(require 'sofa)

(setq sofa-server "sofa.kra.hn"
      sofa-port 443
      sofa-use-https t
      sofa-limit)

(defcustom notes.el-default-db "notes-from-lively"
  "The default database of `sofa-server' to use for notes"
  :type 'string
  :group 'notes.el)

(comment

 (sofa-database-exist-p db)

 (setq info (sofa-get-database-info db))

 (pp info)

 (sofa-get-bulk-documents db )


 (setq doc-ids
       (let* ((json (sofa-get (sofa-view-endpoint db nil nil :include-docs t)))
              (err (assoc-value "error" json nil))
              (rows (assoc-value "rows" json nil)))
	 (->> rows
	      (seq-map (lambda (ea) (assoc-value "id" ea)))
	      (-reject (lambda (ea) (s-starts-with? "_" ea))))))




 (-filter '---truthy? (seq-uniq (seq-map (lambda (doc) (notes.el-get-doc-attr doc "content" "mode")) docs)))

 (notes.el-get-doc-attr (car docs) "_id")
 (notes.el-get-doc-attr (car docs) "content" "mode")

 (funcall 'fundamental-mode)
 (setq doc-id (car doc-ids))
 (setq doc (sofa-get-document db doc-id))

 (helm-build-sync-source "test"
   :candidates '(a b c d e))

 volatile

 (helm :sources (helm-build-sync-source "Major modes" :candidates (list-major-modes)))


 (helm :sources
       `(,(helm-build-dummy-source
	      "New note"
	    :action (helm-make-actions
		     "Create note"
		     (lambda (candidate)
		       (message "creating note %s" candidate)
		       ;; (switch-to-buffer buffer)
		       )))
	 ,(helm-build-sync-source "notes.el list"
	    :candidates doc-ids
	    :action 'notes.el-open-doc-named))
       :buffer "*helm notes.el source*")


 (setq doc-id "SQL Postgres")

 ;; (makunbound 'doc)
 ;; (makunbound 'content)
 ;; (makunbound 'mode)
 ;; (makunbound 'doc-buffer)

 (assoc-value "content" doc)

 (notes.el-get-doc-attr doc "content" "mode")
 (notes.el-create-doc "test-doc-1")

 (setq notes.el-database-doc-cache (remove* notes.el-default-db notes.el-database-doc-cache :test 'equal :key 'car))
 (length notes.el--database-cache)


 (car docs)
 (assoc "_rev" (car docs))


 (push (pairlis '(notes.el-default-db) '(1)) -notes-database-doc-cache)

 (notes.el--cache-clear!)
 (setq docs (notes.el-get-all-docs))
 
 (setq ts (assoc-value "timestamp" (car docs)))

 (format-time-string "%Y-%m-%d %H:%M:%S" (rk/msecs-to-time ts))
 

 (setq test-doc `(("_id" . "test-doc-from-emacs")
		  ("_rev" . ,(assoc-value "_rev" (notes.el-get-doc "test-doc-from-emacs")))
		  ("name" . "test-doc-from-emacs")
		  ("timestamp" . (rk/time-in-msecs))
		  ("content"
		   ("textAndAttributes" . ["foo barrrr bax 2" nil])
		   ("mode" . "md"))))

 (notes.el--set-document test-doc)

 (setq rev (sofa-put-document notes.el-default-db (assoc-value "_id" test-doc) test-doc rev))

 ("_id" . "-- TODO --")

 (notes.el-create-doc "test-doc-1")

 (setq cache (assoc-value notes.el-default-db notes.el-database-doc-cache))
 (assoc-value notes.el-default-db notes.el-database-doc-cache)
 (let* ((cache (assoc-value notes.el-default-db notes.el-database-doc-cache))
	(found (assoc-value (assoc "_id" test-doc) cache)))
   (when found (setq cache (delq found cache)))
   (push test-doc cache)
   )

 (notes.el-delete-doc "test-doc-from-emacs")

 (sofa-document-revision notes.el-default-db "test-doc-from-emacs")

 )


;; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

(require 'cl-lib)

(defvar notes.el-mode-map
  '(("shell" . shell-script-mode)
    ("c_cpp" . c++-mode)
    ("clojure" . clojure-mode)))

(defun notes.el-lookup-mode (mode-name)
  (or
   (assoc-value mode-name notes.el-mode-map)
   (assoc-default (concat "." mode-name) auto-mode-alist 'string-match)
   'fundamental-mode))

(defmacro notes.el-get-doc-attr (doc &rest attrs)
  "retrieves the deep value specified by attrs from doc,
  e.g. (notes.el-get-doc-attr doc \"content\" \"mode\")"
  `(->> ,doc
	,@(seq-map (lambda (sym) `(assoc-value ,sym)) attrs)))

(defun notes.el-get-doc (doc-id &optional database)
  (or (notes.el--cache-get-doc doc-id database)
      (let* ((doc (sofa-get-document (or database notes.el-default-db) doc-id))
	     (error (assoc-value "error" doc))
	     (reason (assoc-value "reason" doc)))
	(when error (error "%s: %s" error reason))
	(unless doc (error "failed to retrieve document %s" doc-id))
	(notes.el--cache-add-doc doc database)
	doc)))

(defvar notes.el--database-cache nil)
;; (setq notes.el--database-cache nil)

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
		    ("mode" . "md")))))
    (condition-case err
	(let ((rev (assoc-value "_rev" (notes.el-get-doc name database))))
	  (setq new-doc (append new-doc `(("_rev" . ,rev)))))
      (error (message "hmmmm %s" err)))
    (notes.el--set-document new-doc database)))

(defun notes.el--set-document (doc &optional database)
  ;; 1. PUT document
  (let* ((db (or database notes.el-default-db))
	 (doc-id (assoc-value "_id" doc))
	 (old-rev (assoc-value "_rev" doc))
	 (new-rev (sofa-put-document db doc-id doc))
	 (new-doc (rk/alist-put "_rev" new-rev test-doc)))

    ;; 2. update cache
    (notes.el--cache-add-doc new-doc db)
    new-doc))

(defun notes.el--cache-get-doc (doc-id &optional database)
  (let ((cached-docs (assoc-value (or database notes.el-default-db) notes.el--database-cache)))
    (assoc-value `("_id" . ,doc-id) cached-docs)))

(defun notes.el--cache-add-doc (doc &optional database)
  (notes.el--cache-remove-doc doc database)
  (let* ((db (or database notes.el-default-db))
	 (cached-docs (assoc-value db notes.el--database-cache)))
    (when cached-docs
      (notes.el--change-cache! db (append doc cached-docs)))))

(defun notes.el--cache-remove-doc (doc-or-id &optional database)
  (let* ((db (or database notes.el-default-db))
	 (cached-docs (assoc-value db notes.el--database-cache))
	 (doc-id (if (stringp doc-or-id) doc-or-id (assoc-value "_id" doc-or-id))))
    (when cached-docs
      (let ((id-pair `("_id" . ,doc-id)))
	(notes.el--change-cache!
	 db
	 (--> cached-docs
	      (remove* id-pair it :test 'equal :key 'car)))))))

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
  (let ((doc-id (if (stringp doc-or-id) doc-or-id (assoc-value "_id" doc-or-id)))
	(database (or database notes.el-default-db)))
    (sofa-delete-document database doc-id)))


(defun notes.el-open-doc-named (doc-id &optional doc database)
  "docstring"
  (let* ((doc (or doc (notes.el-get-doc doc-id database)))
	 (content (notes.el-get-doc-content doc))
	 (mode (notes.el-lookup-mode (notes.el-get-doc-attr doc "content" "mode")))
	 (doc-buffer (rk/get-fresh-buffer (concat "*rk/notes \"" doc-id "\" "))))
    (with-current-buffer doc-buffer
      (funcall mode)
      (notes.el-mode)
      (save-excursion (insert content))
      (set-buffer-modified-p nil)
      (pop-to-buffer doc-buffer))))

(defun notes.el-mode-save-buffer ()
  (message "Saving notes of %s..." (buffer-name))
  (set-buffer-modified-p nil)
  t)

(defun notes.el-mode-really-kill-buffer ()
  (if (buffer-modified-p)
    (yes-or-no-p (format "Kill modified buffer %s? " (buffer-name)))
    t))

(define-minor-mode notes.el-mode
  "Notes from my notes db, https://sofa.kra.hn/notes-from-lively"
  :init-value nil
  :lighter " notes.el"
  :keymap
  :group 'notes.el
  (make-variable-buffer-local 'write-contents-functions)
  (make-variable-buffer-local 'notes.el-mode-really-kill-buffer)
  (setq-local write-contents-functions '(notes.el-mode-save-buffer))
  (setq-local kill-buffer-query-functions '(notes.el-mode-really-kill-buffer)))


(provide 'notes-docs)
