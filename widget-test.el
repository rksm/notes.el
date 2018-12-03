(with-current-buffer notes.el-widgets-buffer-name

  ;; (message "%s" notes.el--widgets-doc-insertion-marker)
  
  ;; (let ((widget (cadr widget-test-state)))
  ;;   widget
  ;;   (setq w widget)
  ;;   (widget-value-set w "test")

  ;;   ;; (widget-delete widget)
  ;;   )


  (goto-char notes.el--widgets-doc-insertion-marker)
  (setq w (widget-create 'push-button :notify (lambda (btn &rest ignore) (message "fooo")) "fooo"))
  )


(widget-get w :from)

(widget-value w)



(setq docs (notes.el-get-all-docs))

(type-of docs)
(type-of w)
(widget-type w)


(seq-map (lambda (doc) (let ((item (make-hash-table))) (puthash :doc doc item) (puthash :widget nil item) item)) docs)

(defclass notes.el-item ()
  ((doc :initarg :doc
         :initform nil
         :type cons)
   (widget :initarg :widget
         :initform nil
         :type cons))
  "A single record for tracking people I know.")
