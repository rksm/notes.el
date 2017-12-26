;; -*- no-byte-compile: t; -*-


;; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
;; note widget buffer
;; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

(require 'widget)
(eval-when-compile (require 'wid-edit))

(defconst notes.el-widgets-buffer-name "*notes.el*")
(defvar notes.el-widgets-rendered-items nil)
(defvar notes.el-widgets-sort-by 'name)
;; (setq notes.el-widgets-sort-by 'name)

(defun notes.el-widgets-create-buffer ()
  "Create the widgets from the Widget manual."
  (interactive)
  (switch-to-buffer notes.el-widgets-buffer-name)
  (kill-all-local-variables)
  (make-local-variable 'notes.el-widgets-rendered-items)
  ;; (make-local-variable 'notes.el-widgets-sort-by)

  (let ((inhibit-read-only t))
    (erase-buffer))
  (remove-overlays)

  (widget-insert "Notes:\n\n")

  ;; sort widget
  (widget-create 'radio-button-choice
                 :value (prin1-to-string notes.el-widgets-sort-by)
                 :notify (lambda (widget &rest ignore)
  			   (setq notes.el-widgets-sort-by
  				 (intern-soft (widget-value widget)))
  			   (notes.el-widgets-create-buffer))
                 '(const "name") '(const "time"))

  (widget-insert "\n")

  ;; add note widget
  (widget-create 'push-button
		 :notify (lambda (btn &rest ignore)
			   )
		 "New note")
  (widget-insert "\n\n")

  ;; the note filter
  (widget-create 'editable-field
                 :size 20
                 :format "Filter: %v"	; Text after the field!
		 :notify (lambda (widget &rest ignore)
			   (notes.el-widgets-apply-filter (widget-value widget))))

  (widget-insert "\n\n")

  ;; render note items
  (setq-local notes.el-widgets-rendered-items nil)
  (let* ((docs (notes.el-get-all-docs))
	 (max-size (seq-max (seq-map
			     (lambda (ea) (->> ea (assoc-value "name") length))
			     docs)))
	 (sorted-docs (notes.el-widgets-sort-docs docs)))
    (dolist (doc sorted-docs)
      (push (notes.el-widgets-make-item-and-render doc max-size)
	    notes.el-widgets-rendered-items))
    (setq notes.el-widgets-rendered-items (reverse notes.el-widgets-rendered-items)))

  (widget-setup)
  (use-local-map widget-keymap)
  (beginning-of-buffer))


(defun notes.el-widgets-sort-docs (docs)
  (let ((sort-fn (cond
		  ((eq notes.el-widgets-sort-by 'time)
		   (lambda (doc-a doc-b) (string<
					  (->> doc-a notes.el--get-doc-timestamp-string)
					  (->> doc-b notes.el--get-doc-timestamp-string))))
		  (t (lambda (doc-a doc-b) (string<
					    (->> doc-a (assoc-value "name"))
					    (->> doc-b (assoc-value "name"))))))))
    (reverse (seq-sort sort-fn docs))))

(defun notes.el-widgets-make-item-and-render (doc &optional name-col-width)
  "create an item and render it"
  (let ((item (make-hash-table)))
    (puthash :visible t item)
    (puthash :doc doc item)
    (notes.el-widgets-render-item item name-col-width)
    item))

(defun notes.el-widgets-render-item (item &optional name-col-width)
  "render the item"
  (let* ((doc (gethash :doc item))
	 (widgets (notes.el-widgets-render-doc doc name-col-width)))
    (puthash :widgets widgets item)
    (puthash :row (line-number-at-pos) item)
    (puthash :rendered t item)
    item))


(defun notes.el-widgets-goto-item (item)
  (when (->> item (gethash :rendered))
   (beginning-of-buffer)
   (search-forward (concat "[" (->> item (gethash :doc) (assoc-value "name")) "]")
		   nil t)
   (beginning-of-line)
   (center-line)))

(defun notes.el-widgets-render-doc (doc &optional name-col-width)
  "build the widget for the item's document object"
  (lexical-let ((name (notes.el-get-doc-attr doc "name"))
		(doc doc))
    (list
     (widget-create 'link
		    :notify (lambda (&rest ignore)
			      (notes.el-open-doc-named name doc)
			      ;; (widget-value-set widget-example-repeat
			      ;;                   '("En" "To" "Tre"))
			      ;; (widget-setup)
			      )
		    name)

     (let ((spacer (if name-col-width (s-pad-right (- name-col-width (length name)) " " "") " ")))
       (widget-insert (concat spacer (notes.el--get-doc-timestamp-string doc) " ")))

     (widget-create 'push-button
		    :notify (lambda (btn &rest ignore)
			      (when
				  (yes-or-no-p (format "Reall delete note '%s'?" name))
				(message "delete")))
		    "X")
     (widget-insert "\n"))))

(defun notes.el-widgets-remove (item)
  "remove a rendered item (it's widget) and mark is as unrendered"
  (when (gethash :rendered item)
    (dolist (widget (gethash :widgets item))
      (when widget
	(widget-delete widget)))
    (puthash :rendered nil item)
    (puthash :widgets nil item)
    (let ((inhibit-read-only t))
      (delete-region (save-excursion (beginning-of-line) (point))
		     (save-excursion (forward-line) (point))))))

;; (with-current-buffer notes.el-widgets-buffer-name (notes.el-widgets-apply-filter "test"))
;; (with-current-buffer notes.el-widgets-buffer-name (notes.el-widgets-apply-filter ""))

(defun notes.el-widgets-apply-filter (filter-string)
  "removes the rendered note widgets that don't match `filter-string'"
  (save-excursion
    (if (string-empty-p filter-string)
	(dolist (item notes.el-widgets-rendered-items) (puthash :visible t item))
      (dolist (item notes.el-widgets-rendered-items)
	(let* ((terms (split-string filter-string "\s+"))
	       (doc (gethash :doc item))
	       (name (notes.el-get-doc-attr doc "name"))
	       (content (notes.el-get-doc-content doc))
	       (visible (seq-every-p (lambda (term) (or (string-match-p term name) (string-match-p term content))) terms)))
	  (puthash :visible visible item))))

    (goto-line 10)
    (let ((max-size (seq-max (seq-map
			      (lambda (ea) (->> ea (gethash :doc) (assoc-value "name") length))
			      notes.el-widgets-rendered-items))))
      (message "%s" max-size)
      (dolist (item notes.el-widgets-rendered-items)
	(let ((visible (gethash :visible item))
	      (rendered (gethash :rendered item)))
	  (cond
	   ((and      visible       rendered) (forward-line))
	   ((and (not visible) (not rendered)))
	   ((and (not visible)      rendered) (notes.el-widgets-remove item))
	   ((and      visible  (not rendered)) (notes.el-widgets-render-item item max-size))))))))

;; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-


(provide 'notes-widgets)
