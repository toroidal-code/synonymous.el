(require 'url)
(require 'url-http)
(require 'request)
(require 'json)
(require 'thingatpt)

(setq lexical-binding t)

(defmacro get-word (word callback)
  `(request
   (format "http://localhost:9292/%s" ,word)
   :parser 'json-read
   :success ,callback
   :error (function* (lambda (&key error-thrown &allow-other-keys&rest _)
                       (message "Got error: %S" error-thrown)))))

(defmacro synonym-filter (data filterfunc)
  "Returns a vector of words (matching data) with their synonyms filtered according to FILTERFUNC."
  `(cl-map 'vector
	   #'(lambda (word-instance)
	       (setcdr (assoc 'synonyms word-instance)
		       (remove-if #'(lambda (w)
				      (not (funcall ,filterfunc w)))
				  (assoc-default 'synonyms word-instance)))
	       word-instance)
	  data))

(defmacro get-synonyms (word callback)
  `(get-word ,word (function* (lambda (&key data &allow-other-keys)
				(setq data (synonym-filter data #'(lambda (w) (< 0 (assoc-default 'relevance w)))))
				(funcall ,callback :data data)))))

(defmacro get-antonyms (word callback)
  `(get-word ,word (function* (lambda (&key data &allow-other-keys)
				(setq data (synonym-filter data #'(lambda (w) (> 0 (assoc-default 'relevance w)))))
				(funcall ,callback :data data)))))

(defun synonymous-synonyms (&optional event opoint)
  (interactive)
  (synonymous-replace-word-before-point-synonyms nil event opoint))

(defun synonymous-antonyms (&optional event opoint)
  (interactive)
  (synonymous-replace-word-before-point-synonyms t event opoint))

(defun synonymous-replace-word-before-point-synonyms (&optional antonym event opoint)
  (unless (mouse-position)
    (error "Pop-up menus do not work on this terminal"))
  (or opoint (setq opoint (point)))
  (lexical-let*
      ((cursor-location (point))
       (word (thing-at-point 'word))
       (bounds (bounds-of-thing-at-point 'word))
       (start (car bounds))
       (end (cdr bounds))
       (event event)
       (opoint opoint)
       (callback (function* (lambda (&key data &allow-other-keys)
			      (let ((replace (synonymous-emacs-popup event data word)))
				(synonymous-do-replace replace data word cursor-location start end opoint))))))
    (if (not antonym)
	(get-synonyms word callback)
      (get-antonyms word callback))))


(defun synonymous-do-replace (replace poss word cursor-location start end save)
  "The popup menu callback."
  (cond ((eq replace 'ignore)
         (goto-char save)
         nil)
        (replace
         (let ((old-max (point-max))
               (new-word (if (atom replace)
                             replace
                           (car replace)))
               (cursor-location (+ (- (length word) (- end start))
                                   cursor-location)))
           (unless (equal new-word (car poss))
             (delete-region start end)
             (goto-char start)
             (insert new-word))
           ;; In the original Emacs code, this was only called in the body
           ;; of the if.  I arbitrarily kept the XEmacs behavior instead.
           (synonymous-ajust-cursor-point save cursor-location old-max)))
        (t
         (goto-char save)
         nil)))

(defun synonymous-ajust-cursor-point (save cursor-location old-max)
  (if (>= save cursor-location)
      (let ((new-pos (+ save (- (point-max) old-max))))
        (goto-char (cond
                    ((< new-pos (point-min))
                     (point-min))
                    ((> new-pos (point-max))
                     (point-max))
                    (t new-pos))))
    (goto-char save)))

(defun extract-synonym-strings (word-instance)
  (mapcar #'(lambda (syn) (assoc-default 'word syn))
	  (assoc-default 'synonyms word-instance)))

(defun synonymous-emacs-popup (event data word)
  "The Emacs popup menu."
  (unless window-system
    (error "This command requires pop-up dialogs"))
  (if (not event)
      (let* ((mouse-pos  (mouse-position))
             (mouse-pos  (if (nth 1 mouse-pos)
                             mouse-pos
                           (set-mouse-position (car mouse-pos)
                                               (/ (frame-width) 2) 2)
                           (mouse-position))))
        (setq event (list (list (car (cdr mouse-pos))
                                (1+ (cdr (cdr mouse-pos))))
                          (car mouse-pos)))))
  (cl-flet ((build-menu (word-instance)
			(let ((part-of-speech (assoc-default 'part_of_speech word-instance))
			      (cor-menu (mapcar #'(lambda (syn) (list syn syn))
					     (extract-synonym-strings word-instance))))
			  (cons (format "%s (%s)" word part-of-speech) cor-menu))))
    (car (x-popup-menu event (cons word (pcase (length data)
					  (`1 (list (build-menu (elt data 0))))
					  (_ (mapcar #'build-menu data))))))))

