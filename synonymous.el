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

(defmacro get-synonyms (word callback)
  `(get-word ,word (function* (lambda (&key data &allow-other-keys)
			      (let* ((synonyms (assoc-default 'synonyms data))
				     (syn-list (cl-loop for syn across synonyms
							collect (assoc-default 'word syn))))
				(funcall ,callback :data syn-list))))))

(defun synonymous-correct-word-before-point (&optional event opoint)
  (interactive)
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
       (opoint opoint))
    (get-synonyms word (function* (lambda (&key data &allow-other-keys)
				    (let ((replace (synonymous-emacs-popup event data word)))
				      (synonymous-do-correct replace data word cursor-location start end opoint)))))))


(defun synonymous-do-correct (replace poss word cursor-location start end save)
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

(defun synonymous-emacs-popup (event poss word)
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
  (let* ((cor-menu (mapcar (lambda (syn)
			     (list syn syn))
			   poss))
	 (menu       (cons "synonymous" cor-menu)))
    (car (x-popup-menu event (list word menu)))))
