(defun make-cd (title artist rating ripped)
(list :title title :artist artist :rating rating :ripped ripped))
(make-cd "Roses" "Kathy Mattea" 7 t)
(defvar *db* nil)
(defun add-record (cd) (push cd *db*))
(add-record (make-cd "Roses" "Kathy Mattea" 7 t))
(add-record (make-cd "Fly" "Dixie Chicks" 8 t))
(add-record (make-cd "Home" "Dixie Chicks" 9 t))
(defun dump-db()
(dolist (cd *db*)
(format t "~{~a:~10t~a~%~}~%" cd)))
(format t "~a" "Dixie Chicks") 
(format t "~a" :title)
(format t "~a:~10t~a" :artist "Dixie Chicks") 
(defun dump-db ()  (format t "~{~{~a:~10t~a~%~}~%~}" *db*))
(defun prompt-read (prompt)  (format *query-io* "~a: " prompt)  
(force-output *query-io*) 
(read-line *query-io*))
(defun prompt-for-cd ()  (make-cd
   (prompt-read "Title")   
   (prompt-read "Artist")   
   (prompt-read "Rating")   
   (prompt-read "Ripped [y/n]")))
(defun prompt-for-cd ()  
(make-cd   (prompt-read "Title")   
(prompt-read "Artist")   
(or (parse-integer (prompt-read "Rating") :junk-allowed t) 0)   
(y-or-n-p "Ripped [y/n]: ")))
(defun add-cds ()  
(loop (add-record (prompt-for-cd))      
(if (not (y-or-n-p "Another? [y/n]: ")) (return))))
(defun save-db (filename)  
(with-open-file (out filename                   
:direction :output                   
:if-exists :supersede)    
(with-standard-io-syntax      (print *db* out))))
(defun load-db (filename)  (with-open-file (in filename)    
(with-standard-io-syntax      (setf *db* (read in)))))
(defun update (selector-fn &key title artist rating (ripped nil ripped-p))  
(setf *db*        
(mapcar         
#'(lambda (row)             
(when (funcall selector-fn row)               
(if title    (setf (getf row :title) title))               
(if artist   (setf (getf row :artist) artist))               
(if rating   (setf (getf row :rating) rating))               
(if ripped-p (setf (getf row :ripped) ripped)))             
row) *db*)))
(select 
#'(lambda (cd)     
(and (equal (getf cd :title) "Give Us a Break")          
(equal (getf cd :ripped) t))))
(defun make-comparison-expr (field value)  
(list 'equal (list 'getf 'cd field) value))
(defun make-comparisons-list (fields)  
(loop while fields     
collecting (make-comparison-expr (pop fields) (pop fields))))
(defmacro where (&rest clauses)  
`#'(lambda (cd) (and ,@(make-comparisons-list clauses))))
 (macroexpand-1 '(where :title "Give Us a Break" :ripped t)) 
 #'(LAMBDA (CD)    (AND (EQUAL (GETF CD :TITLE) "Give Us a Break")         
 (EQUAL (GETF CD :RIPPED) T))) 
 (select (where :title "Give Us a Break" :ripped t)) 
 ((:TITLE "Give Us a Break" :ARTIST "Limpopo" :RATING 10 :RIPPED T))







