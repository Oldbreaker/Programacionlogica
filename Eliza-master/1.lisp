
(ql:quickload '(:cl-ppcre))

(defpackage :eliza
  (:use :cl :cl-ppcre )
  (:export :random-elt :patterns :eliza-repl :search-string-for-pattern :process-string))

(in-package :eliza)

(defun random-elt (list)
  (nth (random (length list)) list))




(defparameter *s* (open "pr.lisp"))
(defparameter patterns (read *s*))
(close *s*)



(defun eliza-repl ()
  (let ((input (progn (print 'ELIZA>)
		      (read-line *standard-input* nil 'eof nil))))
    (cond((or (null input)
	       (string-equal input "goodbye"))
	 			(format t "BYE")
	 	  )
			 (t
			 		(cond ((string-equal input "Ensenar")
					 	(let ((pregunta (progn (print '(Introduce la pregunta))
						 	(read-line *standard-input* nil 'eof nil)
						 )))
						 	(cond ((string-equal pregunta "")
								(format t "~%Aprendizaje fallido, no has escrito la pregunta~%")
							)
							 	(t 
									(let ((respuesta (progn (print '(Introduce la respuesta))
									(read-line *standard-input* nil 'eof nil))))
										(cond ((string-equal respuesta "")
											(format t "~%Aprendizaje fallido, no has escrito la respuesta~%")
										)
											(t 
												(setf pregunta_learn nil)

												(push respuesta pregunta_learn)
												(push pregunta pregunta_learn)
												(write pregunta_learn)

												(push pregunta_learn patterns)

												(with-open-file (out "pr.lisp"
												:direction :output
												:if-exists :supersede)
												(with-standard-io-syntax
												(print patterns out)))
											)
										)
										
									)
								)
							)
							(eliza-repl)
						 )
					)
						(t 
							(format t "~&~a~&" (process-string input))
		   				(eliza-repl)
						)
					)
			 )
		 )
		 
	)
  )

(defun search-string-for-pattern (string)
  (loop for i in patterns
     when (scan (car i) string)
     return i))



(defun process-string (string)
  (let ((pattern (search-string-for-pattern string)))
    (cond (pattern
	   (regex-replace-all (car pattern)
			      string
			      (random-elt (cdr pattern))))
	  (t
	   (format t "No entiendo tu pregunta")))))


(eliza-repl)