
(defun palindromo (palabra)
(if (string-equal palabra (reverse palabra))
    (format t "~& la palabra ~a es palindromo" palabra)
    (format t "~& la palabra ~a es no es palindromo" palabra)
)
)
(format t "~& Ingresa la palabra ~%")
(palindromo (read-line))