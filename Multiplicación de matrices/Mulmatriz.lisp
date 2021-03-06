(defun mult-Matriz()
    (flet ((prompt (integer)
        (format t "~&~a" integer)
        (finish-output)
        (read nil 'eof nil)
    ))

        (let ((fila (prompt "Ingresa las filas para la matriz 1 ")))
            (let ((columnasm1 (prompt "Ingresa las columnas para la matriz 1 ")))
                (let ((columnasm2 (prompt "Ingresa las columnas para la matriz 2 ")))
                    (setq matriz_1 (make-array `(,fila ,columnasm1)))
                    (setq matriz_2 (make-array `(,columnasm1 ,columnasm2)))

                    (dotimes (i fila)
                        (dotimes (j columnasm1)
                            (let ((numero (prompt "Ingresa un numero para llenar la matriz 1 ")))
                                (setf (aref matriz_1 i j) numero)
                            )
                        )
                    )

                    (dotimes (i columnasm1)
                        (dotimes (j columnasm2)
                            (let ((numero (prompt "Ingresa un numero para llenar la matriz 2 ")))
                                (setf (aref matriz_2 i j) numero)
                            )
                        )
                    )

                    (setq matriz_r (make-array `(,fila ,columnasm2)))
                    (setq result 0)
                    (dotimes (i fila)
                        (dotimes (j columnasm2)  
                            (dotimes (k columnasm1)
                                (setq numero (aref matriz_1 i k))
                                (setq numero2 (aref matriz_2 k j))

                                (setq result (+ result (* numero numero2)))
                                ;;(write result)
                                (setf (aref matriz_r i j) result)
                            )
                            (setq result 0)
                        )
                    )
                    
                    (write matriz_1)
                    (format t "~%")
                    (write matriz_2)
                    (format t "~%")
                    (write matriz_r)
                    (format t "~%")
                )
            )
        )

    )
)