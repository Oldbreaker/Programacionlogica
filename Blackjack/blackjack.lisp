(defun black-jack()
    (flet ((prompt (integer)
        (format t "~&~a" integer)
        (finish-output)
        (read nil 'eof nil)
    ))

    'BlackJack

    (let ((opcion (prompt "¿Desea iniciar el juego? 1 - SI   2 - NO ")))
    
        (cond ((= opcion 1) 

            (defvar jugador 0)
            (defvar computadora 0)

            (defvar carta (random 13))

            (cond ((= carta 0)
                (let ((valor(prompt "Carta As, ¿Que valor desea que tome?  1 - 1   2 - 11 ")))
                    
                    (cond ((= valor 1)
                        (setq jugador (+ jugador 1))
                    )
                        (t (setq jugador (+ jugador 11)))
                    )
                )
            )
                (t (cond ((< carta 10)
                    (setq jugador (+ jugador carta 1))
                )
                    (t (setq jugador (+ jugador 10)))
                ))
            )
            
            (format t "Inicio de juego de Jugador 1~%")

            (setq opcion 10)
            
            (format t "Puntuacion del jugador: ~d~%" jugador)
            (dotimes (i opcion)
                (let ((opciont (prompt "¿Desea otra carta? 1 - SI  2 - NO ")))
                    
                    (cond ((= opciont 1)
                        
                        (setq carta (random 13))

                        (cond ((= carta 0)
                            (let ((valor(prompt "Carta As, ¿Que valor desea que tome?  1 - 1   2 - 11 ")))
                                
                                (cond ((= valor 1)
                                    (setq jugador (+ jugador 1))
                                )
                                    (t (setq jugador (+ jugador 11)))
                                )
                            )
                        )
                            (t (cond ((< carta 10)
                                (setq jugador (+ jugador carta 1))
                            )
                                (t (setq jugador (+ jugador 10)))
                            ))
                        )

                        (cond ((> jugador 21)
                            (format t "Has perdido, tu puntuacion es ~d~%" jugador)
                            (return)
                        )
                            (t
                                (setq opcion (+ opcion 2))  
                                (format t "Tu puntuacion es: ~d~%" jugador)
                            )
                        )
                    )
                        (t  (setq opcion (- opcion 1)) 
                            (format t "Tu puntuación es: ~d~%" jugador)
                            (return))
                    )
                )
            )

            (format t "Inicio de juego de computadora~%")
            (defvar control 10)
            (dotimes (j control)
                (setq carta (random 13))

                (cond ((= carta 0)
                    (cond ((< computadora 10)
                        (setq computadora (+ computadora 11))
                    )
                        (t (setq computadora (+ computadora 1)))
                    )
                )
                    (t (cond ((< carta 10)
                        (setq computadora (+ computadora carta 1))
                    )
                        (t (setq computadora (+ computadora 10)))
                    ))
                )

                (cond ((> jugador 21)
                    (cond ((and (>= computadora 15) (< computadora 21))
                        (format t "La computadora ha ganado~%")
                        (return)
                    ))
                )
                    (t 
                        (cond ((>= computadora 21)
                            (cond ((> computadora 21)
                                
                                (cond ((> jugador 21)
                                    (format t "Ningun participante ha ganado~%~%")
                                    (return)
                                )
                                    (t (format t "El jugador ha ganado~%La computadora ha perdido.~%~%")
                                        (return)
                                    )
                                )
                            )
                                (t (cond ((= computadora jugador)
                                        (format t "Juego Empatado~%")
                                        (return)
                                    ))
                                )
                            )
                        )
                            (t (cond ((and (> computadora jugador) (< computadora 21))
                                    (format t "La computadora ha ganado~%")
                                    (return)
                                ))
                            )
                        )
                    )
                )

                

                
            )

            (format t "La puntuacion final es: ~%Computadora: ~d~%Jugador: ~d" computadora jugador)

        ))

        

    )

)
)