(princ "Dame tres numeros: ")
(setq a (read))
(setq b (read))
(setq c (read))
(if (and (= a b) (= a c) (= b c))
(format t "Equilatero"))
(if (and (/= a b) (/= a c) (/= b c))
(format t "Escaleno")
(format t ""))
(format t "Isoceles")


