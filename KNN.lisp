(setq cont 0)
(defparameter in (open "lispknn/database.txt"))
(when in
    (loop for line = (read-line in nil)
    while line do (format t "~d ~a~%" cont line )
      (incf cont)
    )
   (close in)
   )
)
(while line do (format t "~d ~a~%" cont line ))

(defstruct node 
   modelo 
   maxsim 
   maxmem
   core
   clock
   ram
   display
   pixel
   fator
   qual
   prox
)

(defparameter *s* (open "lispknn/database.txt"))

(defun addLista (fil)
  ( setq book 
    (make-node 
      :modelo (read *s*)
      :maxsim (read *s*)
      :maxmem (read *s*)
      :core (read *s*)
      :clock (read *s*)
      :ram (read *s*)
      :display (read *s*)
      :pixel (read *s*)
      :fator (read *s*)
      :qual (read *s*)
      :prox fil
    )
  )
)

( setq fila (addLista (nil)))
( setq fila (addLista (fila)))
   

(when *s*
    (loop for line = (read-line *s* nil)
         while line do (format t "~a~%" line))
         
    (close *s*)))
