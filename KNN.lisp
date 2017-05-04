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

( setq fila (make-node 
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
      :prox nil
  )

(defun addLista (fila mod maxs maxm cor clo ramm dis pix fat qua)
  ( setq book1 (make-node 
      :modelo  mod
      :maxsim  maxs
      :maxmem maxm
      :core cor
      :clock clo
      :ram ramm
      :display dis
      :pixel pix
      :fator fat
      :qual qua
      :prox fila
  )
)

(defparameter *s* (open "lispknn/database.txt"))
(when *s*
    (loop for line = (read-line *s* nil)
         while line do (format t "~a~%" line))
         
    (close *s*)))
