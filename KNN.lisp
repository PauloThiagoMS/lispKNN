;Estrutura de um Aparelho
(defstruct phone 
     modelo 
     maxsim 
     maxmem
     core
     clock
     ram
     display
     pixel
     ratio
     qual
)


;Retorna a Quantidade de linhas que o arquivo (File) possui
(defun LineLen (File) 
     (setq cont 0)
     (defparameter in (open File))
     (when in
          (loop for line = (read-line in nil)
               while line do 
                    (incf cont)
         )
     (close in)
     cont
   )
)


;Adiciona o elemento (Phone) na ultima posição da lista (List) e a retorna
(defun AddList (List Phone)
     (setq List(append List(list Phone)))
     List
)
  
  

;Intancia o Banco de Dados (File)
(defun GetDatabase (File)
     (setq cont (LineLen File))
     (setq List ())
     (defparameter in (open File))
     (loop for x from 1 to cont
           do 
          ( setq cell 
               (make-phone 
                    :modelo (read in)
                    :maxsim (read in)
                    :maxmem (read in)
                    :core (read in)
                    :clock (read in)
                    :ram (read in)
                    :display (read in)
                    :pixel (read in)
                    :qual (read in) 
                )
          )
          (setq List (AddList List cell))
     )
     (close in)
     List
)



;Retorna uma estrutura Phone com as caracteristicas informadas pelo usuario
(defun Menu ()
     (format t "          ##      ##  ######  ######~%")
     (format t "         ##      ##  ######  ##  ##~%")
     (format t "        ##          ##      ##  ##~%")
     (format t "       ##      ##  ##      ##  ##~%")
     (format t "      ##      ##  ######  ######~%")
     (format t "     ##      ##  ######  ######~%")
     (format t "    ##      ##      ##  ##~%")
     (format t "   ##      ##      ##  ##~%")
     (format t "  ##      ##      ##  ##~%")
     (format t " ######  ##  ######  ##~%")
     (format t "######  ##  ######  ## CELLPHONE KNN~%")
     (terpri)
     (format t "Qual a capacidade maxima de ChipsSim do aparelho?~%")
     (format t "MaxSim:")
     (setq maxsim(read))
     (format t "~%Qual a capacidade maxima de Armazenamento interno do aparelho em Gbs?~%")
     (format t "MaxMem:")
     (setq maxmem(read))
     (format t "~%Quantos nucleos o processador do aparelho possui?~%")
     (format t "Core:")
     (setq core(read))
     (format t "~%Em qual frequencia processador do aparelho opera?~%")
     (format t "Clock:")
     (setq clock(read))
     (format t "~%Qual a quantidade de Memorima RAM o aparelho possui em Gbs?~%")
     (format t "Ram:")
     (setq ram(read))
     (format t "~%Quantas polegadas possui o Display do aparelho?~%")
     (format t "Display:")
     (setq display(read))
     (format t "~%Quantos Megapixels possui a camera do aparelho?~%")
     (format t "Pixel:")
     (setq pixel(read))
	 (format t "~%Qual o valor de K? (Range de Pesquisa)~%")
     (format t "Range:")
     (setq ratio(read))
     ( setq cell 
          (make-phone 
               :modelo "User_Definition"
               :maxsim maxsim
               :maxmem maxmem
               :core core
               :clock clock
               :ram ram
               :display display
               :pixel pixel
			   :ratio ratio
           )
     )
     (terpri)
     cell
)

;Calcula o ratio no banco (Fila) segundo o aparelho (cell)
(defun CalcRatio (Fila cell)
     (setq cont (list-length Fila))
     (setq cont (- cont 1))
     (loop for x from 0 to cont do
          (setq GetCell (nth x Fila))
          (setq soma (expt (- (phone-maxsim cell) (phone-maxsim GetCell))2))
          (setq soma (+ soma (expt (- (phone-maxmem cell) (phone-maxmem GetCell))2)))
          (setq soma (+ soma (expt (- (phone-core cell) (phone-core GetCell))2)))
          (setq soma (+ soma (expt (- (phone-clock cell) (phone-clock GetCell))2)))
          (setq soma (+ soma (expt (- (phone-ram cell) (phone-ram GetCell))2)))
          (setq soma (+ soma (expt (- (phone-display cell) (phone-display GetCell))2)))
          (setq soma (+ soma (expt (- (phone-pixel cell) (phone-pixel GetCell))2)))
          (setf (phone-ratio GetCell) (sqrt soma))
     )
     Fila
)

; Função para Trocar
(defun Troca (Lista i j)
     (setq tmp (nth i Lista))
     (setf (nth i Lista) (nth j Lista))
     (setf (nth j Lista) tmp)
)
; Sort em LISP
(defun ListSort (Lista)
     (do ((i (list-length Lista) (- i 1))) ((= i 0))
          (do ((j 0 (+ j 1))) ((= j (- i 1)))
               (if (> (phone-ratio (nth j Lista)) (phone-ratio (nth (+ j 1) Lista)))
                    (Troca Lista j (+ j 1))
               )
          )
     )
)
     
;main
(setq File "database.csv")
(setq Fila (GetDatabase File))
(setq user (menu))
(setq Fila (CalcRatio Fila user))
(write Fila)
(ListSort Fila)
(setq x (read))
(write Fila)
(setq x (read))
(setq user (menu))
(setq Fila (CalcRatio Fila user))
(write Fila)
(ListSort Fila)
(setq x (read))
(write Fila)
(setq x (read))
