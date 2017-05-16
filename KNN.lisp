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

;obtendo parametros de comparação
(defun GetUser (File)
     (defparameter in (open File))
     ( setq cell 
          (make-phone 
               :maxsim (read in)
               :maxmem (read in)
               :core (read in)
               :clock (read in)
               :ram (read in)
               :display (read in)
               :pixel (read in)
			   :ratio (read in)
          )
     )
     (close in)
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

;Inprime a quantidade (k) de itens da lista (Fila)
(defun PrintList (Lista K)
     (setq top 0)
	 (setq med 0)
	 (setq bad 0)
	 (loop for x from 0 to (- K 1) do
	      (setq cell (nth x Lista))
          (write (phone-modelo cell))
		  (case (phone-qual cell)
		       (0 (incf bad))
			   (1 (incf med))
			   (2 (incf top))
		   )
		   (terpri)
	)
	 (format t "Classe: ")
	 (if (> top med)
          	 (if (> top bad)
			      (format t "Top!")
			(format t "Bad!"))
	 (if (> med bad)
			(format t "Med!")
	 (format t "Bad!")))
)

;main
(setq File "database.csv")                              ;definindo banco de dados
(setq Banco (GetDatabase File))                         ;obtendo todos os dados do banco
(setq File "user.txt")                                  ;definindo base de comparação
(setq user (GetUser File))                              ;obtendo todos os dados de comparação
(setq Banco (CalcRatio Banco user))                     ;calculando ratio
(ListSort Banco)                                        ;ordenando banco segundo o ratio
(PrintList Banco (phone-ratio user))                    ;imprimindo resultado
