;Estrutura de um Aparelho

(defstruct phone 
     modelo											;Armazena fabricante e modelo
     maxsim											;Quantidade maxima de Chip-Sim (Operadora)
     maxmem											;Quantidade maxima de armazenamento interno(Gbs)
     core											;Quantidade de nucleos do processador 
     clock											;Frequencia do processador (GHz)
     ram											;Quantidade de memoria RAM (Gbs)
     display										;Dimensao diagonal do display (Plg)
     pixel											;Quantidade de pixel da camera (Mpx)
     ratio											;Distacia entre pontos
     qual											;Qualificacao (2 top,1 med,0 bad)
)
;Retorna a Quantidade de linhas que o arquivo (File) possui

(defun LineLen (File)								;Obtendo parametros da funcao
     (setq cont 0)									;variavel contadora de linhas
     (defparameter in (open File))					;Abrindo arquivo alvo
     (when in										;Verificando sucesso na abertura do arquivo
         (loop for line = (read-line in nil)		;Inicio do laco de repeticao
               while line do 						;Verificando se a linha existe
                    (incf cont)						;Incremento do contador
         )											;Fim do laco de repeticao
     (close in)										;Fechando arquivo alvo
     cont											;Retornando o valor da variavel contadora
   )												;Fim do condicional
)													;Fim da funcao
;Adiciona o elemento (Phone) na ultima posição da lista (List) e a retorna

(defun AddList (List Phone)							;Obtendo parametros da funcao
     (setq List(append List(list Phone)))			;Adiciona o elemento Phone ao fim da lista
     List											;Retorna a lista atualizada
)													;Fim da funcao
;Intancia o Banco de Dados (File)

(defun GetDatabase (File)							;Obtendo parametros da funcao
     (setq cont (LineLen File))						;Obtendo numeros de linhas do arquivo
     (setq List ())									;Criando lista vazia de elementos
     (defparameter in (open File))					;Abrindo arquivo alvo
     (loop for x from 1 to cont do					;Inicio do laco de repeticao
          ( setq cell 								;Criando variavel auxiliar	
               (make-phone 							;Criando estrutura do tipo phone
                    :modelo (read in)				;Obtendo Modelo
                    :maxsim (read in)				;Obtendo MaxSim
                    :maxmem (read in)				;Obtendo MaxMem
                    :core (read in)					;Obtendo Core
                    :clock (read in)				;Obtendo Clock
                    :ram (read in)					;Obtendo RAM
                    :display (read in)				;Obtendo Display
                    :pixel (read in)				;Obtendo Pixel
                    :qual (read in) 				;Obtendo Qualificacao
                )									;Fim da criacao do phone
          )											;Fim da criacao da variavel auxiliar
          (setq List (AddList List cell))			;Adicionando novo cell ao final da lista
     )												;Fim do laco de repeticao
     (close in)										;Fechando arquivo alvo
     List											;Retorna a lista atualizada
)													;Fim da funcao
;obtendo parametros de comparação

(defun GetUser (File)								;Obtendo parametros da funcao
     (defparameter in (open File))					;Abrindo arquivo alvo
     ( setq cell 									;Criando variavel auxiliar	
          (make-phone 								;Criando estrutura do tipo phone
               :maxsim (read in)					;Obtendo MaxSim
               :maxmem (read in)					;Obtendo MaxMem
               :core (read in)						;Obtendo Core
               :clock (read in)						;Obtendo Clock
               :ram (read in)						;Obtendo RAM
               :display (read in)					;Obtendo Display
               :pixel (read in)						;Obtendo Pixel
			   :ratio (read in)						;Obtendo quantidade de linhas
          )											;Fim da criacao do phone
     )												;Fim da criacao da variavel auxiliar
     (close in)										;Fechando arquivo alvo
     cell											;Retornando estrutura phone criada
)													;Fim da funcao
;Calcula o ratio no banco (Fila) segundo o aparelho (cell)

(defun CalcRatio (Fila cell)						;Obtendo parametros da funcao
     (setq cont (list-length Fila))					;Obtendo numeros de elementos da lista
     (setq cont (- cont 1))							;Decremento devido ao endereco inicial (0)
     (loop for x from 0 to cont do					;Inicio do laco de repeticao
          (setq GetCell (nth x Fila))														;Obtendo phone na posicao X
          (setq soma (expt (- (phone-maxsim cell) (phone-maxsim GetCell))2))				;Adicionando diferenca quadrada entre os maxsim
          (setq soma (+ soma (expt (- (phone-maxmem cell) (phone-maxmem GetCell))2)))		;Adicionando diferenca quadrada entre os maxmem
          (setq soma (+ soma (expt (- (phone-core cell) (phone-core GetCell))2)))			;Adicionando diferenca quadrada entre os core
          (setq soma (+ soma (expt (- (phone-clock cell) (phone-clock GetCell))2)))			;Adicionando diferenca quadrada entre os clock
          (setq soma (+ soma (expt (- (phone-ram cell) (phone-ram GetCell))2)))				;Adicionando diferenca quadrada entre as ram
          (setq soma (+ soma (expt (- (phone-display cell) (phone-display GetCell))2)))		;Adicionando diferenca quadrada entre os display
          (setq soma (+ soma (expt (- (phone-pixel cell) (phone-pixel GetCell))2)))			;Adicionando diferenca quadrada entre os pixel
          (setf (phone-ratio GetCell) (sqrt soma))											;Atualizando valor do ratio no phone da posicao X
     )												;Fim do laco de repeticao
     List											;Retorna a lista atualizada
)													;Fim da funcao
; Função para Trocar

(defun Troca (Lista i j)							;Obtendo parametros da funcao
     (setq tmp (nth i Lista))						;Copiando phone na posicao i para variavel auxiliar
     (setf (nth i Lista) (nth j Lista))				;Copiando phone na posicao j para posicao i
     (setf (nth j Lista) tmp)						;Copiando phone na variavel auxiliar para j
)													;Fim da funcao
; Sort em LISP

(defun ListSort (Lista)								;Obtendo parametros da funcao
     (do ((i (list-length Lista) (- i 1))) ((= i 0))										;Laco de repeticao segundo numero de phones da lista
          (do ((j 0 (+ j 1))) ((= j (- i 1)))												;Laco de repeticao da posicao atual ate o final da lista
               (if (> (phone-ratio (nth j Lista)) (phone-ratio (nth (+ j 1) Lista)))		;Condicional para troca de posicoes
                    (Troca Lista j (+ j 1))													;Troca de posicoes
               )																			;Fim do condicional
          )											;Fim do laco de repeticao
     )												;Fim do laco de repeticao
)													;Fim da funcao
;Inprime a quantidade (k) de itens da lista (Fila)

(defun PrintList (Lista K)							;Obtendo parametros da funcao
     (setq top 0)									;Variavel contadora de phones Tops
	 (setq med 0)									;Variavel contadora de phones Meds
	 (setq bad 0)									;Variavel contadora de phones Bads
	 (loop for x from 0 to (- K 1) do				;Inicio do laco de repeticao
	      (setq cell (nth x Lista))					;Obtendo phone na posicao X
          (write (phone-modelo cell))				;Imprimindo modelo do cell
		  (format t " ratio: ~d"(phone-ratio cell))	;Imrimindo ratio do cell
		  (case (phone-qual cell)					;Condicional segundo o valor de qual
		       (0 (incf bad))						;Incrementa bad caso qual = 0
			   (1 (incf med))						;Incrementa med caso qual = 1
			   (2 (incf top))						;Incrementa top caso qual = 2
		   )										;Fim do condicional
		   (terpri)									;Proxima linha
	)												;Fim do laco de repeticao
	 (if (> top med)								;Condicional (0) caso Top > Med
          	 (if (> top bad)						;Condicional (1) caso Top > Bad
			      (format t "2")					;Imprimindo 2
			(format t "0"))							;Contrario condicional (1) Imprime 0
	 (if (> med bad)								;Condicional (2) caso Med > Bad
			(format t "1")							;Imprimindo 2
	 (format t "0")))								;Contrario condicional (2) Imprime 0
)													;Fim da funcao)
;main

(setq File "database.csv")                          ;definindo banco de dados
(setq Banco (GetDatabase File))                     ;obtendo todos os dados do banco
(setq File "user.txt")                              ;definindo base de comparação
(setq user (GetUser File))                          ;obtendo todos os dados de comparação
(setq Banco (CalcRatio Banco user))                 ;calculando ratio
(ListSort Banco)                                    ;ordenando banco segundo o ratio
(PrintList Banco (phone-ratio user))                ;imprimindo resultado
