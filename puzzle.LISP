;;; Projeto Solitario 2 - Inteligencia Artificial 2025/2026
;;; Autores: 
;; Felisberto de Carvalho 202200359
;; Tiago Campos 202300064
;; Filipe Patricio 202300133

;;; puzzle.lisp
;;; Representacao do tabuleiro e operadores de movimento do Solitario.

;; valor no codigo: 0 -> casa vazia
;; valor no codigo: 1 -> pino do jogador 1
;; valor no codigo: 2 -> pino do jogador 2
;; valor no codigo: nil -> fora do tabuleiro



;;; Representacao do tabuleiro inicial
(defun tabuleiro-inicial ()
  "Disposicao do tabuleiro inicial"
  '((nil nil 1 1 1 nil nil)
    (nil nil 1 1 1 nil nil)
    (0 0 0 0 0 0 0)
    (0 0 0 0 0 0 0)
    (0 0 0 0 0 0 0)
    (nil nil 2 2 2 nil nil)
    (nil nil 2 2 2 nil nil))
)

(defun adversario (jogador)
  "Devolve o valor dos pinos do adversario"
  (if (= jogador 1) 2 1)
)

(defun get-nth (n lst)
  "Retorna o n-esimo elemento de uma lista (recursivamente)"
  (if (zerop n)
      (first lst)
      (get-nth (1- n) (rest lst)))
)

(defun set-nth (n new-val lst)
  "Substitui o elemento na posicao n de uma lista (recursivamente)"
  (if (zerop n)
      (cons new-val (rest lst))
      (cons (first lst) (set-nth (1- n) new-val (rest lst))))
)

(defun get-pos (linha coluna tab)
  "Acede a uma posicao (linha, coluna)"
  (if (or (< linha 1) (> linha 7) (< coluna 1) (> coluna 7))
      nil
      (let ((linha-val (get-nth (1- linha) tab)))
        (if (null linha-val)
            nil
            (get-nth (1- coluna) linha-val))))
)

(defun set-pos (linha coluna new-val tab)
  (if (or (< linha 1) (> linha 7) (< coluna 1) (> coluna 7))
      tab  ; devolve o tabuleiro inalterado
      (if (= linha 1)
          (cons (set-nth (1- coluna) new-val (first tab)) (rest tab))
          (cons (first tab) (set-pos (1- linha) coluna new-val (rest tab)))))
)


(defun pos-valida? (linha coluna tab)
  "Verifica se uma posicao (linha, coluna) e valida no tabuleiro"
  (cond
    ;; fora dos limites (menor que 1 ou maior que 7)
    ((or (< linha 1) (> linha 7) (< coluna 1) (> coluna 7))
     nil)
    ;; posicao e nil (fora da cruz)
    ((null (get-pos linha coluna tab))
     nil)
    ;; caso contrario, e valida
    (t t))
)



;;; Movimentos e Capturas

;; Movimentos

(defun d (linha coluna tab jogador)
  "O pino move-se uma casa para a direita"
  (if (d-validop linha coluna tab jogador)

      (let* ((tab1 (set-pos linha coluna 0 tab))
             (tab2 (set-pos linha (+ coluna 1) jogador tab1)))
        tab2)
    tab)
)

(defun d-validop (linha coluna tab jogador)
  "Verifica se o movimento para a Direita e valido"
  (and 
   (pos-valida? linha coluna tab)
   (pos-valida? linha (+ coluna 1) tab)

   (= (get-pos linha coluna tab) jogador)
   (= (get-pos linha (+ coluna 1) tab) 0))
)


(defun e (linha coluna tab jogador)
  "O pino move-se uma casa para a esquerda"
  (if (e-validop linha coluna tab jogador)

      (let* ((tab1 (set-pos linha coluna 0 tab))
             (tab2 (set-pos linha (- coluna 1) jogador tab1)))
        tab2)
    tab)
)

(defun e-validop (linha coluna tab jogador)
  "Verifica se o movimento para a Esquerda e valido"
  (and 
   (pos-valida? linha coluna tab)
   (pos-valida? linha (- coluna 1) tab)

   (= (get-pos linha coluna tab) jogador)
   (= (get-pos linha (- coluna 1) tab) 0))
)


(defun c (linha coluna tab jogador)
  "O pino move-se uma casa para cima"
  (if (c-validop linha coluna tab jogador)

      (let* ((tab1 (set-pos linha coluna 0 tab))
             (tab2 (set-pos (- linha 1) coluna jogador tab1)))
        tab2)
    tab)
)

(defun c-validop (linha coluna tab jogador)
  "Verifica se o movimento para Cima e valido"
  (and 
   (pos-valida? linha coluna tab)
   (pos-valida? (- linha 1) coluna tab)

   (= (get-pos linha coluna tab) jogador)
   (= (get-pos (- linha 1) coluna tab) 0))
)


(defun b (linha coluna tab jogador)
  "O pino move-se uma casa para baixo"
  (if (b-validop linha coluna tab jogador)

      (let* ((tab1 (set-pos linha coluna 0 tab))
             (tab2 (set-pos (+ linha 1) coluna jogador tab1)))
        tab2)
    tab)
)

(defun b-validop (linha coluna tab jogador)
  "Verifica se o movimento para a Baixo e valido"
  (and 
   (pos-valida? linha coluna tab)
   (pos-valida? (+ linha 1) coluna tab)

   (= (get-pos linha coluna tab) jogador)
   (= (get-pos (+ linha 1) coluna tab) 0))
)



;; Capturas

; Evitar redefinir cd
(in-package :puzzle)
(shadow 'cd)


(defun cd (linha coluna tab jogador)
  "O pino move-se duas casas para a direita, capturando outro pino"
  (if (cd-validop linha coluna tab jogador)

      ;; Entao cria novo tabuleiro recursivamente
      (let* ((tab1 (set-pos linha coluna 0 tab))               ; Origem vazia
             (tab2 (set-pos linha (+ coluna 1) 0 tab1))        ; Remove o pino do meio
             (tab3 (set-pos linha (+ coluna 2) jogador tab2))) ; Destino ganha pino
        tab3)
    ;; Caso contrario, devolve o mesmo tabuleiro (movimento invalido)
    tab)
)

(defun cd-validop (linha coluna tab jogador)
  "Verifica se o movimento Captura Direita e valido"
  (and 
   (pos-valida? linha coluna tab)
   (pos-valida? linha (+ coluna 1) tab)
   (pos-valida? linha (+ coluna 2) tab)

   (= (get-pos linha coluna tab) jogador)
   (= (get-pos linha (+ coluna 1) tab) (adversario jogador))
   (= (get-pos linha (+ coluna 2) tab) 0))
)


(defun ce (linha coluna tab jogador)
  "O pino move-se duas casas para a esquerda, capturando outro pino"
  (if (ce-validop linha coluna tab jogador)

      ;; Movimento valido devolve novo tabuleiro
      (let* ((tab1 (set-pos linha coluna 0 tab))               ; Origem vazia
             (tab2 (set-pos linha (- coluna 1) 0 tab1))        ; Remove o pino do meio
             (tab3 (set-pos linha (- coluna 2) jogador tab2))) ; Destino ganha pino
        tab3)
    ;; Movimento invalido devolve o mesmo tabuleiro
    tab)
)

(defun ce-validop (linha coluna tab jogador)
  "Verifica se o movimento Captura Esquerda e valido"
  (and 
   (pos-valida? linha coluna tab)
   (pos-valida? linha (- coluna 1) tab)
   (pos-valida? linha (- coluna 2) tab)

   (= (get-pos linha coluna tab) jogador)
   (= (get-pos linha (- coluna 1) tab) (adversario jogador))
   (= (get-pos linha (- coluna 2) tab) 0))
)


(defun cc (linha coluna tab jogador)
  "O pino move-se duas casas para cima, capturando outro pino"
  (if (cc-validop linha coluna tab jogador)

      ;; Aplica as mudancas recursivamente
      (let* ((tab1 (set-pos linha coluna 0 tab))               ; Origem vazia
             (tab2 (set-pos (- linha 1) coluna 0 tab1))        ; Remove o pino do meio
             (tab3 (set-pos (- linha 2) coluna jogador tab2))) ; Destino ganha o pino
        tab3)
    ;; Movimento invalido
    tab)
)

(defun cc-validop (linha coluna tab jogador)
  "Verifica se o movimento Captura Cima e valido"
  (and 
   (pos-valida? linha coluna tab)
   (pos-valida? (- linha 1) coluna tab)
   (pos-valida? (- linha 2) coluna tab)

   (= (get-pos linha coluna tab) jogador)
   (= (get-pos (- linha 1) coluna tab) (adversario jogador))
   (= (get-pos (- linha 2) coluna tab) 0))
)


(defun cb (linha coluna tab jogador)
  "O pino move-se duas casas para baixo, capturando de outro pino"
  (if (cb-validop linha coluna tab jogador)

      ;; Aplica as mudanças recursivamente
      (let* ((tab1 (set-pos linha coluna 0 tab))               ; Origem vazia
             (tab2 (set-pos (+ linha 1) coluna 0 tab1))        ; Remove o pino do meio
             (tab3 (set-pos (+ linha 2) coluna jogador tab2))) ; Destino ganha pino
        tab3)
    ;; Movimento invalido devolve o mesmo tabuleiro
    tab)
)

(defun cb-validop (linha coluna tab jogador)
  "Verifica se o movimento Captura Baixo e valido"
  (and 
   ;; Posicoes validas
   (pos-valida? linha coluna tab)
   (pos-valida? (+ linha 1) coluna tab)
   (pos-valida? (+ linha 2) coluna tab)

   ;; Condicoes do movimento
   (= (get-pos linha coluna tab) jogador)                    ; Origem com pino
   (= (get-pos (+ linha 1) coluna tab) (adversario jogador)) ; Pino a ser comido
   (= (get-pos (+ linha 2) coluna tab) 0))                   ; Destino vazio
)

(defun movimentos-validos-de-pos (linha coluna tab jogador)
  "Verifica quais os movimentos disponiveis de uma dada posicao num tabuleiro"
  (append
   (if (d-validop  linha coluna tab jogador) (list (list 'd  linha coluna)) '())
   (if (e-validop  linha coluna tab jogador) (list (list 'e  linha coluna)) '())
   (if (c-validop  linha coluna tab jogador) (list (list 'c  linha coluna)) '())
   (if (b-validop  linha coluna tab jogador) (list (list 'b  linha coluna)) '())
   (if (cd-validop linha coluna tab jogador) (list (list 'cd linha coluna)) '())
   (if (ce-validop linha coluna tab jogador) (list (list 'ce linha coluna)) '())
   (if (cc-validop linha coluna tab jogador) (list (list 'cc linha coluna)) '())
   (if (cb-validop linha coluna tab jogador) (list (list 'cb linha coluna)) '())
   )
)


(defun aplica-movimento (mov tab jogador)
  "Aplica mov (por exemplo '(cd 4 2)) ao tabuleiro e devolve o novo tabuleiro se o mov for desconhecido devolve NIL"
  (let ((tipo (first mov))
        (linha (second mov))
        (coluna (third mov)))
    (cond
     ((eq tipo 'd)  (d  linha coluna tab jogador))
     ((eq tipo 'e)  (e  linha coluna tab jogador))
     ((eq tipo 'c)  (c  linha coluna tab jogador))
     ((eq tipo 'b)  (b  linha coluna tab jogador))
     ((eq tipo 'cd) (cd linha coluna tab jogador))
     ((eq tipo 'ce) (ce linha coluna tab jogador))
     ((eq tipo 'cc) (cc linha coluna tab jogador))
     ((eq tipo 'cb) (cb linha coluna tab jogador))
     (t (error "Tipo de movimento desconhecido: ~A" tipo))))
)


(defun gera-sucessores (tab jogador)
  "Devolve lista de pares (movimento . novo-tabuleiro) com todos os movimentos válidos do tabuleiro"
  (labels
      ;; Gera para colunas de 1..7 recursivamente
      ((iter-cols (linha coluna acc)

         (cond ((> coluna 7) acc)
               (t
                (let* ((movs (movimentos-validos-de-pos linha coluna tab jogador))
                       (pares (mapcar (lambda (m) 
                                        (cons m (aplica-movimento m tab jogador))) 
                                      movs)))
                  (iter-cols linha (1+ coluna) (append acc pares)))))))

    ;; Gera para linhas de 1..7 recursivamente, reutilizando iter-cols
    (labels ((iter-linhas (linha acc)
               (cond ((> linha 7) acc)
                     (t 
                      (iter-linhas (1+ linha) (iter-cols linha 1 acc))))))
      (iter-linhas 1 '())))
)


(defun print-linha (linha)
  "Imprime uma linha do tabuleiro, convertendo 1 , 0 , nil espaço"
  (cond
    ((null linha) (format t "~%")) ; fim da linha
    (t
     (let ((celula (first linha)))
       (cond
         ((null celula) (format t "   "))   ; fora da cruz
         ((= celula 1) (format t " X "))   ; pino
         ((= celula 0) (format t " O "))))
     (print-linha (rest linha)))) ; recursão para o resto da linha
)
     

(defun print-tabuleiro (tab)
  "Imprime o tabuleiro completo de forma visual"
  (cond
    ((null tab) (format t "~%")) ; fim das linhas
    (t
     (print-linha (first tab))     ; imprime primeira linha
     (print-tabuleiro (rest tab)))) ; recursão para as restantes
)


(defun conta-pinos-linha (linha)
  "Conta os pinos de uma dada linha"
  (cond
    ((null linha) 0)
    ((null (first linha)) (conta-pinos-linha (rest linha)))
    ((= (first linha) 1) (+ 1 (conta-pinos-linha (rest linha))))
    (t (conta-pinos-linha (rest linha))))
)


(defun conta-pinos (tab)
  "Conta os pinos de um dado tabuleiro"
  (cond
    ((null tab) 0)
    (t (+ (conta-pinos-linha (first tab))
          (conta-pinos (rest tab)))))
)


(defun objetivo? (tab)
  "Verifica se um dado tabuleiro tem apenas 1 pino, i.e. chegou ao objetivo"
  (= (conta-pinos tab) 1)
)

(defun o (tab jogador)
  "Calcula os sucessores de um dado tabuleiro"
  (length (gera-sucessores tab jogador))
)

(defun estados-iguais? (a b)
  "Verifica se os estados a e b sao iguais"
  (equal a b)
)

(defun estado-visitado? (estado visitados)
  ""
  (cond
    ((null visitados) nil)
    ((estados-iguais? estado (first visitados)) t)
    (t (estado-visitado? estado (rest visitados))))
)
