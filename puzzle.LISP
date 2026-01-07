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
  '((nil nil 1 1 1 nil nil)
    (nil nil 1 1 1 nil nil)
    (0 0 0 0 0 0 0)
    (0 0 0 0 0 0 0)
    (0 0 0 0 0 0 0)
    (nil nil 2 2 2 nil nil)
    (nil nil 2 2 2 nil nil))
)

;;; Retorna o n-esimo elemento de uma lista (recursivamente)
(defun get-nth (n lst)
  (if (zerop n)
      (first lst)
      (get-nth (1- n) (rest lst)))
)

;;; Substitui o elemento na posicao n de uma lista (recursivamente)
(defun set-nth (n new-val lst)
  (if (zerop n)
      (cons new-val (rest lst))
      (cons (first lst) (set-nth (1- n) new-val (rest lst))))
)

;;; Acede a uma posicao (linha, coluna)
(defun get-pos (linha coluna tab)
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



;;; Movimentos

(defun cd (linha coluna tab)
  "O pino move-se duas casas para a direita, capturando outro pino"
  (if (and
        ;; todas as posicoes envolvidas sao validas
        (pos-valida? linha coluna tab)
        (pos-valida? linha (+ coluna 1) tab)
        (pos-valida? linha (+ coluna 2) tab)
        ;; condicoes do movimento
        (= (get-pos linha coluna tab) 1)     ; origem tem pino
        (= (get-pos linha (+ coluna 1) tab) 1) ; pino a ser comido
        (= (get-pos linha (+ coluna 2) tab) 0)) ; destino vazio
      ;; entao cria novo tabuleiro recursivamente
      (let* ((tab1 (set-pos linha coluna 0 tab))             ; origem vazia
             (tab2 (set-pos linha (+ coluna 1) 0 tab1))      ; remove o pino do meio
             (tab3 (set-pos linha (+ coluna 2) 1 tab2)))     ; destino ganha pino
        tab3)
      ;; caso contrario, devolve o mesmo tabuleiro (movimento invalido)
      tab)
)


(defun ce (linha coluna tab)
  "O pino move-se duas casas para a esquerda, capturando outro pino"
  (if (and
        ;; todas as posicoes envolvidas sao validas
        (pos-valida? linha coluna tab)
        (pos-valida? linha (- coluna 1) tab)
        (pos-valida? linha (- coluna 2) tab)
        ;; condioes do movimento
        (= (get-pos linha coluna tab) 1)       ; origem tem pino
        (= (get-pos linha (- coluna 1) tab) 1)  ; pino a ser comido
        (= (get-pos linha (- coluna 2) tab) 0)) ; destino vazio
      ;; movimento valido devolve novo tabuleiro
      (let* ((tab1 (set-pos linha coluna 0 tab))             ; origem vazia
             (tab2 (set-pos linha (- coluna 1) 0 tab1))      ; remove o pino do meio
             (tab3 (set-pos linha (- coluna 2) 1 tab2)))     ; destino ganha pino
        tab3)
      ;; movimento invalido  devolve o mesmo tabuleiro
      tab)
)


(defun cc (linha coluna tab)
  "O pino move-se duas casas para cima, capturando outro pino"
  (if (and
        ;; posicoes validas
        (pos-valida? linha coluna tab)
        (pos-valida? (- linha 1) coluna tab)
        (pos-valida? (- linha 2) coluna tab)
        ;; condicoes do movimento
        (= (get-pos linha coluna tab) 1)       ; origem com pino
        (= (get-pos (- linha 1) coluna tab) 1) ; pino a ser comido
        (= (get-pos (- linha 2) coluna tab) 0)) ; destino vazio
      ;; aplica as mudancas recursivamente
      (let* ((tab1 (set-pos linha coluna 0 tab))            ; origem vazia
             (tab2 (set-pos (- linha 1) coluna 0 tab1))     ; remove o pino do meio
             (tab3 (set-pos (- linha 2) coluna 1 tab2)))    ; destino ganha o pino
        tab3)
      ;; movimento invalido
      tab)
)


(defun cb (linha coluna tab)
  "O pino move-se duas casas para baixo, capturando de outro pino"
  (if (and
        ;; posições válidas
        (pos-valida? linha coluna tab)
        (pos-valida? (+ linha 1) coluna tab)
        (pos-valida? (+ linha 2) coluna tab)
        ;; condições do movimento
        (= (get-pos linha coluna tab) 1)        ; origem com pino
        (= (get-pos (+ linha 1) coluna tab) 1)  ; pino a ser comido
        (= (get-pos (+ linha 2) coluna tab) 0)) ; destino vazio
      ;; aplica as mudanças recursivamente
      (let* ((tab1 (set-pos linha coluna 0 tab))             ; origem vazia
             (tab2 (set-pos (+ linha 1) coluna 0 tab1))      ; remove o pino do meio
             (tab3 (set-pos (+ linha 2) coluna 1 tab2)))     ; destino ganha pino
        tab3)
      ;; movimento inválido  devolve o mesmo tabuleiro
      tab)
)



(defun movimentos-validos-de-pos (linha coluna tab)
  "Verifica quais os movimentos disponiveis de uma dada posicao num tabuleiro"
  (let ((cd-valid (and (pos-valida? linha coluna tab)
                       (pos-valida? linha (+ coluna 1) tab)
                       (pos-valida? linha (+ coluna 2) tab)
                       (= (get-pos linha coluna tab) 1)
                       (= (get-pos linha (+ coluna 1) tab) 1)
                       (= (get-pos linha (+ coluna 2) tab) 0)))
        (ce-valid (and (pos-valida? linha coluna tab)
                       (pos-valida? linha (- coluna 1) tab)
                       (pos-valida? linha (- coluna 2) tab)
                       (= (get-pos linha coluna tab) 1)
                       (= (get-pos linha (- coluna 1) tab) 1)
                       (= (get-pos linha (- coluna 2) tab) 0)))
        (cc-valid (and (pos-valida? linha coluna tab)
                       (pos-valida? (- linha 1) coluna tab)
                       (pos-valida? (- linha 2) coluna tab)
                       (= (get-pos linha coluna tab) 1)
                       (= (get-pos (- linha 1) coluna tab) 1)
                       (= (get-pos (- linha 2) coluna tab) 0)))
        (cb-valid (and (pos-valida? linha coluna tab)
                       (pos-valida? (+ linha 1) coluna tab)
                       (pos-valida? (+ linha 2) coluna tab)
                       (= (get-pos linha coluna tab) 1)
                       (= (get-pos (+ linha 1) coluna tab) 1)
                       (= (get-pos (+ linha 2) coluna tab) 0))))
    (append
      (if cd-valid (list (list 'cd linha coluna)) '())
      (if ce-valid (list (list 'ce linha coluna)) '())
      (if cc-valid (list (list 'cc linha coluna)) '())
      (if cb-valid (list (list 'cb linha coluna)) '())))
)


(defun aplica-movimento (mov tab)
  "Aplica mov (por exemplo '(cd 4 2)) ao tabuleiro e devolve o novo tabuleiro se o mov for desconhecido devolve NIL"
  (let ((tipo (first mov))
        (linha (second mov))
        (coluna (third mov)))
    (cond
      ((eq tipo 'cd) (cd linha coluna tab))
      ((eq tipo 'ce) (ce linha coluna tab))
      ((eq tipo 'cc) (cc linha coluna tab))
      ((eq tipo 'cb) (cb linha coluna tab))
      (t (error "Tipo de movimento desconhecido: ~A" tipo))))
)


(defun gera-sucessores (tab)
  "Devolve lista de pares (movimento . novo-tabuleiro) com todos os movimentos válidos do tabuleiro"
  (labels
      ;; gera para colunas de 1..7 recursivamente
      ((iter-cols (linha coluna acc)
         (cond
           ((> coluna 7) acc)
           (t
            (let* ((movs (movimentos-validos-de-pos linha coluna tab))
                   (pares (mapcar (lambda (m) (cons m (aplica-movimento m tab))) movs))
                   (novo-acc (append acc pares)))
              (iter-cols linha (1+ coluna) novo-acc))))))
    ;; gera para linhas de 1..7 recursivamente, reutilizando iter-cols
    (labels ((iter-linhas (linha acc)
               (cond
                 ((> linha 7) acc)
                 (t (iter-linhas (1+ linha) (iter-cols linha 1 acc))))))
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

(defun o (tab)
  "Calcula os sucessores de um dado tabuleiro"
  (length (gera-sucessores tab))
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