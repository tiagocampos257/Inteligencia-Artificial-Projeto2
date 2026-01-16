;Mudar este parametro para o diretório correspondente do seu PC 
(defparameter *ficheiro-log*
  "C:/Users/filip/OneDrive/Área de Trabalho/Universidade/3ºano/AI/projetoAI2/logs/log.dat")



;------------------------------------------------ESTADO----------------------------------------------------
;Cria um estado
(defun make-estado (tabuleiro jogador)
  (list tabuleiro jogador))


(defun estado-tabuleiro (estado)
  (first estado))

(defun estado-jogador (estado)
  (second estado))

(defun sucessores-estado (estado)
  (let ((jog (estado-jogador estado)))
    (mapcar
      (lambda (par)
        (make-estado
          (cdr par)              ; tabuleiro já resultante
          (puzzle::adversario jog)))     ; troca de jogador
      (puzzle::gera-sucessores
        (estado-tabuleiro estado)
        jog))))

(defun estado-terminal? (estado)
  (puzzle::objetivo? (estado-tabuleiro estado)))

;------------------------------------------------AVALIZACAO---------------------------------------------

(defun utilidade (estado jogador-max)
  "1 se jogador-max ganhou, -1 se perdeu"
  (let ((vencedor (puzzle::adversario (estado-jogador estado))))
    (if (= vencedor jogador-max) 1 -1)))

(defun avaliacao (estado jogador-max)
  "Funcao heuristica: vantagem do jogador-max no estado dado"
  (let* ((tab (estado-tabuleiro estado))
         (jog-actual (estado-jogador estado))
         (jog-min (if (= jogador-max 1) 2 1))

         (movs-max (length (puzzle::gera-sucessores tab jogador-max)))

         (movs-min (length (puzzle::gera-sucessores tab jog-min))))
    (- movs-max movs-min)))

(defun valor-estado (estado jogador-max)
  (if (estado-terminal? estado)
      (* 1000 (utilidade estado jogador-max)) ; vitória domina tudo
      (avaliacao estado jogador-max)))

;ou seja nenhuma heuristica intermedia consegue competir ao ganhar o jogo
;-------------------------------------------------MINIMAX----------------------------------------------------

(defun max-valor (sucessores prof jogador-max)
  (cond
    ((null sucessores) -1000) ; estado mau para MAX
    (t
     (max
       (minimax (first sucessores) prof jogador-max)
       (max-valor (rest sucessores) prof jogador-max)))))

(defun min-valor (sucessores prof jogador-max)
  (cond
    ((null sucessores) 1000) ; estado mau para MIN (bom para MAX)
    (t
     (min
       (minimax (first sucessores) prof jogador-max)
       (min-valor (rest sucessores) prof jogador-max)))))

(defun minimax (estado prof jogador-max)
  (cond
    ;; estado terminal ou limite de profundidade
    ((or (estado-terminal? estado) (= prof 0))
     (valor-estado estado jogador-max))

    ;; turno do MAX
    ((= (estado-jogador estado) jogador-max)
     (max-valor
       (sucessores-estado estado)
       (1- prof)
       jogador-max))

    ;; turno do MIN
    (t
     (min-valor
       (sucessores-estado estado)
       (1- prof)
       jogador-max))))


(defun escolhe-melhor (melhor-atual restantes prof jogador-max)
  (cond
    ((null restantes) melhor-atual)

    (t
     (let* ((valor-atual
              (minimax melhor-atual (1- prof) jogador-max))
            (valor-novo
              (minimax (first restantes) (1- prof) jogador-max)))

       (if (> valor-novo valor-atual)
           (escolhe-melhor
             (first restantes)
             (rest restantes)
             prof
             jogador-max)
           (escolhe-melhor
             melhor-atual
             (rest restantes)
             prof
             jogador-max))))))



(defun melhor-jogada-aux (sucessores prof jogador-max)
  (cond
    ((null sucessores) nil)

    (t
     (escolhe-melhor
       (first sucessores)
       (rest sucessores)
       prof
       jogador-max))))


(defun melhor-jogada (estado prof jogador-max)
  (melhor-jogada-aux
    (sucessores-estado estado)
    prof
    jogador-max))

;------------------------------------------------ALFABETA-----------------------------------------------------------

(defun alpha-max (sucessores prof alpha beta jogador-max)
  (cond
    ((null sucessores) alpha)

    (t
     (let* ((valor
              (alpha-beta
                (first sucessores)
                (1- prof)
                alpha
                beta
                jogador-max))
            (novo-alpha (max alpha valor)))

       (if (>= novo-alpha beta)
           novo-alpha
           (alpha-max
             (rest sucessores)
             prof
             novo-alpha
             beta
             jogador-max))))))


(defun alpha-min (sucessores prof alpha beta jogador-max)
  (cond
    ((null sucessores) beta)

    (t
     (let* ((valor
              (alpha-beta
                (first sucessores)
                (1- prof)
                alpha
                beta
                jogador-max))
            (novo-beta (min beta valor)))

       (if (<= novo-beta alpha)
           novo-beta
           (alpha-min
             (rest sucessores)
             prof
             alpha
             novo-beta
             jogador-max))))))


(defun alpha-beta (estado prof alpha beta jogador-max)
  (cond
    ;; terminal ou profundidade limite
    ((or (estado-terminal? estado) (= prof 0))
     (valor-estado estado jogador-max))

    ;; nó MAX
    ((= (estado-jogador estado) jogador-max)
     (alpha-max
       (sucessores-estado estado)
       (1- prof)
       alpha
       beta
       jogador-max))

    ;; nó MIN
    (t
     (alpha-min
       (sucessores-estado estado)
       (1- prof)
       alpha
       beta
       jogador-max))))

(defun melhor-jogada-ab (estado prof jogador-max)
  (melhor-jogada-ab-aux
    (sucessores-estado estado)
    prof
    jogador-max
    -1000
    1000))


(defun melhor-jogada-ab-aux (sucessores prof jogador-max alpha beta)
  (cond
    ((null sucessores) nil)

    (t
     (escolhe-melhor-ab
       (first sucessores)
       (rest sucessores)
       prof
       jogador-max
       alpha
       beta))))

(defun escolhe-melhor-ab (melhor-atual restantes prof jogador-max alpha beta)
  (cond
    ((null restantes) melhor-atual)

    (t
     (let* ((valor-atual
              (alpha-beta melhor-atual (1- prof) alpha beta jogador-max))
            (valor-novo
              (alpha-beta (first restantes) (1- prof) alpha beta jogador-max)))

       (if (> valor-novo valor-atual)
           (escolhe-melhor-ab
             (first restantes)
             (rest restantes)
             prof
             jogador-max
             (max alpha valor-novo)
             beta)
           (escolhe-melhor-ab
             melhor-atual
             (rest restantes)
             prof
             jogador-max
             alpha
             beta))))))


(defun sucessores-com-jogada (estado)
  "Devolve lista de pares (mov . novo-estado) para o jogador atual.
    usa o tabuleiro resultante que ja vem do puzzle."
  (let* ((tab (estado-tabuleiro estado))
         (jog (estado-jogador estado))
         (pares (puzzle::gera-sucessores tab jog))) ; (mov . novo-tab)
    (mapcar (lambda (par)
              (cons (car par)
                    (make-estado (cdr par) (puzzle::adversario jog))))
            pares)))

;---------------------------------------------ALFABETA COM STATS------------------------------------------------------

(defun alpha-beta-stats (estado prof alpha beta jogador-max)
  (cond
    ((or (estado-terminal? estado) (= prof 0))
     (values (valor-estado estado jogador-max) 1 0 0))

    ((= (estado-jogador estado) jogador-max)
     (alpha-max-stats (sucessores-com-jogada estado) prof alpha beta jogador-max))

    (t
     (alpha-min-stats (sucessores-com-jogada estado) prof alpha beta jogador-max))))

(defun alpha-max-stats (sucessores prof alpha beta jogador-max)
  (alpha-max-stats-aux sucessores prof alpha beta jogador-max alpha 0 0 0))

(defun alpha-max-stats-aux (sucessores prof alpha beta jogador-max melhor nos ca cb)
  (cond
    ((null sucessores)
     (values melhor nos ca cb))
    (t
     (multiple-value-bind (valor-filho nos1 ca1 cb1)
         (alpha-beta-stats (cdar sucessores) (1- prof) alpha beta jogador-max)
       (let* ((nos2 (+ nos nos1))
              (ca2 (+ ca ca1))
              (cb2 (+ cb cb1))
              (melhor2 (max melhor valor-filho))
              (alpha2 (max alpha melhor2)))
         (if (>= alpha2 beta)
             (values melhor2 nos2 ca2 (1+ cb2))
             (alpha-max-stats-aux (cdr sucessores) prof alpha2 beta
                                  jogador-max melhor2 nos2 ca2 cb2)))))))

(defun alpha-min-stats (sucessores prof alpha beta jogador-max)
  (alpha-min-stats-aux sucessores prof alpha beta jogador-max beta 0 0 0))

(defun alpha-min-stats-aux (sucessores prof alpha beta jogador-max melhor nos ca cb)
  (cond
    ((null sucessores)
     (values melhor nos ca cb))
    (t
     (multiple-value-bind (valor-filho nos1 ca1 cb1)
         (alpha-beta-stats (cdar sucessores) (1- prof) alpha beta jogador-max)
       (let* ((nos2 (+ nos nos1))
              (ca2 (+ ca ca1))
              (cb2 (+ cb cb1))
              (melhor2 (min melhor valor-filho))
              (beta2 (min beta melhor2)))
         (if (<= beta2 alpha)
             (values melhor2 nos2 (1+ ca2) cb2)
             (alpha-min-stats-aux (cdr sucessores) prof alpha beta2
                                  jogador-max melhor2 nos2 ca2 cb2)))))))


(defun escolhe-melhor-ab-stats (pares prof jogador-max alpha beta)
  (escolhe-melhor-ab-stats-aux pares prof jogador-max alpha beta nil nil -1000000 0 0 0))

(defun escolhe-melhor-ab-stats-aux (pares prof jogador-max alpha beta melhor-mov melhor-est melhor-val nos ca cb)
  (cond
    ((null pares)
     (values melhor-mov melhor-est melhor-val nos ca cb))

    (t
     (let ((mov (caar pares))
           (est (cdar pares)))
       (multiple-value-bind (val nos1 ca1 cb1)
           (alpha-beta-stats est (1- prof) alpha beta jogador-max)
         (let* ((nos2 (+ nos nos1))
                (ca2 (+ ca ca1))
                (cb2 (+ cb cb1))
                (melhor? (> val melhor-val))
                (melhor-mov2 (if melhor? mov melhor-mov))
                (melhor-est2 (if melhor? est melhor-est))
                (melhor-val2 (if melhor? val melhor-val))
                (alpha2 (max alpha melhor-val2)))
           (if (>= alpha2 beta)
               ;; corte beta ao escolher (opcional; mantém coerência com AB)
               (values melhor-mov2 melhor-est2 melhor-val2 nos2 ca2 (1+ cb2))
             (escolhe-melhor-ab-stats-aux (cdr pares) prof jogador-max alpha2 beta
                                          melhor-mov2 melhor-est2 melhor-val2
                                          nos2 ca2 cb2))))))))


(defun melhor-jogada-ab-stats (estado prof jogador-max)
  (escolhe-melhor-ab-stats
   (sucessores-com-jogada estado)
   prof
   jogador-max
   -1000
   1000))


(defun simbolo-sem-package (sym)
  (intern (symbol-name sym) :cl-user))

(defun normaliza-mov (mov)
  "Converte o tipo do movimento para um símbolo no CL-USER (ex: PUZZLE::B -> B)."
  (list (simbolo-sem-package (first mov))
        (second mov)
        (third mov)))

(defun escreve-log (mov nos ca cb tempo-seg)
  (let* ((mov-limpo (normaliza-mov mov))
         (linha (format nil
                        "Jogada: ~A | Nos: ~A | Cortes-alpha: ~A | Cortes-beta: ~A | Tempo: ~,3Fs~%"
                        mov-limpo nos ca cb tempo-seg)))
    ;; ecrã
    (format t "~A" linha)
    ;; ficheiro
    (with-open-file (s *ficheiro-log*
                       :direction :output
                       :if-exists :append
                       :if-does-not-exist :create)
      (format s "~A" linha))))

;-------------------funcao que o jogo.lisp vai usar-----------------------------------------------

(defun jogada-computador (estado prof jogador-max)
  "Devolve (mov . novo-estado). Escreve log antes de devolver."
  (let ((t0 (get-internal-real-time)))
    (multiple-value-bind (mov est val nos ca cb)
        (melhor-jogada-ab-stats estado prof jogador-max)
      (declare (ignore val))
      (let* ((t1 (get-internal-real-time))
             (dt (/ (- t1 t0) internal-time-units-per-second)))
        (escreve-log mov nos ca cb dt)
        (cons mov est)))))










