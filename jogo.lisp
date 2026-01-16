;;; Projeto Solitario 2 - Inteligencia Artificial 2025/2026
;;; Autores: 
;; Felisberto de Carvalho 202200359
;; Tiago Campos 202300064
;; Filipe Patricio 202300133

; Ficheiro jogo.lisp
 
; Interacao com o utilizador
;;; Carrega: algoritmo.lisp e puzzle.lisp

(defun carregar-ficheiros ()
 "carregar os ficheiros algoritmo e puzzle"
 (load "C:/Users/filip/OneDrive/Área de Trabalho/Universidade/3ºano/AI/projetoAI2/puzzle.LISP")
 (load "C:/Users/filip/OneDrive/Área de Trabalho/Universidade/3ºano/AI/projetoAI2/algoritmo.LISP")
)

;; Listas de escolhas

(defun menu-principal ()
  "Imprime o menu principal que inicia a interacao com o utilizador."
  (format t "~%--- Bem-vindo ao jogo do Solitario! ---~%")
  (format t "1 - Humano vs Computador ~%")
  (format t "2 - Computador vs Computador ~%")
  (format t "3 - Sair ~% ")
  (read)
)

;Opcao que o utilizador escolher

(defun iniciar-jogo()
  (carregar-ficheiros)
  (let ((opcao (menu-principal)))
    (cond
     ((= opcao 1) (modo-humano-vs-computador))
     ((= opcao 2) (modo-computador-vs-computador))
     ((= opcao 3) (format t "A sair do jogo... ~%"))
     (t (format t "Opção Invalida! ~%") (iniciar-jogo)))))


;;; --------- Utilitarios de input/normalizacao ---------

(defun simbolo-para-puzzle (sym)
  (intern (symbol-name sym) :puzzle))

(defun normaliza-mov-humano (mov)
  (list (simbolo-para-puzzle (first mov))
        (second mov)
        (third mov)))

(defun ler-jogada-humano ()
  (format t "~%Introduza uma jogada (ex: (b 2 3)) ou 'quit: ")
  (read))

(defun lista-jogadas-validas (estado)
  (let* ((tab (estado-tabuleiro estado))
         (jog (estado-jogador estado))
         (pares (puzzle::gera-sucessores tab jog)))
    (mapcar #'car pares)))

(defun print-jogadas (jogadas)
  (cond
    ((null jogadas) (format t "~%"))
    (t (format t "~A " (first jogadas))
       (print-jogadas (rest jogadas)))))

(defun procura-sucessor-por-mov (mov pares)
  (cond
    ((null pares) nil)
    ((equal mov (caar pares)) (car pares))
    (t (procura-sucessor-por-mov mov (cdr pares)))))

(defun turno-humano (estado)
  (let* ((tab (estado-tabuleiro estado))
         (jog (estado-jogador estado))
         (mov-lido (ler-jogada-humano)))
    (cond
      ((eq mov-lido 'quit) 'quit)
      (t
       (let* ((mov (normaliza-mov-humano mov-lido))
              (pares (puzzle::gera-sucessores tab jog))
              (hit (procura-sucessor-por-mov mov pares)))
         (if (null hit)
             (progn
               (format t "~%Jogada invalida, tente novamente.~%")
               (turno-humano estado))
           (make-estado (cdr hit) (adversario jog))))))))

;;; --------- Apresentacao ---------

(defun mostra-estado (estado)
  (format t "~%Estado atual (Jogador ~A a jogar):~%" (estado-jogador estado))
  (puzzle::print-tabuleiro (estado-tabuleiro estado)))

;;; --------- Modos de jogo ---------

(defun modo-humano-vs-computador ()
  (format t "~%Modo Humano vs Computador~%")
  (format t "Quem e o Humano? (1 ou 2): ")
  (let ((jog-humano (read)))
    (format t "Profundidade do computador (ex: 3 ou 4): ")
    (let ((prof (read)))
      (let ((estado (make-estado (puzzle::tabuleiro-inicial) 1)))
        (ciclo-hvc estado prof jog-humano)))))

(defun ciclo-hvc (estado prof jog-humano)
  (cond
    ((eq estado 'quit)
     (format t "~%Jogo interrompido pelo utilizador.~%")
     estado)

    ((estado-terminal? estado)
     (mostra-estado estado)
     (let ((v (vencedor estado)))
   (if (= v jog-humano)
       (format t "~% Parabéns! Ganhaste o jogo! ~%")
       (format t "~% O computador ganhou o jogo. ~%")))
 estado)

    ((= (estado-jogador estado) jog-humano)
     (mostra-estado estado)
     (format t "~%--- TURNO HUMANO (Jogador ~A) ---~%" jog-humano)
     (format t "~%Jogadas validas: ")
     (print-jogadas (lista-jogadas-validas estado))
     (ciclo-hvc (turno-humano estado) prof jog-humano))

   (t
     (mostra-estado estado)
     (format t "~%--- TURNO COMPUTADOR (Jogador ~A) ---~%" (estado-jogador estado))
     (let* ((par (jogada-computador estado prof (estado-jogador estado)))
            (novo-estado (cdr par)))
       (ciclo-hvc novo-estado prof jog-humano)))
 ))

(defun modo-computador-vs-computador ()
  (format t "~%Modo Computador vs Computador~%")
  (format t "Profundidade (ex: 3 ou 4): ")
  (let ((prof (read)))
    (let ((estado (make-estado (puzzle::tabuleiro-inicial) 1)))
      (ciclo-cvc estado prof))))

(defun ciclo-cvc (estado prof)
  (cond
    ((estado-terminal? estado)
     (mostra-estado estado)
     (format t "~%Fim de jogo! Vencedor: Jogador ~A~%" (vencedor estado))
     estado)

    (t
     (mostra-estado estado)
     (format t "~%--- TURNO COMPUTADOR (Jogador ~A) ---~%" (estado-jogador estado))
     (let ((par (jogada-computador estado prof (estado-jogador estado))))
       (ciclo-cvc (cdr par) prof)))))


(defun vencedor (estado)
  "Devolve o jogador que fez a última jogada válida."
  (puzzle::adversario (estado-jogador estado)))
