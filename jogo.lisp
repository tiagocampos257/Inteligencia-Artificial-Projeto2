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
 (load "algoritmo.lisp")
 (load "puzzle.lisp")
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


(defun modo-humano-vs-computador ()
  (format t "~%Modo Humano vs Computador~%")
  (format t "Quem começa? (1 - Humano 2 - Computador): ")
  (let ((quem-comeca (read)))
    (format t "Tempo limite para cada jogada (ms): ")
    (let ((tempo (read)))
      (ciclo-jogo-inicial quem-comeca tempo))))


(defun modo-computador-vs-computador ()
  (format t "~%Modo Computador vs Computador~%")
  (format t "Tempo limite para cada jogada (ms): ")
  (let ((tempo (read)))
    (ciclo-jogo-inicial 1 tempo))) ; 1 significa que começa o computador1



(defun ciclo-jogo-inicial (quem-comeca tempo)
  (format t "~%Jogo iniciado!~%")
  (let ((estado (estado-inicial)))
    (ciclo-jogo estado quem-comeca tempo))) ; 1 = jogador1 comeca


(defun ler-jogada-humano ()
  (format t "Introduza uma jogada: ")     ; deve devolver algo como (b 2 3) ou tipo (cd 4 2)
  (read))

(defun jogada-humana-valida (mov estado jogador)
  (let ((sucessores (gera-sucessores estado jogador)))
    (assoc mov sucessores :test #'equal)))      ; se existir devolve o par mov . novo-estado, se nao nil


(defun turno-humano (estado jogador)
  (let ((mov (ler-jogada-humano)))
    (let ((par (jogada-humana-valida mov estado jogador)))
      (if par
          (cdr par) ; devolve o novo estado
          (progn
            (format t "Jogada invalida, tente novamente.~%")
            (turno-humano estado jogador))))))


(defun ciclo-jogo (estado jogador tempo)     ; recebe o estado do tabuleiro, 1 ou 2 quem joga agora e tempo limite por jogada
  (format t "~%Estado atual:~%")
  (print-tabuleiro estado)
  (if (objetivo? estado) (format t "Fim de jogo!~%")
    (if (= jogador 1)                                       ; se jogador1 joga ele se, caso contrario joga o jogador 2
        (let ((novo-estado (turno-humano estado 1)))
          (ciclo-jogo novo-estado 2 tempo))

        (let ((resultado (jogar estado tempo)))             ; computador calcula a melhor jogada

           (let ((novo-estado (second resultado)))          ; vai buscar o tabuleiro depois da jogada do computador
              (ciclo-jogo novo-estado 1 tempo))))))

;------------------------------FUNCAO PRINCIPAL DE JOGAR -----------------------------------------------------

(defun jogar (tabuleiro tempo)
  "Chama o alg AlfaBeta e devolve ((mov) novo-tabuleiro)"
  (let* ((estado (make-estado tabuleiro 2))  ; computador é jogador 2
         (prof 6)                            ; profundidade fixa (simples)
         (par (jogada-computador estado prof 2))
         (mov (car par))
         (novo-estado (cdr par)))
    (list mov (estado-tabuleiro novo-estado))))

