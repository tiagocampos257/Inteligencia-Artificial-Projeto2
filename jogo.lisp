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
 (load "puzzle.LISP")
 (load "algoritmo.LISP")
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
    (let ((estado (make-estado (puzzle::tabuleiro-inicial) 1)))
      (jogar 'cvc estado tempo))))



(defun ciclo-jogo-inicial (quem-comeca tempo)
  (format t "~%Jogo iniciado!~%")
  (let ((estado (make-estado (puzzle::tabuleiro-inicial) quem-comeca)))
    (jogar 'hvc estado tempo))) 


(defun ler-jogada-humano ()
  (format t "~%Insira jogada (ex: (cd 4 2)): ")
  (read))

(defun jogada-humano (estado)
  "Devolve novo estado apos jogada valida do humano. Se invalida, repete."
  (let* ((tab (estado-tabuleiro estado))
         (jog (estado-jogador estado))
         (mov (ler-jogada-humano))
         (pares (puzzle::gera-sucessores tab jog))
         (hit (procura-sucessor-por-mov mov pares)))
    (if (null hit)
        (progn
          (format t "~%Jogada invalida. Tenta outra vez.~%")
          (jogada-humano estado))
      (make-estado (cdr hit) (puzzle::adversario jog)))))



(defun ciclo-jogo-hvc (estado tempo)     
  (format t "~%Estado atual (jogador ~A a jogar):~%" (estado-jogador estado))
  (puzzle::print-tabuleiro (estado-tabuleiro estado))

  (if (estado-terminal? estado) (format t "Fim de jogo! Vencedor: jogador ~A~%" (puzzle::adversario (estado-jogador estado)))

    (if (= (estado-jogador estado) 1) ; se jogador1 joga ele se, caso contrario joga o jogador 2
        (progn
          (format t "~%Jogadas validas: ")
          (print-jogadas (lista-jogadas-validas estado))
          (ciclo-jogo-hvc (jogada-humano estado) tempo))
          

          (let* ((prof 6)            
                 (par (jogada-computador estado prof 2))
                 (novo-estado (cdr par)))
            (ciclo-jogo-hvc novo-estado tempo)))))

(defun ciclo-jogo-cvc (estado tempo)
  (format t "~%Estado atual (jogador ~A a jogar):~%" (estado-jogador estado))
  (puzzle::print-tabuleiro (estado-tabuleiro estado))

  (if (estado-terminal? estado)
      (format t "~%Fim de jogo! Vencedor: jogador ~A~%" (puzzle::adversario (estado-jogador estado)))

      (let* ((prof 6)
             (par (jogada-computador estado prof (estado-jogador estado)))
             (novo-estado (cdr par)))
        (ciclo-jogo-cvc novo-estado tempo))))




(defun procura-sucessor-por-mov (mov pares)
  "Devolve o par (mov . novo-tab) correspondente, ou NIL se nao existir."
  (cond
    ((null pares) nil)
    ((equal mov (caar pares)) (car pares))
    (t (procura-sucessor-por-mov mov (cdr pares)))))


(defun lista-jogadas-validas (estado)
  (let* ((tab (estado-tabuleiro estado))
         (jog (estado-jogador estado))
         (pares (puzzle::gera-sucessores tab jog)))
    (mapcar #'car pares)))

(defun print-jogadas (jogadas)
  (cond
    ((null jogadas) (format t "~%"))
    (t
     (format t "~A " (first jogadas))
     (print-jogadas (rest jogadas)))))

(defun jogar (modo estado tempo)
  (cond
   ((eq modo 'hvc) (ciclo-jogo-hvc estado tempo))
   ((eq modo 'cvc) (ciclo-jogo-cvc estado tempo))))
