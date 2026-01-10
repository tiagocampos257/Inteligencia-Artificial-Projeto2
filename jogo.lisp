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
  (let ((quem (read)))
    (format t "Tempo limite para cada jogada (ms): ")
    (let ((tempo (read)))
      (ciclo-jogo-inicial quem tempo))))

(defun modo-computador-vs-computador ()
  (format t "~%Modo Computador vs Computador~%")
  (format t "Tempo limite para cada jogada (ms): ")
  (let ((tempo (read)))
    (ciclo-jogo-inicial 1 tempo))) ; 1 significa que começa o computador1
