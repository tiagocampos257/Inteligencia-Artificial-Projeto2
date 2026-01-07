;;; Projeto Solitario 2 - Inteligencia Artificial 2025/2026
;;; Autores: 
;; Felisberto de Carvalho 202200359
;; Tiago Campos 202300064
;; Filipe Patricio 202300133

;;; puzzle.lisp
;;; Representacao do tabuleiro e operadores de movimento do Solitario.

;; valor no codigo: 1 - significado: ha um pino
;; valor no codigo: 0 - significado: casa vazia
;; valor no codigo: nil - significado: fora do tabuleiro


;;; Representacao do tabuleiro inicial
(defun tabuleiro-inicial ()
  '((nil nil 1 1 1 nil nil)
    (nil nil 1 1 1 nil nil)
    (0 0 0 0 0 0 0)
    (0 0 0 0 0 0 0)
    (0 0 0 0 0 0 0)
    (nil nil 2 2 2 nil nil)
    (nil nil 2 2 2 nil nil)))