#lang info
(define collection "ollama-chat")
(define deps '("base"))
(define build-deps '("scribble-lib" "racket-doc" "rackunit-lib"))
(define scribblings '(("scribblings/ollama-chat.scrbl" ())))
(define pkg-desc "Ollama chat clienet")
(define version "0.0")
(define pkg-authors '(tojoqk))
(define license '(Apache-2.0))
