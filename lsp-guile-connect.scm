#!/usr/bin/env sh
exec guile --r7rs -e main -s "$0" "$@"
!#

(import (lsp-server)
        (lsp-server guile)
        (only (lsp-server private) write-log)
        (scheme base)
        (scheme process-context)
        (scheme write)
        (srfi srfi-18)
        (srfi srfi-28))

(define (port-number-available? port-num)
  (with-exception-handler
   (lambda (exn)
     #t)
   (lambda ()
     (let-values (((inp outp) ($tcp-connect "127.0.0.1" port-num)))
       (close-input-port inp)
       #f))
   #:unwind? #t))

(define (find-available-port start-port-number)
  (define max-tries 25)
  (let loop ((port-number start-port-number)
             (num-tries 0))
    (cond ((> num-tries max-tries)
           (error "Can't find available port. Consider changing the start port number or increasing the number of tries."))
          ((port-number-available? port-number)
           port-number)
          (else (loop (+ port-number 1)
                      (+ num-tries 1))))))

(define (main args)
  (define command-port-number (string->number (list-ref args 1)))
  (define lsp-port-number (string->number (list-ref args 2)))
  (define lsp-start-error-port-number
    (string->number (list-ref args 3)))
  (define log-level (string->symbol (list-ref args 4)))
  (define lsp-error-port-number
    (find-available-port lsp-start-error-port-number))

  (write-log 'debug
             (format
              "Requesting through command port ~a main connection at ~a and error connection at ~a "
              command-port-number
              lsp-port-number
              lsp-error-port-number))
  (parameterize ((lsp-server-log-level log-level))
    (lsp-server-request-connection command-port-number
                                   lsp-port-number
                                   lsp-error-port-number)))
