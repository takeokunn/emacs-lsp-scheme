#! /usr/local/bin/csi -ss

(import (lsp-server)
        (lsp-server chicken)
        (only (lsp-server private) write-log)

        (chicken tcp)
        (scheme process-context)
        (srfi 28))

(define (port-number-available? port-num)
  (condition-case
   (let-values (((inp outp) (tcp-connect "127.0.0.1" port-num)))
     (close-input-port inp)
     (close-input-port outp)
     #f)
   [(exn i/o net) #t]))

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
  (define command-port-number (string->number (list-ref args 0)))
  (define lsp-port-number (string->number (list-ref args 1)))
  (define lsp-start-error-port-number (string->number (list-ref args 2)))
  (define log-level (string->symbol (list-ref args 3)))
  (define lsp-error-port-number
    (find-available-port lsp-start-error-port-number))

  (write-log 'debug
             (format
              "Requesting through command port ~a main connection at ~a and error connection at ~a "
              command-port-number
              lsp-port-number
              lsp-error-port-number))
  (parameterize ((tcp-read-timeout #f)
                 (tcp-write-timeout #f)
                 (tcp-accept-timeout #f)
                 (lsp-server-log-level log-level))
    (lsp-server-request-connection command-port-number
                                   lsp-port-number
                                   lsp-error-port-number)))

(main (cdr (command-line)))
