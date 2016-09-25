#lang racket
(require xml net/url)

(define (serve port-no)
  (define main-cust (make-custodian))
  (parameterize ([current-custodian main-cust])
    (define listener (tcp-listen port-no 5 #t))
    (define (loop)
      (accept-and-handle listener)
      (loop))
    (thread loop))
  (lambda ()
    (custodian-shutdown-all main-cust)))

(define (accept-and-handle listener)
  (define cust (make-custodian))
  (parameterize ([current-custodian cust])
  (define-values (in out) (tcp-accept listener))
    (thread
     (lambda ()
       (handle in out)
       (close-input-port in)
       (close-output-port out))))
  (thread (lambda ()
            (sleep 10)
            (custodian-shutdown-all cust))))

(define (handle in out)
  (define req
    (regexp-match #rx"GET (.+) HTTP/[0-9]+\\.[0-9]+"
                  (read-line in)))
  (when req
    (regexp-match #rx"(\r\n|^)\r\n" in)
    (let ([xexpr (dispatch (list-ref req 1))])
      (display "HTTP/1.0 200 OK\r\n" out)
      (display "Server: racket\r\nContent-Type: text/html\r\n\r\n" out)
      (display (xexpr->string xexpr) out))))

(define (dispatch str-path)
  (define url (string->url str-path))
  (define path (map path/param-path (url-path url)))
  (define h (hash-ref dispatch-table (car path) #f))
  (if h
      (h (url-query url))
      `(html (head (title "Error"))
             (body
              (font ((color "red"))
                    "Unknown Page: "
                    ,str-path)))))

(define dispatch-table (make-hash))

(hash-set! dispatch-table "hello"
           (lambda (query)
             `(html (body "Hello, Racket!"))))