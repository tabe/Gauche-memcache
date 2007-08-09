;;;
;;; memcache - memcached client utility
;;;
;;;   Copyright (c) 2007 Takeshi Abe. All rights reserved.
;;;
;;;   Redistribution and use in source and binary forms, with or without
;;;   modification, are permitted provided that the following conditions
;;;   are met:
;;;
;;;   1. Redistributions of source code must retain the above copyright
;;;      notice, this list of conditions and the following disclaimer.
;;;
;;;   2. Redistributions in binary form must reproduce the above copyright
;;;      notice, this list of conditions and the following disclaimer in the
;;;      documentation and/or other materials provided with the distribution.
;;;
;;;   3. Neither the name of the authors nor the names of its contributors
;;;      may be used to endorse or promote products derived from this
;;;      software without specific prior written permission.
;;;
;;;   THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
;;;   "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
;;;   LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
;;;   A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
;;;   OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
;;;   SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED
;;;   TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
;;;   PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
;;;   LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
;;;   NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
;;;   SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
;;;
;;;  $Id$

(define-module memcache
  (use gauche.net)
  (export <memcache-error> <memcache-client-error> <memcache-server-error>
          <memcache-connection> memcache-connect memcache-close
          set add replace get delete incr decr
          stats flush-all version quit))

(select-module memcache)

(define-condition-type <memcache-error> <error> #f)
(define-condition-type <memcache-client-error> <memcache-error> #f
  (message))
(define-condition-type <memcache-server-error> <memcache-error> #f
  (message))

(define-class <memcache-connection> ()
  ((socket :init-keyword :socket
           :getter socket-of)
   (input-port :init-keyword :input-port
               :getter input-port-of)
   (output-port :init-keyword :output-port
                :getter output-port-of)))

(define (memcache-connect host port)
  (let ((socket (make-client-socket 'inet host port)))
    (make <memcache-connection>
      :socket socket
      :input-port (socket-input-port socket)
      :output-port (socket-output-port socket))))

(define-method memcache-close ((connection <memcache-connection>))
  (socket-close (socket-of connection)))

(define (%read-line iport . opt)
  (let-keywords* opt ((allow-eof #f))
    (let ((line (read-line iport)))
      (rxmatch-case line
        (test eof-object? (if allow-eof line (error "unexpected eof")))
        (#/^ERROR$/
         (#f)
         (raise (condition (<memcache-error>))))
        (#/^CLIENT_ERROR (.+)$/
         (#f s)
         (raise (condition (<memcache-client-error> (message s)))))
        (#/^SERVER_ERROR (.+)$/
         (#f s)
         (raise (condition (<memcache-server-error> (message s)))))
        (else
         line)))))

(define (%reply-2 iport success not-found)
  (let ((line (%read-line iport)))
    (cond ((string=? success line) #t)
          ((string=? not-found line) #f)
          (else
           (error "unexpected reply:" line)))))

(define-syntax %define-storage-command
  (syntax-rules ()
    ((_ name)
     (define-method name ((conn <memcache-connection>) key value . opt)
       (let ((iport (slot-ref conn 'input-port))
             (oport (slot-ref conn 'output-port))
             (s (format #f "~s" value)))
         (let-keywords* opt ((flags 0)
                             (exptime 0))
           (format oport "~a ~a ~d ~d ~d\r\n" 'name key flags exptime (string-size s)))
         (format oport "~a\r\n" s)
         (%reply-2 iport "STORED" "NOT_STORED"))))))

(%define-storage-command set)
(%define-storage-command add)
(%define-storage-command replace)

(define (%read-and-test iport reader expected)
  (let ((x (reader iport)))
    (cond ((eof-object? x)
           (error "unexpected eof, instead of" expected))
          ((procedure? expected)
           (expected x))
          (else
           (equal? expected x)))))

(define-method get ((conn <memcache-connection>) . keys)
  (let ((iport (slot-ref conn 'input-port))
        (oport (slot-ref conn 'output-port)))
    (display "get" oport)
    (for-each (cut format oport " ~a" <>) keys)
    (display "\r\n" oport)
    (let lp ((line (%read-line iport))
             (result '()))
      (rxmatch-case line
        (#/^END$/
         (#f)
         (reverse! result))
        (#/^VALUE (\S+) ([0-9]+) ([0-9]+)$/
         (#f key flags bytes)
         (let ((data (read iport)))
           (%read-and-test iport read-char #\return)
           (%read-and-test iport read-char #\newline)
           (lp (%read-line iport) (acons (string->symbol key) data result))))
        (else
         (error "unexpected reply:" line))))))

(define-method delete ((conn <memcache-connection>) key . opt)
  (let ((iport (slot-ref conn 'input-port))
        (oport (slot-ref conn 'output-port)))
    (if (null? opt)
        (format oport "delete ~a\r\n" key)
        (format oport "delete ~a ~d\r\n" key (car opt)))
    (%reply-2 iport "DELETED" "NOT_FOUND")))

(define-syntax %define-*crement-command
  (syntax-rules ()
    ((_ name)
     (define-method name ((conn <memcache-connection>) key value)
       (let ((iport (slot-ref conn 'input-port))
             (oport (slot-ref conn 'output-port)))
         (format oport "~a ~a ~d\r\n" 'name key value)
         (let ((line (%read-line iport)))
           (rxmatch-case line
             (#/^NOT_FOUND$/
              (#f)
              #f)
             (#/^[0-9]+$/
              (carried)
              (string->number carried))
             (else
              (error "unexpected reply:" line)))))))))

(%define-*crement-command incr)
(%define-*crement-command decr)

(define-method stats ((conn <memcache-connection>) . opt)
  (let ((iport (slot-ref conn 'input-port))
        (oport (slot-ref conn 'output-port)))
    (display "stats\r\n" oport)
    (let lp ((line (%read-line iport))
             (result '()))
      (rxmatch-case line
        (#/^END$/
         (#f)
         (reverse! result))
        (#/^STAT (\S+) (\S+)$/
         (#f name value)
         (lp (%read-line iport) (acons (string->symbol name) value result)))
        (else
         (error "unexpected reply:" line))))))

(define-method flush-all ((conn <memcache-connection>) . opt)
  (let ((iport (slot-ref conn 'input-port))
        (oport (slot-ref conn 'output-port)))
    (if (null? opt)
        (display "flush_all\r\n" oport)
        (format oport "flush_all ~d\r\n" (car opt)))
    (let ((line (%read-line iport)))
      (if (string=? "OK" line)
          #t
          (error "unexpected reply:" line)))))

(define-method version ((conn <memcache-connection>))
  (let ((iport (slot-ref conn 'input-port))
        (oport (slot-ref conn 'output-port)))
    (display "version\r\n" oport)
    (let ((line (%read-line iport)))
      (rxmatch-case line
        (#/^VERSION (\S+)$/
         (#f s)
         s)
        (else
         (error "unexpected reply:" line))))))

(define-syntax %define-simple-command
  (syntax-rules ()
    ((_ name x)
     (define-method name ((conn <memcache-connection>))
       (let ((iport (slot-ref conn 'input-port))
             (oport (slot-ref conn 'output-port)))
         (format oport "~a\r\n" 'name)
         (%read-line iport :allow-eof x)
         )))))

(%define-simple-command quit #t)

;; Epilogue
(provide "memcache")
