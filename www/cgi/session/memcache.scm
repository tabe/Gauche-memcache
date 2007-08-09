;;;
;;; www.cgi.session.memcache - memcached based handler
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

(define-module www.cgi.session.memcache
  (extend www.cgi.session)
  (use gauche.parameter)
  (use memcache)
  (export <cgi-session-memcache>
          *session-memcache-host* *session-memcache-port*))

(select-module www.cgi.session.memcache)

(define-class <cgi-session-memcache-meta> (<cgi-session-meta>)
  ())

(define-class <cgi-session-memcache> (<cgi-session>)
  ((connection :init-keyword :connection)
   (key        :init-keyword :key))
  :metaclass <cgi-session-memcache-meta>)

(define *session-memcache-host* (make-parameter "localhost"))
(define *session-memcache-port* (make-parameter 11211))

(define (%id->key id) (string->symbol (string-append "sess-" id)))

(define-method make-id ((class <cgi-session-memcache-meta>))
  (let ((seed (next-method))
        (connection (memcache-connect (*session-memcache-host*) (*session-memcache-port*))))
    (add connection (%id->key seed) '())
    seed))

(define-method session-begin ((class <cgi-session-memcache-meta>) . opt)
  (let ((session (next-method))
        (connection (memcache-connect (*session-memcache-host*) (*session-memcache-port*))))
    (slot-set! session 'connection connection)
    (slot-set! session 'key (%id->key (slot-ref session 'id)))
    session))

(define-method variables-of ((session <cgi-session-memcache>))
  (let* ((result (get (slot-ref session 'connection) (slot-ref session 'key))))
    (cond ((null? result)
           #f)
          ((pair? (car result))
           (cdar result))
          (else
           (error "unexpected form" result)))))

(define-method set-variables ((session <cgi-session-memcache>) vars)
  (set (slot-ref session 'connection) (slot-ref session 'key) vars))

(define-method session-close ((session <cgi-session-memcache>))
  (memcache-close (slot-ref session 'connection)))

(define-method destroyed? ((session <cgi-session-memcache>))
  (let ((result (get (slot-ref session 'connection) (slot-ref session 'key))))
    (null? result)))

(define-method session-destroy ((session <cgi-session-memcache>))
  (delete (slot-ref session 'connection) (slot-ref session 'key)))

(provide "www/cgi/session/memcache")
