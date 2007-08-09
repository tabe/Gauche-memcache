#!/usr/bin/env gosh

(use text.html-lite)
(use www.cgi)
(use www.cgi.session.memcache)

;; Note that the parameters are set correctly.
(*session-cookie-domain* "localhost")
(*session-memcache-host* "localhost")
(*session-memcache-port* 11211)

(define (main args)
  (cgi-main
   (lambda (params)
     (let ((session (session-begin <cgi-session-memcache>)))
       `(,(cgi-header :cookies (construct-cookie-string session))
         ,(html-doctype)
         ,(html:html
           (html:head (html:title "example: counter"))
           (html:body
            (html:h1 "Counter")
            (html:p "Your deposit: "
                    (let ((current (session-get session 'counter)))
                      (session-set session 'counter (if current (+ current 1) 1))
                      (or current
                          0))))))))))
