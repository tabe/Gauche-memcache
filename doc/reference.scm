#!/usr/bin/env gosh
;; -*- coding: euc-jp -*-

(use text.html-lite)
(use text.tree)

(define *version* "0.1.0")
(define *last-update* "Fri Aug 10 2007")

(define-syntax def
  (syntax-rules (en ja procedure method)
	((_ en)
     '())
    ((_ ja)
	 '())
	((_ en (synopsis x y z ...) rest ...)
     (cons
      (def (synopsis x z ...))
      (def en rest ...)))
	((_ ja (synopsis x y z ...) rest ...)
     (cons
      (def (synopsis y z ...))
      (def ja rest ...)))
	((_ ((procedure (name arg ...) ...) (p ...) z ...))
     (list
      (html:h3 (html:span :class "type" "procedure") ": "
               (html:span :class "procedure" (html-escape-string (symbol->string 'name))) " "
               (cons (html:span :class "argument" 'arg) " ") ...)
      ...
      (map
       (lambda (x)
         (if (string? x)
             (html:p (html-escape-string x))
             (html:pre (html-escape-string (list-ref '(z ...) x)))))
       (list p ...))
      (html:hr)))
	((_ ((method (name arg ...) ...) (p ...) z ...))
	 (list
	  (html:h3 (html:span :class "type" "method") ": "
			   (html:span :class "method" (html-escape-string (symbol->string 'name))) " "
			   (cons (html:span :class "argument" (html-escape-string (x->string 'arg))) " ") ...)
	  ...
      (map
       (lambda (x)
         (if (string? x)
             (html:p (html-escape-string x))
             (html:pre (html-escape-string (list-ref '(z ...) x)))))
       (list p ...))
	  (html:hr)))
	((_ ((type name ...) (p ...) z ...))
	 (list
	  (html:h3 (html:span :class "type" 'type) ": "
			   (html:span :class 'type (html-escape-string (symbol->string 'name))))
	  ...
      (map
       (lambda (x)
         (if (string? x)
             (html:p (html-escape-string x))
             (html:pre (html-escape-string (list-ref '(z ...) x)))))
       (list p ...))
	  (html:hr)))))

(define-macro (api lang)
  `(def ,lang
		((class <memcache-connection>)
		 ("The class abstracts connections to memcached."
          "You will get its instance by calling procedure \"memcache-connect\".")
		 ("このクラスは memcached への接続を抽象化しています。"
          "memcache-connect を呼ぶことでインスタンスを取得します。"))

        ((class <memcache-error>
                <memcache-client-error>
                <memcache-server-error>)
         ("These errors represent ERROR, CLIENT_ERROR and SERVER_ERROR from memcached respectively."
          "An instance of either <memcache-client-error> or <memcache-server-error> has slot 'message'.")
         ("これらのエラーはそれぞれ memcached から受け取る ERROR、CLIENT_ERROR および SERVER_ERROR を表します。"
          "<memcache-client-error> と <memcache-server-error> のインスタンスはスロット 'message' を持ちます。"))

		((procedure (memcache-connect host port))
		 ("Return a connection to memcached. An error occurs if failed.")
		 ("memcached への接続を返します。接続に失敗した場合はエラーが報告されます。"))

		((method (memcache-close (conn <memcache-connection>)))
		 ("Close the connection. See also method \"quit\".")
		 ("接続を閉じます。メソッド \"quit\" も参照してください。"))

        ((parameter *memcache-read-line-max*
                    *memcache-read-retry-max*
                    *memcache-read-nanosecond*)
         ("The parameter *memcache-read-line-max* keeps the max of length of the response line, which default value is 256"
          "For *memcache-read-retry-max* and *memcache-read-nanosecond*, read the source.")
         ("パラメータ *memcache-read-line-max* は memcached のレスポンス行の最大の長さを保持し、規定値は256です。"
          "*memcache-read-retry-max* と *memcache-read-nanosecond* についてはソースを読んでください。"))

        ((method (set (conn <memcache-connection>) key value &optional opt)
                 (add (conn <memcache-connection>) key value &optional opt)
                 (replace (conn <memcache-connection>) key value &optional opt))
         ("Set the value with key by command 'set'/'add'/'replace', resp."
          "Return #t in case of success, or #f otherwise.")
         ("キー key および値 value を持つエントリを登録します。"
          "成功の場合には #t を、そうでなければ #f を返します。"))

        ((method (get (conn <memcache-connection>) &optional keys))
         ("Return entries with keys as alist.")
         ("キー keys を持つエントリを連想配列で取得します。"))

        ((method (delete (conn <memcache-connection>) key &optional opt))
         ("Delete the entry with key."
          "Return #t if done, or #f otherwise.")
         ("キー key を持つエントリを削除します。"
          "削除した場合には #t を、そうでなければ #f を返します。"))

        ((method (incr (conn <memcache-connection>) key value)
                 (decr (conn <memcache-connection>) key value))
         ("Increment/decrement the entry with key by value."
          "Return the resulting value if done, or #f if there is no such entry.")
         ("キー key を持つエントリの値を value 分増やします/減らします。"
          "増/減した場合には結果の値を返し、エントリが見つからなければ #f を返します。"))

        ((method (stats (conn <memcache-connection>) &optional opt))
         ("Return the stats of memcached as string.")
         ("memcached の統計情報を文字列で返します。"))

        ((method (flush-all (conn <memcache-connection>) &optional opt))
         ("Drop all of the entries with possible delay."
          "Return #t in case of success.")
         ("全てのエントリを削除します。opt で遅延させることができます。"
          "成功ならば #t を返します。"))

        ((method (version (conn <memcache-connection>)))
         ("Return the version of memcached as string.")
         ("memcached のバージョンを文字列で返します。"))

        ((method (quit (conn <memcache-connection>)))
         ("Publish command 'quit'. See also method \"memcache-close\".")
         ("コマンド 'quit' を発行します。メソッド \"memcache-close\" も参照してください。"))
		))

(define-macro (api-session lang)
  `(def ,lang
		((parameter *session-memcache-host*
                    *session-memcache-port*)
		 ("You can specify the hostname and port number of memcached by them, respectively."
          "As default they have value \"localhost\" and 11211.")
		 ("これらのパラメータによって memcached のホスト名とポート番号を指定できます。"
          "既定値はそれぞれ \"localhost\" と 11211 です。"))))

(define (document-tree lang)
  (let ((title (if (eq? 'ja lang) "Gauche-memcache リファレンスマニュアル" "Gauche-memcache Reference Manual")))
	(html:html
	 (html:head
	  (if (eq? 'ja lang) (html:meta :http-equiv "Content-Type" :content "text/html; charset=UTF-8") '())
	  (html:title title))
	 (html:body
	  (html:h1 title)
	  (html:style
	   :type "text/css"
	   "<!-- \n"
	   "h2 { background-color:#dddddd; }\n"
	   "address { text-align: right; }\n"
	   ".type { font-size: medium; text-decoration: underline; }\n"
	   ".procedure { font-size: medium; font-weight: normal; }\n"
	   ".method { font-size: medium; font-weight: normal; }\n"
	   ".argument { font-size: small; font-style: oblique; font-weight: normal; }\n"
	   ".constant { font-size: medium; font-weight: normal; }\n"
	   ".variable { font-size: medium; font-weight: normal; }\n"
	   "#last_update { text-align: right; font-size: small; }\n"
	   "#project { text-align: right; }\n"
	   " -->")
	  (html:p "For version " *version*)
	  (html:p :id "last_update" "last update: " *last-update*)
	  (html:p :id "project" (html:a :href "http://fixedpoint.jp/gauche-memcache/" "http://fixedpoint.jp/gauche-memcache/"))
	  (if (eq? 'en lang)
		  (html:p (html:span :style "color:red;" "Warning:") " still unstable.")
		  (html:p (html:span :style "color:red;" "警告:") " 今後変更の可能性があります。"))
      (html:h2 "Information")
      (if (eq? 'en lang)
          "At first see the protocol docs: "
          "まずプロトコルのドキュメントを参照してください: ")
      (html:a :href "http://code.sixapart.com/svn/memcached/trunk/server/doc/protocol.txt"
              "http://code.sixapart.com/svn/memcached/trunk/server/doc/protocol.txt")
      (html:h2 "API")
	  (if (eq? 'en lang)
          (api en)
          (api ja))
      (html:h2 "CGI Session with memcache")
      (if (eq? 'en lang)
          "For documentation on CGI session with Gauche-cgi-ext we assume, see: "
          "仮定する Gauche-cgi-ext による CGI セッションについては次を参照してください: ")
      (html:a :href "http://fixedpoint.jp/gauche-cgi-ext/" "http://fixedpoint.jp/gauche-cgi-ext/")
      (if (eq? 'en lang)
          (api-session en)
          (api-session ja))
	  (html:address "&copy; 2007 Takeshi Abe")
	  ))))

(define (main args)
  (define (usage)
	(format (current-error-port) "usage: gosh reference.scm (en|ja)\n")
	(exit 1))
  (when (< (length args) 2)
	(usage))
  (write-tree (document-tree (string->symbol (cadr args))))
  0)
