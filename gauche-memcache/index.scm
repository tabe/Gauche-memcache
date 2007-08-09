#!/usr/bin/env gosh

(use file.util)
(use fixedpoint.site)
(use text.html-lite)
(use text.tree)

(//
 (memcached "http://www.danga.com/memcached/")
 )

(define *last-update* "Fri Jul 20 2007")
(define *gauche-memcache-version* (file->string "../VERSION"))
(define *gauche-memcache-tarball-basename* (string-append "Gauche-memcache-" *gauche-memcache-version* ".tgz"))
(define *gauche-memcache-tarball-size* (file-size (string-append "../../" *gauche-memcache-tarball-basename*)))
(define *gauche-memcache-tarball-url* *gauche-memcache-tarball-basename*)

(define (index lang)
  (let-syntax ((en/ja (syntax-rules ()
						((_ en ja)
						 (if (string=? "en" lang) en ja)))))
	((fixedpoint:frame "Gauche-memcache")
	 (html:p :id "lang_navi" (html:a :href (en/ja "index.html" "index.en.html")
										"[" (en/ja "Japanese" "English") "]"))
	 (html:p :id "last_update" "Last update: " *last-update*)
	 (fixedpoint:separator)
	 (fixedpoint:adsense)
	 (fixedpoint:separator)
	 (html:p (html:dfn /Gauche-memcache/)
			 (en/ja
				 (list " is an extension package of " /Gauche/ " containing a kind of CGI utilities such as session.")
				 (list " は " /Scheme/ " 処理系 " /Gauche/ " で CGI で利用するセッションなどの機能を提供する拡張パッケージです。")))

	 (html:h2 :style "border-bottom: 1px solid #bbbbbb;" (en/ja "News" "最新情報"))
	 (html:ul
	  (html:li "[2006-07-20] " (en/ja "Release 0.1.0." "バージョン 0.1.0 を公開しました。")))

	 (html:h2 :style "border-bottom: 1px solid #bbbbbb;" (en/ja "Features" "特徴"))
	 (html:ul
	  (html:li (en/ja "an extensible session handling, based on either files or memcache."
					  "拡張可能なセッションハンドリング(ファイルによるもの、もしくは memcache によるもの)。"))
	  (html:li (en/ja "some of neat macros."
					  "いくつかの便利なマクロ。"))
	  )

	 (html:h2 :style "border-bottom: 1px solid #bbbbbb;" (en/ja "Requirements" "導入"))
	 (html:p (en/ja "This package is for Gauche 0.8.10 or later."
					"このパッケージは Gauche 0.8.10 またはそれ以上で動作します。"))
	 (html:ul
	  (html:li (en/ja (list "If you would like memcache-based session, "
                            /memcached/ " and also " /Gauche-memcache/ " are necessary.")
					  (list "また memcache によるセッションを利用する場合には"
                            /memcached/ " および "/Gauche-memcache/ " が必要です。"))))

	 (html:h2 :style "border-bottom: 1px solid #bbbbbb;" (en/ja "Download" "ダウンロード"))
	 (html:p (html:a :href *gauche-memcache-tarball-url*
					 *gauche-memcache-tarball-basename* " (" *gauche-memcache-tarball-size*  " bytes)"))

	 (html:h2 :style "border-bottom: 1px solid #bbbbbb;" (en/ja "Documentation" "文書"))
	 (html:ul
	  (html:li (html:a :href (en/ja "reference.en.html" "reference.ja.html")
					   "Gauche-memcache " (en/ja "Reference Manual" "リファレンスマニュアル"))))

	 (html:h2 :style "border-bottom: 1px solid #bbbbbb;" (en/ja "Links" "リンク"))
	 (html:ul
	  (html:li /PHP/)
	  (html:li /memcached/)
	  (html:li /Gauche-memcache/)
	  )
	 )))

(define (main args)
  (define (usage)
	(format (current-error-port) "usage: gosh ~a (en|ja)\n" *program-name*)
	(exit 1))
  (when (< (length args) 2)
	(usage))
  (write-tree (index (cadr args)))
  0)
