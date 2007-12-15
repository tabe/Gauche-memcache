#!/usr/bin/env gosh
;; -*- mode: scheme; coding: utf-8 -*-

(use fixedpoint.package)
(use fixedpoint.site)
(use text.html-lite)

(//
 (PHP "http://www.php.net/")
 (memcached "http://www.danga.com/memcached/")
 )

(define-package Gauche-memcache 2007 11 1)

(define-index Gauche-memcache
  (html:p (html:dfn /Gauche-memcache/)
          (en/ja
           (list " is an extension package of " /Gauche/ " providing " /memcached/ " client utility.")
           (list " は " /Scheme/ " 処理系 " /Gauche/ " で " /memcached/ " のクライアント機能を利用するための拡張パッケージです。")))

  (html:h2 :style "border-bottom: 1px solid #bbbbbb;" (en/ja "News" "最新情報"))
  (html:ul
   (html:li "[2007-11-01] " (en/ja "It is confirmed that the current version 0.1.0 runs on Gauche 0.8.12."
                                   "Gauche 0.8.12 で現在のバージョン 0.1.0 が動作することを確認しました。"))
   (html:li "[2007-09-12] " (let ((url "http://practical-scheme.net/wiliki/wiliki.cgi?Gauche%3ABugs"))
                              (en/ja (list (html:a :href url "The announced patch")
                                           " is necessary to deal with a port buffering bug if using Gauche 0.8.11.")
                                     (list "Gauche 0.8.11 で動作させるには、ポートバッファリングのバグを修正するため"
                                           (html:a :href url "告知されているパッチ")
                                           "が必要です。"))))
   (html:li "[2007-08-10] " (en/ja "Release 0.1.0." "バージョン 0.1.0 を公開しました。")))

  (html:h2 :style "border-bottom: 1px solid #bbbbbb;" (en/ja "Features" "特徴"))
  (html:ul
   (html:li (en/ja "memcached client API"
                   "memcached クライアント API"))
   (html:li (en/ja (list "memcache based CGI session with " /Gauche-cgi-ext/)
                   (list /Gauche-cgi-ext/ " による memcached ベースの CGI セッション")))
   )

  (html:h2 :style "border-bottom: 1px solid #bbbbbb;" (en/ja "Requirements" "導入"))
  (html:p (en/ja "This package is for Gauche 0.8.10 or later."
                 "このパッケージは Gauche 0.8.10 またはそれ以上で動作します。"))
  (html:ul
   (html:li (en/ja (list "If you would like memcache based CGI session, "
                         /memcached/ " and also " /Gauche-memcache/ " are necessary.")
                   (list "また memcache による CGI セッションを利用する場合には"
                         /memcached/ " および "/Gauche-memcache/ " が必要です。"))))

  (html:h2 :style "border-bottom: 1px solid #bbbbbb;" (en/ja "License" "ライセンス"))
  (html:p "The BSD License")

  (html:h2 :style "border-bottom: 1px solid #bbbbbb;" (en/ja "Download" "ダウンロード"))
  (*package-download*)

  (html:h2 :style "border-bottom: 1px solid #bbbbbb;" (en/ja "Documentation" "文書"))
  (html:ul
   (html:li (html:a :href (en/ja "reference.en.html" "reference.ja.html")
                    "Gauche-memcache " (en/ja "Reference Manual" "リファレンスマニュアル"))))

  (html:h2 :style "border-bottom: 1px solid #bbbbbb;" "FYI")
  (html:ul
   (html:li /memcached/)
   (html:li /PHP/)
   )
  )

(define main package-main)
