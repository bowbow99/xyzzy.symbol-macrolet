[xyzzy.ansify] に取り込まれました。今後はそっちでメンテされるのでこっちは放置になります。

  [xyzzy.ansify]: http://github.com/bowbow99/xyzzy.ansify

これは何
=========
マクロで実装した Common Lisp の symbol-macrolet です。

インストール
============
NOTE: NetInstaller からインストールしたなら以下は不要です。

1. `*load-path*` のどっかに symbol-macrolet.l を置きます
2. OPTIONAL: byte-compile します

「XXXって拡張をインストールしたら symbol-macrolet が必要とか言われた！！」
という人はここまでやれば万事オッケーです。

使い方（開発者向け）
====================
マクロ `symbol-macrolet` が lisp パッケージから `export` してあるので、読み
込んでおけば使えるようになります。

    (eval-when (:load-toplevel :compile-toplevel :execute)
      (require "symbol-macrolet"))

参考: [CLHS: Special Operator SYMBOL-MACROLET][1]

  [1]: http://www.lispworks.com/documentation/lw50/CLHS/Body/s_symbol.htm#symbol-macrolet

EXAMPLE
-------
    (setq *list* '(1 2 3 4))
    => (1 2 3 4)
    
    (symbol-macrolet ((first (first *list*))
                      (second (second *list*)))
      (+ first second))
    => 3
    
    (setq *table* (make-hash-table))
    => #<hashtable 83890708>
    
    (symbol-macrolet ((first (first *list*))
                      (second (second *list*))
                      (third (third *list*)))
      (symbol-macrolet ((foo (gethash 'foo *table*))
                        (bar (gethash 'bar *table*))
                        (baz (gethash 'baz *table*)))
        (setq foo first
              bar second
              baz third)))
    => 3
    
    (maphash (lambda (key value)
               (format t "~S: ~S~%" key value))
             *table*)
    baz: 3
    bar: 2
    foo: 1
    => nil

注意点、既知の問題など
======================
- symbol-macrolet は macro 展開時になんかごちゃごちゃやってるので、symbol-
  macrolet を使ったコードは byte-compile 推奨です。
- `macroexpand`, `macroexpand-1` でシンボルマクロを展開できない。

バグ報告、質問、要望などは [GitHub Issues][2] か [@bowbow99][3] あたりへ
お願いします。

  [2]: http://github.com/bowbow99/xyzzy.symbol-macrolet/issues
  [3]: http://twitter.com/bowbow99/

ライセンス
==========
MIT (see COPYING)
