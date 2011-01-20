これは何
============
マクロで実装した Common Lisp の symbol-macrolet です。

License
=========
The MIT License

Copyright (c) 2009-2011 bowbow99

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in
all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
THE SOFTWARE.


インストール
============
NOTE: NetInstaller からインストールしたなら以下は不要です。

1. `*load-path*` のどっかに symbol-macrolet.l を置きます
2. OPTIONAL: byte-compile します

「XXXって拡張をインストールしたら symbol-macrolet が必要とか言われた！！」という人はここまでやれば万事オッケーです。


使い方（開発者向け）
====================
NOTE: ここは主に symbol-macrolet を使って拡張/lisp を書く人向け

    (eval-when (:load-toplevel :compile-toplevel :execute)
      (require "symbol-macrolet")
      (use-package :symbol-macrolet))

symbol-macrolet パッケージからマクロ `symbol-macrolet` を `export` してあるので、これで `symbol-macrolet` を使えるようになります。


macro symbol-macrolet: ((_symbol_ _expansion_)\*) _declaration_ _form\*_
-----------------------------------------------------------------

- _symbol_: [symbol] symbol-macro の名前
- _expansion_: [form] symbol-macro の展開形
- _declaration_: (declare ...)
   - xyzzy で使える declare は special のみですが、special で symbol-macro の名前を指定するとエラーを吐くので、つまり使い道はありません。忘れてください。
- _form_: [form]

_form*_ 内で **symbol-macro** として指定された _symbol_ が（変数として参照される位置にあれば)
、_expansion_ に展開（置換）された後、評価されます。

- 参考: [CLHS: Special Operator SYMBOL-MACROLET](http://www.lispworks.com/documentation/lw50/CLHS/Body/s_symbol.htm#symbol-macrolet)

# EXAMPLE
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

パッケージについて
------------------
ver. 0.01.04 までマクロ `symbol-macrolet` を lisp パッケージから `export` してましたが、symbol-macrolet パッケージから `export` するように変更しました。

注意点、既知の問題など
======================
- symbol-macrolet は macro 展開時になんかごちゃごちゃやってるので、symbol-macrolet を使ったコードは byte-compile 推奨です。
- `macroexpand`, `macroexpand-1` でシンボルマクロを展開できない。

