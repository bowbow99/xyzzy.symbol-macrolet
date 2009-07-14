OVERVIEW
============
common-lisp の symbol-macrolet です。

INSTALL
==========
NOTE: NIY: NetInstaller からインストールしたなら以下は不要です。
1. `*load-path*` のどっかに symbol-macrolet.l を置きます
2. OPTIONAL: byte-compile します

「XXXって拡張をインストールしたら symbol-macrolet が必要とか言われた！！」という人はここまでやれば万事オッケーです。


USAGE (for developer)
======================
NOTE: ここは主に symbol-macrolet を使って拡張/lisp を書く人向け

まず require します。
    (require "symbol-macrolet")

symbol-macrolet: (_symbol_ _expansion_)* _declaration*_ _form*_
- _symbol_: [symbol] symbol-macro の名前
- _expansion_: [form] symbol-macro の展開形
- _declaration_: (declare ...)
   - xyzzy で使える declare は special のみですが、special で symbol-macro の名前を
     指定するとエラーを吐くので、つまり使い道はありません。
- _form_: [form]
_form*_ 内で **symbol-macro** として指定された _symbol_ が（変数として）参照される部分が、_expansion_ に置換された後、評価されます。
- 参考:[CLHS: Special Operator SYMBOL-MACROLET](http://www.lispworks.com/documentation/lw50/CLHS/Body/s_symbol.htm#symbol-macrolet)

EXAMPLE
==========
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

about package
--------------
symbol-macrolet.l は macro symbol-macrolet を lisp package から export しているので、ほとんどの場合は気にする必要ないです。

それとは別に、いくつか関数を定義したりするのに "symbol-macrolet" という package を作っていますが、これは macro symbol-macrolet から symbol-macrolet::expand-form みたいに指定して使ってるだけで、普通は触る必要はないです。

