OVERVIEW
============
common-lisp �� symbol-macrolet �ł��B

INSTALL
==========
NOTE: NIY: NetInstaller ����C���X�g�[�������Ȃ�ȉ��͕s�v�ł��B
1. `*load-path*` �̂ǂ����� symbol-macrolet.l ��u���܂�
2. OPTIONAL: byte-compile ���܂�

�uXXX���Ċg�����C���X�g�[�������� symbol-macrolet ���K�v�Ƃ�����ꂽ�I�I�v�Ƃ����l�͂����܂ł��Ζ����I�b�P�[�ł��B


USAGE (for developer)
======================
NOTE: �����͎�� symbol-macrolet ���g���Ċg��/lisp �������l����

�܂� require ���܂��B
    (require "symbol-macrolet")

symbol-macrolet: (_symbol_ _expansion_)* _declaration*_ _form*_
- _symbol_: [symbol] symbol-macro �̖��O
- _expansion_: [form] symbol-macro �̓W�J�`
- _declaration_: (declare ...)
   - xyzzy �Ŏg���� declare �� special �݂̂ł����Aspecial �� symbol-macro �̖��O��
     �w�肷��ƃG���[��f���̂ŁA�܂�g�����͂���܂���B
- _form_: [form]
_form*_ ���� **symbol-macro** �Ƃ��Ďw�肳�ꂽ _symbol_ ���i�ϐ��Ƃ��āj�Q�Ƃ���镔�����A_expansion_ �ɒu�����ꂽ��A�]������܂��B
- �Q�l:[CLHS: Special Operator SYMBOL-MACROLET](http://www.lispworks.com/documentation/lw50/CLHS/Body/s_symbol.htm#symbol-macrolet)

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
symbol-macrolet.l �� macro symbol-macrolet �� lisp package ���� export ���Ă���̂ŁA�قƂ�ǂ̏ꍇ�͋C�ɂ���K�v�Ȃ��ł��B

����Ƃ͕ʂɁA�������֐����`�����肷��̂� "symbol-macrolet" �Ƃ��� package ������Ă��܂����A����� macro symbol-macrolet ���� symbol-macrolet::expand-form �݂����Ɏw�肵�Ďg���Ă邾���ŁA���ʂ͐G��K�v�͂Ȃ��ł��B

