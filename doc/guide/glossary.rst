==========
 Glossary
==========

.. glossary::

   syntax checker
     A symbol which is defined as syntax checker with
     :macro:`flycheck-define-checker`

   registered syntax checker
     A :term:`syntax checker` available for automatic selection, i.e. that is
     contained in :option:`flycheck-checkers`.

   disabled syntax checker
     A :term:`syntax checker` explicitly excluded from available selection,
     i.e. that is contained in :option:`flycheck-disabled-checkers`.

   enabled syntax checker
     A :term:`registered syntax checker` which is not a :term:`disabled syntax
     checker`.

   suitable syntax checker
      A :term:`syntax checker` which can be used to check the current buffer.
      Precisely, a syntax checker which

      - which can be used for the current major mode,
      - and whose predicate succeeds,
      - and whose syntax checker tool exists.

   chaining
      In some languages, multiple :term:`syntax checkers <syntax checker>` can
      be applied to a buffer.  In such a case, each syntax checker specifies
      which checkers are to be used after it and when.  The result is a “syntax
      checker chain”, specifying all syntax checkers to be applied to the
      buffer.

      For instance, Haskell buffers are first checked with GHC for syntax and
      type errors, and then with `hlint` for idiomatic and semantic mistakes.
      Hence, the syntax checker chain consists of :flyc-checker:`haskell-ghc`
      and :flyc-checker:`haskell-hlint`.
