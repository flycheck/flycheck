==========
 Glossary
==========

The glossary explains most of the special terms we use in this documentation.
some of these are originally explained in the `Emacs manual`_ or the `Emacs Lisp
reference`_, but we reproduce them here for convenience.

.. _Emacs manual: https://www.gnu.org/software/emacs/manual/html_node/emacs/index.html
.. _Emacs Lisp reference: https://www.gnu.org/software/emacs/manual/html_node/elisp/index.html

.. glossary::

   init file
   user init file

      Your main Emacs configuration file.  It’s typically located in
      :file:`$HOME/.emacs` (old style) or :file:`$HOME/.emacs.d/init.el` (new
      style, recommended).  To find out the actual path to your init file of
      your Emacs session inspect the value of the variable `user-init-file` with
      :kbd:`C-h v user-init-file`.  You can visit it directly with :kbd:`M-:
      (find-file user-init-file)`.
