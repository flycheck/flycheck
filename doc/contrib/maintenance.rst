===================
 Maintenance tasks
===================

This document explains various maintenance tasks for Flycheck, that contributors
have to perform under specific circumstances.

Rasterized logo files
=====================

The source of the Flycheck logo is ``flycheck.svg``.  The documentation contains
rasterized versions of this logo at

- ``doc/images/logo.png`` (the logo shown in the documentation and the README)
- ``doc/images/favicon.ico`` (the logo for use as favicon of the documentation)

.. warning::

   Do **not** change these files directly.  They are generated automatically
   from the SVG source.

When changing the Flycheck logo, you need to rebuild these files with:

.. code-block:: console

   make images

You need :program:`convert` from ImageMagick_ and Inkscape_.  Make sure that
``inkscape`` is in ``$PATH``, because ImageMagick's own SVG renderer cannot
properly render the logo.

.. _ImageMagick: http://www.imagemagick.org/
.. _Inkscape: http://www.inkscape.org/
