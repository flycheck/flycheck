# -*- coding: utf-8 -*-
# Copyright (c) 2014 Sebastian Wiesner <swiesner@lunaryorn.com>

# Permission is hereby granted, free of charge, to any person obtaining a copy
# of this software and associated documentation files (the "Software"), to deal
# in the Software without restriction, including without limitation the rights
# to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
# copies of the Software, and to permit persons to whom the Software is
# furnished to do so, subject to the following conditions:

# The above copyright notice and this permission notice shall be included in
# all copies or substantial portions of the Software.

# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
# IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
# FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
# AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
# LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
# OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
# SOFTWARE.

import re
import os
import sys

SOURCE_DIR = os.path.dirname(os.path.abspath(__file__))
sys.path.append(SOURCE_DIR)

# Whether we are building on ReadTheDocs or not
ON_RTD = os.environ.get('READTHEDOCS', None) == 'True'

needs_sphinx = '1.2'

extensions = ['sphinx.ext.extlinks', 'sphinxcontrib.emacs',
              'flycheck', 'issues']
default_role = 'code'
primary_domain = 'el'

source_suffix = '.rst'
master_doc = 'index'

def flycheck_version():
    version_re = re.compile('^;; Version: (?P<version>.*)$')
    flycheck = os.path.join(SOURCE_DIR, os.pardir, 'flycheck.el')
    with open(flycheck) as source:
        for line in source:
            match = version_re.match(line)
            if match:
                return match.group('version')
    raise ValueError('Failed to extract the version')

project = u'Flycheck'
copyright = u'2014, Sebastian Wiesner'
release = flycheck_version()
version = release.split('-')[0]

pygments_style = 'emacs'

nitpicky = True
# Do not warn about missing references to built-in Emacs functions
nitpick_ignore =[
    ('el:function', 'symbolp'),
    ('el:function', 'stringp'),
    ('el:function', 'numberp'),
    ('el:function', 'integerp'),
    ('el:function', 'booleanp'),
    ('el:option', 'icomplete-mode'),
    # This is a markup artifact
    ('el:variable', 'flycheck-CHECKER-executable'),
]

linkcheck_ignore = [
    r'^https://help.github.com/.*$', # Gives 404 for some strange reason
    r'^http://www.erlang.org/$',     # Gives 500 during linkcheck
    # We know that our issue references are correct, so don't waste time on
    # checking them
    r'^https://github.com/flycheck/flycheck/issues/.*$',
]

html_title = '{0} {1}'.format(project, release)
html_logo = 'images/logo.png'
html_favicon = 'images/favicon.ico'

texinfo_documents = [
  ('index', 'flycheck', 'Flycheck', u'Sebastian Wiesner',
   'flycheck', 'On the fly syntax checking for GNU Emacs',
   'Emacs', True),
]

# Restore standard formatting of emphasis, as by
# http://sphinx-doc.org/faq.html#notes
texinfo_elements = {
    'preamble': """
@definfoenclose strong,*,*
@definfoenclose emph,_,_
""",
    'copying': """\
This manual is for Flycheck version {release}.

Copyright @copyright{{}} {copyright}

@quotation
Permission is granted to copy, distribute and/or modify this documentation under
the terms of the GNU Free Documentation License, Version 1.3 or any later
version published by the Free Software Foundation; with no Invariant Sections,
no Front-Cover Texts, and no Back-Cover Texts.  A copy of the license is
included in the section entitled ``GNU Free Documentation License.''.

Alternatively, you may copy, distribute and/or modify this documentation under
the terms of the Creative Commons Attribution-ShareAlike 4.0 International
Public License.  A copy of the license can be obtained at
@uref{{https://creativecommons.org/licenses/by-sa/4.0/legalcode}}.
@end quotation
""".format(release=release, copyright=copyright)}

emacs_lisp_load_path = [os.path.abspath(os.path.join(SOURCE_DIR, os.pardir))]

info_xref = {'ert': 'http://www.gnu.org/software/emacs/manual/html_node/ert/'}
