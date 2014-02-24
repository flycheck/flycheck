# -*- coding: utf-8 -*-
# Copyright (c) 2014 Sebastian Wiesner <lunaryorn@gmail.com>

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


sys.path.append(os.path.dirname(os.path.abspath(__file__)))

# Whether we are building on ReadTheDocs or not
ON_RTD = os.environ.get('READTHEDOCS', None) == 'True'

needs_sphinx = '1.2'

extensions = ['sphinx.ext.extlinks',
              'eldomain', 'texinfo', 'issues', 'flycheck']
default_role = 'code'
primary_domain = 'el'

source_suffix = '.rst'
master_doc = 'index'

def cask_version():
    version_re = re.compile('^;; Version: (?P<version>.*)$')
    doc_directory = os.path.abspath(os.path.dirname(__file__))
    flycheck = os.path.join(doc_directory, os.pardir, 'flycheck.el')
    with open(flycheck) as source:
        for line in source:
            match = version_re.match(line)
            if match:
                return match.group('version')
    raise ValueError('Failed to extract the version')

project = u'Flycheck'
copyright = u'2014, Sebastian Wiesner'
release = cask_version()
version = release.split('-')[0]

# Exclude the build directory
exclude_patterns = ['_build']
templates_path = ['_templates']

pygments_style = 'emacs'

html_title = '{0} {1}'.format(project, release)
html_theme = 'nature'
html_sidebars = {'**': ['info.html',
                        'localtoc.html',
                        'relations.html',
                        # For RTD, we point to the source on Github
                        'rtdsourcelink.html' if ON_RTD else 'sourcelink.html',
                        'searchbox.html']}

texinfo_documents = [
  ('manual/index', 'flycheck', u'Flycheck Documentation',
   u'Sebastian Wiesner', 'flycheck',
   'On the fly syntax checking (aka \"flymake done right\").',
   'Emacs'),
]

# Restore standard formatting of emphasis, as by
# http://sphinx-doc.org/faq.html#notes
texinfo_elements = {'preamble': """
@definfoenclose strong,*,*
@definfoenclose emph,_,_
"""}

extlinks = {'github': ('https://github.com/flycheck/flycheck/%s', None),
            'issue': ('https://github.com/flycheck/flycheck/issues/%s', '#')}

emacs_lisp_load_path = os.path.join(
    os.path.dirname(os.path.abspath(__file__)), os.pardir)
