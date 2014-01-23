# -*- coding: utf-8 -*-
# This file is not part of GNU Emacs.

# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.

# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.

# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.

import re
import os
import sys
from sphinx import addnodes
from docutils import utils, nodes

sys.path.append(os.path.dirname(os.path.abspath(__file__)))

needs_sphinx = '1.2'

extensions = ['eldomain']
default_role = 'code'

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

pygments_style = 'emacs'

html_theme = 'agogo'


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

PROPERTY_PARAMETER_RE = re.compile('{([^}]+)}')

def parse_checker_property(env, signature, node):
    if not signature.startswith(':'):
        raise ValueError('Invalid signature: ' + signature)

    keyword, value = signature.split(' ', 1)
    node += addnodes.desc_name(keyword, keyword)
    node += nodes.Text(' ', ' ')

    # Taken from sphinx.roles.emph_literal_role
    value = utils.unescape(value)
    pos = 0
    for match in PROPERTY_PARAMETER_RE.finditer(value):
        if match.start() > pos:
            text = value[pos:match.start()]
            node += nodes.Text(text, text)
        param = match.group(1)
        node += nodes.emphasis(param, param)
        pos = match.end()
    if pos < len(value):
        node += nodes.Text(value[pos:], value[pos:])

    return keyword.strip()


def setup(app):
    app.add_object_type('checker-property', 'checkprop',
                        indextemplate='pair: %s; Syntax checker property',
                        parse_node=parse_checker_property,
                        objname='Syntax checker property')
    app.add_object_type('syntax-checker', 'syntax-checker',
                        indextemplate='pair: %s, Syntax checker',
                        objname='Syntax checker')
