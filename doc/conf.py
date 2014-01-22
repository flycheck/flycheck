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

needs_sphinx = '1.2'

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
