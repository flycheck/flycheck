# Copyright (C) 2016 Sebastian Wiesner and Flycheck contributors

# This file is not part of GNU Emacs.

# This program is free software: you can redistribute it and/or modify it under
# the terms of the GNU General Public License as published by the Free Software
# Foundation, either version 3 of the License, or (at your option) any later
# version.

# This program is distributed in the hope that it will be useful, but WITHOUT
# ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
# FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
# details.

# You should have received a copy of the GNU General Public License along with
# this program.  If not, see <http://www.gnu.org/licenses/>.

import re
import sys
from pathlib import Path
from docutils import nodes
from docutils.parsers.rst import Directive, directives
from sphinx import addnodes
from sphinx.util.nodes import set_source_info, process_index_entry

sys.path.append(str(Path(__file__).parent))

needs_sphinx = '1.3'
extensions = ['sphinx.ext.extlinks', 'sphinx.ext.todo', 'elisp']

# Project metadata
project = 'Flycheck'
copyright = ' 2014-2016, Sebastian Wiesner and Flycheck contributors'
author = 'Sebastian Wiesner'


def read_version():
    """Extract version number from ``flycheck.el`` and return it as string."""
    version_pattern = re.compile(r'Version:\s+(\d.+)$')
    flycheck_el = Path(__file__).parent.parent.joinpath('flycheck.el')
    for line in flycheck_el.open(encoding='utf-8'):
        match = version_pattern.search(line)
        if match:
            return match.group(1)

release = read_version()
version = '.'.join(release.split('.')[:2])

# Source settings
source_suffix = '.rst'
master_doc = 'index'

# Build settings
exclude_patterns = ['_build']
# TODO: Choose a default role
#default_role = None
primary_domain = 'el'

# The name of the Pygments (syntax highlighting) style to use.
pygments_style = 'sphinx'

# HTML settings
html_theme = 'alabaster'
html_theme_options = {
    'logo': 'logo.png',
    'logo_name': False,
    'description': 'Syntax checking for GNU Emacs',
    'github_user': 'flycheck',
    'github_repo': 'flycheck',
    'github_banner': True,
}
html_sidebars = {
    'index': [
        'about.html',
        'localtoc.html',
        'relations.html',
        'searchbox.html',
    ],
    '**': [
        'about.html',
        'navigation.html',
        'relations.html',
        'searchbox.html',
    ]
}
html_static_path = ['_static']
html_favicon = '_static/favicon.png'

# Ignore localhost when checking links
linkcheck_ignore = [r'http://localhost:\d+/?']

extlinks = {
    'gh': ('https://github.com/%s', ''),
    'flyc': ('https://github.com/flycheck/%s', '')
}

# While still have work to do :)
# FIXME: Remove when the old Texinfo manual is completed ported
todo_include_todos = True


class SupportedLanguage(Directive):

    required_arguments = 1
    final_argument_whitespace = True
    has_content = True
    option_spec = {
        'index_as': directives.unchanged
    }

    def run(self):
        language = self.arguments[0]

        indexed_languages = self.options.get('index_as') or language
        index_specs = ['pair: {}; language'.format(l)
                       for l in indexed_languages.splitlines()]

        name = nodes.fully_normalize_name(language)
        target = 'language-{}'.format(name)
        targetnode = nodes.target('', '', ids=[target])
        self.state.document.note_explicit_target(targetnode)

        indexnode = addnodes.index()
        indexnode['entries'] = []
        indexnode['inline'] = False
        set_source_info(self, indexnode)
        for spec in index_specs:
            indexnode['entries'].extend(process_index_entry(spec, target))

        sectionnode = nodes.section()
        sectionnode['names'].append(name)

        title, messages = self.state.inline_text(language, self.lineno)
        titlenode = nodes.title(language, '', *title)

        sectionnode += titlenode
        sectionnode += messages
        self.state.document.note_implicit_target(sectionnode, sectionnode)

        self.state.nested_parse(self.content, 0, sectionnode)

        return [indexnode, targetnode, sectionnode]


def setup(app):
    app.add_object_type('syntax-checker', 'checker', 'pair: %s; Syntax checker')
    app.add_directive('supported-language', SupportedLanguage)
