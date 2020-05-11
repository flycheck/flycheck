# Copyright (C) 2017, 2018 Flycheck contributors
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
import os
from pathlib import Path
from docutils import nodes
from docutils.statemachine import ViewList
from docutils.transforms import Transform
from docutils.parsers.rst import Directive, directives
from sphinx import addnodes
from sphinx.util.nodes import set_source_info, process_index_entry
from sphinx.util import logging

logger = logging.getLogger(__name__)

sys.path.append(str(Path(__file__).parent))

ON_RTD = os.environ.get('READTHEDOCS', None) == 'True'

needs_sphinx = '1.3'
extensions = [
    'sphinx.ext.intersphinx',
    'sphinx.ext.extlinks',
    'sphinx.ext.todo',
    # Domain for Emacs Lisp
    'elisp',
    # Cross-references to info nodes
    'info'
]

# Project metadata
project = 'Flycheck'
copyright = ' 2014-2017, Sebastian Wiesner and Flycheck contributors'
author = 'Sebastian Wiesner'


def read_version():
    """Extract version number from ``flycheck.el`` and return it as string."""
    version_pattern = re.compile(r'^;;\s*Version:\s+(\d.+)$', re.MULTILINE)
    flycheck = Path(__file__).resolve().parent.parent.joinpath('flycheck.el')
    with flycheck.open(encoding='utf-8') as source:
        match = version_pattern.search(source.read())
        if match:
            return match.group(1)
        else:
            raise ValueError('Failed to parse Flycheck version from '
                             'Version: of flycheck.el')


def read_minimum_emacs_version():
    """Extract minimum Emacs version from ``flycheck.el``."""
    version_pattern = re.compile(
        r'^;; Package-Requires:.*\(emacs\s*"([^"]+)"\).*$', re.MULTILINE)
    flycheck = Path(__file__).resolve().parent.parent.joinpath('flycheck.el')
    with flycheck.open(encoding='utf-8') as source:
        match = version_pattern.search(source.read())
        if match:
            return match.group(1)
        else:
            raise ValueError('Vailed to parse minimum Emacs version from '
                             'Package-Requires of flycheck.el!')


release = read_version()
version = '.'.join(release.split('.')[:2])

# Source settings
source_suffix = '.rst'
master_doc = 'index'

rst_prolog = """\
.. role:: elisp(code)
   :language: elisp

.. |min-emacs| replace:: {emacs_version}+
""".format(emacs_version=read_minimum_emacs_version())

# Build settings
exclude_patterns = ['_build']
default_role = 'any'
primary_domain = 'el'
templates_path = ['_templates']

# The name of the Pygments (syntax highlighting) style to use.
pygments_style = 'sphinx'

# Warn about all undefined references, but exclude references to built-in
# symbols which we don't document here.
# TODO: Resolve built-in symbols to the Emacs Lisp references?
nitpicky = True
nitpick_ignore = [
    ('any', 'default-directory'),
    ('any', 'package-initialize'),
    ('any', 'package-archives'),
    ('any', 'user-init-file'),
    ('any', 'user-emacs-directory'),
    ('any', 'check-declare-file'),
    ('any', 'declare-function'),
    ('any', 'exec-path'),
    ('any', 'sh-shell'),
    ('any', 'rx'),
]

# HTML settings
html_theme = 'alabaster'
html_theme_options = {
    'logo': 'logo.png',
    'logo_name': False,
    'description': 'Syntax checking for GNU Emacs',
    'github_user': 'flycheck',
    'github_repo': 'flycheck',
    'github_type': 'star',
    'github_banner': True,
    'travis_button': False,
}
html_sidebars = {
    '**': [
        'about.html',
        'tables.html',
        'navigation.html',
        'relations.html',
        'searchbox.html',
    ]
}
html_static_path = ['_static']
html_favicon = '_static/favicon.ico'

# Ignore localhost when checking links
linkcheck_ignore = [r'http://localhost:\d+/?']

# Cross-reference remote Sphinx sites
intersphinx_mapping = {
    'python': ('https://docs.python.org/3.5', None)
}

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
        index_specs = ['pair: {}; language'.format(lang)
                       for lang in indexed_languages.splitlines()]

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

        self.state.nested_parse(self.content, self.content_offset, sectionnode)

        return [indexnode, targetnode, sectionnode]


class SyntaxCheckerConfigurationFile(Directive):

    required_arguments = 1
    final_argument_whitespace = True

    def run(self):
        option = self.arguments[0]

        wrapper = nodes.paragraph()
        docname = self.state.document.settings.env.docname
        template = ViewList("""\
.. index:: single: Configuration file; {0}

.. el:defcustom:: {0}

   Configuration file for this syntax checker.  See
   :ref:`flycheck-checker-config-files`.
""".format(option).splitlines(), docname)
        self.state.nested_parse(template, self.content_offset, wrapper)

        return wrapper.children.copy()


class IssueReferences(Transform):

    ISSUE_PATTERN = re.compile(r'\[GH-(\d+)\]')
    ISSUE_URL_TEMPLATE = 'https://github.com/flycheck/flycheck/issues/{}'

    default_priority = 999

    def apply(self):
        docname = self.document.settings.env.docname
        if docname != 'changes':
            # Only transform issue references in changelo
            return

        for node in self.document.traverse(nodes.Text):
            parent = node.parent
            new_nodes = []
            last_issue_ref_end = 0
            text = str(node)
            for match in self.ISSUE_PATTERN.finditer(text):
                # Extract the text between the last issue reference and the
                # current issue reference and put it into a new text node
                head = text[last_issue_ref_end:match.start()]
                if head:
                    new_nodes.append(nodes.Text(head))
                # Adjust the position of the last issue reference in the
                # text
                last_issue_ref_end = match.end()
                # Extract the issue text and the issue number
                issuetext = match.group(0)
                issue_id = match.group(1)
                # Turn the issue into a proper reference
                refnode = nodes.reference()
                refnode['refuri'] = self.ISSUE_URL_TEMPLATE.format(issue_id)
                refnode.append(nodes.inline(
                    issuetext, issuetext, classes=['xref', 'issue']))
                new_nodes.append(refnode)

            # No issue references were found, move on to the next node
            if not new_nodes:
                continue
            # Extract the remaining text after the last issue reference
            tail = text[last_issue_ref_end:]
            if tail:
                new_nodes.append(nodes.Text(tail))
            parent.replace(node, new_nodes)


def build_offline_html(app):
    from sphinx.builders.html import StandaloneHTMLBuilder
    build_standalone = isinstance(app.builder, StandaloneHTMLBuilder)
    if app.config.flycheck_offline_html and build_standalone:
        logger.info(
            'Building offline documentation without external resources!')
        app.builder.theme_options['github_banner'] = 'false'
        app.builder.theme_options['github_button'] = 'false'


def add_offline_to_context(app, _pagename, _templatename, context, _doctree):
    # Expose offline setting in HTML context
    context['flycheck_offline_html'] = app.config.flycheck_offline_html


def setup(app):
    app.add_object_type('syntax-checker', 'checker',
                        'pair: %s; Syntax checker')
    app.add_directive('supported-language', SupportedLanguage)
    app.add_directive('syntax-checker-config-file',
                      SyntaxCheckerConfigurationFile)
    app.add_transform(IssueReferences)
    # Build offline HTML that loads no external resources, for use in 3rd party
    # packages, see https://github.com/flycheck/flycheck/issues/999
    app.add_config_value('flycheck_offline_html', False, 'html')
    app.connect('builder-inited', build_offline_html)
    app.connect('html-page-context', add_offline_to_context)
