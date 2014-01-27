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
from sphinx import addnodes
from sphinx.roles import XRefRole
from docutils import utils, nodes

sys.path.append(os.path.dirname(os.path.abspath(__file__)))

needs_sphinx = '1.2'

extensions = ['eldomain']
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

pygments_style = 'emacs'

html_theme = 'nature'

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


def parse_syntax_checker(env, signature, node):
    node += addnodes.desc_annotation('Syntax checker ', 'Syntax checker ')
    node += addnodes.desc_name(signature, signature)
    return signature


class InfoNodeXRefRole(XRefRole):

    def process_link(self, env, refnode, has_explicit_title, title, target):
        # Normalize whitespace in info node targets
        target = re.sub(r'\s+', ' ', target, flags=re.UNICODE)
        refnode['has_explicit_title'] = has_explicit_title
        return title, target


INFO_RE = re.compile(r'^(?P<node>.+?)\((?P<manual>.+)\)$')


INFO_MANUAL_URLS = {
    'emacs': 'http://www.gnu.org/software/emacs/manual/html_node/emacs/{node}.html#{node}',
    'elisp': 'http://www.gnu.org/software/emacs/manual/html_node/elisp/{node}.html#{node}',
    'cl': 'http://www.gnu.org/software/emacs/manual/html_node/cl/{node}.html#{node}',
}


class infonode_reference(nodes.reference):
    pass


def resolve_info_references(app, env, refnode, contnode):
    if refnode['reftype'] != 'infonode':
        return None

    target = refnode['reftarget']
    match = INFO_RE.match(target)
    if not match:
        app.warn('Invalid info target: {0}'.format(target))
        return contnode

    manual = match.group('manual')
    node = match.group('node')

    if app.builder.format == 'texinfo':
        reference = infonode_reference('', '')
        reference['refnode'] = node
        reference['refmanual'] = manual
        reference['has_explicit_title'] = refnode['has_explicit_title']
        reference.append(contnode)
        return reference
    else:
        base_uri = INFO_MANUAL_URLS.get(manual)
        if not base_uri:
            app.warn('Cannot resolve info manual {0}'.format(manual))
            return contnode
        else:
            reference = nodes.reference('', '', internal=False)
            reference['refuri'] = base_uri.format(node=node.replace(' ', '-'))
            reference['reftitle'] = target
            reference.append(contnode)
            return reference


def visit_infonode_reference(self, node):
    infonode = node['refnode']
    manual = node['refmanual']

    name = node.astext().strip() if node['has_explicit_title'] else ''

    self.body.append('@ref{{{node},,{name},{manual}}}'.format(
        node=infonode,name=self.escape_menu(name),manual=manual))

    # Skip the node body
    raise nodes.SkipNode


def setup(app):
    app.add_object_type('checker-property', 'checkprop',
                        indextemplate='pair: %s; Syntax checker property',
                        parse_node=parse_checker_property,
                        objname='Syntax checker property')
    app.add_object_type('syntax-checker', 'checker',
                        indextemplate='pair: %s; Syntax checker',
                        parse_node=parse_syntax_checker,
                        objname='Syntax checker')
    app.add_role('infonode', InfoNodeXRefRole(innernodeclass=nodes.emphasis))
    app.add_node(infonode_reference, texinfo=(visit_infonode_reference, None))
    app.connect('missing-reference', resolve_info_references)
