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

from docutils import nodes
from docutils.transforms import Transform
from sphinx.roles import XRefRole
from sphinx.domains import ObjType
from sphinxcontrib.emacs import register_interpreter_function
from sphinxcontrib.emacs.domain import EmacsLispDomain
from sphinxcontrib.emacs.directives.desc import EmacsLispSymbol


def flycheck_define_checker(interpreter, context, _function, name, docstring,
                            *_rest):
    symbol = interpreter.intern_in_scope(name, 'flycheck-checker', context)
    symbol.properties['flycheck-documentation'] = docstring


register_interpreter_function('flycheck-define-checker',
                              flycheck_define_checker)


# Register our object type
EmacsLispDomain.object_types['flyc-checker'] = ObjType(
    'Flycheck syntax checker', 'flyc-checker', scope='flycheck-checker')


class FlycheckChecker(EmacsLispSymbol):
    docstring_property = 'flycheck-documentation'


FLYCHECK_SUBSTITUTIONS = ('#flycheck-languages', '#flycheck-checkers')


class flycheck_info(nodes.inline):
    pass


class FlycheckSubstitutions(Transform):

    default_priority = 100

    def apply(self):
        for node in self.document.traverse(nodes.substitution_reference):
            if node['refname'] in FLYCHECK_SUBSTITUTIONS:
                new_node = flycheck_info()
                new_node['flycheck-info'] = node['refname']
                node.replace_self(new_node)


def count_languages(app, doctree):
    if app.env.docname == 'manual/languages':
        top_level = doctree[doctree.first_child_matching_class(nodes.section)]
        # The number of languages is the number of second-level sections in the
        # languages document
        languages = sum(1 for n in top_level if isinstance(n, nodes.section))
        data = app.env.domaindata[EmacsLispDomain.name]
        data.setdefault('flycheck', {})['#languages'] = languages


def substitute_flycheck_info(app, doctree, docname):
    data = app.env.domaindata[EmacsLispDomain.name]
    for info in doctree.traverse(flycheck_info):
        kind = info['flycheck-info']
        if kind == '#flycheck-languages':
            value = str(data['flycheck']['#languages'])
        elif kind == '#flycheck-checkers':
            value = str(sum(1 for symbol, scopes in data['namespace'].iteritems()
                            if 'flycheck-checker' in scopes))
        else:
            raise ValueError('Unknown kind ' + kind)
        info.replace_self(nodes.Text(value, value))


def setup(app):
    app.add_role_to_domain('el', 'flyc-checker', XRefRole())
    app.add_directive_to_domain('el', 'flyc-checker', FlycheckChecker)
    app.add_transform(FlycheckSubstitutions)
    app.connect('doctree-read', count_languages)
    app.connect('doctree-resolved', substitute_flycheck_info)
