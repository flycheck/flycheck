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
from collections import namedtuple

from sexpdata import Symbol
from docutils import nodes
from docutils.transforms import Transform
from sphinx import addnodes
from sphinx.roles import XRefRole
from sphinx.domains import ObjType
from sphinx.util.nodes import set_role_source_info
from sphinxcontrib.emacs import register_interpreter_function
from sphinxcontrib.emacs.domain import EmacsLispDomain
from sphinxcontrib.emacs.directives.desc import EmacsLispSymbol
from sphinxcontrib.emacs.lisp.util import parse_plist, parse_cons_cell


class NextChecker(namedtuple('_NextChecker', 'checker maximum_level')):

    @classmethod
    def from_sexp(cls, sexp):
        if isinstance(sexp, Symbol):
            return cls(checker=sexp.value(), maximum_level=None)
        else:
            level, checker = parse_cons_cell(sexp)
            if not isinstance(checker, Symbol):
                raise ValueError('Invalid checker: ' + repr(checker))
            if not isinstance(level, Symbol):
                raise ValueError('Invalid level: ' + repr(level))
            return cls(checker=checker.value(),
                       maximum_level=level.value())


def parse_next_checkers(properties):
    next_checkers = properties.get(':next-checkers', [])
    if isinstance(next_checkers, Symbol):
        next_checkers = [next_checkers]
    return map(NextChecker.from_sexp, next_checkers)


def flycheck_define_checker(interpreter, context, _function, name, docstring,
                            *properties):
    symbol = interpreter.intern_in_scope(name, 'flycheck-checker', context)
    symbol.properties['flycheck-documentation'] = docstring
    symbol.properties['flycheck-next-checkers'] = parse_next_checkers(
        parse_plist(properties))


def flycheck_def_option_var(interpreter, context, function,
                            name, initial_value, checker, docstring, *rest):
    # Let the standard variable command do the heavy lifting
    interpreter.defvar(
        context, 'defcustom', name, initial_value, docstring, *rest)
    symbol = interpreter.env.intern(name)
    checker = interpreter.env.intern(checker)
    checker.properties.setdefault('flycheck-options', set()).add(symbol)
    # Our variables are always buffer-local
    symbol.properties['buffer-local'] = True
    # Add the checker to the documentation
    doc = symbol.properties['variable-documentation']
    symbol.properties['variable-documentation'] = """{0}

This variable is an option for the syntax checker `{1}'""".format(
    doc, checker.name)


def flycheck_def_config_file_var(interpreter, context, function,
                                 name, checker, filename, *rest):
    interpreter.defvar(context, 'defcustom', name, filename, '', *rest)
    symbol = interpreter.env.intern(name)
    checker = interpreter.env.intern(checker)
    checker.properties['flycheck-config-file'] = symbol
    symbol.properties['buffer-local'] = True
    doc = "Configuration file for `{0}'".format(checker.name)
    symbol.properties['variable-documentation'] = doc


register_interpreter_function('flycheck-define-checker',
                              flycheck_define_checker)
register_interpreter_function('flycheck-def-option-var',
                              flycheck_def_option_var)
register_interpreter_function('flycheck-def-config-file-var',
                              flycheck_def_config_file_var)


# Register our object type
EmacsLispDomain.object_types['flyc-checker'] = ObjType(
    'Flycheck syntax checker', 'flyc-checker', scope='flycheck-checker')


class FlycheckChecker(EmacsLispSymbol):
    docstring_property = 'flycheck-documentation'

    def make_checker_chaining(self):
        symbol = self.lookup_auto_symbol()
        if not symbol:
            return
        next_checkers = symbol.properties.get('flycheck-next-checkers')
        if not next_checkers:
            return
        title = nodes.title('', 'Chained syntax checkers')
        intro = nodes.paragraph()
        intro += nodes.Text('The following syntax checkers are ')
        chained = addnodes.pending_xref(
            'chained', reftype='term', refdomain='std', reftarget='chaining',
            refwarn=True, refexplicit=True, reftitle='chaining',
            refdoc=self.env.docname)
        chained += nodes.emphasis('', 'chained')
        intro += chained
        intro += nodes.Text(' after this syntax checker:')
        checker_list = nodes.enumerated_list()
        para = nodes.paragraph()
        para += checker_list
        outro = nodes.paragraph()
        outro += nodes.Text('The ')
        outro += nodes.strong('', 'first')
        outro += nodes.Text(' suitable syntax checker is used.')
        chaining = nodes.admonition(
            '', title, intro, para, outro,
            classes=['note', 'el-flycheck-checker-chaining'])
        for next_checker in next_checkers:
            xref = addnodes.pending_xref(
                next_checker.checker,
                reftype='flyc-checker', refdomain='el',
                refexplicit=False, reftarget=next_checker.checker,
                refwarn=False, refdoc=self.env.docname)
            set_role_source_info(self.state.inliner, self.lineno, xref)
            xref += nodes.literal('', next_checker.checker,
                                  classes=['xref', 'el', 'el-flyc-checker'])
            para = nodes.paragraph()
            para += xref
            if next_checker.maximum_level:
                para += nodes.Text(', if there are no errors above level ')
                para += nodes.literal('', next_checker.maximum_level)
            checker_list += nodes.list_item('', para)
        return chaining

    def run(self):
        result_nodes = EmacsLispSymbol.run(self)
        cont_node = result_nodes[-1][-1]

        chaining = self.make_checker_chaining()
        if chaining:
            cont_node.insert(0, chaining)

        return result_nodes


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
    if app.env.docname == 'guide/languages':
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
