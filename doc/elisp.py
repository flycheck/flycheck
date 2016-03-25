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


from collections import namedtuple
from sphinx import addnodes
from sphinx.roles import XRefRole
from sphinx.domains import Domain, ObjType
from sphinx.util.nodes import make_refnode
from sphinx.directives import ObjectDescription


def make_target(prefix, name):
    return 'el-{prefix}-{name}'.format(prefix=prefix, name=name)

class Cell(namedtuple('Cell', 'objtype docname')):
    pass

class EmacsLispSymbol(ObjectDescription):
    pass


class EmacsLispVariable(EmacsLispSymbol):

    label_for_objtype = {
        'option': 'User option',
        'hook': 'Hook',
        'variable': 'Variable',
        'hook': 'Hook'
    }

    @property
    def objtype_label(self):
        return self.label_for_objtype[self.objtype]

    def handle_signature(self, signature, signode):
        label = self.objtype_label + ' '
        signode += addnodes.desc_annotation(label, label)
        signode += addnodes.desc_addname(signature, signature)
        return signature

    def add_target_and_index(self, name, sig, signode):
        target_name = make_target('variable', name)
        if target_name not in self.state.document.ids:
            signode['names'].append(target_name)
            signode['ids'].append(target_name)
            signode['first'] = (not self.names)
            self.state.document.note_explicit_target(signode)

            obarray = self.env.domaindata['el']['obarray']
            symbol = obarray.setdefault(name, {})
            if 'variable' in symbol:
                self.state_machine.reporter.warning(
                    'duplicate description of %s %s, ' % (self.objtype, name) +
                    'other instance in ' +
                    self.env.doc2path(symbol['variable'].docname),
                    line=self.lineno)
            symbol['variable'] = Cell(self.objtype, self.env.docname)

        index_text = '{name} (Emacs Lisp {label})'.format(
            name=name, label=self.objtype_label.lower())
        self.indexnode['entries'].append(
            ('single', index_text, target_name, ''))


class EmacsLispDomain(Domain):
    """A domain to document Emacs Lisp code."""

    name = 'el'
    label = 'Emacs Lisp'

    object_types = {
        # Types for user-facing options and commands
        'command': ObjType('command', 'command'),
        'option': ObjType('option', 'option'),
        'face': ObjType('face', 'face'),
        # Object types for code
        'function': ObjType('function', 'function'),
        'macro': ObjType('macro', 'macro'),
        'variable': ObjType('variable', 'variable'),
        'constant': ObjType('constant', 'constant'),
        'hook': ObjType('hook', 'hook'),
    }
    directives = {
        'option': EmacsLispVariable,
        'variable': EmacsLispVariable,
        'constant': EmacsLispVariable,
        'hook': EmacsLispVariable
    }
    roles = {
        'function': XRefRole(),
        'macro': XRefRole(),
        'command': XRefRole(),
        'variable': XRefRole(),
        'constant': XRefRole(),
        'option': XRefRole(),
        'hook': XRefRole(),
        'face': XRefRole(),
    }

    data_version = 0
    initial_data = {
       'obarray': {}
    }

    def clear_doc(self, docname):
        for symbol in self.data['obarray'].values():
            for cell in symbol.keys():
                if docname == cell.docname:
                    del symbol[cell]

    def resolve_xref(self, env, fromdocname, builder,
                     typ, target, node, contnode):
        pass

    def resolve_any_xref(self, env, fromdocname, builder,
                         target, node, contnode):
        pass

    def get_objects(self):
        return []


def setup(app):
    app.add_domain(EmacsLispDomain)
