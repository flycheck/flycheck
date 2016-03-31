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


def make_target(cell, name):
    """Create a target name from ``cell`` and ``name``.

    ``cell`` is the name of a symbol cell, and ``name`` is a symbol name, both
    as strings.

    The target names are used as cross-reference targets for Sphinx.

    """
    return 'el-{cell}-{name}'.format(cell=cell, name=name)


class Cell(namedtuple('Cell', 'objtype docname')):
    """A cell in a symbol.

    A cell holds the object type and the document name of the description for
    the cell.

    Cell objects are used within symbol entries in the domain data.

    """
    pass


class EmacsLispSymbol(ObjectDescription):
    """An abstract base class for directives documenting symbols.

    Provide target and index generation and registration of documented symbols
    within the domain data.

    Deriving classes must have a ``cell`` attribute which refers to the cell
    the documentation goes in, and a ``label`` attribute which provides a
    human-readable name for what is documented, used in the index entry.

    """

    def add_target_and_index(self, name, sig, signode):
        target_name = make_target(self.cell, name)
        if target_name not in self.state.document.ids:
            signode['names'].append(target_name)
            signode['ids'].append(target_name)
            signode['first'] = (not self.names)
            self.state.document.note_explicit_target(signode)

            obarray = self.env.domaindata['el']['obarray']
            symbol = obarray.setdefault(name, {})
            if self.cell in symbol:
                self.state_machine.reporter.warning(
                    'duplicate description of %s %s, ' % (self.objtype, name) +
                    'other instance in ' +
                    self.env.doc2path(symbol[self.cell].docname),
                    line=self.lineno)
            symbol[self.cell] = Cell(self.objtype, self.env.docname)

        index_text = '{name}; {label}'.format(
             name=name, label=self.label)
        self.indexnode['entries'].append(
            ('pair', index_text, target_name, ''))


class EmacsLispVariable(EmacsLispSymbol):
    """A directive to document the variable cell of symbols.

    Provides support for user options, hooks, variables and constants.

    """

    cell = 'variable'
    label_for_objtype = {
        'option': 'User option',
        'constant': 'Constant',
        'variable': 'Variable',
        'hook': 'Hook'
    }

    @property
    def label(self):
        """The label for the documented object type."""
        return self.label_for_objtype[self.objtype]

    def handle_signature(self, signature, signode):
        """Create nodes in ``signode`` for the ``signature``.

        ``signode`` is a docutils node to which to add the nodes, and
        ``signature`` is the symbol name.

        Add the object type label before the symbol name and return
        ``signature``.

        """
        label = self.label + ' '
        signode += addnodes.desc_annotation(label, label)
        signode += addnodes.desc_name(signature, signature)
        return signature


class EmacsLispDomain(Domain):
    """A domain to document Emacs Lisp code."""

    name = 'el'
    label = 'Emacs Lisp'

    object_types = {
        # Types for user-facing options and commands
        'command': ObjType('command', 'command', cell='command'),
        'option': ObjType('option', 'option', cell='variable'),
        'face': ObjType('face', 'face', cell='face'),
        # Object types for code
        'function': ObjType('function', 'function', cell='function'),
        'macro': ObjType('macro', 'macro', cell='function'),
        'variable': ObjType('variable', 'variable', cell='variable'),
        'constant': ObjType('constant', 'constant', cell='variable'),
        'hook': ObjType('hook', 'hook', cell='variable'),
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
        """Clear all cells documented ``docname``."""
        for symbol in self.data['obarray'].values():
            for cell in list(symbol.keys()):
                if docname == symbol[cell].docname:
                    del symbol[cell]

    def resolve_xref(self, env, fromdocname, builder,
                     objtype, target, node, contnode):
        """Resolve a cross reference to ``target``."""
        cell = self.object_types[objtype].attrs['cell']
        symbol = self.data['obarray'].get(target, {})
        if cell not in symbol:
            return None

        todocname = symbol[cell].docname
        return make_refnode(builder, fromdocname, todocname,
                            make_target(cell, target), contnode, target)

    def resolve_any_xref(self, env, fromdocname, builder,
                         target, node, contnode):
        """Return all possible cross references for ``target``."""
        cells = ['command', 'function', 'variable', 'face']
        nodes = ((cell, self.resolve_xref(env, fromdocname, builder,
                                          cell, target, node, contnode))
                 for cell in cells)
        return [('el:{}'.format(cell), node) for (cell, node) in nodes
                if node is not None]

    def get_objects(self):
        """Get all documented symbols for use in the search index."""
        for name, symbol in self.data['obarray'].items():
            for cellname, cell in symbol.items():
                yield (name, name, cell.objtype, cell.docname,
                       make_target(cellname, name),
                       self.object_types[cell.objtype].attrs['searchprio'])


def setup(app):
    app.add_domain(EmacsLispDomain)
