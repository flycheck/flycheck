# -*- coding: utf-8; -*-
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

from docutils.parsers.rst import directives

from sphinx import addnodes
from sphinx.domains import Domain, ObjType
from sphinx.roles import XRefRole
from sphinx.directives import ObjectDescription
from sphinx.util.nodes import make_refnode


def make_target(scope, name):
    return 'el.{0}.{1}'.format(scope, name)


class EmacsLispSymbol(ObjectDescription):

    @property
    def object_type(self):
        return self.env.domains[self.domain].object_types[self.objtype]

    @property
    def emacs_lisp_scope(self):
        return self.object_type.attrs['scope']

    def handle_signature(self, sig, signode):
        parts = sig.split()
        name = parts[0]
        arguments = parts[1:]

        type_name = self.object_type.lname.title() + ' '
        signode += addnodes.desc_annotation(type_name, type_name)

        signode += addnodes.desc_name(name, name)

        if self.emacs_lisp_scope == 'functions':
            for arg in arguments:
                signode += addnodes.desc_parameter(arg, arg)

        binding = self.options.get('binding')
        if binding:
            bindingnode = addnodes.desc_signature(binding, '')
            signode.parent.append(bindingnode)
            bindingnode += addnodes.desc_annotation(
                'Key binding ', 'Key binding ')
            bindingnode += addnodes.desc_name(binding, binding)

        return name

    def add_target_and_index(self, name, sig, signode):
        # We must add the scope to target names, because Emacs Lisp allows for
        # variables and commands with the same name
        targetname = make_target(self.emacs_lisp_scope, name)
        if targetname not in self.state.document.ids:
            signode['names'].append(targetname)
            signode['ids'].append(targetname)
            signode['first'] = not self.names
            self.state.document.note_explicit_target(signode)

            data = self.env.domaindata[self.domain]
            objects = data['symbols'][self.emacs_lisp_scope]
            if name in objects:
                self.state_machine.reporter.warning(
                    'duplicate object description of %s, ' % name +
                    'other instance in ' +
                    self.env.doc2path(objects[name][0]),
                    line=self.lineno)
            objects[name] = (self.env.docname, self.objtype)

        indextext = '{0} (Emacs Lisp {1})'.format(name, self.object_type.lname)
        self.indexnode['entries'].append(('single', indextext, targetname, ''))


class EmacsLispCommand(EmacsLispSymbol):

    option_spec = {
        'binding': directives.unchanged
    }
    option_spec.update(EmacsLispSymbol.option_spec)


class EmacsLispDomain(Domain):
    """Emacs Lisp domain"""

    name = 'el'
    label = 'Emacs Lisp'
    object_types = {
        'function': ObjType('function', 'function', scope='functions',
                            searchprio=0),
        'macro': ObjType('macro', 'macro', scope='functions',
                         searchprio=0),
        'command': ObjType('command', 'command', scope='functions',
                           searchprio=1),
        'variable': ObjType('variable', 'variable', scope='variables',
                            searchprio=0),
        'option': ObjType('option', 'option', scope='variables',
                          searchprio=1),
        'hook': ObjType('hook', 'hook', scope='variables',
                        searchprio=0),
        'face': ObjType('face', 'face', scope='faces', searchprio=0)}
    directives = {
        'function': EmacsLispSymbol,
        'macro': EmacsLispSymbol,
        'command': EmacsLispCommand,
        'variable': EmacsLispSymbol,
        'option': EmacsLispSymbol,
        'hook': EmacsLispSymbol,
        'face': EmacsLispSymbol}
    roles = {
        'function': XRefRole(),
        'macro': XRefRole(),
        'command': XRefRole(),
        'variable': XRefRole(),
        'option': XRefRole(),
        'hook': XRefRole(),
        'face': XRefRole()}
    indices = []

    data_version = 1
    initial_data = {
        'symbols': {
            # fullname -> docname, objtype
            'functions': {},
            'variables': {},
            'faces': {},
        }
    }

    def clear_doc(self, docname):
        symbols = self.data['symbols']
        for scope, objects in symbols.iteritems():
            for name, (object_docname, _) in objects.iteritems():
                if docname == object_docname:
                    del self.data[scope][name]

    def resolve_xref(self, env, fromdoc, builder, objtype, target, node,
                     content):
        scope = self.object_types[objtype].attrs['scope']
        if target not in self.data['symbols'][scope]:
            return None
        docname, _ = self.data['symbols'][scope][target]
        return make_refnode(builder, fromdoc, docname,
                            make_target(scope, target), content, target)

    def get_objects(self):
        for scope, objects in self.data['symbols'].iteritems():
            for name, (docname, objtype) in objects.iteritems():
                yield (name, name, objtype, docname, make_target(scope, name),
                       self.object_types[objtype].attrs['searchprio'])


def setup(app):
    app.add_domain(EmacsLispDomain)
