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


class el_parameterlist(addnodes.desc_parameterlist):
    child_text_separator = ' '


class EmacsLispSymbol(ObjectDescription):

    @property
    def object_type(self):
        return self.env.domains[self.domain].object_types[self.objtype]

    @property
    def emacs_lisp_scope(self):
        return self.object_type.attrs['scope']

    def make_type_annotation(self):
        type_name = self.object_type.lname.title() + ' '
        return addnodes.desc_annotation(type_name, type_name)

    def handle_signature(self, sig, signode):
        parts = sig.split()
        name = parts[0]
        arguments = parts[1:]

        annotation = self.make_type_annotation()
        if annotation:
            signode += annotation

        signode += addnodes.desc_name(name, name)

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

        indextext = '{0}; Emacs Lisp {1}'.format(name, self.object_type.lname)
        self.indexnode['entries'].append(('pair', indextext, targetname, ''))


class EmacsLispFunction(EmacsLispSymbol):

    def handle_signature(self, sig, signode):
        parts = sig.split(' ')
        name = parts[0]
        arguments = parts[1:]
        name = EmacsLispSymbol.handle_signature(self, name, signode)

        paramlist = el_parameterlist(' '.join(arguments), '')
        signode += paramlist
        for arg in arguments:
            if arg.startswith('&'):
                paramlist += addnodes.desc_annotation(' ' + arg, ' ' + arg)
            else:
                node = addnodes.desc_parameter(arg, arg)
                node['noemph'] = True
                paramlist += node

        return name


class EmacsLispCommand(EmacsLispSymbol):

    option_spec = {
        'binding': directives.unchanged
    }
    option_spec.update(EmacsLispSymbol.option_spec)

    def make_type_annotation(self):
        return addnodes.desc_annotation('M-x ', 'M-x ')

    def run(self):
        nodes = ObjectDescription.run(self)

        # Insert a dedicated signature for the key binding before all other
        # signatures, but only for commands.  Nothing else has key bindings.
        binding = self.options.get('binding')
        if binding:
            desc_node = nodes[-1]
            assert isinstance(desc_node, addnodes.desc)
            signode = addnodes.desc_signature(binding, '')
            # No clue what this property is for, but ObjectDescription sets it
            # for its signatures, so we should do as well for our signature.
            signode['first'] = False
            desc_node.insert(0, signode)
            signode += addnodes.desc_name(binding, binding)

        return nodes


class EmacsLispCLStruct(EmacsLispSymbol):

    def before_content(self):
        EmacsLispSymbol.before_content(self)
        if self.names:
            self.env.temp_data['el:cl-struct'] = self.names[0]

    def after_content(self):
        EmacsLispSymbol.after_content(self)
        del self.env.temp_data['el:cl-struct']


class EmacsLispCLSlot(EmacsLispSymbol):

    def handle_signature(self, sig, signode):
        name = EmacsLispSymbol.handle_signature(self, sig, signode)
        struct = self.env.temp_data.get('el:cl-struct')
        if not struct:
            raise ValueError('Missing containing structure')
        return struct + '-' + name


class EmacsLispSlotXRefRole(XRefRole):

    def process_link(self, env, refnode, has_explicit_title, title, target):
        # Obtain the current structure
        current_struct = env.temp_data.get('el:cl-struct')
        omit_struct = target.startswith('~')
        target = target.lstrip('~')
        parts = target.split(' ', 1)
        # If the reference is given as "structure slot", adjust the title, and
        # reconstruct the function name
        if len(parts) > 1:
            struct, slot = parts
            target = parts.join('-')
            # If the first character is a tilde, or if there is a current
            # structure, omit the structure name
            if not has_explicit_title and (omit_struct or current_struct == struct):
                title = slot
        elif current_struct:
            # Resolve slot against the current struct
            target = current_struct + '-' + target

        return title,target


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
        'option': ObjType('user option', 'option', scope='variables',
                          searchprio=1),
        'hook': ObjType('hook', 'hook', scope='variables',
                        searchprio=0),
        'face': ObjType('face', 'face', scope='faces', searchprio=0),
        'cl-struct': ObjType('CL struct', 'cl-struct', scope='structs',
                             searchprio=0),
        'cl-slot': ObjType('slot', 'cl-slot', scope='functions',
                           searchprio=0)}
    directives = {
        'function': EmacsLispFunction,
        'macro': EmacsLispFunction,
        'command': EmacsLispCommand,
        'variable': EmacsLispSymbol,
        'option': EmacsLispSymbol,
        'hook': EmacsLispSymbol,
        'face': EmacsLispSymbol,
        'cl-struct': EmacsLispCLStruct,
        'cl-slot': EmacsLispCLSlot,
    }
    roles = {
        'function': XRefRole(),
        'macro': XRefRole(),
        'command': XRefRole(),
        'variable': XRefRole(),
        'option': XRefRole(),
        'hook': XRefRole(),
        'face': XRefRole(),
        'cl-struct': XRefRole(),
        'cl-slot': EmacsLispSlotXRefRole()}
    indices = []

    data_version = 1
    initial_data = {
        'symbols': {
            # fullname -> docname, objtype
            'functions': {},
            'variables': {},
            'faces': {},
            'structs': {},
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


def visit_el_parameterlist_html(self, node):
    self.body.append(' ')
    self.first_param = 1
    self.param_separator = node.child_text_separator


def visit_el_parameterlist_texinfo(self, node):
    self.body.append(' ')
    self.first_param = 1


def setup(app):
    app.add_domain(EmacsLispDomain)
    app.add_node(el_parameterlist,
                 html=(visit_el_parameterlist_html, lambda s, v: None),
                 texinfo=(visit_el_parameterlist_texinfo, lambda s, v: None))
