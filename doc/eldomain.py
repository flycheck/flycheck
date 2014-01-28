# -*- coding: utf-8; -*-
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
        'binding': directives.unchanged,
        'prefix-arg': directives.unchanged,
    }
    option_spec.update(EmacsLispSymbol.option_spec)

    def with_prefix_arg(self, binding):
        prefix_arg = self.options.get('prefix-arg')
        return prefix_arg + ' ' + binding if prefix_arg else binding

    def make_type_annotation(self):
        keys = self.with_prefix_arg('M-x')
        return addnodes.desc_annotation(keys + ' ', keys + ' ')

    def run(self):
        nodes = ObjectDescription.run(self)

        # Insert a dedicated signature for the key binding before all other
        # signatures, but only for commands.  Nothing else has key bindings.
        binding = self.options.get('binding')
        if binding:
            binding = self.with_prefix_arg(binding)
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
        for scope, objects in symbols.items():
            for name, (object_docname, _) in objects.items():
                if docname == object_docname:
                    del symbols[scope][name]

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


# These two are copied from the LaTeX write
def visit_el_parameterlist_latex(self, node):
    self.body.append('}{')
    self.first_param = 1
def depart_el_parameterlist_latex(self, node):
    self.body.append('}{')


def setup(app):
    app.add_domain(EmacsLispDomain)
    app.add_node(el_parameterlist,
                 html=(visit_el_parameterlist_html, lambda s, v: None),
                 latex=(visit_el_parameterlist_latex,
                        depart_el_parameterlist_latex),
                 texinfo=(visit_el_parameterlist_texinfo, lambda s, v: None))
