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

import os
import re
from itertools import ifilter

import sexpdata

from docutils import nodes, utils
from docutils.statemachine import (string2lines,
                                   StateMachineWS, StateWS,
                                   UnexpectedIndentationError)
from docutils.parsers.rst import Directive, directives

from sphinx import addnodes
from sphinx.domains import Domain, ObjType
from sphinx.roles import XRefRole
from sphinx.directives import ObjectDescription
from sphinx.util.nodes import make_refnode


def make_target(scope, name):
    """Create a target from ``scope`` and ``name``.

    ``name`` is the name of the Emacs Lisp symbol to reference, and ``scope``
    is the scope in which to reference the symbol.  Both arguments are strings.

    Return the target name as string.

    """
    return 'el.{0}.{1}'.format(scope, name)


class el_parameterlist(addnodes.desc_parameterlist):
    """A container node for the parameter list of a Emacs Lisp function."""
    child_text_separator = ' '


class el_annotation(addnodes.desc_annotation):
    """A node for the type annotation of Emacs Lisp namespace."""
    pass


class el_parameter(addnodes.desc_parameter):
    """A node for parameters of Emacs Lisp functions."""
    pass


class el_metavariable(nodes.emphasis):
    """A node for a meta variable."""
    pass


class EmacsLispSymbol(ObjectDescription):
    """A directive to describe an Emacs Lisp symbol."""

    option_spec = {
        'auto': directives.flag
    }
    option_spec.update(ObjectDescription.option_spec)

    @property
    def object_type(self):
        """The :class:`~sphinx.domains.ObjType` of this directive."""
        return self.env.domains[self.domain].object_types[self.objtype]

    @property
    def emacs_lisp_scope(self):
        """The scope of this object type as string."""
        return self.object_type.attrs['scope']

    def make_type_annotation(self):
        """Create the type annotation for this directive.

        Return the type annotation node, preferably a :class:`el_annotation`
        node.

        """
        type_name = self.object_type.lname.title() + ' '
        return el_annotation(type_name, type_name)

    def get_signatures(self):
        if 'auto' in self.options:
            return [self.get_auto_signature()]
        else:
            return ObjectDescription.get_signatures(self)

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
            symbol_scopes = data['namespace'].setdefault(name, {})
            if self.emacs_lisp_scope in symbol_scopes:
                self.state_machine.reporter.warning(
                    'duplicate object description of %s, ' % name +
                    'other instance in ' +
                    self.env.doc2path(symbol_scopes[self.emacs_lisp_scope][0]),
                    line=self.lineno)
            symbol_scopes[self.emacs_lisp_scope] = (self.env.docname,
                                                    self.objtype)

        indextext = '{0}; Emacs Lisp {1}'.format(name, self.object_type.lname)
        self.indexnode['entries'].append(('pair', indextext, targetname, ''))

    def get_auto_signature(self):
        name = self.arguments[0].strip()
        if not self.lookup_symbol(name):
            self.state_machine.reporter.warning(
                'Undefined symbol {0}'.format(name), line=self.lineno)
        return name

    def lookup_symbol(self, name):
        return self.env.domaindata[self.domain]['loaded_symbols'].get(name)

    def get_auto_docstring(self):
        symbol = self.lookup_symbol(self.names[0])
        if symbol:
            return symbol.properties.get('variable_documentation')

    def parse_docstring(self, docstring):
        parser = DocstringParser(self.state_machine.reporter)
        doc = nodes.document(self.state.document.settings,
                             self.state_machine.reporter)
        return parser.parse(docstring)

    def get_auto_doc(self):
        docstring = self.get_auto_docstring()
        if not docstring:
            self.state_machine.reporter.warning(
                'no docstring for symbol {0}'.format(self.names[0]),
                line=self.lineno)
            return []
        else:
            return [nodes.literal_block(docstring, docstring)] + self.parse_docstring(docstring)

    def run(self):
        nodes = ObjectDescription.run(self)

        if 'auto' in self.options:
            desc_node = nodes[-1]
            cont_node = desc_node[-1]
            self.before_content()
            for i, node in enumerate(self.get_auto_doc()):
                cont_node.insert(0 + i, node)
            self.after_content()

        return nodes


class EmacsLispFunction(EmacsLispSymbol):
    """A directive to describe an Emacs Lisp function.

    This directive is different from :class:`EmacsLispSymbol` in that it
    accepts a parameter list.

    """

    def get_auto_signature(self):
        symbol = self.lookup_symbol(self.arguments[0])
        sig = self.arguments[0]
        if symbol:
            arglist = ' '.join(symbol.properties.get('function_arglist', []))
            sig += ' ' + arglist
        return sig

    def get_auto_docstring(self):
        symbol = self.lookup_symbol(self.names[0])
        if symbol:
            return symbol.properties.get('function_documentation')

    def handle_signature(self, sig, signode):
        parts = sig.split(' ')
        name = parts[0]
        arguments = parts[1:]
        name = EmacsLispSymbol.handle_signature(self, name, signode)

        paramlist = el_parameterlist(' '.join(arguments), '')
        signode += paramlist
        for arg in arguments:
            if arg.startswith('&'):
                paramlist += addnodes.desc_annotation(' ' + arg + ' ',
                                                      ' ' + arg + ' ')
            else:
                node = el_parameter(arg, arg)
                node['noemph'] = True
                paramlist += node

        return name


class EmacsLispCommand(EmacsLispSymbol):
    """A directive to describe an interactive Emacs Lisp command.

    This directive is different from :class:`EmacsLispSymbol` in that it
    describes the command with its keybindings.  For this purpose, it has two
    additional options ``:binding:`` and ``:prefix-arg``.

    The former documents key bindings for this command (in addition to ``M-x``),
    and the latter adds a prefix argument to the description of this command.

    Typically, this directive is used multiple times for the same command,
    where the first use describes the command without prefix argument, and the
    latter describes the use with prefix argument.  The latter usually has
    ``:noindex:`` set.

    """

    option_spec = {
        'binding': directives.unchanged,
        'prefix-arg': directives.unchanged,
    }
    option_spec.update(EmacsLispSymbol.option_spec)

    def get_auto_docstring(self):
        symbol = self.lookup_symbol(self.names[0])
        if symbol:
            return symbol.properties.get('function_documentation')

    def with_prefix_arg(self, binding):
        """Add the ``:prefix-arg:`` option to the given ``binding``.

        Return the complete key binding including the ``:prefix-arg:`` option
        as string.  If there is no ``:prefix-arg:``, return ``binding``.

        """
        prefix_arg = self.options.get('prefix-arg')
        return prefix_arg + ' ' + binding if prefix_arg else binding

    def make_type_annotation(self):
        keys = self.with_prefix_arg('M-x')
        node = el_annotation(keys + ' ', keys + ' ')
        node['keep_texinfo'] = True
        return node

    def run(self):
        nodes = EmacsLispSymbol.run(self)

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
    """A directive to describe a CL struct."""

    def before_content(self):
        EmacsLispSymbol.before_content(self)
        if self.names:
            self.env.temp_data['el:cl-struct'] = self.names[0]

    def after_content(self):
        EmacsLispSymbol.after_content(self)
        del self.env.temp_data['el:cl-struct']


class EmacsLispCLSlot(EmacsLispSymbol):
    """A directive to describe a slot of a CL struct.

    This directive prepends the name of the current CL struct to the slot.

    """

    def handle_signature(self, sig, signode):
        name = EmacsLispSymbol.handle_signature(self, sig, signode)
        struct = self.env.temp_data.get('el:cl-struct')
        if not struct:
            raise ValueError('Missing containing structure')
        return struct + '-' + name


class EmacsLispSlotXRefRole(XRefRole):
    """A role to reference a CL slot."""

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

        return title, target


def var_role(role, rawtext, text, lineno, inliner,
             options={}, content=[]):
    return [el_metavariable(rawtext, text)], []


METAVAR_RE = re.compile('{([^}]+)}')


def varcode_role(role, rawtext, text, lineno, inliner,
                 options={}, content=[]):
    text = utils.unescape(text)
    position = 0
    node = nodes.literal(rawtext, '', role=role, classes=[role])
    for match in METAVAR_RE.finditer(text):
        if match.start() > position:
            trailing_text = text[position:match.start()]
            node += nodes.Text(trailing_text, trailing_text)
        node += el_metavariable(match.group(1), match.group(1))
        position = match.end()
    if position < len(text):
        node += nodes.Text(text[position:], text[position:])
    return [node], []


class Symbol(object):
    def __init__(self, name):
        self.name = name,
        self.scopes = set()
        self.properties = {}

    def __str__(self):
        return self.name

    def __repr__(self):
        return 'Symbol({0}, {1!r})'.format(self.name, self.properties)

    def __hash__(self):
        return hash(self.name)

    def __eq__(self, other):
        return self.name == other.name

    def __ne__(self, other):
        return self.name != other.name


class AbstractInterpreter(object):

    def defun(self, function, name, arglist, docstring=None, *rest):
        symbol = self.intern(name)
        symbol.scopes.add('function')
        symbol.properties['function_arglist'] = [s.value() for s in arglist]
        if docstring:
            symbol.properties['function_documentation'] = docstring

    def defvar(self, function, name, _initial_value=None, docstring=None,
               *rest):
        symbol = self.intern(name)
        symbol.scopes.add('variable')
        if docstring:
            symbol.properties['variable_documentation'] = docstring
        symbol.properties['local_variable'] = function.endswith('-local')

    def eval_inner(self, function, *args):
        self.eval_all(args)

    DEFAULT_FUNCTIONS = {
        'defun': defun,
        'defmacro': defun,
        'defvar': defvar,
        'defcustom': defvar,
        'defvar-local': defvar,
        'eval-and-compile': eval_inner,
        'eval-when-compile': eval_inner,
    }

    def __init__(self, namespace=None, **functions):
        self.functions = dict(self.DEFAULT_FUNCTIONS)
        self.functions.update(functions)
        self.namespace = namespace if namespace is not None else {}

    def intern(self, symbol):
        name = symbol.value()
        return self.namespace.setdefault(name, Symbol(name))

    def eval(self, sexp):
        function_name = sexp[0]
        args = sexp[1:]
        function = self.functions.get(function_name.value())
        if function:
            return function(self, function_name.value(), *args)

    def eval_all(self, sexps):
        for sexp in sexps:
            self.eval(sexp)


class RequireLibrary(Directive):
    """Load and parse an Emacs Lisp library."""

    required_arguments = 1
    optional_arguments = 0
    has_content = False

    def _find_library(self, feature):
        config = self.env.config
        if not config.emacs_lisp_load_path:
            self.state_machine.reporter.warning('Empty load path!',
                                                line=self.lineno)
        filename = feature + '.el'
        candidates = (os.path.join(d, filename)
                      for d in config.emacs_lisp_load_path)
        return next((f for f in candidates if os.path.isfile(f)), None)

    def _load(self, library):
        with open(library, 'r') as source:
            # Wrap source into a top-level sexp, to make it consumable for
            # sexpdata
            whole_file_sexp = '(\n{0}\n)'.format(source.read())
            # Read the sexp, and don't mangle it!
            expressions = sexpdata.loads(
                whole_file_sexp, nil=None, true=None, false=None)
            return expressions

    def run(self):
        self.domain, self.objtype = self.name.split(':', 1)
        self.env = self.state.document.settings.env
        feature = self.arguments[0]

        library = self._find_library(feature)
        if not library:
            self.state_machine.reporter.warning(
                'Cannot find library for feature {0}'.format(feature),
                line=self.lineno)
            return []
        else:
            self.env.note_dependency(library)
            data = self.env.domaindata[self.domain]
            if feature not in data['features']:
                expressions = self._load(library)
                interpreter = AbstractInterpreter(
                    namespace=data['loaded_symbols'])
                interpreter.eval_all(expressions)
                data['features'].add(feature)

            return []


class Body(StateWS):
    patterns = { 'text': r'' }
    initial_transitions = ('text',)

    def text(self, match, context, next_state):
        return [match.string], 'Text', []


class Text(StateWS):
    patterns = { 'text': r'' }
    initial_transitions = [('text', 'Body')]

    inline_patterns = re.compile(
        """
        (?:`(?P<literal>[^']+)') # A literal text
        """,
        re.MULTILINE | re.UNICODE | re.VERBOSE)

    def parse_inline(self, text):
        position = 0
        result_nodes = []
        for match in self.inline_patterns.finditer(text):
            if match.start() > position:
                leading_text = text[position:match.start()]
                result_nodes.append(nodes.Text(leading_text, leading_text))
            position = match.end()
            if match.group('literal'):
                target = match.group('literal')
                # A literal in a docstring may reference a known symbol, so we
                # turn it into a pending reference.  If the literal doesn't
                # refer to a symbol, Sphinx will fail to resolve the reference
                # and automatically degrade it to a plain literal.
                ref = addnodes.pending_xref("`{0}'".format(target),
                                            reftype='symbol', refdomain='el',
                                            refexplicit=False,
                                            reftarget=target,
                                            refwarn=False)
                ref += nodes.literal(target, target)
                result_nodes.append(ref)
            else:
                raise NotImplementedError('Failed to handle a branch of the inline patterns!')
        if position < len(text):
            trailing_text = text[position:]
            result_nodes.append(nodes.Text(trailing_text, trailing_text))
        return result_nodes

    def paragraph(self, lines, lineno):
        text = '\n'.join(lines).rstrip()
        p = nodes.paragraph(text, '', *self.parse_inline(text))
        p.source, p.line = self.state_machine.get_source_and_line(lineno)
        return p

    def blank(self, match, context, next_state):
        """End of a paragraph"""
        p = self.paragraph(
            context, self.state_machine.abs_line_number() - 1)
        return [], 'Body', [p]

    def text(self, match, context, next_state):
        startline = self.state_machine.abs_line_number() - 1
        msg = None
        try:
            block = self.state_machine.get_text_block(flush_left=True)
        except UnexpectedIndentationError as err:
            block, src, srcline = err.args
            msg = self.state_machine.reporter.error(
                'Unexpected indentation.', source=src, line=srcline)
        lines = context + list(block)
        paragraph = self.paragraph(lines, startline)
        return [], next_state, [paragraph, msg]


class DocstringParser(object):
    def __init__(self, reporter):
        self.state_classes = [Body, Text]
        self.reporter = reporter

    def parse(self, inputstring):
        self.statemachine = StateMachineWS(state_classes=self.state_classes,
                                           initial_state='Body',
                                           debug=True)
        self.statemachine.reporter = self.reporter
        inputlines = string2lines(inputstring, tab_width=8,
                                  convert_whitespace=True)
        nodes = self.statemachine.run(inputlines)
        return nodes


class EmacsLispDomain(Domain):
    """A domain to document Emacs Lisp namespace."""

    name = 'el'
    label = 'Emacs Lisp'
    object_types = {
        'function': ObjType('function', 'function', scope='function',
                            searchprio=0),
        'macro': ObjType('macro', 'macro', scope='function',
                         searchprio=0),
        'command': ObjType('command', 'command', scope='function',
                           searchprio=1),
        'variable': ObjType('variable', 'variable', scope='variable',
                            searchprio=0),
        'option': ObjType('user option', 'option', scope='variable',
                          searchprio=1),
        'hook': ObjType('hook', 'hook', scope='variable',
                        searchprio=0),
        'face': ObjType('face', 'face', scope='face', searchprio=0),
        'cl-struct': ObjType('CL struct', 'cl-struct', scope='struct',
                             searchprio=0),
        'cl-slot': ObjType('slot', 'cl-slot', scope='function',
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
        'require': RequireLibrary,
    }
    roles = {
        'symbol': XRefRole(),
        'function': XRefRole(),
        'macro': XRefRole(),
        'command': XRefRole(),
        'variable': XRefRole(),
        'option': XRefRole(),
        'hook': XRefRole(),
        'face': XRefRole(),
        'cl-struct': XRefRole(),
        'cl-slot': EmacsLispSlotXRefRole(),
        # Special markup roles
        'var': var_role,
        'varcode': varcode_role,
    }
    indices = []

    data_version = 3
    initial_data = {
        # fullname -> scope -> (docname, objtype)
        'namespace': {},
        'features': set(),
        'loaded_symbols': {}
    }

    def clear_doc(self, docname):
        namespace = self.data['namespace']
        for symbol, scopes in namespace.items():
            for scope, (object_docname, _) in scopes.items():
                if docname == object_docname:
                    del namespace[symbol][scope]

    def resolve_xref(self, env, fromdoc, builder, objtype, target, node,
                     content):
        scopes = self.data['namespace'].get(target, {})
        scope = None
        if objtype == 'symbol':
            # A generic reference.  Figure out the target scope
            if len(scopes) == 1:
                # The symbol has just one scope, so use it
                scope = scopes.keys()[0]
            elif len(scopes) > 1:
                # The generic symbol reference is ambiguous, because the
                # symbol has multiple scopes attached
                scope = next(ifilter(lambda s: s in scopes,
                                     ['function', 'variable', 'face', 'struct']),
                             None)
                if not scope:
                    # If we have an unknown scope
                    raise ValueError('Unknown scope: {0!r}'.format(scopes))
                message = 'Ambiguous reference to {0}, in scopes {1}, using {2}'.format(
                    target, ', '.join(scopes), scope)
                env.warn(fromdoc, message, getattr(node, 'line'))
        else:
            # A reference with a specific objtype, so get the scope
            # corresponding to the object type
            scope = self.object_types[objtype].attrs['scope']
        # If the target isn't present in the scope, we can't resolve the
        # reference
        if scope not in scopes:
            return None
        docname, _ = scopes[scope]
        return make_refnode(builder, fromdoc, docname,
                            make_target(scope, target), content, target)

    def get_objects(self):
        for symbol, scopes in self.data['namespace'].iteritems():
            for scope, (docname, objtype) in scopes.iteritems():
                yield (symbol, symbol, objtype, docname,
                       make_target(scope, symbol),
                       self.object_types[objtype].attrs['searchprio'])


def noop(self, node):
    """Do nothing with ``node``."""
    pass


def delegate(target_type):
    """Create visitor functions to delegate the processing of a node.

    ``target_type`` is a type object whose visitor functions shall be used to
    process a node.

    """
    visit = lambda s, n: getattr(s, 'visit_{0}'.format(
        target_type.__name__))(n)
    depart = lambda s, n: getattr(s, 'depart_{0}'.format(
        target_type.__name__))(n)
    return (visit, depart)


def visit_el_parameterlist_html(self, node):
    self.body.append(' ')
    self.first_param = 1
    self.optional_param_level = 0
    self.required_params_left = sum([isinstance(c, addnodes.desc_parameter)
                                         for c in node.children])
    self.param_separator = node.child_text_separator


def visit_el_parameterlist_texinfo(self, node):
    self.body.append(' ')
    self.first_param = 1


def visit_el_annotation_texinfo(self, node):
    if not node.get('keep_texinfo'):
        raise nodes.SkipNode
    else:
        self.visit_desc_annotation(node)


def depart_el_annotation_texinfo(self, node):
    self.depart_desc_annotation(node)


def visit_el_parameter_texinfo(self, node):
    if not self.first_param:
        self.body.append(' ')
    else:
        self.first_param = 0
    text = self.escape(node.astext())
    # replace no-break spaces with normal ones
    text = text.replace(u' ', '@w{ }')
    self.body.append(text)
    # Don't process the children
    raise nodes.SkipNode


def visit_el_metavariable_texinfo(self, node):
    self.body.append('@var{{{0}}}'.format(self.escape(node.astext())))
    # Do not process the children of this node, since we do not allow
    # formatting inside.
    raise nodes.SkipNode


def visit_el_metavariable_html(self, node):
    self.body.append(self.starttag(node, 'var', ''))


def depart_el_metavariable_html(self, node):
    self.body.append('</var>')


def setup(app):
    app.add_domain(EmacsLispDomain)
    app.add_node(el_parameterlist,
                 html=(visit_el_parameterlist_html, noop),
                 latex=delegate(addnodes.desc_parameterlist),
                 texinfo=(visit_el_parameterlist_texinfo, noop))
    app.add_node(el_annotation,
                 html=delegate(addnodes.desc_annotation),
                 latex=delegate(addnodes.desc_annotation),
                 texinfo=(visit_el_annotation_texinfo,
                          depart_el_annotation_texinfo))
    app.add_node(el_parameter,
                 html=delegate(addnodes.desc_parameter),
                 latex=delegate(addnodes.desc_parameter),
                 texinfo=(visit_el_parameter_texinfo, None))
    app.add_node(el_metavariable,
                 html=(visit_el_metavariable_html,
                       depart_el_metavariable_html),
                 latex=delegate(nodes.emphasis),
                 texinfo=(visit_el_metavariable_texinfo, None))
    # Auto docs
    app.add_config_value('emacs_lisp_load_path', [], 'env')
