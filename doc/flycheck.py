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

from docutils import utils, nodes
from docutils.transforms import Transform

from sphinx import addnodes
from sphinx.roles import XRefRole
from sphinx.directives import ObjectDescription
from sphinx.domains import Domain, ObjType
from sphinx.util.nodes import make_refnode


class FlycheckObjectDescription(ObjectDescription):

    @property
    def object_type(self):
        return self.env.domains[self.domain].object_types[self.objtype]

    def make_type_annotation(self):
        type_name = self.object_type.lname.title() + ' '
        return addnodes.desc_annotation(type_name, type_name)

    def handle_signature(self, sig, signode):
        signode += self.make_type_annotation()
        signode += addnodes.desc_name(sig, sig)
        return sig

    def add_target_and_index(self, name, sig, signode):
        target = 'flyc:'+name
        if target not in self.state.document.ids:
            signode['names'].append(target)
            signode['ids'].append(target)
            signode['first'] = not self.names
            self.state.document.note_explicit_target(signode)

            objects = self.env.domaindata[self.domain]['objects']
            if name in objects:
                self.state_machine.reporter.warning(
                    'duplicate object description of %s, ' % name +
                    'other instance in ' +
                    self.env.doc2path(objects[name][0]),
                    line=self.lineno)
            objects[name] = (self.env.docname, self.objtype)

        indextext = '{0}; Flycheck {1}'.format(name, self.object_type.lname)
        self.indexnode['entries'].append(('pair', indextext, target, ''))


PROPERTY_PARAMETER_RE = re.compile('{([^}]+)}')


class CheckerProperty(FlycheckObjectDescription):

    def handle_signature(self, sig, signode):
        if not sig.startswith(':'):
            raise ValueError('Invalid signature: ' + sig)

        keyword, value = sig.split(' ', 1)
        signode += addnodes.desc_name(keyword, keyword)
        signode += nodes.Text(' ', ' ')

        # Taken from sphinx.roles.emph_literal_role
        value = utils.unescape(value)
        pos = 0
        for match in PROPERTY_PARAMETER_RE.finditer(value):
            if match.start() > pos:
                text = value[pos:match.start()]
                signode += nodes.Text(text, text)
            param = match.group(1)
            signode += nodes.emphasis(param, param)
            pos = match.end()
        if pos < len(value):
            signode += nodes.Text(value[pos:], value[pos:])

        return keyword.strip()


class FlycheckDomain(Domain):
    name = 'flyc'
    label = 'Flycheck'

    object_types = {
        'checker-property': ObjType('syntax checker property', 'checker-property'),
        'syntax-checker': ObjType('syntax checker', 'syntax-checker'),
    }
    directives = {
        'syntax-checker': FlycheckObjectDescription,
        'checker-property': CheckerProperty,
    }
    roles = {
        'checkprop': XRefRole(),
        'checker': XRefRole(),
    }
    indices = []

    data_version = 1
    initial_data = {
        # fullname -> docname, objtype
        'objects': {},
        'info': {'#languages': 0},
    }

    def clear_doc(self, docname):
        objects = self.data['objects']
        for name, (object_docname, _) in objects.items():
            if docname == object_docname:
                del objects[name]

    def resolve_xref(self, env, fromdoc, builder, objtype, target, node,
                     content):
        if target not in self.data['objects']:
            return None
        docname, _ = self.data['objects'][target]
        return make_refnode(builder, fromdoc, docname,
                            'flyc:'+target, content, target)

    def get_objects(self):
        for name, (docname, objtype) in self.data['objects'].iteritems():
            yield (name, name, objtype, docname, 'flyc:'+name,
                   self.object_types[objtype].attrs['searchprio'])


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
        app.env.domaindata['flyc']['info']['#languages'] = languages


def substitute_flycheck_info(app, doctree, docname):
    data = app.env.domaindata['flyc']
    for info in doctree.traverse(flycheck_info):
        kind = info['flycheck-info']
        if kind == '#flycheck-languages':
            value = str(data['info']['#languages'])
        elif kind == '#flycheck-checkers':
            value = str(sum(1 for (_, t) in data['objects'].itervalues()
                            if t == 'syntax-checker'))
        else:
            raise ValueError('Unknown kind ' + kind)
        info.replace_self(nodes.Text(value, value))


def setup(app):
    app.add_domain(FlycheckDomain)
    app.add_transform(FlycheckSubstitutions)
    app.connect('doctree-read', count_languages)
    app.connect('doctree-resolved', substitute_flycheck_info)
