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
                            'flc:'+target, content, target)

    def get_objects(self):
        for name, (docname, objtype) in self.data['objects'].iteritems():
            yield (name, name, objtype, docname, 'flyc:'+name,
                   self.object_types[objtype].attrs['searchprio'])



def setup(app):
    app.add_domain(FlycheckDomain)
