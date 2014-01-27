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

from sphinx import addnodes
from docutils import utils, nodes


PROPERTY_PARAMETER_RE = re.compile('{([^}]+)}')


def parse_checker_property(env, signature, node):
    if not signature.startswith(':'):
        raise ValueError('Invalid signature: ' + signature)

    keyword, value = signature.split(' ', 1)
    node += addnodes.desc_name(keyword, keyword)
    node += nodes.Text(' ', ' ')

    # Taken from sphinx.roles.emph_literal_role
    value = utils.unescape(value)
    pos = 0
    for match in PROPERTY_PARAMETER_RE.finditer(value):
        if match.start() > pos:
            text = value[pos:match.start()]
            node += nodes.Text(text, text)
        param = match.group(1)
        node += nodes.emphasis(param, param)
        pos = match.end()
    if pos < len(value):
        node += nodes.Text(value[pos:], value[pos:])

    return keyword.strip()


def parse_syntax_checker(env, signature, node):
    node += addnodes.desc_annotation('Syntax checker ', 'Syntax checker ')
    node += addnodes.desc_name(signature, signature)
    return signature


def setup(app):
    app.add_object_type('checker-property', 'checkprop',
                        indextemplate='pair: %s; Syntax checker property',
                        parse_node=parse_checker_property,
                        objname='Syntax checker property')
    app.add_object_type('syntax-checker', 'checker',
                        indextemplate='pair: %s; Syntax checker',
                        parse_node=parse_syntax_checker,
                        objname='Syntax checker')
