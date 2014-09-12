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


"""A Sphinx extension to replace textual issue references such as ``#1`` with
real references to issues.

"""


import re

from docutils import nodes
from docutils.transforms import Transform


#: Regular expression for issue references
ISSUE_RE = re.compile(r'\[GH-(?P<id>\d+)\]')


#: Base URL for issue references
ISSUE_URL = 'https://github.com/flycheck/flycheck/issues/{id}'


ISSUE_DOCUMENT_RES = ['^changes$', '^news/.*$']


class IssueReferences(Transform):

    default_priority = 999

    def apply(self):
        docname = self.document.settings.env.docname
        if not any(re.search(p, docname) for p in ISSUE_DOCUMENT_RES):
            # Substitute in the change log only
            return
        for node in self.document.traverse(nodes.Text):
            parent = node.parent
            # Ignore literal references
            if isinstance(parent, (nodes.literal, nodes.FixedTextElement)):
                continue
            text = node.astext()
            new_nodes = []
            pos = 0

            for match in ISSUE_RE.finditer(text):
                if match.start() > pos:
                    leading_text = text[pos:match.start()]
                    new_nodes.append(nodes.Text(leading_text, leading_text))
                issue_id = match.group('id')
                reference = nodes.reference(match.group(0), match.group(0),
                                            internal=False)
                reference['refuri'] = ISSUE_URL.format(id=issue_id)
                new_nodes.append(reference)
                pos = match.end()
            if not new_nodes:
                continue
            if pos < len(text):
                new_nodes.append(nodes.Text(text[pos:], text[pos:]))
            parent.replace(node, new_nodes)


def setup(app):
    app.add_transform(IssueReferences)
