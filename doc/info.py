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

import re
from string import Template

import requests
from docutils import nodes
from sphinx.roles import XRefRole
from sphinx.util import ws_re, logging

logger = logging.getLogger(__name__)

# Regular expression object to parse the contents of an Info reference
# role.
INFO_RE = re.compile(r'\A\((?P<manual>[^)]+)\)(?P<node>.+)\Z')


class InfoNodeXRefRole(XRefRole):
    """A role to reference a node in an Info manual."""

    innernodeclass = nodes.emphasis

    def process_link(self, env, refnode, has_explicit_title, title, target):
        """Process the link created by this role.

        Swap node and manual name, to more closely match the look of references
        in Texinfo.

        """
        # Normalize whitespace in info node targets
        target = ws_re.sub(' ', target)
        refnode['has_explicit_title'] = has_explicit_title
        if not has_explicit_title:
            match = INFO_RE.match(target)
            if match:
                # Swap title and node to create a title like info does
                title = '{0}({1})'.format(match.group('node'),
                                          match.group('manual'))
        return title, target


def node_encode(char):
    if char.isalnum():
        return char
    elif char == ' ':
        return '-'
    else:
        return '_00{:02x}'.format(ord(char))


def expand_node_name(node):
    """Expand ``node`` for use in HTML.

    ``node`` is the name of a node as string.

    Return a pair ``(filename, anchor)``, where ``filename`` is the base-name
    of the corresponding file, sans extension, and ``anchor`` the HTML anchor.

    See
    http://www.gnu.org/software/texinfo/manual/texinfo/html_node/HTML-Xref-Node-Name-Expansion.html.

    """
    if node == 'Top':
        return ('index', 'Top')
    else:
        normalized = ws_re.sub(' ', node.strip())
        encoded = ''.join(node_encode(c) for c in normalized)
        prefix = 'g_t' if not node[0].isalpha() else ''
        return (encoded, prefix + encoded)


class HTMLXRefDB(object):
    """Cross-reference database for Info manuals."""

    #: URL of the htmlxref database of GNU Texinfo
    XREF_URL = 'http://ftpmirror.gnu.org/texinfo/htmlxref.cnf'

    #: Regular expression to parse entries from an xref DB
    XREF_RE = re.compile(r"""
^\s*
(?:
(?P<comment>[#].*) |
(?P<substname>\w+)\s*=\s*(?P<substurl>\S+) |
(?P<manname>\w+)\s*(?P<mantype>node|mono)\s*(?P<manurl>\S+)
)
\s*$""", re.VERBOSE)

    @classmethod
    def parse(cls, htmlxref):
        substitutions = {}
        manuals = {}
        for line in htmlxref.splitlines():
            match = cls.XREF_RE.match(line)
            if match:
                if match.group('substname'):
                    url = Template(match.group('substurl')).substitute(
                        substitutions)
                    substitutions[match.group('substname')] = url
                elif (match.group('manname')
                      and match.group('mantype') == 'node'):
                    url = Template(match.group('manurl')).substitute(
                        substitutions)
                    manuals[match.group('manname')] = url
        return cls(manuals)

    def __init__(self, entries):
        """Initialize the HTMLXrefDB object with the provided entries."""
        self.entries = entries

    def resolve(self, manual, node):
        manual_url = self.entries.get(manual)
        if not manual_url:
            return None
        else:
            filename, anchor = expand_node_name(node)
            return manual_url + filename + '.html#' + anchor


def update_htmlxref(app):
    if not isinstance(getattr(app.env, 'info_htmlxref', None), HTMLXRefDB):
        logger.info('fetching Texinfo htmlxref database from {0}... '.format(
            HTMLXRefDB.XREF_URL))
        try:
            app.env.info_htmlxref = HTMLXRefDB.parse(
                requests.get(HTMLXRefDB.XREF_URL).text)
        except requests.exceptions.ConnectionError:
            logger.warning('Failed to load xref DB.  '
                           'Info references will not be resolved')
            app.env.info_htmlxref = None


def resolve_info_references(app, _env, refnode, contnode):
    """Resolve Info references.

    Process all :class:`~sphinx.addnodes.pending_xref` nodes whose ``reftype``
    is ``infonode``.

    Replace the pending reference with a :class:`~docutils.nodes.reference`
    node, which references the corresponding web URL, as stored in the database
    referred to by :data:`HTMLXREF_URL`.

    """
    if refnode['reftype'] != 'infonode':
        return None

    target = ws_re.sub(' ', refnode['reftarget'])
    match = INFO_RE.match(target)
    if not match:
        logger.warning('Invalid info target: {0}'.format(target),
                       location=(refnode.source, refnode.line))
        return contnode

    manual = match.group('manual')
    node = match.group('node')

    xrefdb = app.env.info_htmlxref
    if xrefdb:
        uri = xrefdb.resolve(manual, node)
        if not uri:
            message = 'Cannot resolve info manual {0}'.format(manual)
            logger.warning(message, location=(refnode.source, refnode.line))
            return contnode
        else:
            reference = nodes.reference('', '', internal=False,
                                        refuri=uri, reftitle=target)
            reference += contnode
            return reference
    else:
        # Without an xref DB we're unable to resolve any info references
        return None


def setup(app):
    app.add_role('infonode', InfoNodeXRefRole())
    app.connect(str('builder-inited'), update_htmlxref)
    app.connect(str('missing-reference'), resolve_info_references)
    return {'version': '0.1', 'parallel_read_safe': True}
