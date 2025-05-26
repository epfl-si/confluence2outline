import collections
from functools import cached_property
import html
import json
import os
import re
import types

from lxml import etree


class classproperty(object):
    def __init__(self, f):
        self.f = f
    def __get__(self, obj, owner):
        return self.f(owner)


class Template:
    priority = 10

    def __init__ (self, templates=None):
        self.templates = (
            templates if templates is not None
            else Template.__subclasses__())

    @classproperty
    def tag (cls):
        name = cls.__name__
        components = name.split("__")
        if len(components) == 2 and components[0] == "Template":
            return re.sub("_", "-", components[1])
        else:
            return None    # Your subclass won't auto-admit; you need
                           # to either call it explicitly, or override
                           # one or both of its its `tag` or `admits`
                           # methods.

    @classmethod
    def admits (cls, element):
        return cls.tag == element.tag

    def apply_templates (self, element_or_elements):
        """Apply the construction-time templates (by default, all of them) recursively.

        Returns a list of Outline-compliant dicts, which may in turn
        contain a list of more Outline-compliant dicts as the value of
        the `content` key.

        The `apply` method of Template subclasses should return a list of such dicts,
        or the empty list to indicate that the element ought to be discarded.
        """
        ret = []
        for element in flatten(element_or_elements):
            matching_templates = sorted([t for t in self.templates if t.admits(element)],
                                        key=lambda cls: - cls.priority)
            if len(matching_templates) == 0:
                raise ValueError(f"Unhandled element: {element}")

            processed = matching_templates[0](templates=self.templates).apply(element)
            ret.extend(flatten(processed))

        return ret


class DefaultTemplate (Template):
    priority = 0    # All other classes have priority

    @classmethod
    def admits (cls, element):
        return True

    def apply (self, element):
        return {"type": "text", "text": f"(? Unable to translate {element.tag})"}


class Template__p (Template):
    def apply (self, element):
        texts = element.xpath(".//text()")
        if texts:
            return {"type":"text","text": "".join(texts)}
        else:
            return []


def is_text_node (node):
    return hasattr(node, "splitlines")


###################### No user-serviceable parts below #########################


def flatten(list_of_lists):
    """It flattens! It iterifies *everything!* Even things that R accidentally unboxes!"""
    def _flatten (wat):   # Sigh.
        if isinstance(wat, list) or isinstance(wat, types.GeneratorType):
            for e in wat:
                yield from flatten(e)
        else:
            yield wat
    return list(_flatten(list_of_lists))


class DocumentConverter:
    """XML-of-Confluence to JSON-of-Outline converter."""
    def __init__ (self, documents_tibble):
        self.documents = [
            _Document(self, *args)
            for args in zip(flatten(documents_tibble["documentId"]),
                            flatten(documents_tibble["body"]))
            if args[1] is not None]

    def get_converted_documents (self):
        return { "documentId": [d.uuid for d in self.documents],
                 "outline.document": [PythonStruct(self.process(d))
                                      for d in self.documents] }

    def process (self, doc):
        return {
            "type": "doc",
            "content": Template().apply_templates(doc.etree.getchildren())
        }


class _Document(collections.namedtuple("Document",
                                       ("converter", "uuid", "body"))):
    """An lxml document, with Confluence-specific namespaces baked in."""

    @cached_property
    def etree (self):
        """Parse the Confluence XML dialect.

        Assume caller (i.e. R code) has wrapped it into a `<confluence-body>` tag.

        Parse it correctly (i.e. strict XML), despite Confluence emitting both HTML
        entities *and* namespaced nodes.
        """
        return _LxmlNamespacedMembrane(
            etree.fromstring(
                self._clean_namespaced_xml(
                    self.body)))
        # Bleh.

    _entities_for_html_unescape_re = re.compile(r"&(?!lt|gt|amp|apos|quot)\w+;")

    @classmethod
    def _clean_namespaced_xml(cls, confluence_xmlish):
        namespaced = re.sub(
            "<confluence-body>",
            "<confluence-body %s>" % " ".join(
                f'xmlns:{k}="{v}"' for (k, v) in _LxmlNamespacedMembrane.xmlns.items()),
            confluence_xmlish)

        # ... We're not done yet. Confluence uses HTML entities, but
        # we don't want to decode &amp; etc. and bugger right through
        # our XML:
        def replace (match):
            return html.unescape(match[0])

        return cls._entities_for_html_unescape_re.sub(replace, namespaced)

    def xpath (self, xpath):
        return self.etree.xpath(xpath)


class _LxmlNamespacedMembrane:
    """Wrapper to paper over the XML namespace nonsense.

    - On the way in, auto-pass `namespaces={...}` to methods that care.
    - On the way out, un-James-Clark the `.tag` of wrapped objects.
    """

    xmlns = { k: f"urn:{k}" for k in ("ri", "ac") }

    def __init__(self, delegate):
        self.delegate = delegate

    @property
    def tag (self):
        return self.unclarkify(self.delegate.tag)

    def __repr__ (self):
        return re.sub("^<", "<~", repr(self.delegate))

    def getchildren (self):
        return [child if is_text_node(child) else self.__class__(child)
                for child in self.delegate.getchildren()]

    def xpath (self, _path, *args, **kwargs):
        kwargs["namespaces"] = self.xmlns
        ret = self.delegate.xpath(_path, *args, **kwargs)
        return [
            r if is_text_node(r) else _LxmlNamespacedMembrane(r)
            for r in ret
        ]

    @classmethod
    def unclarkify (cls, tag_name_in_james_clark_notation):
        return re.sub(r"{urn:([a-z]+)}", r"\1:", tag_name_in_james_clark_notation)


class PythonStruct:
    """A data structure whose JSON serialization is decided by Python (rather than R).

    In order to integrate with R's jsonlite (which itself uses S4
    generics), consuming R code should say something like

        setOldClass("__main__.PythonStruct")  ## Does nothing but squelch a warning
                                              ## in the next stanza, afaict
        setMethod(jsonlite:::asJSON, signature(x="__main__.PythonStruct"),
                  function(x, ...) { x$to_json(...) })
    """
    def __init__ (self, struct):
        self.struct = struct

    def to_json (self, *unused_args, **unused_kwargs):
        return json.dumps(self.struct)

    def __repr__ (self):
        return self.to_json()


if __name__ == "__main__" and os.getenv("R_SESSION_INITIALIZED") is None:
    conv = DocumentConverter(
        documents_tibble =
        {'documentId': ['77767cc0-c4ed-4688-b61c-c2aebf7e3218'],
         'body': [ open('test/data/services-etc.xml').read() ] })
    print(conv.get_converted_documents())
