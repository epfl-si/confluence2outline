import collections
from functools import cached_property
import html
import inspect
import json
import os
import re
import types

from lxml import etree


class Template:
    priority = 10

    def __init__ (self, templates=None):
        self.templates = (
            templates if templates is not None
            else Template.__subclasses__())

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
        for element in  (element_or_elements if is_listish(element_or_elements)
                         else [element_or_elements]):
            matching_templates = sorted([t for t in self.templates if t.admits(element)],
                                        key=lambda cls: - cls.priority)
            if len(matching_templates) == 0:
                raise ValueError(f"Unhandled element: {element}")

            processed = matching_templates[0](templates=self.templates).apply(element)
            ret.extend(flatten(processed))

        return ret


class Template__default (Template):
    priority = 0    # All other classes have priority

    @classmethod
    def admits (self, element):
        return True

    def apply (self, element):
        return Template_p.paragraph_of_text(f"(? Unable to translate {element.tag})")


class Template_p (Template):
    tag = "p"

    @classmethod
    def paragraph_of_text (cls, text):
        return { "type": "paragraph", "content": [{"type":"text","text": text}] }

    def apply (self, element):
        texts = element.xpath(".//text()")
        if texts:
            return self.paragraph_of_text(" ".join(texts))
        else:
            return []


###################### No user-serviceable parts below #########################


def flatten(list_of_lists):
    """It flattens! It iterifies *everything!* Even things that R accidentally unboxes!"""
    def _flatten (wat):   # Sigh.
        if is_listish(wat):
            for e in wat:
                yield from flatten(e)
        else:
            yield wat
    return list(_flatten(list_of_lists))


def is_listish (wat):
    return isinstance(wat, list) or isinstance(wat, types.GeneratorType)


class DocumentConverter:
    """XML-of-Confluence to JSON-of-Outline converter."""
    def __init__ (self, documents_tibble):
        self.documents = documents_tibble

    def get_converted_documents (self):
        docs = self.all_documents()
        return { "documentId": [d.uuid for d in docs],
                 "outline.document": [PythonStruct(self.process(d))
                                      for d in docs] }

    def list_attachments (self):
        docs = self.all_documents()
        ret = {"latest.version": [], "documentId": [], "filename": []}

        for d in docs:
            filenames = self._get_attachment_filenames(d)
            if len(filenames):
                ret["documentId"].append(d.uuid)
                ret["latest.version"].append(d.latest_version_uuid)
                ret["filename"].append(filenames)

        return ret

    def all_documents (self):
        return [_Document(self, *args)
                for args in zip(flatten(self.documents["documentId"]),
                                flatten(self.documents["latest.version"]),
                                flatten(self.documents["body"]))
                if self.documents["body"] is not None]

    def process (self, doc):
        return {
            "type": "doc",
            "content": Template().apply_templates(doc.xpath("./*"))
        }

    def _get_attachment_filenames (self, d):
        return list(flatten(d.etree.xpath("//ri:attachment/@ri:filename")))


class _Document(collections.namedtuple("Document",
                                       ("converter", "uuid",
                                        "latest_version_uuid", "body"))):
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
    """Metaprogrammed object wrapper to paper over the XML namespace nonsense.

    - On the way in, auto-pass `namespaces={...}` to methods that care.
    - On the way out, un-James-Clark the `.tag` of wrapped objects.
    """

    xmlns = { k: f"urn:{k}" for k in ("ri", "ac") }

    def __init__(self, delegate):
        self.delegate = delegate

    def __repr__ (self):
        return re.sub("^<", "<~", repr(self.delegate))

    def __getattr__ (self, name):
        if name == "delegate":
            return self.delegate

        attr = getattr(self.delegate, name)

        if name == "tag":
            return _LxmlNamespacedMembrane.unclarkify(attr)
        if not callable(attr):
            return attr

        sig = inspect.signature(attr)
        args = [p for p in sig.parameters.values()
                if p.kind in (p.POSITIONAL_ONLY,
                              p.POSITIONAL_OR_KEYWORD)]

        if len(args) == 1 and 'namespaces' in sig.parameters:
            def wrapper(*args, **kwargs):
                ret = attr(*args, namespaces=self.xmlns, **kwargs)
                if hasattr(ret, "xpath"):
                    return _LxmlNamespacedMembrane(ret)
                elif ret and isinstance(ret, list) and hasattr(ret[0], "xpath"):
                    return [_LxmlNamespacedMembrane(r) for r in ret]
                else:
                    return ret
            return wrapper

        return attr

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
    print(conv.list_attachment_filenames())
