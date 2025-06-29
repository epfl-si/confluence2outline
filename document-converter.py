import collections
from functools import cached_property, wraps
import html
import json
import os
import re
import types

from lxml import etree


class classproperty (object):
    def __init__(self, f):
        self.f = f
    def __get__(self, obj, owner):
        return self.f(owner)


def returns_inline (method):
    """Method decorator for `apply` that enforces that it return
    a single inline object.
    """

    @wraps(method)
    def wrapped (self, element):
        retval = method(self, element)
        if not is_outline_inline(retval):
            raise TypeError(f"Bad {method.__qualname__} return value: {retval}")
        return retval

    return wrapped

def returns_inlines (method):
    """Method decorator for `apply` that enforces that it return
    inline objects in the Outline sense.
    """

    @wraps(method)
    def wrapped (self, element):
        retval = flatten(method(self, element))
        for e in retval:
            if not is_outline_inline(e):
                raise TypeError(f"Bad {method.__qualname__} return value: {e}")
        return retval

    return wrapped

def returns_block (method):
    """Method decorator for `apply` that enforces that it return
    a single block object.
    """

    @wraps(method)
    def wrapped (self, element):
        retval = method(self, element)
        if is_outline_inline(retval):
            raise TypeError(f"Bad {method.__qualname__} return value: {retval}")
        return retval

    return wrapped

def returns_blocks (method):
    """Method decorator for `apply` that enforces that it return
    block objects.
    """

    @wraps(method)
    def wrapped (self, element):
        retval = flatten(method(self, element))
        for e in retval:
            if is_outline_inline(e):
                raise TypeError(f"Bad {method.__qualname__} return value: {e}")
        return retval

    return wrapped


def is_outline_inline (transformed):
    """Sorry, couldn't resist."""
    return transformed["type"] in ("text", "image", "mention")

def as_outline_block (transformed):
    if is_outline_inline(transformed):
        return { "type": "paragraph", "content": [transformed] }
    else:
        return transformed

def as_outline_blocks (transformed):
    return [as_outline_block(t)
            for t in flatten(transformed)]


def tag (thing):
    """Returns the tag (or approximation thereof) of an XML node,
    template or template instance."""
    if hasattr(thing, "tag"):
        return thing.tag
    elif is_text_node(thing):
        return "text()"
    elif isinstance(thing, Template) or issubclass(thing, Template):
        return "(No template tag)"
    else:
        raise TypeError(f"{thing} cannot have a tag")


class Template:
    priority = 10

    def __init__ (self, templates=None):
        self.templates = (
            templates if templates is not None
            else flatten(Template._all_subclasses()))

    @classmethod
    def _all_subclasses (cls):
        yield cls
        for subcls in cls.__subclasses__():
            yield from subcls._all_subclasses()

    @classproperty
    def nomenclature (cls):
        name = cls.__name__
        return name.split("__")

    @classproperty
    def tag (cls):
        components = cls.nomenclature
        if len(components) > 1 and components[0] == "Template":
            return ":".join(re.sub("_", "-", c) for c in components[1:])
        else:
            return None    # Your subclass won't auto-admit; you need
                           # to either call it explicitly, or override
                           # one or both of its its `tag` or `admits`
                           # methods.

    @classmethod
    def admits (cls, element):
        return tag(cls) == tag(element)

    def find_template (self, element):
        """Return the “best” template class (by priority) to render `element`.

        Among the construction-time templates (or all of them, if the optional
        constructor argument was not used), pick the ones that `.admits(element)`
        and return the one with highest `.priority`.
        """
        matching_templates = sorted([t for t in self.templates if t.admits(element)],
                                    key=lambda cls: - cls.priority)
        if len(matching_templates) == 0:
            raise ValueError(f"Unhandled element: {element}")

        return matching_templates[0]

    def apply_template (self, template_cls, element):
        return template_cls(templates=self.templates).apply(element)

    def apply_templates (self, element_or_elements):
        """Apply construction-time templates recursively.

        Select templates for each element in `element_or_elements`
        using the `find_template` method. Call `apply` on each to
        return a list of Outline-compliant dicts, which may in turn
        contain a list of more Outline-compliant dicts as the value of
        the `content` key.

        The `apply` method of Template subclasses should return a list of such dicts,
        or the empty list to indicate that the element ought to be discarded.
        """
        ret = []
        for element in flatten(element_or_elements):
            template_cls = self.find_template(element)
            processed = self.apply_template(template_cls, element)
            ret.extend(flatten(processed))

        return ret

    def apply (self, element):
        return self.apply_templates(element.getchildren())


class DefaultTemplate (Template):
    priority = 0    # All other classes have priority

    @classmethod
    def admits (cls, element):
        return True

    @returns_inline
    def apply (self, element):
        return {"type": "text",
                "text": f"(? Unable to translate {tag(element)})"}


class DocumentTemplate (Template):
    def admits (element):
        """Explicit construction only."""
        return False

    @returns_block
    def apply (self, document):
        return {
            "type": "doc",
            "content": as_outline_blocks(self.apply_templates(
                document.etree.getchildren()))
        }


class SplittableInlineContainerTemplate (Template):
    """Abstract base class for containers of inline elements, i.e.
    paragraphs and headings.

    Outline demands a strict separation of “block” elements (such as
    `"type": "paragraph"` or `"type": "bulleted_list"`) vs. “inline”
    elements (such as `"type": "text"` or `"type": "image"`; i.e. the
    ones that match the `SubparagraphContentNode` production in
    `test/outline-schema-reverse-engineereed.json`). There is
    no such stringent constraint in Confluence®'s HTML, e.g. a `<p>`
    can embed either an `<image>` or a `<br/>`.

    The `.apply()` method of this class makes sure to respect these
    constraints, e.g. `Template__p().apply()` may end up rendering
    multiple `"type": "paragraph"`s out of a single `<p> tag.
    """
    @returns_blocks
    def apply (self, element):
        def segmented (rendered):
            accumulated_inline_content = []

            def push_inline_content (content):
                accumulated_inline_content.extend(flatten(content))

            def flush_inline_content ():
                if len(accumulated_inline_content):
                    # TODO: we likely want to make a “peephole pass”
                    # on `content` here; at least eliminate blank text
                    # nodes, and maybe also weld together contiguous
                    # "type": "text"s that happen to be split in
                    # Confluence® for no good reason.
                    yield {
                        "type": self.outline_type,
                        # Be sure to copy, not alias!
                        "content": accumulated_inline_content[:]
                    }
                # ⚠ Do *not* perform assignment here! Python is not
                # smart enough for that.
                accumulated_inline_content[:] = []

            for piece in rendered:
                if is_outline_inline(piece):
                    push_inline_content(piece)
                else:
                    yield from flush_inline_content()
                    yield piece
            yield from flush_inline_content()

        rendered = self.apply_templates(element.getchildren())

        yield from segmented(rendered)


class HnTemplate (SplittableInlineContainerTemplate):
    outline_type = "heading"

    @classproperty
    def attrs (cls):
        return { "level": int(cls.nomenclature[-1][-1]) }

    @returns_blocks
    def apply (self, element):
        ret = super().apply(element)

        for heading in ret:
            if heading["type"] == self.outline_type:
                heading["attrs"] = self.attrs
        return ret


class Template__h1 (HnTemplate):
    pass
class Template__h2 (HnTemplate):
    pass
class Template__h3 (HnTemplate):
    pass
class Template__h4 (HnTemplate):
    pass
class Template__h5 (HnTemplate):
    pass
class Template__h6 (HnTemplate):
    pass


class Template__p (SplittableInlineContainerTemplate):
    outline_type = "paragraph"


class TextTemplate (Template):
    @classmethod
    def admits (cls, node):
        return is_text_node(node)

    @returns_inline
    def apply (self, element):
        return {"type":"text", "text": str(element)}


class AcstructuredmacroTemplate (Template):
    @classmethod
    def admits (cls, element):
        return (tag(element) == "ac:structured-macro" and
                cls.ac_macro == element.xpath("./@ac:name")[0])

    @classproperty
    def ac_macro (cls):
        components = cls.nomenclature
        if (len(components) == 2
            and components[0] == "AcstructuredmacroTemplate"):
            return re.sub("_", "-", components[1])


class AcstructuredmacroTemplate__table_plus (AcstructuredmacroTemplate):
    pass


class Template__ac__parameter (Template):
    def apply (self, element):
        return []


class Template__ac__rich_text_body (Template):
    pass


class Template__table (Template):
    @returns_block
    def apply (self, element):
        return dict(
            type="table",
            attrs=dict(layout=None),
            content=self.apply_templates(element.xpath("./tr")
                                         or element.xpath("./tbody/tr")))


class Template__tr (Template):
    @returns_block
    def apply (self, element):
        return dict(
            type="tr",
            content=self.apply_templates(element.xpath("./*[self::td or self::th]")))


class TableCellTemplate (Template):
    @returns_block
    def apply (self, element):
        attrs = {"colspan":1,"rowspan":1,"alignment": None}
        # TODO: we could compute attrs["colwidth"] from
        # element.xpath("../../colgroup/col") or something.

        return dict(
            type=self.tag,
            attrs=attrs,
            content=as_outline_blocks(self.apply_templates(
                element.getchildren())))

class Template__th (TableCellTemplate):
    tag = "th"

class Template__td (TableCellTemplate):
    tag = "td"


class Template__div (Template):
    pass


class Template__a (Template):
    @returns_inlines
    def apply (self, element):
        try:
            href = element.xpath("./@href")[0]

        except IndexError:
            # 🤷‍♂️
            link_text = str(element.xpath('./text()')[0])
            href = "https://" + link_text.strip()

        transformed = self.apply_templates(element.getchildren())

        for t in transformed:
            if t["type"] == "text":
                t["marks"] = [
                    { "type": "link",
                      "attrs": {
                          "href": href,
                          "title": None
                      }
                     }
                ]
            else:
                return {
                    "type": "text",
                    "text": f"(Unable to process hyperlink around {t})"
                }

        return transformed


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
                            flatten(documents_tibble["latest.version"]),
                            flatten(documents_tibble["body"]))
            if args[2] is not None]

    def get_converted_documents (self):
        return { "documentId": [d.uuid for d in self.documents],
                 "outline.document": [PythonStruct(DocumentTemplate().apply(d))
                                      for d in self.documents] }

    def list_attachments (self):
        ret = {"latest.version": [], "documentId": [], "filename": []}

        for d in self.documents:
            filenames = self._get_attachment_filenames(d)
            if len(filenames):
                ret["documentId"].append(d.uuid)
                ret["latest.version"].append(d.latest_version_uuid)
                ret["filename"].append(filenames)

        return ret

    def process (self, doc):
        return {
            "type": "doc",
            "content": Template().apply_templates(doc.etree.getchildren())
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
         'latest.version': ['77767cc0-c4ed-4688-b61c-c2aebf7e3218'],
         'body': [ open('test/data/services-etc.xml').read() ] })

    docs = conv.get_converted_documents()

    with open("test/data/document-converter-out.json", "w") as f:
        def convert (wat):
            return wat.struct
        json.dump(conv.get_converted_documents(), f, default=convert)

    print(docs)
    print(conv.list_attachments())
