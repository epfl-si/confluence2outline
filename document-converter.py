import collections
import colorsys
from functools import cached_property, wraps
import html
import json
import logging
import os
import re
import threading
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

def returns_block_of_blocks (method):
    """Method decorator for `apply` that enforces that it return
    a single block object, in which all sub-objects in `contents`
    are themselves block objects.
    """

    @wraps(method)
    def wrapped (self, element):
        retval = method(self, element)
        if is_outline_inline(retval):
            raise TypeError(f"Bad {method.__qualname__} return value: {retval}")
        if "content" in retval:
            retval["content"] = as_outline_blocks(retval["content"])
        return retval

    return wrapped


def is_outline_inline (transformed):
    """Sorry, couldn't resist."""
    return transformed["type"] in ("text", "image", "mention", "br")

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
        return TextTemplate.complaint(f"Unable to translate {tag(element)}")


class DocumentTemplate (Template):
    def admits (element):
        """Explicit construction only."""
        return False

    @returns_block_of_blocks
    def apply (self, document):
        return {
            "type": "doc",
            "content": self.apply_templates(document.etree.getchildren())
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
        yield from self.segmented(element.getchildren())

    def segmented (self, elements):
        rendered = self.apply_templates(elements)
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
    def apply (self, node):
        ret = {"type":"text", "text": str(node)}
        color = Cascade.of(node).color
        if color is not None and not (
                color.is_black() or
                color.is_confluence_visited_hyperlink()):
            _Complaints.add(f"Rendering Confluence color {color.as_string_orig} as-is")
            ret.setdefault("marks", []).append(dict(
                type="highlight",
                attrs=dict(color=color.as_hex_triple())))
        return ret

    @classmethod
    def complaint (cls, msg):
        _Complaints.add(msg)
        return {"type": "text", "text": f"(? {msg})"}


class ListTemplate (Template):
    @returns_block
    def apply (self, element):
        subnodes = element.getchildren()

        # ⚠ <ul><ul>...</ul></ul> and other platypi spotted in the wild!
        while (len(subnodes) == 1 and tag(subnodes[0]) == self.tag):
            subnodes = subnodes[0].getchildren()

        content = []
        for subnode in subnodes:
            subsubnodes = (subnode.getchildren() if tag(subnode) == "li"
                           else [subnode])
            content.append(dict(
                type="list_item",
                content=flatten(Template__p().segmented(subsubnodes))))

        ret = dict(type=self.block_type,
                   content=content)

        attrs = self.attrs(element)
        if attrs is not None:
            ret["attrs"] = attrs
        return ret

    def attrs (self, element):
        return None

class Template__ul (ListTemplate):
    block_type = "bullet_list"

class Template__ol (ListTemplate):
    block_type = "ordered_list"

    def attrs (self, element):
        return {"order":1}


class Template__br (Template):
    @returns_inlines
    def apply (self, element):
        if len(element.getparent().getchildren()) == 1:
            return []
        else:
            return { "type": "br" }


class Template__span (Template):
    pass    # Everything is looked “up” from below, see e.g. TextTemplate

class Cascade:
    """Poor man's CSS implementation."""

    _all = {}

    @classmethod
    def of (cls, xml_node):
        if xml_node not in cls._all:
            cls._all[xml_node] = cls(xml_node)
        return cls._all[xml_node]

    @classmethod
    def base (cls, node):
        if not hasattr(node, "xpath"):
            node = node.getparent()

        styled = node.xpath("ancestor::*[@style]")
        if len(styled) > 0:
            return cls.of(styled[0])
        else:
            return None

    def __init__ (self, xml_node):
        """Private constructor, call `of()` instead."""
        self.node = xml_node

    @property
    def proper_style (self):
        if not hasattr(self.node, "attrib"):
            return {}

        return dict(
            (s.strip()
            for s in (
                    chunk.split(":", 1) if ":" in chunk else (chunk, "")))
            for chunk in (
                    self.node.attrib.get("style", "")
                    .strip().strip(';').split(';')))

    @property
    def style (self):
        base = self.base(self.node)
        base_style = base.style if base is not None else {}
        return { **base_style, **self.proper_style }

    @property
    def color (self):
        color_string = self.style.get("color", None)
        return self.Color.parse(color_string) if color_string is not None else None

    class Color:
        def __init__(self, r: int, g: int, b: int):
            if not all(0 <= v <= 255 for v in (r, g, b)):
                raise ValueError("RGB values must be in the range 0–255.")
            self.r = r
            self.g = g
            self.b = b

        @classmethod
        def parse (cls, string):
            if string == "windowtext":
                # Frankly, your guess is as good as mine.
                return None

            self = cls.from_rgb(string)  # Meh.
            self.as_string_orig = string
            return self

        @classmethod
        def from_rgb (cls, rgb_string):
            match = re.fullmatch(r"rgb\(\s*(\d{1,3})\s*,\s*(\d{1,3})\s*,\s*(\d{1,3})\s*\)", rgb_string)
            if not match:
                raise ValueError(f"Invalid RGB string format: {rgb_string}")
            r, g, b = map(int, match.groups())
            return cls(r, g, b)

        @cached_property
        def hsv (self):
            return colorsys.rgb_to_hsv(self.r / 255, self.g / 255, self.b / 255)

        @property
        def saturation(self):
            return self.hsv[1]

        @property
        def value(self):
            return self.hsv[2]

        def as_hex_triple(self):
            return "#{:02x}{:02x}{:02x}".format(self.r, self.g, self.b)

        def is_confluence_visited_hyperlink (self):
            return self.r == 23 and self.g == 43 and self.b == 77

        def is_black (self):
            return self.r == self.g == self.b == 0


class AcstructuredmacroTemplate (Template):
    @classmethod
    def admits (cls, element):
        if cls.get_macro_name(element) is None:
            return False

        return cls.ac_macro == cls.get_macro_name(element)

    @classmethod
    def get_macro_name (self, element):
        if tag(element) == "ac:structured-macro":
            return element.xpath("./@ac:name")[0]

    @classproperty
    def ac_macro (cls):
        components = cls.nomenclature
        if (len(components) == 2
            and components[0] == "AcstructuredmacroTemplate"):
            return re.sub("_", "-", components[1])


class AcstructuredmacroTemplateDefault (AcstructuredmacroTemplate):
    priority = 5

    @classmethod
    def admits (cls, element):
        return cls.get_macro_name(element) is not None

    @returns_block
    def apply (self, element):
        return as_outline_block(
            TextTemplate.complaint(
                f"Unable to translate ac:structured-macro {self.get_macro_name(element)}"))


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
    @returns_block_of_blocks
    def apply (self, element):
        attrs = {"colspan":1,"rowspan":1,"alignment": None}
        # TODO: we could compute attrs["colwidth"] from
        # element.xpath("../../colgroup/col") or something.

        return dict(
            type=self.tag,
            attrs=attrs,
            content=self.apply_templates(element.getchildren()))

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
            href = str(element.xpath("./@href")[0])

        except IndexError:
            # 🤷‍♂️
            link_text = str(element.xpath('./text()')[0])
            href = "https://" + str(link_text).strip()

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
                return TextTemplate.complaint(
                    f"Unable to process hyperlink around {t}")

        return transformed


def is_text_node (node):
    return isinstance(node, _LxmlNamespacedTextNodeMembrane)


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
        try:
            return { "documentId": [d.uuid for d in self.documents],
                     "outline.document": [PythonStruct(DocumentTemplate().apply(d))
                                          for d in self.documents] }
        finally:
            _Complaints.flush()

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
        return list(flatten(str(f) for f in d.etree.xpath("//ri:attachment/@ri:filename")))


class _Complaints:
    def __init__ (self):
        self.complaints = {}

    _per_thread = threading.local()

    @classmethod
    def _the (cls):
        if not hasattr(cls._per_thread, "singleton"):
            cls._per_thread.singleton = cls()
        return cls._per_thread.singleton

    @classmethod
    def add (cls, msg):
        self = cls._the()
        self.complaints[msg] = self.complaints.get(msg, 0) + 1

    @classmethod
    def flush (cls):
        self = cls._the()
        for k in sorted(self.complaints, reverse=True,
                        key=lambda k: self.complaints[k]):
            logging.warning(f"{k} ({self.complaints[k]} times)")
        self.complaints.clear()


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
        return _LxmlNamespacedElementMembrane.of(
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
                f'xmlns:{k}="{v}"' for (k, v) in _LxmlNamespacedElementMembrane.xmlns.items()),
            confluence_xmlish)

        # ... We're not done yet. Confluence uses HTML entities, but
        # we don't want to decode &amp; etc. and bugger right through
        # our XML:
        def replace (match):
            return html.unescape(match[0])

        return cls._entities_for_html_unescape_re.sub(replace, namespaced)

    def xpath (self, xpath):
        return self.etree.xpath(xpath)


class _LxmlNamespacedMembraneBase:
    _instances = {}

    def __init__(self, delegate):
        """Private constructor, call `of()` instead."""
        self.delegate = delegate

    @classmethod
    def of (cls, delegate):
        cls_instances = cls._instances.setdefault(cls, {})
        if delegate not in cls_instances:
            cls_instances[delegate] = cls(delegate)
        return cls_instances[delegate]

    def getparent (self):
        return self.__class__.of(self.delegate.getparent())


class _LxmlNamespacedElementMembrane (_LxmlNamespacedMembraneBase):
    """Wrapper to paper over the XML namespace nonsense.

    - On the way in, auto-pass `namespaces={...}` to methods that care.
    - On the way out, un-James-Clark the `.tag` of wrapped objects.
    """
    def __init__(self, delegate):
        self.delegate = delegate

    xmlns = { k: f"urn:{k}" for k in ("ri", "ac") }

    @property
    def tag (self):
        return self.unclarkify(self.delegate.tag)

    def __repr__ (self):
        return re.sub("^<", "<~", repr(self.delegate))

    def getchildren (self):
        return [_LxmlNamespacedTextNodeMembrane.of(child)
                if is_text_node(child)
                else _LxmlNamespacedElementMembrane.of(child)
                for child in self.delegate.getchildren()]

    def xpath (self, _path, *args, **kwargs):
        kwargs["namespaces"] = self.xmlns
        ret = self.delegate.xpath(_path, *args, **kwargs)
        return [
            _LxmlNamespacedTextNodeMembrane.of(r)
            if hasattr(r, "splitlines")
            else _LxmlNamespacedElementMembrane.of(r)
            for r in ret
        ]

    @property
    def attrib (self):
        return dict((self.unclarkify(k), v) for (k, v) in self.delegate.attrib.items())

    @classmethod
    def unclarkify (cls, tag_name_in_james_clark_notation):
        return re.sub(r"{urn:([a-z]+)}", r"\1:", tag_name_in_james_clark_notation)

class _LxmlNamespacedTextNodeMembrane (_LxmlNamespacedMembraneBase):
    """Metaprogrammed object wrapper to paper over the XML namespace nonsense."""

    def __str__ (self):
        return str(self.delegate)


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
