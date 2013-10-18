# Decode HTML entities.

import re
from htmlentitydefs import name2codepoint
from functools import partial


_int16 = partial(int, base=16)

_entity_text = re.compile('&(%s);' % '|'.join(name2codepoint))
_entity_base10 = re.compile('&#(\d+);')
_entity_base16 = re.compile('&#x([0-9a-fA-F]+);')

_entity_text_decode = lambda m: unichr(name2codepoint[m.group(1)])
_entity_base10_decode = lambda m: unichr(int(m.group(1)))
_entity_base16_decode = lambda m: unichr(_int16(m.group(1)))

_entity_text_sub = partial(_entity_text.sub, _entity_text_decode)
_entity_base10_sub = partial(_entity_base10.sub,
                             _entity_base10_decode)
_entity_base16_sub = partial(_entity_base16.sub,
                             _entity_base16_decode)

def decode_html_entities(text):
    """Decode html entities."""
    return _entity_base10_sub(_entity_base16_sub(_entity_text_sub(text)))
