# -*- coding: utf-8 -*-

import sys
import re
import random
import itertools
import urllib
import urllib.parse

from eggsthon.decode_html_entities import decode_html_entities
from eggsthon.url_open import request


def get_search_url(term, base_url, search_url=None):
    if search_url is None:
        search_url = 'w/index.php?%s'
    return base_url + search_url \
        % urllib.parse.urlencode([('fulltext', 'Search'), ('search', term.encode('utf-8'))])

def get_page_urls(term,
                  base_url,
                  link_regex=u'<div class="mw-search-result-heading"><a href="(.+?)"',
                  search_url=None):
    data = request(get_search_url(term, base_url, search_url))
    if data is None:
        return []
    urls = map(lambda u: base_url + u[1:], re.findall(link_regex, data))
    return list(urls)

def get_paragraphs(page):
    paragraphs = re.findall(r'<p>(.+?)</p>', page, re.DOTALL)
    paragraphs = map(lambda s: decode_html_entities(re.sub(r'<.+?>|\[\d+\]', u'',
                                                           re.sub(r'<b>(.+?)</b>', r'*\1*', s))),
                     paragraphs)
    return paragraphs

def get_sentences(paragraph, pre_rules=[], split_regex=r'\. *(?=[A-Z])'):
    pre_rules_formatted = u''.join(
        map(lambda r: u'(?<! %s)' % r.rstrip('.').replace('.', r'\.'),
            pre_rules))
    sentences = re.split(pre_rules_formatted + u'(?<! \w)' + split_regex,
                         paragraph, re.DOTALL)
    return [s.replace(u'\n', u' ') for s in sentences]
