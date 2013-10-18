# -*- coding: utf-8 -*-
#
# Modtag nyttig viden fra den danske Wikipedia.

import sys
import re
import random
import itertools
import urllib

from decode_html_entities import decode_html_entities
from url_open import request


def get_search_url(term, base_url, search_url=None):
    if search_url is None:
        search_url = 'w/index.php?%s'
    return base_url + search_url \
        % urllib.urlencode([('fulltext', 'Search'), ('search', term.encode('utf-8'))])

def get_page_urls(term,
                  base_url,
                  link_regex=u'<div class=\'mw-search-result-heading\'><a href="(.+?)"',
                  search_url=None):
    data = request(get_search_url(term, base_url, search_url))
    if data is None:
        return
    urls = map(lambda u: base_url + u[1:], re.findall(link_regex, data))
    return urls

def get_paragraphs(page):
    paragraphs = re.findall(ur'<p>(.+?)</p>', page, re.DOTALL)
    paragraphs = map(lambda s: decode_html_entities(re.sub(ur'<.+?>|\[\d+\]', u'', s)),
                     paragraphs)
    return paragraphs

def get_sentences(paragraph, pre_rules=[], split_regex=ur'\. *(?=[A-Z])'):
    pre_rules_formatted = u''.join(
        map(lambda r: u'(?<! %s)' % r.rstrip('.').decode('utf-8', 'ignore').replace(u'.', ur'\.'),
            pre_rules))
    sentences = re.split(pre_rules_formatted + split_regex,
                         paragraph, re.DOTALL)
    return [s.replace(u'\n', u' ') for s in sentences]
