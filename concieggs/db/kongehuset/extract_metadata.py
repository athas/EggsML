#!/usr/bin/env python3
#
# Run this program to extract metadata to metadata.json.

import collections
import json
import time

import requests

Item = collections.namedtuple('Item', ['url', 'title', 'type'])

def get_page(page):
    url = f'https://www.kongehuset.dk/umbraco/api/SearchApi/GetSearchResults?culture=da&currentPage={page}'
    req = requests.get(url)
    return req.json()

def extract_item(block):
    link = block['Link']
    return Item(url=link['Url'], title=link['Title'], type=block['Type'])

def extract_items(page):
    return [extract_item(block) for block in page['Items']]

items = []
page = get_page(1)
n_pages = page['TotalPages']
items.extend(extract_items(page))
print(f'1/{n_pages}')
for i in range(2, n_pages + 1):
    page = get_page(i)
    items.extend(extract_items(page))
    time.sleep(0.05)
    print(f'{i}/{n_pages}')

with open('metadata.json', 'w') as f:
    json.dump(items, f)
