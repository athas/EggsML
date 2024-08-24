#!/usr/bin/env python
# encoding: utf-8
#
# Find Twitter-konti for de danske folketingsmedlemmer.  Husk at køre det her
# skrift en gang imellem, og forbedr det gerne.

import sys
import re
import random
import urllib
import urllib2

opener = urllib2.build_opener()
opener.addheaders = [('User-Agent', 'Mozilla/5.0')]

url_template = 'https://www.ft.dk/searchResults.aspx?sortln=desc&pageSize=100&pageNr={}'

member_urls = []
for i in range(2):
    url = url_template.format(i + 1)
    data = opener.open(url).read()
    member_urls.extend(
        ('http://www.ft.dk' + l for l in
         re.findall(r'/da/medlemmer/folketingetsmedlemmer/[^"\']+', data)))
member_urls = list(set(member_urls))

twitter_users = []
for url, i in zip(member_urls, range(len(member_urls))):
    print '[{:03d}/{}] Tjekker {}'.format(i + 1, len(member_urls), url)
    data = urllib.urlopen(url).read()
    s = data.split('Hjemmeside: ', 1)
    if len(s) == 1:
        continue
    s = s[1].split('</a>', 1)[0]
    homepage_url = re.search('href="(http.+?)"', s).group(1)
    if 'facebook' in homepage_url:
        continue
    print "'- Fandt hjemmeside", homepage_url
    try:
        data = opener.open(homepage_url).read()
    except Exception:
        continue
    m = re.search('<frame src="([^"]+)">', data)
    if m is not None:
        homepage_url = m.group(1)
        data = opener.open(homepage_url).read()
    users = re.findall('https?://twitter.com/([^/"]+)', data)
    users = list(filter(lambda user: not (user == 'share'
                                          or user.startswith('home')
                                          or user == 'intent' # ???
                                          or user.startswith('search')),
                        users))
    def fix(u):
        u = u.lower()
        t = '?lang=da'
        if u.endswith(t):
            u = u[:len(u) - len(t)]
        if u.startswith('@'):
            u = u[1:]
        return u
    users = [fix(u) for u in users]
    if not users:
        continue
    users = list(set(users))
    print "   '- Fandt TWITTER-BRUGERE", users
    twitter_users.extend(users)

print twitter_users
twitter_users = list(set(twitter_users))
with open('brugere', 'w') as f:
    for u in twitter_users:
        f.write('{}\n'.format(u))
