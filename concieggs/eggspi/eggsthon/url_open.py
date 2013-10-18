# Hent URLs.

import urllib
import threading


class MozillaOpener(urllib.FancyURLopener):
    version = 'Mozilla/5.0 (X11; Linux x86_64; rv:24.0) Gecko/20100101 Firefox/24.0'
_opener = MozillaOpener()

def url_open(*xs, **kwds):
    return _opener.open(*xs, **kwds)

def request(urls):
    if not isinstance(urls, list):
        return _request(urls)
    rng = range(len(urls))
    results = [None for i in rng]
    threads = [None for i in rng]
    for i in rng:
        threads[i] = threading.Thread(target=_request_save, args=[urls[i], results, i])
        threads[i].start()
    for i in rng:
        threads[i].join()
    return results

def _request_save(url, results, i):
    results[i] = _request(url)
    
def _request(url):
    r = url_open(url)
    if r.getcode() != 200:
        return
    return r.read().decode('utf-8')
