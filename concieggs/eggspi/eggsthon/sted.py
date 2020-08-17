# -*- coding: utf-8 -*-

import requests

KEY = "Am0UsRLD8vgF9F5jKMHI8uNBSVKiry6rzTqbQXWnG2xHa30YoCEV6cSynDE_oGrM"

def get_coords(name):
    place_url = "http://dev.virtualearth.net/REST/v1/Locations"
    payload = {"q": name, "key": KEY}
    data = requests.get(place_url, params=payload).json()
    cs = data["resourceSets"][0]["resources"][0]["point"]["coordinates"]
    return (cs[0], cs[1])
