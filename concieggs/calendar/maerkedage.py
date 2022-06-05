#!/usr/bin/env python3.8
# -*- coding: utf-8 -*-

# installér med
# $ /usr/local/bin/python3.8 -m pip install schedule
# TODO:
# 1. kald python filer i mappen `calendar/` så de kan opdateres uafhængigt af concieggsd
# 2. få topic til at virke
# 3. tilføj helligdage: se cmds/42royal
# 4. tilføj helligdage fra Helligdagreformen af 1770.

from time import sleep
import schedule
import datetime


def begivenhed(besked):
    """Send kontrolbesked"""
    PREFIX = "BEGIVENHED "
    print(f"{PREFIX}{besked}\n")


def angiv_emne(emne):
    """Sæt dagens emne"""
    # se eggspi/setTopic
    begivenhed(f"/topic {emne}")


def sig(noget):
    """Skriv noget om dagen"""
    begivenhed(f"{noget}")


def dagens_overblik():
    """Udgiv dagens overblik"""
    # Tilføj benzinpriser kald til almanak etc.
    pass


def klokken():
    now = datetime.datetime.now()
    return now.strftime("%H:%M")


# Find eksempler her: https://schedule.readthedocs.io/en/stable/examples.html
schedule.every().day.at("06:00").do(sig, noget="Godmorgen!")
schedule.every().day.at("22:00").do(sig, noget="Så er det godnat!")
schedule.every(42).to(2 * 42).seconds.do(sig, noget="Hik!")
# schedule.every().hour.at(":00").do(angiv_emne, emne="Klokken er ??:??")
# schedule.every().hour.at(":30").do(angiv_emne, emne=f"Klokken er {klokken()}")


while True:
    schedule.run_pending()
    sleep(10)
