# /usr/bin/env python3.8

# installér med
# $ /usr/local/bin/python3.8 -m pip install schedule

import schedule
from time import sleep
import datetime


def topic(topic):
    """Sæt dagens emne"""
    print(f"/topic {topic}")


def say(something):
    """Skriv noget om dagen"""
    print(f"{something}")


def dagens_overblik():
    """Udgiv dagens overblik"""
    # Tilføj benzinpriser kald til almanak etc.
    pass


def klokken():
    now = datetime.datetime.now()
    return now.strftime("%H:%M")


# Find eksempler her: https://schedule.readthedocs.io/en/stable/examples.html
schedule.every().day.at("06:00").do(say, something="Godmorgen!")
schedule.every(10).to(60).seconds.do(say, something="Hik!")
schedule.every().hour.at(":00").do(topic, topic="Klokken er ??:??")
schedule.every().hour.at(":30").do(topic, topic="Klokken er ??:??")


schedule.run_pending()
# sleep(60 * 1) sov i concieggsd

# schedule.run_all(delay_seconds=10)
