#!/usr/bin/env python3
"""Election-night prices for the Israel page: Kalshi and Polymarket, current quotes and intraday paths.

Run every 15 minutes by .github/workflows/israel-live.yml; writes html/data/israel-live.json. Kalshi's API
does not allow browser requests, so its prices come only from this file; the page refreshes Polymarket
itself every minute. Stops updating after STOP.
"""
import json
import os
import sys
import time
import urllib.parse
import urllib.request
from datetime import datetime, timezone

OUT = os.path.join(os.path.dirname(__file__), "..", "html", "data", "israel-live.json")
START = int(datetime(2026, 10, 26, 22, 0, tzinfo=timezone.utc).timestamp())    # midnight, Israel time, Oct 27
STOP = datetime(2026, 11, 4, tzinfo=timezone.utc)
KB = "https://api.elections.kalshi.com/trade-api/v2"
UA = {"User-Agent": "Mozilla/5.0 (israel-live; mulysan.github.io)"}

# key -> (series, yes_sub_title)
KALSHI = {
    "pm_bibi": ("KXISRAELPM", "Benjamin Netanyahu"), "pm_eiz": ("KXISRAELPM", "Gadi Eizenkot"),
    "pm_ben": ("KXISRAELPM", "Naftali Bennett"), "pm_none": ("KXISRAELPM", "No one"),
    "largest_likud": ("KXISRAELKNESSET", "Likud"), "largest_yashar": ("KXISRAELKNESSET", "Yashar"),
    "blocmost_bibi": ("KXISRAELBLOCMOST", "Netanyahu bloc"), "bloc61_none": ("KXISRAELBLOCMAJ", "None"),
    "seat_winter": ("KXISRAELANYSEAT", "People of Israel"), "seat_hendel": ("KXISRAELANYSEAT", "Reservists–Economic Party"),
    "wasted7": ("KXISRAELTHRESH", "At least 7%"), "turnout56": ("KXISRAELTURNOUT", "At least 5,600,000"),
    "turnout58": ("KXISRAELTURNOUT", "At least 5,800,000"), "govt_likud": ("KXISRAELGOVT", "Likud"),
    "govt_yashar": ("KXISRAELGOVT", "Yashar"),
}
# key -> (event id, groupItemTitle or None for single-market events); price = first outcome
POLY = {
    "pm_bibi": ("81557", "Benjamin Netanyahu"), "pm_eiz": ("81557", "Gadi Eizenkot"), "pm_ben": ("81557", "Naftali Bennett"),
    "largest_likud": ("425728", "Likud"), "largest_yashar": ("425728", "Yashar"), "blocmost_bibi": ("1161446", None),
    "any61": ("1147434", None), "seat_winter": ("996047", "Amcha Yisrael"),
    "seat_hendel": ("996047", "Reservists and New Economic Party"),
}


def get(url, params=None, tries=6):
    if params:
        url += "?" + urllib.parse.urlencode(params)
    for i in range(tries):
        try:
            return json.load(urllib.request.urlopen(urllib.request.Request(url, headers=UA), timeout=60))
        except Exception as e:
            err = e
            time.sleep(2 ** i)
    print("failed", url, err, file=sys.stderr)
    return None


def mid(bid, ask, last):
    if bid is not None and ask is not None and 0 <= ask - bid <= 0.15:
        return round((bid + ask) / 2, 4)
    return last


def fnum(x):
    try:
        return float(x)
    except (TypeError, ValueError):
        return None


def thin(points, step=300):
    """Last value in each 5-minute slot."""
    out = {}
    for t, v in points:
        if v is not None:
            out[int(t) // step * step] = round(v, 4)
    return sorted(out.items())


def main():
    now = datetime.now(timezone.utc)
    if now > STOP:
        print("past stop date; not updating")
        return
    end = int(now.timestamp())
    res = {"updated": now.strftime("%Y-%m-%dT%H:%M:%SZ"), "kalshi": {}, "poly": {}, "hist": {}}
    series = {}
    for s in {s for s, _ in KALSHI.values()}:
        d = get(f"{KB}/markets", {"series_ticker": s, "limit": 200})
        series[s] = d.get("markets", []) if d else []
        time.sleep(0.3)
    for key, (s, sub) in KALSHI.items():
        m = next((m for m in series[s] if m.get("yes_sub_title") == sub), None)
        if not m:
            continue
        bid, ask, last = fnum(m.get("yes_bid_dollars")), fnum(m.get("yes_ask_dollars")), fnum(m.get("last_price_dollars"))
        res["kalshi"][key] = dict(ticker=m["ticker"], bid=bid, ask=ask, last=last, mid=mid(bid, ask, last),
                                  vol24=fnum(m.get("volume_24h_fp")), status=m.get("status"), result=m.get("result"))
        c = get(f"{KB}/series/{s}/markets/{m['ticker']}/candlesticks", {"start_ts": START, "end_ts": end, "period_interval": 1})
        pts = []
        for x in (c or {}).get("candlesticks", []):
            b = fnum((x.get("yes_bid") or {}).get("close_dollars"))
            a = fnum((x.get("yes_ask") or {}).get("close_dollars"))
            p = fnum((x.get("price") or {}).get("close_dollars"))
            pts.append((x["end_period_ts"], mid(b, a, p)))
        res["hist"]["k_" + key] = thin(pts)
        time.sleep(0.3)
    events = {}
    for key, (eid, title) in POLY.items():
        if eid not in events:
            events[eid] = get(f"https://gamma-api.polymarket.com/events/{eid}") or {}
        m = next((m for m in events[eid].get("markets", []) if title is None or m.get("groupItemTitle") == title), None)
        if not m:
            continue
        bid, ask, last = fnum(m.get("bestBid")), fnum(m.get("bestAsk")), fnum(m.get("lastTradePrice"))
        tok = json.loads(m["clobTokenIds"])[0]
        res["poly"][key] = dict(id=m["id"], token=tok, bid=bid, ask=ask, last=last, mid=mid(bid, ask, last),
                                vol24=fnum(m.get("volume24hr")), closed=m.get("closed"))
        h = get("https://clob.polymarket.com/prices-history", {"market": tok, "startTs": START, "endTs": end, "fidelity": 5})
        res["hist"]["p_" + key] = thin([(x["t"], x["p"]) for x in (h or {}).get("history", [])])
    os.makedirs(os.path.dirname(OUT), exist_ok=True)
    json.dump(res, open(OUT, "w"), separators=(",", ":"))
    print("wrote", OUT, len(res["kalshi"]), "kalshi", len(res["poly"]), "poly")


if __name__ == "__main__":
    main()
