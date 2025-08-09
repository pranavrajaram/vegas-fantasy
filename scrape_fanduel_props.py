import re, time, pandas as pd
from pathlib import Path
from playwright.sync_api import sync_playwright
import os

SEASON = "2025-26"
HEADLESS = os.getenv("HEADLESS", "1") == "1"
DEBUG = os.getenv("DEBUG", "0") == "1"

def log(msg):
    if DEBUG:
        print(f"[DEBUG] {msg}")

def gentle_wait(s=0.4): time.sleep(s)

# parsing div aria labels
def parse_market_labels(labels, market_text, prefix):
    pat = re.compile(
        rf'^(.+?) {re.escape(market_text)} {re.escape(SEASON)},\s*'
        r'(Over|Under)\s*([\d.]+),\s*([+\-]?\d+)\s*$'
    )
    out, skipped = {}, 0
    for label in labels:
        m = pat.match(label or "")
        if not m: skipped += 1; continue
        player, side, line, price = m.groups()
        d = out.setdefault(player, {
            f"{prefix}_line":"", f"{prefix}_over_price":"", f"{prefix}_under_price":""
        })
        d[f"{prefix}_line"] = line
        (d.__setitem__(f"{prefix}_over_price", price) if side=="Over"
         else d.__setitem__(f"{prefix}_under_price", price))
    log(f"Parsed {len(out)} players for '{market_text}'. Skipped: {skipped}")
    return out

# helper functions for scrolling and clicking
def list_frames(page):
    frames = page.frames
    log(f"Total frames: {len(frames)}")
    for i, fr in enumerate(frames):
        log(f"  [{i}] url={fr.url[:120]}")
    return frames

def auto_scroll_frame(frame, passes=15, pause=0.25):
    last_h = 0
    for i in range(passes):
        try:
            h = frame.evaluate("document.body.scrollHeight")
            frame.evaluate("window.scrollTo(0, document.body.scrollHeight)")
            gentle_wait(pause)
            nh = frame.evaluate("document.body.scrollHeight")
            log(f"[frame {frame.url[:60]}] scroll {i+1}: {h}->{nh}")
            if nh == last_h: break
            last_h = nh
        except Exception as e:
            log(f"scroll error: {e}"); break

def click_all_arrows_in_frame(frame):
    loc = frame.locator('[role="button"][data-testid="ArrowAction"]')
    try:
        cnt = loc.count()
    except Exception:
        cnt = 0
    log(f"[frame {frame.url[:60]}] ArrowAction count: {cnt}")
    clicks = 0
    for i in range(cnt):
        try:
            loc.nth(i).click(timeout=1000)
            clicks += 1
            gentle_wait(0.06)
        except Exception as e:
            log(f"  arrow {i} click fail: {e}")
    log(f"[frame {frame.url[:60]}] ArrowAction clicked: {clicks}")

def collect_labels_from_frames(page, market_text):
    labels = []
    for fr in page.frames:
        try:
            sel = f'div[aria-label*="{market_text} {SEASON}"]'
            got = fr.eval_on_selector_all(sel, "els => els.map(e => e.getAttribute('aria-label'))") or []
            if got:
                log(f"[frame {fr.url[:60]}] found {len(got)} labels for {market_text}")
            labels.extend([g for g in got if g])
        except Exception:
            pass

    return labels



# main scrape
def scrape_page(page, url, market_configs, tag):
    log(f"Go to {url}")
    page.goto(url, timeout=120_000)
    gentle_wait(3)

    # handle cookie/consent if present 
    for txt in ("Accept", "Agree", "I Agree", "Got it"):
        try:
            page.get_by_role("button", name=txt, exact=False).click(timeout=1500)
            log(f"Clicked consent '{txt}'")
            break
        except Exception:
            pass

    # list frames
    frames = list_frames(page)

    # scroll + expand inside each frame
    for fr in frames:
        auto_scroll_frame(fr, passes=12)
        click_all_arrows_in_frame(fr)
        auto_scroll_frame(fr, passes=6)


    # collect + parse
    markets_data = {}
    for market_text, prefix in market_configs:
        labels = collect_labels_from_frames(page, market_text)
        markets_data[prefix] = parse_market_labels(labels, market_text, prefix)

    # merge
    players = set()
    for d in markets_data.values():
        players |= set(d.keys())
    log(f"Unique players across markets: {len(players)}")

    rows = []
    for p in sorted(players):
        row = {"player": p, "season": SEASON, "source": "FanDuel"}
        for prefix, data in markets_data.items():
            vals = data.get(p, {})
            row[f"{prefix}_line"] = vals.get(f"{prefix}_line", "")
            row[f"{prefix}_over_price"] = vals.get(f"{prefix}_over_price", "")
            row[f"{prefix}_under_price"] = vals.get(f"{prefix}_under_price", "")
        rows.append(row)

    df = pd.DataFrame(rows)
    log(f"Rows built: {len(df)}")
    if not df.empty: log(df.head().to_string(index=False))
    return df

def safe_merge(a, b):
    if a is None or a.empty: return b if b is not None else pd.DataFrame()
    if b is None or b.empty: return a
    return a.merge(b, on=["player","season","source"], how="outer")

def main():
    with sync_playwright() as p:

        browser = p.chromium.launch(headless=HEADLESS, args=["--disable-blink-features=AutomationControlled"])
        context = browser.new_context(
            user_agent=(
              "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) "
              "AppleWebKit/537.36 (KHTML, like Gecko) "
              "Chrome/126.0.0.0 Safari/537.36"
            ),
            locale="en-US",
            timezone_id="America/New_York",
            geolocation={"latitude": 40.73, "longitude": -74.17},  
            permissions=["geolocation"],
            viewport={"width": 1400, "height": 1800},
        )
        page = context.new_page()

        rec_df = scrape_page(
            page,
            "https://sportsbook.fanduel.com/navigation/nfl?tab=receiving-props",
            [
                ("Regular Season Total Receiving Yards", "rec_yds"),
                ("Regular Season Total Receiving TDs", "rec_tds"),
            ],
            tag="receiving"
        )
        rec_df.to_csv("data/receiving_props.csv", index=False)
        print(f"Receiving rows: {len(rec_df)}")

        rush_df = scrape_page(
            page,
            "https://sportsbook.fanduel.com/navigation/nfl?tab=rushing-props",
            [
                ("Regular Season Total Rushing Yards", "rush_yds"),
                ("Regular Season Total Rushing TDs", "rush_tds"),
            ],
            tag="rushing"
        )
        rush_df.to_csv("data/rushing_props.csv", index=False)
        print(f"Rushing rows: {len(rush_df)}")


        pass_df = scrape_page(
            page,
            "https://sportsbook.fanduel.com/navigation/nfl?tab=passing-props",
            [
                ("Regular Season Total Passing Yards", "pass_yds"),
                ("Regular Season Total Passing TDs", "pass_tds"),
            ],
            tag="passing"
        )

        pass_df.to_csv("data/passing_props.csv", index=False)

        master = safe_merge(rec_df, rush_df)
        master = safe_merge(master, pass_df)
        master.to_csv("data/nfl_season_props_master.csv", index=False)
        print(f"Master rows: {len(master)}")

        browser.close()

if __name__ == "__main__":
    main()
