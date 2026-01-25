from playwright.sync_api import sync_playwright
import sys
import time
import random

if len(sys.argv) < 2:
    raise ValueError("No URL provided")

url = sys.argv[1]

with sync_playwright() as p:
    browser = p.chromium.launch(
        headless=True  # set to False if needed
    )

    context = browser.new_context(
        user_agent=(
            "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) "
            "AppleWebKit/605.1.15 (KHTML, like Gecko) "
            "Version/17.0 Safari/605.1.15"
        ),
        viewport={"width": 1280, "height": 800},
        locale="en-US",
        timezone_id="America/New_York"
    )

    page = context.new_page()

    # Go to page like a human
    page.goto(url, wait_until="domcontentloaded", timeout=30000)

    # Human pause
    time.sleep(random.uniform(1.5, 3.0))

    # Scroll down gradually
    for _ in range(random.randint(2, 4)):
        page.mouse.wheel(0, random.randint(300, 700))
        time.sleep(random.uniform(0.8, 1.6))

    # Final pause before reading content
    time.sleep(random.uniform(1.0, 2.0))

    html = page.content()

    browser.close()

print(html)
