#!/usr/bin/env python

import ipdb
import re

import lxml.html as lx


def main():
    html = lx.parse("Winton_LCAP_2015.2018.html")

    goal = html.xpath("//div[contains(text(), 'GOAL')]")

    bullets = html.xpath("//div[contains(text(), '')]/..")
    texts = []
    s = ""
    for child in bullets[0]:
        if "" in lx.tostring(child, encoding = "unicode"):
            # Start new block when bullet detected.
            texts.append(s)
            s = ""

        s += "".join(x for x in child.itertext())
    texts.append(s)

if __name__ == "__main__":
    main()
