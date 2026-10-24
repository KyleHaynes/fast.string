"""Generate the reference fixtures for html_unescape() and valid_id().

    tests/testthat/fixtures/html_unescape.csv  input and Python's
        html.unescape(input), both as hex-encoded UTF-8
    tests/testthat/fixtures/valid_id.csv  type, number and whether
        python-stdnum accepts it

Run from the repository root with python-stdnum importable:
    py -I tools/gen_text_fixtures.py [path containing stdnum]
"""

import csv
import html
import html.entities
import os
import random
import re
import sys

if len(sys.argv) > 1:
    sys.path.insert(0, sys.argv[1])
import stdnum
from stdnum import ean, iban, isbn, luhn
from stdnum.au import abn, acn, tfn
from stdnum.iso7064 import mod_97_10

OUT = os.path.join("tests", "testthat", "fixtures")
rng = random.Random(20261022)


def html_cases(n):
    names = sorted(html.entities.html5)
    pieces = ["&", "&#", "&#x", ";", " ", "\t", "\n", "<", "#", "a", "x", "0",
              "99999999999999999999", "é", "中", "\U0001F600", "amp",
              "not", "eacute"]
    numbers = [0, 9, 13, 65, 128, 129, 150, 159, 160, 0xB, 0x7F, 0xD800,
               0xFDD0, 0xFFFE, 0x1FFFF, 0x10FFFF, 0x110000, 10 ** 22]
    out = []
    for _ in range(n):
        parts = []
        for _ in range(rng.randint(1, 8)):
            r = rng.random()
            if r < 0.35:
                nm = rng.choice(names)
                if rng.random() < 0.3:
                    nm = nm[:rng.randint(1, len(nm))]
                parts.append("&" + nm)
            elif r < 0.5:
                parts.append("&#" + str(rng.choice(numbers)) +
                             rng.choice([";", "", "x"]))
            elif r < 0.6:
                parts.append("&#x%X" % rng.choice(numbers) + rng.choice([";", ""]))
            elif r < 0.63:
                parts.append("&" + "q" * rng.randint(30, 34))
            else:
                parts.append(rng.choice(pieces))
        out.append("".join(parts))
    return out


def digits(n):
    return "".join(rng.choice("0123456789") for _ in range(n))


def mutate(s):
    s = list(s)
    i = rng.randrange(len(s))
    if rng.random() < 0.6:
        s[i] = rng.choice("0123456789")
    elif i + 1 < len(s):
        s[i], s[i + 1] = s[i + 1], s[i]
    return "".join(s)


def id_cases(per_type):
    rows = []

    def add(kind, value, check):
        if rng.random() < 0.5:
            value = mutate(value)
        rows.append((kind, value, int(check(value))))

    for _ in range(per_type):
        b = digits(rng.randint(1, 18))
        add("luhn", b + luhn.calc_check_digit(b), luhn.is_valid)
        b = digits(9)
        add("abn", abn.calc_check_digits(b) + b, abn.is_valid)
        b = digits(8)
        add("acn", b + acn.calc_check_digit(b), acn.is_valid)
        b = digits(8)
        for d in "0123456789":
            if tfn.is_valid(b + d):
                add("tfn", b + d, tfn.is_valid)
                break
        b = digits(9)
        add("isbn", b + isbn._calc_isbn10_check_digit(b), isbn.is_valid)
        b = rng.choice(["978", "979", "977"]) + digits(9)
        add("isbn", b + ean.calc_check_digit(b), isbn.is_valid)
        b = digits(rng.choice([7, 11, 12, 13]))
        add("ean", b + ean.calc_check_digit(b), ean.is_valid)
    dat = os.path.join(os.path.dirname(stdnum.__file__), "iban.dat")
    structs = {}
    with open(dat, encoding="utf-8") as f:
        for line in f:
            m = re.match(r'^([A-Z]{2}) .*bban="([^"]*)"', line)
            if m:
                structs[m.group(1)] = m.group(2)
    classes = {"n": "0123456789", "a": "ABCDEFGHIJKLMNOPQRSTUVWXYZ",
               "c": "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ"}
    for _ in range(per_type):
        cc = rng.choice(sorted(structs))
        bban = "".join(rng.choice(classes[kind])
                       for count, kind in re.findall(r"(\d+)!([nac])", structs[cc])
                       for _ in range(int(count)))
        value = cc + mod_97_10.calc_check_digits(bban + cc) + bban
        add("iban", value, lambda s: iban.is_valid(s, check_country=False))
    return rows


def main():
    os.makedirs(OUT, exist_ok=True)
    with open(os.path.join(OUT, "html_unescape.csv"), "w", newline="") as f:
        w = csv.writer(f, lineterminator="\n")
        w.writerow(["input", "expected"])
        for s in html_cases(1500):
            w.writerow([s.encode("utf-8").hex(), html.unescape(s).encode("utf-8").hex()])
    with open(os.path.join(OUT, "valid_id.csv"), "w", newline="") as f:
        w = csv.writer(f, lineterminator="\n")
        w.writerow(["type", "x", "valid"])
        for row in id_cases(250):
            w.writerow(row)
    print("python-stdnum", stdnum.__version__)


if __name__ == "__main__":
    main()
