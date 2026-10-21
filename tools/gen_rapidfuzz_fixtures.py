# Writes tests/testthat/fixtures/rapidfuzz_scores.csv: reference scores from
# the Python rapidfuzz package for the fuzz_*() ports to be tested against.
#
#   pip install --target <dir> rapidfuzz
#   python tools/gen_rapidfuzz_fixtures.py <dir> tests/testthat/fixtures/rapidfuzz_scores.csv
#
# Strings are stored as dot-separated hexadecimal code points (an empty field
# is the empty string) so the file stays ASCII. Scores are written with 15
# significant digits.
import csv
import random
import sys

sys.path.insert(0, sys.argv[1])
import rapidfuzz  # noqa: E402
from rapidfuzz import fuzz  # noqa: E402
from rapidfuzz.utils import default_process  # noqa: E402

out_path = sys.argv[2]
CUTOFF = 60

SCORERS = [
    ("ratio", fuzz.ratio),
    ("partial_ratio", fuzz.partial_ratio),
    ("token_sort_ratio", fuzz.token_sort_ratio),
    ("token_set_ratio", fuzz.token_set_ratio),
    ("token_ratio", fuzz.token_ratio),
    ("partial_token_sort_ratio", fuzz.partial_token_sort_ratio),
    ("partial_token_set_ratio", fuzz.partial_token_set_ratio),
    ("partial_token_ratio", fuzz.partial_token_ratio),
    ("wratio", fuzz.WRatio),
    ("qratio", fuzz.QRatio),
]

NBSP, NEL, IDEO = "\u00a0", "\u0085", "\u3000"
cases = [
    ("this is a test", "this is a test!"),
    ("fuzzy wuzzy was a bear", "wuzzy fuzzy was a bear"),
    ("fuzzy was a bear", "fuzzy fuzzy was a bear"),
    ("fuzzy was a bear", "bear was a fuzzy"),
    ("New York Mets vs Atlanta Braves", "Atlanta Braves vs New York Mets"),
    ("fuzzy was a bear but not a dog", "fuzzy was a bear"),
    ("fuzzy was a bear but not a dog", "fuzzy was a bear but not a cat"),
    ("a certain string", "cetain"),
    ("MARTHA", "MARHTA"),
    ("DIXON", "DICKSONX"),
    ("New York Mets", "The New York Mets baseball club"),
    ("Acme Pty Ltd", "ACME Corporation"),
    ("", ""),
    ("", "abc"),
    ("abc", ""),
    ("   ", "   "),
    ("!!!", "???"),
    ("Hello, World!", "hello world"),
    ("snake_case", "snake case"),
    ("caf\u00e9", "cafe"),
    ("M\u00fcller", "MULLER"),
    ("\u0130stanbul", "istanbul"),
    ("\u023a", "\u2c65"),
    ("a" + NBSP + "b", "b a"),
    ("a" + NBSP + "b\u0100", "b\u0100 a"),
    ("a" + NEL + "b", "b" + NEL + "a"),
    ("x" + IDEO + "y", "y x"),
    ("\u6771\u4eac\u90fd", "\u4eac\u90fd"),
    ("\U0001F600 smile", "smile \U0001F600"),
    ("\u03a3\u03c3\u03c2", "\u03c3\u03c3\u03c3"),
    ("a" * 100, "a" * 99 + "b"),
    ("ab" * 40, "ba" * 45),
    ("the quick brown fox", "the the quick quick brown fox"),
    ("O'Brien-Smith", "obrien smith"),
    ("St. John's Rd", "Saint Johns Road"),
    ("ab", "xaby"),
    ("abcd", "abcd"),
]

rng = random.Random(20261016)
WORDS = ["new", "york", "mets", "yankees", "fuzzy", "wuzzy", "was", "a", "bear",
         "the", "smith", "john", "jon", "st", "street", "road", "rd", "co", "ltd",
         "caf\u00e9", "M\u00fcller", "\u0130stanbul", "\u03b1\u03b2\u03b3",
         "\u6771\u4eac", "\U0001F600", "O'Brien", "x_y", "A-B", "123", "1st"]
SPACES = [" ", " ", " ", "  ", "\t", NBSP, NEL, IDEO, "\u2003", "\n"]
ALPHA = "abcdeABCDE  ,.-_'\u00e9\u00c9\u0100\u0101\u03a3\u03c3" + NBSP


def word_string():
    k = rng.randint(0, 6)
    parts = []
    for i in range(k):
        parts.append(rng.choice(WORDS))
        if i < k - 1:
            parts.append(rng.choice(SPACES))
    s = "".join(parts)
    if rng.random() < 0.2:
        s = rng.choice(SPACES) + s
    if rng.random() < 0.2:
        s = s + rng.choice(SPACES) + "!"
    return s


def char_string(maxlen):
    return "".join(rng.choice(ALPHA) for _ in range(rng.randint(0, maxlen)))


def mutate(s):
    s = list(s)
    for _ in range(rng.randint(0, 4)):
        op = rng.random()
        if op < 0.3 and s:
            del s[rng.randrange(len(s))]
        elif op < 0.6:
            s.insert(rng.randint(0, len(s)), rng.choice(ALPHA))
        elif s:
            s[rng.randrange(len(s))] = rng.choice(ALPHA)
    return "".join(s)


for _ in range(110):
    a = word_string()
    cases.append((a, word_string() if rng.random() < 0.5 else mutate(a)))
for _ in range(70):
    a = char_string(12)
    cases.append((a, char_string(30) if rng.random() < 0.5 else mutate(a)))
for _ in range(16):
    a = char_string(rng.choice([70, 140]))
    b = mutate(a) + char_string(rng.choice([0, 40]))
    cases.append((b, a) if rng.random() < 0.5 else (a, b))
for _ in range(40):
    a = char_string(8)
    cases.append((a, char_string(6) + a + char_string(6)))


def encode(s):
    return ".".join("%X" % ord(c) for c in s)


def num(x):
    return "%.15g" % x


header = ["a", "b", "a_processed", "b_processed"]
for mode in ("raw", "proc"):
    header += ["%s_%s" % (name, mode) for name, _ in SCORERS]
    header += ["%s_%s" % (col, mode) for col in
               ("align_score", "src_start", "src_end", "dest_start", "dest_end")]
header += ["%s_cut%d" % (name, CUTOFF) for name, _ in SCORERS]

with open(out_path, "w", encoding="ascii", newline="") as f:
    w = csv.writer(f, lineterminator="\n")
    w.writerow(header)
    for a, b in cases:
        row = [encode(a), encode(b), encode(default_process(a)), encode(default_process(b))]
        for proc in (None, default_process):
            row += [num(scorer(a, b, processor=proc)) for _, scorer in SCORERS]
            al = fuzz.partial_ratio_alignment(a, b, processor=proc)
            row += [num(al.score), al.src_start, al.src_end, al.dest_start, al.dest_end]
        row += [num(scorer(a, b, score_cutoff=CUTOFF)) for _, scorer in SCORERS]
        w.writerow(row)

print("rapidfuzz", rapidfuzz.__version__, "rows", len(cases), file=sys.stderr)
