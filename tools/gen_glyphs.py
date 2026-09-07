"""Regenerate src/fplot_glyphs_data.f90 from DejaVu Sans.

The vector backends hand the string to the format and let the viewer find
the font. PNG has no such luxury: it has to turn text into filled outlines
itself, so the outlines have to be in the library.

DejaVu Sans is matplotlib's default face, so using it is what makes fplot's
PNG text land in the same place as matplotlib's. Regular, bold and oblique
are emitted, and they are the real faces rather than a regular one smeared
or sheared, which is what matplotlib draws too.

What is stored is the font's own representation: quadratic outlines in
integer font units. Every point is a delta from the one before it, coded as
a magnitude class through a fixed prefix code plus the raw bits under the
leading one, which is deflate's trick for distances. Whole outlines repeat
between faces -- DejaVu builds the Greek capitals out of the Latin ones --
so identical ones are stored once and the slots point at them. The slots
themselves are written for the code points the table has something for
rather than for the gaps too, an outline number is usually just "the next
one", and an advance width is a delta from the same character in the face
it is a slanted copy of. The bit stream is then written into the Fortran
source as one base85 string, and fplot_glyphs unpacks it on first use and
elevates the quadratics to the cubics the rendering API speaks.

    pixi run python tools/gen_glyphs.py
"""

import os
from pathlib import Path

import matplotlib
from fontTools.pens.recordingPen import DecomposingRecordingPen
from fontTools.ttLib import TTFont

OUT = Path(__file__).resolve().parent.parent / "src" / "fplot_glyphs_data.f90"
TTFDIR = Path(os.path.dirname(matplotlib.__file__)) / "mpl-data" / "fonts" / "ttf"

# The order here is the face numbering the library uses: regular, bold,
# oblique, bold oblique.
FACES = [
    "DejaVuSans.ttf",
    "DejaVuSans-Bold.ttf",
    "DejaVuSans-Oblique.ttf",
    "DejaVuSans-BoldOblique.ttf",
]

# The table is a few contiguous ranges of code points, each with the set
# of places inside it worth carrying. Gaps are left empty, which costs
# one slot each and keeps the lookup a subtraction.
#
# Latin-1 for text, and the symbols that turn up in axis labels: the
# degree sign a polar axes labels its angles with, and its neighbours.
# Greek because mathtext spells "\\alpha" and there is nothing else to
# draw it with.
GREEK = (set(range(0x391, 0x3AA)) | set(range(0x3B1, 0x3CA))) - {0x3A2}
BLOCKS = [
    (32, 255, set(range(32, 127)) | {0xB0, 0xB1, 0xB5, 0xD7, 0xF7}),
    (0x391, 0x3C9, GREEK),
]

VERB_MOVE, VERB_LINE, VERB_CUBIC, VERB_CLOSE = 1, 2, 3, 4

# Bits per slot record: an advance width and the index of the outline it
# draws, 0 for a blank. Both are checked against the data below.
ADV_BITS = 12
OUT_BITS = 10

# Code lengths for the fourteen magnitude classes, in symbol order. They
# are a Huffman code for the distribution DejaVu's deltas actually have,
# frozen here so that no table has to travel with the data; the encoder
# checks that they still form a complete code. fplot_glyphs carries the
# same fourteen numbers.
CLEN = [2, 10, 9, 8, 7, 6, 5, 4, 3, 2, 3, 4, 4, 10]

# Printable ASCII without the characters that are awkward inside a Fortran
# literal: the two quotes, the backslash some compilers read escapes from,
# and "&" and "!", which lfortran mistakes for a continuation and a comment
# even inside the quotes. That leaves 89; the first 85 are the alphabet.
ALPHABET = "".join(
    c for c in (chr(i) for i in range(33, 127)) if c not in "\"'\\&!"
)[:85]


def quad_contours(glyf, name):
    """The glyph as the font stores it: quadratic contours in font units.

    getCoordinates resolves composites, which matters because DejaVu draws
    the Greek capitals and the accented Latin ones as references to other
    glyphs.
    """
    g = glyf[name]
    if g.numberOfContours == 0:
        return []
    coords, end_pts, flags = g.getCoordinates(glyf)
    coords = list(coords)
    contours = []
    start = 0
    for end in end_pts:
        pts = []
        for x, y in coords[start:end + 1]:
            if int(x) != x or int(y) != y:
                raise SystemExit(f"{name}: non-integer coordinate {(x, y)}")
            pts.append((int(x), int(y)))
        if any(f & 0x80 for f in flags[start:end + 1]):
            raise SystemExit(f"{name}: cubic off-curve points, not quadratic")
        on = [bool(f & 0x01) for f in flags[start:end + 1]]
        contours.append((pts, on))
        start = end + 1
    return contours


def quad_to_cubic(p0, q, p2):
    """Exact degree elevation of a quadratic Bezier to a cubic."""
    c1 = (p0[0] + 2.0 / 3.0 * (q[0] - p0[0]), p0[1] + 2.0 / 3.0 * (q[1] - p0[1]))
    c2 = (p2[0] + 2.0 / 3.0 * (q[0] - p2[0]), p2[1] + 2.0 / 3.0 * (q[1] - p2[1]))
    return c1, c2


def contours_to_cubic(contours):
    """(verbs, points) for quadratic contours, the way fplot_glyphs does it.

    This is the reference for the Fortran in fplot_glyphs: a contour is
    turned so that it starts at an on-curve point, consecutive off-curve
    points have the on-curve point between them put back at their midpoint,
    and every quadratic is elevated to a cubic. main() checks it against
    what the font's own pen draws.
    """
    verbs: list[int] = []
    pts: list[tuple[float, float]] = []

    for cpts, con in contours:
        n = len(cpts)
        first = next((j for j in range(n) if con[j]), None)
        if first is None:
            # An all off-curve contour has its start implied halfway
            # between its last point and its first.
            start = ((cpts[0][0] + cpts[-1][0]) / 2.0,
                     (cpts[0][1] + cpts[-1][1]) / 2.0)
            seq = [(p, False) for p in cpts] + [(start, True)]
        else:
            start = cpts[first]
            seq = [(cpts[(first + j) % n], con[(first + j) % n])
                   for j in range(1, n)] + [(start, True)]

        cur = start
        verbs.append(VERB_MOVE)
        pts.append(start)

        i = 0
        while i < len(seq):
            p, on = seq[i]
            if on:
                # A line back to the start is what closing the contour
                # already means, so the last one is left out.
                if i < len(seq) - 1:
                    verbs.append(VERB_LINE)
                    pts.append(p)
                cur = p
                i += 1
            else:
                nxt, non = seq[i + 1]
                if non:
                    end = nxt
                    i += 2
                else:
                    end = ((p[0] + nxt[0]) / 2.0, (p[1] + nxt[1]) / 2.0)
                    i += 1
                c1, c2 = quad_to_cubic(cur, p, end)
                verbs.append(VERB_CUBIC)
                pts.extend([c1, c2, end])
                cur = end

        verbs.append(VERB_CLOSE)

    return verbs, pts


def pen_outline(glyphset, name):
    """(verbs, points) as the font's own pen draws them, for checking."""
    pen = DecomposingRecordingPen(glyphset)
    glyphset[name].draw(pen)

    verbs: list[int] = []
    pts: list[tuple[float, float]] = []
    cur = (0.0, 0.0)
    start = (0.0, 0.0)

    for op, args in pen.value:
        if op == "moveTo":
            cur = start = args[0]
            verbs.append(VERB_MOVE)
            pts.append(cur)
        elif op == "lineTo":
            cur = args[0]
            verbs.append(VERB_LINE)
            pts.append(cur)
        elif op == "curveTo":
            for i in range(0, len(args), 3):
                c1, c2, p = args[i], args[i + 1], args[i + 2]
                verbs.append(VERB_CUBIC)
                pts.extend([c1, c2, p])
                cur = p
        elif op == "qCurveTo":
            pl = list(args)
            if pl[-1] is None:
                pl = pl[:-1]
                mid = ((pl[0][0] + pl[-1][0]) / 2.0, (pl[0][1] + pl[-1][1]) / 2.0)
                cur = start = mid
                verbs.append(VERB_MOVE)
                pts.append(cur)
                pl = pl + [mid]
            for i in range(len(pl) - 1):
                q = pl[i]
                nxt = pl[i + 1]
                end = nxt if i == len(pl) - 2 else ((q[0] + nxt[0]) / 2.0,
                                                   (q[1] + nxt[1]) / 2.0)
                c1, c2 = quad_to_cubic(cur, q, end)
                verbs.append(VERB_CUBIC)
                pts.extend([c1, c2, end])
                cur = end
        elif op == "closePath":
            verbs.append(VERB_CLOSE)
            cur = start
        elif op == "endPath":
            pass
        else:
            raise SystemExit(f"unhandled pen op {op}")

    return verbs, pts


# --- the bit stream ---------------------------------------------------


def canonical(lengths):
    """Codes for a canonical prefix code, shortest and lowest symbol first."""
    order = sorted(range(len(lengths)), key=lambda s: (lengths[s], s))
    codes = {}
    code = 0
    prev = 0
    for sym in order:
        code <<= lengths[sym] - prev
        prev = lengths[sym]
        codes[sym] = code
        code += 1
    if sum(2.0 ** -length for length in lengths) != 1.0:
        raise SystemExit("CLEN is not a complete prefix code")
    return codes


CODES = canonical(CLEN)


def zigzag(v):
    """Signed to unsigned, keeping small magnitudes small."""
    return 2 * v if v >= 0 else -2 * v - 1


def unzigzag(u):
    return u // 2 if u % 2 == 0 else -(u + 1) // 2


class Writer:
    def __init__(self):
        self.bits: list[int] = []

    def bit(self, b):
        self.bits.append(b)

    def raw(self, value, n):
        for k in range(n - 1, -1, -1):
            self.bits.append((value >> k) & 1)

    def gamma(self, n):
        """Elias gamma: the length in zeros, then the number itself."""
        length = n.bit_length()
        self.raw(0, length - 1)
        self.raw(n, length)

    def delta(self, v):
        u = zigzag(v)
        cls = u.bit_length()
        if cls >= len(CLEN):
            raise SystemExit(f"delta {v} needs class {cls}")
        self.raw(CODES[cls], CLEN[cls])
        if cls > 1:
            self.raw(u - (1 << (cls - 1)), cls - 1)

    def bytes(self):
        pad = (-len(self.bits)) % 32
        bits = self.bits + [0] * pad
        return bytes(
            int("".join(str(b) for b in bits[i:i + 8]), 2)
            for i in range(0, len(bits), 8)
        )


class Reader:
    def __init__(self, blob):
        self.blob = blob
        self.pos = 0

    def bit(self):
        b = (self.blob[self.pos >> 3] >> (7 - (self.pos & 7))) & 1
        self.pos += 1
        return b

    def raw(self, n):
        v = 0
        for _ in range(n):
            v = 2 * v + self.bit()
        return v

    def gamma(self):
        zeros = 0
        while self.bit() == 0:
            zeros += 1
        return (1 << zeros) | self.raw(zeros)

    def delta(self):
        code = 0
        length = 0
        while True:
            code = 2 * code + self.bit()
            length += 1
            for sym, clen in enumerate(CLEN):
                if clen == length and CODES[sym] == code:
                    if sym == 0:
                        return 0
                    return unzigzag((1 << (sym - 1)) | self.raw(sym - 1))
            if length > max(CLEN):
                raise SystemExit("bad prefix code in blob")


def predicted_from(face):
    """How many faces back the advance widths are guessed from.

    The faces run regular, bold, oblique, bold oblique, so the face two
    back is the same weight standing upright, which in DejaVu is the same
    glyph sheared and has exactly the same widths. The second face has
    only the first to go on. Nothing is asserted: a bad guess just costs
    more bits than a good one.
    """
    return min(face, 2)


def encode(outlines, slots, present):
    """The whole table as one bit stream."""
    w = Writer()
    w.gamma(len(outlines))
    for contours in outlines:
        w.gamma(len(contours))
        for pts, _ in contours:
            w.gamma(len(pts))
        px = py = 0
        for pts, on in contours:
            for (x, y), o in zip(pts, on):
                w.bit(1 if o else 0)
                w.delta(x - px)
                w.delta(y - py)
                px, py = x, y

    nch = len(present)
    for p in present:
        w.bit(1 if p else 0)
    used = 0
    for face in range(len(slots) // nch):
        for i in range(nch):
            if not present[i]:
                continue
            adv, idx = slots[face*nch + i]
            if face == 0:
                w.raw(adv, ADV_BITS)
            else:
                w.delta(adv - slots[(face - predicted_from(face))*nch + i][0])
            # Outlines are numbered in the order they are first drawn, so
            # a slot usually wants the next number that has not been used.
            if idx == used + 1:
                used += 1
                w.bit(1)
            else:
                w.bit(0)
                w.raw(idx, OUT_BITS)
    return w.bytes()


def decode(blob, nch, nface):
    """The inverse of encode, so that the round trip can be checked."""
    r = Reader(blob)
    outlines = []
    for _ in range(r.gamma()):
        counts = [r.gamma() for _ in range(r.gamma())]
        contours = []
        px = py = 0
        for n in counts:
            pts, on = [], []
            for _ in range(n):
                on.append(r.bit() == 1)
                px += r.delta()
                py += r.delta()
                pts.append((px, py))
            contours.append((pts, on))
        outlines.append(contours)

    present = [r.bit() == 1 for _ in range(nch)]
    slots = [(0, 0)]*(nch*nface)
    used = 0
    for face in range(nface):
        for i in range(nch):
            if not present[i]:
                continue
            if face == 0:
                adv = r.raw(ADV_BITS)
            else:
                adv = slots[(face - predicted_from(face))*nch + i][0] + r.delta()
            if r.bit() == 1:
                used += 1
                idx = used
            else:
                idx = r.raw(OUT_BITS)
            slots[face*nch + i] = (adv, idx)
    return outlines, slots, present


def base85(blob):
    """Four bytes to five characters, big-endian, like Ascii85 or Z85."""
    out = []
    for i in range(0, len(blob), 4):
        v = int.from_bytes(blob[i:i + 4], "big")
        chunk = []
        for _ in range(5):
            chunk.append(ALPHABET[v % 85])
            v //= 85
        out.extend(reversed(chunk))
    return "".join(out)


# --- the module ------------------------------------------------------


def block_offsets():
    """Each block with the index it starts at in the per-face table."""
    out = []
    off = 0
    for first, last, _ in BLOCKS:
        out.append((first, last, off))
        off += last - first + 1
    return out


def literal(text, width=129):
    """The blob as one Fortran literal, continued inside the quotes.

    Each line ends with "&" and the next begins with one, so the newline
    and the indentation are not part of the string. 129 payload characters
    plus the two ampersands and the quote is the 132 column limit.
    """
    lines = [text[i:i + width] for i in range(0, len(text), width)]
    out = ['    character(len=*), parameter :: BLOB = &']
    for i, line in enumerate(lines):
        head = '"' if i == 0 else "&"
        tail = '"' if i == len(lines) - 1 else "&"
        out.append(f"{head}{line}{tail}")
    return out


def main() -> None:
    codes = [c for first, last, _ in BLOCKS for c in range(first, last + 1)]
    keep = set().union(*[k for _, _, k in BLOCKS])
    nch = len(codes)

    outlines: list[list] = []
    index: dict[str, int] = {}
    slots: list[tuple[int, int]] = []
    upem = asc = desc = xh = None

    for face in FACES:
        font = TTFont(TTFDIR / face)
        glyf = font["glyf"]
        hmtx = font["hmtx"]
        cmap = font.getBestCmap()
        glyphset = font.getGlyphSet()

        for code in codes:
            if code not in keep:
                slots.append((0, 0))
                continue
            name = cmap.get(code)
            if name is None:
                raise SystemExit(f"{face} has no glyph for U+{code:04X}")
            contours = quad_contours(glyf, name)

            # The quadratics have to draw what the font's own pen draws,
            # since that is what the library used to compile in.
            want = pen_outline(glyphset, name)
            got = contours_to_cubic(contours)
            if got[0] != want[0] or any(
                abs(a - b) > 1e-9 for p, q in zip(got[1], want[1]) for a, b in zip(p, q)
            ):
                raise SystemExit(f"{face} U+{code:04X}: outline walk disagrees")

            if contours:
                key = repr(contours)
                if key not in index:
                    outlines.append(contours)
                    index[key] = len(outlines)
                idx = index[key]
            else:
                idx = 0
            slots.append((hmtx[name][0], idx))

        if upem is None:
            upem = font["head"].unitsPerEm
            asc = font["hhea"].ascent
            desc = font["hhea"].descent
            # sxHeight only exists in OS/2 version 2 and later; DejaVu
            # predates it, so take it from the top of the "x" outline,
            # which is its definition.
            xh = max(pt[1] for pt in pen_outline(glyphset, cmap[ord("x")])[1])

    for adv, idx in slots:
        if adv >= 1 << ADV_BITS or idx >= 1 << OUT_BITS:
            raise SystemExit(f"slot ({adv}, {idx}) does not fit its bits")

    # The four faces cover the same code points, so which of them the
    # table has anything for is one bit each rather than one empty slot
    # each in every face.
    present = [c in keep for c in codes]

    blob = encode(outlines, slots, present)
    back = decode(blob, nch, len(FACES))
    if back != (outlines, slots, present):
        raise SystemExit("the blob does not decode to what went into it")

    text = base85(blob)
    ncont = sum(len(c) for c in outlines)
    npt = sum(len(p) for c in outlines for p, _ in c)

    L = [
        "! fplot_glyphs_data — DejaVu Sans outlines, generated by",
        "! tools/gen_glyphs.py. Do not edit.",
        "!",
        "! One base85 string holding the four faces as the font itself stores",
        "! them: quadratic contours in integer font units, deltas coded by",
        "! magnitude class, outlines that repeat between faces stored once,",
        "! and a slot table that leaves out what it can work out.",
        "! fplot_glyphs unpacks it on first use; the format is described",
        "! there and in the generator.",
        "",
        "module fplot_glyphs_data",
        "    use fplot_style, only: dp",
        "    implicit none",
        "    public",
        "",
        f"    real(dp), parameter :: EM = {float(upem)}_dp",
        f"    real(dp), parameter :: ASCENT = {float(asc)}_dp",
        f"    real(dp), parameter :: DESCENT = {float(desc)}_dp",
        f"    real(dp), parameter :: XHEIGHT = {float(xh)}_dp",
        "",
        f"    integer, parameter :: NCH = {nch}",
        f"    integer, parameter :: NFACE = {len(FACES)}",
        f"    integer, parameter :: NSLOT = {len(slots)}",
        f"    integer, parameter :: NOUT = {len(outlines)}",
        f"    integer, parameter :: NCONT = {ncont}",
        f"    integer, parameter :: NPT = {npt}",
        "",
        "    ! The code point ranges the table covers, and where each one",
        "    ! starts in the per-face table.",
        f"    integer, parameter :: NBLK = {len(BLOCKS)}",
    ]
    blocks = [(first, last, off + 1) for first, last, off in block_offsets()]
    for name, col in (("BLK_FIRST", 0), ("BLK_LAST", 1), ("BLK_OFF", 2)):
        vals = ", ".join(str(b[col]) for b in blocks)
        L.append(f"    integer, parameter :: {name}(NBLK) = [{vals}]")
    L += [
        "",
        "    character(len=*), parameter :: ALPHABET = &",
        f'        "{ALPHABET}"',
        "",
    ]
    L += literal(text)
    L += ["", "end module fplot_glyphs_data", ""]

    OUT.write_text("\n".join(L))
    print(
        f"wrote {OUT}: {len(outlines)} outlines, {npt} quadratic points, "
        f"{len(blob)} bytes, {len(text)} characters"
    )


if __name__ == "__main__":
    main()
