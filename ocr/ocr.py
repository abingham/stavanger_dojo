from itertools import count, zip_longest
import unittest

BASIS = [" _     _  _     _  _  _  _  _ ",
         "| |  | _| _||_||_ |_   ||_||_|",
         "|_|  ||_  _|  | _||_|  ||_| _|"]


def grouper(iterable, n):
    "Group an iterable in n-sized chunks."
    args = [iter(iterable)] * n
    return zip_longest(*args)


def entry_to_glyphs(entry):
    "Convert a three-line entry into a sequence of glyphs."
    return zip(*[grouper(line, 3) for line in entry])

# A map of glyphs to digits
INDEX = {glyph: i for i, glyph in enumerate(entry_to_glyphs(BASIS))}


def input_to_entries(lines):
    "Parse line-oriented input into a sequence of three-line entries."
    lines = (line
             for idx, line in enumerate(lines)
             if idx % 4 != 3)
    return grouper(lines, 3)


def parse(lines):
    """Parse line-oriented input into strings.

    This is the droid you're looking for."""
    parsed = (entry_to_glyphs(e)
              for e in input_to_entries(lines))
    return ((INDEX[g] for g in entry) for entry in parsed)


def display(e):
    return ''.join(str(d) for d in e)


def checksum(digits):
    "Determine if digits have valid checksum."
    s = sum(i * d for i, d in zip(count(1), digits))
    return s % 11 == 0


#############
# Test suite

test_data = \
    " _     _  _     _  _  _  _ \n" \
    "| |  | _| _||_||_ |_   ||_|\n" \
    "|_|  ||_  _|  | _||_|  ||_|\n" \
    "\n" \
    "    _  _  _  _  _  _     _ \n" \
    "|_||_|| || ||_   |  |  ||_ \n" \
    "  | _||_||_||_|  |  |  | _|\n" \
    ""


class Tests(unittest.TestCase):
    def test_simple(self):
        results = list(parse(test_data.split('\n')))
        self.assertEqual(display(results[0]), '012345678')
        self.assertEqual(display(results[1]), '490067715')

    def test_checksum_fail(self):
        digits = [3, 4, 5, 8, 8, 2, 8, 6, 5]
        self.assertFalse(checksum(digits))

    def test_checksum_ok(self):
        digits = [1, 4, 5, 8, 8, 2, 9, 6, 5]
        self.assertTrue(checksum(digits))


def test():
    loader = unittest.TestLoader()
    suite = loader.loadTestsFromTestCase(Tests)
    results = unittest.TestResult()
    suite.run(results)
    return results

if __name__ == '__main__':
    unittest.main()
