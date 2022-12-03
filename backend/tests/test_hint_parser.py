from utils.hint_parser.hint_parser import MorseHintParser, unlikely_hint_parser

# HINT DEVELOPMENT TESTS
# C stands for "Command", i.e. what the user must do
# I stands for "Indicator", i.e. what indicates C to the user
# R stands for "Recurse", i.e. a parse of another form is about to occur
# T stands for "Terminate", i.e. no further parsing required here
# S stands for "String", i.e. a string will be found in the explanation and used in the hints

MORSE_PARSER = MorseHintParser()


def test_parses_morse_CIT_hints():
    explanation = "CON(odd letters[crowns]) + DOR(reversal[backward] ROD=staff)"

    expected_hints = [
        "'crowns' means odd letters",
        "odd letters of 'crowns'",
        "'backward' means reversal",
        "'staff' means ROD",
        "reversal of ROD",
        "Put CON, and DOR together"
    ]

    assert expected_hints == MORSE_PARSER.parse(explanation)


def test_parses_morse_CIR_hints():
    explanation = "anagram[shredded] corset"

    expected_hints = [
        "'shredded' means anagram",
        "anagram of corset"
    ]

    assert expected_hints == MORSE_PARSER.parse(explanation)


def test_parses_morse_CISR_hints():
    explanation = "anagram[made from] OCELL(duplicate[two] (O=circles + CELL=circles))"

    expected_hints = [
        "'made from' means anagram",
        "'two' means duplicate",
        "duplicate reasoning to find two answers.",
        "'circles' means O",
        "'circles' means CELL",
        "Combine O and CELL",
        "anagram of OCELL"
    ]

    assert expected_hints == MORSE_PARSER.parse(explanation)


def test_parses_morse_CISRR_hints():
    explanation = "insert[round] FR=brother into AAID(a + AID=hand)"

    expected_hints = [
        "'round' means insert",
        "'brother' means FR",
        "Consider A",
        "'hand' means AID",
        "Put A, and AID together",
        "insert FR into AAID"
    ]

    assert expected_hints == MORSE_PARSER.parse(explanation)


def test_parses_unlikely_hints():
    explanation = "It seems shredded means we need to find an anagram. I believe 'escort' is an anagram of 'corset'. " \
                  "'escort' means companion too"

    expected_hints = [
        "It seems shredded means we need to find an anagram",
        " I believe 'escort' is an anagram of 'corset'",
        " 'escort' means companion too"
    ]

    assert expected_hints == unlikely_hint_parser(explanation)

