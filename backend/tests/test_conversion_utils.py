from collections import OrderedDict

from Server.utils.backtracker.conversion_utils import across_and_down_to_all_clues, find_clues
from Server.utils.crossword_digitiser.crossword_puzzle.utils import Clue
from Server.utils.crossword_digitiser.json_to_crossword import CrosswordJsonProcessor

# CROSSWORD CONVERSION DEVELOPMENT TESTS

TEST_DATA = """
    {
        "grid": [
            ["1", "1", "1", "1", "1", "1"],
            ["0", "0", "1", "0", "0", "0"],
            ["0", "0", "1", "0", "0", "0"],
            ["0", "0", "1", "0", "0", "0"],
            ["0", "0", "1", "0", "0", "0"],
            ["0", "0", "0", "0", "0", "0"]
        ],
        "across": {
            "1": {
                "clue": "Companion shredded corset",
                "length": [6]
            }
        },
        "down": {
            "2": {
                "clue": "Instrument made from two circles",
                "length": [5]
            }
        }
    }
    """
TEST_PUZZLE = CrosswordJsonProcessor.crossword_from_json(TEST_DATA)
TEST_CLUE_ACROSS = Clue("Companion shredded corset", [6], (0, 0), '', '', True, 1)
TEST_CLUE_DOWN = Clue("Instrument made from two circles", [5], (0, 2), '', '', False, 2)
TEST_CLUE_ACROSS.add_neighbour(TEST_CLUE_DOWN)
TEST_CLUE_DOWN.add_neighbour(TEST_CLUE_ACROSS)


def test_find_clues():
    expected = OrderedDict([(1, TEST_CLUE_ACROSS)]), OrderedDict([(2, TEST_CLUE_DOWN)])
    actual = find_clues(TEST_PUZZLE)

    assert expected == actual


def test_across_and_down_to_all_clues():
    expected = {
        (1, True): TEST_CLUE_ACROSS,
        (2, False): TEST_CLUE_DOWN
    }
    across, down = find_clues(TEST_PUZZLE)
    actual = across_and_down_to_all_clues(across, down)

    assert expected == actual
