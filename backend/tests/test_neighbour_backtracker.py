from utils import MorseRequests
from utils.backtracker.neighbour_backtracker import solve_crossword
from utils.crossword_digitiser.json_to_crossword import CrosswordJsonProcessor

import Server.config as config

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


def test_can_solve_correctly():
    MorseRequests.set_config(config.Config(), False)

    TEST_PUZZLE = CrosswordJsonProcessor.crossword_from_json(TEST_DATA)
    expected = [
        ['E', 'S', 'C', 'O', 'R', 'T'],
        ['0', '0', 'E', '0', '0', '0'],
        ['0', '0', 'L', '0', '0', '0'],
        ['0', '0', 'L', '0', '0', '0'],
        ['0', '0', 'O', '0', '0', '0'],
        ['0', '0', '0', '0', '0', '0']
    ]
    solve_crossword(TEST_PUZZLE, publish_sse=False)

    assert expected == TEST_PUZZLE.export_crossword()['grid']
