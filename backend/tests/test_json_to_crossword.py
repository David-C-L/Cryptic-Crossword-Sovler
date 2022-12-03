import pytest

from Server.utils.crossword_digitiser.json_to_crossword import CrosswordJsonProcessor, InvalidJsonCrosswordDataError
from Server.utils.crossword_digitiser.crossword_puzzle.exceptions import *


def test_basic_json_input():

    CrosswordJsonProcessor.crossword_from_json(
        """
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
                    "clue": "Example",
                    "length": [6]
                }
            },
            "down": {
                "2": {
                    "clue": "Example2",
                    "length": [1,1,1,1,1]
                }
            }
        }
        """
    )


def test_semantically_incorrect_json_input():

    with pytest.raises(ClueLengthDoesNotMatchError):
        CrosswordJsonProcessor.crossword_from_json(
            """
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
                        "clue": "Example",
                        "length": [5]
                    }
                },
                "down": {
                    "2": {
                        "clue": "Example2",
                        "length": [1,1,1,1,1]
                    }
                }
            }
            """
        )


def test_syntactically_incorrect_json_input():

    with pytest.raises(InvalidJsonCrosswordDataError):
        CrosswordJsonProcessor.crossword_from_json(
            """
            {
                "grid": [
                    ["1", "1", "4", "1", "1", "1"],
                    ["0", "0", "1", "0", "0", "0"],
                    ["0", "0", "1", "0", "0", "0"],
                    ["0", "0", "^", "WRONG", "0", "0"],
                    ["0", "0", "1", "0", "0", "0"],
                    ["0", "0", "0", "0", "0", "0"]
                ],
                "across": {
                    "1": {
                        "clue": "Example",
                        "length": [6]
                    }
                },
                "down": {
                    "2": {
                        "clue": "Example2",
                        "length": [1,1,1,1,1]
                    }
                }
            }
            """
        )
