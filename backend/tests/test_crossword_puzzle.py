import pytest

from Server.utils.crossword_digitiser.crossword_puzzle.crossword_puzzle import CrosswordPuzzle
from Server.utils.crossword_digitiser.crossword_puzzle.exceptions import *


# PUZZLE DEVELOPMENT TESTS

EXAMPLE_CLUE_TEXT = "Some forget to get here for gathering"
EXAMPLE_CLUE_LENGTH = [3, 8]

EXAMPLE_CLUE_TEXT_2 = "Composition from Bliss on a tape"
EXAMPLE_CLUE_LENGTH_2 = [6]


def test_grid_initialisation():

    expected_grid = [
        ['0', '0', '0'],
        ['0', '0', '0']
    ]

    crossword_puzzle = CrosswordPuzzle()
    crossword_puzzle.set_grid(rows=2, cols=3)

    puzzle_data = crossword_puzzle.export_crossword()

    assert expected_grid == puzzle_data["grid"]


def test_manipulating_uninitialised_grid():

    crossword_puzzle = CrosswordPuzzle()

    with pytest.raises(ManipulateUninitialisedGridError):
        crossword_puzzle.turn_cell_white(0, 0)


def test_grid_creation():

    expected_grid = [
        ['1', '1', '1'],
        ['0', '0', '1'],
        ['0', '0', '1']
    ]

    crossword_puzzle = CrosswordPuzzle()

    crossword_puzzle.set_grid(3, 3)

    crossword_puzzle.turn_cell_white(0, 0)
    crossword_puzzle.turn_cell_white(0, 1)
    crossword_puzzle.turn_cell_white(0, 2)
    crossword_puzzle.turn_cell_white(1, 2)
    crossword_puzzle.turn_cell_white(2, 2)

    puzzle_data = crossword_puzzle.export_crossword()

    assert expected_grid == puzzle_data["grid"]


def test_clue_addition():

    expected_across_clues = {
        1: {
            "clue": EXAMPLE_CLUE_TEXT,
            "length": EXAMPLE_CLUE_LENGTH,
            "answer": "",
            "explanation": "",
            "confidence": 0
        },
        4: {
            "clue": EXAMPLE_CLUE_TEXT_2,
            "length": EXAMPLE_CLUE_LENGTH_2,
            "answer": "",
            "explanation": "",
            "confidence": 0
        },
    }

    crossword_puzzle = CrosswordPuzzle()

    crossword_puzzle.add_clue(1, True, EXAMPLE_CLUE_TEXT, EXAMPLE_CLUE_LENGTH)
    crossword_puzzle.add_clue(4, True, EXAMPLE_CLUE_TEXT_2, EXAMPLE_CLUE_LENGTH_2)

    puzzle_data = crossword_puzzle.export_crossword()

    assert expected_across_clues == puzzle_data["across"]


def test_clue_removal():

    crossword_puzzle = CrosswordPuzzle()

    crossword_puzzle.add_clue(1, True, EXAMPLE_CLUE_TEXT, EXAMPLE_CLUE_LENGTH)
    crossword_puzzle.remove_clue(1, True)

    puzzle_data = crossword_puzzle.export_crossword()

    assert puzzle_data["across"] == {}


def test_add_duplicate_clue():

    crossword_puzzle = CrosswordPuzzle()

    with pytest.raises(ClueAlreadyExistsError):
        crossword_puzzle.add_clue(1, True, EXAMPLE_CLUE_TEXT, EXAMPLE_CLUE_LENGTH)
        crossword_puzzle.add_clue(1, True, EXAMPLE_CLUE_TEXT, EXAMPLE_CLUE_LENGTH)


def test_solve_nonexistent_clue():

    crossword_puzzle = CrosswordPuzzle()

    with pytest.raises(ClueDoesNotExistError):
        crossword_puzzle.solve_clue(1, True, "Example Answer", "Example Explanation")

# GRID VERIFICATION TESTS


def test_clue_length_does_not_match_grid():

    crossword_puzzle = CrosswordPuzzle()

    with pytest.raises(ClueLengthDoesNotMatchError):

        crossword_puzzle.set_grid(3, 3)

        crossword_puzzle.turn_cell_white(0, 0)
        crossword_puzzle.turn_cell_white(0, 1)
        crossword_puzzle.turn_cell_white(0, 2)

        crossword_puzzle.add_clue(1, True, "Example Answer", [2])

        crossword_puzzle.verify_and_sync()


def test_extra_clue_in_puzzle():

    crossword_puzzle = CrosswordPuzzle()

    with pytest.raises(UnexpectedClueError):
        crossword_puzzle.set_grid(3, 3)

        crossword_puzzle.turn_cell_white(0, 0)
        crossword_puzzle.turn_cell_white(0, 1)
        crossword_puzzle.turn_cell_white(0, 2)

        crossword_puzzle.add_clue(1, True, "Example Answer", [3])
        crossword_puzzle.add_clue(2, True, "Example Answer 2", [5])

        crossword_puzzle.verify_and_sync()


def test_correct_clue_pos():

    crossword_puzzle = CrosswordPuzzle()

    crossword_puzzle.set_grid(3, 3)

    crossword_puzzle.turn_cell_white(0, 0)
    crossword_puzzle.turn_cell_white(0, 1)
    crossword_puzzle.turn_cell_white(0, 2)
    crossword_puzzle.turn_cell_white(1, 2)
    crossword_puzzle.turn_cell_white(2, 2)
    crossword_puzzle.turn_cell_white(2, 0)
    crossword_puzzle.turn_cell_white(2, 1)

    crossword_puzzle.add_clue(1, True, "Example Clue", [3])
    crossword_puzzle.add_clue(2, False, "Example Clue 2", [3])
    crossword_puzzle.add_clue(3, True, "Example Clue 3", [3])

    crossword_puzzle.verify_and_sync()

    crossword_data = crossword_puzzle.export_crossword()
    expected_positions = {
        "0": {
            "0": 1,
            "2": 2,
        },
        "2": {
            "0": 3
        }
    }

    assert crossword_data["clue_no_pos"] == expected_positions

# ANSWER INPUT TESTS


def test_solve_with_wrong_length():

    crossword_puzzle = CrosswordPuzzle()

    with pytest.raises(AnswerDoesNotFitError):

        crossword_puzzle.set_grid(3, 3)

        crossword_puzzle.turn_cell_white(0, 0)
        crossword_puzzle.turn_cell_white(0, 1)
        crossword_puzzle.turn_cell_white(0, 2)

        crossword_puzzle.add_clue(1, True, "Example Answer", [3])

        crossword_puzzle.verify_and_sync()

        crossword_puzzle.solve_clue(1, True, "ANSWER", "Example Answer shouldn't fit")


def test_solve_with_non_alpha_char():

    crossword_puzzle = CrosswordPuzzle()

    with pytest.raises(AnswerFormatError):

        crossword_puzzle.set_grid(3, 3)

        crossword_puzzle.turn_cell_white(0, 0)
        crossword_puzzle.turn_cell_white(0, 1)
        crossword_puzzle.turn_cell_white(0, 2)

        crossword_puzzle.add_clue(1, True, "Example Answer", [3])

        crossword_puzzle.verify_and_sync()

        crossword_puzzle.solve_clue(1, True, "123", "Only alphabetical chars allowed")


def test_clashing_answers_in_grid():

    crossword_puzzle = CrosswordPuzzle()

    with pytest.raises(AnswerHasConflictingCharacterError):
        crossword_puzzle.set_grid(3, 3)

        crossword_puzzle.turn_cell_white(0, 0)
        crossword_puzzle.turn_cell_white(0, 1)
        crossword_puzzle.turn_cell_white(0, 2)
        crossword_puzzle.turn_cell_white(1, 2)
        crossword_puzzle.turn_cell_white(2, 2)

        crossword_puzzle.add_clue(1, True, "Example Clue", [3])
        crossword_puzzle.add_clue(2, False, "Example Clue 2", [3])

        crossword_puzzle.verify_and_sync()

        crossword_puzzle.solve_clue(1, True, "ABC", "")
        crossword_puzzle.solve_clue(2, False, "DEF", "D should clash with C")


def test_leave_correct_cells_when_clearing_input():

    expected_grid = [
        ['A', 'B', 'C'],
        ['0', '0', '1'],
        ['D', 'E', 'F']
    ]

    crossword_puzzle = CrosswordPuzzle()

    with pytest.raises(AnswerHasConflictingCharacterError):

        crossword_puzzle.set_grid(3, 3)

        crossword_puzzle.turn_cell_white(0, 0)
        crossword_puzzle.turn_cell_white(0, 1)
        crossword_puzzle.turn_cell_white(0, 2)
        crossword_puzzle.turn_cell_white(1, 2)
        crossword_puzzle.turn_cell_white(2, 2)
        crossword_puzzle.turn_cell_white(2, 0)
        crossword_puzzle.turn_cell_white(2, 1)

        crossword_puzzle.add_clue(1, True, "Example Clue", [3])
        crossword_puzzle.add_clue(2, False, "Example Clue 2", [3])
        crossword_puzzle.add_clue(3, True, "Example Clue 3", [3])

        crossword_puzzle.verify_and_sync()

        crossword_puzzle.solve_clue(1, True, "ABC", "")
        crossword_puzzle.solve_clue(3, True, "DEF", "")
        crossword_puzzle.solve_clue(2, False, "CDE", "E should clash with F")

    puzzle_data = crossword_puzzle.export_crossword()

    assert puzzle_data["grid"] == expected_grid
