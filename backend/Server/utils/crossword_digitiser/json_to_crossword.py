import json

from .crossword_puzzle.crossword_puzzle import CrosswordPuzzle


class CrosswordJsonProcessor:

    @staticmethod
    def crossword_from_json(json_string):
        """
        Takes in a JSON string containing data to build a Crossword Puzzle.
        Verifies the state of the data before building the crossword, after which
        it will verify that the clues align with the structure of the grid.
        :param json_string:
        :return: a fully built CrosswordPuzzle object
        """

        crossword_data = json.loads(json_string)

        num_rows, num_cols = CrosswordJsonProcessor.__verify_json_structure(crossword_data)

        crossword_puzzle = CrosswordPuzzle()

        crossword_puzzle.set_grid(num_rows, num_cols)

        # Extract the grid
        grid = crossword_data["grid"]

        for i, row in enumerate(grid):
            for j, cell in enumerate(row):
                if cell == "1":
                    crossword_puzzle.turn_cell_white(i, j)
                elif cell == "0":
                    crossword_puzzle.turn_cell_black(i, j)
                else:
                    raise InvalidJsonCrosswordDataError(f"Invalid input in cell. Row: {i} Col: {j}")

        # Extract the across clues
        across_clues = crossword_data["across"]

        for clue_no, clue_data in across_clues.items():
            crossword_puzzle.add_clue(int(clue_no), True, clue_data["clue"], clue_data["length"])

        # Extract the down clues
        down_clues = crossword_data["down"]

        for clue_no, clue_data in down_clues.items():
            crossword_puzzle.add_clue(int(clue_no), False, clue_data["clue"], clue_data["length"])

        # Verify the structure of the crossword puzzle
        crossword_puzzle.verify_and_sync()

        return crossword_puzzle

    @staticmethod
    def __verify_json_structure(data):
        """
        Takes in decoded JSON data and verifies that the structure is valid for the crossword solver
        :param data: the decoded JSON data
        :return: the number of rows and columns in the grid
        """

        # VERIFY THE GRID

        if "grid" not in data:
            raise InvalidJsonCrosswordDataError("Grid not found. Expected \"Grid\" in root-level data")

        grid = data["grid"]
        grid_error = InvalidJsonCrosswordDataError("Incorrect \"grid\" structure. Expected a 2D array of \"0s and \"1s")

        # Check that we have a populated list
        if type(grid) is not list or len(grid) < 1 or len(grid[0]) < 1:
            raise grid_error

        # Check that it's a 2D list containing 0s and 1s
        num_rows = len(grid)
        num_cols = len(grid[0])

        for row in grid:
            if (type(row) is not list) or (len(row) != num_cols) or any(cell not in ["0", "1"] for cell in row):
                raise grid_error

        # VERIFY ACROSS/DOWN CLUES

        if "across" not in data:
            raise InvalidJsonCrosswordDataError("Across clues not found. Expected \"across\" in root-level data")

        if "down" not in data:
            raise InvalidJsonCrosswordDataError("Down clues not found. Expected \"down\" in root-level data")

        across_clues = data["across"]
        down_clues = data["down"]

        CrosswordJsonProcessor.__verify_clue_structure(across_clues)
        CrosswordJsonProcessor.__verify_clue_structure(down_clues)

        # FINAL CHECKS

        # Check that only the 3 fields exist
        if type(data) is not dict and len(data) != 3:
            raise InvalidJsonCrosswordDataError("Superfluous root-level data: expected \"across\", \"down\", \"grid\"")

        return num_rows, num_cols

    @staticmethod
    def __verify_clue_structure(clues):
        """
        Verifies that the JSON structures of the across/down clues are correct
        :param clues: the mapping of clue numbers to clue data
        """

        clue_error = InvalidJsonCrosswordDataError(
            "Incorrect \"clues\" structure. "
            "Expected array of JSON objects of $CLUE_NO: {\"clue\": $CLUE, \"length\": [$LENGTH]}"
        )

        for clue_no, clue_data in clues.items():

            # Check that the clue_no is a positive integer
            if not clue_no.isdigit():
                raise InvalidJsonCrosswordDataError("Expected clue numbers to be positive integers")

            # Check that both "clue" and "length" is in the JSON object
            if "clue" not in clue_data or "length" not in clue_data:
                raise clue_error

            # Check if "clue" contains a string
            if type(clue_data["clue"]) is not str:
                raise clue_error

            # Check that the data types are correct in the "length" array
            if type(clue_data["length"]) is not list or any(type(length) is not int for length in clue_data["length"]):
                raise clue_error


class InvalidJsonCrosswordDataError(Exception):

    def __init__(self, msg: str):
        self.msg = msg

    def __str__(self):
        return self.msg
