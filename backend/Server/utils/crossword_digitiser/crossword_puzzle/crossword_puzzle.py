from .utils import Grid, Clue, ClueMetadata
from .exceptions import *

from collections import OrderedDict
from typing import List


class CrosswordPuzzle:

    def __init__(self):
        self._grid: Grid = Grid()
        self._clues_across_map: OrderedDict[int, Clue] = OrderedDict()
        self._clues_down_map: OrderedDict[int, Clue] = OrderedDict()
        self._last_filled = {}
        self._clue_no_pos = {}

    def __str__(self):
        """
        Returns prettified string representing the crossword data stored in this class.
        """
        return ("\n============== GRID ==============\n"
                + str(self._grid)
                + "\n============== CLUES ==============\n"
                + "ACROSS:\n"
                + '\n'.join([f"{pos}. {clue}" for pos, clue in self._clues_across_map.items()])
                + "\nDOWN:\n"
                + '\n'.join([f"{pos}. {clue}" for pos, clue in self._clues_down_map.items()]))

    def __eq__(self, other):
        if isinstance(other, CrosswordPuzzle):
            return other.export_crossword() == self.export_crossword()
        return False

    def export_crossword(self, editable=False):
        """
        Returns the internal representation of the grid and accompanying clues
        :return: the grid as a 2D list of characters, across clues and down clues
        """
        across = {i: clue.to_dict() for (i, clue) in self._clues_across_map.items()}
        down = {i: clue.to_dict() for (i, clue) in self._clues_down_map.items()}
        return {
            'grid': self._grid.data,
            'across': across,
            'down': down,
            'last_filled': self._last_filled,
            'clue_no_pos': self._clue_no_pos,
            'editable': editable,
            'confidence_grid': self._grid.confidence_data
        }

    def import_crossword(self, data):

        # Expects that the data isn't missing any information

        # Handle Grid and Confidence Data

        num_rows = len(data["grid"])
        num_cols = len(data["grid"][0])

        self.set_grid(num_rows, num_cols)

        for i, row in enumerate(data["grid"]):
            for j, cell_value in enumerate(row):
                if cell_value == "0":
                    self.turn_cell_black(i, j)
                elif cell_value == "1":
                    self.turn_cell_white(i, j)
                else:
                    self.fill_cell(i, j, cell_value)
                    confidence = data["confidence_grid"][i][j]
                    if confidence < 0:
                        self.set_confidence(i, j, 1)
                    else:
                        self.set_confidence(i, j, data["confidence_grid"][i][j])

        # Handle clues

        # Across clues
        for clue_no, clue_data_dict in data["across"].items():
            clue_text = clue_data_dict["clue"]
            length = clue_data_dict["length"]
            answer = clue_data_dict["answer"]
            explanation = clue_data_dict["explanation"]
            confidence = clue_data_dict["confidence"]
            self.add_clue(clue_no, True, clue_text, length)

            if answer and explanation:
                self.solve_clue(clue_no, True, answer, explanation, confidence)

        # Down clues
        for clue_no, clue_data_dict in data["down"].items():
            clue_text = clue_data_dict["clue"]
            length = clue_data_dict["length"]
            answer = clue_data_dict["answer"]
            explanation = clue_data_dict["explanation"]
            confidence = clue_data_dict["confidence"]
            self.add_clue(clue_no, False, clue_text, length)

            if answer and explanation:
                self.solve_clue(clue_no, False, answer, explanation, confidence)

        # Handle last filled clue
        self._last_filled = data["last_filled"]

        # Handle clue number positions
        self._clue_no_pos = data["clue_no_pos"]

        # Verify that the components are correct
        self.verify_and_sync()

    def set_grid(self, rows: int, cols: int):
        """
        Sets up the grid for the crossword clue
        :param rows: number of rows in the grid
        :param cols: number of cols in the grid
        """
        self._grid.create_grid(rows, cols)

    def add_clue(self, clue_no: int, is_across: bool, clue_text: str, answer_len: List[int]):
        """
        Adds a clue to the crossword puzzle
        :param clue_no: clue number
        :param is_across: across or down clue
        :param clue_text: the provided clue
        :param answer_len: length of the answer
        """

        new_clue = Clue(clue_text, answer_len, (-1, -1), '', '', is_across, clue_no)

        clues_map = self._clues_across_map if is_across else self._clues_down_map

        if clue_no in clues_map:
            raise ClueAlreadyExistsError(clue_no, is_across)

        clues_map[clue_no] = new_clue

    def remove_clue(self, clue_no: int, is_across: bool):
        """
        Removes a clue from the crossword puzzle
        :param clue_no: clue number
        :param is_across: across or down clue
        """

        clues_map = self._clues_across_map if is_across else self._clues_down_map

        if clue_no not in clues_map:
            raise ClueDoesNotExistError(clue_no, is_across)

        del clues_map[clue_no]

    def get_clue_progress(self, clue_no: int, is_across: bool):
        """
        Returns the solving progress on the requested clue
        :param clue_no: clue number
        :param is_across: True if across clue, False if down clue
        :return: Clue string in unlikely letterpattern format
        """

        clue_map = self._clues_across_map if is_across else self._clues_down_map
        answer_len = sum(clue_map[clue_no].answer_len)
        (row, col) = clue_map[clue_no].pos
        clue_progress = ""
        for i in range(answer_len):
            val = self._grid.data[row][col]
            clue_progress += '?' if val == '1' else val
            row += 0 if is_across else 1
            col += 1 if is_across else 0
        return clue_progress

    def solve_clue(self, clue_no: int, is_across: bool, answer: str, explanation: str, confidence: float = 0):
        """
        Fills in a grid with a given answer to a clue
        :param clue_no: clue number
        :param is_across: True if across clue, False if down clue
        :param answer: answer
        :param explanation: the explanation to the answer provided
        :param confidence: confidence of an answer being correct, between 0 and 1
        """

        answer = answer.upper()

        clue_map = self._clues_across_map if is_across else self._clues_down_map

        if clue_no not in clue_map:
            raise ClueDoesNotExistError(clue_no, is_across)

        answer_len = len(answer)
        expected_answer_len = sum(clue_map[clue_no].answer_len)
        if answer_len != expected_answer_len:
            print(f"Throwing error: answer length is {answer_len} and expected was {expected_answer_len}")
            print(clue_map[clue_no].answer_len)
            raise AnswerDoesNotFitError(answer, expected_answer_len, len(answer))

        clue_pos_row, clue_pos_col = clue_map[clue_no].pos

        correct_cells = []
        filled = {}
        prev_confidence = []

        for i, char in enumerate(answer):

            current_row = clue_pos_row
            current_col = clue_pos_col

            if is_across:
                current_col += i
            else:
                current_row += i

            try:
                prev_confidence.append(self._grid.get_confidence_grid_cell(current_row, current_col))
                if current_row not in filled:
                    filled[current_row] = {}
                filled[current_row][current_col] = True
                self.fill_cell(current_row, current_col, char)
                self.set_confidence(current_row, current_col, confidence)

            # except
            except CellAlreadyFilledError as error:

                # Keep track of the cell if it was correctly filled
                if error.current_entry == error.attempted_entry:
                    correct_cells.append((error.row, error.col))
                    self.set_confidence(current_row, current_col, confidence)

                else:

                    # Revert the answer that was being inputted (every previous cell)
                    for j in range(i):

                        current_row = clue_pos_row
                        current_col = clue_pos_col

                        if is_across:
                            current_col += j
                        else:
                            current_row += j

                        # Clear the cell only if it is incorrect
                        if (current_row, current_col) not in correct_cells:
                            self.clear_cell(current_row, current_col)
                        self._grid.set_confidence_grid_cell(current_row, current_col, prev_confidence[j], False)

                    # Relay that the answer didn't work
                    raise AnswerHasConflictingCharacterError(clue_no, is_across, answer, i)
        clue_map[clue_no].explanation = explanation
        clue_map[clue_no].answer = answer
        clue_map[clue_no].confidence = confidence
        self._last_filled = filled

    def fill_cell(self, row: int, col: int, char: str):
        """
        Fills a cell in the grid
        :param row: row in the crossword
        :param col: column in the crossword
        :param char: character to fill in the grid
        """

        # Verify that the grid has been initialised
        if self._grid.data is None:
            raise ManipulateUninitialisedGridError

        # Verify that the input is a single character
        if not (len(char) == 1 and char.isalpha()):
            raise AnswerFormatError(char, row, col)

        char = char.upper()

        current_cell_value = self._grid.get_grid_cell(row, col)

        if current_cell_value == '1':
            # Cell is available: fill it in
            self._grid.fill_grid_cell(row, col, char)
        elif current_cell_value == '0':
            # Cell isn't meant to have a character - programmer error
            raise BlackCellModificationError(row, col)
        else:
            # A character is already in the grid
            raise CellAlreadyFilledError(char, current_cell_value, row, col)

    def clear_cell(self, row: int, col: int):
        """
        Clears a cell in the grid
        :param row: row in the crossword
        :param col: column in the crossword
        """

        # Verify that the grid has been initialised
        if self._grid.data is None:
            raise ManipulateUninitialisedGridError

        current_cell_value = self._grid.get_grid_cell(row, col)

        if current_cell_value == '0':
            # Cell isn't meant to have a character - programmer error
            raise BlackCellModificationError(row, col)

        self._grid.fill_grid_cell(row, col, '1')

    def turn_cell_white(self, row: int, col: int):
        """
        Turns a cell in the grid white (available)
        :param row: row in the crossword
        :param col: column in the crossword
        """

        # Verify that the grid has been initialised
        if self._grid.data is None:
            raise ManipulateUninitialisedGridError

        self._grid.set_grid_cell(row, col)

    def turn_cell_black(self, row: int, col: int):
        """
        Turns a cell in the grid white (unavailable)
        :param row: row in the crossword
        :param col: column in the crossword
        """

        # Verify that the grid has been initialised
        if self._grid.data is None:
            raise ManipulateUninitialisedGridError

        self._grid.clear_grid_cell(row, col)

    def clear_grid(self):

        if self._grid.data is None:
            raise ManipulateUninitialisedGridError

        for row in range(len(self._grid.data)):
            for col in range(len(self._grid.data)):
                if self._grid.get_grid_cell(row, col) != '0':
                    self._grid.set_grid_cell(row, col)

    def get_clue_text(self, clue):
        clue_pos_row, clue_pos_col = clue.pos

        clue_text = []

        current_row = clue_pos_row
        current_col = clue_pos_col

        for i in range(sum(clue.answer_len)):

            clue_text.append(self._grid.get_grid_cell(current_row, current_col))

            if clue.is_across:
                current_col += 1
            else:
                current_row += 1

        return clue_text

    def reset_clue(self, clue, original_text):
        clue_pos_row, clue_pos_col = clue.pos

        current_row = clue_pos_row
        current_col = clue_pos_col

        for i in range(sum(clue.answer_len)):

            self._grid.fill_grid_cell(current_row, current_col, original_text[i])

            if clue.is_across:
                current_col += 1
            else:
                current_row += 1

    def set_confidence(self, current_row, current_col, confidence):
        self._grid.set_confidence_grid_cell(current_row, current_col, confidence)

    def empty_last_filled(self):
        self._last_filled = {}

    def verify_and_sync(self):
        """
        Verifies that the grid structure matches the clues stored inside the puzzle.
        If successful, updates the clues to store information on where they are in the grid.
        """

        across_clues_metadata, down_clues_metadata = self.__get_metadata_all()

        self.__verify_clues(self._clues_across_map, across_clues_metadata, is_across=True)
        self.__verify_clues(self._clues_down_map, down_clues_metadata, is_across=False)
        self.__generate_neighbours()

    def __get_metadata_all(self):
        """
        Uses the grid to fill out the across/down maps with Clue objects with their
        corresponding metadata (position and length)
        :return: metadata for across and down clues
        """

        across_metadata = self.__get_metadata_set(is_across=True)
        down_metadata = self.__get_metadata_set(is_across=False)

        # Create and enumerate the clues

        clue_no = 1
        enumerated_across_metadata = {}
        enumerated_down_metadata = {}

        while across_metadata or down_metadata:

            # Assign the rest of the clue numbers
            if not across_metadata:
                while down_metadata:
                    clue_metadata = down_metadata.pop(0)
                    self.__add_clue_pos(clue_no, clue_metadata)
                    enumerated_down_metadata[clue_no] = clue_metadata
                    clue_no += 1
            elif not down_metadata:
                while across_metadata:
                    clue_metadata = across_metadata.pop(0)
                    self.__add_clue_pos(clue_no, clue_metadata)
                    enumerated_across_metadata[clue_no] = clue_metadata
                    clue_no += 1
            # Compare the positions of the across and down clues
            elif across_metadata[0] < down_metadata[0]:
                clue_metadata = across_metadata.pop(0)
                self.__add_clue_pos(clue_no, clue_metadata)
                enumerated_across_metadata[clue_no] = clue_metadata
                clue_no += 1
            elif across_metadata[0] > down_metadata[0]:
                clue_metadata = down_metadata.pop(0)
                self.__add_clue_pos(clue_no, clue_metadata)
                enumerated_down_metadata[clue_no] = clue_metadata
                clue_no += 1
            else:
                clue_metadata_across = across_metadata.pop(0)
                clue_metadata_down = down_metadata.pop(0)
                self.__add_clue_pos(clue_no, clue_metadata_across)
                enumerated_across_metadata[clue_no] = clue_metadata_across
                enumerated_down_metadata[clue_no] = clue_metadata_down
                clue_no += 1

        return enumerated_across_metadata, enumerated_down_metadata

    def __add_clue_pos(self, clue_no: int, clue_metadata: ClueMetadata):
        row = str(clue_metadata.pos[0])
        col = str(clue_metadata.pos[1])
        if row not in self._clue_no_pos:
            self._clue_no_pos[row] = {}
        self._clue_no_pos[row][col] = clue_no

    def __get_metadata_set(self, is_across: bool):
        """
        Generates a list of ClueMetadata based on the layout of a grid
        :param is_across: indicate metadata generation for either Across or Down clues
        :return: list of ClueMetadata
        """

        metadata_set = []

        if is_across:
            row_length = self._grid.length_cols()
            grid_data = self._grid.data
        else:
            row_length = self._grid.length_rows()
            grid_data = map(list, zip(*self._grid.data))

        for i, row in enumerate(grid_data):

            # Two pointers
            p1 = p2 = 0

            while p1 < row_length:
                if p1 == p2:
                    # Searching for a new word
                    if row[p1] != '0':
                        # 1st pointer is on a white cell
                        p2 += 1
                    else:
                        # 1st pointer is on a black cell
                        p1 += 1
                        p2 += 1
                else:
                    if p2 == row_length:
                        # End of row
                        word_length = p2 - p1
                        if word_length > 1:
                            # We've got a word, store the information
                            position = (i, p1)
                            metadata_set.append(ClueMetadata(position, word_length))
                        # 1st pointer set to 2nd (ending loop)
                        p1 = p2
                    else:
                        # Check if we have a white cell
                        if row[p2] != '0':
                            # White cell found
                            p2 += 1
                        else:
                            # End of row
                            word_length = p2 - p1
                            if word_length > 1:
                                # We've got a word, store the information
                                position = (i, p1)
                                metadata_set.append(ClueMetadata(position, word_length))

                            # 1st pointer set to 2nd
                            p1 = p2

        # Down clues will have their position flipped which must be corrected
        # i.e. position = (p1, i) instead of (i, p1)
        if not is_across:
            for metadata in metadata_set:
                metadata.pos = tuple(reversed(metadata.pos))

        # Sort metadata by position
        metadata_set.sort()

        return metadata_set

    def __iter__(self):
        clues = []
        for key in self._clues_across_map.keys():
            clue_dict = dict(self._clues_across_map[key])
            clue_dict["direction"] = "ACROSS"
            clue_dict["clue_num"] = key
            clues.append(clue_dict)
        for key in self._clues_down_map.keys():
            clue_dict = dict(self._clues_down_map[key])
            clue_dict["direction"] = "DOWN"
            clue_dict["clue_num"] = key
            clues.append(clue_dict)
        yield from {
            "grid_width": self._grid.length_cols(),
            "grid_length": self._grid.length_rows(),
            "clues": clues
        }.items()

    def __generate_neighbours(self):
        across_clues = self._clues_across_map
        down_clues = self._clues_down_map

        for across_clue in across_clues.values():
            for down_clue in down_clues.values():
                if across_clue.intersects(down_clue):
                    across_clue.add_neighbour(down_clue)
                    down_clue.add_neighbour(across_clue)

    @staticmethod
    def __verify_clues(clues_map, clues_metadata, is_across):
        """
        Checks if a map of enumerated Clues matches a map of enumerated ClueMetadata,
        and updates the Clues' positions if successful
        :param clues_map: map of clue numbers to clues
        :param clues_metadata: map of clue numbers to clue metadata
        """

        for clue_no, clue in clues_map.items():

            # Check if the grid expects a clue
            if clue_no not in clues_metadata:
                raise UnexpectedClueError(clue_no, is_across, clue.clue_text)

            clue_metadata = clues_metadata[clue_no]

            # Check if the grid and clue have matching lengths
            total_length = sum(clue.answer_len)
            if total_length != clue_metadata.length:
                raise ClueLengthDoesNotMatchError(clue_no, is_across, total_length, clue_metadata.length)

            # Clue is verified - sync the clue by storing its position from the metadata
            clue.pos = clue_metadata.pos
