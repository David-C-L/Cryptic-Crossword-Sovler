from typing import List, Tuple


class Grid:

    def __init__(self):
        self._data = None
        self._confidence_data = None

    def __str__(self):
        return "\n".join(map(" ".join, self._data))

    def create_grid(self, rows: int, cols: int):
        """
        Creates an empty grid with specified dimensions
        :param rows: number of rows in the grid
        :param cols: number of columns in the grid
        """
        self._data = [['0'] * cols for _ in range(rows)]
        self._confidence_data = [[0.0] * cols for _ in range(rows)]

    @property
    def data(self):
        return self._data

    @property
    def confidence_data(self):
        return self._confidence_data

    def get_grid_cell(self, row: int, col: int):
        """
        Gets the value in a cell in the crossword grid
        :param row: row number in the grid
        :param col: column number in the grid
        :return: the value stored in the cell
        """
        return self._data[row][col]

    def get_confidence_grid_cell(self, row: int, col: int):
        """
        Gets the value in a cell in the crossword grid
        :param row: row number in the grid
        :param col: column number in the grid
        :return: the value stored in the cell
        """
        return self._confidence_data[row][col]

    def set_grid_cell(self, row: int, col: int):
        """
        Sets a cell in the crossword grid to 1 (white cell)
        :param row: row number in the grid
        :param col: column number in the grid
        """
        self._data[row][col] = '1'

    def clear_grid_cell(self, row: int, col: int):
        """
        Sets a cell in the crossword grid to 0 (black cell)
        :param row: row number in the grid
        :param col: column number in the grid
        """
        self._data[row][col] = '0'

    def fill_grid_cell(self, row: int, col: int, value: str):
        """
        Fills in a cell in the crossword grid using the given value
        :param row: row number in the grid
        :param col: column number in the grid
        :param value: a single character to be placed in the cell
        """
        self._data[row][col] = value.upper()

    def set_confidence_grid_cell(self, row: int, col: int, value: float, take_max=True):
        """
        Fills in a cell in the crossword grid using the given value
        :param row: row number in the grid
        :param col: column number in the grid
        :param value: a single character to be placed in the cell
        :param take_max: Indicates whether the max of both confidences should be taken
        """
        if take_max:
            self._confidence_data[row][col] = max(value, self._confidence_data[row][col])
        else:
            self._confidence_data[row][col] = value

    def length_rows(self):
        """
        Returns the number of rows in the grid
        :return: Number of rows in the grid
        """
        assert self._data is not None
        return len(self._data)

    def length_cols(self):
        """
        Returns the number of columns in the grid
        :return: Number of columns in the grid
        """
        assert self._data is not None
        return len(self._data[0])


class Clue:

    def __init__(self, clue_text: str, answer_len: List[int], pos: Tuple[int, int], explanation: str, answer: str, is_across: bool, clue_no: int):
        self.clue_text = clue_text
        self.answer_len = answer_len
        self.pos = pos
        self.explanation = explanation
        self.answer = answer
        self.is_across = is_across
        self.clue_no = clue_no
        self.neighbours = []
        self.confidence = 0

    def __str__(self):
        return f"{self.clue_text} ({','.join(map(str, self.answer_len))}), at {self.pos}"

    def __lt__(self, other):
        return self.pos < other.pos

    def __iter__(self):
        (row, col) = self.pos
        yield from {
            'clue_text': self.clue_text,
            'answer_length': self.answer_len,
            'pos': {
                'row': row,
                'col': col
            }
        }.items()

    def __eq__(self, other):
        if isinstance(other, Clue):
            return self.to_dict() == other.to_dict() \
                   and self.pos == other.pos \
                   and self.is_across == other.is_across
        return False

    def to_dict(self):
        return {
            'clue': self.clue_text,
            'length': self.answer_len,
            'explanation': self.explanation,
            'answer': self.answer,
            'confidence': self.confidence
        }

    def add_neighbour(self, neighbour):
        self.neighbours.append(neighbour)

    def intersects(self, other_clue):
        if self.is_across == other_clue.is_across:
            return False

        across_clue = other_clue if other_clue.is_across else self
        down_clue = other_clue if not other_clue.is_across else self

        across_start = across_clue.pos[0]
        down_start = down_clue.pos[1]
        x_start, x_end = across_clue.get_range()
        y_start, y_end = down_clue.get_range()

        return x_start <= down_start <= x_end and y_start <= across_start <= y_end

    def get_range(self):
        index = 1 if self.is_across else 0
        return self.pos[index], self.pos[index] + sum(self.answer_len)

    def list_to_dict(self):
        [row, col] = self.pos
        resp = {}
        length = sum(self.answer_len)
        for i in range(length):
            curr_row = row
            curr_col = col
            if self.is_across:
                curr_col += i
            else:
                curr_row += i

            if curr_row not in resp:
                resp[curr_row] = {}
            resp[curr_row][curr_col] = True
        return resp


class ClueMetadata:

    def __init__(self, pos: Tuple[int, int], length: int):
        self.pos = pos
        self.length = length

    def __lt__(self, other):
        return self.pos < other.pos
