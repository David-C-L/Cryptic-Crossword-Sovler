class CrosswordPuzzleError(Exception):
    pass


class PuzzleDevelopmentError(CrosswordPuzzleError):
    pass


class ManipulateUninitialisedGridError(PuzzleDevelopmentError):

    def __str__(self):
        return f"Attempted to manipulate a CrosswordPuzzle without initialising the grid"


class ClueAlreadyExistsError(PuzzleDevelopmentError):

    def __init__(self, clue_no: int, is_across: bool):
        self.clue_no = clue_no
        self.clue_type = "ACROSS" if is_across else "DOWN"

    def __str__(self):
        return f"{self.clue_no} {self.clue_type} already exists"


class ClueDoesNotExistError(PuzzleDevelopmentError):

    def __init__(self, clue_no: int, is_across: bool):
        self.clue_no = clue_no
        self.clue_type = "ACROSS" if is_across else "DOWN"

    def __str__(self):
        return f"{self.clue_no} {self.clue_type} does not exist"


class BlackCellModificationError(PuzzleDevelopmentError):

    def __init__(self, row: int, col: int):
        self.row = row
        self.col = col

    def __str__(self):
        return f"Attempted to modify a black cell. Row: {self.row} Col: {self.col}"


class GridVerificationError(CrosswordPuzzleError):
    pass


class UnexpectedClueError(GridVerificationError):

    def __init__(self, clue_no: int, is_across: bool, clue_text: str):
        self.clue_no = clue_no
        self.clue_type = "ACROSS" if is_across else "DOWN"
        self.clue_text = clue_text

    def __str__(self):
        return f"Grid Verification Error: grid does not support {self.clue_no} {self.clue_type} ({self.clue_text})"


class ClueLengthDoesNotMatchError(GridVerificationError):

    def __init__(self, clue_no: int, is_across: bool, total_answer_len: int, expected_len: int):
        self.clue_no = clue_no
        self.clue_type = "ACROSS" if is_across else "DOWN"
        self.total_answer_len = total_answer_len
        self.expected_len = expected_len

    def __str__(self):
        return f"Grid Verification Error: Answer length of {self.clue_no} {self.clue_type}  does not match grid.\n" \
               f"Grid Expected: {self.expected_len} Received: {self.total_answer_len}"


class AnswerInputError(CrosswordPuzzleError):
    pass


class AnswerDoesNotFitError(AnswerInputError):

    def __init__(self, answer: str, expected_len: int, received_len: int):
        self.answer = answer
        self.expected_len = expected_len
        self.received_len = received_len

    def __str__(self):
        return f"{self.answer} doesn't fit in the grid. Expected: {self.expected_len} Received: {self.received_len}"


class AnswerFormatError(AnswerInputError):

    def __init__(self, cell_input: str, row: int, col: int):
        self.cell_input = cell_input
        self.row = row
        self.col = col

    def __str__(self):
        return f"The input wasn't a single character. Input: {self.cell_input} Row: {self.row} Col: {self.col}"


class CellAlreadyFilledError(AnswerInputError):

    def __init__(self, attempted_entry: str, current_entry: str, row: int, col: int):
        self.attempted_entry = attempted_entry
        self.current_entry = current_entry
        self.row = row
        self.col = col

    def __str__(self):
        return f"Input clashes with existing entry. Attempted: {self.attempted_entry} Current: {self.current_entry} " \
               f"at Row: {self.row} Col: {self.col}"


class AnswerHasConflictingCharacterError(AnswerInputError):

    def __init__(self, clue_no: int, is_across: bool, answer_text: str, conflict_pos: int):
        self.clue_no = clue_no
        self.clue_type = "ACROSS" if is_across else "DOWN"
        self.answer_text = answer_text
        self.conflict_pos = conflict_pos

    def __str__(self):
        return f"Answer of {self.answer_text} does not fit for {self.clue_no} {self.clue_type}. " \
               f"Conflict with the existing grid at character {self.conflict_pos}"
