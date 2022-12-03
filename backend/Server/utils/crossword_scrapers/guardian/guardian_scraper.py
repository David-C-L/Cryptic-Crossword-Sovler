from requests import get
from utils.crossword_scrapers.guardian.utils import get_grid_from_html, get_clues_from_html
from utils.crossword_scrapers.guardian.guardian_crossword_types import GuardianCrosswordTypeEnum
from utils.crossword_digitiser.crossword_puzzle.crossword_puzzle import CrosswordPuzzle


def scrape_cryptic_crossword(puzzle_no):
	return create_puzzle(GuardianCrosswordTypeEnum.CRYPTIC, puzzle_no)


def scrape_quiptic_crossword(puzzle_no):
	return create_puzzle(GuardianCrosswordTypeEnum.QUIPTIC, puzzle_no)


def scrape_everyman_crossword(puzzle_no):
	return create_puzzle(GuardianCrosswordTypeEnum.EVERYMAN, puzzle_no)


def create_puzzle(crossword_type: GuardianCrosswordTypeEnum, puzzle_no: int):

	crossword_type_info = crossword_type.value

	r = get(crossword_type_info.get_url(puzzle_no))
	cwd = CrosswordPuzzle()
	cwd.set_grid(15, 15)
	clue_locations = get_grid_from_html(r.text)
	(across, down) = get_clues_from_html(r.text)
	for key in across.keys():
		(clue_text, clue_length) = across[key]
		(row, col) = clue_locations[key]
		total_len = sum(clue_length)
		for i in range(total_len):
			cwd.turn_cell_white(row, col + i)
		cwd.add_clue(key, True, clue_text, clue_length)
	for key in down.keys():
		(clue_text, clue_length) = down[key]
		(row, col) = clue_locations[key]
		total_len = sum(clue_length)
		for i in range(total_len):
			cwd.turn_cell_white(row + i, col)
		cwd.add_clue(key, False, clue_text, clue_length)
	cwd.verify_and_sync()
	return cwd


if __name__ == "__main__":
	print(scrape_everyman_crossword(3069))
	print(scrape_quiptic_crossword(1000))
	print(scrape_cryptic_crossword(28605))
