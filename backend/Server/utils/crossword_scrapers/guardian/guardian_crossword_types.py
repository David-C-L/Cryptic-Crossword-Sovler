from enum import Enum


class GuardianCrosswordType:

	def __init__(self, url_extension, rows, cols):
		self._url_extension = url_extension
		self._rows = rows
		self._cols = cols

	@property
	def rows(self):
		return self._rows

	@property
	def cols(self):
		return self._cols

	def get_url(self, puzzle_no: int):
		return f"https://www.theguardian.com/crosswords/{self._url_extension}/{puzzle_no}"


class GuardianCrosswordTypeEnum(Enum):
	EVERYMAN = GuardianCrosswordType("everyman", 15, 15)
	QUIPTIC = GuardianCrosswordType("quiptic", 15, 15)
	CRYPTIC = GuardianCrosswordType("cryptic", 15, 15)