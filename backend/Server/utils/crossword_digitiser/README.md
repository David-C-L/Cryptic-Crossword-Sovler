# Crossword Digitiser

Digitises a crossword puzzle from images, using scripts provided in the `image_to_crossword.py` module.

Puzzle data is stored in a CrosswordPuzzle object, with exposed methods that can manipulate the grid and clues stored.

## Usage and Documentation

Running `main.py` will run a demo of the digitiser using images from the `crossword_digitiser/test_images` folder.

Public method documentation can primarily be found in the docstrings.

Run `pydoc -b` to browse the available methods in a legible format.

## Exceptions
User-defined Exceptions have been created for easier debugging/handling. The exception hierarchy can be found below.

```
CrosswordPuzzleError
├── PuzzleDevelopmentError
│   ├── ClueAlreadyExistsError
│   ├── ClueDoesNotExistError
│   └── BlackCellModificationError
├── GridVerificationError
│   ├── UnexpectedClueError
│   └── ClueLengthDoesNotMatchError
└── AnswerInputError
    ├── AnswerDoesNotFitError
    ├── AnswerFormatError
    └── InputClashesWithExistingEntryError
```