from Server.utils.sample_data.test_dataset import CLUE_NOT_FOUND, findClue

OKGREEN = '\033[92m'
ENDC = '\033[0m'


def test_findsExistingOutput():
    assert not findClue('ask to support alternative comedy, mostly rot', [9]) == CLUE_NOT_FOUND
    print(OKGREEN + "Success: test_findsExistingOutput" + ENDC)


def test_outputContainsClueLengthAndAnswers():
    data = findClue('ask to support alternative comedy, mostly rot', [9])
    assert 'clue' in data
    assert 'length' in data
    assert 'answers' in data
    assert 'explanations' in data
    print(OKGREEN + "Success: test_outputContainsClueLengthAndAnswers" + ENDC)


def test_findsExistingOutputIgnoringCase():
    assert not findClue('ask to support ALTERNATIVE comedy, mostly rot', [9]) == CLUE_NOT_FOUND
    print(OKGREEN + "Success: test_findsExistingOutputIgnoringCase" + ENDC)


def test_doesNotFindNonexistentOutput():
    assert findClue('Not a Clue', [9]) == CLUE_NOT_FOUND
    print(OKGREEN + "Success: test_doesNotFindNonexistentOutput" + ENDC)


def test_incorrectLengthDoesNotFindOutput():
    assert findClue('ask to support alternative comedy, mostly rot', [-1]), CLUE_NOT_FOUND
    print(OKGREEN + "Success: test_incorrectLengthDoesNotFindOutput" + ENDC)
