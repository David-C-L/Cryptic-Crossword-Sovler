from .. import MorseRequests
production = None


def set_config(config):
    global production
    production = config.PRODUCTION == 1


def find_clues(crossword):
    return crossword.__getattribute__("_clues_across_map"), crossword.__getattribute__("_clues_down_map")


def across_and_down_to_all_clues(across_clues, down_clues):
    tmp_across_clues = {(i, True): clue for i, clue in across_clues.items()}
    tmp_down_clues = {(i, False): clue for i, clue in down_clues.items()}
    all_clues = {}
    all_clues.update(tmp_across_clues)
    all_clues.update(tmp_down_clues)

    return all_clues
