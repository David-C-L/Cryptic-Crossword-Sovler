import re
import time
from random import Random

from .conversion_utils import find_clues, across_and_down_to_all_clues
from .. import MorseRequests
from ..crossword_digitiser.crossword_puzzle.crossword_puzzle import CrosswordPuzzle
from ..crossword_digitiser.crossword_puzzle.exceptions import *
from flask_sse import sse


def morse_explanation(s):
    return '\n' not in s


def query(clue, pattern):
    query = MorseRequests.solve_single_clue_all(clue.clue_text, clue.answer_len, pattern=pattern)
    query_results = query['results']
    return query_results


def backtrack_solve_crossword_from_clues(crossword: CrosswordPuzzle, all_clues, next,
                                         require_confidence=False, publish_sse=True):
    unsolved = []
    while len(next) > 0:
        i, is_across, clue = next.pop(0)

        if publish_sse:
            publish = {'working_on': clue.list_to_dict()}
            sse.publish(publish, type="working_on")

        pattern = re.sub(r'[^A-Za-z]', '?', ''.join(crossword.get_clue_text(clue)))

        query_results = query(clue, pattern)

        neighbours = clue.neighbours
        for next_clue in neighbours:
            key = next_clue.clue_no
            across = next_clue.is_across
            if (key, across, next_clue) not in next and (key, across) in all_clues:
                del all_clues[(key, across)]
                next.append((key, across, next_clue))

        solved = False
        for query_result in query_results:
            answer = query_result['answer']
            answer = re.sub(r"[^A-Za-z]+", '', answer)
            confidence = query_result['confidence']
            if answer != "" and (require_confidence or confidence > 0.95):
                try:
                    explanation = query_result['explanation']
                    crossword.solve_clue(i, is_across, answer, explanation,
                                         confidence)
                    solved = True
                    if publish_sse:
                        publish = {
                            "update": {
                                "location": f'{i} {"across" if is_across else "down"}',
                                "answer": answer,
                                "explanation": explanation,
                                "confidence": round(100 * confidence, 2)}}

                        sse.publish(publish, type="update_timeline")
                        sse.publish(crossword.export_crossword(), type="show_grid")
                    crossword.empty_last_filled()
                    break
                except AnswerDoesNotFitError:
                    pass
                except AnswerHasConflictingCharacterError:
                    pass
        if not solved:
            unsolved.append((i, is_across, clue))
    return unsolved


def solve_crossword(crossword: CrosswordPuzzle, publish_sse=True):
    across_clues, down_clues = find_clues(crossword)
    all_clues = across_and_down_to_all_clues(across_clues, down_clues)
    prev = all_clues.copy()
    unsolved = all_clues
    i, is_across = list(unsolved)[0]
    start = unsolved[(i, is_across)]
    del unsolved[(i, is_across)]
    first = True
    while first or (0 < len(unsolved) < len(prev)):
        prev = unsolved.copy()
        unsolved = backtrack_solve_crossword_from_clues(crossword, unsolved,
                                                                             [(i, is_across,
                                                                               start)] if first else list(unsolved),
                                                                             require_confidence=False,
                                                                             publish_sse=publish_sse)
        first = False
    backtrack_solve_crossword_from_clues(crossword, all_clues, unsolved,
                                                              require_confidence=True, publish_sse=publish_sse)
