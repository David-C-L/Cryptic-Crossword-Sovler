import re


def unlikely_hint_parser(explanation: str):
    return explanation.split(".")


def split_I_rest(partial_explanation: str):
    indicator, rest = re.split(']', partial_explanation, 1)
    indicator = indicator.replace('[', '').replace(']', '')
    return indicator.strip(), rest.strip()


def split_C_rest(partial_explanation: str):
    split = re.split(r'(?=[\[])', partial_explanation, 1)
    if len(split) == 2:
        command, rest = split
    else:
        command, rest = "NO COMMAND", split
    return command, rest


def split_outer_word(string: str, word: str):
    result = []
    i = 0
    curr_start = 0
    inside = 0
    while i < len(string):
        c = string[i]
        if c == '[' or c == '(':
            inside += 1
        elif c == ']' or c == ')':
            inside -= 1
        elif inside == 0 and i + len(word) < len(string) and string[i:i + len(word)] == word:
            result.append(string[curr_start:i].strip())
            i += len(word)
            curr_start = i
        i += 1

    result.append(string[curr_start:i].strip())

    return result


class MorseHintParser:

    def __init__(self):
        self.command_funcs = {
            "hyponym": self._CIT_parse,
            "abbreviation": self._CIT_parse,
            "anagram": self._CIR_parse,
            "odd letters": self._CIT_parse,
            "even letters": self._CIT_parse,
            "first letters": self._CIR_parse,
            "last letters": self._CIR_parse,
            "middle letters": self._CIR_parse,
            "end letters": self._CIR_parse,
            "duplicate": self._CISR_parse,
            "homophone": self._CIR_parse,
            "reversal": self._CIR_parse,
            "rotation": self._CIR_parse,
            "insert": self._CISRR_parse,
            "subtract": self._CISRR_parse
        }

    @staticmethod
    def _CIT_parse(partial_explanation: str, command: str):
        res = []
        I, T = split_I_rest(partial_explanation)

        res.append(f"\'{I}\' means {command}")
        res.append(f"{command} of \'{I}\'")

        return res

    def _CIR_parse(self, partial_explanation: str, command: str):
        res = []
        I, R = split_I_rest(partial_explanation)

        res.append(f"\'{I}\' means {command}")

        if not R[0].isupper():
            res.append(f"{command} of {R}")
        else:
            other_res, answer = self._recurse_parse(R)
            res += other_res
            res.append(f"{command} of {answer}")

        return res

    def _CISR_parse(self, partial_explanation: str, command: str):
        res = []
        I, R = split_I_rest(partial_explanation)

        res.append(f"\'{I}\' means {command}")

        if command == "duplicate":
            R = R[1:-1]
            fst_parse, snd_parse = split_outer_word(R, "+")
            # Should only be 2
            fst_res, fst_ans = self._recurse_parse(fst_parse)
            snd_res, snd_ans = self._recurse_parse(snd_parse)
            res.append(f"duplicate reasoning to find two answers.")
            res += fst_res
            res += snd_res
            res.append(f"Combine {fst_ans} and {snd_ans}")

        return res

    def _CISRR_parse(self, partial_explanation: str, command: str):
        res = []
        I, R = split_I_rest(partial_explanation)

        res.append(f"\'{I}\' means {command}")

        split_word = None
        if command == "insert":
            split_word = "into"
        elif command == "subtract":
            split_word = "from"

        if split_word is not None:
            fst_tree, snd_tree = split_outer_word(R, split_word)
            fst_res, fst_ans = self._recurse_parse(fst_tree)
            snd_res, snd_ans = self._recurse_parse(snd_tree)

            res += fst_res
            res += snd_res
            res.append(f"{command} {fst_ans} {split_word} {snd_ans}")

        return res

    def _plus_parse(self, partial_explanations):
        res = []
        answers = []
        for partial_exp in partial_explanations:
            other_res, answer = self._recurse_parse(partial_exp)
            res += other_res
            answers.append(answer)

        res.append(f"Put {', '.join(answers[:-1])}, and {answers[-1]} together")
        return res

    def _recurse_parse(self, partial_explanation: str):
        res = []
        equal_idx = partial_explanation.find('=')
        equal_idx = equal_idx if equal_idx > -1 else len(partial_explanation)
        bracket_idx = partial_explanation.find('(')
        bracket_idx = bracket_idx if bracket_idx > -1 else len(partial_explanation)
        if equal_idx == bracket_idx:
            answer = partial_explanation.upper()
            res.append(f"Consider {answer}")
        elif equal_idx < bracket_idx:
            answer, clue_text = partial_explanation.split("=", 1)
            res.append(f"\'{clue_text}\' means {answer}")
        else:
            _, answer, parse_tree = re.split(r'(.*?)\s*\(', partial_explanation, 1)
            parse_tree = parse_tree[:-1]
            other_res = self.parse(parse_tree)
            res += other_res

        return res, answer

    def parse(self, explanation):
        res = []
        C, R = split_C_rest(explanation)
        if C in self.command_funcs:
            func = self.command_funcs[C]
            other_res = func(R, C)
            res += other_res
        elif len(plus_exps := split_outer_word(explanation, "+")) > 1:
            other_res = self._plus_parse(plus_exps)
            res += other_res

        return res
