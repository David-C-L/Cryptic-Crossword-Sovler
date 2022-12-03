from haskellWrapper import runSolveBuiltin, runSolveScratch

morseDir = 'crossword-solver'
# Note that this chain command is set to be changed to stop going up a directory
chainComm = f'cd {morseDir} && '

def parseOutputFromHaskell(output):
    if "Sorry, but I couldn't solve it" in output:
        print("No answer found in output")
        return []
    else:
        solution = []
        # A line that begins with a number is indication of the number of parses- these lines and 'Thesaurus...' and
        # 'Keys...' are not needed
        for line in output:
            if len(line) > 0 and not line[0:1].isdigit() and line not in ['Thesaurus...', 'Keys...'] and '->' in line:
                solution.append(line)
        return solution_parser(solution)

def solution_parser(solution):
    sols = []
    definition = ''
    for sol in solution:
        start_of_explanation = sol.index('(')
        expl = sol[start_of_explanation + 1: len(sol) - 1]

        rest_of_solution = sol[: start_of_explanation]
        (definition, answer) = rest_of_solution.split(' ->')

        if answer[0] == '[':
            answer = answer[answer.index(']') + 1:]

        sols.append((answer[1:], expl))
        # I think it makes sense to have the answer and explanation be linked to each other not in their own individual lists
    return definition, sols

def solveBuiltinClue(clueNum):
    return parseOutputFromHaskell(runSolveBuiltin(clueNum))

def solveClue(clueText, clueLength):
    return parseOutputFromHaskell(runSolveScratch(clueText, clueLength))

if __name__ == '__main__':
    print(solveBuiltinClue(10))