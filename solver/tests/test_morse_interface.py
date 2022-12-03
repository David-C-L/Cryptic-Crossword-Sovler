from morseInterface import solveClue, solveBuiltinClue

def test_solving_builtin_clues():
    assert solveBuiltinClue(1)[1][0][0] == "ESCORT"
    assert solveBuiltinClue(2)[1][0][0] == "JACKET"
    assert solveBuiltinClue(3)[1][0][0] == "MATE"
    assert solveBuiltinClue(4)[1][0][0] == "ANGELA"
    assert solveBuiltinClue(5)[1][0][0] == "HESITATE"

def test_solving_scratch_clues():
    assert solveClue("companion shredded corset", 6)[1][0][0] == "ESCORT"
    assert solveClue("notice in flying coat", 6)[1][0][0] == "JACKET"
    assert solveClue("companion found in oklahoma terminal", 4)[1][0][0] == "MATE"
    assert solveClue("a new member returned a woman", 6)[1][0][0] == "ANGELA"
    assert solveClue("pause at these i fancy", 8)[1][0][0] == "HESITATE"
