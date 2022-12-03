import pexpect


# A COUPLE THINGS I'VE FOUND WORTH NOTING WHILE USING PEXPECT AND GHCI
#   For one thing, ghci from command to command doesn't seem to provide an EOF file (makes sense), this means we need to
#       expect an exception (which is fine and you'll see is how I've implemented the runner with readline())
#   I guess it's just that one for now

class HaskellRunner:
    def __init__(self, com, default_timeout=2, loading_timeout=30, run_preemptive_solve=False, skip_setup_text=False):
        self.proc = pexpect.spawn(com, cwd="crossword-solver")
        self.proc.timeout = loading_timeout
        if run_preemptive_solve:
            self.proc.sendline("solve (clue 1)")  # Run first clue to get the thesaurus and keys etc in cache
            try:
                for x in range(24):
                    # Compiling the Solver and running "solve (clue 1)" produces 24 lines to be ignored
                    # this just gets that ignoring out of the way
                    self.proc.readline()
            except pexpect.exceptions.TIMEOUT:
                # If this occurs, 24 lines were not ignored, that's fine since the point is that we want to clear
                #   any lines produced from setting up... argument could be made to purposefully trigger this exception?
                print("timed out in constructor")
        # Set new smaller timeout once loading libs in complete, since everything else should run much faster

        self.proc.timeout = default_timeout
        if skip_setup_text:
            try:
                while True:
                    self.proc.readline()
            except pexpect.exceptions.TIMEOUT:
                pass


    def readLine(self):
        # What it says on the tin, reads line from shell and blocks until a line is found,
        #   will throw exception if blocked for TIMEOUT duration
        return self.proc.readline()

    def readAll(self):
        out = []
        try:
            while True:
                line = self.proc.readline().decode('utf-8')[:-1]  # Removing last char since it's a new line character
                if line == '':
                    raise pexpect.exceptions.TIMEOUT(1)
                out.append(line)
        except pexpect.exceptions.TIMEOUT:
            return out

    def send(self, msg):
        # Send a message to the process, returns a "spawn" object, not sure what it's uses are and is currently ignored
        return self.proc.sendline(msg)


print("Compiling Morse...")
# A global runner allows us to preserve the same ghci session all the time, this could get difficult with concurrency
# (possibility of reading multiple outputs and conflating them into one incorrectly)
# but I'm not experienced enough to know if that's an actual issue to look out for,
# I doubt we'll be receiving high volumes of requests anyway
runner = HaskellRunner('ghci Solver -package ghc', run_preemptive_solve=True)
print("Compilation complete")


def runSolveBuiltin(num):
    runner.send(f"solve (clue {num})")
    out = []
    while len(out) == 0:
        try:
            while True:
                out.append(
                    runner.readLine().decode('utf-8')[:-1])  # Removing last index since it's a new line character
        except pexpect.exceptions.TIMEOUT:
            out = out[1:]  # First output is just a "solve (X, Y)" line not worth including
    return out


def runSolveScratch(clue, length):
    runner.send(f'solve ("{clue}", {length})')
    out = []
    while len(out) == 0:
        try:
            while True:
                out.append(
                    runner.readLine().decode('utf-8')[:-1])  # Removing last index since it's a new line character
        except pexpect.exceptions.TIMEOUT:
            out = out[1:]  # First output is just a "solve (X, Y)" line not worth including
    return out


if __name__ == "__main__":
    # Interactive session to test/use morse from haskell, should be pretty intuitive
    run = True
    print("Starting interactive process")
    i = input(
        "Select usage mode:\n\t(1) input numbers to solve built-in clues\n\t(2) input clue text and length for solving\n\t(3) input 3 or any other key to cancel process\n")
    if i == "1":
        print('Starting interactive runner, enter "n" or "end" to end the process')
        while run:
            i = input("Next clue num?\t")
            if i.lower() in ['n', 'end', 'stop']:
                run = False
            else:
                n = int(i)
                for l in runSolveBuiltin(n):
                    print(l)
    elif i == "2":
        print('Starting interactive runner, enter "n" or "end" to end the process')
        while run:
            text = input("Enter clue text:\t")
            if text.lower() in ['n', 'end', 'stop']:
                run = False
            else:
                clue_length = input("Enter clue length:\t")
                for l in runSolveScratch(text, clue_length):
                    print(l)
    print("Exiting process, thanks for playing")
