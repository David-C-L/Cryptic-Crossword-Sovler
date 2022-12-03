import pexpect

from haskellWrapper import runner, HaskellRunner

# Test to ensure runner constructor runs smoothly
def test_runner_creation_with_pure_shell():
    echo_str = "Dreams are messages from the deep"
    cmd_str = f'echo "{echo_str}"'
    new_runner = HaskellRunner(cmd_str)
    assert new_runner is not None
    assert new_runner.readAll() == [echo_str+"\r"]


# Test to ensure runner constructor runs smoothly with ghci
def test_runner_creation_with_ghci():
    new_runner = HaskellRunner("ghci", skip_setup_text=True)
    assert new_runner is not None
    assert new_runner.readAll() == []
    num = 12345
    new_runner.send(str(num))
    assert new_runner.readAll() == [f'Prelude> {num}\r\r', f'{num}\r']


# Simple test that hello world can be read from the wrapper using readAll() using static runner
def test_echo_read_all():
    test_str = "Hello world!"
    echo_command = f'putStrLn "{test_str}"'
    runner.send(echo_command)
    expected_output = [f'*Solver> {echo_command}\r\r', f'{test_str}\r']
    assert runner.readAll() == expected_output


# Simple test that hello world can be read from the wrapper using readLine() using static runner
def test_echo_read_line():
    test_str = ".... . .-.. .-.. --- / .-- --- .-. .-.. -.."  # (Hello world in morse code)
    echo_command = f'putStrLn "{test_str}"'
    runner.send(echo_command)
    actual_output = []
    while len(actual_output) == 0:
        try:
            while True:
                actual_output.append(
                    runner.readLine().decode('utf-8')[:-1])  # Removing last char since it's a new line character
        except pexpect.exceptions.TIMEOUT:
            pass
    expected_output = [f'*Solver> {echo_command}\r\r', f'{test_str}\r']
    assert actual_output == expected_output

