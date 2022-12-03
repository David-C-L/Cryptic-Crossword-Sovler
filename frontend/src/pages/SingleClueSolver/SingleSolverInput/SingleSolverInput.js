import {Button, ButtonGroup, Col, Form, Row} from "react-bootstrap";
import axios from 'axios';
import SingleSolverResult from "./SingleSolverResult";
import {useState} from "react";
import SingleSolverHints from "./SingleSolverHints";

function SingleSolverInput() {

    // const baseURL = (Number(process.env.REACT_APP_PRODUCTION) === 1 ? process.env.REACT_APP_EXTERNAL : process.env.REACT_APP_INTERNAL)
    const baseURL = "http://0.0.0.0:5000"
    const solveSingleAnswerURL = baseURL + '/solve';

    const [showResult, setShowResult] = useState(false);
    const [showHints, setShowHints] = useState(false);

    const [clue, setClue] = useState("");
    const [length, setLength] = useState("");
    const [hints, setHints] = useState([]);
    const [hintNum, setHintNum] = useState(-2);
    const [queried, setQueried] = useState(false);
    const [answer, setAnswer] = useState("");
    const [explanation, setExplanation] = useState("");

    const query = (url, data) => {
        axios.post(url, data, {
            headers: {
                'Access-Control-Allow-Origin': '*',
                'Content-Type': 'application/json'
            }
        }).then(response => {
            setAnswer(response.data.results[0].answer);
            setExplanation(response.data.results[0].explanation);
            setHints(response.data.results[0].hints);
            setHintNum(-1);
            setQueried(true);
            return response.data.results[0].answer
        }).catch(() => {
            // Send a popup
            alert("Error: could not connect to the server");
            return ""
        });
    }

    const handleClueChange = (event) => {
        setClue(event.target.value);
        setAnswer("")
        setExplanation("")
        setQueried(false)
        setShowHints(false)
        setShowResult(false)
        setHintNum(-2)
        setHints([])
    };

    const handleLengthChange = (event) => {
        setLength(event.target.value);
        setAnswer("")
        setExplanation("")
        setQueried(false)
        setShowHints(false)
        setShowResult(false)
        setHintNum(-2)
        setHints([])
    };

    const handleSubmit = (event) => {

        event.preventDefault();

        if (!queried) {
            // Prepare dictionary to be sent as request
            const data = {
                clue: clue,
                length: length
            }
            let t = query(solveSingleAnswerURL, data)
            console.log(t)
            if (t !== "") {
                setShowResult(true);
            }
        } else {
            setShowResult(true);
        }

    };

    const getHint = (event) => {

        event.preventDefault();

        if (!queried) {
            // Prepare dictionary to be sent as request
            const data = {
                clue: clue,
                length: length
            }
            query(solveSingleAnswerURL, data, setShowHints(true))
        } else {
            if (hintNum < hints.length - 1) {
                setHintNum(hintNum + 1)
            } else {
                setHintNum(hints.length - 1)
            }
            setShowHints(true);
        }
    }

    return (
        <div>
            <Form className={"text-center my-3"} onSubmit={handleSubmit}>
                <Row className={"mb-3"}>
                    <Form.Group as={Col} xs={8} controlId="singleSolverClue">
                        <Form.Control
                            required
                            type="text"
                            placeholder="Clue (e.g. companion shredded corset)"
                            onChange={handleClueChange}
                        />
                    </Form.Group>

                    <Form.Group as={Col} controlId="singleSolverInput">
                        <Form.Control
                            required
                            type="text"
                            placeholder="Length (e.g. 6 or 3,3)"
                            onChange={handleLengthChange}
                        />
                    </Form.Group>
                </Row>

                <Row className={"my-5"}>
                    <Col/>
                    <ButtonGroup className={"col-4 text-center"}>
                        <Button variant="primary" type="submit">Solve</Button>

                        <Button variant="secondary" disabled={hints && hintNum >= hints.length - 1} onClick={getHint}>
                            {hints && hintNum >= hints.length - 1 ? "No More Hints" : "Get Hint"}
                        </Button>
                    </ButtonGroup>
                    <Col/>
                </Row>

            </Form>

            {showResult && <SingleSolverResult
                show={showResult}
                onHide={() => setShowResult(false)}
                clue={clue}
                length={length}
                answer={answer}
                explanation={explanation}
            />}

            {
                showHints &&
                <SingleSolverHints
                    clue={clue}
                    length={length}
                    hintNum={hintNum}
                    hints={hints}
                />
            }
        </div>
    );
}

export default SingleSolverInput;
