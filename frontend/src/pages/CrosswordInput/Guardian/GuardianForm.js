import './GuardianForm.css';

import {useCallback, useEffect, useState} from 'react';
import axios from 'axios';
import {Button, Col, Form, Row} from "react-bootstrap";

// Form for inputing Guardian reference number
function GuardianForm(props) {
    // Constants for the Guardian reference number and boolean flag for validity of input
    const [number, setNumber] = useState(0);
    const [type, setType] = useState("Everyman")
    const [invalidInput, setInvalidInput] = useState(false);

    // Boolean flag for showing error message if answer could not be found
    const [showError, setShowError] = useState(false);

    // const baseURL = (Number(process.env.REACT_APP_PRODUCTION) === 1 ? process.env.REACT_APP_EXTERNAL : process.env.REACT_APP_INTERNAL)
    const baseURL = "http://0.0.0.0:5000"
    const eventSource = baseURL + '/stream';

    const handleSubmit = (e) => {
        e.preventDefault();
        props.setResult(false)
        if (number > 0) {
            const url = `${baseURL}/${type.toLowerCase()}/${number}`;
            axios.get(url, {
                headers: {
                    'Access-Control-Allow-Origin': '*',
                    'Content-Type': 'application/json'
                }
            }).then(response => {
                props.setValueCrosswordData(response.data);
                setShowError(false)
                setInvalidInput(false)
            }).catch(() => {
                props.setResult(false)
                setShowError(true)
                setInvalidInput(false)
            });
        } else {
            setInvalidInput(true)
            props.setResult(false)
        }
    };

    const eL = useCallback((e) => {
        window.location = "/crosswordSolver"
    }, [])

    useEffect(() => {
        const source = new EventSource(eventSource);
        source.addEventListener('change_screen', eL)
        return () => source.removeEventListener("change_screen", eL)
    }, [eL, eventSource])

    return (
        <Form>
            <Row className={"my-5"}>
                <Form.Group className={"col-7"}>

                    <Form.Label>Crossword Type</Form.Label>
                    <Form.Select
                        value={type}
                        onChange={e => {setType(e.target.value)}}
                    >
                        <option value={"Everyman"}>Everyman</option>
                        <option value={"Quiptic"}>Quiptic</option>
                        <option value={"Cryptic"}>Cryptic</option>
                    </Form.Select>
                </Form.Group>

                <Form.Group className={"col-5"}>
                    <Form.Label>Puzzle Number</Form.Label>
                    <Form.Control
                        value={number > 0 ? number : ''}
                        onChange={e => setNumber(e.target.value)}
                    />
                    {
                        invalidInput && "Please enter a number greater than zero."
                    }
                </Form.Group>
            </Row>

            <Row className={"mb-5"}>
                <Col className={"text-center"}>
                    <Button type={"submit"} className={"my-3 btn btn-primary"} onClick={handleSubmit}>
                        Submit
                    </Button>
                    {
                        showError &&
                        <div className="alert alert-warning text-center">
                            <strong>Oh no!</strong> No answer nor explanation could be found. Please check your input.
                        </div>
                    }
                </Col>
            </Row>

        </Form>
    );
}

export default GuardianForm;