import {Button, Modal} from "react-bootstrap";

function SingleSolverResult(props) {
    return (
        <Modal {...props} size="lg" aria-labelledby="contained-modal-title-vcenter" centered>

            <Modal.Header closeButton>
                <Modal.Title id="contained-modal-title-vcenter">
                    Answer for {props.clue} ({props.length})
                </Modal.Title>
            </Modal.Header>

            <Modal.Body>
                <p>Answer: {props.answer}</p>
                <p>Explanation: {props.explanation}</p>
            </Modal.Body>

            <Modal.Footer>
                <Button onClick={props.onHide}>Close</Button>
            </Modal.Footer>

        </Modal>
    );
}

export default SingleSolverResult;