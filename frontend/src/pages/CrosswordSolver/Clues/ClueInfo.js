import './ClueInfo.css';

function ClueInfo(props) {

    return (
        <div className="clueInfo">
            <h4 className="mb-3 text-center">{props.clueNo}, {props.isAcross ? "Across" : "Down"}</h4>
            {props.hasAnswer
            ?
            <div className="container">
                <div className="row">
                    <div className="col-3">
                        <h6 className="mb-3 text-center">Answer:</h6>
                    </div>
                    <div className="col-9">
                        <p>{props.answer}</p>
                    </div>
                </div>
                <div className="row">
                    <div className="col-3">
                        <h6>Confidence:</h6>
                    </div>
                    <div className="col-9">
                        <p>{props.confidence}</p>
                    </div>
                </div>
                <div className="row">
                    <div className="col-3">
                        <h6>Explanation:</h6>
                    </div>
                    <div className="col-9">
                        <p>{props.explanation}</p>
                    </div>
                </div>
            </div>
            :
            <div className="alert alert-warning text-center">
                <strong>Oh no!</strong> No answer nor explanation has been found yet.
            </div>
            }
        </div>
    );

}

export default ClueInfo;