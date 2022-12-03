function SingleSolverHints(props) {
    return (
        <div>
            <h4 className="mb-3 text-center">{props.clue}, {props.length}</h4>
            <div className="mt-lg-3">
                {
                    props.hints && props.hints.slice(0, props.hintNum + 1).map((val, i) =>
                        <div className="row" key={i}>
                            <div className="col-3">
                                <h6 className="mb-3 text-center">Hint {i + 1}:</h6>
                            </div>
                            <div className="col-6">
                                <p>{val}</p>
                            </div>
                        </div>
                    )
                }
            </div>
        </div>
    );
}

export default SingleSolverHints;