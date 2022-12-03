import "./ImageUploadForm.css"
import { useCallback, useEffect, useState } from 'react';
import axios from 'axios';

function ImageUploadForm(props) {

    // Constants to store image files
    const [grid, setGrid] = useState(null);
    const [across, setAcross] = useState(null);
    const [down, setDown] = useState(null);

    // Information about dimensions of grid
    const [noOfRows, setNoOfRows] = useState(0);
    const [noOfColumns, setNoOfColumns] = useState(0);

    // Boolean flag for showing error message if answer can not be found
    const [showError, setShowError] = useState(false);

    // const baseURL = (Number(process.env.REACT_APP_PRODUCTION) === 1 ? process.env.REACT_APP_EXTERNAL : process.env.REACT_APP_INTERNAL)
    const baseURL = "http://0.0.0.0:5000"
    const solveEntireGridURL = baseURL + '/uploadImage';
    const eventSource = baseURL + '/stream';

    const eL = useCallback((e) => {
        window.location = "/crosswordSolver"
    }, [])

    useEffect(() => {
        const source = new EventSource(eventSource);
        source.addEventListener('change_screen', eL)
        return () => source.removeEventListener("change_screen", eL)
    }, [eL, eventSource])

    const handlePuzzleToSolve = (e) => {
        e.preventDefault();
        setGrid(e.target.files[0]);
    }

    const handleAcrossClues = (e) => {
        e.preventDefault();
        setAcross(e.target.files[0]);
    }

    const handleDownClues = (e) => {
        e.preventDefault();
        setDown(e.target.files[0]);
    }

    const handleNoOfRowsChange = (e) => {
        e.preventDefault();
        setNoOfRows(e.target.value);
    };

    const handleNoOfColumnsChange = (e) => {
        e.preventDefault();
        setNoOfColumns(e.target.value);
    };

    const handleSubmit = (e) => {
        e.preventDefault();
        props.setResult(false);

        // Wrapping input metadata into Form data object
        let formData = new FormData()
        formData.append('noOfColumns', noOfColumns);
        formData.append('noOfRows', noOfRows);
        formData.append('grid', grid);
        formData.append('across', across);
        formData.append('down', down);

        axios.post(solveEntireGridURL, formData, {
            'Content-Type': 'multipart/form-data'
        }).then(response => {
            props.setValueGrid(response.data.grid);
            props.setResult(true)
        }).catch(() => {
            setShowError(true)
        })
    }

    return (
        <form>

            <div className={"row mb-5"}>
                <div className="col form-group">
                    <label htmlFor="puzzleUpload">Crossword Puzzle</label>
                    <input className="form-control" id="puzzleUpload" type="file" accept={".jpg,.png"} onChange={handlePuzzleToSolve}/>
                </div>
            </div>

            <div className={"row mb-5"}>
                <div className="col form-group">
                    <label htmlFor="acrossCluesUpload">Across Clues</label>
                    <input className="form-control" id="acrossCluesUpload" type="file" accept={".jpg,.png"} onChange={handleAcrossClues}/>
                </div>
            </div>

            <div className={"row mb-5"}>
                <div className="col form-group">
                    <label htmlFor="downCluesUpload">Down Clues</label>
                    <input className="form-control" id="downCluesUpload" type="file" accept={".jpg,.png"} onChange={handleDownClues}/>
                </div>
            </div>

            <div className={"row mb-5"}>
                <div className="col form-group">
                    <label htmlFor="inputNoOfRows">Number of Rows</label>
                    <input type="number" className="form-control" id="inputNoOfRows" aria-describedby="noOfRows" value={noOfRows || ''} onChange={handleNoOfRowsChange} />
                </div>
                <div className="col form-group">
                    <label htmlFor="inputNoOfColumns">Number of Columns</label>
                    <input type="number" className="form-control" id="inputNoOfColumns" aria-describedby="noOfColumns" value={noOfColumns || ''} onChange={handleNoOfColumnsChange} />
                </div>
            </div>

            <div className={"row mb-5"}>
                <div className={"col text-center"}>
                    <button type="submit" className="my-3 btn btn-primary" onClick={handleSubmit}>Submit</button>
                    {
                        showError &&
                        <div className="alert alert-warning text-center">
                            <strong>Oh no!</strong> No answer nor explanation could be found. Please check your input.
                        </div>
                    }
                </div>
            </div>
        </form>
    )
}

export default ImageUploadForm;