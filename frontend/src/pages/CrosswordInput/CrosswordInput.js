import './CrosswordInput.css';
import Guardian from './Guardian/Guardian';
import ImageUpload from './ImageUpload/ImageUpload';

import {useState} from 'react';
import {Link} from "react-router-dom";
import leftArrow from "../../images/arrow-left.png";

// Main page for solving an entire grid, showing three options of grid input
function CrosswordInput() {

    // 3 input options
    const [everyman, setEveryman] = useState(false)
    const [imageUpload, setImageUpload] = useState(false)

    const [, setValueCrosswordData] = useState(null)
    const [, setResult] = useState(false)

    // Setting above flags correctly depending on which button is clicked
    const handleDefaultClick = () => {
        setResult(false)
        setValueCrosswordData(null)
    }

    const handleGuardianClick = () => {
        setEveryman(true)
        setImageUpload(false)
        handleDefaultClick()
    }
    const handleImageUploadClick = () => {
        setEveryman(false)
        setImageUpload(true)
        handleDefaultClick()
    }

    return (
        <div className="container-fluid w-100 h-100">
            <div className={"row w-100 h-100"}>

                <div className={"col-1 my-auto p-5"}>
                    <Link to='/'>
                        <div>
                            <img src={leftArrow} className={"img-fluid"} alt={"Left Arrow"}/>
                        </div>
                    </Link>
                </div>

                <div className={"col-10 h-100 my-auto"}>
                    <div className={"row h-100 my-auto"}>

                        <div className={"col-2"} />

                        <div className={"col-8 my-auto"}>

                            <div className={"row my-5"}>
                                <h3 className="mb-3 text-center">Select crossword input method</h3>
                            </div>

                            <div className={"row my-5"}>
                                <div className={"btn-group btn-group-lg"}>
                                    <button type="submit" className="btn btn-primary" onClick={handleGuardianClick}>
                                        Guardian Crossword Number
                                    </button>
                                    <button type='submit' className="btn btn-primary" onClick={handleImageUploadClick}>
                                        Image Upload
                                    </button>
                                </div>
                            </div>

                            {
                                everyman && <Guardian setValueCrosswordData={setValueCrosswordData} setResult={setResult}/>
                            }
                            {
                                imageUpload && <ImageUpload setValueGrid={setValueCrosswordData} setResult={setResult}/>
                            }

                        </div>

                        <div className={"col-2"} />
                    </div>
                </div>

                <div className={"col-1 my-auto"} />
            </div>
        </div>

    );
}

export default CrosswordInput;