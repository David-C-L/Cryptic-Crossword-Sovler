import './SingleClueSolver.css';
import {Link} from "react-router-dom";
import rightArrow from "../../images/arrow-right.png";
import SingleSolverInput from "./SingleSolverInput/SingleSolverInput";

function SingleClueSolver() {

    return (

        <div className="container-fluid w-100 h-100 align-items-center">
            <div className={"row w-100 h-100 justify-content-center"}>

                <div className={"col-1"}/>

                <div className={"col-10 my-auto"}>
                    <div className={"row p-5"}>
                        <div className={"col"}>
                            <h2 className="text-center">Input a clue to generate an answer and explanation</h2>
                        </div>
                    </div>
                    <div className={"row p-5 justify-content-center"}>
                        <div className={"col-8"}>
                            <SingleSolverInput/>
                        </div>
                    </div>
                </div>

                <div className={"col-1 my-auto p-5"}>
                    <Link to='/'>
                        <div>
                            <img src={rightArrow} className={"img-fluid"} alt={"Right Arrow"}/>
                        </div>
                    </Link>
                </div>

            </div>
        </div>

    );
}

export default SingleClueSolver;