import './Home.css';
import leftArrow from '../../images/arrow-left.png'
import rightArrow from '../../images/arrow-right.png'
import exampleCrossword from '../../images/crossword_example.jpg'
import {Link} from "react-router-dom"

function Home() {
    return (
        <div className={"container-fluid w-100 h-100"}>
            <div className={"row w-100 h-100"}>

                <div className={"col-1 my-auto p-5"}>
                    <Link to='/singleSolver'>
                        <div>
                            <img src={leftArrow} className={"img-fluid"} alt={"Left Arrow"}/>
                        </div>
                    </Link>
                </div>

                <div className={"col-10 h-100 my-auto"}>
                    <div className={"row h-100 my-auto"}>
                        <div className={"col-8 my-auto"}>

                            <div className={"p-4"}>
                                <h1>Cryptic Crossword Solver</h1>
                            </div>

                            <div className={"p-4"}>
                                {/*<h3>Hello! We do the crosswords :)</h3>*/}
                                <h4>
                                    This app uses programs written by Imperial College London staff and alumni to
                                    solve difficult crosswords from the Guardian website.
                                </h4>
                            </div>

                            <div className={"p-4"}>
                                <h4><b>Morse</b></h4>
                                <h6>A Haskell-based cryptic crossword clue solver that uses functional programming
                                    to work out and explain answers, written by Anthony Field.</h6>
                            </div>

                            <div className={"p-4"}>
                                <h4><b>Crossword Genius</b></h4>
                                <h6>A solver that uses machine learning to also solve cryptic crossword clues,
                                    developed by Unlikely Artificial Intelligence.</h6>
                            </div>

                        </div>
                        <div className={"col-4 my-auto"}>
                            <div>
                                <img src={exampleCrossword} className={"img-fluid"} alt={"Example Crossword"}/>
                            </div>
                        </div>

                    </div>
                </div>

                <div className={"col-1 my-auto p-5"}>
                    <Link to='/crosswordInput'>
                        <div>
                            <img src={rightArrow} className={"img-fluid"} alt={"Right Arrow"}/>
                        </div>
                    </Link>
                </div>

            </div>
        </div>


    );
}

export default Home;