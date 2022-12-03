import './CrosswordSolver.css'

import Timeline from "./Timeline/Timeline";
import Clues from "./Clues/Clues";
import ClueInfo from "./Clues/ClueInfo";
import { useCallback, useEffect, useState } from 'react';
import ConfidenceGrid from "./Grid/ConfidenceGrid";
import ConfidenceKey from "./ConfidenceKey/ConfidenceKey";
import { Link } from "react-router-dom";
import leftArrow from "../../images/arrow-left.png";

function CrosswordSolver() {
    const [valueCrosswordData, setValueCrosswordData] = useState(null)
    const [result, setResult] = useState(false)
    const [timeline, setTimeline] = useState([]);
    const [showConfidence, setShowConfidence] = useState(false)
    const [showTimeline, setShowTimeline] = useState(true)
    const [clueList, setClueList] = useState([])
    const [selectedClue, setSelectedClue] = useState({ isAcross: false, clueNumber: 0 })
    const [workingOn, setWorkingOn] = useState({})

    const updateClues = useCallback((valueCrosswordData) => {
        const cl = []
        for (const [key, value] of Object.entries(valueCrosswordData.across)) {
            cl.push({ number: key, clue: value.clue, is_across: true, explanation: value.explanation, length: value.length })
        }
        for (const [key, value] of Object.entries(valueCrosswordData.down)) {
            cl.push({ number: key, clue: value.clue, is_across: false, explanation: value.explanation, length: value.length })
        }
        setClueList(cl)
    }, [setClueList])

    const updateCell = (row, col, val) => {
        setResult(false)
        valueCrosswordData.grid[row][col] = val
        valueCrosswordData.confidence_grid[row][col] = -1
        setResult(true)
    }

    const showGridEL = useCallback((event) => {
        setResult(false);
        const data = JSON.parse(event.data)
        setValueCrosswordData(data);
        updateClues(data)
        setResult(true);
    }, [setResult, setValueCrosswordData, updateClues])

    const updateTimelineEL = useCallback((event) => {
        const queryResult = JSON.parse(event.data).update;
        setTimeline(tl => [queryResult, ...tl])
    }, [setTimeline])

    const workingOnEL = useCallback((event) => {
        setResult(false);
        setWorkingOn(JSON.parse(event.data).working_on)
        setResult(true);
    }, [setResult, setWorkingOn])

    // const baseURL = (Number(process.env.REACT_APP_PRODUCTION) === 1 ? process.env.REACT_APP_EXTERNAL : process.env.REACT_APP_INTERNAL)
    const baseURL = "http://0.0.0.0:5000"

    const eventSource = baseURL + '/stream';

    useEffect(() => {
        const source = new EventSource(eventSource);
        source.addEventListener('show_grid', showGridEL, false);
        source.addEventListener('update_timeline', updateTimelineEL, false);
        source.addEventListener('working_on', workingOnEL, false);

        return () => {
            source.removeEventListener("show_grid", showGridEL)
            source.removeEventListener("update_timeline", updateTimelineEL)
            source.removeEventListener("working_on", workingOnEL)
        }

    }, [eventSource, showGridEL, updateTimelineEL, workingOnEL]);

    const handleShowConfidenceChange = () => {
        setResult(false)
        setShowConfidence(!showConfidence)
        setResult(true)
    }

    const handleShowTimelineChange = () => {
        setResult(false)
        setShowTimeline(!showTimeline)
        setResult(true)
    }

    return (
        <div className="container-fluid w-100 h-100 align-items-center ">
            <div className={"row w-100 h-100"}>

                <div className={"col-1 my-auto p-5"}>
                    <Link to='/'>
                        <div>
                            <img src={leftArrow} className={"img-fluid"} alt={"Left Arrow"} />
                        </div>
                    </Link>
                </div>

                <div className={"col-10 h-100 my-auto"}>

                    <div className="row h-100 my-auto">
                        <div className="col-3 border">
                            {result && showTimeline && <Timeline timeline={timeline} />}
                            {result && !showTimeline && <Clues clues={clueList} setSelectedClue={setSelectedClue} selectedClue={selectedClue} across={true} down={false} showExplanation={true} />}
                        </div>
                        <div className="col-6 border middle-column">
                            {result && !showConfidence &&
                                <ConfidenceGrid crosswordData={valueCrosswordData} showConfidence={false}
                                    updateCell={updateCell} workingOn={workingOn} />}
                            {result && showConfidence &&
                                <ConfidenceGrid crosswordData={valueCrosswordData} showConfidence={true}
                                    updateCell={updateCell} workingOn={workingOn} />}
                            <div className="control-buttons">
                                <button type="button" className={"control-button-left btn btn-primary text-center"}
                                    onClick={handleShowConfidenceChange}>{showConfidence ? 'Hide' : 'Show'} Confidence
                                </button>
                                <button type="button" className={"control-button-right btn btn-primary text-center"}
                                    onClick={handleShowTimelineChange}>{showTimeline ? 'Hide' : 'Show'} Timeline
                                </button>
                            </div>
                            {result && showConfidence && <ConfidenceKey />}
                            {
                                result && valueCrosswordData !== null && selectedClue.clueNumber !== 0 &&
                                <ClueInfo
                                    clueNo={selectedClue.clueNumber}
                                    isAcross={selectedClue.isAcross}
                                    answer={valueCrosswordData[selectedClue.isAcross ? "across" : "down"][selectedClue.clueNumber].answer}
                                    explanation={valueCrosswordData[selectedClue.isAcross ? "across" : "down"][selectedClue.clueNumber].explanation}
                                    confidence={valueCrosswordData[selectedClue.isAcross ? "across" : "down"][selectedClue.clueNumber].confidence}
                                    hasAnswer={valueCrosswordData[selectedClue.isAcross ? "across" : "down"][selectedClue.clueNumber].answer !== ""}
                                />
                            }
                        </div>
                        <div className="col-3 border">
                            {result && showTimeline && <Clues clues={clueList} setSelectedClue={setSelectedClue} selectedClue={selectedClue} across={true} down={true} showExplanation={false} />}
                            {result && !showTimeline && <Clues clues={clueList} setSelectedClue={setSelectedClue} selectedClue={selectedClue} across={false} down={true} showExplanation={true} />}
                        </div>
                    </div>
                </div>

                <div className={"col-1"} />

            </div>
        </div>

    )
}

export default CrosswordSolver;