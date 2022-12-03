import ConfidenceGridBlack from "./ConfidenceGridBlack";
import ConfidenceGridCharEntry from "./ConfidenceGridCharEntry";

function ConfidenceGridEntry(props) {
    return (
        <>
            {
                props.value === '0' ? <ConfidenceGridBlack /> :
                    <ConfidenceGridCharEntry value={props.value === '1' ? '' : props.value} updateCell={props.updateCell} confidence={props.confidence} row={props.row} col={props.col} editable={props.editable} lastFilled={props.lastFilled} showConfidence={props.showConfidence} clueNo={props.clueNo} workingOn={props.workingOn}/>
            }
        </>
    )
}

export default ConfidenceGridEntry;