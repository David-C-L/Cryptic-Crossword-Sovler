import ConfidenceGridEntry from "./ConfidenceGridEntry";
import './ConfidenceGridRow.css'

function ConfidenceGridRow(props) {
    return (
        <tr className="grid-row row" key={props.row}>
            {
                props.values && props.values.map((value, col) =>
                    <ConfidenceGridEntry row={props.row} col={col} value={value} updateCell={props.updateCell} editable={props.editable} confidence={props.confidence[col]} lastFilled={props.lastFilled ? props.lastFilled[col] : false} showConfidence={props.showConfidence} clueNo={props.cluesPositions ? props.cluesPositions[col] : false} workingOn={props.workingOn ? props.workingOn[col] : false}/>
                )
            }
        </tr>
    )
}

export default ConfidenceGridRow;