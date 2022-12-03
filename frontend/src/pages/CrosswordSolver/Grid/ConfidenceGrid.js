import './ConfidenceGrid.css'
import ConfidenceGridRow from "./ConfidenceGridRow"

function ConfidenceGrid(props) {
    return (
        <table className="center-grid crossword-grid">
            {
                props.crosswordData && props.crosswordData.grid &&
                props.crosswordData.grid.map((values, row) =>
                    <ConfidenceGridRow row={row} values={values} updateCell={props.updateCell} confidence={props.crosswordData.confidence_grid[row]} lastFilled={props.crosswordData.last_filled[row]} editable={props.crosswordData.editable} showConfidence={props.showConfidence} cluesPositions={props.crosswordData.clue_no_pos[row]} workingOn={props.workingOn[row] ? props.workingOn[row] : false}/>
                )
            }
        </table>
    )
}

export default ConfidenceGrid