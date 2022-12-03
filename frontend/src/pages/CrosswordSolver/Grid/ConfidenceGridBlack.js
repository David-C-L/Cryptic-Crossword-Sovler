import './ConfidenceGridBlack.css'
import './ConfidenceGridCell.css'

function ConfidenceGridBlack(props) {
    return (
        <th scope="col" className="grid-column cell grid-cell black-cell" key={props.col} />
    )
}

export default ConfidenceGridBlack;