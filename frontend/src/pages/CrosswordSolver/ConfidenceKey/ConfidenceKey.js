import "../Grid/ConfidenceGridCell.css"
import './ConfidenceKey.css'

function ConfidenceKey() {
    return (
        <table className="confidence-key">
            <tr>
                <td colSpan="2" className="confidence-key-entry"/>
            </tr>
            <tr>
                <td>
                    <p className="confidence-key-label left-label">Low confidence</p>
                </td>
                <td>
                    <p className="confidence-key-label right-label">High confidence</p>
                </td>
            </tr>
        </table>
    )
}

export default ConfidenceKey