import { useState } from 'react';
import './ConfidenceGridCell.css'

function ConfidenceGridCharEntry(props) {

    const getColourClass = (conf) => {
        if (props.value === '' || conf < 0) {
            return ''
        }
        if (conf < 0.2) {
            return 'conf1'
        } else if (conf < 0.5) {
            return 'conf2'
        } else if (conf < 0.9) {
            return 'conf3'
        } else if (conf < 0.99) {
            return 'conf4'
        } else {
            return 'conf5'
        }
    }


    const [value, setValue] = useState(props.value)
    const [confidenceClass] = useState(props.showConfidence ? getColourClass(props.confidence) : '')
    const [workingOn] = useState(!props.editable && props.workingOn ? 'working-on' : '')


    const handleChange = (e) => {
        const isAlpha = e.target.value.length === 1 && e.target.value.match(/[A-Za-z]/i)
        const isEmpty = e.target.value.length === 0
        if (props.editable && (isAlpha || isEmpty)) {
            const val = e.target.value.toUpperCase()
            setValue(val)
            props.updateCell(props.row, props.col, val)
        }
    }

    return (
        <th scope="col" className={`grid-column cell grid-cell ${confidenceClass} ${workingOn}`} key={props.col}>
            {props.clueNo && <p className="clue-number">{props.clueNo}</p>}
            <input className='cell input-cell' value={value} type="text" onChange={handleChange} />
        </th>
    )
}

export default ConfidenceGridCharEntry;