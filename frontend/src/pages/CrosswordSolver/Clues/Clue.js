function Clue(props) {
    const handleClick = () => {
        props.setSelectedClue({ isAcross: props.isAcross, clueNumber: props.index });
    }

    return (
        <div style={{ background: props.selectedClue && props.selectedClue.clueNumber === props.index && props.isAcross === props.selectedClue.isAcross ? 'lightgrey' : 'white' }} onClick={handleClick}>
            <hr style={{borderTop: '1px solid #8c8b8b', margin: 0, padding: 0}}/>
            <li value={props.index}>
                <p style={{margin: 0, padding: 0}}>{props.clue} {props.length ? '(' + props.length.join(', ') + ')' : ''}</p>
            </li>
        </div>
    );

}

export default Clue;