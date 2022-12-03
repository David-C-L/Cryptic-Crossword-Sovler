import React from 'react';
import Clue from "./Clue"

function Clues(props) {
    const headerClass = 'crossword__clues-header';
    let cluesByDirection;
    if (props.clues !== undefined) {
        cluesByDirection = isAcross => props.clues.filter(clue => clue.is_across === isAcross)
            .map(clue => (<Clue isAcross={isAcross} index={clue.number} clue={clue.clue} setSelectedClue={props.setSelectedClue} selectedClue={props.selectedClue} explanation={clue.explanation} showExplanation={props.showExplanation} length={clue.length}/>
            ));
    } else {
        cluesByDirection = isAcross => <div>No clues yet</div>
    }

    return (
        <div className="crossword__clues" style={{'overflow-y': 'scroll', 'maxHeight': '80vh'}}>
            {props.across && <div className="crossword__clues--across">
                <h3 className={headerClass}>Across</h3>
                <ol className="crossword__clues-list">
                    {cluesByDirection(true)}
                </ol>
            </div>}
            {props.down && <div className="crossword__clues--down">
                <h3 className={headerClass}>Down</h3>
                <ol className="crossword__clues-list">
                    {cluesByDirection(false)}
                </ol>
            </div>}
        </div>

    )
        ;
}

export default Clues;