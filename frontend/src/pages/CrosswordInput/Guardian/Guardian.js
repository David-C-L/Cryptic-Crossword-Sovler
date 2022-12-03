import './Guardian.css';
import GuardianForm from './GuardianForm';

// Option that is rendered when Guardian input option is clicked
function Guardian(props) {

    return (
        <div className="container align-items-center">
            <GuardianForm setValueCrosswordData={props.setValueCrosswordData} setResult={props.setResult}/>
        </div>
    )

}

export default Guardian;