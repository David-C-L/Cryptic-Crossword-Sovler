function TimelineEntry(props) {

  return (
    <div>
      
      <hr style={{borderTop: '1px solid #8c8b8b', margin: 0, padding: 0}}/>
      {props.entry &&
      <p><strong>{props.entry.location}</strong> is <strong>{props.entry.answer}</strong> because {props.entry.explanation} with confidence <strong>{props.entry.confidence}%</strong></p>
      }
    </div>
  )
} 

export default TimelineEntry
