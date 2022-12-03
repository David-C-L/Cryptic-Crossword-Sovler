import TimelineEntry from "./TimelineEntry";

function Timeline(props) {

  return (
    <div style={{ 'overflow-y': 'scroll', 'maxHeight': '80vh' }}>
      <h3 className="text-center">Timeline</h3>
      {props.timeline && props.timeline.map(entry =>
        <TimelineEntry entry={entry} />
      )
      }
    </div>

  )
}

export default Timeline