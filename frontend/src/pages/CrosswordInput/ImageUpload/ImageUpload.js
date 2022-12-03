import './ImageUpload.css';
import ImageUploadForm from './ImageUploadForm';

// Option that is rendered when option to upload images is clicked
function ImageUpload(props) {

  return (
      <div className="container align-items-center">
        <ImageUploadForm setValueGrid={props.setValueGrid} setResult={props.setResult}/>
      </div>
  
  );

}

export default ImageUpload;