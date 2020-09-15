import RedBox, { RedBoxError } from 'redbox-react';

<RedBox error={new Error()}/>;
<RedBoxError error={new Error()}/>;

<RedBox
  error={new Error()}
  filename='somefile.tsx'
  editorScheme='subl://open?url=file:///filename'
  useLines
  useColumns={false}
  style={{
    backgroundColor: 'blue'
  }}
  className='custom-class'/>;
