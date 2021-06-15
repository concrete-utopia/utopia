import * as React from 'react'

export const ImageContainer = (props) => (
  <div
    className={props.className}
    style={{
      display: 'flex',
      fontFamily: 'Moderat-Regular',
      fontSize: 11,
      flexDirection: 'column',
      color: 'white',
      border: '1px solid #383C4A',
      borderRadius: 8,
      background: '#FFFFFF',
      overflow: 'hidden',
    }}
  >
    {props.children}
  </div>
)
