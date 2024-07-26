export const ComponentsHonouringPropsStylesProject = `import * as React from 'react'
import { Scene, Storyboard } from 'utopia-api'

export const MyCompDiv = (props) => {
  return (
    <div data-uid='div-div' style={props.style}>
      MyCompDiv
    </div>
  )
}

export const MyCompFrag = (props) => {
  return (
    <>
      <div data-uid='frag-div' style={props.style}>
        MyCompFrag
      </div>
    </>
  )
}

export var storyboard = (
  <Storyboard data-uid='storyboard'>
    <div
      data-uid='scene'
      style={{
        backgroundColor: '#aaaaaa33',
        position: 'absolute',
        left: 100,
        top: 200,
        width: 300,
        height: 300,
      }}
    >
      <MyCompDiv data-uid='mycompdiv' />
      <MyCompFrag data-uid='mycompfrag' />
    </div>
  </Storyboard>
)
`
