export const ComponentsHonouringPropsStylesProject = `import * as React from 'react'
import { Scene, Storyboard } from 'utopia-api'

export const MyCompDiv = (props) => {
  return (
    <div
      data-uid='mycompdivinnerdiv'
      data-testid='mycompdivinnerdiv'
      style={props.style}
    >
      MyCompDiv
    </div>
  )
}

export const MyCompFrag = (props) => {
  return (
    <>
      <div
        data-uid='mycompfraginnerdiv'
        data-testid='mycompfraginnerdiv'
        style={props.style}
      >
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
        left: 118,
        top: 172,
        width: 300,
        height: 300,
      }}
    >
      <MyCompDiv
        data-uid='mycompdiv'
        style={{
          position: 'absolute',
          width: 137,
          height: 91.5,
          left: 137,
          top: 36,
        }}
      />
      <MyCompFrag
        data-uid='mycompfrag'
        style={{
          position: 'absolute',
          width: 150,
          height: 60.5,
          left: 21,
          top: 36,
        }}
      />
      <div
        data-uid='regulardiv'
        data-testid='regulardiv'
        style={{
          position: 'absolute',
          backgroundColor: 'lightblue',
          left: 171,
          top: 109,
          height: 93,
          width: 108,
        }}
      >
        Regular Div
      </div>
    </div>
  </Storyboard>
)
`
