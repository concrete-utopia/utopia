import { testParseThenPrint } from './parser-printer.test-utils'
import { BakedInStoryboardUID, BakedInStoryboardVariableName } from '../../model/scene-utils'

describe('Storyboard project files', () => {
  it('are parsed and printed correctly', () => {
    const originalCode = `import * as React from 'react'
import { Canvas, Scene, View } from 'utopia-api'

export var App = (props) => {
  return (
    <View
      style={{
        ...props.style,
        position: 'relative',
        backgroundColor: '#FFFFFF',
      }}
      data-uid='aaa'
    >
      <View data-uid='9ec'>hi</View>
    </View>
  )
}

export var ${BakedInStoryboardVariableName} = (props) => {
  return (
    <Canvas data-uid='${BakedInStoryboardUID}'>
      <Scene
        style={{
          height: 200,
          left: 59,
          width: 200,
          top: 79,
        }}
        data-uid='scene-0'
      >
        <App
          data-uid='app-0'
          style={{ height: '100%', width: '100%' }}
          title='Hi there!'
        />
      </Scene>
      <Scene
        style={{
          height: 400,
          left: 459,
          width: 400,
          top: 79,
        }}
        data-uid='scene-1'
      >
        <App
          data-uid='app-1'
          style={{ height: '100%', width: '100%' }}
          title='woo there!'
        />
      </Scene>
    </Canvas>
  )
}
`
    testParseThenPrint('/index.js', originalCode, originalCode)
  })
})
