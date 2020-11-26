import { testParseThenPrint } from './parser-printer.test-utils'
import { BakedInStoryboardUID, BakedInStoryboardVariableName } from '../../model/scene-utils'

describe('Storyboard project files', () => {
  it('are parsed and printed correctly', () => {
    const originalCode = `/** @jsx jsx */
import * as React from 'react'
import { Canvas, Scene, View, jsx } from 'utopia-api'
export var App = (props) => {
  return (
    <View
      style={{ ...props.style, backgroundColor: '#FFFFFF' }}
      layout={{ layoutSystem: 'pinSystem' }}
      data-uid={'aaa'}
    >
      <View data-uid={'9ec'}>hi</View>
    </View>
  )
}
export var ${BakedInStoryboardVariableName} = (props) => {
  return (
    <Canvas data-uid={'${BakedInStoryboardUID}'}>
      <Scene
        style={{ height: 200, left: 59, width: 200, top: 79 }}
        component={App}
        layout={{ layoutSystem: 'pinSystem' }}
        props={{ style: { height: '100%', width: '100%' }, title: 'Hi there!' }}
        data-uid={'scene-0'}
      />
      <Scene
        style={{ height: 400, left: 459, width: 400, top: 79 }}
        component={App}
        layout={{ layoutSystem: 'pinSystem' }}
        props={{ style: { height: '100%', width: '100%' }, title: 'woo there!' }}
        data-uid={'scene-1'}
      />
    </Canvas>
  )
}
`
    testParseThenPrint(originalCode, originalCode)
  })
})
