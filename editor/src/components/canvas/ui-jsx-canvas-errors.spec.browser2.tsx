import { BakedInStoryboardUID, BakedInStoryboardVariableName } from '../../core/model/scene-utils'
import { act, render } from '@testing-library/react'
import * as React from 'react'
import { testCanvasErrorInline } from './ui-jsx-canvas.test-utils'
import { TestAppUID, TestSceneUID } from './ui-jsx.test-utils'

describe('UiJsxCanvas React errors', () => {
  it('shows error when the React import is missing from storyboard', () => {
    const canvasErrors = testCanvasErrorInline(
      null,
      `
    import { View, Storyboard, Scene } from 'utopia-api'
    import { Card } from '/card.js'

    export var ${BakedInStoryboardVariableName} = (props) => {
      return (
        <Storyboard data-uid={'${BakedInStoryboardUID}'}>
          <Scene
            style={{ position: 'absolute', left: 0, top: 0, width: 400, height: 400 }}
            data-uid={'${TestSceneUID}'}
          >
            <Card
              data-uid='${TestAppUID}'
              style={{ position: 'absolute', bottom: 0, left: 0, right: 0, top: 0 }}
            />
          </Scene>
        </Storyboard>
      )
    }
    `,
      {
        'card.js': `
      import * as React from "react"
      export var Card = (props) => {
        return (
          <div style={props.style}>This is a card</div>
        )
      }
      `,
      },
    )
    expect(canvasErrors.length).toEqual(1)
    expect(canvasErrors[0].message).toEqual('React is not defined')
    // TODO a nicer matcher would be nice
    expect(canvasErrors[0].originalCode?.length).toEqual(7)
    expect(canvasErrors[0].originalCode?.[4].content).toEqual(
      `        <Storyboard data-uid={'utopia-storyboard-uid'}>`,
    )
  })

  it('shows error when the React import is missing from card.js', () => {
    const canvasErrors = testCanvasErrorInline(
      null,
      `
    import * as React from "react"
    import { View, Storyboard, Scene } from 'utopia-api'
    import { Card } from '/card.js'

    export var ${BakedInStoryboardVariableName} = (props) => {
      return (
        <Storyboard data-uid={'${BakedInStoryboardUID}'}>
          <Scene
            style={{ position: 'absolute', left: 0, top: 0, width: 400, height: 400 }}
            data-uid={'${TestSceneUID}'}
          >
            <Card
              data-uid='${TestAppUID}'
              style={{ position: 'absolute', bottom: 0, left: 0, right: 0, top: 0 }}
            />
          </Scene>
        </Storyboard>
      )
    }
    `,
      {
        'card.js': `
      export var Card = (props) => {
        return (
          <div style={props.style}>This is a card</div>
        )
      }
      `,
      },
    )
    expect(canvasErrors.length).toEqual(1)
    expect(canvasErrors[0].message).toEqual('React is not defined')
    expect(canvasErrors[0].originalCode?.length).toEqual(6)
    // TODO Figure out a way to do an inline snapshot-type thing here
    expect(canvasErrors[0].originalCode?.[1].content).toEqual(
      '      export var Card = (props) => {',
    )
  })
})
