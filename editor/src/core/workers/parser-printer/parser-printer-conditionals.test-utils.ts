import { BakedInStoryboardVariableName } from '../../model/scene-utils'

export const SimpleConditionalsExample = `
import * as React from 'react'
import { Scene, Storyboard, View, Group } from 'utopia-api'
export var App = (props) => {
  return <div data-uid={'div'}>
    {1 === 2 /* @utopia/uid=conditional */ ? <div data-uid={'hello'}>Hello</div> : <div data-uid={'world'}>World</div>}
  </div>
}
export var ${BakedInStoryboardVariableName} = (
  <Storyboard data-uid={'eee'}>
    <Scene
      style={{ position: 'absolute', height: 812, left: 0, width: 375, top: 0 }}
      data-uid={'fff'}
    >
      <App data-uid={'app'} style={{ position: 'absolute', bottom: 0, left: 0, right: 0, top: 0 }} />
    </Scene>
  </Storyboard>
)
`

export const NestedTernariesExample = `
import * as React from 'react'
import { Scene, Storyboard, View, Group } from 'utopia-api'
export var App = (props) => {
  return <div data-uid={'div'}>
    {
      // @utopia/uid=conditional1
      [0, 1].length > 1 ? (
        // @utopia/uid=conditional2
        [0, 1].length === 0 ? (
          <div data-uid='middle'/>
        ) : null
      ) : null}
  </div>
}
export var ${BakedInStoryboardVariableName} = (
  <Storyboard data-uid={'eee'}>
    <Scene
      style={{ position: 'absolute', height: 812, left: 0, width: 375, top: 0 }}
      data-uid={'fff'}
    >
      <App data-uid={'app'} style={{ position: 'absolute', bottom: 0, left: 0, right: 0, top: 0 }} />
    </Scene>
  </Storyboard>
)
`
