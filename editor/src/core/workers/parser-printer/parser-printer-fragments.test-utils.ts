import { BakedInStoryboardVariableName } from '../../model/scene-utils'

export const AwkwardFragmentsCode = `
import * as React from 'react'
import { Scene, Storyboard, View, Group } from 'utopia-api'
export var App = (props) => {
  return (
    <View
      style={{ ...props.style, position: 'relative', backgroundColor: '#FFFFFF' }}
      data-uid={'aaa'}
    >
      <React.Fragment data-uid={'ggg'}>
        <div data-label="random-div" style={{ width: 100, height: 100 }} data-uid={'bbb'}>Hello<>
          <div data-label="some-other-div" style={{ width: 100, height: 100 }} data-uid={'ccc'} />
        </></div>
      </React.Fragment>
      <div data-uid={'ddd'}>World</div>
    </View>
  )
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
