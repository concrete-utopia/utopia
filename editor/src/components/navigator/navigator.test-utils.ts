export const NavigatorTestProjectWithSyntheticElements = `
import * as React from 'react'
import { Scene, Storyboard } from 'utopia-api'

export var Card = (props) => {
  return (
    <div
      style={{
        height: 100,
        width: 100,
        backgroundColor: 'white',
      }}
      data-uid='card-root'
    >
      <span data-uid='card-span'>Top of Card</span>
      {
        // @utopia/uid=30d
        props.children
      }
    </div>
  )
}

export var App = (props) => {
  return (
    <div
      style={{
        height: '100%',
        width: '100%',
        contain: 'layout',
      }}
      data-uid='app-root'
    >
      <Card data-uid='card'>
        <span data-uid='card-child'>Child of Card</span>
      </Card>
      <React.Fragment data-uid='frag'>
        <div data-uid='frag-child'>Before Conditional</div>
        {
          // @utopia/uid=cond-1
          true ? (
            <div
              style={{
                backgroundColor: '#aaaaaa33',
                width: 300,
                height: 300,
              }}
              data-uid='cond-1-true'
            >
              <div
                style={{
                  backgroundColor: '#aaaaaa33',
                  width: 100,
                  height: 100,
                }}
                data-uid='cond-1-true-child'
              >
                Top
              </div>
              {
                // @utopia/uid=cond-2
                true ? (
                  <div
                    style={{
                      backgroundColor: '#aaaaaa33',
                      width: 100,
                      height: 100,
                    }}
                    data-uid='cond-2-child'
                  >
                    Bottom
                  </div>
                ) : null
              }
            </div>
          ) : null
        }
      </React.Fragment>
      {
        // @utopia/uid=children-code-block
        props.children
      }
    </div>
  )
}

export var storyboard = (
  <Storyboard data-uid='sb'>
    <Scene
      style={{
        width: 700,
        height: 759,
        position: 'absolute',
        left: 10,
        top: 10,
      }}
      data-uid='sc'
    >
      <App data-uid='app'>
        <span data-uid='app-child'>Child of App</span>
      </App>
    </Scene>
    {
      // @utopia/uid=1e7
      null
    }
  </Storyboard>
)
`
