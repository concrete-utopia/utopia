import type { EditorRenderResult } from '../canvas/ui-jsx.test-utils'
import {
  getPrintedUiJsCode,
  makeTestProjectCodeWithSnippet,
  renderTestEditorWithCode,
  renderTestEditorWithModel,
  TestAppUID,
  TestSceneUID,
} from '../canvas/ui-jsx.test-utils'
import { act, fireEvent, screen, waitFor } from '@testing-library/react'
import type { WindowPoint } from '../../core/shared/math-utils'
import {
  canvasPoint,
  getRectCenter,
  windowPoint,
  windowRectangle,
} from '../../core/shared/math-utils'
import { BakedInStoryboardVariableName, BakedInStoryboardUID } from '../../core/model/scene-utils'
import { getDomRectCenter } from '../../core/shared/dom-utils'
import {
  selectComponents,
  setFocusedElement,
  setNavigatorRenamingTarget,
  toggleCollapse,
} from '../editor/actions/action-creators'
import * as EP from '../../core/shared/element-path'
import {
  dispatchMouseClickEventAtPoint,
  dragElementWithDNDEvents,
  mouseClickAtPoint,
  mouseDoubleClickAtPoint,
  pressKey,
} from '../canvas/event-helpers.test-utils'
import { NavigatorItemTestId } from './navigator-item/navigator-item'
import { expectNoAction, selectComponentsForTest, wait } from '../../utils/utils.test-utils'
import {
  DefaultNavigatorWidth,
  navigatorEntryToKey,
  regularNavigatorEntry,
  StoryboardFilePath,
  varSafeNavigatorEntryToKey,
} from '../editor/store/editor-state'
import {
  BottomDropTargetLineTestId,
  DragItemTestId,
  ReparentDropTargetTestId,
  TopDropTargetLineTestId,
  WiggleUnit,
} from './navigator-item/navigator-item-dnd-container'
import type { ElementPath } from '../../core/shared/project-file-types'
import { MetadataUtils } from '../../core/model/element-metadata-utils'
import type { Modifiers } from '../../utils/modifiers'
import { shiftModifier } from '../../utils/modifiers'
import { back, front } from '../../utils/utils'
import { createNavigatorReparentPostActionActions } from '../canvas/canvas-strategies/post-action-options/post-action-options'
import { createModifiedProject } from '../../sample-projects/sample-project-utils.test-utils'

const SceneRootId = 'sceneroot'
const DragMeId = 'dragme'

function getProjectCode(): string {
  return `import * as React from 'react'
import { Scene, Storyboard } from 'utopia-api'

const unmoveableColour = 'orange'

export var ${BakedInStoryboardVariableName} = (
  <Storyboard data-uid='${BakedInStoryboardUID}'>
    <Scene
      style={{
        backgroundColor: 'white',
        position: 'absolute',
        left: 0,
        top: 0,
        width: 400,
        height: 700,
      }}
      data-uid='${TestSceneUID}'
    >
      <div
        style={{
          backgroundColor: 'white',
          position: 'absolute',
          left: 0,
          top: 0,
          width: 400,
          height: 500,
        }}
        data-uid='${SceneRootId}'
        data-testid='${SceneRootId}'
        data-label='${SceneRootId}'
      >
        <div
          style={{
            backgroundColor: 'teal',
            position: 'absolute',
            left: 255,
            top: 35,
            width: 109,
            height: 123,
          }}
          data-uid='firstdiv'
          data-testid='firstdiv'
          data-label='firstdiv'
        />
        <div
          style={{
            backgroundColor: 'purple',
            position: 'absolute',
            left: 21,
            top: 215.5,
            width: 123,
            height: 100,
          }}
          data-uid='seconddiv'
          data-testid='seconddiv'
          data-label='seconddiv'
        />
        <div
          style={{
            backgroundColor: 'green',
            position: 'absolute',
            left: 26,
            top: 35,
            width: 118,
            height: 123,
          }}
          data-uid='thirddiv'
          data-testid='thirddiv'
          data-label='thirddiv'
        />
        <div
          style={{
            backgroundColor: '#aaaaaa33',
            height: 65,
            width: 66,
            position: 'absolute',
            left: 265,
            top: 233,
          }}
          data-uid='${DragMeId}'
          data-testid='${DragMeId}'
          data-label='${DragMeId}'
        >
          drag me
        </div>
        <div
          style={{
            backgroundColor: unmoveableColour,
            height: 65,
            width: 66,
            position: 'absolute',
            left: 265,
            top: 300,
          }}
          data-uid='notdrag'
          data-testid='notdrag'
          data-label='notdrag'
        >
          not drag
        </div>
      </div>
      <div
        style={{
          backgroundColor: 'white',
          position: 'absolute',
          left: 0,
          top: 500,
          width: 400,
          height: 200,
        }}
        data-uid='parentsibling'
        data-testid='parentsibling'
        data-label='parentsibling'
      />
    </Scene>
  </Storyboard>
)
`
}

function getProjectCodeWithMapExpression(): string {
  return `import * as React from 'react'
import { Scene, Storyboard } from 'utopia-api'

const unmoveableColour = 'orange'

export var ${BakedInStoryboardVariableName} = (
  <Storyboard data-uid='${BakedInStoryboardUID}'>
    <Scene
      style={{
        backgroundColor: 'white',
        position: 'absolute',
        left: 0,
        top: 0,
        width: 400,
        height: 700,
      }}
      data-uid='${TestSceneUID}'
    >
      <div
        style={{
          backgroundColor: 'white',
          position: 'absolute',
          left: 0,
          top: 0,
          width: 400,
          height: 500,
        }}
        data-uid='${SceneRootId}'
        data-testid='${SceneRootId}'
        data-label='${SceneRootId}'
      >
        <div
          style={{
            backgroundColor: 'teal',
            position: 'absolute',
            left: 255,
            top: 35,
            width: 109,
            height: 123,
          }}
          data-uid='firstdiv'
          data-testid='firstdiv'
          data-label='firstdiv'
        />
        <div
          style={{
            backgroundColor: 'purple',
            position: 'absolute',
            left: 21,
            top: 215.5,
            width: 123,
            height: 100,
          }}
          data-uid='seconddiv'
          data-testid='seconddiv'
          data-label='seconddiv'
        />
        <div
          style={{
            backgroundColor: 'green',
            position: 'absolute',
            left: 26,
            top: 35,
            width: 118,
            height: 123,
          }}
          data-uid='thirddiv'
          data-testid='thirddiv'
          data-label='thirddiv'
        />
        {
          // @utopia/uid=e46
          [0,1,2,3].map(i => (<div
            style={{
              backgroundColor: '#aaaaaa33',
              height: 65,
              width: 66,
              position: 'absolute',
              left: 265 + 100*i,
              top: 233,
            }}
            data-uid='202'
          >
            drag my parent
          </div>))
        }
        <div
          style={{
            backgroundColor: unmoveableColour,
            height: 65,
            width: 66,
            position: 'absolute',
            left: 265,
            top: 300,
          }}
          data-uid='notdrag'
          data-testid='notdrag'
          data-label='notdrag'
        >
          not drag
        </div>
      </div>
      <div
        style={{
          backgroundColor: 'white',
          position: 'absolute',
          left: 0,
          top: 500,
          width: 400,
          height: 200,
        }}
        data-uid='parentsibling'
        data-testid='parentsibling'
        data-label='parentsibling'
      />
    </Scene>
  </Storyboard>
)
`
}

function getProjectCodeWithExpression(): string {
  return `import * as React from 'react'
import { Scene, Storyboard } from 'utopia-api'

const unmoveableColour = 'orange'

export var ${BakedInStoryboardVariableName} = (
  <Storyboard data-uid='${BakedInStoryboardUID}'>
    <Scene
      style={{
        backgroundColor: 'white',
        position: 'absolute',
        left: 0,
        top: 0,
        width: 400,
        height: 700,
      }}
      data-uid='${TestSceneUID}'
    >
      <div
        style={{
          backgroundColor: 'white',
          position: 'absolute',
          left: 0,
          top: 0,
          width: 400,
          height: 500,
        }}
        data-uid='${SceneRootId}'
        data-testid='${SceneRootId}'
        data-label='${SceneRootId}'
      >
        <div
          style={{
            backgroundColor: 'teal',
            position: 'absolute',
            left: 255,
            top: 35,
            width: 109,
            height: 123,
          }}
          data-uid='firstdiv'
          data-testid='firstdiv'
          data-label='firstdiv'
        />
        <div
          style={{
            backgroundColor: 'purple',
            position: 'absolute',
            left: 21,
            top: 215.5,
            width: 123,
            height: 100,
          }}
          data-uid='seconddiv'
          data-testid='seconddiv'
          data-label='seconddiv'
        />
        <div
          style={{
            backgroundColor: 'green',
            position: 'absolute',
            left: 26,
            top: 35,
            width: 118,
            height: 123,
          }}
          data-uid='thirddiv'
          data-testid='thirddiv'
          data-label='thirddiv'
        />
        {
          // @utopia/uid=b62
          (() => <div
            style={{
              backgroundColor: '#aaaaaa33',
              height: 65,
              width: 66,
              position: 'absolute',
              left: 265,
              top: 233,
            }}
          >
            drag my parent
          </div>)()
        }
        <div
          style={{
            backgroundColor: unmoveableColour,
            height: 65,
            width: 66,
            position: 'absolute',
            left: 265,
            top: 300,
          }}
          data-uid='notdrag'
          data-testid='notdrag'
          data-label='notdrag'
        >
          not drag
        </div>
      </div>
      <div
        style={{
          backgroundColor: 'white',
          position: 'absolute',
          left: 0,
          top: 500,
          width: 400,
          height: 200,
        }}
        data-uid='parentsibling'
        data-testid='parentsibling'
        data-label='parentsibling'
      />
    </Scene>
  </Storyboard>
)
`
}

const projectWithHierarchy = `import * as React from 'react'
import { Storyboard } from 'utopia-api'

export var storyboard = (
  <Storyboard data-uid='sb'>
    <div
      style={{
        backgroundColor: '#aaaaaa33',
        position: 'absolute',
        left: 0,
        top: 0,
        width: 207,
        height: 311,
      }}
      data-testid='parent1'
      data-uid='parent1'
    >
      <div
        style={{
          backgroundColor: '#aaaaaa33',
          width: 73,
          height: 109,
          left: -18,
          top: 164,
          position: 'absolute',
        }}
        data-uid='child1'
      />
      <div
        style={{
          backgroundColor: '#aaaaaa33',
          width: 207,
          height: 202,
          left: -18,
          top: -38,
          position: 'absolute',
        }}
        data-uid='755'
      />
    </div>
    <div
      style={{
        backgroundColor: '#aaaaaa33',
        position: 'absolute',
        left: 51,
        top: 444,
        width: 207,
        height: 311,
      }}
      data-uid='parent2'
      data-testid='parent2'
    >
      <div
        style={{
          backgroundColor: '#aaaaaa33',
          width: 73,
          height: 109,
          left: -18,
          top: 164,
          position: 'absolute',
        }}
        data-uid='aaa'
      />
      <div
        style={{
          backgroundColor: '#aaaaaa33',
          width: 207,
          height: 202,
          left: -18,
          top: -38,
          position: 'absolute',
        }}
        data-uid='aab'
      />
    </div>
    <span
      style={{
        position: 'absolute',
        wordBreak: 'break-word',
        left: 326,
        top: 215,
        width: 143,
        height: 19,
      }}
      data-testid='text'
      data-uid='text'
    >
      Cannot reparent here
    </span>
  </Storyboard>
)
`

const projectWithGroupsAndNotGroups = `import * as React from 'react'
import { Storyboard } from 'utopia-api'
export var storyboard = (
  <Storyboard data-uid='sb'>
    <div data-uid='group'>
      <div
        style={{
          backgroundColor: '#aaaaaa33',
          position: 'absolute',
          left: 1136,
          top: 325,
          width: 141,
          height: 190,
        }}
        data-uid='groupchild'
      />
    </div>
    <React.Fragment data-uid='fragment'>
      <div
        style={{
          backgroundColor: '#aaaaaa33',
          position: 'absolute',
          left: 257.5,
          top: 229,
          width: 250,
          height: 238,
        }}
        data-uid='fragmentchild'
      />
    </React.Fragment>
    <div
      data-uid='offsetparent'
      style={{
        position: 'absolute',
        width: 310,
        height: 575,
        left: 566,
        top: -2,
      }}
    >
      <div
        style={{
          backgroundColor: '#aaaaaa33',
          position: 'absolute',
          left: 165,
          top: 360,
          width: 119,
          height: 193,
        }}
        data-uid='offsetchild'
      />
    </div>
    <div
      data-uid='nonoffsetparent'
      style={{
        width: 310,
        height: 575,
        left: 248,
        top: 515,
      }}
    >
      <div
        style={{
          backgroundColor: '#aaaaaa33',
          position: 'absolute',
          left: 83,
          top: 274,
          width: 119,
          height: 193,
        }}
        data-uid='nonoffsetchild'
      />
    </div>
  </Storyboard>
)
`

const projectWithMapExpression = `import * as React from 'react'
import { Storyboard } from 'utopia-api'
export var storyboard = (
  <Storyboard data-uid='sb'>
    <div data-uid='group'>
      {
        // @utopia/uid=70f
        [0, 1, 2].map((i) => (
          <div data-uid='33d'>first {i}</div>
        ))
      }

      <div data-uid='foo'>foo</div>
      {
        // @utopia/uid=b68
        [0, 1, 2].map((i) => (
          <>
            <div data-uid='46a'>second {i}</div>
            <div data-uid='46a'>third {i}</div>
          </>
        ))
      }

      {
        // @utopia/uid=cond
        true ? (
          // @utopia/uid=929
          [0, 1, 2].map((i) => <div data-uid='f23'>fourth {i}</div>)
        ) : (
          <div />
        )
      }

      <div data-uid='bar'>bar</div>

      {
        // @utopia/uid=651
        // @utopia/map-count=2
        [0, 1, 2].map((i) => (
          <div data-uid='3bc'>fifth {i}</div>
        ))
      }

      <div data-uid='text-expr-cond-wrapper'>{
        // @utopia/uid=text-expr-cond
        true ? (
          // @utopia/uid=619
          [0, 1, 2].map(() => 'fourth')
        ) : (
          <div />
        )
      }</div>

      <div data-uid='zero-length-map-wrapper'>
        {[].map(() => (
          <div>will not render</div>
        ))}
      </div>

    </div>
  </Storyboard>
)
`

const projectWithExpression = `import * as React from 'react'
import { Storyboard } from 'utopia-api'
export var storyboard = (
  <Storyboard data-uid='sb'>
    <div data-uid='group'>
      {
        // @utopia/uid=0b5
        (() => 
          <div>first</div>
        )()
      }

      <div data-uid='foo'>foo</div>

      {
        // @utopia/uid=809
        (() => 
          <>
            <div>second</div>
            <div>third</div>
          </>
        )()
      }

      {
        // @utopia/uid=cond
        true ? (
          // @utopia/uid=4ce
          (() => <div>fourth</div>)()
        ) : (
          <div />
        )
      }

      <div data-uid='bar'>bar</div>

      {
        // @utopia/uid=bf0
        (() => 
        <div>fifth</div>
        )()
      }
    </div>
  </Storyboard>
)
`

const projectWithTextFromExpression = `import * as React from 'react'
import { Storyboard } from 'utopia-api'
export var storyboard = (
  <Storyboard data-uid='sb'>
    <div data-uid='div'>{1+1}</div>
  </Storyboard>
)
`

const projectWithFlexContainerAndCanvas = `import * as React from 'react'
import { Scene, Storyboard, View, Group } from 'utopia-api'

export var App = (props) => {
  return (
    <div
      style={{
        width: '100%',
        height: '100%',
        contain: 'layout',
      }}
      data-uid='root'
    >
      <div
        style={{
          backgroundColor: '#aaaaaa33',
          position: 'absolute',
          left: 50.5,
          top: 26,
          width: 'max-content',
          height: 'max-content',
          display: 'flex',
          flexDirection: 'row',
          gap: 37,
          padding: '21px 28.5px',
          alignItems: 'flex-end',
        }}
        data-uid='container'
      >
        <div
          style={{
            backgroundColor: '#0075ff',
            width: 58,
            height: 75,
            contain: 'layout',
          }}
          data-uid='flex-child'
        />
        <div
          style={{
            backgroundColor: '#f24e1d',
            width: 65,
            height: 63,
            contain: 'layout',
          }}
          data-uid='663'
        />
      </div>
      <div
        style={{
          backgroundColor: '#aaaaaa33',
          position: 'absolute',
          left: 55,
          top: 226,
          width: 298,
          height: 117,
        }}
        data-uid='abs-container'
      >
        <div
          style={{
            backgroundColor: '#35a853',
            position: 'absolute',
            left: 32,
            top: 23,
            width: 55,
            height: 74,
          }}
          data-uid='abs-child'
        />
        <div
          style={{
            backgroundColor: '#fbbc07',
            position: 'absolute',
            left: 207,
            top: 25,
            width: 55,
            height: 72,
          }}
          data-uid='8ae'
        />
      </div>
    </div>
  )
}

export var storyboard = (props) => {
  return (
    <Storyboard data-uid='sb'>
      <Scene
        style={{ left: 0, top: 0, width: 400, height: 400 }}
        data-uid='scene'
      >
        <App
          data-uid='app'
          style={{
            position: '',
            bottom: 0,
            left: 0,
            right: 0,
            top: 0,
          }}
        />
      </Scene>
      <div
        style={{
          backgroundColor: '#09cf83',
          position: 'absolute',
          left: 456,
          top: 26,
          width: 96,
          height: 95,
        }}
        data-uid='dragme'
      />
    </Storyboard>
  )
}
`

const projectWithRenderProp = (renderPropSource: string) =>
  createModifiedProject({
    [StoryboardFilePath]: `import * as React from 'react'
import * as Utopia from 'utopia-api'
import {
  Storyboard,
  Scene,
} from 'utopia-api'

export function Card({ header, children }) {
  return (
    <div data-uid='root'>
      <h2>{header}</h2>
      {children}
    </div>
  )
}

var Playground = ({ style }) => {
  return (
    <div style={style} data-uid='dbc'>
      <Card ${renderPropSource} data-uid='78c'>
        <p>Card contents</p>
      </Card>
    </div>
  )
}

export var storyboard = (
  <Storyboard data-uid='sb'>
    <Scene
      style={{
        width: 521,
        height: 266,
        position: 'absolute',
        left: 554,
        top: 247,
        backgroundColor: 'white',
      }}
      data-uid='scene'
      data-testid='scene'
      commentId='120'
    >
      <Playground
        style={{
          width: 454,
          height: 177,
          position: 'absolute',
          left: 34,
          top: 44,
          backgroundColor: 'white',
          display: 'flex',
          alignItems: 'center',
          justifyContent: 'center',
        }}
        className='playground'
        css={{ color: 'red' }}
        data-uid='pg'
      />
    </Scene>
  </Storyboard>
)
`,
    ['/utopia/components.utopia.js']: `import { Card } from './storyboard'
    
    const Components = {
  '/utopia/storyboard': {
    Card: {
      component: Card,
      properties: {
        header: {
          control: 'jsx',
          preferredContents: {
            component: 'span',
            variants: {
              label: 'Span with Title',
              code: '<span>Title</span>',
            }
          },
        },
      },
      variants: [],
    },
  },
}

export default Components
`,
  })

const projectWithThirdPartyRenderProp = (renderPropSource: string) =>
  createModifiedProject({
    [StoryboardFilePath]: `import * as React from 'react'
import * as Utopia from 'utopia-api'
import {
  Storyboard,
  Scene,
} from 'utopia-api'
export function Card({ header, children }) {
  return (
    <div data-uid='root'>
      <h2>{header}</h2>
      {children}
    </div>
  )
}
var Playground = ({ style }) => {
  return (
    <div style={style} data-uid='dbc'>
      <Card ${renderPropSource} data-uid='78c'>
        <p>Card contents</p>
      </Card>
    </div>
  )
}
export var storyboard = (
  <Storyboard data-uid='sb'>
    <Scene
      style={{
        width: 521,
        height: 266,
        position: 'absolute',
        left: 554,
        top: 247,
        backgroundColor: 'white',
      }}
      data-uid='scene'
      data-testid='scene'
      commentId='120'
    >
      <Playground
        style={{
          width: 454,
          height: 177,
          position: 'absolute',
          left: 34,
          top: 44,
          backgroundColor: 'white',
          display: 'flex',
          alignItems: 'center',
          justifyContent: 'center',
        }}
        className='playground'
        css={{ color: 'red' }}
        data-uid='pg'
      />
    </Scene>
  </Storyboard>
)
`,
    ['/utopia/components.utopia.js']: `
    import { Card } from './storyboard'
    import { ExportedForTheSakeOfExportingIt } from "../src/header"
    
    const Components = {
      '/utopia/storyboard': {
        Card: {
          component: Card,
          properties: {
            [ExportedForTheSakeOfExportingIt]: {
              control: 'jsx',
              preferredContents: [
                {
                  component: 'Heading',
                  variants: [
                    {
                      label: 'Heading with Title',
                      imports: 'import { Heading } from "/src/heading"',
                      code: '<Heading>Title</Heading>',
                    },
                  ],
                },
              ],
            },
          },
          variants: [],
        },
      },
    }
    
    export default Components    
`,
    ['/src/header.js']: `
    export const ExportedForTheSakeOfExportingIt = 'header'
`,
    ['/src/heading.js']: `import React from 'react'

export function Heading({ children }) {
  return <h1 data-uid='708'>{children}</h1>
}
`,
  })

const projectWithEarlyReturn = createModifiedProject({
  [StoryboardFilePath]: `import * as React from 'react'
import * as Utopia from 'utopia-api'
import {
  Storyboard,
  Scene,
} from 'utopia-api'

var Playground = ({ style }) => {
  if (true) {
    return null
  }
  return (
    <div style={style} data-uid='dbc'>Cake</div>
  )
}
export var storyboard = (
  <Storyboard data-uid='sb'>
    <Scene
      style={{
        width: 521,
        height: 266,
        position: 'absolute',
        left: 554,
        top: 247,
        backgroundColor: 'white',
      }}
      data-uid='scene'
      data-testid='scene'
      commentId='120'
    >
      <Playground
        style={{
          width: 454,
          height: 177,
          position: 'absolute',
          left: 34,
          top: 44,
          backgroundColor: 'white',
          display: 'flex',
          alignItems: 'center',
          justifyContent: 'center',
        }}
        className='playground'
        css={{ color: 'red' }}
        data-uid='pg'
      />
    </Scene>
  </Storyboard>
)
`,
})

function getProjectCodeForMultipleSelection(): string {
  return `import * as React from 'react'
import { Scene, Storyboard } from 'utopia-api'

const unmoveableColour = 'orange'

export var ${BakedInStoryboardVariableName} = (
  <Storyboard data-uid='${BakedInStoryboardUID}'>
    <Scene
      style={{
        left: 0,
        top: 0,
        width: 400,
        height: 700,
      }}
      data-uid='${TestSceneUID}'
    >
      <div
        data-uid='${SceneRootId}'
        data-testid='${SceneRootId}'
        data-label='${SceneRootId}'
      >
        <div
          data-uid='one'
          data-testid='one'
          data-label='one'
        />
        <div
          data-uid='two'
          data-testid='two'
          data-label='two'
        />
        <div
          data-uid='three'
          data-testid='three'
          data-label='three'
        >
          <div
            data-uid='four'
            data-testid='four'
            data-label='four'
          />
          <div
            data-uid='five'
            data-testid='five'
            data-label='five'
          />
        </div>
        <div
          data-uid='six'
          data-testid='six'
          data-label='six'
        >
          <div
            data-uid='seven'
            data-testid='seven'
            data-label='seven'
          />
          <div
            data-uid='eight'
            data-testid='eight'
            data-label='eight'
          />
        </div>
        <div
          data-uid='nine'
          data-testid='nine'
          data-label='nine'
        />
      </div>
    </Scene>
  </Storyboard>
)
`
}

describe('Navigator', () => {
  async function doBasicDrag(
    editor: EditorRenderResult,
    dragMeElementPath: ElementPath,
    targetElementPath: ElementPath,
    dropTarget = BottomDropTargetLineTestId,
  ): Promise<{ dragMeElement: HTMLElement; startingDragMeElementStyle: CSSStyleDeclaration }> {
    const dragMeElement = await editor.renderedDOM.findByTestId(
      `navigator-item-${varSafeNavigatorEntryToKey(regularNavigatorEntry(dragMeElementPath))}`,
    )

    const startingDragMeElementStyle = { ...dragMeElement.style }

    const dragMeElementRect = dragMeElement.getBoundingClientRect()
    const dragMeElementCenter = getDomRectCenter(dragMeElementRect)

    const targetElement = await editor.renderedDOM.findByTestId(
      `navigator-item-${varSafeNavigatorEntryToKey(regularNavigatorEntry(targetElementPath))}`,
    )
    const targetElementRect = targetElement.getBoundingClientRect()
    const targetElementCenter = getDomRectCenter(targetElementRect)
    const dragTo = {
      x: targetElementCenter.x,
      y: targetElementRect.y + 3,
    }

    const dragDelta = windowPoint({
      x: dragTo.x - dragMeElementCenter.x,
      y: dragTo.y - dragMeElementCenter.y,
    })

    await selectComponentsForTest(editor, [dragMeElementPath])

    await act(async () =>
      dragElementWithDNDEvents(
        editor,
        DragItemTestId(varSafeNavigatorEntryToKey(regularNavigatorEntry(dragMeElementPath))),
        dropTarget(varSafeNavigatorEntryToKey(regularNavigatorEntry(targetElementPath))),
        windowPoint(dragMeElementCenter),
        dragDelta,
      ),
    )

    await editor.getDispatchFollowUpActionsFinished()

    return { dragMeElement, startingDragMeElementStyle }
  }
  async function doBasicDragMultiselect(
    editor: EditorRenderResult,
    dragMeElementPaths: Array<ElementPath>,
    targetElementPath: ElementPath,
    dropTarget = BottomDropTargetLineTestId,
    dragDeltaOffset: WindowPoint = windowPoint({ x: 0, y: 0 }), // to target an ancestor
  ): Promise<{ dragMeElement: HTMLElement; startingDragMeElementStyle: CSSStyleDeclaration }> {
    const dragMeElement = await editor.renderedDOM.findByTestId(
      `navigator-item-${varSafeNavigatorEntryToKey(regularNavigatorEntry(dragMeElementPaths[0]))}`,
    )

    const startingDragMeElementStyle = { ...dragMeElement.style }

    const dragMeElementRect = dragMeElement.getBoundingClientRect()
    const dragMeElementCenter = getDomRectCenter(dragMeElementRect)

    const targetElement = await editor.renderedDOM.findByTestId(
      `navigator-item-${varSafeNavigatorEntryToKey(regularNavigatorEntry(targetElementPath))}`,
    )
    const targetElementRect = targetElement.getBoundingClientRect()
    const targetElementCenter = getDomRectCenter(targetElementRect)
    const dragTo = {
      x: targetElementCenter.x,
      y: targetElementRect.y + 3,
    }

    const dragDelta = windowPoint({
      x: dragTo.x - dragMeElementCenter.x + dragDeltaOffset.x,
      y: dragTo.y - dragMeElementCenter.y + dragDeltaOffset.y,
    })

    await selectComponentsForTest(editor, dragMeElementPaths)

    await act(async () =>
      dragElementWithDNDEvents(
        editor,
        DragItemTestId(varSafeNavigatorEntryToKey(regularNavigatorEntry(dragMeElementPaths[0]))),
        dropTarget(varSafeNavigatorEntryToKey(regularNavigatorEntry(targetElementPath))),
        windowPoint(dragMeElementCenter),
        dragDelta,
      ),
    )

    await editor.getDispatchFollowUpActionsFinished()

    return { dragMeElement, startingDragMeElementStyle }
  }

  describe('selecting elements', () => {
    it('by clicking the center of the item', async () => {
      const renderResult = await renderTestEditorWithCode(
        getProjectCode(),
        'await-first-dom-report',
      )

      const dragMePath = EP.fromString(`${BakedInStoryboardUID}/${TestSceneUID}/sceneroot/dragme`)

      const dragMeElement = await renderResult.renderedDOM.findByTestId(
        NavigatorItemTestId(varSafeNavigatorEntryToKey(regularNavigatorEntry(dragMePath))),
      )

      const dragMeElementRect = dragMeElement.getBoundingClientRect()

      await mouseClickAtPoint(dragMeElement, {
        x: dragMeElementRect.x + dragMeElementRect.width / 2,
        y: dragMeElementRect.y + dragMeElementRect.height / 2,
      })

      await renderResult.getDispatchFollowUpActionsFinished()

      const selectedViewPaths = renderResult.getEditorState().editor.selectedViews.map(EP.toString)
      expect(selectedViewPaths).toEqual([EP.toString(dragMePath)])
    })

    it('by clicking the center of the item which is a map expression', async () => {
      const renderResult = await renderTestEditorWithCode(
        projectWithMapExpression,
        'await-first-dom-report',
      )

      const dragMePath = EP.fromString('sb/group/b68')

      const dragMeElement = await renderResult.renderedDOM.findByTestId(
        'NavigatorItemTestId-regular_sb/group/b68',
      )

      const dragMeElementRect = dragMeElement.getBoundingClientRect()

      await mouseClickAtPoint(dragMeElement, {
        x: dragMeElementRect.x + dragMeElementRect.width / 2,
        y: dragMeElementRect.y + dragMeElementRect.height / 2,
      })

      await renderResult.getDispatchFollowUpActionsFinished()

      const selectedViewPaths = renderResult.getEditorState().editor.selectedViews.map(EP.toString)
      expect(selectedViewPaths).toEqual([EP.toString(dragMePath)])
    })

    it('by clicking the center of the item which is an expression', async () => {
      const renderResult = await renderTestEditorWithCode(
        projectWithExpression,
        'await-first-dom-report',
      )

      const dragMePath = EP.fromString('sb/group/809')

      const dragMeElement = await renderResult.renderedDOM.findByTestId(
        'NavigatorItemTestId-regular_sb/group/809',
      )

      const dragMeElementRect = dragMeElement.getBoundingClientRect()

      await mouseClickAtPoint(dragMeElement, {
        x: dragMeElementRect.x + dragMeElementRect.width / 2,
        y: dragMeElementRect.y + dragMeElementRect.height / 2,
      })

      await renderResult.getDispatchFollowUpActionsFinished()

      const selectedViewPaths = renderResult.getEditorState().editor.selectedViews.map(EP.toString)
      expect(selectedViewPaths).toEqual([EP.toString(dragMePath)])
    })

    it('by clicking the top of the item', async () => {
      const renderResult = await renderTestEditorWithCode(
        getProjectCode(),
        'await-first-dom-report',
      )

      const clickMePath = EP.fromString(`${BakedInStoryboardUID}/${TestSceneUID}/sceneroot/dragme`)

      const clickMeElement = renderResult.renderedDOM.getByTestId(
        DragItemTestId(varSafeNavigatorEntryToKey(regularNavigatorEntry(clickMePath))),
      )

      const clickMeElementRect = clickMeElement.getBoundingClientRect()

      dispatchMouseClickEventAtPoint(
        windowPoint({
          x: clickMeElementRect.x + clickMeElementRect.width / 2,
          y: clickMeElementRect.y + 1,
        }),
      )

      const selectedViewPaths = renderResult.getEditorState().editor.selectedViews.map(EP.toString)
      expect(selectedViewPaths).toEqual([EP.toString(clickMePath)])
    })

    it('by clicking the bottom of the item', async () => {
      const renderResult = await renderTestEditorWithCode(
        getProjectCode(),
        'await-first-dom-report',
      )

      const clickMePath = EP.fromString(`${BakedInStoryboardUID}/${TestSceneUID}/sceneroot/dragme`)

      const clickMeElement = await renderResult.renderedDOM.findByTestId(
        DragItemTestId(varSafeNavigatorEntryToKey(regularNavigatorEntry(clickMePath))),
      )

      const clickMeElementRect = clickMeElement.getBoundingClientRect()
      dispatchMouseClickEventAtPoint(
        windowPoint({
          x: clickMeElementRect.x + clickMeElementRect.width / 2,
          y: clickMeElementRect.y + clickMeElementRect.height - 1,
        }),
      )

      const selectedViewPaths = renderResult.getEditorState().editor.selectedViews.map(EP.toString)
      expect(selectedViewPaths).toEqual([EP.toString(clickMePath)])
    })

    describe('multiple items', () => {
      function makePathString(uid: string) {
        return `${BakedInStoryboardUID}/${TestSceneUID}/sceneroot/${uid}`
      }

      async function clickElement(
        renderResult: EditorRenderResult,
        uid: string,
        modifiers?: Modifiers,
      ) {
        const path = EP.fromString(makePathString(uid))
        const element = await renderResult.renderedDOM.findByTestId(
          NavigatorItemTestId(varSafeNavigatorEntryToKey(regularNavigatorEntry(path))),
        )
        const rect = element.getBoundingClientRect()
        await mouseClickAtPoint(
          element,
          {
            x: rect.x + rect.width / 2,
            y: rect.y + rect.height / 2,
          },
          { modifiers },
        )
        await renderResult.getDispatchFollowUpActionsFinished()

        return path
      }

      it('selects a range of items with shift', async () => {
        const renderResult = await renderTestEditorWithCode(
          getProjectCodeForMultipleSelection(),
          'await-first-dom-report',
        )

        await clickElement(renderResult, 'one')
        await clickElement(renderResult, 'three/four', shiftModifier)

        const selectedViewPaths = renderResult
          .getEditorState()
          .editor.selectedViews.map(EP.toString)

        expect(selectedViewPaths).toEqual([
          makePathString('one'),
          makePathString('two'),
          makePathString('three'),
          makePathString('three/four'),
        ])
      })

      it('selects a range of items with shift even when they are collapsed', async () => {
        const renderResult = await renderTestEditorWithCode(
          getProjectCodeForMultipleSelection(),
          'await-first-dom-report',
        )

        await renderResult.dispatch([toggleCollapse(EP.fromString(makePathString('three')))], true)

        await clickElement(renderResult, 'two')
        await clickElement(renderResult, 'six', shiftModifier)

        const selectedViewPaths = renderResult
          .getEditorState()
          .editor.selectedViews.map(EP.toString)

        expect(selectedViewPaths).toEqual([
          makePathString('two'),
          makePathString('three'),
          makePathString('three/four'),
          makePathString('three/five'),
          makePathString('six'),
        ])
      })
    })
  })

  describe('reordering', () => {
    it('reorders to before the first sibling', async () => {
      const renderResult = await renderTestEditorWithCode(
        getProjectCode(),
        'await-first-dom-report',
      )

      const dragMeElement = await renderResult.renderedDOM.findByTestId(
        `navigator-item-regular_utopia_storyboard_uid/scene_aaa/sceneroot/dragme`,
      )
      const dragMeElementRect = dragMeElement.getBoundingClientRect()
      const dragMeElementCenter = getDomRectCenter(dragMeElementRect)
      const firstDivElement = await renderResult.renderedDOM.findByTestId(
        `navigator-item-regular_utopia_storyboard_uid/scene_aaa/sceneroot/firstdiv`,
      )
      const firstDivElementRect = firstDivElement.getBoundingClientRect()
      const firstDivElementCenter = getDomRectCenter(firstDivElementRect)
      const dragTo = {
        x: firstDivElementCenter.x,
        y: firstDivElementRect.y + 3,
      }

      const dragDelta = windowPoint({
        x: dragTo.x - dragMeElementCenter.x,
        y: dragTo.y - dragMeElementCenter.y,
      })

      const targetElement = EP.fromString('utopia-storyboard-uid/scene-aaa/sceneroot/dragme')
      await selectComponentsForTest(renderResult, [targetElement])

      await dragElementWithDNDEvents(
        renderResult,
        `navigator-item-drag-regular_utopia_storyboard_uid/scene_aaa/sceneroot/dragme`,
        `navigator-item-drop-before-regular_utopia_storyboard_uid/scene_aaa/sceneroot/firstdiv`,
        windowPoint(dragMeElementCenter),
        dragDelta,

        async () => {
          expect(renderResult.getEditorState().editor.navigator.dropTargetHint?.type).toEqual(
            'before',
          )
          // parent highlight is shown
          const parentEntry = renderResult.renderedDOM.getByTestId(
            `navigator-item-regular_utopia_storyboard_uid/scene_aaa/sceneroot`,
          )
          expect((parentEntry.firstChild?.firstChild as HTMLElement).style.outline).toEqual(
            '1px solid var(--utopitheme-navigatorResizeHintBorder)',
          )

          // drop target line is shown
          const dropTarget = renderResult.renderedDOM.getByTestId(
            `navigator-item-drop-before-regular_utopia_storyboard_uid/scene_aaa/sceneroot/firstdiv`,
          )
          expect(dropTarget.style.opacity).toEqual('1')
        },
      )

      expect(
        renderResult.getEditorState().derived.navigatorTargets.map(navigatorEntryToKey),
      ).toEqual([
        'regular-utopia-storyboard-uid/scene-aaa',
        'regular-utopia-storyboard-uid/scene-aaa/sceneroot',
        'regular-utopia-storyboard-uid/scene-aaa/sceneroot/dragme', // <- moved to before `firstdiv`
        'regular-utopia-storyboard-uid/scene-aaa/sceneroot/firstdiv',
        'regular-utopia-storyboard-uid/scene-aaa/sceneroot/seconddiv',
        'regular-utopia-storyboard-uid/scene-aaa/sceneroot/thirddiv',
        'regular-utopia-storyboard-uid/scene-aaa/sceneroot/notdrag',
        'regular-utopia-storyboard-uid/scene-aaa/parentsibling',
      ])
    })

    it('reorders to after the first sibling', async () => {
      const renderResult = await renderTestEditorWithCode(
        getProjectCode(),
        'await-first-dom-report',
      )

      const dragMeElement = await renderResult.renderedDOM.findByTestId(
        `navigator-item-regular_utopia_storyboard_uid/scene_aaa/sceneroot/dragme`,
      )
      const dragMeElementRect = dragMeElement.getBoundingClientRect()
      const dragMeElementCenter = getDomRectCenter(dragMeElementRect)
      const firstDivElement = await renderResult.renderedDOM.findByTestId(
        `navigator-item-regular_utopia_storyboard_uid/scene_aaa/sceneroot/firstdiv`,
      )
      const firstDivElementRect = firstDivElement.getBoundingClientRect()
      const firstDivElementCenter = getDomRectCenter(firstDivElementRect)
      const dragTo = {
        x: firstDivElementCenter.x,
        y: firstDivElementRect.y + firstDivElementRect.height,
      }

      const dragDelta = windowPoint({
        x: dragTo.x - dragMeElementCenter.x,
        y: dragTo.y - dragMeElementCenter.y,
      })

      const targetElement = EP.fromString('utopia-storyboard-uid/scene-aaa/sceneroot/dragme')
      await act(async () => {
        const dispatchDone = renderResult.getDispatchFollowUpActionsFinished()
        await renderResult.dispatch([selectComponents([targetElement], false)], false)
        await dispatchDone
      })

      await dragElementWithDNDEvents(
        renderResult,
        `navigator-item-drag-regular_utopia_storyboard_uid/scene_aaa/sceneroot/dragme`,
        `navigator-item-drop-after-regular_utopia_storyboard_uid/scene_aaa/sceneroot/firstdiv`,
        windowPoint(dragMeElementCenter),
        dragDelta,

        async () => {
          expect(renderResult.getEditorState().editor.navigator.dropTargetHint?.type).toEqual(
            'after',
          )
          // parent highlight is shown
          const parentEntry = renderResult.renderedDOM.getByTestId(
            `navigator-item-regular_utopia_storyboard_uid/scene_aaa/sceneroot`,
          )
          expect((parentEntry.firstChild?.firstChild as HTMLElement).style.outline).toEqual(
            '1px solid var(--utopitheme-navigatorResizeHintBorder)',
          )

          // drop target line is shown
          const dropTarget = renderResult.renderedDOM.getByTestId(
            `navigator-item-drop-after-regular_utopia_storyboard_uid/scene_aaa/sceneroot/firstdiv`,
          )
          expect(dropTarget.style.opacity).toEqual('1')
        },
      )

      expect(
        renderResult.getEditorState().derived.navigatorTargets.map(navigatorEntryToKey),
      ).toEqual([
        'regular-utopia-storyboard-uid/scene-aaa',
        'regular-utopia-storyboard-uid/scene-aaa/sceneroot',
        'regular-utopia-storyboard-uid/scene-aaa/sceneroot/firstdiv',
        'regular-utopia-storyboard-uid/scene-aaa/sceneroot/dragme', // <- moved to after `firstdiv`
        'regular-utopia-storyboard-uid/scene-aaa/sceneroot/seconddiv',
        'regular-utopia-storyboard-uid/scene-aaa/sceneroot/thirddiv',
        'regular-utopia-storyboard-uid/scene-aaa/sceneroot/notdrag',
        'regular-utopia-storyboard-uid/scene-aaa/parentsibling',
      ])
    })

    it('reorders to after the last sibling', async () => {
      const renderResult = await renderTestEditorWithCode(
        getProjectCode(),
        'await-first-dom-report',
      )

      const dragMeElement = await renderResult.renderedDOM.findByTestId(
        `navigator-item-regular_utopia_storyboard_uid/scene_aaa/sceneroot/dragme`,
      )
      const dragMeElementRect = dragMeElement.getBoundingClientRect()
      const dragMeElementCenter = getDomRectCenter(dragMeElementRect)
      const notDraggableDivElement = await renderResult.renderedDOM.findByTestId(
        `navigator-item-regular_utopia_storyboard_uid/scene_aaa/sceneroot/notdrag`,
      )
      const notDraggableDivElementRect = notDraggableDivElement.getBoundingClientRect()
      const notDraggableDivElementCenter = getDomRectCenter(notDraggableDivElementRect)
      const dragTo = {
        x: notDraggableDivElementCenter.x,
        y: notDraggableDivElementRect.y + notDraggableDivElementRect.height - 3,
      }

      const dragDelta = windowPoint({
        x: dragTo.x - dragMeElementCenter.x,
        y: dragTo.y - dragMeElementCenter.y,
      })

      const targetElement = EP.fromString('utopia-storyboard-uid/scene-aaa/sceneroot/dragme')
      await act(async () => {
        const dispatchDone = renderResult.getDispatchFollowUpActionsFinished()
        await renderResult.dispatch([selectComponents([targetElement], false)], false)
        await dispatchDone
      })

      await dragElementWithDNDEvents(
        renderResult,
        `navigator-item-drag-regular_utopia_storyboard_uid/scene_aaa/sceneroot/dragme`,
        `navigator-item-drop-after-regular_utopia_storyboard_uid/scene_aaa/sceneroot/notdrag`,
        windowPoint(dragMeElementCenter),
        dragDelta,

        async () => {
          expect(renderResult.getEditorState().editor.navigator.dropTargetHint?.type).toEqual(
            'after',
          )
          // parent highlight is shown
          const parentEntry = renderResult.renderedDOM.getByTestId(
            `navigator-item-regular_utopia_storyboard_uid/scene_aaa/sceneroot`,
          )
          expect((parentEntry.firstChild?.firstChild as HTMLElement).style.outline).toEqual(
            '1px solid var(--utopitheme-navigatorResizeHintBorder)',
          )

          // drop target line is shown
          const dropTarget = renderResult.renderedDOM.getByTestId(
            `navigator-item-drop-after-regular_utopia_storyboard_uid/scene_aaa/sceneroot/notdrag`,
          )
          expect(dropTarget.style.opacity).toEqual('1')
        },
      )

      await renderResult.getDispatchFollowUpActionsFinished()

      expect(
        renderResult.getEditorState().derived.navigatorTargets.map(navigatorEntryToKey),
      ).toEqual([
        'regular-utopia-storyboard-uid/scene-aaa',
        'regular-utopia-storyboard-uid/scene-aaa/sceneroot',
        'regular-utopia-storyboard-uid/scene-aaa/sceneroot/firstdiv',
        'regular-utopia-storyboard-uid/scene-aaa/sceneroot/seconddiv',
        'regular-utopia-storyboard-uid/scene-aaa/sceneroot/thirddiv',
        'regular-utopia-storyboard-uid/scene-aaa/sceneroot/notdrag',
        'regular-utopia-storyboard-uid/scene-aaa/sceneroot/dragme', // <- moved to after the last sibling `notdrag` under its parent
        'regular-utopia-storyboard-uid/scene-aaa/parentsibling',
      ])
    })

    it('reparents under the first sibling', async () => {
      const renderResult = await renderTestEditorWithCode(
        getProjectCode(),
        'await-first-dom-report',
      )

      const dragMeElement = await renderResult.renderedDOM.findByTestId(
        `navigator-item-regular_utopia_storyboard_uid/scene_aaa/sceneroot/dragme`,
      )
      const dragMeElementRect = dragMeElement.getBoundingClientRect()
      const dragMeElementCenter = getDomRectCenter(dragMeElementRect)
      const firstDivElement = await renderResult.renderedDOM.findByTestId(
        `navigator-item-regular_utopia_storyboard_uid/scene_aaa/sceneroot/firstdiv`,
      )
      const firstDivElementRect = firstDivElement.getBoundingClientRect()
      const firstDivElementCenter = getDomRectCenter(firstDivElementRect)
      const dragTo = {
        x: firstDivElementCenter.x,
        y: firstDivElementRect.y + 20,
      }

      const dragDelta = windowPoint({
        x: dragTo.x - dragMeElementCenter.x,
        y: dragTo.y - dragMeElementCenter.y,
      })

      const targetElement = EP.fromString('utopia-storyboard-uid/scene-aaa/sceneroot/dragme')
      await act(async () => {
        const dispatchDone = renderResult.getDispatchFollowUpActionsFinished()
        await renderResult.dispatch([selectComponents([targetElement], false)], false)
        await dispatchDone
      })

      await act(async () =>
        dragElementWithDNDEvents(
          renderResult,
          `navigator-item-drag-regular_utopia_storyboard_uid/scene_aaa/sceneroot/dragme`,
          `navigator-item-regular_utopia_storyboard_uid/scene_aaa/sceneroot/firstdiv`,
          windowPoint(dragMeElementCenter),
          dragDelta,

          async () => {
            expect(renderResult.getEditorState().editor.navigator.dropTargetHint?.type).toEqual(
              'reparent',
            )
            // parent highlight is shown
            const parentEntry = renderResult.renderedDOM.getByTestId(
              `navigator-item-regular_utopia_storyboard_uid/scene_aaa/sceneroot/firstdiv`,
            )
            expect((parentEntry.firstChild?.firstChild as HTMLElement).style.outline).toEqual(
              '1px solid var(--utopitheme-navigatorResizeHintBorder)',
            )

            // drop target lines are not shown
            const dropTargetBefore = renderResult.renderedDOM.getByTestId(
              `navigator-item-drop-before-regular_utopia_storyboard_uid/scene_aaa/sceneroot/firstdiv`,
            )
            expect(dropTargetBefore.style.opacity).toEqual('0')

            const dropTargetAfter = renderResult.renderedDOM.getByTestId(
              `navigator-item-drop-after-regular_utopia_storyboard_uid/scene_aaa/sceneroot/firstdiv`,
            )
            expect(dropTargetAfter.style.opacity).toEqual('0')
          },
        ),
      )

      await renderResult.getDispatchFollowUpActionsFinished()

      expect(
        renderResult.getEditorState().derived.navigatorTargets.map(navigatorEntryToKey),
      ).toEqual([
        'regular-utopia-storyboard-uid/scene-aaa',
        'regular-utopia-storyboard-uid/scene-aaa/sceneroot',
        'regular-utopia-storyboard-uid/scene-aaa/sceneroot/firstdiv',
        'regular-utopia-storyboard-uid/scene-aaa/sceneroot/firstdiv/dragme', // <- moved to under the first sibling
        'regular-utopia-storyboard-uid/scene-aaa/sceneroot/seconddiv',
        'regular-utopia-storyboard-uid/scene-aaa/sceneroot/thirddiv',
        'regular-utopia-storyboard-uid/scene-aaa/sceneroot/notdrag',
        'regular-utopia-storyboard-uid/scene-aaa/parentsibling',
      ])
    })

    it('cannot reparent under granparent from a non-last sibling', async () => {
      const renderResult = await renderTestEditorWithCode(
        getProjectCode(),
        'await-first-dom-report',
      )

      const originalNavigatorOrder = renderResult
        .getEditorState()
        .derived.navigatorTargets.map(navigatorEntryToKey)

      const dragMeElement = await renderResult.renderedDOM.findByTestId(
        `navigator-item-regular_utopia_storyboard_uid/scene_aaa/sceneroot/dragme`,
      )
      const dragMeElementRect = dragMeElement.getBoundingClientRect()
      const dragMeElementCenter = getDomRectCenter(dragMeElementRect)

      const targetElement = EP.fromString('utopia-storyboard-uid/scene-aaa/sceneroot/dragme')
      await act(async () => {
        const dispatchDone = renderResult.getDispatchFollowUpActionsFinished()
        await renderResult.dispatch([selectComponents([targetElement], false)], false)
        await dispatchDone
      })

      await dragElementWithDNDEvents(
        renderResult,
        `navigator-item-drag-regular_utopia_storyboard_uid/scene_aaa/sceneroot/dragme`,
        `navigator-item-drop-after-regular_utopia_storyboard_uid/scene_aaa/sceneroot/thirddiv`,
        windowPoint(dragMeElementCenter),
        windowPoint({ x: -25, y: -25 }),

        async () => {
          expect(renderResult.getEditorState().editor.navigator.dropTargetHint?.type).toEqual(
            'after',
          )

          // highlight is shown on grandparent
          const parentEntry = renderResult.renderedDOM.getByTestId(
            `navigator-item-regular_utopia_storyboard_uid/scene_aaa`,
          )
          expect((parentEntry.firstChild?.firstChild as HTMLElement).style.outline).toEqual(
            'transparent solid 1px',
          )

          // drop target line is shown in original location
          const dropTarget = renderResult.renderedDOM.getByTestId(
            `navigator-item-drop-after-regular_utopia_storyboard_uid/scene_aaa/sceneroot/thirddiv`,
          )
          expect(dropTarget.style.opacity).toEqual('1')
        },
      )

      await renderResult.getDispatchFollowUpActionsFinished()

      expect(
        renderResult.getEditorState().derived.navigatorTargets.map(navigatorEntryToKey),
      ).toEqual(originalNavigatorOrder)
    })

    it('reparents under grandparent from the last sibling', async () => {
      const renderResult = await renderTestEditorWithCode(
        getProjectCode(),
        'await-first-dom-report',
      )

      const dragMeElement = await renderResult.renderedDOM.findByTestId(
        `navigator-item-regular_utopia_storyboard_uid/scene_aaa/sceneroot/dragme`,
      )
      const dragMeElementRect = dragMeElement.getBoundingClientRect()
      const dragMeElementCenter = getDomRectCenter(dragMeElementRect)

      const targetElement = EP.fromString('utopia-storyboard-uid/scene-aaa/sceneroot/dragme')
      await act(async () => {
        const dispatchDone = renderResult.getDispatchFollowUpActionsFinished()
        await renderResult.dispatch([selectComponents([targetElement], false)], false)
        await dispatchDone
      })
      await act(async () =>
        dragElementWithDNDEvents(
          renderResult,
          `navigator-item-drag-regular_utopia_storyboard_uid/scene_aaa/sceneroot/dragme`,
          `navigator-item-drop-after-regular_utopia_storyboard_uid/scene_aaa/sceneroot/notdrag`,
          windowPoint(dragMeElementCenter),
          windowPoint({ x: -35, y: -35 }),

          async () => {
            expect(renderResult.getEditorState().editor.navigator.dropTargetHint?.type).toEqual(
              'after',
            )

            // highlight is shown on grandparent
            const parentEntry = renderResult.renderedDOM.getByTestId(
              `navigator-item-regular_utopia_storyboard_uid/scene_aaa`,
            )
            expect((parentEntry.firstChild?.firstChild as HTMLElement).style.outline).toEqual(
              '1px solid var(--utopitheme-navigatorResizeHintBorder)',
            )

            // drop target line is shown in original location
            const dropTarget = renderResult.renderedDOM.getByTestId(
              `navigator-item-drop-after-regular_utopia_storyboard_uid/scene_aaa/sceneroot/notdrag`,
            )
            expect(dropTarget.style.opacity).toEqual('1')
          },
        ),
      )

      await renderResult.getDispatchFollowUpActionsFinished()

      expect(
        renderResult.getEditorState().derived.navigatorTargets.map(navigatorEntryToKey),
      ).toEqual([
        'regular-utopia-storyboard-uid/scene-aaa',
        'regular-utopia-storyboard-uid/scene-aaa/sceneroot',
        'regular-utopia-storyboard-uid/scene-aaa/sceneroot/firstdiv',
        'regular-utopia-storyboard-uid/scene-aaa/sceneroot/seconddiv',
        'regular-utopia-storyboard-uid/scene-aaa/sceneroot/thirddiv',
        'regular-utopia-storyboard-uid/scene-aaa/sceneroot/notdrag',
        'regular-utopia-storyboard-uid/scene-aaa/dragme',
        'regular-utopia-storyboard-uid/scene-aaa/parentsibling',
      ])
    })

    it('reparents under cousin element', async () => {
      const renderResult = await renderTestEditorWithCode(
        getProjectCode(),
        'await-first-dom-report',
      )

      const dragMeElement = await renderResult.renderedDOM.findByTestId(
        `navigator-item-regular_utopia_storyboard_uid/scene_aaa/sceneroot/dragme`,
      )
      const dragMeElementRect = dragMeElement.getBoundingClientRect()
      const dragMeElementCenter = getDomRectCenter(dragMeElementRect)
      const cousinDivElement = await renderResult.renderedDOM.findByTestId(
        `navigator-item-regular_utopia_storyboard_uid/scene_aaa/parentsibling`,
      )
      const cousinDivElementRect = cousinDivElement.getBoundingClientRect()
      const cousinDivElementCenter = getDomRectCenter(cousinDivElementRect)
      const dragTo = {
        x: cousinDivElementCenter.x,
        y: cousinDivElementRect.y + 14,
      }

      const dragDelta = windowPoint({
        x: dragTo.x - dragMeElementCenter.x,
        y: dragTo.y - dragMeElementCenter.y,
      })

      const targetElement = EP.fromString('utopia-storyboard-uid/scene-aaa/sceneroot/dragme')
      await act(async () => {
        const dispatchDone = renderResult.getDispatchFollowUpActionsFinished()
        await renderResult.dispatch([selectComponents([targetElement], false)], false)
        await dispatchDone
      })

      await act(async () =>
        dragElementWithDNDEvents(
          renderResult,
          `navigator-item-drag-regular_utopia_storyboard_uid/scene_aaa/sceneroot/dragme`,
          `navigator-item-regular_utopia_storyboard_uid/scene_aaa/parentsibling`,
          windowPoint(dragMeElementCenter),
          dragDelta,

          async () => {
            expect(renderResult.getEditorState().editor.navigator.dropTargetHint?.type).toEqual(
              'reparent',
            )
            // parent highlight is shown
            const parentEntry = renderResult.renderedDOM.getByTestId(
              `navigator-item-regular_utopia_storyboard_uid/scene_aaa/parentsibling`,
            )
            expect((parentEntry.firstChild?.firstChild as HTMLElement).style.outline).toEqual(
              '1px solid var(--utopitheme-navigatorResizeHintBorder)',
            )

            const dropTargetAfter = renderResult.renderedDOM.getByTestId(
              `navigator-item-drop-after-regular_utopia_storyboard_uid/scene_aaa/parentsibling`,
            )
            expect(dropTargetAfter.style.opacity).toEqual('0')
          },
        ),
      )

      await renderResult.getDispatchFollowUpActionsFinished()

      expect(
        renderResult.getEditorState().derived.navigatorTargets.map(navigatorEntryToKey),
      ).toEqual([
        'regular-utopia-storyboard-uid/scene-aaa',
        'regular-utopia-storyboard-uid/scene-aaa/sceneroot',
        'regular-utopia-storyboard-uid/scene-aaa/sceneroot/firstdiv',
        'regular-utopia-storyboard-uid/scene-aaa/sceneroot/seconddiv',
        'regular-utopia-storyboard-uid/scene-aaa/sceneroot/thirddiv',
        'regular-utopia-storyboard-uid/scene-aaa/sceneroot/notdrag',
        'regular-utopia-storyboard-uid/scene-aaa/parentsibling',
        'regular-utopia-storyboard-uid/scene-aaa/parentsibling/dragme', // <- moved to under the cousin element
      ])
    })

    it('reparents to storyboard from the last sibling', async () => {
      const renderResult = await renderTestEditorWithCode(
        getProjectCode(),
        'await-first-dom-report',
      )

      const dragMeElement = await renderResult.renderedDOM.findByTestId(
        `navigator-item-regular_utopia_storyboard_uid/scene_aaa/sceneroot/dragme`,
      )
      const dragMeElementRect = dragMeElement.getBoundingClientRect()
      const dragMeElementCenter = getDomRectCenter(dragMeElementRect)

      const targetElement = EP.fromString('utopia-storyboard-uid/scene-aaa/sceneroot/dragme')
      await act(async () => {
        const dispatchDone = renderResult.getDispatchFollowUpActionsFinished()
        await renderResult.dispatch([selectComponents([targetElement], false)], false)
        await dispatchDone
      })
      await act(async () =>
        dragElementWithDNDEvents(
          renderResult,
          `navigator-item-drag-regular_utopia_storyboard_uid/scene_aaa/sceneroot/dragme`,
          `navigator-item-drop-after-regular_utopia_storyboard_uid/scene_aaa/parentsibling`,
          windowPoint(dragMeElementCenter),
          windowPoint({ x: -92, y: -35 }),

          async () => {
            expect(renderResult.getEditorState().editor.navigator.dropTargetHint?.type).toEqual(
              'after',
            )

            // drop target line is shown in original location
            const dropTarget = renderResult.renderedDOM.getByTestId(
              `navigator-item-drop-after-regular_utopia_storyboard_uid/scene_aaa/parentsibling`,
            )
            expect(dropTarget.style.opacity).toEqual('1')
          },
        ),
      )

      await renderResult.getDispatchFollowUpActionsFinished()

      expect(
        renderResult.getEditorState().derived.navigatorTargets.map(navigatorEntryToKey),
      ).toEqual([
        'regular-utopia-storyboard-uid/scene-aaa',
        'regular-utopia-storyboard-uid/scene-aaa/sceneroot',
        'regular-utopia-storyboard-uid/scene-aaa/sceneroot/firstdiv',
        'regular-utopia-storyboard-uid/scene-aaa/sceneroot/seconddiv',
        'regular-utopia-storyboard-uid/scene-aaa/sceneroot/thirddiv',
        'regular-utopia-storyboard-uid/scene-aaa/sceneroot/notdrag',
        'regular-utopia-storyboard-uid/scene-aaa/parentsibling',
        'regular-utopia-storyboard-uid/dragme',
      ])

      expect(renderResult.getEditorState().editor.selectedViews).toEqual([
        EP.fromString('utopia-storyboard-uid/dragme'),
      ])
      expect(
        renderResult.getEditorState().editor.jsxMetadata['utopia-storyboard-uid/dragme']
          ?.globalFrame,
      ).toEqual({ height: 65, width: 66, x: 265, y: 233 })
      expect(renderResult.getEditorState().editor.navigator.dropTargetHint).toEqual(null)
    })

    it('reparents the last element to storyboard', async () => {
      const renderResult = await renderTestEditorWithCode(
        getProjectCode(),
        'await-first-dom-report',
      )

      const dragMeElement = await renderResult.renderedDOM.findByTestId(
        `navigator-item-regular_utopia_storyboard_uid/scene_aaa/parentsibling`,
      )
      const dragMeElementRect = dragMeElement.getBoundingClientRect()
      const dragMeElementCenter = getDomRectCenter(dragMeElementRect)

      const targetElement = EP.fromString('utopia-storyboard-uid/scene-aaa/parentsibling')
      await act(async () => {
        const dispatchDone = renderResult.getDispatchFollowUpActionsFinished()
        await renderResult.dispatch([selectComponents([targetElement], false)], false)
        await dispatchDone
      })
      await act(async () =>
        dragElementWithDNDEvents(
          renderResult,
          `navigator-item-drag-regular_utopia_storyboard_uid/scene_aaa/parentsibling`,
          `navigator-item-drop-after-regular_utopia_storyboard_uid/scene_aaa/parentsibling`,
          windowPoint(dragMeElementCenter),
          windowPoint({ x: -32, y: 0 }),

          async () => {
            expect(renderResult.getEditorState().editor.navigator.dropTargetHint?.type).toEqual(
              'after',
            )

            // drop target line is shown in original location
            const dropTarget = renderResult.renderedDOM.getByTestId(
              `navigator-item-drop-after-regular_utopia_storyboard_uid/scene_aaa/parentsibling`,
            )
            expect(dropTarget.style.opacity).toEqual('1')
          },
        ),
      )

      await renderResult.getDispatchFollowUpActionsFinished()

      expect(
        renderResult.getEditorState().derived.navigatorTargets.map(navigatorEntryToKey),
      ).toEqual([
        'regular-utopia-storyboard-uid/scene-aaa',
        'regular-utopia-storyboard-uid/scene-aaa/sceneroot',
        'regular-utopia-storyboard-uid/scene-aaa/sceneroot/firstdiv',
        'regular-utopia-storyboard-uid/scene-aaa/sceneroot/seconddiv',
        'regular-utopia-storyboard-uid/scene-aaa/sceneroot/thirddiv',
        'regular-utopia-storyboard-uid/scene-aaa/sceneroot/dragme',
        'regular-utopia-storyboard-uid/scene-aaa/sceneroot/notdrag',
        'regular-utopia-storyboard-uid/parentsibling',
      ])

      expect(renderResult.getEditorState().editor.selectedViews).toEqual([
        EP.fromString('utopia-storyboard-uid/parentsibling'),
      ])
      expect(
        renderResult.getEditorState().editor.jsxMetadata['utopia-storyboard-uid/parentsibling']
          ?.globalFrame,
      ).toEqual({ height: 200, width: 400, x: 0, y: 500 })
      expect(renderResult.getEditorState().editor.navigator.dropTargetHint).toEqual(null)
    })

    it('does not reparent to invalid target', async () => {
      const renderResult = await renderTestEditorWithCode(
        getProjectCode(),
        'await-first-dom-report',
      )

      const dragMeElement = await renderResult.renderedDOM.findByTestId(
        `navigator-item-regular_utopia_storyboard_uid/scene_aaa/sceneroot/dragme`,
      )
      const dragMeElementRect = dragMeElement.getBoundingClientRect()
      const dragMeElementCenter = getDomRectCenter(dragMeElementRect)
      const notDraggableDivElement = await renderResult.renderedDOM.findByTestId(
        `navigator-item-regular_utopia_storyboard_uid/scene_aaa/sceneroot/notdrag`,
      )
      const notDraggableDivElementRect = notDraggableDivElement.getBoundingClientRect()
      const notDraggableDivElementCenter = getDomRectCenter(notDraggableDivElementRect)
      const dragTo = {
        x: notDraggableDivElementCenter.x,
        y: notDraggableDivElementRect.y,
      }

      const dragDelta = windowPoint({
        x: dragTo.x - dragMeElementCenter.x,
        y: dragTo.y - dragMeElementCenter.y,
      })

      const targetElement = EP.fromString(
        'regular-utopia-storyboard-uid/scene-aaa/sceneroot/dragme',
      )
      await act(async () => {
        const dispatchDone = renderResult.getDispatchFollowUpActionsFinished()
        await renderResult.dispatch([selectComponents([targetElement], false)], false)
        await dispatchDone
      })

      await act(async () =>
        dragElementWithDNDEvents(
          renderResult,
          `navigator-item-drag-regular_utopia_storyboard_uid/scene_aaa/sceneroot/dragme`,
          `navigator-item-regular_utopia_storyboard_uid/scene_aaa/sceneroot/notdrag`,
          windowPoint(dragMeElementCenter),
          dragDelta,
        ),
      )

      await renderResult.getDispatchFollowUpActionsFinished()

      expect(
        renderResult.getEditorState().derived.navigatorTargets.map(navigatorEntryToKey),
      ).toEqual([
        'regular-utopia-storyboard-uid/scene-aaa',
        'regular-utopia-storyboard-uid/scene-aaa/sceneroot',
        'regular-utopia-storyboard-uid/scene-aaa/sceneroot/firstdiv',
        'regular-utopia-storyboard-uid/scene-aaa/sceneroot/seconddiv',
        'regular-utopia-storyboard-uid/scene-aaa/sceneroot/thirddiv',
        'regular-utopia-storyboard-uid/scene-aaa/sceneroot/dragme', // <- cannot be moved
        'regular-utopia-storyboard-uid/scene-aaa/sceneroot/notdrag',
        'regular-utopia-storyboard-uid/scene-aaa/parentsibling',
      ])
    })

    it('can reparent top-level element', async () => {
      const renderResult = await renderTestEditorWithCode(
        projectWithHierarchy,
        'await-first-dom-report',
      )

      const parent1DndContainer = await renderResult.renderedDOM.findByTestId(
        `navigator-item-regular_sb/parent1`,
      )
      const parent1DndContainerRect = parent1DndContainer.getBoundingClientRect()
      const parent1DndContainerCenter = getDomRectCenter(parent1DndContainerRect)

      const child1DndContainer = await renderResult.renderedDOM.findByTestId(
        `navigator-item-regular_sb/parent2`,
      )
      const child1DndContainerRect = child1DndContainer.getBoundingClientRect()
      const child1DndContainerCenter = getDomRectCenter(child1DndContainerRect)
      const dragTo = {
        x: child1DndContainerCenter.x,
        y: child1DndContainerRect.y + 14,
      }

      const dragDelta = windowPoint({
        x: dragTo.x - parent1DndContainerCenter.x,
        y: dragTo.y - parent1DndContainerCenter.y,
      })

      await selectComponentsForTest(renderResult, [EP.fromString('sb/parent1')])

      await act(async () =>
        dragElementWithDNDEvents(
          renderResult,
          `navigator-item-drag-regular_sb/parent1`,
          `navigator-item-regular_sb/parent2`,
          windowPoint(parent1DndContainerCenter),
          dragDelta,
        ),
      )

      expect(
        renderResult.getEditorState().derived.navigatorTargets.map(navigatorEntryToKey),
      ).toEqual([
        'regular-sb/parent2',
        'regular-sb/parent2/aaa',
        'regular-sb/parent2/aab',
        'regular-sb/parent2/parent1', // <- parent1 and its children moved under parent2
        'regular-sb/parent2/parent1/child1', // <- parent1 and its children moved under parent2
        'regular-sb/parent2/parent1/755', // <- parent1 and its children moved under parent2
        'regular-sb/text',
      ])
      expect(renderResult.getEditorState().editor.selectedViews).toEqual([
        EP.fromString('sb/parent2/parent1'),
      ])
    })

    it('cannot reparent parent element inside itself', async () => {
      const renderResult = await renderTestEditorWithCode(
        projectWithHierarchy,
        'await-first-dom-report',
      )

      const parent1DndContainer = await renderResult.renderedDOM.findByTestId(
        `navigator-item-regular_sb/parent1`,
      )
      const parent1DndContainerRect = parent1DndContainer.getBoundingClientRect()
      const parent1DndContainerCenter = getDomRectCenter(parent1DndContainerRect)

      const child1DndContainer = await renderResult.renderedDOM.findByTestId(
        `navigator-item-regular_sb/parent1/child1`,
      )
      const child1DndContainerRect = child1DndContainer.getBoundingClientRect()
      const child1DndContainerCenter = getDomRectCenter(child1DndContainerRect)
      const dragTo = {
        x: child1DndContainerCenter.x,
        y: child1DndContainerRect.y + 14,
      }

      const dragDelta = windowPoint({
        x: dragTo.x - parent1DndContainerCenter.x,
        y: dragTo.y - parent1DndContainerCenter.y,
      })

      await selectComponentsForTest(renderResult, [EP.fromString('sb/parent1')])

      await act(async () =>
        dragElementWithDNDEvents(
          renderResult,
          `navigator-item-drag-regular_sb/parent1`,
          `navigator-item-regular_sb/parent1/child1`,
          windowPoint(parent1DndContainerCenter),
          dragDelta,
        ),
      )

      expect(
        renderResult.getEditorState().derived.navigatorTargets.map(navigatorEntryToKey),
      ).toEqual([
        'regular-sb/parent1', // <- cannot be reparented under its own child
        'regular-sb/parent1/child1',
        'regular-sb/parent1/755',
        'regular-sb/parent2',
        'regular-sb/parent2/aaa',
        'regular-sb/parent2/aab',
        'regular-sb/text',
      ])
    })

    it('cannot reparent inside an element that does not support children', async () => {
      const renderResult = await renderTestEditorWithCode(
        projectWithHierarchy,
        'await-first-dom-report',
      )

      const parent1DndContainer = await renderResult.renderedDOM.findByTestId(
        `navigator-item-regular_sb/parent1`,
      )
      const parent1DndContainerRect = parent1DndContainer.getBoundingClientRect()
      const parent1DndContainerCenter = getDomRectCenter(parent1DndContainerRect)

      const textDndContainer = await renderResult.renderedDOM.findByTestId(
        `navigator-item-regular_sb/text`,
      )
      const textDndContainerRect = textDndContainer.getBoundingClientRect()
      const textDndContainerCenter = getDomRectCenter(textDndContainerRect)
      const dragTo = {
        x: textDndContainerCenter.x,
        y: textDndContainerRect.y + 14,
      }

      const dragDelta = windowPoint({
        x: dragTo.x - parent1DndContainerCenter.x,
        y: dragTo.y - parent1DndContainerCenter.y,
      })

      await selectComponentsForTest(renderResult, [EP.fromString('sb/parent1')])

      await act(async () =>
        dragElementWithDNDEvents(
          renderResult,
          `navigator-item-drag-regular_sb/parent1`,
          `navigator-item-regular_sb/text`,
          windowPoint(parent1DndContainerCenter),
          dragDelta,
        ),
      )

      expect(
        renderResult.getEditorState().derived.navigatorTargets.map(navigatorEntryToKey),
      ).toEqual([
        'regular-sb/parent1', // <- cannot be reparented under `text`
        'regular-sb/parent1/child1',
        'regular-sb/parent1/755',
        'regular-sb/parent2',
        'regular-sb/parent2/aaa',
        'regular-sb/parent2/aab',
        'regular-sb/text',
      ])
    })

    it('cannot reparent into component that does not support children', async () => {
      const editor = await renderTestEditorWithCode(
        makeTestProjectCodeWithSnippet(`
        <div data-uid='root'>
          <div data-uid='container'>
            <div data-uid='dragme' />
            </div>
          </div>
            `),
        'await-first-dom-report',
      )

      const initialEditor = getPrintedUiJsCode(editor.getEditorState())

      const dragMeElementPath = EP.fromString(
        `${BakedInStoryboardUID}/${TestSceneUID}/${TestAppUID}:root/container/dragme`,
      )

      const targetElementPath = EP.fromString(
        `${BakedInStoryboardUID}/${TestSceneUID}/${TestAppUID}`,
      )

      await doBasicDrag(editor, dragMeElementPath, targetElementPath, ReparentDropTargetTestId)

      expect(getPrintedUiJsCode(editor.getEditorState())).toEqual(initialEditor)
    })

    it('cannot reparent before the root element of component', async () => {
      const editor = await renderTestEditorWithCode(
        makeTestProjectCodeWithSnippet(`
        <div data-uid='root'>
          <div data-uid='container'>
            <div data-uid='dragme' />
            </div>
          </div>
            `),
        'await-first-dom-report',
      )

      const initialEditor = getPrintedUiJsCode(editor.getEditorState())

      const dragMeElementPath = EP.fromString(
        `${BakedInStoryboardUID}/${TestSceneUID}/${TestAppUID}:root/container/dragme`,
      )

      const targetElementPath = EP.fromString(
        `${BakedInStoryboardUID}/${TestSceneUID}/${TestAppUID}:root`,
      )

      await doBasicDrag(editor, dragMeElementPath, targetElementPath, TopDropTargetLineTestId)

      expect(getPrintedUiJsCode(editor.getEditorState())).toEqual(initialEditor)
    })

    it('cannot reparent after the root element of component', async () => {
      const editor = await renderTestEditorWithCode(
        makeTestProjectCodeWithSnippet(`
        <div data-uid='root'>
          <div data-uid='container'>
            <div data-uid='dragme' />
            </div>
          </div>
            `),
        'await-first-dom-report',
      )

      const initialEditor = getPrintedUiJsCode(editor.getEditorState())

      const dragMeElementPath = EP.fromString(
        `${BakedInStoryboardUID}/${TestSceneUID}/${TestAppUID}:root/container/dragme`,
      )

      const targetElementPath = EP.fromString(
        `${BakedInStoryboardUID}/${TestSceneUID}/${TestAppUID}:root`,
      )

      await doBasicDrag(editor, dragMeElementPath, targetElementPath, BottomDropTargetLineTestId)

      expect(getPrintedUiJsCode(editor.getEditorState())).toEqual(initialEditor)
    })

    it('reparenting an element to the storyboard between 2 scenes', async () => {
      const renderResult = await renderTestEditorWithCode(
        projectWithHierarchy,
        'await-first-dom-report',
      )

      const childElement = await renderResult.renderedDOM.findByTestId(
        `navigator-item-regular_sb/parent1/child1`,
      )
      const childElementRect = childElement.getBoundingClientRect()
      const childElementRectCenter = getDomRectCenter(childElementRect)

      const lastChildElement = await renderResult.renderedDOM.findByTestId(
        `navigator-item-regular_sb/parent1/755`,
      )
      const lastChildElementRect = lastChildElement.getBoundingClientRect()
      const dragTo = {
        x: lastChildElementRect.x,
        y: lastChildElementRect.y + lastChildElementRect.height,
      }

      const dragDelta = windowPoint({
        x: dragTo.x - childElementRectCenter.x,
        y: dragTo.y - childElementRectCenter.y,
      })

      await selectComponentsForTest(renderResult, [EP.fromString('sb/parent1/child1')])

      await act(async () =>
        dragElementWithDNDEvents(
          renderResult,
          DragItemTestId('regular_sb/parent1/child1'),
          BottomDropTargetLineTestId('regular_sb/parent1/755'),
          windowPoint(childElementRectCenter),
          dragDelta,
        ),
      )

      expect(
        renderResult.getEditorState().derived.navigatorTargets.map(navigatorEntryToKey),
      ).toEqual([
        'regular-sb/parent1',
        'regular-sb/parent1/755',
        'regular-sb/child1',
        'regular-sb/parent2',
        'regular-sb/parent2/aaa',
        'regular-sb/parent2/aab',
        'regular-sb/text',
      ])
      expect(renderResult.getEditorState().editor.selectedViews).toEqual([
        EP.fromString('sb/child1'),
      ])
    })

    it('can reparent into collapsed element', async () => {
      const renderResult = await renderTestEditorWithCode(
        getProjectCode(),
        'await-first-dom-report',
      )

      await renderResult.dispatch(
        [toggleCollapse(EP.fromString(`${BakedInStoryboardUID}/${TestSceneUID}/${SceneRootId}`))],
        true,
      )

      await doBasicDrag(
        renderResult,
        EP.fromString(`${BakedInStoryboardUID}/${TestSceneUID}/parentsibling`),
        EP.fromString(`${BakedInStoryboardUID}/${TestSceneUID}/${SceneRootId}`),
        ReparentDropTargetTestId,
      )

      expect(
        renderResult.getEditorState().derived.navigatorTargets.map(navigatorEntryToKey),
      ).toEqual([
        'regular-utopia-storyboard-uid/scene-aaa',
        'regular-utopia-storyboard-uid/scene-aaa/sceneroot',
        'regular-utopia-storyboard-uid/scene-aaa/sceneroot/firstdiv',
        'regular-utopia-storyboard-uid/scene-aaa/sceneroot/seconddiv',
        'regular-utopia-storyboard-uid/scene-aaa/sceneroot/thirddiv',
        'regular-utopia-storyboard-uid/scene-aaa/sceneroot/dragme',
        'regular-utopia-storyboard-uid/scene-aaa/sceneroot/notdrag',
        'regular-utopia-storyboard-uid/scene-aaa/sceneroot/parentsibling', // <- moved under sceneroot
      ])
    })

    describe('reordering into the same place', () => {
      it('before itself', async () => {
        const renderResult = await renderTestEditorWithCode(
          getProjectCode(),
          'await-first-dom-report',
        )

        const initialOrder = renderResult
          .getEditorState()
          .derived.navigatorTargets.map(navigatorEntryToKey)

        const target = EP.fromString(
          `${BakedInStoryboardUID}/${TestSceneUID}/${SceneRootId}/firstdiv`,
        )

        await selectComponentsForTest(renderResult, [target])

        // check if all selected elements are actually in the metadata
        expect(
          renderResult
            .getEditorState()
            .editor.selectedViews.map((path) =>
              MetadataUtils.findElementByElementPath(
                renderResult.getEditorState().editor.jsxMetadata,
                path,
              ),
            )
            .every((i) => i != null),
        ).toEqual(true)

        await expectNoAction(renderResult, async () => {
          await doBasicDrag(renderResult, target, target, TopDropTargetLineTestId)
        })

        expect(
          renderResult.getEditorState().derived.navigatorTargets.map(navigatorEntryToKey),
        ).toEqual(initialOrder)
      })
    })

    it('after itself', async () => {
      const renderResult = await renderTestEditorWithCode(
        getProjectCode(),
        'await-first-dom-report',
      )

      const initialOrder = renderResult
        .getEditorState()
        .derived.navigatorTargets.map(navigatorEntryToKey)

      const target = EP.fromString(
        `${BakedInStoryboardUID}/${TestSceneUID}/${SceneRootId}/firstdiv`,
      )

      await selectComponentsForTest(renderResult, [target])

      // check if all selected elements are actually in the metadata
      expect(
        renderResult
          .getEditorState()
          .editor.selectedViews.map((path) =>
            MetadataUtils.findElementByElementPath(
              renderResult.getEditorState().editor.jsxMetadata,
              path,
            ),
          )
          .every((i) => i != null),
      ).toEqual(true)

      await expectNoAction(renderResult, async () => {
        await doBasicDrag(renderResult, target, target, BottomDropTargetLineTestId)
      })

      expect(
        renderResult.getEditorState().derived.navigatorTargets.map(navigatorEntryToKey),
      ).toEqual(initialOrder)
    })

    it('after the previous entry', async () => {
      const renderResult = await renderTestEditorWithCode(
        getProjectCode(),
        'await-first-dom-report',
      )

      const initialOrder = renderResult
        .getEditorState()
        .derived.navigatorTargets.map(navigatorEntryToKey)

      const target = EP.fromString(
        `${BakedInStoryboardUID}/${TestSceneUID}/${SceneRootId}/seconddiv`,
      )

      await selectComponentsForTest(renderResult, [target])

      // check if all selected elements are actually in the metadata
      expect(
        renderResult
          .getEditorState()
          .editor.selectedViews.map((path) =>
            MetadataUtils.findElementByElementPath(
              renderResult.getEditorState().editor.jsxMetadata,
              path,
            ),
          )
          .every((i) => i != null),
      ).toEqual(true)

      await expectNoAction(renderResult, async () => {
        await doBasicDrag(
          renderResult,
          target,
          EP.fromString(`${BakedInStoryboardUID}/${TestSceneUID}/${SceneRootId}/firstdiv`),
          BottomDropTargetLineTestId,
        )
      })

      expect(
        renderResult.getEditorState().derived.navigatorTargets.map(navigatorEntryToKey),
      ).toEqual(initialOrder)
    })

    describe('reparenting component instances', () => {
      it('cannot reparent component instance into its own definition via moving it next to a child', async () => {
        const editor = await renderTestEditorWithCode(
          `import * as React from 'react'
      import { Storyboard, Scene } from 'utopia-api'
      
      const App = (props) => (
        <div style={{ ...props.style }} data-uid='app-root'>
          <ThisComponent data-uid='component-1' />
          <ThisComponent data-uid='component-2' />
        </div>
      )
      
      const ThisComponent = (props) => (
        <div style={{ ...props.style }} data-uid='custom-root'>
          <div data-uid='hello-1' data-testid='target'>
            Hello there 1!
          </div>
          <div data-uid='aap'>Hello there 2!</div>
          <div data-uid='aat'>Hello there 3!</div>
        </div>
      )
      
      export var storyboard = (
        <Storyboard data-uid='sb'>
          <Scene
            data-uid='scene'
            style={{ width: 432, height: 356, left: 98, top: 36 }}
          >
            <App
              style={{
                backgroundColor: '#aaaaaa33',
                position: 'absolute',
                left: 34,
                top: 31,
                width: 346,
                height: 223,
              }}
              data-uid='app'
            />
          </Scene>
        </Storyboard>
      )
      `,
          'await-first-dom-report',
        )

        await mouseDoubleClickAtPoint(editor.renderedDOM.getAllByText('ThisComponent')[0], {
          x: 2,
          y: 2,
        })

        const startingVisibleNavigatorEntries = editor
          .getEditorState()
          .derived.visibleNavigatorTargets.map(navigatorEntryToKey)

        expect(startingVisibleNavigatorEntries).toEqual([
          'regular-sb/scene',
          'regular-sb/scene/app',
          'regular-sb/scene/app:app-root',
          'regular-sb/scene/app:app-root/component-1',
          'regular-sb/scene/app:app-root/component-1:custom-root',
          'regular-sb/scene/app:app-root/component-1:custom-root/hello-1',
          'regular-sb/scene/app:app-root/component-1:custom-root/aap',
          'regular-sb/scene/app:app-root/component-1:custom-root/aat',
          'regular-sb/scene/app:app-root/component-2',
        ])

        await doBasicDrag(
          editor,
          EP.fromString('sb/scene/app:app-root/component-2'),
          EP.fromString('sb/scene/app:app-root/component-1:custom-root/hello-1'),
        )
        await editor.getDispatchFollowUpActionsFinished()

        expect(
          editor.getEditorState().derived.visibleNavigatorTargets.map(navigatorEntryToKey),
        ).toEqual(startingVisibleNavigatorEntries)
      })

      it('cannot reparent component instance into its own definition, transitively', async () => {
        const editor = await renderTestEditorWithCode(
          `import * as React from 'react'
      import { Storyboard, Scene } from 'utopia-api'
      
      const App = (props) => (
        <div style={{ ...props.style }} data-uid='app-root'>
          <ThisComponent data-uid='component-1' />
          <ThisComponent data-uid='component-2' />
        </div>
      )
      
      const ThisComponent = (props) => (
        <div style={{ ...props.style }} data-uid='custom-root'>
          <div data-uid='hello-1' data-testid='target'>
            Hello there 1!
          </div>
          <div data-uid='aap'>Hello there 2!</div>
          <div data-uid='aat'>Hello there 3!</div>
        </div>
      )
      
      export var storyboard = (
        <Storyboard data-uid='sb'>
          <Scene
            data-uid='scene-1'
            style={{ width: 300, height: 300, left: 10, top: 10 }}
          >
            <App
              style={{
                backgroundColor: '#aaaaaa33',
                position: 'absolute',
                left: 10,
                top: 10,
                width: 280,
                height: 280,
              }}
              data-uid='app'
            />
          </Scene>
          <Scene
            data-uid='scene-2'
            style={{ width: 300, height: 300, left: 320, top: 10 }}
          >
            <App
              style={{
                backgroundColor: '#aaaaaa33',
                position: 'absolute',
                left: 10,
                top: 10,
                width: 280,
                height: 280,
              }}
              data-uid='app-2'
            />
          </Scene>
        </Storyboard>
      )
      `,
          'await-first-dom-report',
        )

        await mouseDoubleClickAtPoint(editor.renderedDOM.getAllByText('ThisComponent')[0], {
          x: 2,
          y: 2,
        })

        const startingVisibleNavigatorEntries = editor
          .getEditorState()
          .derived.visibleNavigatorTargets.map(navigatorEntryToKey)

        expect(startingVisibleNavigatorEntries).toEqual([
          'regular-sb/scene-1',
          'regular-sb/scene-1/app',
          'regular-sb/scene-1/app:app-root',
          'regular-sb/scene-1/app:app-root/component-1',
          'regular-sb/scene-1/app:app-root/component-1:custom-root',
          'regular-sb/scene-1/app:app-root/component-1:custom-root/hello-1',
          'regular-sb/scene-1/app:app-root/component-1:custom-root/aap',
          'regular-sb/scene-1/app:app-root/component-1:custom-root/aat',
          'regular-sb/scene-1/app:app-root/component-2',
          'regular-sb/scene-2',
          'regular-sb/scene-2/app-2',
          'regular-sb/scene-2/app-2:app-root',
          'regular-sb/scene-2/app-2:app-root/component-1',
          'regular-sb/scene-2/app-2:app-root/component-2',
        ])

        await doBasicDrag(
          editor,
          EP.fromString('sb/scene-2/app-2'),
          EP.fromString('sb/scene-1/app:app-root/component-1:custom-root/hello-1'),
        )
        await editor.getDispatchFollowUpActionsFinished()

        expect(
          editor.getEditorState().derived.visibleNavigatorTargets.map(navigatorEntryToKey),
        ).toEqual(startingVisibleNavigatorEntries)
      })
    })
  })

  describe('reordering a map expression', () => {
    it('reorders a map expression to before the first sibling', async () => {
      const renderResult = await renderTestEditorWithCode(
        getProjectCodeWithMapExpression(),
        'await-first-dom-report',
      )
      const dragMeElement = await renderResult.renderedDOM.findByTestId(
        `navigator-item-regular_utopia_storyboard_uid/scene_aaa/sceneroot/e46`,
      )
      const dragMeElementRect = dragMeElement.getBoundingClientRect()
      const dragMeElementCenter = getDomRectCenter(dragMeElementRect)
      const firstDivElement = await renderResult.renderedDOM.findByTestId(
        `navigator-item-regular_utopia_storyboard_uid/scene_aaa/sceneroot/firstdiv`,
      )
      const firstDivElementRect = firstDivElement.getBoundingClientRect()
      const firstDivElementCenter = getDomRectCenter(firstDivElementRect)
      const dragTo = {
        x: firstDivElementCenter.x,
        y: firstDivElementRect.y + 3,
      }

      const dragDelta = windowPoint({
        x: dragTo.x - dragMeElementCenter.x,
        y: dragTo.y - dragMeElementCenter.y,
      })

      const targetElement = EP.fromString('utopia-storyboard-uid/scene-aaa/sceneroot/e46')
      await act(async () => {
        const dispatchDone = renderResult.getDispatchFollowUpActionsFinished()
        await renderResult.dispatch([selectComponents([targetElement], false)], false)
        await dispatchDone
      })

      await dragElementWithDNDEvents(
        renderResult,
        `navigator-item-drag-regular_utopia_storyboard_uid/scene_aaa/sceneroot/e46`,
        `navigator-item-drop-before-regular_utopia_storyboard_uid/scene_aaa/sceneroot/firstdiv`,
        windowPoint(dragMeElementCenter),
        dragDelta,

        async () => {
          expect(renderResult.getEditorState().editor.navigator.dropTargetHint?.type).toEqual(
            'before',
          )
          // parent highlight is shown
          const parentEntry = renderResult.renderedDOM.getByTestId(
            `navigator-item-regular_utopia_storyboard_uid/scene_aaa/sceneroot`,
          )
          expect((parentEntry.firstChild?.firstChild as HTMLElement).style.outline).toEqual(
            '1px solid var(--utopitheme-navigatorResizeHintBorder)',
          )

          // drop target line is shown
          const dropTarget = renderResult.renderedDOM.getByTestId(
            `navigator-item-drop-before-regular_utopia_storyboard_uid/scene_aaa/sceneroot/firstdiv`,
          )
          expect(dropTarget.style.opacity).toEqual('1')
        },
      )

      expect(
        renderResult.getEditorState().derived.navigatorTargets.map(navigatorEntryToKey),
      ).toEqual([
        'regular-utopia-storyboard-uid/scene-aaa',
        'regular-utopia-storyboard-uid/scene-aaa/sceneroot',
        'regular-utopia-storyboard-uid/scene-aaa/sceneroot/e46', // <- moved to before `firstdiv`
        'regular-utopia-storyboard-uid/scene-aaa/sceneroot/e46/202~~~1',
        'regular-utopia-storyboard-uid/scene-aaa/sceneroot/e46/202~~~2',
        'regular-utopia-storyboard-uid/scene-aaa/sceneroot/e46/202~~~3',
        'regular-utopia-storyboard-uid/scene-aaa/sceneroot/e46/202~~~4',
        'regular-utopia-storyboard-uid/scene-aaa/sceneroot/firstdiv',
        'regular-utopia-storyboard-uid/scene-aaa/sceneroot/seconddiv',
        'regular-utopia-storyboard-uid/scene-aaa/sceneroot/thirddiv',
        'regular-utopia-storyboard-uid/scene-aaa/sceneroot/notdrag',
        'regular-utopia-storyboard-uid/scene-aaa/parentsibling',
      ])
    })

    it('reorders a map expression to after the first sibling', async () => {
      const renderResult = await renderTestEditorWithCode(
        getProjectCodeWithMapExpression(),
        'await-first-dom-report',
      )

      const dragMeElement = await renderResult.renderedDOM.findByTestId(
        `navigator-item-regular_utopia_storyboard_uid/scene_aaa/sceneroot/e46`,
      )
      const dragMeElementRect = dragMeElement.getBoundingClientRect()
      const dragMeElementCenter = getDomRectCenter(dragMeElementRect)
      const firstDivElement = await renderResult.renderedDOM.findByTestId(
        `navigator-item-regular_utopia_storyboard_uid/scene_aaa/sceneroot/firstdiv`,
      )
      const firstDivElementRect = firstDivElement.getBoundingClientRect()
      const firstDivElementCenter = getDomRectCenter(firstDivElementRect)
      const dragTo = {
        x: firstDivElementCenter.x,
        y: firstDivElementRect.y + firstDivElementRect.height,
      }

      const dragDelta = windowPoint({
        x: dragTo.x - dragMeElementCenter.x,
        y: dragTo.y - dragMeElementCenter.y,
      })

      const targetElement = EP.fromString('utopia-storyboard-uid/scene-aaa/sceneroot/e46')
      await act(async () => {
        const dispatchDone = renderResult.getDispatchFollowUpActionsFinished()
        await renderResult.dispatch([selectComponents([targetElement], false)], false)
        await dispatchDone
      })

      await dragElementWithDNDEvents(
        renderResult,
        `navigator-item-drag-regular_utopia_storyboard_uid/scene_aaa/sceneroot/e46`,
        `navigator-item-drop-after-regular_utopia_storyboard_uid/scene_aaa/sceneroot/firstdiv`,
        windowPoint(dragMeElementCenter),
        dragDelta,

        async () => {
          expect(renderResult.getEditorState().editor.navigator.dropTargetHint?.type).toEqual(
            'after',
          )
          // parent highlight is shown
          const parentEntry = renderResult.renderedDOM.getByTestId(
            `navigator-item-regular_utopia_storyboard_uid/scene_aaa/sceneroot`,
          )
          expect((parentEntry.firstChild?.firstChild as HTMLElement).style.outline).toEqual(
            '1px solid var(--utopitheme-navigatorResizeHintBorder)',
          )

          // drop target line is shown
          const dropTarget = renderResult.renderedDOM.getByTestId(
            `navigator-item-drop-after-regular_utopia_storyboard_uid/scene_aaa/sceneroot/firstdiv`,
          )
          expect(dropTarget.style.opacity).toEqual('1')
        },
      )

      expect(
        renderResult.getEditorState().derived.navigatorTargets.map(navigatorEntryToKey),
      ).toEqual([
        'regular-utopia-storyboard-uid/scene-aaa',
        'regular-utopia-storyboard-uid/scene-aaa/sceneroot',
        'regular-utopia-storyboard-uid/scene-aaa/sceneroot/firstdiv',
        'regular-utopia-storyboard-uid/scene-aaa/sceneroot/e46', // <- moved to after `firstdiv`
        'regular-utopia-storyboard-uid/scene-aaa/sceneroot/e46/202~~~1',
        'regular-utopia-storyboard-uid/scene-aaa/sceneroot/e46/202~~~2',
        'regular-utopia-storyboard-uid/scene-aaa/sceneroot/e46/202~~~3',
        'regular-utopia-storyboard-uid/scene-aaa/sceneroot/e46/202~~~4',
        'regular-utopia-storyboard-uid/scene-aaa/sceneroot/seconddiv',
        'regular-utopia-storyboard-uid/scene-aaa/sceneroot/thirddiv',
        'regular-utopia-storyboard-uid/scene-aaa/sceneroot/notdrag',
        'regular-utopia-storyboard-uid/scene-aaa/parentsibling',
      ])
    })

    it('reorders a map expression to after the last sibling', async () => {
      const renderResult = await renderTestEditorWithCode(
        getProjectCodeWithMapExpression(),
        'await-first-dom-report',
      )

      const dragMeElement = await renderResult.renderedDOM.findByTestId(
        `navigator-item-regular_utopia_storyboard_uid/scene_aaa/sceneroot/e46`,
      )
      const dragMeElementRect = dragMeElement.getBoundingClientRect()
      const dragMeElementCenter = getDomRectCenter(dragMeElementRect)
      const notDraggableDivElement = await renderResult.renderedDOM.findByTestId(
        `navigator-item-regular_utopia_storyboard_uid/scene_aaa/sceneroot/notdrag`,
      )
      const notDraggableDivElementRect = notDraggableDivElement.getBoundingClientRect()
      const notDraggableDivElementCenter = getDomRectCenter(notDraggableDivElementRect)
      const dragTo = {
        x: notDraggableDivElementCenter.x,
        y: notDraggableDivElementRect.y + notDraggableDivElementRect.height - 3,
      }

      const dragDelta = windowPoint({
        x: dragTo.x - dragMeElementCenter.x,
        y: dragTo.y - dragMeElementCenter.y,
      })

      const targetElement = EP.fromString('utopia-storyboard-uid/scene-aaa/sceneroot/e46')
      await act(async () => {
        const dispatchDone = renderResult.getDispatchFollowUpActionsFinished()
        await renderResult.dispatch([selectComponents([targetElement], false)], false)
        await dispatchDone
      })

      await dragElementWithDNDEvents(
        renderResult,
        `navigator-item-drag-regular_utopia_storyboard_uid/scene_aaa/sceneroot/e46`,
        `navigator-item-drop-after-regular_utopia_storyboard_uid/scene_aaa/sceneroot/notdrag`,
        windowPoint(dragMeElementCenter),
        dragDelta,

        async () => {
          expect(renderResult.getEditorState().editor.navigator.dropTargetHint?.type).toEqual(
            'after',
          )
          // parent highlight is shown
          const parentEntry = renderResult.renderedDOM.getByTestId(
            `navigator-item-regular_utopia_storyboard_uid/scene_aaa/sceneroot`,
          )
          expect((parentEntry.firstChild?.firstChild as HTMLElement).style.outline).toEqual(
            '1px solid var(--utopitheme-navigatorResizeHintBorder)',
          )

          // drop target line is shown
          const dropTarget = renderResult.renderedDOM.getByTestId(
            `navigator-item-drop-after-regular_utopia_storyboard_uid/scene_aaa/sceneroot/notdrag`,
          )
          expect(dropTarget.style.opacity).toEqual('1')
        },
      )

      await renderResult.getDispatchFollowUpActionsFinished()

      expect(
        renderResult.getEditorState().derived.navigatorTargets.map(navigatorEntryToKey),
      ).toEqual([
        'regular-utopia-storyboard-uid/scene-aaa',
        'regular-utopia-storyboard-uid/scene-aaa/sceneroot',
        'regular-utopia-storyboard-uid/scene-aaa/sceneroot/firstdiv',
        'regular-utopia-storyboard-uid/scene-aaa/sceneroot/seconddiv',
        'regular-utopia-storyboard-uid/scene-aaa/sceneroot/thirddiv',
        'regular-utopia-storyboard-uid/scene-aaa/sceneroot/notdrag',
        'regular-utopia-storyboard-uid/scene-aaa/sceneroot/e46', // <- moved to after the last sibling `notdrag` under its parent
        'regular-utopia-storyboard-uid/scene-aaa/sceneroot/e46/202~~~1',
        'regular-utopia-storyboard-uid/scene-aaa/sceneroot/e46/202~~~2',
        'regular-utopia-storyboard-uid/scene-aaa/sceneroot/e46/202~~~3',
        'regular-utopia-storyboard-uid/scene-aaa/sceneroot/e46/202~~~4',
        'regular-utopia-storyboard-uid/scene-aaa/parentsibling',
      ])
    })

    it('reparents a map expression under the first sibling', async () => {
      const renderResult = await renderTestEditorWithCode(
        getProjectCodeWithMapExpression(),
        'await-first-dom-report',
      )

      const dragMeElement = await renderResult.renderedDOM.findByTestId(
        `navigator-item-regular_utopia_storyboard_uid/scene_aaa/sceneroot/e46`,
      )
      const dragMeElementRect = dragMeElement.getBoundingClientRect()
      const dragMeElementCenter = getDomRectCenter(dragMeElementRect)
      const firstDivElement = await renderResult.renderedDOM.findByTestId(
        `navigator-item-regular_utopia_storyboard_uid/scene_aaa/sceneroot/firstdiv`,
      )
      const firstDivElementRect = firstDivElement.getBoundingClientRect()
      const firstDivElementCenter = getDomRectCenter(firstDivElementRect)
      const dragTo = {
        x: firstDivElementCenter.x,
        y: firstDivElementRect.y + 20,
      }

      const dragDelta = windowPoint({
        x: dragTo.x - dragMeElementCenter.x,
        y: dragTo.y - dragMeElementCenter.y,
      })

      const targetElement = EP.fromString('utopia-storyboard-uid/scene-aaa/sceneroot/e46')
      await act(async () => {
        const dispatchDone = renderResult.getDispatchFollowUpActionsFinished()
        await renderResult.dispatch([selectComponents([targetElement], false)], false)
        await dispatchDone
      })

      await act(async () =>
        dragElementWithDNDEvents(
          renderResult,
          `navigator-item-drag-regular_utopia_storyboard_uid/scene_aaa/sceneroot/e46`,
          `navigator-item-regular_utopia_storyboard_uid/scene_aaa/sceneroot/firstdiv`,
          windowPoint(dragMeElementCenter),
          dragDelta,

          async () => {
            expect(renderResult.getEditorState().editor.navigator.dropTargetHint?.type).toEqual(
              'reparent',
            )
            // parent highlight is shown
            const parentEntry = renderResult.renderedDOM.getByTestId(
              `navigator-item-regular_utopia_storyboard_uid/scene_aaa/sceneroot/firstdiv`,
            )
            expect((parentEntry.firstChild?.firstChild as HTMLElement).style.outline).toEqual(
              '1px solid var(--utopitheme-navigatorResizeHintBorder)',
            )

            // drop target lines are not shown
            const dropTargetBefore = renderResult.renderedDOM.getByTestId(
              `navigator-item-drop-before-regular_utopia_storyboard_uid/scene_aaa/sceneroot/firstdiv`,
            )
            expect(dropTargetBefore.style.opacity).toEqual('0')

            const dropTargetAfter = renderResult.renderedDOM.getByTestId(
              `navigator-item-drop-after-regular_utopia_storyboard_uid/scene_aaa/sceneroot/firstdiv`,
            )
            expect(dropTargetAfter.style.opacity).toEqual('0')
          },
        ),
      )

      await renderResult.getDispatchFollowUpActionsFinished()

      expect(
        renderResult.getEditorState().derived.navigatorTargets.map(navigatorEntryToKey),
      ).toEqual([
        'regular-utopia-storyboard-uid/scene-aaa',
        'regular-utopia-storyboard-uid/scene-aaa/sceneroot',
        'regular-utopia-storyboard-uid/scene-aaa/sceneroot/firstdiv',
        'regular-utopia-storyboard-uid/scene-aaa/sceneroot/firstdiv/e46', // <- moved to under the first sibling
        'regular-utopia-storyboard-uid/scene-aaa/sceneroot/firstdiv/e46/202~~~1',
        'regular-utopia-storyboard-uid/scene-aaa/sceneroot/firstdiv/e46/202~~~2',
        'regular-utopia-storyboard-uid/scene-aaa/sceneroot/firstdiv/e46/202~~~3',
        'regular-utopia-storyboard-uid/scene-aaa/sceneroot/firstdiv/e46/202~~~4',
        'regular-utopia-storyboard-uid/scene-aaa/sceneroot/seconddiv',
        'regular-utopia-storyboard-uid/scene-aaa/sceneroot/thirddiv',
        'regular-utopia-storyboard-uid/scene-aaa/sceneroot/notdrag',
        'regular-utopia-storyboard-uid/scene-aaa/parentsibling',
      ])
    })

    it('does not reparent to a map expression', async () => {
      const renderResult = await renderTestEditorWithCode(
        getProjectCodeWithMapExpression(),
        'await-first-dom-report',
      )

      const dragMeElement = await renderResult.renderedDOM.findByTestId(
        `navigator-item-regular_utopia_storyboard_uid/scene_aaa/sceneroot/firstdiv`,
      )
      const dragMeElementRect = dragMeElement.getBoundingClientRect()
      const dragMeElementCenter = getDomRectCenter(dragMeElementRect)
      const notDraggableDivElement = await renderResult.renderedDOM.findByTestId(
        `navigator-item-regular_utopia_storyboard_uid/scene_aaa/sceneroot/e46`,
      )
      const notDraggableDivElementRect = notDraggableDivElement.getBoundingClientRect()
      const notDraggableDivElementCenter = getDomRectCenter(notDraggableDivElementRect)
      const dragTo = {
        x: notDraggableDivElementCenter.x,
        y: notDraggableDivElementRect.y,
      }

      const dragDelta = windowPoint({
        x: dragTo.x - dragMeElementCenter.x,
        y: dragTo.y - dragMeElementCenter.y,
      })

      const targetElement = EP.fromString(
        'regular-utopia-storyboard-uid/scene-aaa/sceneroot/firstdiv',
      )
      await act(async () => {
        const dispatchDone = renderResult.getDispatchFollowUpActionsFinished()
        await renderResult.dispatch([selectComponents([targetElement], false)], false)
        await dispatchDone
      })

      await act(async () =>
        dragElementWithDNDEvents(
          renderResult,
          `navigator-item-drag-regular_utopia_storyboard_uid/scene_aaa/sceneroot/firstdiv`,
          `navigator-item-regular_utopia_storyboard_uid/scene_aaa/sceneroot/e46`,
          windowPoint(dragMeElementCenter),
          dragDelta,
        ),
      )

      await renderResult.getDispatchFollowUpActionsFinished()

      expect(
        renderResult.getEditorState().derived.navigatorTargets.map(navigatorEntryToKey),
      ).toEqual([
        'regular-utopia-storyboard-uid/scene-aaa',
        'regular-utopia-storyboard-uid/scene-aaa/sceneroot',
        'regular-utopia-storyboard-uid/scene-aaa/sceneroot/firstdiv', // <- cannot be moved
        'regular-utopia-storyboard-uid/scene-aaa/sceneroot/seconddiv',
        'regular-utopia-storyboard-uid/scene-aaa/sceneroot/thirddiv',
        'regular-utopia-storyboard-uid/scene-aaa/sceneroot/e46',
        'regular-utopia-storyboard-uid/scene-aaa/sceneroot/e46/202~~~1',
        'regular-utopia-storyboard-uid/scene-aaa/sceneroot/e46/202~~~2',
        'regular-utopia-storyboard-uid/scene-aaa/sceneroot/e46/202~~~3',
        'regular-utopia-storyboard-uid/scene-aaa/sceneroot/e46/202~~~4',
        'regular-utopia-storyboard-uid/scene-aaa/sceneroot/notdrag',
        'regular-utopia-storyboard-uid/scene-aaa/parentsibling',
      ])
    })

    it('can not reparent into generated child of a map expression', async () => {
      const renderResult = await renderTestEditorWithCode(
        getProjectCodeWithMapExpression(),
        'await-first-dom-report',
      )

      const dragMeElement = await renderResult.renderedDOM.findByTestId(
        `navigator-item-regular_utopia_storyboard_uid/scene_aaa/sceneroot/firstdiv`,
      )
      const dragMeElementRect = dragMeElement.getBoundingClientRect()
      const dragMeElementCenter = getDomRectCenter(dragMeElementRect)
      const notDraggableDivElement = await renderResult.renderedDOM.findByTestId(
        `navigator-item-regular_utopia_storyboard_uid/scene_aaa/sceneroot/e46/202~~~3`,
      )
      const notDraggableDivElementRect = notDraggableDivElement.getBoundingClientRect()
      const notDraggableDivElementCenter = getDomRectCenter(notDraggableDivElementRect)
      const dragTo = {
        x: notDraggableDivElementCenter.x,
        y: notDraggableDivElementRect.y,
      }

      const dragDelta = windowPoint({
        x: dragTo.x - dragMeElementCenter.x,
        y: dragTo.y - dragMeElementCenter.y,
      })

      const targetElement = EP.fromString(
        'regular-utopia-storyboard-uid/scene-aaa/sceneroot/firstdiv',
      )
      await act(async () => {
        const dispatchDone = renderResult.getDispatchFollowUpActionsFinished()
        await renderResult.dispatch([selectComponents([targetElement], false)], false)
        await dispatchDone
      })

      await act(async () =>
        dragElementWithDNDEvents(
          renderResult,
          `navigator-item-drag-regular_utopia_storyboard_uid/scene_aaa/sceneroot/firstdiv`,
          `navigator-item-regular_utopia_storyboard_uid/scene_aaa/sceneroot/e46/202~~~3`,
          windowPoint(dragMeElementCenter),
          dragDelta,
        ),
      )

      await renderResult.getDispatchFollowUpActionsFinished()

      expect(
        renderResult.getEditorState().derived.navigatorTargets.map(navigatorEntryToKey),
      ).toEqual([
        'regular-utopia-storyboard-uid/scene-aaa',
        'regular-utopia-storyboard-uid/scene-aaa/sceneroot',
        'regular-utopia-storyboard-uid/scene-aaa/sceneroot/firstdiv', // <- cannot be moved
        'regular-utopia-storyboard-uid/scene-aaa/sceneroot/seconddiv',
        'regular-utopia-storyboard-uid/scene-aaa/sceneroot/thirddiv',
        'regular-utopia-storyboard-uid/scene-aaa/sceneroot/e46',
        'regular-utopia-storyboard-uid/scene-aaa/sceneroot/e46/202~~~1',
        'regular-utopia-storyboard-uid/scene-aaa/sceneroot/e46/202~~~2',
        'regular-utopia-storyboard-uid/scene-aaa/sceneroot/e46/202~~~3',
        'regular-utopia-storyboard-uid/scene-aaa/sceneroot/e46/202~~~4',
        'regular-utopia-storyboard-uid/scene-aaa/sceneroot/notdrag',
        'regular-utopia-storyboard-uid/scene-aaa/parentsibling',
      ])
    })
  })

  describe('reordering an expression', () => {
    it('reorders an expression to before the first sibling', async () => {
      const renderResult = await renderTestEditorWithCode(
        getProjectCodeWithExpression(),
        'await-first-dom-report',
      )

      const dragMeElement = await renderResult.renderedDOM.findByTestId(
        `navigator-item-regular_utopia_storyboard_uid/scene_aaa/sceneroot/b62`,
      )
      const dragMeElementRect = dragMeElement.getBoundingClientRect()
      const dragMeElementCenter = getDomRectCenter(dragMeElementRect)
      const firstDivElement = await renderResult.renderedDOM.findByTestId(
        `navigator-item-regular_utopia_storyboard_uid/scene_aaa/sceneroot/firstdiv`,
      )
      const firstDivElementRect = firstDivElement.getBoundingClientRect()
      const firstDivElementCenter = getDomRectCenter(firstDivElementRect)
      const dragTo = {
        x: firstDivElementCenter.x,
        y: firstDivElementRect.y + 3,
      }

      const dragDelta = windowPoint({
        x: dragTo.x - dragMeElementCenter.x,
        y: dragTo.y - dragMeElementCenter.y,
      })

      const targetElement = EP.fromString('utopia-storyboard-uid/scene-aaa/sceneroot/b62')
      await act(async () => {
        const dispatchDone = renderResult.getDispatchFollowUpActionsFinished()
        await renderResult.dispatch([selectComponents([targetElement], false)], false)
        await dispatchDone
      })

      await dragElementWithDNDEvents(
        renderResult,
        `navigator-item-drag-regular_utopia_storyboard_uid/scene_aaa/sceneroot/b62`,
        `navigator-item-drop-before-regular_utopia_storyboard_uid/scene_aaa/sceneroot/firstdiv`,
        windowPoint(dragMeElementCenter),
        dragDelta,

        async () => {
          expect(renderResult.getEditorState().editor.navigator.dropTargetHint?.type).toEqual(
            'before',
          )
          // parent highlight is shown
          const parentEntry = renderResult.renderedDOM.getByTestId(
            `navigator-item-regular_utopia_storyboard_uid/scene_aaa/sceneroot`,
          )
          expect((parentEntry.firstChild?.firstChild as HTMLElement).style.outline).toEqual(
            '1px solid var(--utopitheme-navigatorResizeHintBorder)',
          )

          // drop target line is shown
          const dropTarget = renderResult.renderedDOM.getByTestId(
            `navigator-item-drop-before-regular_utopia_storyboard_uid/scene_aaa/sceneroot/firstdiv`,
          )
          expect(dropTarget.style.opacity).toEqual('1')
        },
      )

      expect(
        renderResult.getEditorState().derived.navigatorTargets.map(navigatorEntryToKey),
      ).toEqual([
        'regular-utopia-storyboard-uid/scene-aaa',
        'regular-utopia-storyboard-uid/scene-aaa/sceneroot',
        'regular-utopia-storyboard-uid/scene-aaa/sceneroot/b62', // <- moved to before `firstdiv`
        'regular-utopia-storyboard-uid/scene-aaa/sceneroot/b62/ccc~~~1',
        'regular-utopia-storyboard-uid/scene-aaa/sceneroot/firstdiv',
        'regular-utopia-storyboard-uid/scene-aaa/sceneroot/seconddiv',
        'regular-utopia-storyboard-uid/scene-aaa/sceneroot/thirddiv',
        'regular-utopia-storyboard-uid/scene-aaa/sceneroot/notdrag',
        'regular-utopia-storyboard-uid/scene-aaa/parentsibling',
      ])
    })

    it('reorders an expression to after the first sibling', async () => {
      const renderResult = await renderTestEditorWithCode(
        getProjectCodeWithExpression(),
        'await-first-dom-report',
      )

      const dragMeElement = await renderResult.renderedDOM.findByTestId(
        `navigator-item-regular_utopia_storyboard_uid/scene_aaa/sceneroot/b62`,
      )
      const dragMeElementRect = dragMeElement.getBoundingClientRect()
      const dragMeElementCenter = getDomRectCenter(dragMeElementRect)
      const firstDivElement = await renderResult.renderedDOM.findByTestId(
        `navigator-item-regular_utopia_storyboard_uid/scene_aaa/sceneroot/firstdiv`,
      )
      const firstDivElementRect = firstDivElement.getBoundingClientRect()
      const firstDivElementCenter = getDomRectCenter(firstDivElementRect)
      const dragTo = {
        x: firstDivElementCenter.x,
        y: firstDivElementRect.y + firstDivElementRect.height,
      }

      const dragDelta = windowPoint({
        x: dragTo.x - dragMeElementCenter.x,
        y: dragTo.y - dragMeElementCenter.y,
      })

      const targetElement = EP.fromString('utopia-storyboard-uid/scene-aaa/sceneroot/b62')
      await act(async () => {
        const dispatchDone = renderResult.getDispatchFollowUpActionsFinished()
        await renderResult.dispatch([selectComponents([targetElement], false)], false)
        await dispatchDone
      })

      await dragElementWithDNDEvents(
        renderResult,
        `navigator-item-drag-regular_utopia_storyboard_uid/scene_aaa/sceneroot/b62`,
        `navigator-item-drop-after-regular_utopia_storyboard_uid/scene_aaa/sceneroot/firstdiv`,
        windowPoint(dragMeElementCenter),
        dragDelta,

        async () => {
          expect(renderResult.getEditorState().editor.navigator.dropTargetHint?.type).toEqual(
            'after',
          )
          // parent highlight is shown
          const parentEntry = renderResult.renderedDOM.getByTestId(
            `navigator-item-regular_utopia_storyboard_uid/scene_aaa/sceneroot`,
          )
          expect((parentEntry.firstChild?.firstChild as HTMLElement).style.outline).toEqual(
            '1px solid var(--utopitheme-navigatorResizeHintBorder)',
          )

          // drop target line is shown
          const dropTarget = renderResult.renderedDOM.getByTestId(
            `navigator-item-drop-after-regular_utopia_storyboard_uid/scene_aaa/sceneroot/firstdiv`,
          )
          expect(dropTarget.style.opacity).toEqual('1')
        },
      )

      expect(
        renderResult.getEditorState().derived.navigatorTargets.map(navigatorEntryToKey),
      ).toEqual([
        'regular-utopia-storyboard-uid/scene-aaa',
        'regular-utopia-storyboard-uid/scene-aaa/sceneroot',
        'regular-utopia-storyboard-uid/scene-aaa/sceneroot/firstdiv',
        'regular-utopia-storyboard-uid/scene-aaa/sceneroot/b62', // <- moved to after `firstdiv`
        'regular-utopia-storyboard-uid/scene-aaa/sceneroot/b62/ccc~~~1',
        'regular-utopia-storyboard-uid/scene-aaa/sceneroot/seconddiv',
        'regular-utopia-storyboard-uid/scene-aaa/sceneroot/thirddiv',
        'regular-utopia-storyboard-uid/scene-aaa/sceneroot/notdrag',
        'regular-utopia-storyboard-uid/scene-aaa/parentsibling',
      ])
    })

    it('reorders an expression to after the last sibling', async () => {
      const renderResult = await renderTestEditorWithCode(
        getProjectCodeWithExpression(),
        'await-first-dom-report',
      )

      const dragMeElement = await renderResult.renderedDOM.findByTestId(
        `navigator-item-regular_utopia_storyboard_uid/scene_aaa/sceneroot/b62`,
      )
      const dragMeElementRect = dragMeElement.getBoundingClientRect()
      const dragMeElementCenter = getDomRectCenter(dragMeElementRect)
      const notDraggableDivElement = await renderResult.renderedDOM.findByTestId(
        `navigator-item-regular_utopia_storyboard_uid/scene_aaa/sceneroot/notdrag`,
      )
      const notDraggableDivElementRect = notDraggableDivElement.getBoundingClientRect()
      const notDraggableDivElementCenter = getDomRectCenter(notDraggableDivElementRect)
      const dragTo = {
        x: notDraggableDivElementCenter.x,
        y: notDraggableDivElementRect.y + notDraggableDivElementRect.height - 3,
      }

      const dragDelta = windowPoint({
        x: dragTo.x - dragMeElementCenter.x,
        y: dragTo.y - dragMeElementCenter.y,
      })

      const targetElement = EP.fromString('utopia-storyboard-uid/scene-aaa/sceneroot/b62')
      await act(async () => {
        const dispatchDone = renderResult.getDispatchFollowUpActionsFinished()
        await renderResult.dispatch([selectComponents([targetElement], false)], false)
        await dispatchDone
      })

      await dragElementWithDNDEvents(
        renderResult,
        `navigator-item-drag-regular_utopia_storyboard_uid/scene_aaa/sceneroot/b62`,
        `navigator-item-drop-after-regular_utopia_storyboard_uid/scene_aaa/sceneroot/notdrag`,
        windowPoint(dragMeElementCenter),
        dragDelta,

        async () => {
          expect(renderResult.getEditorState().editor.navigator.dropTargetHint?.type).toEqual(
            'after',
          )
          // parent highlight is shown
          const parentEntry = renderResult.renderedDOM.getByTestId(
            `navigator-item-regular_utopia_storyboard_uid/scene_aaa/sceneroot`,
          )
          expect((parentEntry.firstChild?.firstChild as HTMLElement).style.outline).toEqual(
            '1px solid var(--utopitheme-navigatorResizeHintBorder)',
          )

          // drop target line is shown
          const dropTarget = renderResult.renderedDOM.getByTestId(
            `navigator-item-drop-after-regular_utopia_storyboard_uid/scene_aaa/sceneroot/notdrag`,
          )
          expect(dropTarget.style.opacity).toEqual('1')
        },
      )

      await renderResult.getDispatchFollowUpActionsFinished()

      expect(
        renderResult.getEditorState().derived.navigatorTargets.map(navigatorEntryToKey),
      ).toEqual([
        'regular-utopia-storyboard-uid/scene-aaa',
        'regular-utopia-storyboard-uid/scene-aaa/sceneroot',
        'regular-utopia-storyboard-uid/scene-aaa/sceneroot/firstdiv',
        'regular-utopia-storyboard-uid/scene-aaa/sceneroot/seconddiv',
        'regular-utopia-storyboard-uid/scene-aaa/sceneroot/thirddiv',
        'regular-utopia-storyboard-uid/scene-aaa/sceneroot/notdrag',
        'regular-utopia-storyboard-uid/scene-aaa/sceneroot/b62', // <- moved to after the last sibling `notdrag` under its parent
        'regular-utopia-storyboard-uid/scene-aaa/sceneroot/b62/ccc~~~1',
        'regular-utopia-storyboard-uid/scene-aaa/parentsibling',
      ])
    })

    it('reparents an expression under the first sibling', async () => {
      const renderResult = await renderTestEditorWithCode(
        getProjectCodeWithExpression(),
        'await-first-dom-report',
      )

      const dragMeElement = await renderResult.renderedDOM.findByTestId(
        `navigator-item-regular_utopia_storyboard_uid/scene_aaa/sceneroot/b62`,
      )
      const dragMeElementRect = dragMeElement.getBoundingClientRect()
      const dragMeElementCenter = getDomRectCenter(dragMeElementRect)
      const firstDivElement = await renderResult.renderedDOM.findByTestId(
        `navigator-item-regular_utopia_storyboard_uid/scene_aaa/sceneroot/firstdiv`,
      )
      const firstDivElementRect = firstDivElement.getBoundingClientRect()
      const firstDivElementCenter = getDomRectCenter(firstDivElementRect)
      const dragTo = {
        x: firstDivElementCenter.x,
        y: firstDivElementRect.y + 20,
      }

      const dragDelta = windowPoint({
        x: dragTo.x - dragMeElementCenter.x,
        y: dragTo.y - dragMeElementCenter.y,
      })

      const targetElement = EP.fromString('utopia-storyboard-uid/scene-aaa/sceneroot/b62')
      await act(async () => {
        const dispatchDone = renderResult.getDispatchFollowUpActionsFinished()
        await renderResult.dispatch([selectComponents([targetElement], false)], false)
        await dispatchDone
      })

      await act(async () =>
        dragElementWithDNDEvents(
          renderResult,
          `navigator-item-drag-regular_utopia_storyboard_uid/scene_aaa/sceneroot/b62`,
          `navigator-item-regular_utopia_storyboard_uid/scene_aaa/sceneroot/firstdiv`,
          windowPoint(dragMeElementCenter),
          dragDelta,

          async () => {
            expect(renderResult.getEditorState().editor.navigator.dropTargetHint?.type).toEqual(
              'reparent',
            )
            // parent highlight is shown
            const parentEntry = renderResult.renderedDOM.getByTestId(
              `navigator-item-regular_utopia_storyboard_uid/scene_aaa/sceneroot/firstdiv`,
            )
            expect((parentEntry.firstChild?.firstChild as HTMLElement).style.outline).toEqual(
              '1px solid var(--utopitheme-navigatorResizeHintBorder)',
            )

            // drop target lines are not shown
            const dropTargetBefore = renderResult.renderedDOM.getByTestId(
              `navigator-item-drop-before-regular_utopia_storyboard_uid/scene_aaa/sceneroot/firstdiv`,
            )
            expect(dropTargetBefore.style.opacity).toEqual('0')

            const dropTargetAfter = renderResult.renderedDOM.getByTestId(
              `navigator-item-drop-after-regular_utopia_storyboard_uid/scene_aaa/sceneroot/firstdiv`,
            )
            expect(dropTargetAfter.style.opacity).toEqual('0')
          },
        ),
      )

      await renderResult.getDispatchFollowUpActionsFinished()

      expect(
        renderResult.getEditorState().derived.navigatorTargets.map(navigatorEntryToKey),
      ).toEqual([
        'regular-utopia-storyboard-uid/scene-aaa',
        'regular-utopia-storyboard-uid/scene-aaa/sceneroot',
        'regular-utopia-storyboard-uid/scene-aaa/sceneroot/firstdiv',
        'regular-utopia-storyboard-uid/scene-aaa/sceneroot/firstdiv/b62', // <- moved to under the first sibling
        'regular-utopia-storyboard-uid/scene-aaa/sceneroot/firstdiv/b62/ccc~~~1',
        'regular-utopia-storyboard-uid/scene-aaa/sceneroot/seconddiv',
        'regular-utopia-storyboard-uid/scene-aaa/sceneroot/thirddiv',
        'regular-utopia-storyboard-uid/scene-aaa/sceneroot/notdrag',
        'regular-utopia-storyboard-uid/scene-aaa/parentsibling',
      ])
    })

    it('does not reparent to an expression', async () => {
      const renderResult = await renderTestEditorWithCode(
        getProjectCodeWithExpression(),
        'await-first-dom-report',
      )

      const dragMeElement = await renderResult.renderedDOM.findByTestId(
        `navigator-item-regular_utopia_storyboard_uid/scene_aaa/sceneroot/firstdiv`,
      )
      const dragMeElementRect = dragMeElement.getBoundingClientRect()
      const dragMeElementCenter = getDomRectCenter(dragMeElementRect)
      const notDraggableDivElement = await renderResult.renderedDOM.findByTestId(
        `navigator-item-regular_utopia_storyboard_uid/scene_aaa/sceneroot/b62`,
      )
      const notDraggableDivElementRect = notDraggableDivElement.getBoundingClientRect()
      const notDraggableDivElementCenter = getDomRectCenter(notDraggableDivElementRect)
      const dragTo = {
        x: notDraggableDivElementCenter.x,
        y: notDraggableDivElementRect.y,
      }

      const dragDelta = windowPoint({
        x: dragTo.x - dragMeElementCenter.x,
        y: dragTo.y - dragMeElementCenter.y,
      })

      const targetElement = EP.fromString(
        'regular-utopia-storyboard-uid/scene-aaa/sceneroot/firstdiv',
      )
      await act(async () => {
        const dispatchDone = renderResult.getDispatchFollowUpActionsFinished()
        await renderResult.dispatch([selectComponents([targetElement], false)], false)
        await dispatchDone
      })

      await act(async () =>
        dragElementWithDNDEvents(
          renderResult,
          `navigator-item-drag-regular_utopia_storyboard_uid/scene_aaa/sceneroot/firstdiv`,
          `navigator-item-regular_utopia_storyboard_uid/scene_aaa/sceneroot/b62`,
          windowPoint(dragMeElementCenter),
          dragDelta,
        ),
      )

      await renderResult.getDispatchFollowUpActionsFinished()

      expect(
        renderResult.getEditorState().derived.navigatorTargets.map(navigatorEntryToKey),
      ).toEqual([
        'regular-utopia-storyboard-uid/scene-aaa',
        'regular-utopia-storyboard-uid/scene-aaa/sceneroot',
        'regular-utopia-storyboard-uid/scene-aaa/sceneroot/firstdiv', // <- cannot be moved
        'regular-utopia-storyboard-uid/scene-aaa/sceneroot/seconddiv',
        'regular-utopia-storyboard-uid/scene-aaa/sceneroot/thirddiv',
        'regular-utopia-storyboard-uid/scene-aaa/sceneroot/b62',
        'regular-utopia-storyboard-uid/scene-aaa/sceneroot/b62/ccc~~~1',
        'regular-utopia-storyboard-uid/scene-aaa/sceneroot/notdrag',
        'regular-utopia-storyboard-uid/scene-aaa/parentsibling',
      ])
    })

    it('can not reparent into generated child of an expression', async () => {
      const renderResult = await renderTestEditorWithCode(
        getProjectCodeWithExpression(),
        'await-first-dom-report',
      )

      const dragMeElement = await renderResult.renderedDOM.findByTestId(
        `navigator-item-regular_utopia_storyboard_uid/scene_aaa/sceneroot/firstdiv`,
      )
      const dragMeElementRect = dragMeElement.getBoundingClientRect()
      const dragMeElementCenter = getDomRectCenter(dragMeElementRect)
      const notDraggableDivElement = await renderResult.renderedDOM.findByTestId(
        `navigator-item-regular_utopia_storyboard_uid/scene_aaa/sceneroot/b62/ccc~~~1`,
      )
      const notDraggableDivElementRect = notDraggableDivElement.getBoundingClientRect()
      const notDraggableDivElementCenter = getDomRectCenter(notDraggableDivElementRect)
      const dragTo = {
        x: notDraggableDivElementCenter.x,
        y: notDraggableDivElementRect.y,
      }

      const dragDelta = windowPoint({
        x: dragTo.x - dragMeElementCenter.x,
        y: dragTo.y - dragMeElementCenter.y,
      })

      const targetElement = EP.fromString(
        'regular-utopia-storyboard-uid/scene-aaa/sceneroot/firstdiv',
      )
      await act(async () => {
        const dispatchDone = renderResult.getDispatchFollowUpActionsFinished()
        await renderResult.dispatch([selectComponents([targetElement], false)], false)
        await dispatchDone
      })

      await act(async () =>
        dragElementWithDNDEvents(
          renderResult,
          `navigator-item-drag-regular_utopia_storyboard_uid/scene_aaa/sceneroot/firstdiv`,
          `navigator-item-regular_utopia_storyboard_uid/scene_aaa/sceneroot/b62/ccc~~~1`,
          windowPoint(dragMeElementCenter),
          dragDelta,
        ),
      )

      await renderResult.getDispatchFollowUpActionsFinished()

      expect(
        renderResult.getEditorState().derived.navigatorTargets.map(navigatorEntryToKey),
      ).toEqual([
        'regular-utopia-storyboard-uid/scene-aaa',
        'regular-utopia-storyboard-uid/scene-aaa/sceneroot',
        'regular-utopia-storyboard-uid/scene-aaa/sceneroot/firstdiv', // <- cannot be moved
        'regular-utopia-storyboard-uid/scene-aaa/sceneroot/seconddiv',
        'regular-utopia-storyboard-uid/scene-aaa/sceneroot/thirddiv',
        'regular-utopia-storyboard-uid/scene-aaa/sceneroot/b62',
        'regular-utopia-storyboard-uid/scene-aaa/sceneroot/b62/ccc~~~1',
        'regular-utopia-storyboard-uid/scene-aaa/sceneroot/notdrag',
        'regular-utopia-storyboard-uid/scene-aaa/parentsibling',
      ])
    })
  })

  describe('reparenting among layout systems', () => {
    describe('reparenting to flex', () => {
      it('reparenting from the canvas to a flex container', async () => {
        const editor = await renderTestEditorWithCode(
          projectWithFlexContainerAndCanvas,
          'await-first-dom-report',
        )

        const dragMeElementPath = EP.fromString('sb/dragme')
        const targetElementPath = EP.fromString('sb/scene/app:root/container/flex-child')

        const { dragMeElement, startingDragMeElementStyle } = await doBasicDrag(
          editor,
          dragMeElementPath,
          targetElementPath,
        )

        expect(editor.getEditorState().derived.navigatorTargets.map(navigatorEntryToKey)).toEqual([
          'regular-sb/scene',
          'regular-sb/scene/app',
          'regular-sb/scene/app:root',
          'regular-sb/scene/app:root/container',
          'regular-sb/scene/app:root/container/flex-child',
          'regular-sb/scene/app:root/container/dragme', // <- dragme moved here
          'regular-sb/scene/app:root/container/663',
          'regular-sb/scene/app:root/abs-container',
          'regular-sb/scene/app:root/abs-container/abs-child',
          'regular-sb/scene/app:root/abs-container/8ae',
        ])

        expect(dragMeElement.style.position).toEqual('')
        expect(dragMeElement.style.top).toEqual('')
        expect(dragMeElement.style.left).toEqual('')
        expect(dragMeElement.style.bottom).toEqual('')
        expect(dragMeElement.style.right).toEqual('')
        expect(dragMeElement.style.width).toEqual(startingDragMeElementStyle.width)
        expect(dragMeElement.style.height).toEqual(startingDragMeElementStyle.height)
      })

      const flexLayoutSnippet = (thing: string) => `
      <div
        style={{
          width: '100%',
          height: '100%',
          contain: 'layout',
        }}
        data-uid='root'
      >
        <div
          style={{
            backgroundColor: '#aaaaaa33',
            position: 'absolute',
            left: 50.5,
            top: 26,
            width: 'max-content',
            height: 'max-content',
            display: 'flex',
            flexDirection: 'row',
            gap: 37,
            padding: '21px 28.5px',
            alignItems: 'flex-end',
          }}
          data-uid='flex'
        >
          <div
            style={{
              backgroundColor: '#35a853',
              width: 106,
              height: 193,
              contain: 'layout',
            }}
            data-uid='aaa'
          />
          <div
            style={{
              backgroundColor: '#f24e1d',
              width: 65,
              height: 63,
              contain: 'layout',
            }}
            data-uid='aab'
          />
          <div
            style={{
              backgroundColor: '#0075ff',
              width: 58,
              height: 75,
              contain: 'layout',
            }}
            data-uid='fle'
          />
        </div>
        <div
          style={{
            backgroundColor: '#aaaaaa33',
            position: 'absolute',
            left: 40.5,
            top: 311.5,
            width: 362,
            height: 235,
            padding: '21px 28.5px',
          }}
          data-uid='container'
        >
          ${thing}
        </div>
      </div>
      `

      it('reparenting an element pinned to the top/left to flex', async () => {
        const editor = await renderTestEditorWithCode(
          makeTestProjectCodeWithSnippet(
            flexLayoutSnippet(
              `<div
            style={{
              backgroundColor: '#35a853',
              contain: 'layout',
              top: 21,
              position: 'absolute',
              width: 300,
              height: 200,
              left: 26,
            }}
            data-uid='thing'
            data-testid='thing'
            data-label='the Thing'
          />`,
            ),
          ),
          'await-first-dom-report',
        )

        const dragMeElementPath = EP.fromString(
          `${BakedInStoryboardUID}/${TestSceneUID}/${TestAppUID}:root/container/thing`,
        )

        const targetElementPath = EP.fromString(
          `${BakedInStoryboardUID}/${TestSceneUID}/${TestAppUID}:root/flex/aaa`,
        )

        await doBasicDrag(editor, dragMeElementPath, targetElementPath)

        expect(editor.getEditorState().derived.navigatorTargets.map(navigatorEntryToKey)).toEqual([
          'regular-utopia-storyboard-uid/scene-aaa',
          'regular-utopia-storyboard-uid/scene-aaa/app-entity',
          'regular-utopia-storyboard-uid/scene-aaa/app-entity:root',
          'regular-utopia-storyboard-uid/scene-aaa/app-entity:root/flex',
          'regular-utopia-storyboard-uid/scene-aaa/app-entity:root/flex/aaa',
          'regular-utopia-storyboard-uid/scene-aaa/app-entity:root/flex/thing', // <- thing is moved into the flex container
          'regular-utopia-storyboard-uid/scene-aaa/app-entity:root/flex/aab',
          'regular-utopia-storyboard-uid/scene-aaa/app-entity:root/flex/fle',
          'regular-utopia-storyboard-uid/scene-aaa/app-entity:root/container',
        ])

        const element = editor.renderedDOM.getByTestId('thing')

        expect(element.style.position).toEqual('')
        expect(element.style.top).toEqual('')
        expect(element.style.left).toEqual('')
        expect(element.style.bottom).toEqual('')
        expect(element.style.right).toEqual('')
        expect(element.style.width).toEqual('300px')
        expect(element.style.height).toEqual('200px')
      })

      it('reparenting an element pinned on multiple sides to flex', async () => {
        const editor = await renderTestEditorWithCode(
          makeTestProjectCodeWithSnippet(
            flexLayoutSnippet(`
          <div
            style={{
              backgroundColor: '#35a853',
              contain: 'layout',
              top: 21,
              position: 'absolute',
              bottom: 21,
              width: 300,
              left: 26,
            }}
            data-uid='thing'
            data-testid='thing'
            data-label='the Thing'
          />`),
          ),
          'await-first-dom-report',
        )

        const dragMeElementPath = EP.fromString(
          `${BakedInStoryboardUID}/${TestSceneUID}/${TestAppUID}:root/container/thing`,
        )

        const targetElementPath = EP.fromString(
          `${BakedInStoryboardUID}/${TestSceneUID}/${TestAppUID}:root/flex/aaa`,
        )

        await doBasicDrag(editor, dragMeElementPath, targetElementPath)

        expect(editor.getEditorState().derived.navigatorTargets.map(navigatorEntryToKey)).toEqual([
          'regular-utopia-storyboard-uid/scene-aaa',
          'regular-utopia-storyboard-uid/scene-aaa/app-entity',
          'regular-utopia-storyboard-uid/scene-aaa/app-entity:root',
          'regular-utopia-storyboard-uid/scene-aaa/app-entity:root/flex',
          'regular-utopia-storyboard-uid/scene-aaa/app-entity:root/flex/aaa',
          'regular-utopia-storyboard-uid/scene-aaa/app-entity:root/flex/thing', // <- thing is moved into the flex container
          'regular-utopia-storyboard-uid/scene-aaa/app-entity:root/flex/aab',
          'regular-utopia-storyboard-uid/scene-aaa/app-entity:root/flex/fle',
          'regular-utopia-storyboard-uid/scene-aaa/app-entity:root/container',
        ])

        const element = editor.renderedDOM.getByTestId('thing')

        expect(element.style.position).toEqual('')
        expect(element.style.top).toEqual('')
        expect(element.style.left).toEqual('')
        expect(element.style.bottom).toEqual('')
        expect(element.style.right).toEqual('')
        expect(element.style.width).toEqual('300px')
        expect(element.style.height).toEqual('193px')
      })

      it('reparenting an element with relative sizing to flex', async () => {
        const editor = await renderTestEditorWithCode(
          makeTestProjectCodeWithSnippet(
            flexLayoutSnippet(`
          <div
            style={{
              backgroundColor: '#35a853',
              contain: 'layout',
              width: '60%',
              height: '50%',
              position: 'absolute',
              left: 31,
              top: 21,
            }}
            data-uid='thing'
            data-testid='thing'
            data-label='the Thing'
          />`),
          ),
          'await-first-dom-report',
        )

        const dragMeElementPath = EP.fromString(
          `${BakedInStoryboardUID}/${TestSceneUID}/${TestAppUID}:root/container/thing`,
        )

        const targetElementPath = EP.fromString(
          `${BakedInStoryboardUID}/${TestSceneUID}/${TestAppUID}:root/flex/aaa`,
        )

        await doBasicDrag(editor, dragMeElementPath, targetElementPath)

        expect(editor.getEditorState().derived.navigatorTargets.map(navigatorEntryToKey)).toEqual([
          'regular-utopia-storyboard-uid/scene-aaa',
          'regular-utopia-storyboard-uid/scene-aaa/app-entity',
          'regular-utopia-storyboard-uid/scene-aaa/app-entity:root',
          'regular-utopia-storyboard-uid/scene-aaa/app-entity:root/flex',
          'regular-utopia-storyboard-uid/scene-aaa/app-entity:root/flex/aaa',
          'regular-utopia-storyboard-uid/scene-aaa/app-entity:root/flex/thing', // <- thing is moved into the flex container
          'regular-utopia-storyboard-uid/scene-aaa/app-entity:root/flex/aab',
          'regular-utopia-storyboard-uid/scene-aaa/app-entity:root/flex/fle',
          'regular-utopia-storyboard-uid/scene-aaa/app-entity:root/container',
        ])

        const element = editor.renderedDOM.getByTestId('thing')

        expect(element.style.position).toEqual('')
        expect(element.style.top).toEqual('')
        expect(element.style.left).toEqual('')
        expect(element.style.bottom).toEqual('')
        expect(element.style.right).toEqual('')
        expect(element.style.width).toEqual('217px')
        expect(element.style.height).toEqual('117.5px')
      })

      it('reparenting from a flex container to the canvas', async () => {
        const editor = await renderTestEditorWithCode(
          projectWithFlexContainerAndCanvas,
          'await-first-dom-report',
        )

        const dragMeElementPath = EP.fromString('sb/scene/app:root/container/flex-child')
        const targetElementPath = EP.fromString('sb/dragme')

        const { dragMeElement, startingDragMeElementStyle } = await doBasicDrag(
          editor,
          dragMeElementPath,
          targetElementPath,
        )

        expect(editor.getEditorState().derived.navigatorTargets.map(navigatorEntryToKey)).toEqual([
          'regular-sb/scene',
          'regular-sb/scene/app',
          'regular-sb/scene/app:root',
          'regular-sb/scene/app:root/container',
          'regular-sb/scene/app:root/container/663',
          'regular-sb/scene/app:root/abs-container',
          'regular-sb/scene/app:root/abs-container/abs-child',
          'regular-sb/scene/app:root/abs-container/8ae',
          'regular-sb/dragme',
          'regular-sb/flex-child', // <- flex-child moved here
        ])

        expect(dragMeElement.style.position).toEqual('')
        expect(dragMeElement.style.top).toEqual('')
        expect(dragMeElement.style.left).toEqual('')
        expect(dragMeElement.style.bottom).toEqual('')
        expect(dragMeElement.style.right).toEqual('')
        expect(dragMeElement.style.width).toEqual(startingDragMeElementStyle.width)
        expect(dragMeElement.style.height).toEqual(startingDragMeElementStyle.height)
      })

      it('reparenting from an absolute container to a flex container', async () => {
        const editor = await renderTestEditorWithCode(
          projectWithFlexContainerAndCanvas,
          'await-first-dom-report',
        )

        const dragMeElementPath = EP.fromString('sb/scene/app:root/abs-container/abs-child')
        const targetElementPath = EP.fromString('sb/scene/app:root/container/flex-child')

        const { dragMeElement, startingDragMeElementStyle } = await doBasicDrag(
          editor,
          dragMeElementPath,
          targetElementPath,
        )

        expect(editor.getEditorState().derived.navigatorTargets.map(navigatorEntryToKey)).toEqual([
          'regular-sb/scene',
          'regular-sb/scene/app',
          'regular-sb/scene/app:root',
          'regular-sb/scene/app:root/container',
          'regular-sb/scene/app:root/container/flex-child',
          'regular-sb/scene/app:root/container/abs-child', // <- abs-child moved here
          'regular-sb/scene/app:root/container/663',
          'regular-sb/scene/app:root/abs-container',
          'regular-sb/scene/app:root/abs-container/8ae',
          'regular-sb/dragme',
        ])

        expect(dragMeElement.style.position).toEqual('')
        expect(dragMeElement.style.top).toEqual('')
        expect(dragMeElement.style.left).toEqual('')
        expect(dragMeElement.style.bottom).toEqual('')
        expect(dragMeElement.style.right).toEqual('')
        expect(dragMeElement.style.width).toEqual(startingDragMeElementStyle.width)
        expect(dragMeElement.style.height).toEqual(startingDragMeElementStyle.height)
      })

      it('reparenting from a flex container to an absolute container', async () => {
        const editor = await renderTestEditorWithCode(
          projectWithFlexContainerAndCanvas,
          'await-first-dom-report',
        )

        const dragMeElementPath = EP.fromString('sb/scene/app:root/container/flex-child')
        const targetElementPath = EP.fromString('sb/scene/app:root/abs-container/abs-child')
        const { dragMeElement, startingDragMeElementStyle } = await doBasicDrag(
          editor,
          dragMeElementPath,
          targetElementPath,
        )

        expect(editor.getEditorState().derived.navigatorTargets.map(navigatorEntryToKey)).toEqual([
          'regular-sb/scene',
          'regular-sb/scene/app',
          'regular-sb/scene/app:root',
          'regular-sb/scene/app:root/container',
          'regular-sb/scene/app:root/container/663',
          'regular-sb/scene/app:root/abs-container',
          'regular-sb/scene/app:root/abs-container/abs-child',
          'regular-sb/scene/app:root/abs-container/flex-child', // <- flex-child moved here
          'regular-sb/scene/app:root/abs-container/8ae',
          'regular-sb/dragme',
        ])

        expect(dragMeElement.style.position).toEqual('')
        expect(dragMeElement.style.top).toEqual('')
        expect(dragMeElement.style.left).toEqual('')
        expect(dragMeElement.style.bottom).toEqual('')
        expect(dragMeElement.style.right).toEqual('')
        expect(dragMeElement.style.width).toEqual(startingDragMeElementStyle.width)
        expect(dragMeElement.style.height).toEqual(startingDragMeElementStyle.height)
      })

      it('reparenting between flex containers hardcodes width/height', async () => {
        const editor = await renderTestEditorWithCode(
          makeTestProjectCodeWithSnippet(`
        <div
        style={{
          height: '100%',
          width: '100%',
          contain: 'layout',
        }}
        data-uid='root'
      >
        <div
          style={{
            backgroundColor: '#aaaaaa33',
            position: 'absolute',
            left: 69,
            top: 65,
            width: 383,
            height: 579,
            display: 'flex',
            flexDirection: 'row',
            gap: 29,
            padding: '18px 27px',
            alignItems: 'flex-end',
          }}
          data-uid='flex1'
        >
          <div
            style={{
              backgroundColor: '#aaaaaa33',
              height: '100%',
              contain: 'layout',
              flexGrow: 1,
            }}
            data-uid='flexchild1'
            data-testid='flexchild1'
          />
          <div
            style={{
              backgroundColor: '#aaaaaa33',
              height: '100%',
              contain: 'layout',
              flexGrow: 1,
            }}
            data-uid='c8e'
          />
        </div>
        <div
          style={{
            backgroundColor: '#aaaaaa33',
            position: 'absolute',
            left: 592,
            top: 40,
            display: 'flex',
            flexDirection: 'column',
            gap: 29,
            padding: '18px 27px',
            alignItems: 'flex-end',
            width: 195,
            height: 771,
          }}
          data-uid='flex2'
        >
          <div
            style={{
              backgroundColor: '#aaaaaa33',
              contain: 'layout',
              width: 141,
              height: 353,
            }}
            data-uid='flexchild2'
          />
          <div
            style={{
              backgroundColor: '#aaaaaa33',
              contain: 'layout',
              width: '100%',
              flexGrow: 1,
            }}
            data-uid='aaf'
          />
        </div>
      </div>
        `),
          'await-first-dom-report',
        )

        const dragMeElementPath = EP.fromString(
          `${BakedInStoryboardUID}/${TestSceneUID}/${TestAppUID}:root/flex1/flexchild1`,
        )

        const targetElementPath = EP.fromString(
          `${BakedInStoryboardUID}/${TestSceneUID}/${TestAppUID}:root/flex2/flexchild2`,
        )

        await doBasicDrag(editor, dragMeElementPath, targetElementPath)

        expect(editor.getEditorState().derived.navigatorTargets.map(navigatorEntryToKey)).toEqual([
          'regular-utopia-storyboard-uid/scene-aaa',
          'regular-utopia-storyboard-uid/scene-aaa/app-entity',
          'regular-utopia-storyboard-uid/scene-aaa/app-entity:root',
          'regular-utopia-storyboard-uid/scene-aaa/app-entity:root/flex1',
          'regular-utopia-storyboard-uid/scene-aaa/app-entity:root/flex1/c8e',
          'regular-utopia-storyboard-uid/scene-aaa/app-entity:root/flex2',
          'regular-utopia-storyboard-uid/scene-aaa/app-entity:root/flex2/flexchild2',
          'regular-utopia-storyboard-uid/scene-aaa/app-entity:root/flex2/flexchild1',
          'regular-utopia-storyboard-uid/scene-aaa/app-entity:root/flex2/aaf',
        ])

        const element = editor.renderedDOM.getByTestId('flexchild1')

        expect(element.style.position).toEqual('')
        expect(element.style.top).toEqual('')
        expect(element.style.left).toEqual('')
        expect(element.style.bottom).toEqual('')
        expect(element.style.right).toEqual('')
        expect(element.style.width).toEqual('150px')
        expect(element.style.height).toEqual('543px')
      })
      it('reparenting a fragment with child elements to flex updates children position and size', async () => {
        const editor = await renderTestEditorWithCode(
          makeTestProjectCodeWithSnippet(
            flexLayoutSnippet(`
            <React.Fragment data-uid='fragment'>
              <div
                style={{
                  backgroundColor: '#35a853',
                  contain: 'layout',
                  width: '60%',
                  height: '50%',
                  position: 'absolute',
                  left: 31,
                  top: 21,
                }}
                data-uid='child1'
                data-testid='child1'
              />
              <div
                style={{
                  backgroundColor: '#35a853',
                  contain: 'layout',
                  width: 150,
                  height: 100,
                  position: 'absolute',
                  bottom: 50,
                  right: 40,
                }}
                data-uid='child2'
                data-testid='child2'
              />
            </React.Fragment>`),
          ),
          'await-first-dom-report',
        )

        const dragMeElementPath = EP.fromString(
          `${BakedInStoryboardUID}/${TestSceneUID}/${TestAppUID}:root/container/fragment`,
        )

        const targetElementPath = EP.fromString(
          `${BakedInStoryboardUID}/${TestSceneUID}/${TestAppUID}:root/flex/aaa`,
        )

        await doBasicDrag(editor, dragMeElementPath, targetElementPath)

        expect(editor.getEditorState().derived.navigatorTargets.map(navigatorEntryToKey)).toEqual([
          'regular-utopia-storyboard-uid/scene-aaa',
          'regular-utopia-storyboard-uid/scene-aaa/app-entity',
          'regular-utopia-storyboard-uid/scene-aaa/app-entity:root',
          'regular-utopia-storyboard-uid/scene-aaa/app-entity:root/flex',
          'regular-utopia-storyboard-uid/scene-aaa/app-entity:root/flex/aaa',
          'regular-utopia-storyboard-uid/scene-aaa/app-entity:root/flex/fragment', // <- fragment is moved into the flex container
          'regular-utopia-storyboard-uid/scene-aaa/app-entity:root/flex/fragment/child1', // <- fragment child is moved into the flex container
          'regular-utopia-storyboard-uid/scene-aaa/app-entity:root/flex/fragment/child2', // <- fragment child is moved into the flex container
          'regular-utopia-storyboard-uid/scene-aaa/app-entity:root/flex/aab',
          'regular-utopia-storyboard-uid/scene-aaa/app-entity:root/flex/fle',
          'regular-utopia-storyboard-uid/scene-aaa/app-entity:root/container',
        ])

        const child1 = editor.renderedDOM.getByTestId('child1')

        expect(child1.style.position).toEqual('')
        expect(child1.style.top).toEqual('')
        expect(child1.style.left).toEqual('')
        expect(child1.style.bottom).toEqual('')
        expect(child1.style.right).toEqual('')
        expect(child1.style.width).toEqual('217px')
        expect(child1.style.height).toEqual('117.5px')

        const child2 = editor.renderedDOM.getByTestId('child2')

        expect(child2.style.position).toEqual('')
        expect(child2.style.top).toEqual('')
        expect(child2.style.left).toEqual('')
        expect(child2.style.bottom).toEqual('')
        expect(child2.style.right).toEqual('')
        expect(child2.style.width).toEqual('150px')
        expect(child2.style.height).toEqual('100px')
      })
    })

    describe('reparenting to absolute', () => {
      const absoluteSnippet = (insides: string) =>
        makeTestProjectCodeWithSnippet(`
      <div
      style={{
        height: '100%',
        width: '100%',
        contain: 'layout',
      }}
      data-uid='root'
    >
        <div
          style={{
            backgroundColor: '#aaaaaa33',
            position: 'absolute',
            left: 103,
            top: 104,
            width: 364,
            height: 216,
          }}
          data-uid='new-container'
        />
        ${insides}
      </div>
      `)

      it('reparenting an element positioned with pins to an absolute container', async () => {
        const editor = await renderTestEditorWithCode(
          absoluteSnippet(`
          <div
            style={{
              backgroundColor: '#aaaaaa33',
              position: 'absolute',
              left: 123,
              top: 379,
              width: 176,
              height: 183,
            }}
            data-uid='old-container'
          >
            <div
              style={{
                backgroundColor: '#aaaaaa33',
                position: 'absolute',
                left: 24,
                top: 54,
                right: 57,
                bottom: 26,
              }}
              data-uid='dragme'
              data-testid='dragme'
            />
          </div>
        `),
          'await-first-dom-report',
        )

        const dragMeElementPath = EP.fromString(
          `${BakedInStoryboardUID}/${TestSceneUID}/${TestAppUID}:root/old-container/dragme`,
        )

        const targetElementPath = EP.fromString(
          `${BakedInStoryboardUID}/${TestSceneUID}/${TestAppUID}:root/new-container`,
        )

        await doBasicDrag(editor, dragMeElementPath, targetElementPath, ReparentDropTargetTestId)

        expect(editor.getEditorState().derived.navigatorTargets.map(navigatorEntryToKey)).toEqual([
          'regular-utopia-storyboard-uid/scene-aaa',
          'regular-utopia-storyboard-uid/scene-aaa/app-entity',
          'regular-utopia-storyboard-uid/scene-aaa/app-entity:root',
          'regular-utopia-storyboard-uid/scene-aaa/app-entity:root/new-container',
          'regular-utopia-storyboard-uid/scene-aaa/app-entity:root/new-container/dragme',
          'regular-utopia-storyboard-uid/scene-aaa/app-entity:root/old-container',
        ])

        const element = editor.renderedDOM.getByTestId('dragme')
        const { position, top, left, bottom, right, width, height } = element.style

        expect({ position, top, left, bottom, right, width, height }).toEqual({
          bottom: '',
          right: '',
          height: '103px',
          left: '44px',
          position: 'absolute',
          top: '57px',
          width: '95px',
        })
      })

      it('reparenting an element sized with % to an absolute container', async () => {
        const editor = await renderTestEditorWithCode(
          absoluteSnippet(`
          <div
            style={{
              backgroundColor: '#aaaaaa33',
              position: 'absolute',
              left: 123,
              top: 379,
              width: 176,
              height: 183,
            }}
            data-uid='old-container'
          >
            <div
              style={{
                backgroundColor: '#aaaaaa33',
                position: 'absolute',
                left: 24,
                top: 54,
                width: '44%',
                height: '51%',
              }}
              data-uid='dragme'
              data-testid='dragme'
            />
          </div>
        `),
          'await-first-dom-report',
        )

        const dragMeElementPath = EP.fromString(
          `${BakedInStoryboardUID}/${TestSceneUID}/${TestAppUID}:root/old-container/dragme`,
        )

        const targetElementPath = EP.fromString(
          `${BakedInStoryboardUID}/${TestSceneUID}/${TestAppUID}:root/new-container`,
        )

        await doBasicDrag(editor, dragMeElementPath, targetElementPath, ReparentDropTargetTestId)

        expect(editor.getEditorState().derived.navigatorTargets.map(navigatorEntryToKey)).toEqual([
          'regular-utopia-storyboard-uid/scene-aaa',
          'regular-utopia-storyboard-uid/scene-aaa/app-entity',
          'regular-utopia-storyboard-uid/scene-aaa/app-entity:root',
          'regular-utopia-storyboard-uid/scene-aaa/app-entity:root/new-container',
          'regular-utopia-storyboard-uid/scene-aaa/app-entity:root/new-container/dragme',
          'regular-utopia-storyboard-uid/scene-aaa/app-entity:root/old-container',
        ])

        const element = editor.renderedDOM.getByTestId('dragme')
        const { position, top, left, bottom, right, width, height } = element.style

        expect({ position, top, left, bottom, right, width, height }).toEqual({
          bottom: '',
          right: '',
          height: '93.5px',
          left: '44px',
          position: 'absolute',
          top: '61px',
          width: '77.5px',
        })
      })

      it('reparenting an element slightly to the bottom to an absolute container', async () => {
        const editor = await renderTestEditorWithCode(
          absoluteSnippet(`
          <div
            style={{
              backgroundColor: '#aaaaaa33',
              position: 'absolute',
              left: 151,
              top: 379.5,
              width: 77,
              height: 93,
            }}
            data-uid='dragme'
            data-testid='dragme'
          />
        `),
          'await-first-dom-report',
        )

        const dragMeElementPath = EP.fromString(
          `${BakedInStoryboardUID}/${TestSceneUID}/${TestAppUID}:root/dragme`,
        )

        const targetElementPath = EP.fromString(
          `${BakedInStoryboardUID}/${TestSceneUID}/${TestAppUID}:root/new-container`,
        )

        await doBasicDrag(editor, dragMeElementPath, targetElementPath, ReparentDropTargetTestId)

        expect(editor.getEditorState().derived.navigatorTargets.map(navigatorEntryToKey)).toEqual([
          'regular-utopia-storyboard-uid/scene-aaa',
          'regular-utopia-storyboard-uid/scene-aaa/app-entity',
          'regular-utopia-storyboard-uid/scene-aaa/app-entity:root',
          'regular-utopia-storyboard-uid/scene-aaa/app-entity:root/new-container',
          'regular-utopia-storyboard-uid/scene-aaa/app-entity:root/new-container/dragme',
        ])

        const element = editor.renderedDOM.getByTestId('dragme')
        const { position, top, left, bottom, right, width, height } = element.style

        expect({ position, top, left, bottom, right, width, height }).toEqual({
          bottom: '',
          right: '',
          position: 'absolute',
          height: '93px',
          left: '48px',
          top: '62px',
          width: '77px',
        })
      })

      it('reparenting an element slightly to the right to an absolute container', async () => {
        const editor = await renderTestEditorWithCode(
          absoluteSnippet(`
          <div
            style={{
              backgroundColor: '#aaaaaa33',
              position: 'absolute',
              left: 504,
              top: 142,
              width: 77,
              height: 93,
            }}
            data-uid='dragme'
            data-testid='dragme'
          />
        `),
          'await-first-dom-report',
        )

        const dragMeElementPath = EP.fromString(
          `${BakedInStoryboardUID}/${TestSceneUID}/${TestAppUID}:root/dragme`,
        )

        const targetElementPath = EP.fromString(
          `${BakedInStoryboardUID}/${TestSceneUID}/${TestAppUID}:root/new-container`,
        )

        await doBasicDrag(editor, dragMeElementPath, targetElementPath, ReparentDropTargetTestId)

        expect(editor.getEditorState().derived.navigatorTargets.map(navigatorEntryToKey)).toEqual([
          'regular-utopia-storyboard-uid/scene-aaa',
          'regular-utopia-storyboard-uid/scene-aaa/app-entity',
          'regular-utopia-storyboard-uid/scene-aaa/app-entity:root',
          'regular-utopia-storyboard-uid/scene-aaa/app-entity:root/new-container',
          'regular-utopia-storyboard-uid/scene-aaa/app-entity:root/new-container/dragme',
        ])

        const element = editor.renderedDOM.getByTestId('dragme')
        const { position, top, left, bottom, right, width, height } = element.style

        expect({ position, top, left, bottom, right, width, height }).toEqual({
          bottom: '',
          right: '',
          position: 'absolute',
          height: '93px',
          left: '144px',
          top: '38px',
          width: '77px',
        })
      })

      it('reparenting an element that is out of bounds to an absolute container', async () => {
        const editor = await renderTestEditorWithCode(
          absoluteSnippet(`
          <div
            style={{
              backgroundColor: '#aaaaaa33',
              position: 'absolute',
              left: 143,
              top: 61,
              width: 77,
              height: 93,
            }}
            data-uid='dragme'
            data-testid='dragme'
          />
        `),
          'await-first-dom-report',
        )

        const dragMeElementPath = EP.fromString(
          `${BakedInStoryboardUID}/${TestSceneUID}/${TestAppUID}:root/dragme`,
        )

        const targetElementPath = EP.fromString(
          `${BakedInStoryboardUID}/${TestSceneUID}/${TestAppUID}:root/new-container`,
        )

        await doBasicDrag(editor, dragMeElementPath, targetElementPath, ReparentDropTargetTestId)

        expect(editor.getEditorState().derived.navigatorTargets.map(navigatorEntryToKey)).toEqual([
          'regular-utopia-storyboard-uid/scene-aaa',
          'regular-utopia-storyboard-uid/scene-aaa/app-entity',
          'regular-utopia-storyboard-uid/scene-aaa/app-entity:root',
          'regular-utopia-storyboard-uid/scene-aaa/app-entity:root/new-container',
          'regular-utopia-storyboard-uid/scene-aaa/app-entity:root/new-container/dragme',
        ])

        const element = editor.renderedDOM.getByTestId('dragme')
        const { position, top, left, bottom, right, width, height } = element.style

        expect({ position, top, left, bottom, right, width, height }).toEqual({
          bottom: '',
          height: '93px',
          left: '40px',
          position: 'absolute',
          right: '',
          top: '62px',
          width: '77px',
        })
      })

      it('reparenting an element to an absolute container that has children with z-index set', async () => {
        const editor = await renderTestEditorWithCode(
          makeTestProjectCodeWithSnippet(`
          <div
            style={{
              height: '100%',
              width: '100%',
              contain: 'layout',
            }}
            data-uid='root'
          >
            <div
              style={{
                backgroundColor: '#aaaaaa33',
                position: 'absolute',
                left: 103,
                top: 103,
                width: 364,
                height: 216,
              }}
              data-uid='new-container'
            >
              <div
                style={{
                  backgroundColor: '#0075ff',
                  position: 'absolute',
                  left: 97,
                  top: 66,
                  width: 164,
                  height: 111,
                  zIndex: 1,
                }}
                data-uid='child-with-z-index'
              />
            </div>
            <div
              style={{
                backgroundColor: '#ff4400',
                position: 'absolute',
                left: 488.5,
                top: 339.5,
                width: 77,
                height: 93,
              }}
              data-uid='dragme'
              data-testid='dragme'
            />
          </div>
        `),
          'await-first-dom-report',
        )

        const dragMeElementPath = EP.fromString(
          `${BakedInStoryboardUID}/${TestSceneUID}/${TestAppUID}:root/dragme`,
        )

        const targetElementPath = EP.fromString(
          `${BakedInStoryboardUID}/${TestSceneUID}/${TestAppUID}:root/new-container`,
        )

        await doBasicDrag(editor, dragMeElementPath, targetElementPath, ReparentDropTargetTestId)

        expect(editor.getEditorState().derived.navigatorTargets.map(navigatorEntryToKey)).toEqual([
          'regular-utopia-storyboard-uid/scene-aaa',
          'regular-utopia-storyboard-uid/scene-aaa/app-entity',
          'regular-utopia-storyboard-uid/scene-aaa/app-entity:root',
          'regular-utopia-storyboard-uid/scene-aaa/app-entity:root/new-container',
          'regular-utopia-storyboard-uid/scene-aaa/app-entity:root/new-container/child-with-z-index',
          'regular-utopia-storyboard-uid/scene-aaa/app-entity:root/new-container/dragme',
        ])

        const element = editor.renderedDOM.getByTestId('dragme')
        const { position, top, left, bottom, right, width, height, zIndex } = element.style

        expect({ position, top, left, bottom, right, width, height, zIndex }).toEqual({
          bottom: '',
          height: '93px',
          left: '144px',
          position: 'absolute',
          right: '',
          top: '62px',
          width: '77px',
          zIndex: '1',
        })
      })

      it('reparenting multiple absolute elements to an absolute container', async () => {
        const editor = await renderTestEditorWithCode(
          absoluteSnippet(`
          <div
            style={{
              backgroundColor: '#aaaaaa33',
              position: 'absolute',
              left: 123,
              top: 379,
              width: 176,
              height: 183,
            }}
            data-uid='old-container'
          >
            <div
              style={{
                backgroundColor: '#aaaaaa33',
                position: 'absolute',
                left: 14,
                top: 74,
                right: 68,
                bottom: 8,
              }}
              data-uid='dragme1'
              data-testid='dragme1'
            />
            <div data-uid='wrapper'>
              <div
                style={{
                  backgroundColor: '#aaaaaa33',
                  position: 'absolute',
                  left: 60,
                  top: 22,
                  width: 96,
                  height: 104,
                }}
                data-uid='dragme2'
                data-testid='dragme2'
              />
            </div>
          </div>
        `),
          'await-first-dom-report',
        )

        const dragMeElementPaths = [
          EP.fromString(
            `${BakedInStoryboardUID}/${TestSceneUID}/${TestAppUID}:root/old-container/dragme1`,
          ),
          EP.fromString(
            `${BakedInStoryboardUID}/${TestSceneUID}/${TestAppUID}:root/old-container/wrapper/dragme2`,
          ),
        ]

        const targetElementPath = EP.fromString(
          `${BakedInStoryboardUID}/${TestSceneUID}/${TestAppUID}:root/new-container`,
        )

        await doBasicDragMultiselect(
          editor,
          dragMeElementPaths,
          targetElementPath,
          ReparentDropTargetTestId,
        )

        expect(editor.getEditorState().derived.navigatorTargets.map(navigatorEntryToKey)).toEqual([
          'regular-utopia-storyboard-uid/scene-aaa',
          'regular-utopia-storyboard-uid/scene-aaa/app-entity',
          'regular-utopia-storyboard-uid/scene-aaa/app-entity:root',
          'regular-utopia-storyboard-uid/scene-aaa/app-entity:root/new-container',
          'regular-utopia-storyboard-uid/scene-aaa/app-entity:root/new-container/dragme1',
          'regular-utopia-storyboard-uid/scene-aaa/app-entity:root/new-container/dragme2',
          'regular-utopia-storyboard-uid/scene-aaa/app-entity:root/old-container',
          'regular-utopia-storyboard-uid/scene-aaa/app-entity:root/old-container/wrapper',
        ])
        expect(
          editor.getEditorState().editor.jsxMetadata[
            'utopia-storyboard-uid/scene-aaa/app-entity:root/new-container/dragme1'
          ].globalFrame,
        ).toEqual({ height: 101, width: 94, x: 137, y: 188 })
        expect(
          editor.getEditorState().editor.jsxMetadata[
            'utopia-storyboard-uid/scene-aaa/app-entity:root/new-container/dragme2'
          ].globalFrame,
        ).toEqual({ height: 104, width: 96, x: 183, y: 136 })
      })
      it('reparenting absolute elements to the storyboard keeps visible position', async () => {
        const renderResult = await renderTestEditorWithCode(
          absoluteSnippet(`
          <div
            style={{
              backgroundColor: '#aaaaaa33',
              position: 'absolute',
              left: 123,
              top: 379,
              width: 176,
              height: 183,
            }}
            data-uid='old-container'
          >
            <div
              style={{
                backgroundColor: '#aaaaaa33',
                position: 'absolute',
                left: 14,
                top: 74,
                right: 68,
                bottom: 8,
              }}
              data-uid='dragme1'
              data-testid='dragme1'
            />
            <div data-uid='wrapper'>
              <div
                style={{
                  backgroundColor: '#aaaaaa33',
                  position: 'absolute',
                  left: 60,
                  top: 22,
                  width: 96,
                  height: 104,
                }}
                data-uid='dragme2'
                data-testid='dragme2'
              />
            </div>
          </div>
        `),
          'await-first-dom-report',
        )

        const target1 = EP.fromString(
          `${BakedInStoryboardUID}/${TestSceneUID}/${TestAppUID}:root/old-container/dragme1`,
        )
        const target2 = EP.fromString(
          `${BakedInStoryboardUID}/${TestSceneUID}/${TestAppUID}:root/old-container/wrapper/dragme2`,
        )
        const dragMeElementPaths = [target2, target1]

        const originalFrame1 = MetadataUtils.getFrameOrZeroRectInCanvasCoords(
          target1,
          renderResult.getEditorState().editor.jsxMetadata,
        )
        const originalFrame2 = MetadataUtils.getFrameOrZeroRectInCanvasCoords(
          target2,
          renderResult.getEditorState().editor.jsxMetadata,
        )

        const targetElementPath = EP.fromString(
          `${BakedInStoryboardUID}/${TestSceneUID}/${TestAppUID}:root/old-container/wrapper/dragme2`,
        )
        await doBasicDragMultiselect(
          renderResult,
          dragMeElementPaths,
          targetElementPath,
          BottomDropTargetLineTestId,
          windowPoint({ x: -WiggleUnit * 6, y: 10 }),
        )

        expect(
          renderResult.getEditorState().derived.navigatorTargets.map(navigatorEntryToKey),
        ).toEqual([
          'regular-utopia-storyboard-uid/scene-aaa',
          'regular-utopia-storyboard-uid/scene-aaa/app-entity',
          'regular-utopia-storyboard-uid/scene-aaa/app-entity:root',
          'regular-utopia-storyboard-uid/scene-aaa/app-entity:root/new-container',
          'regular-utopia-storyboard-uid/scene-aaa/app-entity:root/old-container',
          'regular-utopia-storyboard-uid/scene-aaa/app-entity:root/old-container/wrapper',
          'regular-utopia-storyboard-uid/dragme2',
          'regular-utopia-storyboard-uid/dragme1',
        ])
        expect(
          MetadataUtils.getFrameOrZeroRectInCanvasCoords(
            EP.fromString('utopia-storyboard-uid/dragme1'),
            renderResult.getEditorState().editor.jsxMetadata,
          ),
        ).toEqual(originalFrame1)
        expect(
          MetadataUtils.getFrameOrZeroRectInCanvasCoords(
            EP.fromString('utopia-storyboard-uid/dragme2'),
            renderResult.getEditorState().editor.jsxMetadata,
          ),
        ).toEqual(originalFrame2)
      })
      it('reparenting a fragment with absolute children to an absolute container updates children position', async () => {
        const editor = await renderTestEditorWithCode(
          absoluteSnippet(`
          <React.Fragment data-uid='fragment'>
            <div
              style={{
                width: '20%',
                height: '15%',
                position: 'absolute',
                left: 31,
                top: 21,
              }}
              data-uid='child1'
              data-testid='child1'
            />
            <div
              style={{
                top: 10,
                left: 10,
                width: 50,
                height: 60,
                position: 'absolute',
              }}
              data-uid='child2'
              data-testid='child2'
            />
          </React.Fragment>
        `),
          'await-first-dom-report',
        )

        const dragMeElementPath = EP.fromString(
          `${BakedInStoryboardUID}/${TestSceneUID}/${TestAppUID}:root/fragment`,
        )

        const targetElementPath = EP.fromString(
          `${BakedInStoryboardUID}/${TestSceneUID}/${TestAppUID}:root/new-container`,
        )

        await doBasicDrag(editor, dragMeElementPath, targetElementPath, ReparentDropTargetTestId)

        expect(editor.getEditorState().derived.navigatorTargets.map(navigatorEntryToKey)).toEqual([
          'regular-utopia-storyboard-uid/scene-aaa',
          'regular-utopia-storyboard-uid/scene-aaa/app-entity',
          'regular-utopia-storyboard-uid/scene-aaa/app-entity:root',
          'regular-utopia-storyboard-uid/scene-aaa/app-entity:root/new-container',
          'regular-utopia-storyboard-uid/scene-aaa/app-entity:root/new-container/fragment',
          'regular-utopia-storyboard-uid/scene-aaa/app-entity:root/new-container/fragment/child1',
          'regular-utopia-storyboard-uid/scene-aaa/app-entity:root/new-container/fragment/child2',
        ])

        const child1 = editor.renderedDOM.getByTestId('child1')
        expect(child1.style.position).toEqual('absolute')
        expect(child1.style.top).toEqual('84px')
        expect(child1.style.left).toEqual('153px')
        expect(child1.style.bottom).toEqual('')
        expect(child1.style.right).toEqual('')
        expect(child1.style.width).toEqual('80px')
        expect(child1.style.height).toEqual('60px')

        const child2 = editor.renderedDOM.getByTestId('child2')
        expect(child2.style.position).toEqual('absolute')
        expect(child2.style.top).toEqual('73px')
        expect(child2.style.left).toEqual('132px')
        expect(child2.style.bottom).toEqual('')
        expect(child2.style.right).toEqual('')
        expect(child2.style.width).toEqual('50px')
        expect(child2.style.height).toEqual('60px')
      })
    })
  })

  describe('derived data', () => {
    it('element warnings', async () => {
      const editor = await renderTestEditorWithCode(
        projectWithGroupsAndNotGroups,
        'await-first-dom-report',
      )
      await selectComponentsForTest(editor, [EP.fromString('sb/group/groupchild')])

      const { elementWarnings } = editor.getEditorState().derived

      expect(elementWarnings['sb/group/groupchild'].absoluteWithUnpositionedParent).toEqual(false)
      expect(elementWarnings['sb/fragment/fragmentchild'].absoluteWithUnpositionedParent).toEqual(
        false,
      )
      expect(elementWarnings['sb/offsetparent/offsetchild'].absoluteWithUnpositionedParent).toEqual(
        false,
      )
      expect(
        elementWarnings['sb/nonoffsetparent/nonoffsetchild'].absoluteWithUnpositionedParent,
      ).toEqual(true)
    })
  })

  describe('reparenting to fragment-like elements', () => {
    it('reparenting into fragment reparents to the correct index', async () => {
      const renderResult = await renderTestEditorWithCode(
        projectWithGroupsAndNotGroups,
        'await-first-dom-report',
      )

      const dragMeElement = await renderResult.renderedDOM.findByTestId(
        `navigator-item-regular_sb/offsetparent/offsetchild`,
      )
      const dragMeElementRect = dragMeElement.getBoundingClientRect()
      const dragMeElementCenter = getDomRectCenter(dragMeElementRect)
      const dragToElement = await renderResult.renderedDOM.findByTestId(
        `navigator-item-regular_sb/fragment/fragmentchild`,
      )
      const dragToElementRect = dragToElement.getBoundingClientRect()
      const dragToElementRectCenter = getDomRectCenter(dragToElementRect)
      const dragTo = {
        x: dragToElementRectCenter.x,
        y: dragToElementRect.y + 3,
      }

      const dragDelta = windowPoint({
        x: dragTo.x - dragMeElementCenter.x,
        y: dragTo.y - dragMeElementCenter.y,
      })

      await selectComponentsForTest(renderResult, [EP.fromString('sb/offsetparent/offsetchild')])

      await act(async () =>
        dragElementWithDNDEvents(
          renderResult,
          `navigator-item-drag-regular_sb/offsetparent/offsetchild`,
          `navigator-item-drop-before-regular_sb/fragment/fragmentchild`,
          windowPoint(dragMeElementCenter),
          dragDelta,
        ),
      )

      expect(
        renderResult.getEditorState().derived.navigatorTargets.map(navigatorEntryToKey),
      ).toEqual([
        'regular-sb/group',
        'regular-sb/group/groupchild',
        'regular-sb/fragment',
        'regular-sb/fragment/offsetchild', // <- offsetchild is moved to under fragment from offsetparent
        'regular-sb/fragment/fragmentchild',
        'regular-sb/offsetparent',
        'regular-sb/nonoffsetparent',
        'regular-sb/nonoffsetparent/nonoffsetchild',
      ])
    })

    it('reparenting into sizeless div reparents to the right index', async () => {
      const renderResult = await renderTestEditorWithCode(
        projectWithGroupsAndNotGroups,
        'await-first-dom-report',
      )

      const dragMeElement = await renderResult.renderedDOM.findByTestId(
        `navigator-item-regular_sb/offsetparent/offsetchild`,
      )
      const dragMeElementRect = dragMeElement.getBoundingClientRect()
      const dragMeElementCenter = getDomRectCenter(dragMeElementRect)
      const dragToElement = await renderResult.renderedDOM.findByTestId(
        `navigator-item-regular_sb/nonoffsetparent/nonoffsetchild`,
      )
      const dragToElementRect = dragToElement.getBoundingClientRect()
      const dragToElementRectCenter = getDomRectCenter(dragToElementRect)
      const dragTo = {
        x: dragToElementRectCenter.x,
        y: dragToElementRect.y + 3,
      }

      const dragDelta = windowPoint({
        x: dragTo.x - dragMeElementCenter.x,
        y: dragTo.y - dragMeElementCenter.y,
      })

      await selectComponentsForTest(renderResult, [EP.fromString('sb/offsetparent/offsetchild')])

      await act(async () =>
        dragElementWithDNDEvents(
          renderResult,
          `navigator-item-drag-regular_sb/offsetparent/offsetchild`,
          `navigator-item-drop-before-regular_sb/nonoffsetparent/nonoffsetchild`,
          windowPoint(dragMeElementCenter),
          dragDelta,
        ),
      )

      expect(
        renderResult.getEditorState().derived.navigatorTargets.map(navigatorEntryToKey),
      ).toEqual([
        'regular-sb/group',
        'regular-sb/group/groupchild',
        'regular-sb/fragment',
        'regular-sb/fragment/fragmentchild',
        'regular-sb/offsetparent',
        'regular-sb/nonoffsetparent',
        'regular-sb/nonoffsetparent/offsetchild', // <- offsetchild is moved to under nonoffsetparent from offsetparent
        'regular-sb/nonoffsetparent/nonoffsetchild',
      ])
    })
  })

  describe('rename', () => {
    it('can rename entries', async () => {
      const renderResult = await renderTestEditorWithCode(
        makeTestProjectCodeWithSnippet(`
        <div data-uid='root'>
          <div data-uid='aaa'>hello</div>
          <div data-uid='bbb'>there</div>
        </div>
        `),
        'await-first-dom-report',
      )

      const target = EP.fromString(`utopia-storyboard-uid/scene-aaa/app-entity:root/aaa`)
      await act(async () => {
        await renderResult.dispatch([setNavigatorRenamingTarget(target)], true)
      })

      const input: HTMLInputElement = await screen.findByTestId('navigator-item-label-hello')
      await act(async () => {
        document.execCommand('insertText', false, 'aloha')
        fireEvent.blur(input)
      })

      await renderResult.getDispatchFollowUpActionsFinished()

      expect(
        renderResult.getEditorState().derived.navigatorTargets.map(navigatorEntryToKey),
      ).toEqual([
        'regular-utopia-storyboard-uid/scene-aaa',
        'regular-utopia-storyboard-uid/scene-aaa/app-entity',
        'regular-utopia-storyboard-uid/scene-aaa/app-entity:root',
        'regular-utopia-storyboard-uid/scene-aaa/app-entity:root/aaa',
        'regular-utopia-storyboard-uid/scene-aaa/app-entity:root/bbb',
      ])

      expect(getPrintedUiJsCode(renderResult.getEditorState())).toEqual(
        makeTestProjectCodeWithSnippet(`
          <div data-uid='root'>
            <div data-uid='aaa' data-label='aloha'>hello</div>
            <div data-uid='bbb'>there</div>
          </div>
        `),
      )
    })
  })

  describe('render props', () => {
    it('can insert an empty intrinsic element into render prop', async () => {
      const renderResult = await renderTestEditorWithModel(
        projectWithRenderProp(''), // <- no render prop
        'await-first-dom-report',
      )

      const slotElement = renderResult.renderedDOM.getByTestId(
        'toggle-render-prop-NavigatorItemTestId-slot_sb/scene/pg:dbc/78c/prop_label_header',
      )

      await mouseClickAtPoint(slotElement, { x: 3, y: 3 })

      const renderPropOptionElement = await waitFor(() =>
        renderResult.renderedDOM.getByText('(empty)'),
      )
      await mouseClickAtPoint(renderPropOptionElement, { x: 3, y: 3 })

      expect(
        renderResult.getEditorState().derived.navigatorTargets.map(navigatorEntryToKey),
      ).toEqual([
        'regular-sb/scene',
        'regular-sb/scene/pg',
        'regular-sb/scene/pg:dbc',
        'regular-sb/scene/pg:dbc/78c',
        'render-prop-sb/scene/pg:dbc/78c/prop-label-header-header',
        'render-prop-value-sb/scene/pg:dbc/78c/pro-header', // <- the inserted render prop
        'render-prop-sb/scene/pg:dbc/78c/prop-label-children-children',
        'regular-sb/scene/pg:dbc/78c/d93',
      ])
      expect(
        renderResult.getEditorState().derived.visibleNavigatorTargets.map(navigatorEntryToKey),
      ).toEqual([
        'regular-sb/scene',
        'regular-sb/scene/pg',
        'regular-sb/scene/pg:dbc',
        'regular-sb/scene/pg:dbc/78c',
        'render-prop-sb/scene/pg:dbc/78c/prop-label-header-header',
        'render-prop-value-sb/scene/pg:dbc/78c/pro-header', // <- the inserted render prop
        'render-prop-sb/scene/pg:dbc/78c/prop-label-children-children',
        'regular-sb/scene/pg:dbc/78c/d93',
      ])
    })
    it('can insert an intrinsic element into render prop', async () => {
      const renderResult = await renderTestEditorWithModel(
        projectWithRenderProp(''), // <- no render prop
        'await-first-dom-report',
      )

      const slotElement = renderResult.renderedDOM.getByTestId(
        'toggle-render-prop-NavigatorItemTestId-slot_sb/scene/pg:dbc/78c/prop_label_header',
      )

      await mouseClickAtPoint(slotElement, { x: 3, y: 3 })

      const renderPropOptionElement = await waitFor(() =>
        renderResult.renderedDOM.getByText('Span with Title'),
      )
      await mouseClickAtPoint(renderPropOptionElement, { x: 3, y: 3 })

      expect(
        renderResult.getEditorState().derived.navigatorTargets.map(navigatorEntryToKey),
      ).toEqual([
        'regular-sb/scene',
        'regular-sb/scene/pg',
        'regular-sb/scene/pg:dbc',
        'regular-sb/scene/pg:dbc/78c',
        'render-prop-sb/scene/pg:dbc/78c/prop-label-header-header',
        'render-prop-value-sb/scene/pg:dbc/78c/pro-header', // <- the inserted render prop
        'render-prop-sb/scene/pg:dbc/78c/prop-label-children-children',
        'regular-sb/scene/pg:dbc/78c/d93',
      ])
      expect(
        renderResult.getEditorState().derived.visibleNavigatorTargets.map(navigatorEntryToKey),
      ).toEqual([
        'regular-sb/scene',
        'regular-sb/scene/pg',
        'regular-sb/scene/pg:dbc',
        'regular-sb/scene/pg:dbc/78c',
        'render-prop-sb/scene/pg:dbc/78c/prop-label-header-header',
        'render-prop-value-sb/scene/pg:dbc/78c/pro-header', // <- the inserted render prop
        'render-prop-sb/scene/pg:dbc/78c/prop-label-children-children',
        'regular-sb/scene/pg:dbc/78c/d93',
      ])
    })
    it('can insert an third-party component into render prop', async () => {
      const renderResult = await renderTestEditorWithModel(
        projectWithThirdPartyRenderProp(''), // <- no render prop
        'await-first-dom-report',
      )

      const slotElement = renderResult.renderedDOM.getByTestId(
        'toggle-render-prop-NavigatorItemTestId-slot_sb/scene/pg:dbc/78c/prop_label_header',
      )

      await mouseClickAtPoint(slotElement, { x: 3, y: 3 })

      const renderPropOptionElement = await waitFor(() =>
        renderResult.renderedDOM.getByText('Heading with Title'),
      )
      await mouseClickAtPoint(renderPropOptionElement, { x: 3, y: 3 })

      expect(
        renderResult.getEditorState().derived.navigatorTargets.map(navigatorEntryToKey),
      ).toEqual([
        'regular-sb/scene',
        'regular-sb/scene/pg',
        'regular-sb/scene/pg:dbc',
        'regular-sb/scene/pg:dbc/78c',
        'render-prop-sb/scene/pg:dbc/78c/prop-label-header-header',
        'render-prop-value-sb/scene/pg:dbc/78c/pro-header', // <- the inserted render prop
        'render-prop-sb/scene/pg:dbc/78c/prop-label-children-children',
        'regular-sb/scene/pg:dbc/78c/891',
      ])
      expect(
        renderResult.getEditorState().derived.visibleNavigatorTargets.map(navigatorEntryToKey),
      ).toEqual([
        'regular-sb/scene',
        'regular-sb/scene/pg',
        'regular-sb/scene/pg:dbc',
        'regular-sb/scene/pg:dbc/78c',
        'render-prop-sb/scene/pg:dbc/78c/prop-label-header-header',
        'render-prop-value-sb/scene/pg:dbc/78c/pro-header', // <- the inserted render prop
        'render-prop-sb/scene/pg:dbc/78c/prop-label-children-children',
        'regular-sb/scene/pg:dbc/78c/891',
      ])
    })

    it('can delete an element descended from a render prop', async () => {
      const editor = await renderTestEditorWithModel(
        projectWithRenderProp(
          `header={<div data-uid='render-prop-parent'><span data-uid='render-prop-child'>hi</span></div>}`,
        ),
        'await-first-dom-report',
      )

      // before the render prop child is deleted
      expect(editor.getEditorState().derived.navigatorTargets.map(navigatorEntryToKey)).toEqual([
        'regular-sb/scene',
        'regular-sb/scene/pg',
        'regular-sb/scene/pg:dbc',
        'regular-sb/scene/pg:dbc/78c',
        'render-prop-sb/scene/pg:dbc/78c/prop-label-header-header',
        'render-prop-value-sb/scene/pg:dbc/78c/render-prop-parent-header',
        'regular-sb/scene/pg:dbc/78c/render-prop-parent/render-prop-child',
        'render-prop-sb/scene/pg:dbc/78c/prop-label-children-children',
        'regular-sb/scene/pg:dbc/78c/f59',
      ])

      await selectComponentsForTest(editor, [
        EP.fromString('sb/scene/pg:dbc/78c/render-prop-parent/render-prop-child'),
      ])
      await pressKey('Backspace')

      // before the render prop child is deleted
      expect(editor.getEditorState().derived.navigatorTargets.map(navigatorEntryToKey)).toEqual([
        'regular-sb/scene',
        'regular-sb/scene/pg',
        'regular-sb/scene/pg:dbc',
        'regular-sb/scene/pg:dbc/78c',
        'render-prop-sb/scene/pg:dbc/78c/prop-label-header-header',
        'render-prop-value-sb/scene/pg:dbc/78c/render-prop-parent-header',
        'render-prop-sb/scene/pg:dbc/78c/prop-label-children-children',
        'regular-sb/scene/pg:dbc/78c/f59',
      ])

      expect(editor.getEditorState().derived.navigatorTargets.map(navigatorEntryToKey)).toEqual([
        'regular-sb/scene',
        'regular-sb/scene/pg',
        'regular-sb/scene/pg:dbc',
        'regular-sb/scene/pg:dbc/78c',
        'render-prop-sb/scene/pg:dbc/78c/prop-label-header-header',
        'render-prop-value-sb/scene/pg:dbc/78c/render-prop-parent-header',
        'render-prop-sb/scene/pg:dbc/78c/prop-label-children-children',
        'regular-sb/scene/pg:dbc/78c/f59',
      ])
    })

    it(`can delete an element that's inside a render prop`, async () => {
      const editor = await renderTestEditorWithModel(
        projectWithRenderProp(
          `header={<div data-uid='render-prop-parent'><span data-uid='render-prop-child'>hi</span></div>}`,
        ),
        'await-first-dom-report',
      )

      // before the render prop child is deleted
      expect(editor.getEditorState().derived.navigatorTargets.map(navigatorEntryToKey)).toEqual([
        'regular-sb/scene',
        'regular-sb/scene/pg',
        'regular-sb/scene/pg:dbc',
        'regular-sb/scene/pg:dbc/78c',
        'render-prop-sb/scene/pg:dbc/78c/prop-label-header-header',
        'render-prop-value-sb/scene/pg:dbc/78c/render-prop-parent-header',
        'regular-sb/scene/pg:dbc/78c/render-prop-parent/render-prop-child',
        'render-prop-sb/scene/pg:dbc/78c/prop-label-children-children',
        'regular-sb/scene/pg:dbc/78c/f59',
      ])

      await selectComponentsForTest(editor, [
        EP.fromString('sb/scene/pg:dbc/78c/render-prop-parent'),
      ])
      await pressKey('Backspace')

      // before the render prop child is deleted
      expect(editor.getEditorState().derived.navigatorTargets.map(navigatorEntryToKey)).toEqual([
        'regular-sb/scene',
        'regular-sb/scene/pg',
        'regular-sb/scene/pg:dbc',
        'regular-sb/scene/pg:dbc/78c',
        'render-prop-sb/scene/pg:dbc/78c/prop-label-header-header',
        'slot_sb/scene/pg:dbc/78c/prop-label-header',
        'render-prop-sb/scene/pg:dbc/78c/prop-label-children-children',
        'regular-sb/scene/pg:dbc/78c/f59',
      ])

      expect(editor.getEditorState().derived.navigatorTargets.map(navigatorEntryToKey)).toEqual([
        'regular-sb/scene',
        'regular-sb/scene/pg',
        'regular-sb/scene/pg:dbc',
        'regular-sb/scene/pg:dbc/78c',
        'render-prop-sb/scene/pg:dbc/78c/prop-label-header-header',
        'slot_sb/scene/pg:dbc/78c/prop-label-header',
        'render-prop-sb/scene/pg:dbc/78c/prop-label-children-children',
        'regular-sb/scene/pg:dbc/78c/f59',
      ])
    })
  })
})

describe('Navigator row order', () => {
  const TestCode = `
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
            // @utopia/uid=52e
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

  it('Is correct for a test project with synthetic elements', async () => {
    const renderResult = await renderTestEditorWithCode(TestCode, 'await-first-dom-report')

    await renderResult.dispatch([setFocusedElement(EP.fromString('sb/sc/app:app-root/card'))], true)
    await renderResult.getDispatchFollowUpActionsFinished()
    expect(renderResult.getEditorState().derived.navigatorTargets.map(navigatorEntryToKey)).toEqual(
      [
        'regular-sb/1e7',
        'regular-sb/sc',
        'regular-sb/sc/app',
        'regular-sb/sc/app:app-root',
        'regular-sb/sc/app:app-root/card',
        'regular-sb/sc/app:app-root/card:card-root',
        'regular-sb/sc/app:app-root/card:card-root/30d',
        'regular-sb/sc/app:app-root/card:card-root/card-span',
        'regular-sb/sc/app:app-root/card/card-child',
        'regular-sb/sc/app:app-root/52e',
        'regular-sb/sc/app:app-root/frag',
        'regular-sb/sc/app:app-root/frag/frag-child',
        'regular-sb/sc/app:app-root/frag/cond-1',
        'conditional-clause-sb/sc/app:app-root/frag/cond-1-true-case',
        'regular-sb/sc/app:app-root/frag/cond-1/cond-1-true',
        'regular-sb/sc/app:app-root/frag/cond-1/cond-1-true/cond-1-true-child',
        'regular-sb/sc/app:app-root/frag/cond-1/cond-1-true/cond-2',
        'conditional-clause-sb/sc/app:app-root/frag/cond-1/cond-1-true/cond-2-true-case',
        'regular-sb/sc/app:app-root/frag/cond-1/cond-1-true/cond-2/cond-2-child',
        'conditional-clause-sb/sc/app:app-root/frag/cond-1/cond-1-true/cond-2-false-case',
        'synthetic-sb/sc/app:app-root/frag/cond-1/cond-1-true/cond-2/d84-attribute',
        'conditional-clause-sb/sc/app:app-root/frag/cond-1-false-case',
        'synthetic-sb/sc/app:app-root/frag/cond-1/019-attribute',
        'regular-sb/sc/app/app-child',
      ],
    )
  })

  it('is correct for js map expressions with multiple values', async () => {
    const renderResult = await renderTestEditorWithCode(
      projectWithMapExpression,
      'await-first-dom-report',
    )

    await renderResult.getDispatchFollowUpActionsFinished()

    expect(renderResult.getEditorState().derived.navigatorTargets.map(navigatorEntryToKey)).toEqual(
      [
        'regular-sb/group',
        'regular-sb/group/70f',
        'regular-sb/group/70f/33d~~~1',
        'regular-sb/group/70f/33d~~~1/b1f',
        'regular-sb/group/70f/33d~~~2',
        'regular-sb/group/70f/33d~~~2/b1f',
        'regular-sb/group/70f/33d~~~3',
        'regular-sb/group/70f/33d~~~3/b1f',
        'regular-sb/group/foo',
        'regular-sb/group/b68',
        'regular-sb/group/b68/46a~~~1',
        'regular-sb/group/b68/46a~~~1/255',
        'regular-sb/group/b68/46a~~~2',
        'regular-sb/group/b68/46a~~~2/255',
        'regular-sb/group/b68/46a~~~3',
        'regular-sb/group/b68/46a~~~3/255',
        'regular-sb/group/b68/46a~~~4',
        'regular-sb/group/b68/46a~~~4/255',
        'regular-sb/group/b68/46a~~~5',
        'regular-sb/group/b68/46a~~~5/255',
        'regular-sb/group/b68/46a~~~6',
        'regular-sb/group/b68/46a~~~6/255',
        'regular-sb/group/cond',
        'conditional-clause-sb/group/cond-true-case',
        'regular-sb/group/cond/929',
        'regular-sb/group/cond/929/f23~~~1',
        'regular-sb/group/cond/929/f23~~~1/c6b',
        'regular-sb/group/cond/929/f23~~~2',
        'regular-sb/group/cond/929/f23~~~2/c6b',
        'regular-sb/group/cond/929/f23~~~3',
        'regular-sb/group/cond/929/f23~~~3/c6b',
        'conditional-clause-sb/group/cond-false-case',
        'synthetic-sb/group/cond/235-element-235',
        'regular-sb/group/bar',
        'regular-sb/group/651',
        'regular-sb/group/651/3bc~~~1',
        'regular-sb/group/651/3bc~~~1/7a7',
        'regular-sb/group/651/3bc~~~2',
        'regular-sb/group/651/3bc~~~2/7a7',
        'regular-sb/group/text-expr-cond-wrapper',
        'regular-sb/group/text-expr-cond-wrapper/text-expr-cond',
        'conditional-clause-sb/group/text-expr-cond-wrapper/text-expr-cond-true-case',
        'regular-sb/group/text-expr-cond-wrapper/text-expr-cond/619',
        'conditional-clause-sb/group/text-expr-cond-wrapper/text-expr-cond-false-case',
        'synthetic-sb/group/text-expr-cond-wrapper/text-expr-cond/e4a-element-e4a',
        'regular-sb/group/zero-length-map-wrapper',
        'regular-sb/group/zero-length-map-wrapper/a47',
      ],
    )
    expect(
      renderResult.getEditorState().derived.visibleNavigatorTargets.map(navigatorEntryToKey),
    ).toEqual([
      'regular-sb/group',
      'regular-sb/group/70f',
      'regular-sb/group/70f/33d~~~1',
      'regular-sb/group/70f/33d~~~2',
      'regular-sb/group/70f/33d~~~3',
      'regular-sb/group/foo',
      'regular-sb/group/b68',
      'regular-sb/group/b68/46a~~~1',
      'regular-sb/group/b68/46a~~~2',
      'regular-sb/group/b68/46a~~~3',
      'regular-sb/group/b68/46a~~~4',
      'regular-sb/group/b68/46a~~~5',
      'regular-sb/group/b68/46a~~~6',
      'regular-sb/group/cond',
      'conditional-clause-sb/group/cond-true-case',
      'regular-sb/group/cond/929',
      'regular-sb/group/cond/929/f23~~~1',
      'regular-sb/group/cond/929/f23~~~2',
      'regular-sb/group/cond/929/f23~~~3',
      'conditional-clause-sb/group/cond-false-case',
      'synthetic-sb/group/cond/235-element-235',
      'regular-sb/group/bar',
      'regular-sb/group/651',
      'regular-sb/group/651/3bc~~~1',
      'regular-sb/group/651/3bc~~~2',
      'regular-sb/group/text-expr-cond-wrapper',
      'regular-sb/group/text-expr-cond-wrapper/text-expr-cond',
      'conditional-clause-sb/group/text-expr-cond-wrapper/text-expr-cond-true-case',
      'regular-sb/group/text-expr-cond-wrapper/text-expr-cond/619',
      'conditional-clause-sb/group/text-expr-cond-wrapper/text-expr-cond-false-case',
      'synthetic-sb/group/text-expr-cond-wrapper/text-expr-cond/e4a-element-e4a',
      'regular-sb/group/zero-length-map-wrapper',
      'regular-sb/group/zero-length-map-wrapper/a47',
    ])
  })

  it('is correct for an expressions with a generated element', async () => {
    const renderResult = await renderTestEditorWithCode(
      projectWithExpression,
      'await-first-dom-report',
    )

    await renderResult.getDispatchFollowUpActionsFinished()

    expect(renderResult.getEditorState().derived.navigatorTargets.map(navigatorEntryToKey)).toEqual(
      [
        'regular-sb/group',
        'regular-sb/group/0b5',
        'regular-sb/group/0b5/995~~~1',
        'regular-sb/group/foo',
        'regular-sb/group/809',
        'regular-sb/group/809/ea0~~~1',
        'regular-sb/group/809/ea0~~~2',
        'regular-sb/group/cond',
        'conditional-clause-sb/group/cond-true-case',
        'regular-sb/group/cond/4ce',
        'regular-sb/group/cond/4ce/27d~~~1',
        'conditional-clause-sb/group/cond-false-case',
        'synthetic-sb/group/cond/75a-element-75a',
        'regular-sb/group/bar',
        'regular-sb/group/bf0',
        'regular-sb/group/bf0/b78~~~1',
      ],
    )
    expect(
      renderResult.getEditorState().derived.visibleNavigatorTargets.map(navigatorEntryToKey),
    ).toEqual([
      'regular-sb/group',
      'regular-sb/group/0b5',
      'regular-sb/group/0b5/995~~~1',
      'regular-sb/group/foo',
      'regular-sb/group/809',
      'regular-sb/group/809/ea0~~~1',
      'regular-sb/group/809/ea0~~~2',
      'regular-sb/group/cond',
      'conditional-clause-sb/group/cond-true-case',
      'regular-sb/group/cond/4ce',
      'regular-sb/group/cond/4ce/27d~~~1',
      'conditional-clause-sb/group/cond-false-case',
      'synthetic-sb/group/cond/75a-element-75a',
      'regular-sb/group/bar',
      'regular-sb/group/bf0',
      'regular-sb/group/bf0/b78~~~1',
    ])
  })

  it('is correct for a project with elements with render prop', async () => {
    const renderResult = await renderTestEditorWithModel(
      projectWithRenderProp('header={<span>Title</span>}'),
      'await-first-dom-report',
    )

    await renderResult.getDispatchFollowUpActionsFinished()

    expect(renderResult.getEditorState().derived.navigatorTargets.map(navigatorEntryToKey)).toEqual(
      [
        'regular-sb/scene',
        'regular-sb/scene/pg',
        'regular-sb/scene/pg:dbc',
        'regular-sb/scene/pg:dbc/78c',
        'render-prop-sb/scene/pg:dbc/78c/prop-label-header-header',
        'render-prop-value-sb/scene/pg:dbc/78c/298-header',
        'render-prop-sb/scene/pg:dbc/78c/prop-label-children-children',
        'regular-sb/scene/pg:dbc/78c/d93',
      ],
    )
    expect(
      renderResult.getEditorState().derived.visibleNavigatorTargets.map(navigatorEntryToKey),
    ).toEqual([
      'regular-sb/scene',
      'regular-sb/scene/pg',
      'regular-sb/scene/pg:dbc',
      'regular-sb/scene/pg:dbc/78c',
      'render-prop-sb/scene/pg:dbc/78c/prop-label-header-header',
      'render-prop-value-sb/scene/pg:dbc/78c/298-header',
      'render-prop-sb/scene/pg:dbc/78c/prop-label-children-children',
      'regular-sb/scene/pg:dbc/78c/d93',
    ])
  })
  it('is correct for a project with elements with string render prop', async () => {
    const renderResult = await renderTestEditorWithModel(
      projectWithRenderProp('header={"Title"}'),
      'await-first-dom-report',
    )

    await renderResult.getDispatchFollowUpActionsFinished()

    expect(renderResult.getEditorState().derived.navigatorTargets.map(navigatorEntryToKey)).toEqual(
      [
        'regular-sb/scene',
        'regular-sb/scene/pg',
        'regular-sb/scene/pg:dbc',
        'regular-sb/scene/pg:dbc/78c',
        'render-prop-sb/scene/pg:dbc/78c/prop-label-header-header',
        'synthetic-sb/scene/pg:dbc/78c/ee8-attribute',
        'render-prop-sb/scene/pg:dbc/78c/prop-label-children-children',
        'regular-sb/scene/pg:dbc/78c/d93',
      ],
    )
    expect(
      renderResult.getEditorState().derived.visibleNavigatorTargets.map(navigatorEntryToKey),
    ).toEqual([
      'regular-sb/scene',
      'regular-sb/scene/pg',
      'regular-sb/scene/pg:dbc',
      'regular-sb/scene/pg:dbc/78c',
      'render-prop-sb/scene/pg:dbc/78c/prop-label-header-header',
      'synthetic-sb/scene/pg:dbc/78c/ee8-attribute',
      'render-prop-sb/scene/pg:dbc/78c/prop-label-children-children',
      'regular-sb/scene/pg:dbc/78c/d93',
    ])
  })
  it('is correct for a project with elements with missing render prop', async () => {
    const renderResult = await renderTestEditorWithModel(
      projectWithRenderProp(''), // <- no render prop
      'await-first-dom-report',
    )

    await renderResult.getDispatchFollowUpActionsFinished()
    expect(renderResult.getEditorState().derived.navigatorTargets.map(navigatorEntryToKey)).toEqual(
      [
        'regular-sb/scene',
        'regular-sb/scene/pg',
        'regular-sb/scene/pg:dbc',
        'regular-sb/scene/pg:dbc/78c',
        'render-prop-sb/scene/pg:dbc/78c/prop-label-header-header',
        'slot_sb/scene/pg:dbc/78c/prop-label-header', // <- the slot is shown
        'render-prop-sb/scene/pg:dbc/78c/prop-label-children-children',
        'regular-sb/scene/pg:dbc/78c/d93',
      ],
    )
    expect(
      renderResult.getEditorState().derived.visibleNavigatorTargets.map(navigatorEntryToKey),
    ).toEqual([
      'regular-sb/scene',
      'regular-sb/scene/pg',
      'regular-sb/scene/pg:dbc',
      'regular-sb/scene/pg:dbc/78c',
      'render-prop-sb/scene/pg:dbc/78c/prop-label-header-header',
      'slot_sb/scene/pg:dbc/78c/prop-label-header', // <- the slot is shown
      'render-prop-sb/scene/pg:dbc/78c/prop-label-children-children',
      'regular-sb/scene/pg:dbc/78c/d93',
    ])
  })
  it('is correct for a project with elements with render prop set to `null`', async () => {
    const renderResult = await renderTestEditorWithModel(
      projectWithRenderProp('header={null}'),
      'await-first-dom-report',
    )

    await renderResult.getDispatchFollowUpActionsFinished()

    expect(renderResult.getEditorState().derived.navigatorTargets.map(navigatorEntryToKey)).toEqual(
      [
        'regular-sb/scene',
        'regular-sb/scene/pg',
        'regular-sb/scene/pg:dbc',
        'regular-sb/scene/pg:dbc/78c',
        'render-prop-sb/scene/pg:dbc/78c/prop-label-header-header',
        'slot_sb/scene/pg:dbc/78c/prop-label-header', // <- the slot is shown
        'render-prop-sb/scene/pg:dbc/78c/prop-label-children-children',
        'regular-sb/scene/pg:dbc/78c/d93',
      ],
    )
    expect(
      renderResult.getEditorState().derived.visibleNavigatorTargets.map(navigatorEntryToKey),
    ).toEqual([
      'regular-sb/scene',
      'regular-sb/scene/pg',
      'regular-sb/scene/pg:dbc',
      'regular-sb/scene/pg:dbc/78c',
      'render-prop-sb/scene/pg:dbc/78c/prop-label-header-header',
      'slot_sb/scene/pg:dbc/78c/prop-label-header', // <- the slot is shown
      'render-prop-sb/scene/pg:dbc/78c/prop-label-children-children',
      'regular-sb/scene/pg:dbc/78c/d93',
    ])
  })

  it('is correct for a project where an import is present in the component registration', async () => {
    const renderResult = await renderTestEditorWithModel(
      projectWithThirdPartyRenderProp(''),
      'await-first-dom-report',
    )

    await renderResult.getDispatchFollowUpActionsFinished()

    expect(renderResult.getEditorState().derived.navigatorTargets.map(navigatorEntryToKey)).toEqual(
      [
        'regular-sb/scene',
        'regular-sb/scene/pg',
        'regular-sb/scene/pg:dbc',
        'regular-sb/scene/pg:dbc/78c',
        'render-prop-sb/scene/pg:dbc/78c/prop-label-header-header', // <- the name of this is coming from an import
        'slot_sb/scene/pg:dbc/78c/prop-label-header',
        'render-prop-sb/scene/pg:dbc/78c/prop-label-children-children',
        'regular-sb/scene/pg:dbc/78c/891',
      ],
    )
    expect(
      renderResult.getEditorState().derived.visibleNavigatorTargets.map(navigatorEntryToKey),
    ).toEqual([
      'regular-sb/scene',
      'regular-sb/scene/pg',
      'regular-sb/scene/pg:dbc',
      'regular-sb/scene/pg:dbc/78c',
      'render-prop-sb/scene/pg:dbc/78c/prop-label-header-header', // <- the name of this is coming from an import
      'slot_sb/scene/pg:dbc/78c/prop-label-header',
      'render-prop-sb/scene/pg:dbc/78c/prop-label-children-children',
      'regular-sb/scene/pg:dbc/78c/891',
    ])
  })

  it('is correct in the presence of an early return', async () => {
    const renderResult = await renderTestEditorWithModel(
      projectWithEarlyReturn,
      'await-first-dom-report',
    )

    await renderResult.getDispatchFollowUpActionsFinished()

    expect(renderResult.getEditorState().derived.navigatorTargets.map(navigatorEntryToKey)).toEqual(
      ['regular-sb/scene'],
    )
    expect(
      renderResult.getEditorState().derived.visibleNavigatorTargets.map(navigatorEntryToKey),
    ).toEqual(['regular-sb/scene'])
  })
})

describe('Navigator labels', () => {
  it('Labels are correct for text coming from expressions', async () => {
    const renderResult = await renderTestEditorWithCode(
      projectWithTextFromExpression,
      'await-first-dom-report',
    )

    await renderResult.getDispatchFollowUpActionsFinished()

    const navigatorItem = await renderResult.renderedDOM.findByTestId(
      'NavigatorItemTestId-regular_sb/div-label',
    )
    expect(navigatorItem.textContent).toEqual('2')
  })
})

describe('groups', () => {
  describe('reparenting', () => {
    it('adjusts the group when reparenting into a group', async () => {
      const renderResult = await renderTestEditorWithCode(
        makeTestProjectCodeWithSnippet(`
        <div data-uid='root'>
          <Group
            data-uid='group'
            style={{
              background: 'white',
              position: 'absolute',
              left: 100,
              top: 100,
            }}
          >
            <div data-uid='group-child-1' style={{
              background: 'gray',
              width: 50,
              height: 50,
              position: 'absolute',
              left: 10,
              top: 10
            }} />
            <div data-uid='group-child-2' style={{
              background: 'gray',
              width: 50,
              height: 50,
              position: 'absolute',
              left: 200,
              top: 200
            }} />
          </Group>
          <div data-uid='dragme' style={{
            background: 'red',
            width: 50,
            height: 50,
            position: 'absolute',
            left: 10,
            top: 10
          }} />
        </div>
      `),
        'await-first-dom-report',
      )

      const dragmePath = EP.fromString(
        `${BakedInStoryboardUID}/${TestSceneUID}/${TestAppUID}:root/dragme`,
      )

      const groupPath = EP.fromString(
        `${BakedInStoryboardUID}/${TestSceneUID}/${TestAppUID}:root/group`,
      )

      const canvasRect = renderResult.renderedDOM.getByTestId('canvas-root').getBoundingClientRect()
      const canvasCenter = getRectCenter(
        windowRectangle({
          x: canvasRect.x + DefaultNavigatorWidth,
          y: canvasRect.y,
          width: canvasRect.width - DefaultNavigatorWidth,
          height: canvasRect.height,
        }),
      )

      const originalDragmeGlobalFrame =
        MetadataUtils.findElementByElementPath(
          renderResult.getEditorState().editor.jsxMetadata,
          dragmePath,
        )?.globalFrame ?? null
      if (originalDragmeGlobalFrame == null) {
        throw new Error('global frame not found')
      }

      await renderResult.dispatch(
        createNavigatorReparentPostActionActions(
          [dragmePath],
          groupPath,
          back(),
          canvasPoint(canvasCenter),
          renderResult.getEditorState().editor.jsxMetadata,
          renderResult.getEditorState().editor.allElementProps,
        ),
        true,
      )

      expect(
        MetadataUtils.findElementByElementPath(
          renderResult.getEditorState().editor.jsxMetadata,
          EP.appendToPath(groupPath, 'dragme'),
        )?.globalFrame ?? null,
      ).toEqual(originalDragmeGlobalFrame)

      expect(getPrintedUiJsCode(renderResult.getEditorState())).toEqual(
        makeTestProjectCodeWithSnippet(`
          <div data-uid='root'>
            <Group
              data-uid='group'
              style={{
                background: 'white',
                position: 'absolute',
                left: 10,
                top: 10,
              }}
            >
              <div data-uid='dragme' style={{
                background: 'red',
                width: 50,
                height: 50,
                position: 'absolute',
                left: 0,
                top: 0
              }} />
              <div data-uid='group-child-1' style={{
                background: 'gray',
                width: 50,
                height: 50,
                position: 'absolute',
                left: 100,
                top: 100
              }} />
              <div data-uid='group-child-2' style={{
                background: 'gray',
                width: 50,
                height: 50,
                position: 'absolute',
                left: 290,
                top: 290
              }} />
            </Group>
          </div>
        `),
      )
    })
    it('adjusts the group when reparenting out of a group', async () => {
      const renderResult = await renderTestEditorWithCode(
        makeTestProjectCodeWithSnippet(`
          <div data-uid='root'>
            <div
              data-uid='move-here'
              style={{
                position: 'absolute',
                left: 10,
                top: 10,
                width: 10,
                height: 10,
                background: 'red',
              }}
            />
            <Group
              data-uid='group'
              style={{
                background: 'white',
                position: 'absolute',
                left: 10,
                top: 10,
                width: 340,
                height: 340,
              }}
            >
              <div data-uid='dragme' style={{
                background: 'red',
                width: 50,
                height: 50,
                position: 'absolute',
                left: 0,
                top: 0
              }} />
              <div data-uid='group-child-1' style={{
                background: 'gray',
                width: 50,
                height: 50,
                position: 'absolute',
                left: 100,
                top: 100
              }} />
              <div data-uid='group-child-2' style={{
                background: 'gray',
                width: 50,
                height: 50,
                position: 'absolute',
                left: 290,
                top: 290
              }} />
            </Group>
          </div>
      `),
        'await-first-dom-report',
      )

      const dragmePath = EP.fromString(
        `${BakedInStoryboardUID}/${TestSceneUID}/${TestAppUID}:root/group/dragme`,
      )

      const moveHerePath = EP.fromString(
        `${BakedInStoryboardUID}/${TestSceneUID}/${TestAppUID}:root/move-here`,
      )

      const canvasRect = renderResult.renderedDOM.getByTestId('canvas-root').getBoundingClientRect()
      const canvasCenter = getRectCenter(
        windowRectangle({
          x: canvasRect.x + DefaultNavigatorWidth,
          y: canvasRect.y,
          width: canvasRect.width - DefaultNavigatorWidth,
          height: canvasRect.height,
        }),
      )

      const originalDragmeGlobalFrame =
        MetadataUtils.findElementByElementPath(
          renderResult.getEditorState().editor.jsxMetadata,
          dragmePath,
        )?.globalFrame ?? null
      if (originalDragmeGlobalFrame == null) {
        throw new Error('global frame not found')
      }

      await renderResult.dispatch(
        createNavigatorReparentPostActionActions(
          [dragmePath],
          moveHerePath,
          front(),
          canvasPoint(canvasCenter),
          renderResult.getEditorState().editor.jsxMetadata,
          renderResult.getEditorState().editor.allElementProps,
        ),
        true,
      )

      expect(
        MetadataUtils.findElementByElementPath(
          renderResult.getEditorState().editor.jsxMetadata,
          EP.appendToPath(moveHerePath, 'dragme'),
        )?.globalFrame ?? null,
      ).toEqual(originalDragmeGlobalFrame)

      expect(getPrintedUiJsCode(renderResult.getEditorState())).toEqual(
        makeTestProjectCodeWithSnippet(`
          <div data-uid='root'>
            <div
              data-uid='move-here'
              style={{
                position: 'absolute',
                left: 10,
                top: 10,
                width: 10,
                height: 10,
                background: 'red',
              }}
            >
              <div data-uid='dragme' style={{
                background: 'red',
                width: 50,
                height: 50,
                position: 'absolute',
                left: 0,
                top: 0
              }} />
            </div>
            <Group
              data-uid='group'
              style={{
                background: 'white',
                position: 'absolute',
                left: 110,
                top: 110,
                width: 240,
                height: 240,
              }}
            >
              <div data-uid='group-child-1' style={{
                background: 'gray',
                width: 50,
                height: 50,
                position: 'absolute',
                left: 0,
                top: 0
              }} />
              <div data-uid='group-child-2' style={{
                background: 'gray',
                width: 50,
                height: 50,
                position: 'absolute',
                left: 190,
                top: 190
              }} />
            </Group>
          </div>
        `),
      )
    })

    it('adjusts the group when reparenting out of a group, take 2', async () => {
      const renderResult = await renderTestEditorWithCode(
        makeTestProjectCodeWithSnippet(`
          <div data-uid='root'>
            <div
              data-uid='move-here'
              style={{
                position: 'absolute',
                left: 300,
                top: 300,
                width: 10,
                height: 10,
                background: 'red',
              }}
            />
            <Group
              data-uid='group'
              style={{
                background: 'white',
                position: 'absolute',
                left: 10,
                top: 10,
                width: 340,
                height: 340,
              }}
            >
              <div data-uid='group-child-0' style={{
                background: 'red',
                width: 50,
                height: 50,
                position: 'absolute',
                left: 0,
                top: 0
              }} />
              <div data-uid='group-child-1' style={{
                background: 'gray',
                width: 50,
                height: 50,
                position: 'absolute',
                left: 100,
                top: 100
              }} />
              <div data-uid='group-child-2' style={{
                background: 'gray',
                width: 50,
                height: 50,
                position: 'absolute',
                left: 290,
                top: 290
              }} />
            </Group>
          </div>
      `),
        'await-first-dom-report',
      )

      const dragmePath = EP.fromString(
        `${BakedInStoryboardUID}/${TestSceneUID}/${TestAppUID}:root/group/group-child-2`,
      )

      const moveHerePath = EP.fromString(
        `${BakedInStoryboardUID}/${TestSceneUID}/${TestAppUID}:root/move-here`,
      )

      const canvasRect = renderResult.renderedDOM.getByTestId('canvas-root').getBoundingClientRect()
      const canvasCenter = getRectCenter(
        windowRectangle({
          x: canvasRect.x + DefaultNavigatorWidth,
          y: canvasRect.y,
          width: canvasRect.width - DefaultNavigatorWidth,
          height: canvasRect.height,
        }),
      )

      const originalDragmeGlobalFrame =
        MetadataUtils.findElementByElementPath(
          renderResult.getEditorState().editor.jsxMetadata,
          dragmePath,
        )?.globalFrame ?? null
      if (originalDragmeGlobalFrame == null) {
        throw new Error('global frame not found')
      }

      await renderResult.dispatch(
        createNavigatorReparentPostActionActions(
          [dragmePath],
          moveHerePath,
          front(),
          canvasPoint(canvasCenter),
          renderResult.getEditorState().editor.jsxMetadata,
          renderResult.getEditorState().editor.allElementProps,
        ),
        true,
      )

      expect(
        MetadataUtils.findElementByElementPath(
          renderResult.getEditorState().editor.jsxMetadata,
          EP.appendToPath(moveHerePath, 'group-child-2'),
        )?.globalFrame ?? null,
      ).toEqual(originalDragmeGlobalFrame)

      expect(getPrintedUiJsCode(renderResult.getEditorState())).toEqual(
        makeTestProjectCodeWithSnippet(`
        <div data-uid='root'>
          <div
            data-uid='move-here'
            style={{
              position: 'absolute',
              left: 300,
              top: 300,
              width: 10,
              height: 10,
              background: 'red',
            }}
          >
            <div data-uid='group-child-2' style={{
              background: 'gray',
              width: 50,
              height: 50,
              position: 'absolute',
              left: 0,
              top: 0,
            }} />
          </div>
          <Group
            data-uid='group'
            style={{
              background: 'white',
              position: 'absolute',
              left: 10,
              top: 10,
              width: 150,
              height: 150,
            }}
          >
            <div data-uid='group-child-0' style={{
              background: 'red',
              width: 50,
              height: 50,
              position: 'absolute',
              left: 0,
              top: 0,
            }} />
            <div data-uid='group-child-1' style={{
              background: 'gray',
              width: 50,
              height: 50,
              position: 'absolute',
              left: 100,
              top: 100,
            }} />
            </Group>
        </div>
        `),
      )
    })

    it('adjusts the groups when reparenting into a nested group', async () => {
      const renderResult = await renderTestEditorWithCode(
        makeTestProjectCodeWithSnippet(`
          <div data-uid='root'>
            <Group
              data-uid='group-1'
              style={{
                background: 'white',
                position: 'absolute',
                left: 100,
                top: 100,
              }}
            >
              <div data-uid='intruder' style={{
                background: 'orange',
                width: 20,
                height: 20,
                position: 'absolute',
                left: 10,
                top: 10,
              }} />
              <Group
                data-uid='group-2'
                style={{
                  background: 'cyan',
                  position: 'absolute',
                  left: 50,
                  top: 50,
                }}
              >
                <div data-uid='group-child-1' style={{
                  background: 'gray',
                  width: 50,
                  height: 50,
                  position: 'absolute',
                  left: 10,
                  top: 10
                }} />
                <div data-uid='group-child-2' style={{
                  background: 'gray',
                  width: 50,
                  height: 50,
                  position: 'absolute',
                  left: 200,
                  top: 200
                }} />
              </Group>
            </Group>
            <div data-uid='dragme' style={{
              background: 'red',
              width: 50,
              height: 50,
              position: 'absolute',
              left: 10,
              top: 10
            }} />
          </div>
      `),
        'await-first-dom-report',
      )

      const dragmePath = EP.fromString(
        `${BakedInStoryboardUID}/${TestSceneUID}/${TestAppUID}:root/dragme`,
      )

      const groupPath = EP.fromString(
        `${BakedInStoryboardUID}/${TestSceneUID}/${TestAppUID}:root/group-1/group-2`,
      )

      const canvasRect = renderResult.renderedDOM.getByTestId('canvas-root').getBoundingClientRect()
      const canvasCenter = getRectCenter(
        windowRectangle({
          x: canvasRect.x + DefaultNavigatorWidth,
          y: canvasRect.y,
          width: canvasRect.width - DefaultNavigatorWidth,
          height: canvasRect.height,
        }),
      )

      const originalDragmeGlobalFrame =
        MetadataUtils.findElementByElementPath(
          renderResult.getEditorState().editor.jsxMetadata,
          dragmePath,
        )?.globalFrame ?? null
      if (originalDragmeGlobalFrame == null) {
        throw new Error('global frame not found')
      }

      await renderResult.dispatch(
        createNavigatorReparentPostActionActions(
          [dragmePath],
          groupPath,
          back(),
          canvasPoint(canvasCenter),
          renderResult.getEditorState().editor.jsxMetadata,
          renderResult.getEditorState().editor.allElementProps,
        ),
        true,
      )

      expect(
        MetadataUtils.findElementByElementPath(
          renderResult.getEditorState().editor.jsxMetadata,
          EP.appendToPath(groupPath, 'dragme'),
        )?.globalFrame ?? null,
      ).toEqual(originalDragmeGlobalFrame)

      expect(getPrintedUiJsCode(renderResult.getEditorState())).toEqual(
        makeTestProjectCodeWithSnippet(`
          <div data-uid='root'>
            <Group
              data-uid='group-1'
              style={{
                background: 'white',
                position: 'absolute',
                left: 10,
                top: 10,
              }}
            >
              <div data-uid='intruder' style={{
                background: 'orange',
                width: 20,
                height: 20,
                position: 'absolute',
                left: 100,
                top: 100,
              }} />
              <Group
                data-uid='group-2'
                style={{
                  background: 'cyan',
                  position: 'absolute',
                  left: 0,
                  top: 0,
                }}
              >
                <div data-uid='dragme' style={{
                  background: 'red',
                  width: 50,
                  height: 50,
                  position: 'absolute',
                  left: 0,
                  top: 0
                }} />
                <div data-uid='group-child-1' style={{
                  background: 'gray',
                  width: 50,
                  height: 50,
                  position: 'absolute',
                  left: 150,
                  top: 150
                }} />
                <div data-uid='group-child-2' style={{
                  background: 'gray',
                  width: 50,
                  height: 50,
                  position: 'absolute',
                  left: 340,
                  top: 340
                }} />
              </Group>
            </Group>
          </div>
        `),
      )
    })
    it('adjusts the groups when reparenting out of a nested group', async () => {
      const renderResult = await renderTestEditorWithCode(
        makeTestProjectCodeWithSnippet(`
          <div data-uid='root'>
            <div
              data-uid='move-here'
              style={{
                position: 'absolute',
                left: 10,
                top: 10,
                width: 10,
                height: 10,
                background: 'red',
              }}
            />
            <Group
              data-uid='group-1'
              style={{
                background: 'white',
                position: 'absolute',
                left: 10,
                top: 10,
                width: 390,
                height: 390,
              }}
            >
              <div data-uid='intruder' style={{
                background: 'orange',
                width: 20,
                height: 20,
                position: 'absolute',
                left: 100,
                top: 100,
              }} />
              <Group
                data-uid='group-2'
                style={{
                  background: 'cyan',
                  position: 'absolute',
                  left: 0,
                  top: 0,
                  width: 390,
                  height: 390,
                }}
              >
                <div data-uid='dragme' style={{
                  background: 'red',
                  width: 50,
                  height: 50,
                  position: 'absolute',
                  left: 0,
                  top: 0
                }} />
                <div data-uid='group-child-1' style={{
                  background: 'gray',
                  width: 50,
                  height: 50,
                  position: 'absolute',
                  left: 150,
                  top: 150
                }} />
                <div data-uid='group-child-2' style={{
                  background: 'gray',
                  width: 50,
                  height: 50,
                  position: 'absolute',
                  left: 340,
                  top: 340
                }} />
              </Group>
            </Group>
          </div>
      `),
        'await-first-dom-report',
      )

      const dragmePath = EP.fromString(
        `${BakedInStoryboardUID}/${TestSceneUID}/${TestAppUID}:root/group-1/group-2/dragme`,
      )

      const moveHerePath = EP.fromString(
        `${BakedInStoryboardUID}/${TestSceneUID}/${TestAppUID}:root/move-here`,
      )

      const canvasRect = renderResult.renderedDOM.getByTestId('canvas-root').getBoundingClientRect()
      const canvasCenter = getRectCenter(
        windowRectangle({
          x: canvasRect.x + DefaultNavigatorWidth,
          y: canvasRect.y,
          width: canvasRect.width - DefaultNavigatorWidth,
          height: canvasRect.height,
        }),
      )

      const originalDragmeGlobalFrame =
        MetadataUtils.findElementByElementPath(
          renderResult.getEditorState().editor.jsxMetadata,
          dragmePath,
        )?.globalFrame ?? null
      if (originalDragmeGlobalFrame == null) {
        throw new Error('global frame not found')
      }

      await renderResult.dispatch(
        createNavigatorReparentPostActionActions(
          [dragmePath],
          moveHerePath,
          front(),
          canvasPoint(canvasCenter),
          renderResult.getEditorState().editor.jsxMetadata,
          renderResult.getEditorState().editor.allElementProps,
        ),
        true,
      )

      expect(
        MetadataUtils.findElementByElementPath(
          renderResult.getEditorState().editor.jsxMetadata,
          EP.appendToPath(moveHerePath, 'dragme'),
        )?.globalFrame ?? null,
      ).toEqual(originalDragmeGlobalFrame)

      expect(getPrintedUiJsCode(renderResult.getEditorState())).toEqual(
        makeTestProjectCodeWithSnippet(`
          <div data-uid='root'>
            <div
              data-uid='move-here'
              style={{
                position: 'absolute',
                left: 10,
                top: 10,
                width: 10,
                height: 10,
                background: 'red',
              }}
            >
              <div data-uid='dragme' style={{
                background: 'red',
                width: 50,
                height: 50,
                position: 'absolute',
                left: 0,
                top: 0
              }} />
            </div>
            <Group
              data-uid='group-1'
              style={{
                background: 'white',
                position: 'absolute',
                left: 110,
                top: 110,
                width: 290,
                height: 290,
              }}
            >
              <div data-uid='intruder' style={{
                background: 'orange',
                width: 20,
                height: 20,
                position: 'absolute',
                left: 0,
                top: 0,
              }} />
              <Group
                data-uid='group-2'
                style={{
                  background: 'cyan',
                  position: 'absolute',
                  left: 50,
                  top: 50,
                  width: 240,
                  height: 240,
                }}
              >
              <div data-uid='group-child-1' style={{
                background: 'gray',
                  width: 50,
                  height: 50,
                  position: 'absolute',
                  left: 0,
                  top: 0
                }} />
                <div data-uid='group-child-2' style={{
                  background: 'gray',
                  width: 50,
                  height: 50,
                  position: 'absolute',
                  left: 190,
                  top: 190
                }} />
              </Group>
            </Group>
          </div>
        `),
      )
    })
  })
})
