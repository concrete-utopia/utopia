import * as Prettier from 'prettier'
import * as React from 'react'
import * as ReactDOM from 'react-dom'
import * as ReactDOMServer from 'react-dom/server'
import * as UtopiaAPI from 'utopia-api'
import * as UUIUI from 'uuiui'
import {
  clearJSXElementUniqueIDs,
  MetadataWithoutChildren,
  TopLevelElement,
} from '../../core/shared/element-template'
import { RequireFn } from '../../core/shared/npm-dependency-types'
import { Imports } from '../../core/shared/project-file-types'
import { testParseCode } from '../../core/workers/parser-printer/parser-printer-test-utils'
import { foldEither, isRight, right } from '../../core/shared/either'
import Utils from '../../utils/utils'
import { FancyError } from '../../core/shared/code-exec-utils'
import { canvasPoint } from '../../core/shared/math-utils'
import { normalizeName } from '../custom-code/custom-code-utils'
import {
  CanvasReactErrorCallback,
  UiJsxCanvas,
  UiJsxCanvasContext,
  UiJsxCanvasContextData,
  UiJsxCanvasProps,
  UiJsxCanvasPropsWithErrorCallback,
} from './ui-jsx-canvas'
import { emptyImports } from '../../core/workers/common/project-file-utils'
import { BakedInStoryboardUID, BakedInStoryboardVariableName } from '../../core/model/scene-utils'
import { ConsoleLog } from '../editor/store/editor-state'

interface PartialCanvasProps {
  offset: UiJsxCanvasProps['offset']
  scale: UiJsxCanvasProps['scale']
  hiddenInstances: UiJsxCanvasProps['hiddenInstances']
  editedTextElement: UiJsxCanvasProps['editedTextElement']
  mountCount: UiJsxCanvasProps['mountCount']
}

const dumbRequireFn: RequireFn = (importOrigin: string, toImport: string) => {
  const normalizedName = normalizeName(importOrigin, toImport)
  switch (normalizedName) {
    case 'utopia-api':
      return UtopiaAPI
    case 'react':
      return React
    case 'react-dom':
      return ReactDOM
    case 'uuiui':
      return UUIUI
    default:
      throw new Error(`Unhandled values of ${importOrigin} and ${toImport}.`)
  }
}

function stripUidsFromMetadata(metadata: MetadataWithoutChildren): MetadataWithoutChildren {
  if (isRight(metadata.element)) {
    return {
      ...metadata,
      element: right(clearJSXElementUniqueIDs(metadata.element.value)),
    }
  } else {
    return metadata
  }
}

function renderCanvasReturnResultAndError(possibleProps: PartialCanvasProps | null, code: string) {
  const spyCollector: UiJsxCanvasContextData = {
    current: {
      spyValues: { metadata: {}, scenes: {} },
    },
  }

  const parsedCode = testParseCode(code)
  let errorsReported: Array<{
    editedFile: string
    error: FancyError
    errorInfo?: React.ErrorInfo
  }> = []
  const uiFilePath: UiJsxCanvasProps['uiFilePath'] = 'test.ui.js'
  const requireFn: UiJsxCanvasProps['requireFn'] = dumbRequireFn
  const fileBlobs: UiJsxCanvasProps['fileBlobs'] = {}
  const reportError: CanvasReactErrorCallback['reportError'] = (
    editedFile: string,
    error: FancyError,
    errorInfo?: React.ErrorInfo,
  ) => {
    errorsReported.push({ editedFile: editedFile, error: error, errorInfo: errorInfo })
  }
  const clearErrors: CanvasReactErrorCallback['clearErrors'] = Utils.NO_OP
  const imports: Imports = foldEither(
    (_) => emptyImports(),
    (success) => success.imports,
    parsedCode,
  )
  const topLevelElements: Array<TopLevelElement> = foldEither(
    (_) => [],
    (success) => success.topLevelElements,
    parsedCode,
  )
  const dependencyOrdering = foldEither(
    (_) => [],
    (success) => success.dependencyOrdering,
    parsedCode,
  )
  const jsxFactoryFunction = foldEither(
    (_) => null,
    (success) => success.jsxFactoryFunction,
    parsedCode,
  )
  let canvasProps: UiJsxCanvasPropsWithErrorCallback
  let consoleLogs: Array<ConsoleLog> = []
  function clearConsoleLogs(): void {
    consoleLogs = []
  }
  function addToConsoleLogs(log: ConsoleLog): void {
    consoleLogs.push(log)
  }
  if (possibleProps == null) {
    canvasProps = {
      uiFilePath: uiFilePath,
      requireFn: requireFn,
      fileBlobs: fileBlobs,
      onDomReport: Utils.NO_OP,
      reportError: reportError,
      clearErrors: clearErrors,
      offset: canvasPoint({ x: 0, y: 0 }),
      scale: 1,
      hiddenInstances: [],
      editedTextElement: null,
      mountCount: 0,
      walkDOM: false,
      spyEnabled: true,
      imports: imports,
      topLevelElementsIncludingScenes: topLevelElements,
      dependencyOrdering: dependencyOrdering,
      jsxFactoryFunction: jsxFactoryFunction,
      canvasIsLive: false,
      shouldIncludeCanvasRootInTheSpy: false,
      clearConsoleLogs: clearConsoleLogs,
      addToConsoleLogs: addToConsoleLogs,
    }
  } else {
    canvasProps = {
      ...possibleProps,
      uiFilePath: uiFilePath,
      requireFn: requireFn,
      fileBlobs: fileBlobs,
      onDomReport: Utils.NO_OP,
      reportError: reportError,
      clearErrors: clearErrors,
      walkDOM: false,
      spyEnabled: true,
      imports: imports,
      topLevelElementsIncludingScenes: topLevelElements,
      dependencyOrdering: dependencyOrdering,
      jsxFactoryFunction: jsxFactoryFunction,
      canvasIsLive: false,
      shouldIncludeCanvasRootInTheSpy: false,
      clearConsoleLogs: clearConsoleLogs,
      addToConsoleLogs: addToConsoleLogs,
    }
  }

  const canvasPropsSpyDisabled = {
    ...canvasProps,
    spyEnabled: false,
  }

  let formattedSpyEnabled
  let errorsReportedSpyEnabled = []
  try {
    const flatFormat = ReactDOMServer.renderToStaticMarkup(
      <UiJsxCanvasContext.Provider value={spyCollector}>
        <UiJsxCanvas {...canvasProps} />
      </UiJsxCanvasContext.Provider>,
    )
    formattedSpyEnabled = Prettier.format(flatFormat, { parser: 'html' })
    errorsReportedSpyEnabled = errorsReported
  } catch (e) {
    errorsReportedSpyEnabled = [e]
  }
  errorsReported = []

  let formattedSpyDisabled
  let errorsReportedSpyDisabled = []

  try {
    const flatFormatSpyDisabled = ReactDOMServer.renderToStaticMarkup(
      <UiJsxCanvasContext.Provider value={{ current: { spyValues: { metadata: {}, scenes: {} } } }}>
        <UiJsxCanvas {...canvasPropsSpyDisabled} />
      </UiJsxCanvasContext.Provider>,
    )
    formattedSpyDisabled = Prettier.format(flatFormatSpyDisabled, { parser: 'html' })
    errorsReportedSpyDisabled = errorsReported
  } catch (e) {
    errorsReportedSpyDisabled = [e]
  }

  return {
    formattedSpyEnabled,
    formattedSpyDisabled,
    errorsReportedSpyEnabled,
    errorsReportedSpyDisabled,
    spyValues: spyCollector.current.spyValues,
  }
}

function testCanvasRender(possibleProps: PartialCanvasProps | null, code: string): void {
  const {
    formattedSpyEnabled,
    formattedSpyDisabled,
    errorsReportedSpyEnabled,
    errorsReportedSpyDisabled,
    spyValues,
  } = renderCanvasReturnResultAndError(possibleProps, code)
  expect(errorsReportedSpyEnabled.length).toBe(0)
  expect(errorsReportedSpyDisabled.length).toBe(0)

  // Spy enabled or disabled should have no effect on the rendered HTML
  expect(formattedSpyEnabled).toEqual(formattedSpyDisabled)

  expect(formattedSpyEnabled).toMatchSnapshot()

  const metadataWithoutUIDs = Utils.objectMap(stripUidsFromMetadata, spyValues.metadata)
  expect(metadataWithoutUIDs).toMatchSnapshot()
}

function testCanvasRenderInline(possibleProps: PartialCanvasProps | null, code: string): string {
  const {
    formattedSpyEnabled,
    formattedSpyDisabled,
    errorsReportedSpyEnabled,
    errorsReportedSpyDisabled,
    spyValues,
  } = renderCanvasReturnResultAndError(possibleProps, code)
  if (errorsReportedSpyEnabled.length > 0) {
    console.error(errorsReportedSpyEnabled)
  }
  expect(errorsReportedSpyEnabled.length).toBe(0)
  expect(errorsReportedSpyDisabled.length).toBe(0)

  // Spy enabled or disabled should have no effect on the rendered HTML
  expect(formattedSpyEnabled).toEqual(formattedSpyDisabled)
  expect(formattedSpyEnabled).toBeDefined()

  return formattedSpyEnabled!
}

function testCanvasError(possibleProps: PartialCanvasProps | null, code: string): void {
  const { errorsReportedSpyEnabled, errorsReportedSpyDisabled } = renderCanvasReturnResultAndError(
    possibleProps,
    code,
  )

  expect(errorsReportedSpyEnabled.length).toEqual(errorsReportedSpyDisabled.length)
  expect(errorsReportedSpyEnabled.length).toBeGreaterThan(0)
  const errorsToCheck = errorsReportedSpyEnabled.map((error) => {
    let realError = error.error != null ? error.error : error
    const stackFrame = realError.stackFrames?.[0]
    return {
      name: realError.name,
      message: realError.message,
      originalCode: stackFrame?._originalScriptCode,
      lineNumber: stackFrame?._originalLineNumber,
      columnNumber: stackFrame?._originalColumnNumber,
    }
  })
  expect(errorsToCheck).toMatchSnapshot()
}

describe('UiJsxCanvas render', () => {
  it('renders a canvas defined by a utopia storyboard component', () => {
    testCanvasRender(
      null,
      `/** @jsx jsx */
      import * as React from 'react'
      import { View, jsx, Storyboard, Scene } from 'utopia-api'
      
      export var App = (props) => {
        return (
          <View
            style={{ ...(props.style || {}), backgroundColor: '#FFFFFF' }}
            layout={{ layoutSystem: 'pinSystem' }}
            data-uid={'aaa'}
          >
            <View data-uid={'bbb'}>hi</View>
          </View>
        )
      }
      export var ${BakedInStoryboardVariableName} = (props) => {
        return (
          <Storyboard data-uid={'${BakedInStoryboardUID}'}>
            <Scene
              style={{ height: 200, left: 59, width: 200, top: 79 }}
              component={App}
              layout={{ layoutSystem: 'pinSystem' }}
              props={{ style: { height: '100%', width: '100%' }, title: 'Hi there!' }}
              data-uid={'scene-0'}
            />
          </Storyboard>
        )
      }
      `,
    )
  })

  it('handles a component that renames its props object', () => {
    testCanvasRender(
      null,
      `/** @jsx jsx */
      import * as React from 'react'
      import { View, jsx, Storyboard, Scene } from 'utopia-api'

      const Card = (cardProps) => <div data-uid={'xxx'}>{cardProps.title}</div>
      export var App = (props) => {
        return (
          <View
            style={{ ...(props.style || {}), backgroundColor: '#FFFFFF' }}
            layout={{ layoutSystem: 'pinSystem' }}
            data-uid={'aaa'}
          >
            <Card data-uid={'bbb'} style={{backgroundColor: '#000000'}} title={props.title} />
          </View>
        )
      }
      export var ${BakedInStoryboardVariableName} = (props) => {
        return (
          <Storyboard data-uid={'${BakedInStoryboardUID}'}>
            <Scene
              style={{ height: 200, left: 59, width: 200, top: 79 }}
              component={App}
              layout={{ layoutSystem: 'pinSystem' }}
              props={{ style: { height: '100%', width: '100%' }, title: 'Hi there!' }}
              data-uid={'scene-0'}
            />
          </Storyboard>
        )
      }
      `,
    )
  })

  it('handles a component that destructures its props object', () => {
    testCanvasRender(
      null,
      `/** @jsx jsx */
      import * as React from 'react'
      import { View, jsx, Storyboard, Scene } from 'utopia-api'
      
      const Card = ({style, title}) => <div style={style} data-uid={'xxx'}>{title}</div>
      export var App = (props) => {
        return (
          <View
            style={{ ...(props.style || {}), backgroundColor: '#FFFFFF' }}
            layout={{ layoutSystem: 'pinSystem' }}
            data-uid={'aaa'}
          >
            <Card data-uid={'bbb'} style={{backgroundColor: '#000000'}} title={props.title} />
          </View>
        )
      }
      export var ${BakedInStoryboardVariableName} = (props) => {
        return (
          <Storyboard data-uid={'${BakedInStoryboardUID}'}>
            <Scene
              style={{ height: 200, left: 59, width: 200, top: 79 }}
              component={App}
              layout={{ layoutSystem: 'pinSystem' }}
              props={{ style: { height: '100%', width: '100%' }, title: 'Hi there!' }}
              data-uid={'scene-0'}
            />
          </Storyboard>
        )
      }
      `,
    )
  })

  it('handles a component with a props object written by someone that wants to watch the world burn', () => {
    testCanvasRender(
      null,
      `/** @jsx jsx */
      import * as React from 'react'
      import { View, jsx, Storyboard, Scene } from 'utopia-api'

      const Card = ({style, titles: [title1, , {title: title2}]}) => <div style={style} data-uid={'xxx'}>{title1} - {title2}</div>
      export var App = (props) => {
        return (
          <View
            style={{ ...(props.style || {}), backgroundColor: '#FFFFFF' }}
            layout={{ layoutSystem: 'pinSystem' }}
            data-uid={'aaa'}
          >
            <Card data-uid={'bbb'} style={{backgroundColor: '#000000'}} titles={[props.title, 'ignored', {title: 'and hello!'}]} />
          </View>
        )
      }
      export var ${BakedInStoryboardVariableName} = (props) => {
        return (
          <Storyboard data-uid={'${BakedInStoryboardUID}'}>
            <Scene
              style={{ height: 200, left: 59, width: 200, top: 79 }}
              component={App}
              layout={{ layoutSystem: 'pinSystem' }}
              props={{ style: { height: '100%', width: '100%' }, title: 'Hi there!' }}
              data-uid={'scene-0'}
            />
          </Storyboard>
        )
      }
      `,
    )
  })

  it('handles a component with a props object written by someone that wants to watch the world burn and also loves defaults', () => {
    testCanvasRender(
      null,
      `/** @jsx jsx */
      import * as React from 'react'
      import { View, jsx, Storyboard, Scene } from 'utopia-api'

      const Card = ({style, titles: [title1, , {title: title2}, title3 = 'Now begone!']}) => <div style={style} data-uid={'xxx'}>{title1} - {title2} - {title3}</div>
      export var App = (props) => {
        return (
          <View
            style={{ ...(props.style || {}), backgroundColor: '#FFFFFF' }}
            layout={{ layoutSystem: 'pinSystem' }}
            data-uid={'aaa'}
          >
            <Card data-uid={'bbb'} style={{backgroundColor: '#000000'}} titles={[props.title, 'ignored', {title: 'and hello!'}]} />
          </View>
        )
      }
      export var ${BakedInStoryboardVariableName} = (props) => {
        return (
          <Storyboard data-uid={'${BakedInStoryboardUID}'}>
            <Scene
              style={{ height: 200, left: 59, width: 200, top: 79 }}
              component={App}
              layout={{ layoutSystem: 'pinSystem' }}
              props={{ style: { height: '100%', width: '100%' }, title: 'Hi there!' }}
              data-uid={'scene-0'}
            />
          </Storyboard>
        )
      }
      `,
    )
  })

  it('handles a component that is not imported by throwing a ReferenceError', () => {
    testCanvasError(
      null,
      `/** @jsx jsx */
    import * as React from "react"
    import { View, jsx, Storyboard, Scene } from 'utopia-api'

    export var App = props => <MyCard data-uid={'bbb'} />
    export var ${BakedInStoryboardVariableName} = (props) => {
      return (
        <Storyboard data-uid={'${BakedInStoryboardUID}'}>
          <Scene
            style={{ left: 0, top: 0, width: 400, height: 400 }}
            component={App}
            layout={{ layoutSystem: 'pinSystem' }}
            props={{ layout: { bottom: 0, left: 0, right: 0, top: 0 } }}
            data-uid={'scene-aaa'}
          />
        </Storyboard>
      )
    }
    `,
    )
  })
  it('handles an undefined component gracefully', () => {
    testCanvasRender(
      null,
      `/** @jsx jsx */
      import * as React from "react"
      import { View, jsx, Storyboard, Scene } from 'utopia-api'

      const MyCard = undefined
      export var App = props => <MyCard data-uid={'bbb'} />
      export var ${BakedInStoryboardVariableName} = (props) => {
        return (
          <Storyboard data-uid={'${BakedInStoryboardUID}'}>
            <Scene
              style={{ left: 0, top: 0, width: 400, height: 400 }}
              component={App}
              layout={{ layoutSystem: 'pinSystem' }}
              props={{ layout: { bottom: 0, left: 0, right: 0, top: 0 } }}
              data-uid={'scene-aaa'}
            />
          </Storyboard>
        )
      }
      `,
    )
  })
  it('renders a component used in an arbitrary block correctly', () => {
    testCanvasRender(
      null,
      `/** @jsx jsx */
      import * as React from "react"
      import { View, jsx, Storyboard, Scene } from 'utopia-api'

       const MyCard = (props) => <div data-uid={'xxx'}>{props.title} </div>
       export var App = props => {
         return <View data-uid={'aaa'}>
           {[1,2,3].map(n => (
             <MyCard data-uid={'bbb'} title={'n' + n} />
           ))}
         </View>
       }
       export var ${BakedInStoryboardVariableName} = (props) => {
        return (
          <Storyboard data-uid={'${BakedInStoryboardUID}'}>
            <Scene
              style={{ left: 0, top: 0, width: 400, height: 400 }}
              component={App}
              layout={{ layoutSystem: 'pinSystem' }}
              props={{ layout: { bottom: 0, left: 0, right: 0, top: 0 } }}
              data-uid={'scene-aaa'}
            />
          </Storyboard>
        )
      }
      `,
    )
  })
  it('renders a component used in an arbitrary block correctly, with an HTML element name as a parameter name', () => {
    testCanvasRender(
      null,
      `/** @jsx jsx */
      import * as React from "react"
      import { View, jsx, Storyboard, Scene } from 'utopia-api'

       const MyCard = (props) => <div data-uid={'xxx'}>{props.title} </div>
       export var App = props => {
         return <View data-uid={'aaa'}>
           {[1,2,3].map(div => (
             <MyCard data-uid={'bbb'} title={'n' + div} />
           ))}
         </View>
       }
       export var ${BakedInStoryboardVariableName} = (props) => {
        return (
          <Storyboard data-uid={'${BakedInStoryboardUID}'}>
            <Scene
              style={{ left: 0, top: 0, width: 400, height: 400 }}
              component={App}
              layout={{ layoutSystem: 'pinSystem' }}
              props={{ layout: { bottom: 0, left: 0, right: 0, top: 0 } }}
              data-uid={'scene-aaa'}
            />
          </Storyboard>
        )
      }
       `,
    )
  })
  it('supports passing down the scope to children of components', () => {
    testCanvasRender(
      null,
      `/** @jsx jsx */
      import * as React from "react"
      import { View, jsx, Storyboard, Scene } from 'utopia-api'

       export var App = (props) => {
         return <View data-uid={'aaa'}>
           { [1, 2, 3].map(n => {
             return <div data-uid={'bbb'}>
               <div data-uid={'ccc'}>{n}</div>
             </div>
           })}
         </View>
       }
       export var ${BakedInStoryboardVariableName} = (props) => {
        return (
          <Storyboard data-uid={'${BakedInStoryboardUID}'}>
            <Scene
              style={{ left: 0, top: 0, width: 400, height: 400 }}
              component={App}
              layout={{ layoutSystem: 'pinSystem' }}
              props={{ layout: { bottom: 0, left: 0, right: 0, top: 0 } }}
              data-uid={'scene-aaa'}
            />
          </Storyboard>
        )
      }
       `,
    )
  })
  it('renders a component used in an arbitrary block with eye-stabbingly awful nested destructuring correctly', () => {
    testCanvasRender(
      null,
      `/** @jsx jsx */
      import * as React from "react"
      import { View, jsx, Storyboard, Scene } from 'utopia-api'

       const MyCard = (props) => <div data-uid={'xxx'}>{props.title} </div>
       export var App = props => {
         const nestedThings = [ {a: { b: { c: [ 1 ] } } }, {a: { b: { c: [ 2 ] } } }, {a: { b: { c: [ 3 ] } } } ]
         return <View data-uid={'aaa'}>
           {nestedThings.map(({ a: { b: { c: [ n ] } } }) => (
             <MyCard data-uid={'bbb'} title={'n' + n} />
           ))}
         </View>
       }
       export var ${BakedInStoryboardVariableName} = (props) => {
        return (
          <Storyboard data-uid={'${BakedInStoryboardUID}'}>
            <Scene
              style={{ left: 0, top: 0, width: 400, height: 400 }}
              component={App}
              layout={{ layoutSystem: 'pinSystem' }}
              props={{ layout: { bottom: 0, left: 0, right: 0, top: 0 } }}
              data-uid={'scene-aaa'}
            />
          </Storyboard>
        )
      }`,
    )
  })
  xit('renders a component used in an arbitrary block with even more eye-stabbingly awful nested destructuring correctly', () => {
    // FIXME Nested array destructuring doesn't work
    testCanvasRender(
      null,
      `/** @jsx jsx */
      import * as React from "react"
      import { View, jsx, Storyboard, Scene } from 'utopia-api'

       const MyCard = (props) => <div data-uid={'xxx'}>{props.title} </div>
       export var App = props => {
         const nestedThings = [ {a: { b: { c: [ [ { d: [ 1 ] } ] ] } } }, {a: { b: { c: [ [ { d: [ 2 ] } ] ] } } }, {a: { b: { c: [ [ { d: [ 3 ] } ] ] } } } ]
         return <View data-uid={'aaa'}>
           {nestedThings.map(({ a: { b: { c: [ [ { d: [ n ] } ] ] } } }) => (
             <MyCard data-uid={'bbb'} title={'n' + n} />
           ))}
         </View>
       }
       export var ${BakedInStoryboardVariableName} = (props) => {
        return (
          <Storyboard data-uid={'${BakedInStoryboardUID}'}>
            <Scene
              style={{ left: 0, top: 0, width: 400, height: 400 }}
              component={App}
              layout={{ layoutSystem: 'pinSystem' }}
              props={{ layout: { bottom: 0, left: 0, right: 0, top: 0 } }}
              data-uid={'scene-aaa'}
            />
          </Storyboard>
        )
      }
       `,
    )
  })
  it('renders a 1st party component with uids correctly, using the passed uid instead inside App', () => {
    testCanvasRender(
      null,
      `/** @jsx jsx */
      import * as React from 'react'
      import { View, jsx, Storyboard, Scene } from 'utopia-api'

      export var Inner = (props) => {
        return (
          <View style={{ ...props.style, backgroundColor: '#DDDDDD' }} data-uid={'6be'}>
            <View
              style={{ backgroundColor: '#000000' }}
              data-uid={'d03'}
              layout={{ left: 10, top: 10, right: 10, height: 30 }}
            />
            {props.children}
            <View
              style={{ backgroundColor: '#000000' }}
              data-uid={'41e'}
              layout={{ left: 10, top: 100, right: 10, height: 30 }}
            />
          </View>
        )
      }
      export var App = (props) => {
        return (
          <View
            layout={{ left: 0, top: 0, right: 0, bottom: 0 }}
            style={{ position: 'absolute', backgroundColor: 'lightgrey' }}
            data-uid={'aaa'}
          >
            <Inner data-uid={'d59'} layout={{ left: 28, top: 27, width: 221, height: 348 }}>
              <View
                style={{ backgroundColor: '#fff' }}
                layout={{ left: 14, top: 21, width: 193, height: 244 }}
                data-uid={'dd5'}
              />
            </Inner>
          </View>
        )
      }
      export var ${BakedInStoryboardVariableName} = (props) => {
        return (
          <Storyboard data-uid={'${BakedInStoryboardUID}'}>
            <Scene
              style={{ left: 0, top: 0, width: 400, height: 400 }}
              component={App}
              layout={{ layoutSystem: 'pinSystem' }}
              props={{ layout: { bottom: 0, left: 0, right: 0, top: 0 } }}
              data-uid={'scene-aaa'}
            />
          </Storyboard>
        )
      }
      `,
    )
  })

  it('class component is available from arbitrary block in JSX element', () => {
    testCanvasRender(
      null,
      `/** @jsx jsx */
      import { jsx, Storyboard, Scene } from 'utopia-api'
      import * as React from "react"
       class Thing extends React.Component {
         render() {
           return <div data-uid="ccc">Thing</div>
         }
       }
       export var App = (props) => {
         return <div data-uid="zzz">
           <Thing data-uid="aaa" />
           <Thing data-uid="bbb" />
         </div>
       }
       export var ${BakedInStoryboardVariableName} = (props) => {
        return (
          <Storyboard data-uid={'${BakedInStoryboardUID}'}>
            <Scene
              style={{ left: 0, top: 0, width: 400, height: 400 }}
              component={App}
              layout={{ layoutSystem: 'pinSystem' }}
              props={{ layout: { bottom: 0, left: 0, right: 0, top: 0 } }}
              data-uid={'scene-aaa'}
            />
          </Storyboard>
        )
      }
       `,
    )
  })
  it('function component is available from arbitrary block in JSX element', () => {
    testCanvasRender(
      null,
      `/** @jsx jsx */
      import { jsx, Storyboard, Scene } from 'utopia-api'
      import * as React from "react"
       const Thing = (props) => <div data-uid="ccc">Thing</div>
       export var App = (props) => {
         return <div data-uid="zzz">
           <Thing data-uid="aaa" />
           <Thing data-uid="bbb" />
         </div>
       }
       export var ${BakedInStoryboardVariableName} = (props) => {
        return (
          <Storyboard data-uid={'${BakedInStoryboardUID}'}>
            <Scene
              style={{ left: 0, top: 0, width: 400, height: 400 }}
              component={App}
              layout={{ layoutSystem: 'pinSystem' }}
              props={{ layout: { bottom: 0, left: 0, right: 0, top: 0 } }}
              data-uid={'scene-aaa'}
            />
          </Storyboard>
        )
      }
       `,
    )
  })
  it('function component works inside a map', () => {
    testCanvasRender(
      null,
      `/** @jsx jsx */
      import { jsx, Storyboard, Scene } from 'utopia-api'
      import * as React from "react"
       export const Thing = (props) => <div data-uid="ccc" >Thing</div>
       export var App = (props) => {
         return <div data-uid="zzz">
            {[1, 2].map((data) => {
              return (<Thing data-uid="aaa" />)
            })}
         </div>
       }
       export var ${BakedInStoryboardVariableName} = (props) => {
        return (
          <Storyboard data-uid={'${BakedInStoryboardUID}'}>
            <Scene
              style={{ left: 0, top: 0, width: 400, height: 400 }}
              component={App}
              layout={{ layoutSystem: 'pinSystem' }}
              props={{ layout: { bottom: 0, left: 0, right: 0, top: 0 } }}
              data-uid={'scene-aaa'}
            />
          </Storyboard>
        )
      }
       `,
    )
  })
  it('props can be accessed inside the arbitrary js block inside a text range', () => {
    testCanvasRender(
      null,
      `/** @jsx jsx */
      import { jsx, Storyboard, Scene } from 'utopia-api'
      import * as React from "react"
       export const Thing = (props) => <div data-uid="ccc">Hello {props.name}!</div>
       const Thang = (props) => <div data-uid="ddd">Hello {props.name}!</div>

       export var App = (props) => {
         return <div data-uid="zzz">
           <Thing data-uid="aaa" name='World!'/>
           <Thang data-uid="bbb" name='Dolly!'/>
         </div>
       }
       export var ${BakedInStoryboardVariableName} = (props) => {
        return (
          <Storyboard data-uid={'${BakedInStoryboardUID}'}>
            <Scene
              style={{ left: 0, top: 0, width: 400, height: 400 }}
              component={App}
              layout={{ layoutSystem: 'pinSystem' }}
              props={{ layout: { bottom: 0, left: 0, right: 0, top: 0 } }}
              data-uid={'scene-aaa'}
            />
          </Storyboard>
        )
      }
       `,
    )
  })
  it('arbitrary jsx block inside an element inside an arbitrary jsx block', () => {
    testCanvasRender(
      null,
      `/** @jsx jsx */
      import { jsx, Storyboard, Scene } from 'utopia-api'
      import * as React from "react"
       export var App = (props) => {
         return <div data-uid="zzz">
           {[1, 2, 3].map((n) => {
             return <div data-uid="aaa">{n}</div>
           })}
         </div>
       }
       export var ${BakedInStoryboardVariableName} = (props) => {
        return (
          <Storyboard data-uid={'${BakedInStoryboardUID}'}>
            <Scene
              style={{ left: 0, top: 0, width: 400, height: 400 }}
              component={App}
              layout={{ layoutSystem: 'pinSystem' }}
              props={{ layout: { bottom: 0, left: 0, right: 0, top: 0 } }}
              data-uid={'scene-aaa'}
            />
          </Storyboard>
        )
      }
       `,
    )
  })
  it('arbitrary jsx block inside an element inside an arbitrary jsx block inside an element inside an arbitrary jsx block', () => {
    testCanvasRender(
      null,
      `/** @jsx jsx */
      import { jsx, Storyboard, Scene } from 'utopia-api'
      import * as React from "react"

       export var App = (props) => {
         return <div data-uid="zzz">
           {[1, 2, 3].map((n) => {
             return <div data-uid="aaa">
               {[4, 5, 6].map((m) => {
                 return <div data-uid="bbb">{n * m}</div>
               })}
             </div>
           })}
         </div>
       }
       export var ${BakedInStoryboardVariableName} = (props) => {
        return (
          <Storyboard data-uid={'${BakedInStoryboardUID}'}>
            <Scene
              style={{ left: 0, top: 0, width: 400, height: 400 }}
              component={App}
              layout={{ layoutSystem: 'pinSystem' }}
              props={{ layout: { bottom: 0, left: 0, right: 0, top: 0 } }}
              data-uid={'scene-aaa'}
            />
          </Storyboard>
        )
      }
       `,
    )
  })
  it('renders img tag', () => {
    testCanvasRender(
      null,
      `/** @jsx jsx */
      import { View, jsx, Storyboard, Scene } from 'utopia-api'
      import * as React from "react"

      export var App = (props) => {
        return <View data-uid={'aaa'}>
        <img data-uid={'bbb'} src="data:image/gif;base64,R0lGODlhAQABAIAAAAUEBAAAACwAAAAAAQABAAACAkQBADs=" />
        </View>
      }
      export var ${BakedInStoryboardVariableName} = (props) => {
        return (
          <Storyboard data-uid={'${BakedInStoryboardUID}'}>
            <Scene
              style={{ left: 0, top: 0, width: 400, height: 400 }}
              component={App}
              layout={{ layoutSystem: 'pinSystem' }}
              props={{ layout: { bottom: 0, left: 0, right: 0, top: 0 } }}
              data-uid={'scene-aaa'}
            />
          </Storyboard>
        )
      }
      `,
    )
  })
  it('the spy wrapper is compatible with React.cloneElement', () => {
    testCanvasRender(
      null,
      `/** @jsx jsx */
      import { jsx, Storyboard, Scene } from 'utopia-api'
      import * as React from "react"
       export const ClonerComponent = (props) => {
        return (
          <div data-uid='cloner-root'>
            {React.Children.map(props.children, (child, index) => {
              if (React.isValidElement(child)) {
                return React.cloneElement(child, {
                  style: { color: 'red', fontWeight: 800 },
                })
              } else {
                return child
              }
            })}
          </div>
        )
      }
      
      export var App = (props) => {
        return (
          <div data-uid='zzz'>
            <ClonerComponent data-uid='cloner'>
              <div data-uid='cloned'>ha</div>
            </ClonerComponent>
          </div>
        )
      }
      export var ${BakedInStoryboardVariableName} = (props) => {
        return (
          <Storyboard data-uid={'${BakedInStoryboardUID}'}>
            <Scene
              style={{ left: 0, top: 0, width: 400, height: 400 }}
              component={App}
              layout={{ layoutSystem: 'pinSystem' }}
              props={{ layout: { bottom: 0, left: 0, right: 0, top: 0 } }}
              data-uid={'scene-aaa'}
            />
          </Storyboard>
        )
      }
      `,
    )
  })

  it('handles chaining dependencies into the appropriate order', () => {
    testCanvasRender(
      null,
      `/** @jsx jsx */
      import { jsx } from 'utopia-api'
      import * as React from 'react'
import { Scene, Storyboard, View } from 'utopia-api'
export var App = (props) => {
  return <Widget data-uid={'bbb'} />
}
export var Widget = (props) => {
  return <View data-uid={'aaa'} />
}
export var ${BakedInStoryboardVariableName} = (props) => {
  return (
    <Storyboard data-uid={'${BakedInStoryboardUID}'}>
      <Scene
        style={{ left: 0, top: 0, width: 400, height: 400 }}
        component={App}
        layout={{ layoutSystem: 'pinSystem' }}
        props={{ layout: { bottom: 0, left: 0, right: 0, top: 0 } }}
        data-uid={'scene-aaa'}
      />
    </Storyboard>
  )
}
`,
    )
  })

  it('respects a jsx pragma', () => {
    testCanvasRender(
      null,
      `/** @jsx myFactoryFunction */
      import * as React from "react"
      import { jsx, Storyboard, Scene } from 'utopia-api'
      const MyComp = (props) => <div>Utopia</div>
      export var App = (props) => {
        return (<MyComp data-uid={'aaa'}/>)
      }

      function myFactoryFunction(type, props, children) {
        const modifiedProps = {...props, 'data-factory-function-works': "true"}
        if (children == null) {
          return React.createElement(type, modifiedProps)
        } else {
          return React.createElement(type, modifiedProps, children)
        }
      }
      export var ${BakedInStoryboardVariableName} = (props) => {
        return (
          <Storyboard data-uid={'${BakedInStoryboardUID}'}>
            <Scene
              style={{ left: 0, top: 0, width: 400, height: 400 }}
              component={App}
              layout={{ layoutSystem: 'pinSystem' }}
              props={{ layout: { bottom: 0, left: 0, right: 0, top: 0 } }}
              data-uid={'scene-aaa'}
            />
          </Storyboard>
        )
      }
      `,
    )
  })

  it('the utopia jsx pragma (and layout prop) works well', () => {
    testCanvasRender(
      null,
      `/** @jsx jsx */
      import { Scene, Storyboard, jsx } from 'utopia-api'
      const MyComp = (props) => <div layout={{ left: 15, top: 15, width: 50, height: 50, flex: 15 }}>Utopia</div>
      export var App = (props) => {
        return (<MyComp data-uid={'aaa'}/>)
      }

      function myFactoryFunction(type, props, children) {
        const modifiedProps = {...props, 'data-factory-function-works': "true"}
        return React.createElement(type, modifiedProps, children)
      }
      export var ${BakedInStoryboardVariableName} = (props) => {
        return (
          <Storyboard data-uid={'${BakedInStoryboardUID}'}>
            <Scene
              style={{ left: 0, top: 0, width: 400, height: 400 }}
              component={App}
              layout={{ layoutSystem: 'pinSystem' }}
              props={{ layout: { bottom: 0, left: 0, right: 0, top: 0 } }}
              data-uid={'scene-aaa'}
            />
          </Storyboard>
        )
      }
      `,
    )
  })

  it('the utopia jsx pragma supports emotion CSS prop', () => {
    testCanvasRender(
      null,
      `/** @jsx jsx */
      import { Scene, Storyboard, jsx } from 'utopia-api'
      const MyComp = (props) => <div layout={{ left: 15, top: 15, width: 50, height: 50, flex: 15 }} css={{ backgroundColor: 'blue' }}>Utopia</div>
      export var App = (props) => {
        return (<MyComp data-uid={'aaa'}/>)
      }

      function myFactoryFunction(type, props, children) {
        const modifiedProps = {...props, 'data-factory-function-works': "true"}
        return React.createElement(type, modifiedProps, children)
      }
      export var ${BakedInStoryboardVariableName} = (props) => {
        return (
          <Storyboard data-uid={'${BakedInStoryboardUID}'}>
            <Scene
              style={{ left: 0, top: 0, width: 400, height: 400 }}
              component={App}
              layout={{ layoutSystem: 'pinSystem' }}
              props={{ layout: { bottom: 0, left: 0, right: 0, top: 0 } }}
              data-uid={'scene-aaa'}
            />
          </Storyboard>
        )
      }
      `,
    )
  })

  xit('mutated variable refers to code component', () => {
    testCanvasRender(
      null,
      `/** @jsx jsx */
      import { Scene, Storyboard, jsx } from 'utopia-api'
      import * as React from "react"
      let MyComp
      MyComp = (props) => <div>Utopia</div>
      export var App = (props) => {
        return (<MyComp />)
      }
      export var ${BakedInStoryboardVariableName} = (props) => {
        return (
          <Storyboard data-uid={'${BakedInStoryboardUID}'}>
            <Scene
              style={{ left: 0, top: 0, width: 400, height: 400 }}
              component={App}
              layout={{ layoutSystem: 'pinSystem' }}
              props={{ layout: { bottom: 0, left: 0, right: 0, top: 0 } }}
              data-uid={'scene-aaa'}
            />
          </Storyboard>
        )
      }
      `,
    )
  })
  it('does not crash if the metadata scenes are not the appropriate value', () => {
    testCanvasRender(
      null,
      `/** @jsx jsx */
      import * as React from "react"
      import { View, jsx, Storyboard, Scene } from 'utopia-api'

       const MyCard = (props) => <div data-uid={'xxx'}>{props.title} </div>
       export var App = props => {
         return <View data-uid={'aaa'}>
           {[1,2,3].map(n => (
             <MyCard data-uid={'bbb'} title={'n' + n} />
           ))}
         </View>
       }
       export var ${BakedInStoryboardVariableName} = (props) => {
        return (
          <Storyboard data-uid={'${BakedInStoryboardUID}'}>
            <Scene
              style={{ left: 0, top: 0, width: 400, height: 400 }}
              component={App}
              layout={{ layoutSystem: 'pinSystem' }}
              props={{ layout: { bottom: 0, left: 0, right: 0, top: 0 } }}
              data-uid={'scene-aaa'}
            />
          </Storyboard>
        )
      }
       `,
    )
  })
  it('does not crash if the metadata scenes are undefined', () => {
    testCanvasRender(
      null,
      `/** @jsx jsx */
      import { View, jsx, Storyboard, Scene } from 'utopia-api'
      import * as React from "react"

       const MyCard = (props) => <div data-uid={'xxx'}>{props.title} </div>
       export var App = props => {
         return <View data-uid={'aaa'}>
           {[1,2,3].map(n => (
             <MyCard data-uid={'bbb'} title={'n' + n} />
           ))}
         </View>
       }
       export var ${BakedInStoryboardVariableName} = (props) => {
        return (
          <Storyboard data-uid={'${BakedInStoryboardUID}'}>
            <Scene
              style={{ left: 0, top: 0, width: 400, height: 400 }}
              component={App}
              layout={{ layoutSystem: 'pinSystem' }}
              props={{ layout: { bottom: 0, left: 0, right: 0, top: 0 } }}
              data-uid={'scene-aaa'}
            />
          </Storyboard>
        )
      }
       `,
    )
  })

  it('Renders input tag without errors', async () => {
    testCanvasRender(
      null,
      `
      /** @jsx jsx */
      import * as React from 'react'
      import { View, jsx, Storyboard, Scene } from 'utopia-api'
      export var InputElement = (props) => <input data-uid={props['data-uid']} style={{top: 10}} />
      export var App = (props) => {
        return <InputElement data-uid={'567'} />
      }
      export var ${BakedInStoryboardVariableName} = (props) => {
        return (
          <Storyboard data-uid={'${BakedInStoryboardUID}'}>
            <Scene
              style={{ left: 0, top: 0, width: 400, height: 400 }}
              component={App}
              layout={{ layoutSystem: 'pinSystem' }}
              props={{ layout: { bottom: 0, left: 0, right: 0, top: 0 } }}
              data-uid={'scene-aaa'}
            />
          </Storyboard>
        )
      }
      `,
    )
  })

  it('Label carried through for normal elements', async () => {
    testCanvasRender(
      null,
      `
      /** @jsx jsx */
      import * as React from 'react'
      import { View, jsx, Storyboard, Scene } from 'utopia-api'
      export var App = (props) => {
        return <div style={{ ...(props.style || {})}} data-uid={'aaa'} data-label={'Hat'} />
      }
      export var ${BakedInStoryboardVariableName} = (props) => {
        return (
          <Storyboard data-uid={'${BakedInStoryboardUID}'}>
            <Scene
              style={{ left: 0, top: 0, width: 400, height: 400 }}
              component={App}
              layout={{ layoutSystem: 'pinSystem' }}
              props={{ layout: { bottom: 0, left: 0, right: 0, top: 0 } }}
              data-uid={'scene-aaa'}
            />
          </Storyboard>
        )
      }
      `,
    )
  })

  it('Label carried through for generated elements', async () => {
    testCanvasRender(
      null,
      `
      /** @jsx jsx */
      import * as React from 'react'
      import { View, jsx, Storyboard, Scene } from 'utopia-api'
      export var App = (props) => {
        return <div style={{ ...(props.style || {})}} data-uid={'aaa'}>
          {[1, 2, 3].map(n => {
            return <div data-uid={'bbb'} data-label={'Plane'} />
          })}
        </div>
      }
      export var ${BakedInStoryboardVariableName} = (props) => {
        return (
          <Storyboard data-uid={'${BakedInStoryboardUID}'}>
            <Scene
              style={{ left: 0, top: 0, width: 400, height: 400 }}
              component={App}
              layout={{ layoutSystem: 'pinSystem' }}
              props={{ layout: { bottom: 0, left: 0, right: 0, top: 0 } }}
              data-uid={'scene-aaa'}
            />
          </Storyboard>
        )
      }
      `,
    )
  })

  it('console logging does not do anything bizarre', async () => {
    testCanvasRender(
      null,
      `
      /** @jsx jsx */
      import * as React from 'react'
      import { View, jsx, Storyboard, Scene } from 'utopia-api'
      console.log('root log')
      export var App = (props) => {
        console.log('inside component log')
        return <div style={{ ...(props.style || {})}} data-uid={'aaa'} />
      }
      export var ${BakedInStoryboardVariableName} = (props) => {
        return (
          <Storyboard data-uid={'${BakedInStoryboardUID}'}>
            <Scene
              style={{ left: 0, top: 0, width: 400, height: 400 }}
              component={App}
              layout={{ layoutSystem: 'pinSystem' }}
              props={{ layout: { bottom: 0, left: 0, right: 0, top: 0 } }}
              data-uid={'scene-aaa'}
            />
          </Storyboard>
        )
      }
      `,
    )
  })

  it('refs are handled and triggered correctly in a functional component', async () => {
    testCanvasRender(
      null,
      `
      /** @jsx jsx */
      import * as React from 'react'
      import { View, jsx, Storyboard, Scene } from 'utopia-api'
      export var App = (props) => {
        return <div ref={() => console.log('functional component')} style={{ ...(props.style || {})}} data-uid={'aaa'} />
      }
      export var ${BakedInStoryboardVariableName} = (props) => {
        return (
          <Storyboard data-uid={'${BakedInStoryboardUID}'}>
            <Scene
              style={{ left: 0, top: 0, width: 400, height: 400 }}
              component={App}
              layout={{ layoutSystem: 'pinSystem' }}
              props={{ layout: { bottom: 0, left: 0, right: 0, top: 0 } }}
              data-uid={'scene-aaa'}
            />
          </Storyboard>
        )
      }
      `,
    )
  })

  it('refs are handled and triggered correctly in a class component', async () => {
    testCanvasRender(
      null,
      `
      /** @jsx jsx */
      import * as React from 'react'
      import { View, jsx, Storyboard, Scene } from 'utopia-api'
      class Thing extends React.Component {
        render() {
          return <div data-uid="ccc">Thing</div>
        }
      }
      export var App = (props) => {
        return <Thing ref={() => console.log('class component')} data-uid={'aaa'} />
      }
      export var ${BakedInStoryboardVariableName} = (props) => {
        return (
          <Storyboard data-uid={'${BakedInStoryboardUID}'}>
            <Scene
              style={{ left: 0, top: 0, width: 400, height: 400 }}
              component={App}
              layout={{ layoutSystem: 'pinSystem' }}
              props={{ layout: { bottom: 0, left: 0, right: 0, top: 0 } }}
              data-uid={'scene-aaa'}
            />
          </Storyboard>
        )
      }
      `,
    )
  })

  it('class components have working contexts', async () => {
    const printedDom = testCanvasRenderInline(
      null,
      `
      /** @jsx jsx */
      import * as React from 'react'
      import { View, jsx, Storyboard, Scene } from 'utopia-api'

      const MyContext = React.createContext({ textToShow: 'hello' });

      class Thing extends React.Component {
        render() {
          const { textToShow } = this.context;
          return <div data-uid="ccc">{textToShow}</div>
        }
      }
      Thing.contextType = MyContext;
      export var App = (props) => {
        return <Thing ref={() => console.log('class component')} data-uid={'aaa'} />
      }
      export var ${BakedInStoryboardVariableName} = (props) => {
        return (
          <Storyboard data-uid={'${BakedInStoryboardUID}'}>
            <Scene
              style={{ left: 0, top: 0, width: 400, height: 400 }}
              component={App}
              layout={{ layoutSystem: 'pinSystem' }}
              props={{ layout: { bottom: 0, left: 0, right: 0, top: 0 } }}
              data-uid={'scene-aaa'}
            />
          </Storyboard>
        )
      }
      `,
    )
    expect(printedDom).toMatchInlineSnapshot(`
      "<div
        id=\\"canvas-container\\"
        style=\\"
          all: initial;
          position: absolute;
          zoom: 100%;
          transform: translate3d(0px, 0px, 0);
        \\"
      >
        <div
          data-utopia-scene-id=\\"utopia-storyboard-uid/scene-aaa\\"
          data-utopia-valid-paths=\\"utopia-storyboard-uid/scene-aaa:aaa\\"
          style=\\"
            position: absolute;
            width: 400px;
            height: 400px;
            left: 0;
            top: 0;
            background-color: rgba(255, 255, 255, 1);
            box-shadow: 0px 0px 1px 0px rgba(26, 26, 26, 0.3);
          \\"
        >
          <div data-uid=\\"aaa\\">hello</div>
        </div>
      </div>
      "
    `)
  })

  it('handles fragments in an arbitrary block', () => {
    testCanvasRender(
      null,
      `/** @jsx jsx */
import * as React from 'react'
import { View, jsx, Scene, Storyboard } from 'utopia-api'
const keyboardShortcuts = [
  {
    label: 'Copy',
    macKeys: ['⌘', '⎇', 'C'],
  },
  {
    label: 'Paste',
    macKeys: '⌘⎇V',
  },
  {
    label: 'Cut',
    macKeys: '⌘⎇C',
  },
]
export var KeyboardShortcut = (props) => {
  return (
    <span data-uid={'6a8'}>
      {[...props.shortcut].map((keyb) => (
        <span
          style={{
            padding: 6,
          }}
          data-uid={'726'}
        >
          {keyb}
        </span>
      ))}
    </span>
  )
}
export var Grid = (props) => {
  return (
    <div
      style={{
        display: 'grid',
        gridTemplateColumns: '2fr 1fr',
        gridAutoRows: '31px',
      }}
      data-uid={'eb1'}
    >
      {props.children}
    </div>
  )
}
export var App = (props) => {
  return (
    <View
      style={{
        ...(props.style || {}),
        fontSize: '12px',
        fontFamily: 'Inter',
        color: '#237AFF',
        left: 0,
        top: 0,
        width: 376,
        height: 812,
        backgroundColor: '#171111',
      }}
      layout={{ layoutSystem: 'pinSystem' }}
      data-uid={'aaa'}
    >
      <Grid data-uid={'03a'}>
        {keyboardShortcuts.map((pair) => (
          <>
            <div data-uid={'834'} data-label={pair.label}>
              {pair.label}
            </div>
            <div data-uid={'999'} data-label={pair.macKeys}>
              <KeyboardShortcut data-uid={'000'} shortcut={pair.macKeys}></KeyboardShortcut>
            </div>
          </>
        ))}
      </Grid>
    </View>
  )
}
export var ${BakedInStoryboardVariableName} = (props) => {
  return (
    <Storyboard data-uid={'${BakedInStoryboardUID}'}>
      <Scene
        style={{ left: 0, top: 0, width: 375, height: 812 }}
        component={App}
        layout={{ layoutSystem: 'pinSystem' }}
        props={{ layout: { bottom: 0, left: 0, right: 0, top: 0 } }}
        data-uid={'scene-aaa'}
      />
    </Storyboard>
  )
}
    `,
    )
  })
})

describe('UiJsxCanvas runtime errors', () => {
  it('throws an error!', () => {
    testCanvasError(
      null,
      `
import * as React from "react"
import { View, jsx, Storyboard, Scene } from 'utopia-api'
const MyComp = (props) => <div data-uid={'bbb'}>Utopia</div>
a.a // 16,1 this shall throw an error!
export var App = (props) => {
  return (<MyComp data-uid={'aaa'}/>)
}
export var ${BakedInStoryboardVariableName} = (props) => {
  return (
    <Storyboard data-uid={'${BakedInStoryboardUID}'}>
      <Scene
        style={{ left: 0, top: 0, width: 400, height: 400 }}
        component={App}
        layout={{ layoutSystem: 'pinSystem' }}
        props={{ layout: { bottom: 0, left: 0, right: 0, top: 0 } }}
        data-uid={'scene-aaa'}
      />
    </Storyboard>
  )
}
`,
    )
  })
  it('an arbitrary jsx child has correct source map', () => {
    testCanvasError(
      null,
      `
import * as React from "react"
import { View, jsx, Storyboard, Scene } from 'utopia-api'
const MyComp = (props) => <div data-uid={'bbb'}>Utopia</div>

export var App = (props) => {
  return (
    <MyComp data-uid={'aaa'}>
      {'hello' + a.a /* 20,18 */}
    </MyComp>
  )
}
export var ${BakedInStoryboardVariableName} = (props) => {
  return (
    <Storyboard data-uid={'${BakedInStoryboardUID}'}>
      <Scene
        style={{ left: 0, top: 0, width: 400, height: 400 }}
        component={App}
        layout={{ layoutSystem: 'pinSystem' }}
        props={{ layout: { bottom: 0, left: 0, right: 0, top: 0 } }}
        data-uid={'scene-aaa'}
      />
    </Storyboard>
  )
}
`,
    )
  })

  it('an arbitrary jsx child has correct source map even if the entire expression is broken', () => {
    testCanvasError(
      null,
      `
import * as React from "react"
import { View, jsx, Storyboard, Scene } from 'utopia-api'
const MyComp = (props) => <div data-uid={'bbb'}>Utopia</div>

export var App = (props) => {
  return (
    <MyComp data-uid={'aaa'}>
      {a.a /* 20,8 */}
    </MyComp>
  )
}
  export var ${BakedInStoryboardVariableName} = (props) => {
    return (
      <Storyboard data-uid={'${BakedInStoryboardUID}'}>
        <Scene
          style={{ left: 0, top: 0, width: 400, height: 400 }}
          component={App}
          layout={{ layoutSystem: 'pinSystem' }}
          props={{ layout: { bottom: 0, left: 0, right: 0, top: 0 } }}
          data-uid={'scene-aaa'}
        />
      </Storyboard>
    )
  }
`,
    )
  })

  it('an arbitrary jsx attribute has correct source map', () => {
    testCanvasError(
      null,
      `
import * as React from "react"
import { View, jsx, Storyboard, Scene } from 'utopia-api'
const MyComp = (props) => <div data-uid={'bbb'}>Utopia</div>

export var App = (props) => {
  return (
    <MyComp data-uid={'aaa'} someAttribute={'hello' + a.a /* 19,55 */}>
      hello!
    </MyComp>
  )
}
export var ${BakedInStoryboardVariableName} = (props) => {
  return (
    <Storyboard data-uid={'${BakedInStoryboardUID}'}>
      <Scene
        style={{ left: 0, top: 0, width: 400, height: 400 }}
        component={App}
        layout={{ layoutSystem: 'pinSystem' }}
        props={{ layout: { bottom: 0, left: 0, right: 0, top: 0 } }}
        data-uid={'scene-aaa'}
      />
    </Storyboard>
  )
}
`,
    )
  })

  it('an arbitrary jsx attribute has correct source map even if the entire expression is broken', () => {
    testCanvasError(
      null,
      `
import * as React from "react"
import { View, jsx, Storyboard, Scene } from 'utopia-api'
const MyComp = (props) => <div data-uid={'bbb'}>Utopia</div>

export var App = (props) => {
  return (
    <MyComp data-uid={'aaa'} someAttribute={a.a /* 19,45 */}>
      hello!
    </MyComp>
  )
}
export var ${BakedInStoryboardVariableName} = (props) => {
  return (
    <Storyboard data-uid={'${BakedInStoryboardUID}'}>
      <Scene
        style={{ left: 0, top: 0, width: 400, height: 400 }}
        component={App}
        layout={{ layoutSystem: 'pinSystem' }}
        props={{ layout: { bottom: 0, left: 0, right: 0, top: 0 } }}
        data-uid={'scene-aaa'}
      />
    </Storyboard>
  )
}
`,
    )
  })

  it('arbitrary at the top of a component has correct source map', () => {
    testCanvasError(
      null,
      `
import * as React from "react"
import { View, jsx, Storyboard, Scene } from 'utopia-api'
const MyComp = (props) => <div data-uid={'bbb'}>Utopia</div>

export var App = (props) => {
  '5' + a.a // 18,9
  return (
    <MyComp data-uid={'aaa'}>
      hello!
    </MyComp>
  )
}
export var ${BakedInStoryboardVariableName} = (props) => {
  return (
    <Storyboard data-uid={'${BakedInStoryboardUID}'}>
      <Scene
        style={{ left: 0, top: 0, width: 400, height: 400 }}
        component={App}
        layout={{ layoutSystem: 'pinSystem' }}
        props={{ layout: { bottom: 0, left: 0, right: 0, top: 0 } }}
        data-uid={'scene-aaa'}
      />
    </Storyboard>
  )
}
`,
    )
  })

  it('arbitrary at the top of a component has correct source map even if the entire expression is broken', () => {
    testCanvasError(
      null,
      `
import * as React from "react"
import { View, jsx, Storyboard, Scene } from 'utopia-api'
const MyComp = (props) => <div data-uid={'bbb'}>Utopia</div>

export var App = (props) => {
  a.a // 18,3
  return (
    <MyComp data-uid={'aaa'}>
      hello!
    </MyComp>
  )
}
export var ${BakedInStoryboardVariableName} = (props) => {
  return (
    <Storyboard data-uid={'${BakedInStoryboardUID}'}>
      <Scene
        style={{ left: 0, top: 0, width: 400, height: 400 }}
        component={App}
        layout={{ layoutSystem: 'pinSystem' }}
        props={{ layout: { bottom: 0, left: 0, right: 0, top: 0 } }}
        data-uid={'scene-aaa'}
      />
    </Storyboard>
  )
}
`,
    )
  })
})
