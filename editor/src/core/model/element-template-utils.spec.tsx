/* eslint jest/expect-expect: ["error", { "assertFunctionNames": ["expect", "expectElementAtPathHasMatchingUID*", "expectElementFoundNull"] }] */

import {
  jsExpressionValue,
  isJSXAttributeValue,
  jsExpressionFunctionCall,
  UtopiaJSXComponent,
  JSXElement,
  jsxElement,
  utopiaJSXComponent,
  defaultPropsParam,
  jsxAttributesFromMap,
  getJSXAttribute,
  isJSXElement,
  emptyComments,
  isJSXConditionalExpression,
} from '../shared/element-template'
import {
  componentHonoursPropsPosition,
  componentHonoursPropsSize,
  componentUsesProperty,
  findJSXElementChildAtPath,
  guaranteeUniqueUids,
  insertJSXElementChild,
  rearrangeJsxChildren,
  removeJSXElementChild,
} from './element-template-utils'
import Utils from '../../utils/utils'
import { BakedInStoryboardUID } from './scene-utils'
import { testStaticElementPath } from '../shared/element-path.test-utils'
import { getComponentFromCode } from './element-template.test-utils'
import {
  createTestProjectWithCode,
  getParseSuccessForStoryboardCode,
} from '../../sample-projects/sample-project-utils.test-utils'
import {
  makeTestProjectCodeWithSnippet,
  testPrintCodeFromParseSuccess,
} from '../../components/canvas/ui-jsx.test-utils'
import {
  modifyParseSuccessWithSimple,
  StoryboardFilePath,
} from '../../components/editor/store/editor-state'
import { ElementPath, ParseSuccess, StaticElementPath } from '../shared/project-file-types'
import * as EP from '../shared/element-path'
import {
  dynamicPathToStaticPath,
  fromString,
  fromStringStatic,
  toUid,
} from '../shared/element-path'
import { getComponentsFromTopLevelElements } from './project-file-utils'
import { setFeatureForUnitTests } from '../../utils/utils.test-utils'
import { getUtopiaID } from '../shared/uid-utils'
import { getContentsTreeFileFromString } from '../../components/assets'
import { childInsertionPath } from '../../components/editor/store/insertion-path'

describe('guaranteeUniqueUids', () => {
  it('if two siblings have the same ID, one will be replaced', () => {
    const exampleElements = [
      jsxElement(
        'View',
        'aaa',
        jsxAttributesFromMap({ 'data-uid': jsExpressionValue('aaa', emptyComments, 'aaa') }),
        [],
      ),
      jsxElement(
        'View',
        'aaa',
        jsxAttributesFromMap({ 'data-uid': jsExpressionValue('aaa', emptyComments, 'aaa') }),
        [],
      ),
    ]
    const fixedElements = guaranteeUniqueUids(exampleElements, [])

    const child0Props = Utils.pathOr([], [0, 'props'], fixedElements)
    const child0UID = getJSXAttribute(child0Props, 'data-uid')
    expect(child0UID).toEqual(jsExpressionValue('aaa', emptyComments, 'aaa'))
    const child1Props = Utils.pathOr([], [1, 'props'], fixedElements)
    const child1UID = getJSXAttribute(child1Props, 'data-uid')
    expect(child1UID).not.toEqual(jsExpressionValue('aaa', emptyComments, 'aaa'))
  })

  it('if an element has an existing value, it will be replaced', () => {
    const exampleElements = [
      jsxElement(
        'View',
        'aaa',
        jsxAttributesFromMap({ 'data-uid': jsExpressionValue('aaa', emptyComments, 'aaa') }),
        [],
      ),
      jsxElement(
        'View',
        'aab',
        jsxAttributesFromMap({ 'data-uid': jsExpressionValue('aab', emptyComments, 'aab') }),
        [],
      ),
    ]
    const existingIDs = ['aab', 'bbb']
    const fixedElements = guaranteeUniqueUids(exampleElements, existingIDs)

    const child0Props = Utils.pathOr([], [0, 'props'], fixedElements)
    const child0UID = getJSXAttribute(child0Props, 'data-uid')
    const child1Props = Utils.pathOr([], [1, 'props'], fixedElements)
    const child1UID = getJSXAttribute(child1Props, 'data-uid')
    expect(child0UID).toEqual(jsExpressionValue('aaa', emptyComments, 'aaa'))
    expect(child1UID).not.toEqual(jsExpressionValue('aab', emptyComments, 'aab'))
  })

  it('if the uid prop is not a simple value, replace it with a simple value', () => {
    const exampleElement = jsxElement(
      'View',
      '',
      jsxAttributesFromMap({ 'data-uid': jsExpressionFunctionCall('someFunction', []) }),
      [],
    )
    const fixedElements = guaranteeUniqueUids([exampleElement], [])
    const fixedElement = fixedElements[0]
    const fixedElementProps = Utils.pathOr([], ['props'], fixedElement)
    const fixedElementUIDProp = getJSXAttribute(fixedElementProps, 'data-uid')
    if (fixedElementUIDProp == null) {
      throw new Error('Unable to find uid for element.')
    } else if (isJSXAttributeValue(fixedElementUIDProp)) {
      const fixedElementUID = (fixedElement as JSXElement).uid
      expect(fixedElementUID).toEqual(fixedElementUIDProp.value)
      expect(fixedElementUID).not.toEqual('')
    } else {
      throw new Error('fixedElementUIDProp is not a simple value')
    }
  })
})

describe('getUtopiaID', () => {
  it('returns an id if there is one', () => {
    const element = jsxElement(
      'View',
      'hello',
      jsxAttributesFromMap({ 'data-uid': jsExpressionValue('hello', emptyComments) }),
      [],
    )
    const id = getUtopiaID(element as JSXElement)
    expect(id).toEqual('hello')
  })
})

describe('removeJSXElementChild', () => {
  const utopiaComponents: UtopiaJSXComponent[] = [
    utopiaJSXComponent(
      'test1',
      true,
      'var',
      'block',
      defaultPropsParam,
      [],
      jsxElement(
        'View',
        'aaa',
        jsxAttributesFromMap({
          'data-uid': jsExpressionValue('aaa', emptyComments),
          prop1: jsExpressionValue(5, emptyComments),
        }),
        [],
      ),
      null,
      false,
      emptyComments,
    ),
    utopiaJSXComponent(
      'test2WithChildren',
      true,
      'var',
      'block',
      defaultPropsParam,
      [],
      jsxElement(
        'View',
        'aab',
        jsxAttributesFromMap({
          'data-uid': jsExpressionValue('aab', emptyComments),
          prop2: jsExpressionValue(15, emptyComments),
        }),
        [
          jsxElement(
            'View',
            'aac',
            jsxAttributesFromMap({ 'data-uid': jsExpressionValue('aac', emptyComments) }),
            [],
          ),
          jsxElement(
            'View',
            'aad',
            jsxAttributesFromMap({
              'data-uid': jsExpressionValue('aad', emptyComments),
              prop3: jsExpressionValue(100, emptyComments),
            }),
            [],
          ),
          jsxElement(
            'View',
            'aae',
            jsxAttributesFromMap({ 'data-uid': jsExpressionValue('aae', emptyComments) }),
            [],
          ),
        ],
      ),
      null,
      false,
      emptyComments,
    ),
  ]
  xit('removes a root element', () => {
    // TODO Scene Implementation
    const updatedElements = removeJSXElementChild(
      testStaticElementPath([[BakedInStoryboardUID, 'scene-aaa'], ['aaa']]),
      utopiaComponents,
    )
    expect(updatedElements.length).toEqual(1)
    expect(updatedElements[0]).toEqual(utopiaComponents[1])
  })
  it('removes a non-root element', () => {
    const updatedElements = removeJSXElementChild(
      testStaticElementPath([
        [BakedInStoryboardUID, 'scene-aaa'],
        ['aab', 'aac'],
      ]),
      utopiaComponents,
    )
    const expectedResult = utopiaComponents.map((component, index) => {
      if (index === 1) {
        const rootElement = component.rootElement
        if (isJSXElement(rootElement)) {
          return {
            ...component,
            rootElement: {
              ...rootElement,
              children: [rootElement.children[1], rootElement.children[2]],
            },
          }
        } else {
          return component
        }
      } else {
        return component
      }
    })
    expect(updatedElements).toEqual(expectedResult)
  })
})

describe('componentUsesProperty', () => {
  it('returns false for a component that in no way uses the children property', () => {
    const component = getComponentFromCode(
      'TestComponent',
      `
export const TestComponent = (props) => {
  return <div>The Best Test Component</div>
}
    `,
    )
    const result = componentUsesProperty(component, 'children')
    expect(result).toEqual(false)
  })
  it('returns true for a component that uses the children property', () => {
    const component = getComponentFromCode(
      'TestComponent',
      `
export const TestComponent = (props) => {
  return <div>{props.children}</div>
}
    `,
    )
    const result = componentUsesProperty(component, 'children')
    expect(result).toEqual(true)
  })
  it('returns true for a component that uses the children property from a spread version of the props', () => {
    const component = getComponentFromCode(
      'TestComponent',
      `
export const TestComponent = ({children}) => {
  return <div>{children}</div>
}
    `,
    )
    const result = componentUsesProperty(component, 'children')
    expect(result).toEqual(true)
  })
  it('returns true for a component that uses the children property from a spread version of the props with children aliased', () => {
    const component = getComponentFromCode(
      'TestComponent',
      `
export const TestComponent = ({children: alias}) => {
  return <div>{alias}</div>
}
    `,
    )
    const result = componentUsesProperty(component, 'children')
    expect(result).toEqual(true)
  })
  it('returns false for a component that in no way uses the children property (deeply structured)', () => {
    const component = getComponentFromCode(
      'TestComponent',
      `
export const TestComponent = (props) => {
  return <div>
    <h2>
      <div>The Best Test Component</div>
    <h2>
  </div>
}
    `,
    )
    const result = componentUsesProperty(component, 'children')
    expect(result).toEqual(false)
  })
  it('returns true for a component that uses the children property (deeply structured)', () => {
    const component = getComponentFromCode(
      'TestComponent',
      `
export const TestComponent = (props) => {
  return <div>
    <h2>
      <div>{props.children}</div>
    <h2>
  </div>
}
    `,
    )
    const result = componentUsesProperty(component, 'children')
    expect(result).toEqual(true)
  })
  it('returns true for a component that uses the children property from a spread version of the props (deeply structured)', () => {
    const component = getComponentFromCode(
      'TestComponent',
      `
export const TestComponent = ({children}) => {
  return <div>
    <h2>
      <div>{children}</div>
    <h2>
  </div>
}
    `,
    )
    const result = componentUsesProperty(component, 'children')
    expect(result).toEqual(true)
  })
  it('returns true for a component that uses the children property from a spread version of the props with children aliased (deeply structured)', () => {
    const component = getComponentFromCode(
      'TestComponent',
      `
export const TestComponent = ({children: alias}) => {
  return <div>
    <h2>
      <div>{alias}</div>
    <h2>
  </div>
}
    `,
    )
    const result = componentUsesProperty(component, 'children')
    expect(result).toEqual(true)
  })
  it('returns true for a component that uses the children property in an element property', () => {
    const component = getComponentFromCode(
      'TestComponent',
      `
import { LayoutElement } from './layoutelement'
export const TestComponent = (props) => {
  return <div>
    <h2>
      <LayoutElement toLayout={props.children} />
    <h2>
  </div>
}
    `,
    )
    const result = componentUsesProperty(component, 'children')
    expect(result).toEqual(true)
  })
})

describe('componentHonoursPropsPosition', () => {
  it('returns true for a component that uses top and left directly', () => {
    const component = getComponentFromCode(
      'TestComponent',
      `
export const TestComponent = (props) => {
  return <div style={{position: 'absolute', left: props.style.left, top: props.style.top}}>The Best Test Component</div>
}
    `,
    )
    const result = componentHonoursPropsPosition(component)
    expect(result).toEqual(true)
  })
  it('returns true for a component that uses top and right directly', () => {
    const component = getComponentFromCode(
      'TestComponent',
      `
export const TestComponent = (props) => {
  return <div style={{position: 'absolute', right: props.style.right, top: props.style.top}}>The Best Test Component</div>
}
    `,
    )
    const result = componentHonoursPropsPosition(component)
    expect(result).toEqual(true)
  })
  it('returns false for a component that uses left and right only', () => {
    const component = getComponentFromCode(
      'TestComponent',
      `
export const TestComponent = (props) => {
  return <div style={{position: 'absolute', left: props.style.left, right: props.style.right}}>The Best Test Component</div>
}
    `,
    )
    const result = componentHonoursPropsPosition(component)
    expect(result).toEqual(false)
  })
  it('returns false for a component that uses top only', () => {
    const component = getComponentFromCode(
      'TestComponent',
      `
export const TestComponent = (props) => {
  return <div style={{position: 'absolute', top: props.style.top}}>The Best Test Component</div>
}
    `,
    )
    const result = componentHonoursPropsPosition(component)
    expect(result).toEqual(false)
  })
  it('returns false for a component that uses nothing from props', () => {
    const component = getComponentFromCode(
      'TestComponent',
      `
export const TestComponent = (props) => {
  return <div style={{position: 'absolute'}}>The Best Test Component</div>
}
    `,
    )
    const result = componentHonoursPropsPosition(component)
    expect(result).toEqual(false)
  })
  it('returns false for a component that has no props parameter', () => {
    const component = getComponentFromCode(
      'TestComponent',
      `
export const TestComponent = () => {
  return <div style={{position: 'absolute'}}>The Best Test Component</div>
}
    `,
    )
    const result = componentHonoursPropsPosition(component)
    expect(result).toEqual(false)
  })
  it('returns true for a component that uses top and left directly with destructuring', () => {
    const component = getComponentFromCode(
      'TestComponent',
      `
export const TestComponent = ({style}) => {
  return <div style={{position: 'absolute', left: style.left, top: style.top}}>The Best Test Component</div>
}
    `,
    )
    const result = componentHonoursPropsPosition(component)
    expect(result).toEqual(true)
  })
  it('returns true for a component that uses bottom and right directly', () => {
    const component = getComponentFromCode(
      'TestComponent',
      `
export const TestComponent = (props) => {
  return <div style={{position: 'absolute', right: props.style.right, bottom: props.style.bottom}}>The Best Test Component</div>
}
    `,
    )
    const result = componentHonoursPropsPosition(component)
    expect(result).toEqual(true)
  })
  it('returns true for a component that spreads style into the props', () => {
    const component = getComponentFromCode(
      'TestComponent',
      `
export const TestComponent = (props) => {
  return <div style={{position: 'absolute', ...props.style}}>The Best Test Component</div>
}
    `,
    )
    const result = componentHonoursPropsPosition(component)
    expect(result).toEqual(true)
  })
  it('returns true for a component that spreads style into the props with destructuring', () => {
    const component = getComponentFromCode(
      'TestComponent',
      `
export const TestComponent = ({style}) => {
  return <div style={{position: 'absolute', ...style}}>The Best Test Component</div>
}
    `,
    )
    const result = componentHonoursPropsPosition(component)
    expect(result).toEqual(true)
  })
})

describe('componentHonoursPropsSize', () => {
  it('returns true for a component that uses width and height directly', () => {
    const component = getComponentFromCode(
      'TestComponent',
      `
export const TestComponent = (props) => {
  return <div style={{position: 'absolute', width: props.style.width, height: props.style.height}}>The Best Test Component</div>
}
    `,
    )
    const result = componentHonoursPropsSize(component)
    expect(result).toEqual(true)
  })
  it('returns false for a component that uses width only', () => {
    const component = getComponentFromCode(
      'TestComponent',
      `
export const TestComponent = (props) => {
  return <div style={{position: 'absolute', width: props.style.width}}>The Best Test Component</div>
}
    `,
    )
    const result = componentHonoursPropsSize(component)
    expect(result).toEqual(false)
  })
  it('returns false for a component that do not use anything from props', () => {
    const component = getComponentFromCode(
      'TestComponent',
      `
export const TestComponent = (props) => {
  return <div style={{position: 'absolute'}}>The Best Test Component</div>
}
    `,
    )
    const result = componentHonoursPropsSize(component)
    expect(result).toEqual(false)
  })
  it('returns false for a component that has no props parameter', () => {
    const component = getComponentFromCode(
      'TestComponent',
      `
export const TestComponent = () => {
  return <div style={{position: 'absolute'}}>The Best Test Component</div>
}
    `,
    )
    const result = componentHonoursPropsSize(component)
    expect(result).toEqual(false)
  })
  it('returns true for a component that uses width and height directly with destructuring', () => {
    const component = getComponentFromCode(
      'TestComponent',
      `
export const TestComponent = ({style}) => {
  return <div style={{position: 'absolute', width: style.width, height: style.height}}>The Best Test Component</div>
}
    `,
    )
    const result = componentHonoursPropsSize(component)
    expect(result).toEqual(true)
  })
  it('returns true for a component that spreads style into the props', () => {
    const component = getComponentFromCode(
      'TestComponent',
      `
export const TestComponent = (props) => {
  return <div style={{position: 'absolute', ...props.style}}>The Best Test Component</div>
}
    `,
    )
    const result = componentHonoursPropsSize(component)
    expect(result).toEqual(true)
  })
  it('returns true for a component that spreads style into the props with destructuring', () => {
    const component = getComponentFromCode(
      'TestComponent',
      `
export const TestComponent = ({style}) => {
  return <div style={{position: 'absolute', ...style}}>The Best Test Component</div>
}
    `,
    )
    const result = componentHonoursPropsSize(component)
    expect(result).toEqual(true)
  })
})

describe('rearrangeJsxChildren', () => {
  it('rearranges three simple children', () => {
    const projectFile = getParseSuccessForStoryboardCode(
      makeTestProjectCodeWithSnippet(`
      <div style={{ ...props.style }} data-uid='aaa'>
        <div data-uid='parent' >
          <div data-uid='child-d' />
          <div data-uid='child-c'>
            <div data-uid='grandchild-c' />
          </div>
          <div data-uid='child-b' />
          <div data-uid='child-a' />
        </div>
      </div>
  `),
    )

    const target: StaticElementPath = dynamicPathToStaticPath(fromString('aaa/parent'))

    const result = modifyParseSuccessWithSimple((s) => {
      return {
        ...s,
        utopiaComponents: rearrangeJsxChildren(
          target,
          [
            fromStringStatic('aaa/parent/child-a'),
            fromStringStatic('aaa/parent/child-b'),
            fromStringStatic('aaa/parent/child-c'),
            fromStringStatic('aaa/parent/child-d'),
          ],
          s.utopiaComponents,
        ),
      }
    }, projectFile)

    expect(printCode(result)).toEqual(
      makeTestProjectCodeWithSnippet(`
      <div style={{ ...props.style }} data-uid='aaa'>
        <div data-uid='parent' >
          <div data-uid='child-a' />
          <div data-uid='child-b' />
          <div data-uid='child-c'>
            <div data-uid='grandchild-c' />
          </div>
          <div data-uid='child-d' />
        </div>
      </div>
  `),
    )
  })

  it('not a problem if no need to rearrange', () => {
    const projectFile = getParseSuccessForStoryboardCode(
      makeTestProjectCodeWithSnippet(`
      <div style={{ ...props.style }} data-uid='aaa'>
        <div data-uid='parent' >
          <div data-uid='child-a' />
          <div data-uid='child-b' />
          <div data-uid='child-c'>
            <div data-uid='grandchild-c' />
          </div>
          <div data-uid='child-d' />
        </div>
      </div>
  `),
    )

    const target: StaticElementPath = dynamicPathToStaticPath(fromString('aaa/parent'))

    const result = modifyParseSuccessWithSimple((s) => {
      return {
        ...s,
        utopiaComponents: rearrangeJsxChildren(
          target,
          [
            fromStringStatic('aaa/parent/child-a'),
            fromStringStatic('aaa/parent/child-b'),
            fromStringStatic('aaa/parent/child-c'),
            fromStringStatic('aaa/parent/child-d'),
          ],
          s.utopiaComponents,
        ),
      }
    }, projectFile)

    expect(printCode(result)).toEqual(
      makeTestProjectCodeWithSnippet(`
      <div style={{ ...props.style }} data-uid='aaa'>
        <div data-uid='parent' >
          <div data-uid='child-a' />
          <div data-uid='child-b' />
          <div data-uid='child-c'>
            <div data-uid='grandchild-c' />
          </div>
          <div data-uid='child-d' />
        </div>
      </div>
  `),
    )
  })

  it('throws error if child count doesnt match rearrangedChildPaths length', () => {
    const projectFile = getParseSuccessForStoryboardCode(
      makeTestProjectCodeWithSnippet(`
      <div style={{ ...props.style }} data-uid='aaa'>
        <div data-uid='parent' >
          <div data-uid='child-d' />
          <div data-uid='child-c'>
            <div data-uid='grandchild-c' />
          </div>
          <div data-uid='child-b' />
          <div data-uid='child-a' />
        </div>
      </div>
  `),
    )

    const target: StaticElementPath = dynamicPathToStaticPath(fromString('aaa/parent'))

    expect(() => {
      modifyParseSuccessWithSimple((s) => {
        return {
          ...s,
          utopiaComponents: rearrangeJsxChildren(
            target,
            [fromStringStatic('aaa/parent/child-a')],
            s.utopiaComponents,
          ),
        }
      }, projectFile)
    }).toThrow()
  })

  it('handles zero children all right', () => {
    const projectFile = getParseSuccessForStoryboardCode(
      makeTestProjectCodeWithSnippet(`
      <div style={{ ...props.style }} data-uid='aaa'>
        <div data-uid='parent' />
      </div>
  `),
    )

    const target: StaticElementPath = dynamicPathToStaticPath(fromString('aaa/parent'))

    const result = modifyParseSuccessWithSimple((s) => {
      return {
        ...s,
        utopiaComponents: rearrangeJsxChildren(target, [], s.utopiaComponents),
      }
    }, projectFile)

    expect(printCode(result)).toEqual(
      makeTestProjectCodeWithSnippet(`
      <div style={{ ...props.style }} data-uid='aaa'>
        <div data-uid='parent' />
      </div>
  `),
    )
  })
})

function printCode(projectFile: ParseSuccess): string {
  return testPrintCodeFromParseSuccess('storyboard.js', projectFile)
}

describe('findJSXElementChildAtPath', () => {
  function findElement(file: ParseSuccess, pathString: string) {
    const path = fromStringStatic(pathString)
    const foundElement = findJSXElementChildAtPath(
      getComponentsFromTopLevelElements(file.topLevelElements),
      path,
    )
    return foundElement
  }

  function expectElementAtPathHasMatchingUID(file: ParseSuccess, pathString: string) {
    const foundElement = findElement(file, pathString)
    expect(foundElement).not.toBeNull()
    expect(getUtopiaID(foundElement!)).toEqual(toUid(fromStringStatic(pathString)))
  }

  function expectElementAtPathHasMatchingUIDForPaths(file: ParseSuccess, paths: Array<string>) {
    paths.forEach((path) => expectElementAtPathHasMatchingUID(file, path))
  }

  function expectElementFoundNull(file: ParseSuccess, paths: Array<string>) {
    paths.forEach((path) => expect(findElement(file, path)).toBeNull())
  }

  it('simple project with divs', () => {
    const projectFile = getParseSuccessForStoryboardCode(
      makeTestProjectCodeWithSnippet(`
        <div style={{ ...props.style }} data-uid='aaa'>
          <div data-uid='parent' >
            <div data-uid='child-d' />
            <div data-uid='child-c'>
              <div data-uid='grandchild-c' />
            </div>
            <div data-uid='child-b' />
            <div data-uid='child-a' />
          </div>
        </div>
      `),
    )

    expectElementAtPathHasMatchingUIDForPaths(projectFile, [
      'utopia-storyboard-uid/scene-aaa/app-entity:aaa',
      'utopia-storyboard-uid/scene-aaa/app-entity:aaa/parent',
      'utopia-storyboard-uid/scene-aaa/app-entity:aaa/parent/child-a',
    ])
    expectElementFoundNull(projectFile, [
      'utopia-storyboard-uid/scene-aaa/app-entity:aaa/parent/not-existing-child',
    ])
  })

  it('simple project with divs with a conditional expression as sibling', () => {
    const projectFile = getParseSuccessForStoryboardCode(
      makeTestProjectCodeWithSnippet(`
        <div style={{ ...props.style }} data-uid='aaa'>
          <div data-uid='parent' >
            {true ? 
              (
                <div data-uid='ternary-true-root'>
                  <div data-uid='ternary-true-child' />
                </div> 
              ) : (
                <div data-uid='ternary-false-root'>
                  <div data-uid='ternary-false-child' />
                </div> 
              )
            }
            <div data-uid='child-d' />
            <div data-uid='child-c'>
              <div data-uid='grandchild-c' />
            </div>
            <div data-uid='child-b' />
            <div data-uid='child-a' />
          </div>
        </div>
      `),
    )

    expectElementAtPathHasMatchingUIDForPaths(projectFile, [
      'utopia-storyboard-uid/scene-aaa/app-entity:aaa',
      'utopia-storyboard-uid/scene-aaa/app-entity:aaa/parent',
      'utopia-storyboard-uid/scene-aaa/app-entity:aaa/parent/child-a',
    ])

    expectElementFoundNull(projectFile, [
      'utopia-storyboard-uid/scene-aaa/app-entity:aaa/parent/not-existing-child',
    ])
  })

  it('project with arbitrary js block', () => {
    const projectFile = getParseSuccessForStoryboardCode(
      makeTestProjectCodeWithSnippet(`
        <div style={{ ...props.style }} data-uid='aaa'>
          <div data-uid='parent' >
            <div data-uid='child-d' />
            {[1, 2, 3].map(id => {
              return (
                <div data-uid='mapped-root'>
                  <div data-uid='mapped-child' />
                </div> 
              )
            })}
            {[1, 2, 3].map(id => {
              return (
                <div data-uid='other-mapped-element' />
              )
            })}
            <div data-uid='child-b' />
            <div data-uid='child-a' />
          </div>
        </div>
      `),
    )

    expectElementAtPathHasMatchingUIDForPaths(projectFile, [
      'utopia-storyboard-uid/scene-aaa/app-entity:aaa',
      'utopia-storyboard-uid/scene-aaa/app-entity:aaa/parent',
      'utopia-storyboard-uid/scene-aaa/app-entity:aaa/parent/mapped-root',
      'utopia-storyboard-uid/scene-aaa/app-entity:aaa/parent/mapped-root/mapped-child',
      'utopia-storyboard-uid/scene-aaa/app-entity:aaa/parent/other-mapped-element',
    ])
  })

  it('project with arbitrary js block that has a conditional expression inside', () => {
    const projectFile = getParseSuccessForStoryboardCode(
      makeTestProjectCodeWithSnippet(`
        <div style={{ ...props.style }} data-uid='aaa'>
          <div data-uid='parent' >
            <div data-uid='child-d' />
            {[1, 2, 3].map(id => {
              return (
                <div data-uid='mapped-root'>
                  <div data-uid='mapped-child' />
                </div> 
              )
            })}
            {[1, 2, 3].map(id => {
              return (
                true ? 
                  (
                    <div data-uid='ternary-true-root'>
                      <div data-uid='ternary-true-child' />
                    </div> 
                  ) : (
                    <div data-uid='ternary-false-root'>
                      <div data-uid='ternary-false-child' />
                    </div> 
                  )
              )
            })}
            <div data-uid='child-b' />
            <div data-uid='child-a' />
          </div>
        </div>
      `),
    )

    expectElementAtPathHasMatchingUIDForPaths(projectFile, [
      'utopia-storyboard-uid/scene-aaa/app-entity:aaa',
      'utopia-storyboard-uid/scene-aaa/app-entity:aaa/parent',
      'utopia-storyboard-uid/scene-aaa/app-entity:aaa/parent/mapped-root',
      'utopia-storyboard-uid/scene-aaa/app-entity:aaa/parent/mapped-root/mapped-child',
      'utopia-storyboard-uid/scene-aaa/app-entity:aaa/parent/ternary-true-root',
      'utopia-storyboard-uid/scene-aaa/app-entity:aaa/parent/ternary-true-root/ternary-true-child',
      'utopia-storyboard-uid/scene-aaa/app-entity:aaa/parent/ternary-false-root',
      'utopia-storyboard-uid/scene-aaa/app-entity:aaa/parent/ternary-false-root/ternary-false-child',
    ])
  })

  it('conditional expressions', () => {
    const projectFile = getParseSuccessForStoryboardCode(
      makeTestProjectCodeWithSnippet(`
        <div style={{ ...props.style }} data-uid='aaa'>
          <div data-uid='parent' >
            <div data-uid='child-d' />
            {
              // @utopia/uid=conditional-1
              true ? 
              (
                <div data-uid='ternary-true-root'>
                  <div data-uid='ternary-true-child' />
                </div> 
              ) : (
                <div data-uid='ternary-false-root'>
                  <div data-uid='ternary-false-child' />
                </div> 
              )
            }
            <div data-uid='child-b' />
            <div data-uid='child-a' />
          </div>
        </div>
      `),
    )

    expectElementAtPathHasMatchingUIDForPaths(projectFile, [
      'utopia-storyboard-uid/scene-aaa/app-entity:aaa',
      'utopia-storyboard-uid/scene-aaa/app-entity:aaa/parent',
      'utopia-storyboard-uid/scene-aaa/app-entity:aaa/parent/conditional-1',
      'utopia-storyboard-uid/scene-aaa/app-entity:aaa/parent/conditional-1/ternary-true-root',
      'utopia-storyboard-uid/scene-aaa/app-entity:aaa/parent/conditional-1/ternary-true-root/ternary-true-child',
      'utopia-storyboard-uid/scene-aaa/app-entity:aaa/parent/conditional-1/ternary-false-root',
      'utopia-storyboard-uid/scene-aaa/app-entity:aaa/parent/conditional-1/ternary-false-root/ternary-false-child',
    ])
  })

  it('conditional expressions with branches that are JSXAttribute', () => {
    const projectFile = getParseSuccessForStoryboardCode(
      makeTestProjectCodeWithSnippet(`
        <div style={{ ...props.style }} data-uid='aaa'>
          <div data-uid='parent' >
            <div data-uid='child-d' />
            {
              // @utopia/uid=conditional-1
              true ? 
              (
                "hello"
              ) : (
                "world"
              )
            }
            <div data-uid='child-b' />
            <div data-uid='child-a' />
          </div>
        </div>
      `),
    )

    expectElementAtPathHasMatchingUIDForPaths(projectFile, [
      'utopia-storyboard-uid/scene-aaa/app-entity:aaa',
      'utopia-storyboard-uid/scene-aaa/app-entity:aaa/parent',
      'utopia-storyboard-uid/scene-aaa/app-entity:aaa/parent/conditional-1',
      'utopia-storyboard-uid/scene-aaa/app-entity:aaa/parent/conditional-1/409',
      'utopia-storyboard-uid/scene-aaa/app-entity:aaa/parent/conditional-1/831',
    ])
  })

  it('conditional expressions with branches that are mixed JSXAttribute and JSXElementChild', () => {
    const projectFile = getParseSuccessForStoryboardCode(
      makeTestProjectCodeWithSnippet(`
        <div style={{ ...props.style }} data-uid='aaa'>
          <div data-uid='parent' >
            <div data-uid='child-d' />
            {
              // @utopia/uid=conditional-1
              true ? 
              (
                "hello"
              ) : (
                <div data-uid='ternary-false-root'>
                  <div data-uid='ternary-false-child' />
                </div> 
              )
            }
            <div data-uid='child-b' />
            <div data-uid='child-a' />
          </div>
        </div>
      `),
    )

    expectElementAtPathHasMatchingUIDForPaths(projectFile, [
      'utopia-storyboard-uid/scene-aaa/app-entity:aaa',
      'utopia-storyboard-uid/scene-aaa/app-entity:aaa/parent',
      'utopia-storyboard-uid/scene-aaa/app-entity:aaa/parent/conditional-1',
      'utopia-storyboard-uid/scene-aaa/app-entity:aaa/parent/conditional-1/409',
      'utopia-storyboard-uid/scene-aaa/app-entity:aaa/parent/conditional-1/ternary-false-root',
      'utopia-storyboard-uid/scene-aaa/app-entity:aaa/parent/conditional-1/ternary-false-root/ternary-false-child',
    ])
  })
})

describe('insertJSXElementChild', () => {
  function findElement(components: Array<UtopiaJSXComponent>, pathString: string) {
    const path = fromStringStatic(pathString)
    const foundElement = findJSXElementChildAtPath(
      getComponentsFromTopLevelElements(components),
      path,
    )
    return foundElement
  }

  function expectElementAtPathHasMatchingUID(
    components: Array<UtopiaJSXComponent>,
    pathString: string,
  ) {
    const foundElement = findElement(components, pathString)
    expect(foundElement).not.toBeNull()
    expect(getUtopiaID(foundElement!)).toEqual(toUid(fromStringStatic(pathString)))
  }

  function expectElementAtPathHasMatchingUIDForPaths(
    components: Array<UtopiaJSXComponent>,
    paths: Array<string>,
  ) {
    paths.forEach((path) => expectElementAtPathHasMatchingUID(components, path))
  }

  it('inserts simple element as child', () => {
    const projectContents = createTestProjectWithCode(
      makeTestProjectCodeWithSnippet(`
      <div style={{ ...props.style }} data-uid='aaa'>
        <div data-uid='parent' >
          <div data-uid='child-a' />
          <div data-uid='child-b' />
          <div data-uid='child-c'>
            <div data-uid='grandchild-c' />
          </div>
          <div data-uid='child-d' />
        </div>
      </div>
      `),
    ).projectContents

    const file = getContentsTreeFileFromString(projectContents, StoryboardFilePath)

    if (file?.type !== 'TEXT_FILE' || file.lastParseSuccess == null) {
      throw new Error('failed parsing the test project file')
    }

    const components = getComponentsFromTopLevelElements(file.lastParseSuccess.topLevelElements)

    const withInsertedElement = insertJSXElementChild(
      projectContents,
      StoryboardFilePath,
      childInsertionPath(
        EP.fromString('utopia-storyboard-uid/scene-aaa/app-entity:aaa/parent/child-a'),
      ),
      jsxElement('div', 'hello', [], []),
      components,
      null,
    )

    expectElementAtPathHasMatchingUIDForPaths(withInsertedElement.components, [
      'utopia-storyboard-uid/scene-aaa/app-entity:aaa',
      'utopia-storyboard-uid/scene-aaa/app-entity:aaa/parent',
      'utopia-storyboard-uid/scene-aaa/app-entity:aaa/parent/child-a',
      'utopia-storyboard-uid/scene-aaa/app-entity:aaa/parent/child-a/hello', // <- the inserted element!
      'utopia-storyboard-uid/scene-aaa/app-entity:aaa/parent/child-b',
      'utopia-storyboard-uid/scene-aaa/app-entity:aaa/parent/child-c',
      'utopia-storyboard-uid/scene-aaa/app-entity:aaa/parent/child-c/grandchild-c',
      'utopia-storyboard-uid/scene-aaa/app-entity:aaa/parent/child-d',
    ])
  })
})
