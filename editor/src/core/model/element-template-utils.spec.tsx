/* eslint jest/expect-expect: ["error", { "assertFunctionNames": ["expect", "expectElementAtPathHasMatchingUID*", "expectElementFoundNull"] }] */

import { getProjectFileByFilePath } from '../../components/assets'
import {
  makeTestProjectCodeWithSnippet,
  testPrintCodeFromParseSuccess,
} from '../../components/canvas/ui-jsx.test-utils'
import {
  StoryboardFilePath,
  modifyParseSuccessWithSimple,
} from '../../components/editor/store/editor-state'
import {
  childInsertionPath,
  conditionalClauseInsertionPath,
  replaceWithSingleElement,
  wrapInFragmentAndAppendElements,
} from '../../components/editor/store/insertion-path'
import {
  createTestProjectWithCode,
  getParseSuccessForStoryboardCode,
} from '../../sample-projects/sample-project-utils.test-utils'
import Utils, { absolute, after, before } from '../../utils/utils'
import * as EP from '../shared/element-path'
import {
  dynamicPathToStaticPath,
  fromString,
  fromStringStatic,
  toUid,
} from '../shared/element-path'
import { testStaticElementPath } from '../shared/element-path.test-utils'
import type {
  JSXConditionalExpression,
  JSXElement,
  JSXTextBlock,
  UtopiaJSXComponent,
  JSXElementChild,
  JSExpressionOtherJavaScript,
} from '../shared/element-template'
import {
  defaultPropsParam,
  emptyComments,
  getJSXAttribute,
  isJSExpressionMapOrOtherJavaScript,
  isJSXAttributeValue,
  isJSXConditionalExpression,
  isJSXElement,
  isJSXElementLike,
  jsExpressionFunctionCall,
  jsExpressionValue,
  jsxAttributesFromMap,
  jsxElement,
  jsxTextBlock,
  utopiaJSXComponent,
  JSXAttributes,
  jsxConditionalExpression,
  jsxFragment,
} from '../shared/element-template'
import {
  fromArrayIndex,
  fromField,
  fromObjectField,
  fromTypeGuard,
} from '../shared/optics/optic-creators'
import { unsafeGet } from '../shared/optics/optic-utilities'
import { Optic } from '../shared/optics/optics'
import type { ParseSuccess, StaticElementPath } from '../shared/project-file-types'
import { emptySet } from '../shared/set-utils'
import { getUtopiaID } from '../shared/uid-utils'
import { MetadataUtils } from './element-metadata-utils'
import {
  componentHonoursPropsPosition,
  componentHonoursPropsSize,
  componentUsesProperty,
  findJSXElementChildAtPath,
  guaranteeUniqueUids,
  insertJSXElementChildren,
  rearrangeJsxChildren,
  removeJSXElement,
  renameJsxElementChild,
  transformJSXComponentAtPath,
} from './element-template-utils'
import { FOR_TESTS_setNextGeneratedUids } from './element-template-utils.test-utils'
import { FOR_TESTS_setNextGeneratedUid } from './element-template-utils.test-utils'
import { getComponentFromCode } from './element-template.test-utils'
import { getComponentsFromTopLevelElements } from './project-file-utils'
import { BakedInStoryboardUID } from './scene-utils'

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
    const fixedElements = guaranteeUniqueUids(exampleElements, emptySet())

    const child0PropsOptic = fromArrayIndex<JSXElementChild>(0)
      .compose(fromTypeGuard(isJSXElement))
      .compose(fromField('props'))
    const child0Props = unsafeGet(child0PropsOptic, fixedElements.value)
    const child0UID = getJSXAttribute(child0Props, 'data-uid')
    expect(child0UID).toEqual(jsExpressionValue('aaa.1', emptyComments, 'aaa.2'))
    const child1PropsOptic = fromArrayIndex<JSXElementChild>(1)
      .compose(fromTypeGuard(isJSXElement))
      .compose(fromField('props'))
    const child1Props = unsafeGet(child1PropsOptic, fixedElements.value)
    const child1UID = getJSXAttribute(child1Props, 'data-uid')
    expect(child1UID).not.toEqual(jsExpressionValue('aaa', emptyComments, 'aaa'))
  })

  it('if an element has an existing value, it will be replaced', () => {
    const exampleElements = [
      jsxElement(
        'View',
        'aaa',
        jsxAttributesFromMap({ 'data-uid': jsExpressionValue('aaa', emptyComments, 'axa') }),
        [],
      ),
      jsxElement(
        'View',
        'aab',
        jsxAttributesFromMap({ 'data-uid': jsExpressionValue('aab', emptyComments, 'axb') }),
        [],
      ),
    ]

    const existingIDs = new Set(['aab', 'bbb'])
    const fixedElements = guaranteeUniqueUids(exampleElements, existingIDs)
    const child0PropsOptic = fromArrayIndex<JSXElementChild>(0)
      .compose(fromTypeGuard(isJSXElement))
      .compose(fromField('props'))
    const child0Props = unsafeGet(child0PropsOptic, fixedElements.value)
    const child0UID = getJSXAttribute(child0Props, 'data-uid')
    const child1PropsOptic = fromArrayIndex<JSXElementChild>(1)
      .compose(fromTypeGuard(isJSXElement))
      .compose(fromField('props'))
    const child1Props = unsafeGet(child1PropsOptic, fixedElements.value)
    const child1UID = getJSXAttribute(child1Props, 'data-uid')
    expect(child0UID).toEqual(jsExpressionValue('aaa', emptyComments, 'axa'))
    expect(child1UID).toEqual(jsExpressionValue('aab.1', emptyComments, 'aab.2'))
  })

  it('if the uid prop is not a simple value, replace it with a simple value', () => {
    const exampleElement = jsxElement(
      'View',
      '',
      jsxAttributesFromMap({ 'data-uid': jsExpressionFunctionCall('someFunction', []) }),
      [],
    )
    const fixedElements = guaranteeUniqueUids([exampleElement], emptySet())
    const fixedElement = fixedElements.value[0]
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
    const updatedElements = removeJSXElement(
      testStaticElementPath([[BakedInStoryboardUID, 'scene-aaa'], ['aaa']]),
      utopiaComponents,
    )
    expect(updatedElements.length).toEqual(1)
    expect(updatedElements[0]).toEqual(utopiaComponents[1])
  })
  it('removes a non-root element', () => {
    const updatedElements = removeJSXElement(
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
  it('returns true for a component that assigns props.style to style directly', () => {
    const component = getComponentFromCode(
      'TestComponent',
      `
export const TestComponent = (props) => {
  return <div style={props.style}>The Best Test Component</div>
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
  it('returns true for a component that assigns to style directly with destructuring', () => {
    const component = getComponentFromCode(
      'TestComponent',
      `
export const TestComponent = ({style}) => {
  return <div style={style}>The Best Test Component</div>
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
  it('returns true for a component that spreads props.style into the props', () => {
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
  it('returns true for a component that assigns props.style to style directly', () => {
    const component = getComponentFromCode(
      'TestComponent',
      `
export const TestComponent = (props) => {
  return <div style={props.style}>The Best Test Component</div>
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
  it('returns true for a component that assigns to style directly with destructuring', () => {
    const component = getComponentFromCode(
      'TestComponent',
      `
export const TestComponent = ({style}) => {
  return <div style={style}>The Best Test Component</div>
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

  it('conditional expressions with one branch that are JSXAttribute', () => {
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
                <span data-uid='false'>"world"</span>
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
      'utopia-storyboard-uid/scene-aaa/app-entity:aaa/parent/conditional-1/03d0498839c0fb8a34086cd891c8cee7',
      'utopia-storyboard-uid/scene-aaa/app-entity:aaa/parent/conditional-1/false',
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
      // conditionals with only text are not parsed as conditional expressions anymore, rather they are treated as expressions embedded in the text content of their parent
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
      'utopia-storyboard-uid/scene-aaa/app-entity:aaa/parent/conditional-1/03d0498839c0fb8a34086cd891c8cee7',
      'utopia-storyboard-uid/scene-aaa/app-entity:aaa/parent/conditional-1/ternary-false-root',
      'utopia-storyboard-uid/scene-aaa/app-entity:aaa/parent/conditional-1/ternary-false-root/ternary-false-child',
    ])
  })
})

describe('insertJSXElementChildren', () => {
  function createTestComponentsForSnippet(snippet: string) {
    const projectContents = createTestProjectWithCode(
      makeTestProjectCodeWithSnippet(snippet),
    ).projectContents

    const file = getProjectFileByFilePath(projectContents, StoryboardFilePath)

    if (file?.type !== 'TEXT_FILE' || file.lastParseSuccess == null) {
      throw new Error('failed parsing the test project file')
    }

    const components = getComponentsFromTopLevelElements(file.lastParseSuccess.topLevelElements)

    return { components, projectContents }
  }

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

  function expectIndexInParent(
    components: Array<UtopiaJSXComponent>,
    pathString: string,
    expectedIndex: number,
  ) {
    const path = EP.fromString(pathString)
    const parentPathString = EP.toString(EP.parentPath(path))
    const foundParent = findElement(components, parentPathString)
    if (foundParent == null || !isJSXElementLike(foundParent)) {
      throw new Error(`parent found at ${parentPathString} is not JsxElementLike`)
    }
    const childUid = EP.toUid(path)
    const foundChildIndex = foundParent.children.findIndex((child) => child.uid === childUid)

    expect(foundChildIndex).toBe(expectedIndex)
  }
  it('inserts simple elements as children', () => {
    const { components, projectContents } = createTestComponentsForSnippet(`
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
    `)

    const withInsertedElement = insertJSXElementChildren(
      childInsertionPath(
        EP.fromString('utopia-storyboard-uid/scene-aaa/app-entity:aaa/parent/child-a'),
      ),
      [jsxElement('div', 'hello1', [], []), jsxElement('div', 'hello2', [], [])],
      components,
      null,
    )

    expectElementAtPathHasMatchingUIDForPaths(withInsertedElement.components, [
      'utopia-storyboard-uid/scene-aaa/app-entity:aaa',
      'utopia-storyboard-uid/scene-aaa/app-entity:aaa/parent',
      'utopia-storyboard-uid/scene-aaa/app-entity:aaa/parent/child-a',
      'utopia-storyboard-uid/scene-aaa/app-entity:aaa/parent/child-a/hello1', // <- the inserted element!
      'utopia-storyboard-uid/scene-aaa/app-entity:aaa/parent/child-a/hello2', // <- the inserted element!
      'utopia-storyboard-uid/scene-aaa/app-entity:aaa/parent/child-b',
      'utopia-storyboard-uid/scene-aaa/app-entity:aaa/parent/child-c',
      'utopia-storyboard-uid/scene-aaa/app-entity:aaa/parent/child-c/grandchild-c',
      'utopia-storyboard-uid/scene-aaa/app-entity:aaa/parent/child-d',
    ])
  })

  it('inserts simple elements as children with index position', () => {
    const { components, projectContents } = createTestComponentsForSnippet(`
    <div style={{ ...props.style }} data-uid='aaa'>
      <div data-uid='parent' >
        <div data-uid='child-a' />
        <div data-uid='child-b' />
        <div data-uid='child-c'>
          <div data-uid='grandchild-c' />
          <div data-uid='grandchild-d' />
        </div>
        <div data-uid='child-d' />
      </div>
    </div>
    `)

    const withInsertedElement = insertJSXElementChildren(
      childInsertionPath(
        EP.fromString('utopia-storyboard-uid/scene-aaa/app-entity:aaa/parent/child-c'),
      ),
      [jsxElement('div', 'hello1', [], []), jsxElement('div', 'hello2', [], [])],
      components,
      absolute(1),
    )

    expectIndexInParent(
      withInsertedElement.components,
      'utopia-storyboard-uid/scene-aaa/app-entity:aaa/parent/child-c/hello1',
      1,
    )
    expectIndexInParent(
      withInsertedElement.components,
      'utopia-storyboard-uid/scene-aaa/app-entity:aaa/parent/child-c/hello2',
      2,
    )

    expectElementAtPathHasMatchingUIDForPaths(withInsertedElement.components, [
      'utopia-storyboard-uid/scene-aaa/app-entity:aaa',
      'utopia-storyboard-uid/scene-aaa/app-entity:aaa/parent',
      'utopia-storyboard-uid/scene-aaa/app-entity:aaa/parent/child-a',
      'utopia-storyboard-uid/scene-aaa/app-entity:aaa/parent/child-b',
      'utopia-storyboard-uid/scene-aaa/app-entity:aaa/parent/child-c',
      'utopia-storyboard-uid/scene-aaa/app-entity:aaa/parent/child-c/grandchild-c',
      'utopia-storyboard-uid/scene-aaa/app-entity:aaa/parent/child-c/hello1', // <- the inserted element!
      'utopia-storyboard-uid/scene-aaa/app-entity:aaa/parent/child-c/hello2', // <- the inserted element!
      'utopia-storyboard-uid/scene-aaa/app-entity:aaa/parent/child-c/grandchild-d',
      'utopia-storyboard-uid/scene-aaa/app-entity:aaa/parent/child-d',
    ])
  })

  it('inserts simple elements as last with index position pointing to larger index than possible', () => {
    const { components, projectContents } = createTestComponentsForSnippet(`
    <div style={{ ...props.style }} data-uid='aaa'>
      <div data-uid='parent' >
        <div data-uid='child-a' />
        <div data-uid='child-b' />
        <div data-uid='child-c'>
          <div data-uid='grandchild-c-1' />
          <div data-uid='grandchild-c-2' />
        </div>
        <div data-uid='child-d' />
      </div>
    </div>
    `)

    const withInsertedElement = insertJSXElementChildren(
      childInsertionPath(
        EP.fromString('utopia-storyboard-uid/scene-aaa/app-entity:aaa/parent/child-c'),
      ),
      [jsxElement('div', 'hello1', [], []), jsxElement('div', 'hello2', [], [])],
      components,
      after(15), // this means it should come after element index 15, but the array is only 2 long. the resulting index will be 2 (zero based)
    )

    expectIndexInParent(
      withInsertedElement.components,
      'utopia-storyboard-uid/scene-aaa/app-entity:aaa/parent/child-c/hello1',
      2,
    )
    expectIndexInParent(
      withInsertedElement.components,
      'utopia-storyboard-uid/scene-aaa/app-entity:aaa/parent/child-c/hello2',
      3,
    )

    expectElementAtPathHasMatchingUIDForPaths(withInsertedElement.components, [
      'utopia-storyboard-uid/scene-aaa/app-entity:aaa',
      'utopia-storyboard-uid/scene-aaa/app-entity:aaa/parent',
      'utopia-storyboard-uid/scene-aaa/app-entity:aaa/parent/child-a',
      'utopia-storyboard-uid/scene-aaa/app-entity:aaa/parent/child-b',
      'utopia-storyboard-uid/scene-aaa/app-entity:aaa/parent/child-c',
      'utopia-storyboard-uid/scene-aaa/app-entity:aaa/parent/child-c/grandchild-c-1',
      'utopia-storyboard-uid/scene-aaa/app-entity:aaa/parent/child-c/grandchild-c-2',
      'utopia-storyboard-uid/scene-aaa/app-entity:aaa/parent/child-c/hello1', // <- the inserted element!
      'utopia-storyboard-uid/scene-aaa/app-entity:aaa/parent/child-c/hello2', // <- the inserted element!
      'utopia-storyboard-uid/scene-aaa/app-entity:aaa/parent/child-d',
    ])
  })

  it('array insertion throws error if trying to insert into a conditional expression', () => {
    const { components, projectContents } = createTestComponentsForSnippet(`
    <div style={{ ...props.style }} data-uid='aaa'>
      <div data-uid='parent' >
        <div data-uid='child-a' />
        <div data-uid='child-b' />
        {
          // @utopia/uid=child-c
          true ?
          (
            "hello"
          ) : (
            <span>"world"</span>
          )
        }
        <div data-uid='child-d' />
      </div>
    </div>
    `)

    expect(() =>
      insertJSXElementChildren(
        childInsertionPath(
          EP.fromString('utopia-storyboard-uid/scene-aaa/app-entity:aaa/parent/child-c'),
        ),
        [jsxElement('div', 'hello', [], [])],
        components,
        null,
      ),
    ).toThrow()
  })

  it('conditional clause insertion throws error if the parent is not a conditional expression', () => {
    const { components, projectContents } = createTestComponentsForSnippet(`
    <div style={{ ...props.style }} data-uid='aaa'>
      <div data-uid='parent' >
        <div data-uid='child-a' />
        <div data-uid='child-b' />
        <div data-uid='child-c' />
      </div>
    </div>
    `)

    expect(() =>
      insertJSXElementChildren(
        conditionalClauseInsertionPath(
          EP.fromString('utopia-storyboard-uid/scene-aaa/app-entity:aaa/parent/child-a'),
          'true-case',
          replaceWithSingleElement(),
        ),
        [jsxElement('div', 'hello', [], [])],
        components,
        null,
      ),
    ).toThrow()
  })

  it("inserting a single element into the conditional's true branch is working with replace behavior when branch is empty", () => {
    const { components, projectContents } = createTestComponentsForSnippet(`
    <div style={{ ...props.style }} data-uid='aaa'>
      <div data-uid='parent' >
        <div data-uid='child-a' />
        <div data-uid='child-b' />
        {
          // @utopia/uid=child-c
          true ?
          (
            null
          ) : (
            <span>"world"</span>
          )
        }
        <div data-uid='child-d' />
      </div>
    </div>
    `)

    const withInsertedElement = insertJSXElementChildren(
      conditionalClauseInsertionPath(
        EP.fromString('utopia-storyboard-uid/scene-aaa/app-entity:aaa/parent/child-c'),
        'true-case',
        replaceWithSingleElement(),
      ),
      [jsxElement('div', 'hello1', [], [])],
      components,
      null,
    )

    expectElementAtPathHasMatchingUIDForPaths(withInsertedElement.components, [
      'utopia-storyboard-uid/scene-aaa/app-entity:aaa',
      'utopia-storyboard-uid/scene-aaa/app-entity:aaa/parent',
      'utopia-storyboard-uid/scene-aaa/app-entity:aaa/parent/child-a',
      'utopia-storyboard-uid/scene-aaa/app-entity:aaa/parent/child-b',
      'utopia-storyboard-uid/scene-aaa/app-entity:aaa/parent/child-c',
      'utopia-storyboard-uid/scene-aaa/app-entity:aaa/parent/child-c/hello1', // <- the inserted element!
      'utopia-storyboard-uid/scene-aaa/app-entity:aaa/parent/child-d',
    ])
  })
  it("inserting multiple elements into the conditional's true branch throws error", () => {
    const { components, projectContents } = createTestComponentsForSnippet(`
    <div style={{ ...props.style }} data-uid='aaa'>
      <div data-uid='parent' >
        <div data-uid='child-a' />
        <div data-uid='child-b' />
        {
          // @utopia/uid=child-c
          true ?
          (
            null
          ) : (
            <span>"world"</span>
          )
        }
        <div data-uid='child-d' />
      </div>
    </div>
    `)

    FOR_TESTS_setNextGeneratedUid('fragment')
    expect(() =>
      insertJSXElementChildren(
        conditionalClauseInsertionPath(
          EP.fromString('utopia-storyboard-uid/scene-aaa/app-entity:aaa/parent/child-c'),
          'true-case',
          replaceWithSingleElement(),
        ),
        [jsxElement('div', 'hello1', [], []), jsxElement('div', 'hello2', [], [])],
        components,
        null,
      ),
    ).toThrow()
  })

  it("inserting an element into the conditional's true branch is working with wrap into fragment behavior", () => {
    const { components, projectContents } = createTestComponentsForSnippet(`
    <div style={{ ...props.style }} data-uid='aaa'>
      <div data-uid='parent' >
        <div data-uid='child-a' />
        <div data-uid='child-b' />
        {
          // @utopia/uid=child-c
          true ?
          (
            "hello"
          ) : (
            <div>"world"</div>
          )
        }
        <div data-uid='child-d' />
      </div>
    </div>
    `)

    const withInsertedElement = insertJSXElementChildren(
      conditionalClauseInsertionPath(
        EP.fromString('utopia-storyboard-uid/scene-aaa/app-entity:aaa/parent/child-c'),
        'true-case',
        wrapInFragmentAndAppendElements('wrapper-fragment'),
      ),
      [jsxElement('div', 'hello2', [], [])],
      components,
      null,
    )

    expectElementAtPathHasMatchingUIDForPaths(withInsertedElement.components, [
      'utopia-storyboard-uid/scene-aaa/app-entity:aaa',
      'utopia-storyboard-uid/scene-aaa/app-entity:aaa/parent',
      'utopia-storyboard-uid/scene-aaa/app-entity:aaa/parent/child-a',
      'utopia-storyboard-uid/scene-aaa/app-entity:aaa/parent/child-b',
      'utopia-storyboard-uid/scene-aaa/app-entity:aaa/parent/child-c',
      'utopia-storyboard-uid/scene-aaa/app-entity:aaa/parent/child-c/wrapper-fragment/', // <- the new fragment!
      'utopia-storyboard-uid/scene-aaa/app-entity:aaa/parent/child-c/wrapper-fragment/03d0498839c0fb8a34086cd891c8cee7', // <- the original hello!
      'utopia-storyboard-uid/scene-aaa/app-entity:aaa/parent/child-c/wrapper-fragment/hello2', // <- the inserted hello!
      'utopia-storyboard-uid/scene-aaa/app-entity:aaa/parent/child-d',
    ])
  })

  it("inserting multiple elements into the conditional's false branch with replace behavior throws", () => {
    const { components, projectContents } = createTestComponentsForSnippet(`
      <div style={{ ...props.style }} data-uid='aaa'>
        <div data-uid='parent' >
          <div data-uid='child-a' />
          <div data-uid='child-b' />
        {
          // @utopia/uid=child-c
          true ?
          (
            "hello"
          ) : (
            <span>"world-will be deleted"</span>
          )
        }
        <div data-uid='child-d' />
      </div>
    </div>
    `)

    expect(() =>
      insertJSXElementChildren(
        conditionalClauseInsertionPath(
          EP.fromString('utopia-storyboard-uid/scene-aaa/app-entity:aaa/parent/child-c'),
          'false-case',
          replaceWithSingleElement(),
        ),
        [jsxElement('div', 'hello1', [], []), jsxElement('div', 'hello2', [], [])],
        components,
        null,
      ),
    ).toThrow()
  })

  it("inserting into the conditional's false branch with an existing fragment", () => {
    const { components, projectContents } = createTestComponentsForSnippet(`
      <div style={{ ...props.style }} data-uid='aaa'>
        <div data-uid='parent' >
          <div data-uid='child-a' />
          <div data-uid='child-b' />
        {
          // @utopia/uid=child-c
          true ?
          (
            null
          ) : (
            <React.Fragment data-uid='fragment'>
              <span data-uid='child-e'>hello in a fragment</span>
            </React.Fragment>
          )
        }
        <div data-uid='child-d' />
      </div>
    </div>
    `)

    const withInsertedElement = insertJSXElementChildren(
      conditionalClauseInsertionPath(
        EP.fromString('utopia-storyboard-uid/scene-aaa/app-entity:aaa/parent/child-c'),
        'false-case',
        wrapInFragmentAndAppendElements('wrapper-fragment'),
      ),
      [jsxElement('div', 'hello1', [], []), jsxElement('div', 'hello2', [], [])],
      components,
      null,
    )

    expectElementAtPathHasMatchingUIDForPaths(withInsertedElement.components, [
      'utopia-storyboard-uid/scene-aaa/app-entity:aaa',
      'utopia-storyboard-uid/scene-aaa/app-entity:aaa/parent',
      'utopia-storyboard-uid/scene-aaa/app-entity:aaa/parent/child-a',
      'utopia-storyboard-uid/scene-aaa/app-entity:aaa/parent/child-b',
      'utopia-storyboard-uid/scene-aaa/app-entity:aaa/parent/child-c',
      'utopia-storyboard-uid/scene-aaa/app-entity:aaa/parent/child-c/wrapper-fragment/fragment/child-e', // the original fragment is wrapped into a new wrapper
      'utopia-storyboard-uid/scene-aaa/app-entity:aaa/parent/child-c/wrapper-fragment/hello1', // <- the inserted element! (wrapped into a new wrapper)
      'utopia-storyboard-uid/scene-aaa/app-entity:aaa/parent/child-c/wrapper-fragment/hello2', // <- the inserted element! (wrapped into a new wrapper)
      'utopia-storyboard-uid/scene-aaa/app-entity:aaa/parent/child-d',
    ])
  })
})

describe('transformJSXComponentAtPath', () => {
  function findElement(components: Array<UtopiaJSXComponent>, pathString: string) {
    const path = fromStringStatic(pathString)
    const foundElement = findJSXElementChildAtPath(
      getComponentsFromTopLevelElements(components),
      path,
    )
    return foundElement
  }

  function createTestComponentsForSnippet(snippet: string) {
    const projectContents = createTestProjectWithCode(
      makeTestProjectCodeWithSnippet(snippet),
    ).projectContents

    // console.log(JSON.stringify(projectContents))

    const file = getProjectFileByFilePath(projectContents, StoryboardFilePath)

    if (file?.type !== 'TEXT_FILE' || file.lastParseSuccess == null) {
      throw new Error('failed parsing the test project file')
    }

    return getComponentsFromTopLevelElements(file.lastParseSuccess.topLevelElements)
  }

  it('throws exception on missing child of jsx element', () => {
    const components = createTestComponentsForSnippet(`
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
    `)

    const pathToModify = 'utopia-storyboard-uid/scene-aaa/app-entity:aaa/parent/child-b/xxx'

    expect(() =>
      transformJSXComponentAtPath(
        components,
        EP.dynamicPathToStaticPath(EP.fromString(pathToModify)),
        (element) => {
          if (isJSXElement(element)) {
            return {
              ...element,
              children: [jsxTextBlock('hello')],
            }
          }
          return element
        },
      ),
    ).toThrow()
  })
  // TODO activate this after removing absolutely stupid way of falling back to the conditional when the branch doesn't exist
  xit('throws exception on missing branch of conditional', () => {
    const components = createTestComponentsForSnippet(`
    <div style={{ ...props.style }} data-uid='aaa'>
      <div data-uid='parent' >
        <div data-uid='child-a' />
        <div data-uid='child-b' />
        {
          // @utopia/uid=cond
          true ? <div data-uid='eee' /> : null
        }
        <div data-uid='child-d' />
      </div>
    </div>
    `)

    const pathToModify = 'utopia-storyboard-uid/scene-aaa/app-entity:aaa/parent/cond/xxx'

    expect(() =>
      transformJSXComponentAtPath(
        components,
        EP.dynamicPathToStaticPath(EP.fromString(pathToModify)),
        (element) => {
          if (isJSXElement(element)) {
            return {
              ...element,
              children: [jsxTextBlock('hello')],
            }
          }
          return element
        },
      ),
    ).toThrow()
  })
  it('throws exception on missing elementsWithin of expression', () => {
    const components = createTestComponentsForSnippet(`
    <div style={{ ...props.style }} data-uid='aaa'>
      <div data-uid='parent' >
        <div data-uid='child-a' />
        <div data-uid='child-b' />
        {(() => { return <div data-uid='eee' /> })()}
        <div data-uid='child-d' />
      </div>
    </div>
    `)

    const pathToModify = 'utopia-storyboard-uid/scene-aaa/app-entity:aaa/parent/2f2/xxx'

    expect(() =>
      transformJSXComponentAtPath(
        components,
        EP.dynamicPathToStaticPath(EP.fromString(pathToModify)),
        (element) => {
          if (isJSXElement(element)) {
            return {
              ...element,
              children: [jsxTextBlock('hello')],
            }
          }
          return element
        },
      ),
    ).toThrow()
  })
  it('updates a jsx element with jsx element parent', () => {
    const components = createTestComponentsForSnippet(`
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
    `)

    const pathToModify = 'utopia-storyboard-uid/scene-aaa/app-entity:aaa/parent/child-b'

    const updatedComponents = transformJSXComponentAtPath(
      components,
      EP.dynamicPathToStaticPath(EP.fromString(pathToModify)),
      (element) => {
        if (isJSXElement(element)) {
          return {
            ...element,
            children: [jsxTextBlock('hello')],
          }
        }
        return element
      },
    )
    const updatedElement = findElement(updatedComponents, pathToModify) as JSXElement
    expect(updatedElement.children).toHaveLength(1)
    expect(updatedElement.children[0].type).toEqual('JSX_TEXT_BLOCK')
    expect((updatedElement.children[0] as JSXTextBlock).text).toEqual('hello')
  })
  it('updates a conditional expression with jsx element parent', () => {
    const components = createTestComponentsForSnippet(`
    <div style={{ ...props.style }} data-uid='aaa'>
      <div data-uid='parent' >
        <div data-uid='child-a' />
        <div data-uid='child-b' />
        {
          // @utopia/uid=cond
          true ? <div data-uid='eee' /> : null
        }
        <div data-uid='child-d' />
      </div>
    </div>
    `)

    const pathToModify = 'utopia-storyboard-uid/scene-aaa/app-entity:aaa/parent/cond'

    const updatedComponents = transformJSXComponentAtPath(
      components,
      EP.dynamicPathToStaticPath(EP.fromString(pathToModify)),
      (element) => {
        if (isJSXConditionalExpression(element)) {
          return {
            ...element,
            whenFalse: jsxTextBlock('hello'),
          }
        }
        return element
      },
    )
    const updatedElement = findElement(updatedComponents, pathToModify) as JSXConditionalExpression
    expect(updatedElement.whenFalse.type).toEqual('JSX_TEXT_BLOCK')
  })
  it('updates a jsx element inside a conditional true branch', () => {
    const components = createTestComponentsForSnippet(`
    <div style={{ ...props.style }} data-uid='aaa'>
      <div data-uid='parent' >
        <div data-uid='child-a' />
        <div data-uid='child-b' />
        {
          // @utopia/uid=cond
          true ? <div data-uid='eee' /> : null
        }
        <div data-uid='child-d' />
      </div>
    </div>
    `)

    const pathToModify = 'utopia-storyboard-uid/scene-aaa/app-entity:aaa/parent/cond/eee'

    const updatedComponents = transformJSXComponentAtPath(
      components,
      EP.dynamicPathToStaticPath(EP.fromString(pathToModify)),
      (element) => {
        if (isJSXElement(element)) {
          return {
            ...element,
            children: [jsxTextBlock('hello')],
          }
        }
        return element
      },
    )
    const updatedElement = findElement(updatedComponents, pathToModify) as JSXElement
    expect(updatedElement.children).toHaveLength(1)
    expect(updatedElement.children[0].type).toEqual('JSX_TEXT_BLOCK')
    expect((updatedElement.children[0] as JSXTextBlock).text).toEqual('hello')
  })
  it('updates an element inside a conditional false branch', () => {
    const components = createTestComponentsForSnippet(`
    <div style={{ ...props.style }} data-uid='aaa'>
      <div data-uid='parent' >
        <div data-uid='child-a' />
        <div data-uid='child-b' />
        {
          // @utopia/uid=cond
          true ? <div data-uid='eee' /> : null
        }
        <div data-uid='child-d' />
      </div>
    </div>
    `)

    const pathToModify = 'utopia-storyboard-uid/scene-aaa/app-entity:aaa/parent/cond/eee'

    const updatedComponents = transformJSXComponentAtPath(
      components,
      EP.dynamicPathToStaticPath(EP.fromString(pathToModify)),
      (element) => {
        if (isJSXElement(element)) {
          return {
            ...element,
            children: [jsxTextBlock('hello')],
          }
        }
        return element
      },
    )
    const updatedElement = findElement(updatedComponents, pathToModify) as JSXElement
    expect(updatedElement.children).toHaveLength(1)
    expect(updatedElement.children[0].type).toEqual('JSX_TEXT_BLOCK')
    expect((updatedElement.children[0] as JSXTextBlock).text).toEqual('hello')
  })
  it('updates an attribute value expression in a conditional true branch', () => {
    const components = createTestComponentsForSnippet(`
    <div style={{ ...props.style }} data-uid='aaa'>
      <div data-uid='parent' >
        <div data-uid='child-a' />
        <div data-uid='child-b' />
        {
          // @utopia/uid=cond
          true ? 'hello' : <div/>
        }
        <div data-uid='child-d' />
      </div>
    </div>
    `)

    const pathToModify =
      'utopia-storyboard-uid/scene-aaa/app-entity:aaa/parent/cond/03d0498839c0fb8a34086cd891c8cee7'

    const updatedComponents = transformJSXComponentAtPath(
      components,
      EP.dynamicPathToStaticPath(EP.fromString(pathToModify)),
      (element) => {
        if (isJSXAttributeValue(element)) {
          return {
            ...element,
            value: 'hello2',
          }
        }
        return element
      },
    )

    const updatedElement = findElement(updatedComponents, pathToModify)
    expect((updatedElement as any).type).toEqual('ATTRIBUTE_VALUE')
    expect((updatedElement as any).value).toEqual('hello2')
  })
  it('updates other javascript expression', () => {
    const components = createTestComponentsForSnippet(`
    <div style={{ ...props.style }} data-uid='aaa'>
      <div data-uid='parent' >
        <div data-uid='child-a' />
        <div data-uid='child-b' />
        {
          // @utopia/uid=expr
          (() => { return 'hello' })()
        }
        <div data-uid='child-d' />
      </div>
    </div>
    `)

    const pathToModify = 'utopia-storyboard-uid/scene-aaa/app-entity:aaa/parent/expr'

    const updatedComponents = transformJSXComponentAtPath(
      components,
      EP.dynamicPathToStaticPath(EP.fromString(pathToModify)),
      (element) => {
        if (isJSExpressionMapOrOtherJavaScript(element)) {
          return {
            ...element,
            javascriptWithUIDs: "(() => { return 'hello2' })()",
          }
        }
        return element
      },
    )

    const updatedElement = findElement(updatedComponents, pathToModify)
    expect((updatedElement as JSExpressionOtherJavaScript).type).toEqual(
      'ATTRIBUTE_OTHER_JAVASCRIPT',
    )
    expect((updatedElement as JSExpressionOtherJavaScript).javascriptWithUIDs).toEqual(
      "(() => { return 'hello2' })()",
    )
  })
  // enable it after findJSXElementPath is fixed too
  xit('updates elementsWithin of other javascript expression', () => {
    const components = createTestComponentsForSnippet(`
    <div style={{ ...props.style }} data-uid='aaa'>
      <div data-uid='parent' >
        <div data-uid='child-a' />
        <div data-uid='child-b' />
        {(() => { return <div data-uid='eee' /> })()}
        <div data-uid='child-d' />
      </div>
    </div>
    `)

    const pathToModify = 'utopia-storyboard-uid/scene-aaa/app-entity:aaa/parent/2f2/eee'

    const updatedComponents = transformJSXComponentAtPath(
      components,
      EP.dynamicPathToStaticPath(EP.fromString(pathToModify)),
      (element) => {
        if (isJSXElement(element)) {
          return {
            ...element,
            children: [jsxTextBlock('hello')],
          }
        }
        return element
      },
    )

    const updatedElement = findElement(components, pathToModify) as JSXElement
    expect(updatedElement.children).toHaveLength(1)
    expect(updatedElement.children[0].type).toEqual('JSX_TEXT_BLOCK')
    expect((updatedElement.children[0] as JSXTextBlock).text).toEqual('hello')
  })
})

describe('renameJSXElementChild', () => {
  it('should rename a simple element', () => {
    const jsxElementToRename = jsxElement('Flex', 'flex-1', [], [])
    const expected = jsxElement('Flex2', 'flex-1', [], [])
    const actual = renameJsxElementChild(jsxElementToRename, new Map([['Flex', 'Flex2']]))
    expect(actual).toEqual(expected)
  })

  it('should rename a nested element', () => {
    const jsxElementToRename = jsxElement(
      'Flex',
      'flex-1',
      [],
      [jsxElement('Flex', 'flex-2', [], [])],
    )
    const expected = jsxElement('Flex2', 'flex-1', [], [jsxElement('Flex2', 'flex-2', [], [])])
    const actual = renameJsxElementChild(jsxElementToRename, new Map([['Flex', 'Flex2']]))
    expect(actual).toEqual(expected)
  })

  it('should rename an element inside a conditional', () => {
    const jsxConditionalToRename = jsxConditionalExpression(
      'cond-1',
      jsExpressionValue('true', emptyComments, 'x'),
      'true',
      jsxElement('View', 'view-1', [], []),
      jsxElement('Flex', 'flex-2', [], []),
      emptyComments,
    )
    const expected = jsxConditionalExpression(
      'cond-1',
      jsExpressionValue('true', emptyComments, 'x'),
      'true',
      jsxElement('View', 'view-1', [], []),
      jsxElement('Flex2', 'flex-2', [], []),
      emptyComments,
    )
    const actual = renameJsxElementChild(jsxConditionalToRename, new Map([['Flex', 'Flex2']]))
    expect(actual).toEqual(expected)
  })

  it('should rename an element inside a fragment', () => {
    const jsxFragmentToRename = jsxFragment(
      'fragment-1',
      [jsxElement('Flex', 'flex-1', [], [])],
      true,
    )
    const expected = jsxFragment('fragment-1', [jsxElement('Flex2', 'flex-1', [], [])], true)
    const actual = renameJsxElementChild(jsxFragmentToRename, new Map([['Flex', 'Flex2']]))
    expect(actual).toEqual(expected)
  })
})
