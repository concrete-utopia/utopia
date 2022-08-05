import {
  jsxAttributeValue,
  isJSXAttributeValue,
  jsxAttributeFunctionCall,
  UtopiaJSXComponent,
  JSXElement,
  jsxElement,
  utopiaJSXComponent,
  defaultPropsParam,
  jsxAttributesFromMap,
  getJSXAttribute,
  isJSXElement,
  emptyComments,
  isUtopiaJSXComponent,
} from '../shared/element-template'
import {
  componentUsesProperty,
  getUtopiaID,
  guaranteeUniqueUids,
  removeJSXElementChild,
} from './element-template-utils'
import Utils from '../../utils/utils'
import { BakedInStoryboardUID } from './scene-utils'
import { testStaticElementPath } from '../shared/element-path.test-utils'
import { parseCode } from '../workers/parser-printer/parser-printer'
import { isParseSuccess } from '../shared/project-file-types'

describe('guaranteeUniqueUids', () => {
  it('if two siblings have the same ID, one will be replaced', () => {
    const exampleElements = [
      jsxElement(
        'View',
        'aaa',
        jsxAttributesFromMap({ 'data-uid': jsxAttributeValue('aaa', emptyComments) }),
        [],
      ),
      jsxElement(
        'View',
        'aaa',
        jsxAttributesFromMap({ 'data-uid': jsxAttributeValue('aaa', emptyComments) }),
        [],
      ),
    ]
    const fixedElements = guaranteeUniqueUids(exampleElements, [])

    const child0Props = Utils.pathOr([], [0, 'props'], fixedElements)
    const child0UID = getJSXAttribute(child0Props, 'data-uid')
    expect(child0UID).toEqual(jsxAttributeValue('aaa', emptyComments))
    const child1Props = Utils.pathOr([], [1, 'props'], fixedElements)
    const child1UID = getJSXAttribute(child1Props, 'data-uid')
    expect(child1UID).not.toEqual(jsxAttributeValue('aaa', emptyComments))
  })

  it('if an element has an existing value, it will be replaced', () => {
    const exampleElements = [
      jsxElement(
        'View',
        'aaa',
        jsxAttributesFromMap({ 'data-uid': jsxAttributeValue('aaa', emptyComments) }),
        [],
      ),
      jsxElement(
        'View',
        'aab',
        jsxAttributesFromMap({ 'data-uid': jsxAttributeValue('aab', emptyComments) }),
        [],
      ),
    ]
    const existingIDs = ['aab', 'bbb']
    const fixedElements = guaranteeUniqueUids(exampleElements, existingIDs)

    const child0Props = Utils.pathOr([], [0, 'props'], fixedElements)
    const child0UID = getJSXAttribute(child0Props, 'data-uid')
    const child1Props = Utils.pathOr([], [1, 'props'], fixedElements)
    const child1UID = getJSXAttribute(child1Props, 'data-uid')
    expect(child0UID).toEqual(jsxAttributeValue('aaa', emptyComments))
    expect(child1UID).not.toEqual(jsxAttributeValue('aab', emptyComments))
  })

  it('if the uid prop is not a simple value, replace it with a simple value', () => {
    const exampleElement = jsxElement(
      'View',
      '',
      jsxAttributesFromMap({ 'data-uid': jsxAttributeFunctionCall('someFunction', []) }),
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
      jsxAttributesFromMap({ 'data-uid': jsxAttributeValue('hello', emptyComments) }),
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
          'data-uid': jsxAttributeValue('aaa', emptyComments),
          prop1: jsxAttributeValue(5, emptyComments),
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
          'data-uid': jsxAttributeValue('aab', emptyComments),
          prop2: jsxAttributeValue(15, emptyComments),
        }),
        [
          jsxElement(
            'View',
            'aac',
            jsxAttributesFromMap({ 'data-uid': jsxAttributeValue('aac', emptyComments) }),
            [],
          ),
          jsxElement(
            'View',
            'aad',
            jsxAttributesFromMap({
              'data-uid': jsxAttributeValue('aad', emptyComments),
              prop3: jsxAttributeValue(100, emptyComments),
            }),
            [],
          ),
          jsxElement(
            'View',
            'aae',
            jsxAttributesFromMap({ 'data-uid': jsxAttributeValue('aae', emptyComments) }),
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

function getComponentFromCode(componentName: string, code: string): UtopiaJSXComponent {
  const parseResult = parseCode('test.jsx', code, null, new Set())
  if (isParseSuccess(parseResult)) {
    for (const topLevelElement of parseResult.topLevelElements) {
      if (isUtopiaJSXComponent(topLevelElement) && topLevelElement.name === componentName) {
        return topLevelElement
      }
    }
    throw new Error(`Could not find component ${componentName}`)
  } else {
    throw new Error(`Not a parse success: ${parseResult.type}`)
  }
}

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
