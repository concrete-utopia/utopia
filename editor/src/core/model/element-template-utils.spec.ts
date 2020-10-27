import * as R from 'ramda'
import {
  jsxAttributeValue,
  isJSXAttributeValue,
  jsxAttributeFunctionCall,
  UtopiaJSXComponent,
  JSXElement,
  jsxElement,
  utopiaJSXComponent,
  JSXAttribute,
  JSXElementChild,
  defaultPropsParam,
} from '../shared/element-template'
import { getUtopiaID, guaranteeUniqueUids, removeJSXElementChild } from './element-template-utils'
import * as TP from '../shared/template-path'
import Utils from '../../utils/utils'
import { BakedInStoryboardUID } from './scene-utils'

describe('guaranteeUniqueUids', () => {
  it('if two siblings have the same ID, one will be replaced', () => {
    const exampleElements = [
      jsxElement('View', { 'data-uid': jsxAttributeValue('aaa') }, []),
      jsxElement('View', { 'data-uid': jsxAttributeValue('aaa') }, []),
    ]
    const fixedElements = guaranteeUniqueUids(exampleElements, [])

    const child0UID = Utils.path([0, 'props', 'data-uid'], fixedElements)
    expect(child0UID).toEqual(jsxAttributeValue('aaa'))
    const child1UID = Utils.path([1, 'props', 'data-uid'], fixedElements)
    expect(child1UID).not.toEqual(jsxAttributeValue('aaa'))
  })

  it('if an element has an existing value, it will be replaced', () => {
    const exampleElements = [
      jsxElement('View', { 'data-uid': jsxAttributeValue('aaa') }, []),
      jsxElement('View', { 'data-uid': jsxAttributeValue('aab') }, []),
    ]
    const existingIDs = ['aab', 'bbb']
    const fixedElements = guaranteeUniqueUids(exampleElements, existingIDs)

    const child0UID = Utils.path([0, 'props', 'data-uid'], fixedElements)
    expect(child0UID).toEqual(jsxAttributeValue('aaa'))
    const child1UID = Utils.path([1, 'props', 'data-uid'], fixedElements)
    expect(child1UID).not.toEqual(jsxAttributeValue('aab'))
  })

  it('if the uid prop is not a simple value, replace it with a simple value', () => {
    const exampleElement = jsxElement(
      'View',
      { 'data-uid': jsxAttributeFunctionCall('someFunction', []) },
      [],
    )
    const fixedElements = guaranteeUniqueUids([exampleElement], [])

    const fixedElementUID = Utils.path<JSXAttribute>([0, 'props', 'data-uid'], fixedElements)
    if (fixedElementUID == null) {
      fail('Unable to find uid for element.')
    } else {
      expect(isJSXAttributeValue(fixedElementUID)).toBeTruthy()
    }
  })
})

describe('getUtopiaID', () => {
  it('returns an id if there is one', () => {
    const element = jsxElement('View', { 'data-uid': jsxAttributeValue('hello') }, [])
    const id = getUtopiaID(element as JSXElement)
    expect(id).toEqual('hello')
  })

  it('throws if there is no ID', () => {
    const element = jsxElement('View', {} as any, [])
    expect(() => {
      getUtopiaID(element as JSXElement)
    }).toThrow()
  })

  it('throws if there is an ID which is not a simple jsx attribute value', () => {
    const element = jsxElement('View', { 'data-uid': jsxAttributeFunctionCall('hello', []) }, [])
    expect(() => {
      getUtopiaID(element as JSXElement)
    }).toThrow()
  })
})

describe('removeJSXElementChild', () => {
  const utopiaComponents: UtopiaJSXComponent[] = [
    utopiaJSXComponent(
      'test1',
      true,
      defaultPropsParam,
      [],
      jsxElement('View', { 'data-uid': jsxAttributeValue('aaa'), prop1: jsxAttributeValue(5) }, []),
      null,
    ),
    utopiaJSXComponent(
      'test2WithChildren',
      true,
      defaultPropsParam,
      [],
      jsxElement('View', { 'data-uid': jsxAttributeValue('aab'), prop2: jsxAttributeValue(15) }, [
        jsxElement('View', { 'data-uid': jsxAttributeValue('aac') }, []),
        jsxElement(
          'View',
          { 'data-uid': jsxAttributeValue('aad'), prop3: jsxAttributeValue(100) },
          [],
        ),
        jsxElement('View', { 'data-uid': jsxAttributeValue('aae') }, []),
      ]),
      null,
    ),
  ]
  xit('removes a root element', () => {
    // TODO Scene Implementation
    const updatedElements = removeJSXElementChild(
      TP.staticInstancePath([BakedInStoryboardUID, 'scene-aaa'], ['aaa']),
      utopiaComponents,
    )
    expect(updatedElements.length).toEqual(1)
    expect(updatedElements[0]).toEqual(utopiaComponents[1])
  })
  it('removes a non-root element', () => {
    const updatedElements = removeJSXElementChild(
      TP.staticInstancePath([BakedInStoryboardUID, 'scene-aaa'], ['aab', 'aac']),
      utopiaComponents,
    )
    const expectedResult = R.over(
      R.lensPath([1, 'rootElement', 'children']),
      (children: Array<JSXElementChild>) => [children[1], children[2]],
      utopiaComponents,
    )
    expect(updatedElements).toEqual(expectedResult)
  })
})
