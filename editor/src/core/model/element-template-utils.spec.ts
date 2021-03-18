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
  jsxAttributesFromMap,
  getJSXAttribute,
} from '../shared/element-template'
import { getUtopiaID, guaranteeUniqueUids, removeJSXElementChild } from './element-template-utils'
import * as TP from '../shared/template-path'
import Utils from '../../utils/utils'
import { BakedInStoryboardUID } from './scene-utils'
import { emptyComments } from '../workers/parser-printer/parser-printer-comments'
import { testStaticInstancePath, testStaticScenePath } from '../shared/template-path.test-utils'

describe('guaranteeUniqueUids', () => {
  it('if two siblings have the same ID, one will be replaced', () => {
    const exampleElements = [
      jsxElement(
        'View',
        jsxAttributesFromMap({ 'data-uid': jsxAttributeValue('aaa', emptyComments) }),
        [],
      ),
      jsxElement(
        'View',
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
        jsxAttributesFromMap({ 'data-uid': jsxAttributeValue('aaa', emptyComments) }),
        [],
      ),
      jsxElement(
        'View',
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
      jsxAttributesFromMap({ 'data-uid': jsxAttributeFunctionCall('someFunction', []) }),
      [],
    )
    const fixedElements = guaranteeUniqueUids([exampleElement], [])

    const fixedElementProps = Utils.pathOr([], [0, 'props'], fixedElements)
    const fixedElementUID = getJSXAttribute(fixedElementProps, 'data-uid')
    if (fixedElementUID == null) {
      fail('Unable to find uid for element.')
    } else {
      expect(isJSXAttributeValue(fixedElementUID)).toBeTruthy()
    }
  })
})

describe('getUtopiaID', () => {
  it('returns an id if there is one', () => {
    const element = jsxElement(
      'View',
      jsxAttributesFromMap({ 'data-uid': jsxAttributeValue('hello', emptyComments) }),
      [],
    )
    const id = getUtopiaID(element as JSXElement)
    expect(id).toEqual('hello')
  })

  it('throws if there is no ID', () => {
    const element = jsxElement('View', [], [])
    expect(() => {
      getUtopiaID(element as JSXElement)
    }).toThrow()
  })

  it('throws if there is an ID which is not a simple jsx attribute value', () => {
    const element = jsxElement(
      'View',
      jsxAttributesFromMap({ 'data-uid': jsxAttributeFunctionCall('hello', []) }),
      [],
    )
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
      'var',
      'block',
      defaultPropsParam,
      [],
      jsxElement(
        'View',
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
        jsxAttributesFromMap({
          'data-uid': jsxAttributeValue('aab', emptyComments),
          prop2: jsxAttributeValue(15, emptyComments),
        }),
        [
          jsxElement(
            'View',
            jsxAttributesFromMap({ 'data-uid': jsxAttributeValue('aac', emptyComments) }),
            [],
          ),
          jsxElement(
            'View',
            jsxAttributesFromMap({
              'data-uid': jsxAttributeValue('aad', emptyComments),
              prop3: jsxAttributeValue(100, emptyComments),
            }),
            [],
          ),
          jsxElement(
            'View',
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
      testStaticInstancePath(testStaticScenePath([[BakedInStoryboardUID, 'scene-aaa']]), ['aaa']),
      utopiaComponents,
    )
    expect(updatedElements.length).toEqual(1)
    expect(updatedElements[0]).toEqual(utopiaComponents[1])
  })
  it('removes a non-root element', () => {
    const updatedElements = removeJSXElementChild(
      testStaticInstancePath(testStaticScenePath([[BakedInStoryboardUID, 'scene-aaa']]), [
        'aab',
        'aac',
      ]),
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
