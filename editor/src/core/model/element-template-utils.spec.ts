import * as R from 'ramda'
import {
  jsxAttributeValue,
  isJSXAttributeValue,
  jsxAttributeFunctionCall,
  UtopiaJSXComponent,
  JSXElement,
  jsxElement,
  utopiaJSXComponent,
  JSXElementChild,
  defaultPropsParam,
  jsxAttributesFromMap,
  getJSXAttribute,
} from '../shared/element-template'
import { getUtopiaID, guaranteeUniqueUids, removeJSXElementChild } from './element-template-utils'
import Utils from '../../utils/utils'
import { BakedInStoryboardUID } from './scene-utils'
import { emptyComments } from '../workers/parser-printer/parser-printer-comments'
import { testStaticElementPath } from '../shared/element-path.test-utils'

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
      fail('Unable to find uid for element.')
    } else if (isJSXAttributeValue(fixedElementUIDProp)) {
      const fixedElementUID = (fixedElement as JSXElement).uid
      expect(fixedElementUID).toEqual(fixedElementUIDProp.value)
      expect(fixedElementUID).not.toEqual('')
    } else {
      fail('fixedElementUIDProp is not a simple value')
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
    const expectedResult = R.over(
      R.lensPath([1, 'rootElement', 'children']),
      (children: Array<JSXElementChild>) => [children[1], children[2]],
      utopiaComponents,
    )
    expect(updatedElements).toEqual(expectedResult)
  })
})
