import {
  utopiaJSXComponent,
  jsxElement,
  jsxAttributeValue,
  jsxAttributeFunctionCall,
  JSXAttribute,
  UtopiaJSXComponent,
  TopLevelElement,
  isJSXAttributeValue,
  defaultPropsParam,
  jsxAttributesFromMap,
  getJSXAttribute,
  JSXElement,
  JSXAttributes,
} from '../../shared/element-template'
import { guaranteeUniqueUidsFromTopLevel } from './parser-printer-utils'
import Utils from '../../../utils/utils'
import { emptyComments } from './parser-printer-comments'

describe('guaranteeUniqueUidsFromTopLevel', () => {
  it('creates an ID where there was none', () => {
    const exampleComponent = utopiaJSXComponent(
      'Output',
      true,
      'var',
      'block',
      defaultPropsParam,
      [],
      jsxElement(
        'View',
        jsxAttributesFromMap({ 'data-uid': jsxAttributeValue('aa', emptyComments) }),
        [],
      ),
      null,
      false,
      emptyComments,
    )
    const fixedComponent = guaranteeUniqueUidsFromTopLevel([exampleComponent])[0]
    const rootElementProps = Utils.path<JSXAttributes>(['rootElement', 'props'], fixedComponent)
    expect(getJSXAttribute(rootElementProps ?? [], 'data-uid')).toBeDefined()
  })

  it('if two siblings have the same ID, one will be replaced', () => {
    const exampleComponent = utopiaJSXComponent(
      'Output',
      true,
      'var',
      'block',
      defaultPropsParam,
      [],
      jsxElement(
        'View',
        jsxAttributesFromMap({ 'data-uid': jsxAttributeValue('root', emptyComments) }),
        [
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
        ],
      ),
      null,
      false,
      emptyComments,
    )
    const fixedComponent = guaranteeUniqueUidsFromTopLevel([exampleComponent])[0]
    const child0 = Utils.path<JSXElement>(['rootElement', 'children', 0], fixedComponent)
    const child0UID = getJSXAttribute(child0?.props ?? [], 'data-uid')
    expect(child0UID).toEqual(jsxAttributeValue('aaa', emptyComments))
    const child1 = Utils.path<JSXElement>(['rootElement', 'children', 1], fixedComponent)
    const child1UID = getJSXAttribute(child1?.props ?? [], 'data-uid')
    expect(child1UID).not.toEqual(jsxAttributeValue('aaa', emptyComments))
  })

  it('if the uid prop is not a simple value, replace it with a simple value', () => {
    const exampleComponent = utopiaJSXComponent(
      'Output',
      true,
      'var',
      'block',
      defaultPropsParam,
      [],
      jsxElement(
        'View',
        jsxAttributesFromMap({ 'data-uid': jsxAttributeFunctionCall('someFunction', []) }),
        [],
      ),
      null,
      false,
      emptyComments,
    )
    const fixedComponent = guaranteeUniqueUidsFromTopLevel([exampleComponent])[0]

    const rootElementProps = Utils.path<JSXAttributes>(['rootElement', 'props'], fixedComponent)
    const uidProp = getJSXAttribute(rootElementProps ?? [], 'data-uid')
    if (uidProp == null) {
      fail('uid prop does not exist')
    } else {
      expect(isJSXAttributeValue(uidProp)).toBeTruthy()
    }
  })

  it('if everything is correct, we keep references', () => {
    const exampleComponent = utopiaJSXComponent(
      'Output',
      true,
      'var',
      'block',
      defaultPropsParam,
      [],
      jsxElement(
        'View',
        jsxAttributesFromMap({ 'data-uid': jsxAttributeValue('baa', emptyComments) }),
        [
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
        ],
      ),
      null,
      false,
      emptyComments,
    )
    const fixedComponent = guaranteeUniqueUidsFromTopLevel([exampleComponent])[0]
    expect(
      Utils.path(['rootElement'], exampleComponent) === Utils.path(['rootElement'], fixedComponent),
    ).toBeTruthy()
  })

  it('if we had to apply a fix, of course we loose references', () => {
    const exampleComponent = utopiaJSXComponent(
      'Output',
      true,
      'var',
      'block',
      defaultPropsParam,
      [],
      jsxElement(
        'View',
        jsxAttributesFromMap({ 'data-uid': jsxAttributeValue('baa', emptyComments) }),
        [
          jsxElement(
            'View',
            jsxAttributesFromMap({ 'data-uid': jsxAttributeValue('aaa', emptyComments) }),
            [],
          ),
          jsxElement('View', [], []),
        ],
      ),
      null,
      false,
      emptyComments,
    )
    const fixedComponent = guaranteeUniqueUidsFromTopLevel([exampleComponent])[0]
    expect(
      Utils.path(['rootElement'], exampleComponent) === Utils.path(['rootElement'], fixedComponent),
    ).toBeFalsy()
  })

  it('for subtrees which needed no fix, we keep references', () => {
    const exampleComponent = utopiaJSXComponent(
      'Output',
      true,
      'var',
      'block',
      defaultPropsParam,
      [],
      jsxElement(
        'View',
        jsxAttributesFromMap({ 'data-uid': jsxAttributeValue('baa', emptyComments) }),
        [
          jsxElement(
            'View',
            jsxAttributesFromMap({ 'data-uid': jsxAttributeValue('aaa', emptyComments) }),
            [
              jsxElement(
                'View',
                jsxAttributesFromMap({ 'data-uid': jsxAttributeValue('aab', emptyComments) }),
                [],
              ),
              jsxElement(
                'View',
                jsxAttributesFromMap({ 'data-uid': jsxAttributeValue('aac', emptyComments) }),
                [],
              ),
            ],
          ),
          jsxElement('View', [], []),
        ],
      ),
      null,
      false,
      emptyComments,
    )
    const fixedComponent = guaranteeUniqueUidsFromTopLevel([exampleComponent])[0]
    expect(exampleComponent === fixedComponent).toBeFalsy()
    expect(Utils.path(['rootElement', 'children', 0], exampleComponent)).toBe(
      Utils.path(['rootElement', 'children', 0], fixedComponent),
    )
    expect(Utils.path(['rootElement', 'children', 1], exampleComponent)).not.toBe(
      Utils.path(['rootElement', 'children', 1], fixedComponent),
    )
  })
})
