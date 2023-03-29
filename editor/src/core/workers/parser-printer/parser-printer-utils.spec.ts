import {
  utopiaJSXComponent,
  jsxElement,
  jsExpressionValue,
  jsExpressionFunctionCall,
  JSExpression,
  UtopiaJSXComponent,
  TopLevelElement,
  isJSXAttributeValue,
  defaultPropsParam,
  jsxAttributesFromMap,
  getJSXAttribute,
  JSXElement,
  JSXAttributes,
  emptyComments,
} from '../../shared/element-template'
import { guaranteeUniqueUidsFromTopLevel } from './parser-printer-utils'
import Utils from '../../../utils/utils'

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
        'aa',
        jsxAttributesFromMap({ 'data-uid': jsExpressionValue('aa', emptyComments) }),
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
        'root',
        jsxAttributesFromMap({ 'data-uid': jsExpressionValue('root', emptyComments, 'root') }),
        [
          jsxElement(
            'View',
            'aaa',
            jsxAttributesFromMap({ 'data-uid': jsExpressionValue('aaa', emptyComments, 'aaa1') }),
            [],
          ),
          jsxElement(
            'View',
            'aaa',
            jsxAttributesFromMap({ 'data-uid': jsExpressionValue('aaa', emptyComments, 'aaa2') }),
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
    expect(child0UID).toEqual(jsExpressionValue('aaa', emptyComments, 'aaa1'))
    expect(child0?.uid).toEqual('aaa')
    const child1 = Utils.path<JSXElement>(['rootElement', 'children', 1], fixedComponent)
    const child1UID = getJSXAttribute(child1?.props ?? [], 'data-uid')
    expect(child1UID).not.toEqual(jsExpressionValue('aaa', emptyComments, 'aaa2'))
    expect(child1?.uid).not.toEqual('aaa')
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
        'aaa',
        jsxAttributesFromMap({
          'data-uid': jsExpressionFunctionCall('someFunction', [], 'someFunction'),
        }),
        [],
      ),
      null,
      false,
      emptyComments,
    )
    const fixedComponent = guaranteeUniqueUidsFromTopLevel([exampleComponent])[0]

    const rootElement = Utils.path<JSXElement>(['rootElement'], fixedComponent)
    const rootElementProps = Utils.path<JSXAttributes>(['rootElement', 'props'], fixedComponent)
    const uidProp = getJSXAttribute(rootElementProps ?? [], 'data-uid')
    if (uidProp == null) {
      throw new Error('uid prop does not exist')
    } else if (isJSXAttributeValue(uidProp)) {
      expect(rootElement?.uid).toEqual(uidProp.value)
    } else {
      throw new Error('uid prop should be a simple value')
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
        'baa',
        jsxAttributesFromMap({ 'data-uid': jsExpressionValue('baa', emptyComments, 'baa') }),
        [
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

  it('if we had to apply a fix, of course we lose references', () => {
    const exampleComponent = utopiaJSXComponent(
      'Output',
      true,
      'var',
      'block',
      defaultPropsParam,
      [],
      jsxElement(
        'View',
        'baa',
        jsxAttributesFromMap({ 'data-uid': jsExpressionValue('baa', emptyComments, 'baa') }),
        [
          jsxElement(
            'View',
            'aaa',
            jsxAttributesFromMap({ 'data-uid': jsExpressionValue('aaa', emptyComments, 'aaa') }),
            [],
          ),
          jsxElement('View', '', [], []),
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
        'baa',
        jsxAttributesFromMap({ 'data-uid': jsExpressionValue('baa', emptyComments, 'baa') }),
        [
          jsxElement(
            'View',
            'aaa',
            jsxAttributesFromMap({ 'data-uid': jsExpressionValue('aaa', emptyComments, 'aaa') }),
            [
              jsxElement(
                'View',
                'aab',
                jsxAttributesFromMap({
                  'data-uid': jsExpressionValue('aab', emptyComments, 'aab'),
                }),
                [],
              ),
              jsxElement(
                'View',
                'aac',
                jsxAttributesFromMap({
                  'data-uid': jsExpressionValue('aac', emptyComments, 'aac'),
                }),
                [],
              ),
            ],
          ),
          jsxElement('View', '', [], []),
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
