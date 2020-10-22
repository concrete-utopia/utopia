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
} from '../../shared/element-template'
import {
  guaranteeUniqueUidsFromTopLevel,
  TopLevelElementAndCodeContext,
} from './parser-printer-utils'
import Utils from '../../../utils/utils'

function withBounds(topLevelElement: TopLevelElement): TopLevelElementAndCodeContext {
  return {
    element: topLevelElement,
    bounds: {
      start: {
        line: 20,
        character: 10,
      },
      end: {
        line: 21,
        character: 11,
      },
    },
  }
}

describe('guaranteeUniqueUidsFromTopLevel', () => {
  it('creates an ID where there was none', () => {
    const exampleComponent = utopiaJSXComponent(
      'Output',
      true,
      defaultPropsParam,
      [],
      jsxElement('View', { 'data-uid': jsxAttributeValue('aa') }, []),
      null,
    )
    const fixedComponent = guaranteeUniqueUidsFromTopLevel([withBounds(exampleComponent)])[0]
    expect(
      Utils.path(['element', 'rootElement', 'props', 'data-uid'], fixedComponent),
    ).toBeDefined()
  })

  it('if two siblings have the same ID, one will be replaced', () => {
    const exampleComponent = utopiaJSXComponent(
      'Output',
      true,
      defaultPropsParam,
      [],
      jsxElement('View', { 'data-uid': jsxAttributeValue('root') }, [
        jsxElement('View', { 'data-uid': jsxAttributeValue('aaa') }, []),
        jsxElement('View', { 'data-uid': jsxAttributeValue('aaa') }, []),
      ]),
      null,
    )
    const fixedComponent = guaranteeUniqueUidsFromTopLevel([withBounds(exampleComponent)])[0]
    const child0UID = Utils.path(
      ['element', 'rootElement', 'children', 0, 'props', 'data-uid'],
      fixedComponent,
    )
    expect(child0UID).toEqual(jsxAttributeValue('aaa'))
    const child1UID = Utils.path(
      ['element', 'rootElement', 'children', 1, 'props', 'data-uid'],
      fixedComponent,
    )
    expect(child1UID).not.toEqual(jsxAttributeValue('aaa'))
  })

  it('if the uid prop is not a simple value, replace it with a simple value', () => {
    const exampleComponent = utopiaJSXComponent(
      'Output',
      true,
      defaultPropsParam,
      [],
      jsxElement('View', { 'data-uid': jsxAttributeFunctionCall('someFunction', []) }, []),
      null,
    )
    const fixedComponent = guaranteeUniqueUidsFromTopLevel([withBounds(exampleComponent)])[0]

    const uidProp = Utils.path<JSXAttribute>(
      ['element', 'rootElement', 'props', 'data-uid'],
      fixedComponent,
    )
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
      defaultPropsParam,
      [],
      jsxElement('View', { 'data-uid': jsxAttributeValue('baa') }, [
        jsxElement('View', { 'data-uid': jsxAttributeValue('aaa') }, []),
        jsxElement('View', { 'data-uid': jsxAttributeValue('aab') }, []),
      ]),
      null,
    )
    const fixedComponent = guaranteeUniqueUidsFromTopLevel([withBounds(exampleComponent)])[0]
    expect(
      Utils.path(['rootElement'], exampleComponent) ===
        Utils.path(['element', 'rootElement'], fixedComponent),
    ).toBeTruthy()
  })

  it('if we had to apply a fix, of course we loose references', () => {
    const exampleComponent = withBounds(
      utopiaJSXComponent(
        'Output',
        true,
        defaultPropsParam,
        [],
        jsxElement('View', { 'data-uid': jsxAttributeValue('baa') }, [
          jsxElement('View', { 'data-uid': jsxAttributeValue('aaa') }, []),
          jsxElement('View', {} as any, []),
        ]),
        null,
      ),
    )
    const fixedComponent = guaranteeUniqueUidsFromTopLevel([exampleComponent])[0]
    expect(
      Utils.path(['rootElement'], exampleComponent) ===
        Utils.path(['element', 'rootElement'], fixedComponent),
    ).toBeFalsy()
  })

  it('for subtrees which needed no fix, we keep references', () => {
    const exampleComponent = withBounds(
      utopiaJSXComponent(
        'Output',
        true,
        defaultPropsParam,
        [],
        jsxElement('View', { 'data-uid': jsxAttributeValue('baa') }, [
          jsxElement('View', { 'data-uid': jsxAttributeValue('aaa') }, [
            jsxElement('View', { 'data-uid': jsxAttributeValue('aab') }, []),
            jsxElement('View', { 'data-uid': jsxAttributeValue('aac') }, []),
          ]),
          jsxElement('View', {}, []),
        ]),
        null,
      ),
    )
    const fixedComponent = guaranteeUniqueUidsFromTopLevel([exampleComponent])[0]
    expect(exampleComponent === fixedComponent).toBeFalsy()
    expect(Utils.path(['element', 'rootElement', 'children', 0], exampleComponent)).toBe(
      Utils.path(['element', 'rootElement', 'children', 0], fixedComponent),
    )
    expect(Utils.path(['element', 'rootElement', 'children', 1], exampleComponent)).not.toBe(
      Utils.path(['element', 'rootElement', 'children', 1], fixedComponent),
    )
  })
})
