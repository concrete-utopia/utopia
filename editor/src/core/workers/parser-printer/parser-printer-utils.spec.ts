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
      jsxElement('View', { 'data-uid': jsxAttributeValue('aa', emptyComments) }, []),
      null,
      false,
      emptyComments,
      emptyComments,
    )
    const fixedComponent = guaranteeUniqueUidsFromTopLevel([exampleComponent])[0]
    expect(Utils.path(['rootElement', 'props', 'data-uid'], fixedComponent)).toBeDefined()
  })

  it('if two siblings have the same ID, one will be replaced', () => {
    const exampleComponent = utopiaJSXComponent(
      'Output',
      true,
      'var',
      'block',
      defaultPropsParam,
      [],
      jsxElement('View', { 'data-uid': jsxAttributeValue('root', emptyComments) }, [
        jsxElement('View', { 'data-uid': jsxAttributeValue('aaa', emptyComments) }, []),
        jsxElement('View', { 'data-uid': jsxAttributeValue('aaa', emptyComments) }, []),
      ]),
      null,
      false,
      emptyComments,
      emptyComments,
    )
    const fixedComponent = guaranteeUniqueUidsFromTopLevel([exampleComponent])[0]
    const child0UID = Utils.path(
      ['rootElement', 'children', 0, 'props', 'data-uid'],
      fixedComponent,
    )
    expect(child0UID).toEqual(jsxAttributeValue('aaa', emptyComments))
    const child1UID = Utils.path(
      ['rootElement', 'children', 1, 'props', 'data-uid'],
      fixedComponent,
    )
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
      jsxElement('View', { 'data-uid': jsxAttributeFunctionCall('someFunction', []) }, []),
      null,
      false,
      emptyComments,
      emptyComments,
    )
    const fixedComponent = guaranteeUniqueUidsFromTopLevel([exampleComponent])[0]

    const uidProp = Utils.path<JSXAttribute>(['rootElement', 'props', 'data-uid'], fixedComponent)
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
      jsxElement('View', { 'data-uid': jsxAttributeValue('baa', emptyComments) }, [
        jsxElement('View', { 'data-uid': jsxAttributeValue('aaa', emptyComments) }, []),
        jsxElement('View', { 'data-uid': jsxAttributeValue('aab', emptyComments) }, []),
      ]),
      null,
      false,
      emptyComments,
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
      jsxElement('View', { 'data-uid': jsxAttributeValue('baa', emptyComments) }, [
        jsxElement('View', { 'data-uid': jsxAttributeValue('aaa', emptyComments) }, []),
        jsxElement('View', {} as any, []),
      ]),
      null,
      false,
      emptyComments,
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
      jsxElement('View', { 'data-uid': jsxAttributeValue('baa', emptyComments) }, [
        jsxElement('View', { 'data-uid': jsxAttributeValue('aaa', emptyComments) }, [
          jsxElement('View', { 'data-uid': jsxAttributeValue('aab', emptyComments) }, []),
          jsxElement('View', { 'data-uid': jsxAttributeValue('aac', emptyComments) }, []),
        ]),
        jsxElement('View', {}, []),
      ]),
      null,
      false,
      emptyComments,
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
