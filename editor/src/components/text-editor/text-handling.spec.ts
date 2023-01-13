import { getComponentFromCode } from '../../core/model/element-template.test-utils'
import { isJSXElement } from '../../core/shared/element-template'
import { isEligibleForCollapse } from './text-handling'

describe('isEligibleForCollapse', () => {
  it('span element with no properties', () => {
    const component = getComponentFromCode(
      'TestComponent',
      `
export const TestComponent = (props) => {
  return <div data-uid='root-div'>
    <span data-uid='first-span'>First</span>
    <span data-uid='second-span'>Second</span>
  </div>
}
    `,
    )
    const rootElement = component.rootElement
    if (isJSXElement(rootElement)) {
      const firstSpan = rootElement.children[0]
      const secondSpan = rootElement.children[1]
      const actualResult = isEligibleForCollapse(firstSpan, secondSpan)
      expect(actualResult).toEqual(true)
    } else {
      throw new Error('Root element is not an element.')
    }
  })
  it('span element with some ineligible style properties', () => {
    const component = getComponentFromCode(
      'TestComponent',
      `
export const TestComponent = (props) => {
  return <div data-uid='root-div'>
    <span data-uid='first-span' style={{left: 1}}>First</span>
    <span data-uid='second-span' style={{left: 1}}>Second</span>
  </div>
}
    `,
    )
    const rootElement = component.rootElement
    if (isJSXElement(rootElement)) {
      const firstSpan = rootElement.children[0]
      const secondSpan = rootElement.children[1]
      const actualResult = isEligibleForCollapse(firstSpan, secondSpan)
      expect(actualResult).toEqual(false)
    } else {
      throw new Error('Root element is not an element.')
    }
  })
  it('span element with eligible style properties', () => {
    const component = getComponentFromCode(
      'TestComponent',
      `
export const TestComponent = (props) => {
  return <div data-uid='root-div'>
    <span data-uid='first-span' style={{fontWeight: 'bold'}}>First</span>
    <span data-uid='second-span' style={{fontWeight: 'bold'}}>Second</span>
  </div>
}
    `,
    )
    const rootElement = component.rootElement
    if (isJSXElement(rootElement)) {
      const firstSpan = rootElement.children[0]
      const secondSpan = rootElement.children[1]
      const actualResult = isEligibleForCollapse(firstSpan, secondSpan)
      expect(actualResult).toEqual(true)
    } else {
      throw new Error('Root element is not an element.')
    }
  })
  it('span element with other properties', () => {
    const component = getComponentFromCode(
      'TestComponent',
      `
export const TestComponent = (props) => {
  return <div data-uid='root-div'>
    <span data-uid='first-span' otherProp={true}>First</span>
    <span data-uid='second-span' otherProp={true}>Second</span>
  </div>
}
    `,
    )
    const rootElement = component.rootElement
    if (isJSXElement(rootElement)) {
      const firstSpan = rootElement.children[0]
      const secondSpan = rootElement.children[1]
      const actualResult = isEligibleForCollapse(firstSpan, secondSpan)
      expect(actualResult).toEqual(false)
    } else {
      throw new Error('Root element is not an element.')
    }
  })
  it('differing elements', () => {
    const component = getComponentFromCode(
      'TestComponent',
      `
export const TestComponent = (props) => {
  return <div data-uid='root-div'>
    <div data-uid='first-span'>First</span>
    <span data-uid='second-span'>Second</span>
  </div>
}
    `,
    )
    const rootElement = component.rootElement
    if (isJSXElement(rootElement)) {
      const firstSpan = rootElement.children[0]
      const secondSpan = rootElement.children[1]
      const actualResult = isEligibleForCollapse(firstSpan, secondSpan)
      expect(actualResult).toEqual(false)
    } else {
      throw new Error('Root element is not an element.')
    }
  })
  it('span element with other content', () => {
    const component = getComponentFromCode(
      'TestComponent',
      `
export const TestComponent = (props) => {
  return <div data-uid='root-div'>
    <span data-uid='first-span'>First</span>
    <span data-uid='second-span'>Second<div>Oh no</div></span>
  </div>
}
    `,
    )
    const rootElement = component.rootElement
    if (isJSXElement(rootElement)) {
      const firstSpan = rootElement.children[0]
      const secondSpan = rootElement.children[1]
      const actualResult = isEligibleForCollapse(firstSpan, secondSpan)
      expect(actualResult).toEqual(false)
    } else {
      throw new Error('Root element is not an element.')
    }
  })
})
