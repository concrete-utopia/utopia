import { JSXElementName } from '../core/shared/element-template'
import { PropertyPath } from '../core/shared/project-file-types'
import * as TP from '../core/shared/template-path'
import {
  JSXElementNameKeepDeepEqualityCall,
  PropertyPathKeepDeepEquality,
  TemplatePathKeepDeepEquality,
} from './deep-equality-instances'

describe('TemplatePathKeepDeepEquality', () => {
  it('same reference returns the same reference', () => {
    const path = TP.templatePath([['scene'], ['aaa', 'bbb']])
    const result = TemplatePathKeepDeepEquality(path, path)
    expect(result.value).toBe(path)
    expect(result.areEqual).toEqual(true)
  })
  it('same value returns the same reference', () => {
    const oldPath = TP.templatePath([['scene'], ['aaa', 'bbb']])
    const newPath = TP.templatePath([['scene'], ['aaa', 'bbb']])
    const result = TemplatePathKeepDeepEquality(oldPath, newPath)
    expect(result.value).toBe(oldPath)
    expect(result.areEqual).toEqual(true)
  })
  xit('different but similar value handled appropriately', () => {
    // FIXME Do we still want or care about this?
    const oldPath = TP.templatePath([['scene'], ['aaa', 'bbb']])
    const newPath = TP.templatePath([['scene'], ['aaa', 'ccc']])
    const result = TemplatePathKeepDeepEquality(oldPath, newPath)
    expect(result.value).toEqual(newPath)
    expect(result.value.parts[0]).toBe(oldPath.parts[0])
    expect(result.value.parts[1]).toEqual(newPath.parts[1])
    expect(result.areEqual).toEqual(false)
  })
})

describe('PropertyPathKeepDeepEquality', () => {
  it('same reference returns the same reference', () => {
    const path: PropertyPath = {
      propertyElements: ['style', 'backgroundColor'],
    }
    const result = PropertyPathKeepDeepEquality(path, path)
    expect(result.value).toBe(path)
    expect(result.areEqual).toEqual(true)
  })
  it('same value returns the same reference', () => {
    const oldPath: PropertyPath = {
      propertyElements: ['style', 'backgroundColor'],
    }
    const newPath: PropertyPath = {
      propertyElements: ['style', 'backgroundColor'],
    }
    const result = PropertyPathKeepDeepEquality(oldPath, newPath)
    expect(result.value).toBe(oldPath)
    expect(result.areEqual).toEqual(true)
  })
  it('different but similar value handled appropriately', () => {
    const oldPath: PropertyPath = {
      propertyElements: ['style', 'backgroundColor'],
    }
    const newPath: PropertyPath = {
      propertyElements: ['style', 'someOtherColor'],
    }
    const result = PropertyPathKeepDeepEquality(oldPath, newPath)
    expect(result.value).toEqual(newPath)
    expect(result.areEqual).toEqual(false)
  })
})

describe('JSXElementNameKeepDeepEqualityCall', () => {
  it('same reference returns the same reference', () => {
    const elementName: JSXElementName = {
      baseVariable: 'React',
      propertyPath: {
        propertyElements: ['style', 'backgroundColor'],
      },
    }
    const result = JSXElementNameKeepDeepEqualityCall()(elementName, elementName)
    expect(result.value).toBe(elementName)
    expect(result.areEqual).toEqual(true)
  })
  it('same value returns the same reference', () => {
    const oldElementName: JSXElementName = {
      baseVariable: 'React',
      propertyPath: {
        propertyElements: ['style', 'backgroundColor'],
      },
    }
    const newElementName: JSXElementName = {
      baseVariable: 'React',
      propertyPath: {
        propertyElements: ['style', 'backgroundColor'],
      },
    }
    const result = JSXElementNameKeepDeepEqualityCall()(oldElementName, newElementName)
    expect(result.value).toBe(oldElementName)
    expect(result.areEqual).toEqual(true)
  })
  it('different but similar value handled appropriately', () => {
    const oldElementName: JSXElementName = {
      baseVariable: 'React',
      propertyPath: {
        propertyElements: ['style', 'backgroundColor'],
      },
    }
    const newElementName: JSXElementName = {
      baseVariable: 'BetterReact',
      propertyPath: {
        propertyElements: ['style', 'backgroundColor'],
      },
    }
    const result = JSXElementNameKeepDeepEqualityCall()(oldElementName, newElementName)
    expect(result.value).toEqual(newElementName)
    expect(result.value.baseVariable).toEqual(newElementName.baseVariable)
    expect(result.value.propertyPath).toBe(oldElementName.propertyPath)
    expect(result.areEqual).toEqual(false)
  })
})
