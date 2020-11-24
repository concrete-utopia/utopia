import { JSXElementName } from '../core/shared/element-template'
import { InstancePath, PropertyPath, TemplatePath } from '../core/shared/project-file-types'
import * as TP from '../core/shared/template-path'
import * as PP from '../core/shared/property-path'
import {
  JSXElementNameKeepDeepEqualityCall,
  PropertyPathKeepDeepEquality,
  TemplatePathKeepDeepEquality,
} from './deep-equality-instances'

describe('TemplatePathKeepDeepEquality', () => {
  it('same reference returns the same reference', () => {
    const path: TemplatePath = {
      scene: TP.scenePath(['scene']),
      element: ['aaa', 'bbb'],
    }
    const result = TemplatePathKeepDeepEquality(path, path)
    expect(result.value).toBe(path)
    expect(result.areEqual).toEqual(true)
  })
  it('same value returns the same reference', () => {
    const oldPath: TemplatePath = {
      scene: TP.scenePath(['scene']),
      element: ['aaa', 'bbb'],
    }
    const newPath: TemplatePath = {
      scene: TP.scenePath(['scene']),
      element: ['aaa', 'bbb'],
    }
    const result = TemplatePathKeepDeepEquality(oldPath, newPath)
    expect(result.value).toBe(oldPath)
    expect(result.areEqual).toEqual(true)
  })
  it('different but similar value handled appropriately', () => {
    const oldPath: InstancePath = {
      scene: TP.scenePath(['scene']),
      element: ['aaa', 'bbb'],
    }
    const newPath: InstancePath = {
      scene: TP.scenePath(['scene']),
      element: ['aaa', 'ccc'],
    }
    const result = TemplatePathKeepDeepEquality(oldPath, newPath)
    expect(result.value).toEqual(newPath)
    expect((result.value as InstancePath).scene).toBe(oldPath.scene)
    expect((result.value as InstancePath).element).toEqual(newPath.element)
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
