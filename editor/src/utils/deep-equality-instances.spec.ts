import type { JSXElementName } from '../core/shared/element-template'
import type { PropertyPath } from '../core/shared/project-file-types'
import * as EP from '../core/shared/element-path'
import {
  JSXElementNameKeepDeepEqualityCall,
  PropertyPathKeepDeepEquality,
  ElementPathKeepDeepEquality,
  ElementPathArrayKeepDeepEquality,
  HigherOrderControlArrayKeepDeepEquality,
  EitherKeepDeepEquality,
  NameAndIconResultKeepDeepEquality,
  NameAndIconResultArrayKeepDeepEquality,
  LayoutTargetablePropArrayKeepDeepEquality,
} from './deep-equality-instances'
import type { HigherOrderControl } from '../components/canvas/canvas-types'
import type { Either } from '../core/shared/either'
import { left, right } from '../core/shared/either'
import { arrayDeepEquality, createCallWithTripleEquals } from './deep-equality'
import type { NameAndIconResult } from '../components/inspector/common/name-and-icon-hook'
import {
  DropTargetHint,
  NavigatorState,
  regularNavigatorEntry,
} from '../components/editor/store/editor-state'
import type { LayoutTargetableProp } from '../core/layout/layout-helpers-new'

describe('ElementPathKeepDeepEquality', () => {
  const oldValue = EP.elementPath([['scene'], ['aaa', 'bbb']])
  const newSameValue = EP.elementPath([['scene'], ['aaa', 'bbb']])
  const newDifferentValue = EP.elementPath([['scene'], ['aaa', 'ccc']])

  it('same reference returns the same reference', () => {
    const result = ElementPathKeepDeepEquality(oldValue, oldValue)
    expect(result.value).toBe(oldValue)
    expect(result.areEqual).toEqual(true)
  })
  it('same value returns the same reference', () => {
    const result = ElementPathKeepDeepEquality(oldValue, newSameValue)
    expect(result.value).toBe(oldValue)
    expect(result.areEqual).toEqual(true)
  })
  it('different but similar value handled appropriately', () => {
    const result = ElementPathKeepDeepEquality(oldValue, newDifferentValue)
    expect(result.value.parts).toBe(newDifferentValue.parts)
    expect(result.value).toEqual(newDifferentValue)
    expect(result.areEqual).toEqual(false)
  })
})

describe('ElementPathArrayKeepDeepEquality', () => {
  const oldValue = [EP.elementPath([['scene'], ['aaa', 'bbb']])]
  const newSameValue = [EP.elementPath([['scene'], ['aaa', 'bbb']])]
  const newDifferentValue = [EP.elementPath([['scene'], ['aaa', 'ccc']])]

  it('same reference returns the same reference', () => {
    const result = ElementPathArrayKeepDeepEquality(oldValue, oldValue)
    expect(result.value).toBe(oldValue)
    expect(result.areEqual).toEqual(true)
  })
  it('same value returns the same reference', () => {
    const result = ElementPathArrayKeepDeepEquality(oldValue, newSameValue)
    expect(result.value).toBe(oldValue)
    expect(result.areEqual).toEqual(true)
  })
  it('different but similar value handled appropriately', () => {
    const result = ElementPathArrayKeepDeepEquality(oldValue, newDifferentValue)
    expect(result.value[0]).toBe(newDifferentValue[0])
    expect(result.value).toEqual(newDifferentValue)
    expect(result.areEqual).toEqual(false)
  })
})

describe('PropertyPathKeepDeepEquality', () => {
  const oldValue: PropertyPath = {
    propertyElements: ['style', 'backgroundColor'],
  }
  const newSameValue: PropertyPath = {
    propertyElements: ['style', 'backgroundColor'],
  }
  const newDifferentValue: PropertyPath = {
    propertyElements: ['style', 'someOtherColor'],
  }

  it('same reference returns the same reference', () => {
    const result = PropertyPathKeepDeepEquality()(oldValue, oldValue)
    expect(result.value).toBe(oldValue)
    expect(result.areEqual).toEqual(true)
  })
  it('same value returns the same reference', () => {
    const result = PropertyPathKeepDeepEquality()(oldValue, newSameValue)
    expect(result.value).toBe(oldValue)
    expect(result.areEqual).toEqual(true)
  })
  it('different but similar value handled appropriately', () => {
    const result = PropertyPathKeepDeepEquality()(oldValue, newDifferentValue)
    expect(result.value).toBe(newDifferentValue)
    expect(result.areEqual).toEqual(false)
  })
})

describe('HigherOrderControlArrayKeepDeepEquality', () => {
  const oldValue: Array<HigherOrderControl> = [
    {
      type: 'divControl',
      component: {
        fakeKey: 'I am a component',
      } as any,
      controls: [],
      controlid: 'test',
      followCanvas: 'x',
    },
  ]
  const newSameValue: Array<HigherOrderControl> = [
    {
      type: 'divControl',
      component: {
        fakeKey: 'I am a component',
      } as any,
      controls: [],
      controlid: 'test',
      followCanvas: 'x',
    },
  ]
  const newDifferentValue: Array<HigherOrderControl> = [
    {
      type: 'divControl',
      component: {
        fakeKey: 'I am a component',
      } as any,
      controls: [],
      controlid: 'test-different',
      followCanvas: 'x',
    },
  ]

  it('same reference returns the same reference', () => {
    const result = HigherOrderControlArrayKeepDeepEquality(oldValue, oldValue)
    expect(result.value).toBe(oldValue)
    expect(result.areEqual).toEqual(true)
  })
  it('same value returns the same reference', () => {
    const result = HigherOrderControlArrayKeepDeepEquality(oldValue, newSameValue)
    expect(result.value).toBe(oldValue)
    expect(result.areEqual).toEqual(true)
  })
  it('different but similar value handled appropriately', () => {
    const result = HigherOrderControlArrayKeepDeepEquality(oldValue, newDifferentValue)
    expect(result.value[0]?.type).toBe(oldValue[0]?.type)
    expect(result.value[0]?.component).toBe(oldValue[0]?.component)
    expect(result.value[0]?.controls).toBe(oldValue[0]?.controls)
    expect(result.value[0]?.controlid).toBe(newDifferentValue[0]?.controlid)
    expect(result.value[0]?.followCanvas).toBe(oldValue[0]?.followCanvas)
    expect(result.value).toEqual(newDifferentValue)
    expect(result.areEqual).toEqual(false)
  })
})

describe('JSXElementNameKeepDeepEqualityCall', () => {
  const oldValue: JSXElementName = {
    baseVariable: 'React',
    propertyPath: {
      propertyElements: ['style', 'backgroundColor'],
    },
  }
  const newSameValue: JSXElementName = {
    baseVariable: 'React',
    propertyPath: {
      propertyElements: ['style', 'backgroundColor'],
    },
  }
  const newDifferentValue: JSXElementName = {
    baseVariable: 'BetterReact',
    propertyPath: {
      propertyElements: ['style', 'backgroundColor'],
    },
  }

  it('same reference returns the same reference', () => {
    const result = JSXElementNameKeepDeepEqualityCall(oldValue, oldValue)
    expect(result.value).toBe(oldValue)
    expect(result.areEqual).toEqual(true)
  })
  it('same value returns the same reference', () => {
    const result = JSXElementNameKeepDeepEqualityCall(oldValue, newSameValue)
    expect(result.value).toBe(oldValue)
    expect(result.areEqual).toEqual(true)
  })
  it('different but similar value handled appropriately', () => {
    const result = JSXElementNameKeepDeepEqualityCall(oldValue, newDifferentValue)
    expect(result.value).toEqual(newDifferentValue)
    expect(result.value.baseVariable).toBe(newDifferentValue.baseVariable)
    expect(result.value.propertyPath).toBe(oldValue.propertyPath)
    expect(result.value).toEqual(newDifferentValue)
    expect(result.areEqual).toEqual(false)
  })
})

describe('EitherKeepDeepEquality', () => {
  const oldLeftValue: Either<string, Array<number>> = left('left')
  const newSameLeftValue: Either<string, Array<number>> = left('left')
  const newDifferentLeftValue: Either<string, Array<number>> = left('right')

  const oldRightValue: Either<string, Array<number>> = right([100])
  const newSameRightValue: Either<string, Array<number>> = right([100])
  const newDifferentRightValue: Either<string, Array<number>> = right([200])

  const keepDeepEqualityFn = EitherKeepDeepEquality<string, Array<number>>(
    createCallWithTripleEquals(),
    arrayDeepEquality<number>(createCallWithTripleEquals()),
  )
  it('same reference returns the same reference', () => {
    const leftResult = keepDeepEqualityFn(oldLeftValue, oldLeftValue)
    expect(leftResult.value).toBe(oldLeftValue)
    expect(leftResult.areEqual).toEqual(true)

    const rightResult = keepDeepEqualityFn(oldRightValue, oldRightValue)
    expect(rightResult.value).toBe(oldRightValue)
    expect(rightResult.areEqual).toEqual(true)
  })
  it('same value returns the same reference', () => {
    const leftResult = keepDeepEqualityFn(oldLeftValue, newSameLeftValue)
    expect(leftResult.value).toBe(oldLeftValue)
    expect(leftResult.areEqual).toEqual(true)

    const rightResult = keepDeepEqualityFn(oldRightValue, newSameRightValue)
    expect(rightResult.value).toBe(oldRightValue)
    expect(rightResult.areEqual).toEqual(true)
  })
  it('different but similar value handled appropriately', () => {
    const leftResult = keepDeepEqualityFn(oldLeftValue, newDifferentLeftValue)
    expect(leftResult.value.type).toBe(newDifferentLeftValue.type)
    expect(leftResult.value.value).toBe(newDifferentLeftValue.value)
    expect(leftResult.value).toEqual(newDifferentLeftValue)
    expect(leftResult.areEqual).toEqual(false)

    const rightResult = keepDeepEqualityFn(oldRightValue, newDifferentRightValue)
    expect(rightResult.value.type).toBe(newDifferentRightValue.type)
    expect(rightResult.value.value[0]).toBe(newDifferentRightValue.value[0])
    expect(rightResult.value).toEqual(newDifferentRightValue)
    expect(rightResult.areEqual).toEqual(false)
  })
})

describe('NameAndIconResultKeepDeepEquality', () => {
  const oldValue: NameAndIconResult = {
    path: EP.elementPath([['scene'], ['aaa', 'bbb']]),
    name: {
      baseVariable: 'React',
      propertyPath: {
        propertyElements: ['style', 'backgroundColor'],
      },
    },
    label: 'label',
    iconProps: {
      type: 'magnifyingglass-larger',
      color: 'main',
      width: 18,
      height: 18,
    },
  }
  const newSameValue: NameAndIconResult = {
    path: EP.elementPath([['scene'], ['aaa', 'bbb']]),
    name: {
      baseVariable: 'React',
      propertyPath: {
        propertyElements: ['style', 'backgroundColor'],
      },
    },
    label: 'label',
    iconProps: {
      type: 'magnifyingglass-larger',
      color: 'main',
      width: 18,
      height: 18,
    },
  }
  const newDifferentValue: NameAndIconResult = {
    path: EP.elementPath([['scene'], ['aaa', 'ccc']]),
    name: {
      baseVariable: 'React',
      propertyPath: {
        propertyElements: ['style', 'backgroundColor'],
      },
    },
    label: 'label',
    iconProps: {
      type: 'magnifyingglass-small',
      color: 'main',
      width: 18,
      height: 18,
    },
  }

  it('same reference returns the same reference', () => {
    const result = NameAndIconResultKeepDeepEquality(oldValue, oldValue)
    expect(result.value).toBe(oldValue)
    expect(result.areEqual).toEqual(true)
  })
  it('same value returns the same reference', () => {
    const result = NameAndIconResultKeepDeepEquality(oldValue, newSameValue)
    expect(result.value).toBe(oldValue)
    expect(result.areEqual).toEqual(true)
  })
  it('different but similar value handled appropriately', () => {
    const result = NameAndIconResultKeepDeepEquality(oldValue, newDifferentValue)
    expect(result.value.path).toBe(newDifferentValue.path)
    expect(result.value.name).toBe(oldValue.name)
    expect(result.value.label).toBe(oldValue.label)
    expect(result.value.iconProps).toBe(newDifferentValue.iconProps)
    expect(result.value).toEqual(newDifferentValue)
    expect(result.areEqual).toEqual(false)
  })
})

describe('NameAndIconResultArrayKeepDeepEquality', () => {
  const oldValue: Array<NameAndIconResult> = [
    {
      path: EP.elementPath([['scene'], ['aaa', 'bbb']]),
      name: {
        baseVariable: 'React',
        propertyPath: {
          propertyElements: ['style', 'backgroundColor'],
        },
      },
      label: 'label',
      iconProps: {
        type: 'magnifyingglass-larger',
        color: 'main',
        width: 18,
        height: 18,
      },
    },
  ]
  const newSameValue: Array<NameAndIconResult> = [
    {
      path: EP.elementPath([['scene'], ['aaa', 'bbb']]),
      name: {
        baseVariable: 'React',
        propertyPath: {
          propertyElements: ['style', 'backgroundColor'],
        },
      },
      label: 'label',
      iconProps: {
        type: 'magnifyingglass-larger',
        color: 'main',
        width: 18,
        height: 18,
      },
    },
  ]
  const newDifferentValue: Array<NameAndIconResult> = [
    {
      path: EP.elementPath([['scene'], ['aaa', 'ccc']]),
      name: {
        baseVariable: 'React',
        propertyPath: {
          propertyElements: ['style', 'backgroundColor'],
        },
      },
      label: 'label',
      iconProps: {
        type: 'magnifyingglass-small',
        color: 'main',
        width: 18,
        height: 18,
      },
    },
  ]

  it('same reference returns the same reference', () => {
    const result = NameAndIconResultArrayKeepDeepEquality(oldValue, oldValue)
    expect(result.value).toBe(oldValue)
    expect(result.areEqual).toEqual(true)
  })
  it('same value returns the same reference', () => {
    const result = NameAndIconResultArrayKeepDeepEquality(oldValue, newSameValue)
    expect(result.value).toBe(oldValue)
    expect(result.areEqual).toEqual(true)
  })
  it('different but similar value handled appropriately', () => {
    const result = NameAndIconResultArrayKeepDeepEquality(oldValue, newDifferentValue)
    expect(result.value[0]?.path).toBe(newDifferentValue[0]?.path)
    expect(result.value[0]?.name).toBe(oldValue[0]?.name)
    expect(result.value[0]?.label).toBe(oldValue[0]?.label)
    expect(result.value[0]?.iconProps).toBe(newDifferentValue[0]?.iconProps)
    expect(result.value).toEqual(newDifferentValue)
    expect(result.areEqual).toEqual(false)
  })
})

describe('LayoutTargetablePropArrayKeepDeepEquality', () => {
  const oldValue: Array<LayoutTargetableProp> = ['width', 'height']
  const newSameValue: Array<LayoutTargetableProp> = ['width', 'height']
  const newDifferentValue: Array<LayoutTargetableProp> = ['left', 'top']

  it('same reference returns the same reference', () => {
    const result = LayoutTargetablePropArrayKeepDeepEquality(oldValue, oldValue)
    expect(result.value).toBe(oldValue)
    expect(result.areEqual).toEqual(true)
  })
  it('same value returns the same reference', () => {
    const result = LayoutTargetablePropArrayKeepDeepEquality(oldValue, newSameValue)
    expect(result.value).toBe(oldValue)
    expect(result.areEqual).toEqual(true)
  })
  it('different but similar value handled appropriately', () => {
    const result = LayoutTargetablePropArrayKeepDeepEquality(oldValue, newDifferentValue)
    expect(result.value[0]).toBe(newDifferentValue[0])
    expect(result.value[1]).toBe(newDifferentValue[1])
    expect(result.value).toEqual(newDifferentValue)
    expect(result.areEqual).toEqual(false)
  })
})
