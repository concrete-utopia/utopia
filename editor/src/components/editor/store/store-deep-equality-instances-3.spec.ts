import type { Sides } from 'utopia-api/core'
import { left, Right } from '../../../core/shared/either'
import type {
  ElementInstanceMetadata,
  ElementInstanceMetadataMap,
  ImportInfo,
  SpecialSizeMeasurements,
} from '../../../core/shared/element-template'
import { createImportedFrom, createNotImported } from '../../../core/shared/element-template'
import type { CanvasRectangle, LocalPoint, LocalRectangle } from '../../../core/shared/math-utils'
import { canvasRectangle, localRectangle } from '../../../core/shared/math-utils'
import {
  CanvasRectangleKeepDeepEquality,
  DropTargetHintKeepDeepEquality,
  ElementInstanceMetadataKeepDeepEquality,
  ElementInstanceMetadataMapKeepDeepEquality,
  ImportInfoKeepDeepEquality,
  LocalPointKeepDeepEquality,
  LocalRectangleKeepDeepEquality,
  NavigatorStateKeepDeepEquality,
  SidesKeepDeepEquality,
  SpecialSizeMeasurementsKeepDeepEquality,
} from './store-deep-equality-instances'
import * as EP from '../../../core/shared/element-path'
import type { DropTargetHint, NavigatorState } from './editor-state'
import { ElementProps, regularNavigatorEntry } from './editor-state'

describe('CanvasRectangleKeepDeepEquality', () => {
  const oldValue: CanvasRectangle = canvasRectangle({
    x: 10,
    y: 20,
    width: 100,
    height: 200,
  })
  const newSameValue: CanvasRectangle = canvasRectangle({
    x: 10,
    y: 20,
    width: 100,
    height: 200,
  })
  const newDifferentValue: CanvasRectangle = canvasRectangle({
    x: 100,
    y: 20,
    width: 100,
    height: 200,
  })

  it('same reference returns the same reference', () => {
    const result = CanvasRectangleKeepDeepEquality(oldValue, oldValue)
    expect(result.value).toBe(oldValue)
    expect(result.areEqual).toEqual(true)
  })
  it('same value returns the same reference', () => {
    const result = CanvasRectangleKeepDeepEquality(oldValue, newSameValue)
    expect(result.value).toBe(oldValue)
    expect(result.areEqual).toEqual(true)
  })
  it('different but similar value handled appropriately', () => {
    const result = CanvasRectangleKeepDeepEquality(oldValue, newDifferentValue)
    expect(result.value).toBe(newDifferentValue)
    expect(result.areEqual).toEqual(false)
  })
})

describe('LocalRectangleKeepDeepEquality', () => {
  const oldValue: LocalRectangle = localRectangle({
    x: 10,
    y: 20,
    width: 100,
    height: 200,
  })
  const newSameValue: LocalRectangle = localRectangle({
    x: 10,
    y: 20,
    width: 100,
    height: 200,
  })
  const newDifferentValue: LocalRectangle = localRectangle({
    x: 100,
    y: 20,
    width: 100,
    height: 200,
  })

  it('same reference returns the same reference', () => {
    const result = LocalRectangleKeepDeepEquality(oldValue, oldValue)
    expect(result.value).toBe(oldValue)
    expect(result.areEqual).toEqual(true)
  })
  it('same value returns the same reference', () => {
    const result = LocalRectangleKeepDeepEquality(oldValue, newSameValue)
    expect(result.value).toBe(oldValue)
    expect(result.areEqual).toEqual(true)
  })
  it('different but similar value handled appropriately', () => {
    const result = LocalRectangleKeepDeepEquality(oldValue, newDifferentValue)
    expect(result.value).toBe(newDifferentValue)
    expect(result.areEqual).toEqual(false)
  })
})

describe('LocalPointKeepDeepEquality', () => {
  const oldValue: LocalPoint = {
    x: 10,
    y: 20,
  } as LocalPoint
  const newSameValue: LocalPoint = {
    x: 10,
    y: 20,
  } as LocalPoint
  const newDifferentValue: LocalPoint = {
    x: 100,
    y: 20,
  } as LocalPoint

  it('same reference returns the same reference', () => {
    const result = LocalPointKeepDeepEquality(oldValue, oldValue)
    expect(result.value).toBe(oldValue)
    expect(result.areEqual).toEqual(true)
  })
  it('same value returns the same reference', () => {
    const result = LocalPointKeepDeepEquality(oldValue, newSameValue)
    expect(result.value).toBe(oldValue)
    expect(result.areEqual).toEqual(true)
  })
  it('different but similar value handled appropriately', () => {
    const result = LocalPointKeepDeepEquality(oldValue, newDifferentValue)
    expect(result.value).toBe(newDifferentValue)
    expect(result.areEqual).toEqual(false)
  })
})

describe('SidesKeepDeepEquality', () => {
  const oldValue: Sides = {
    top: 10,
    right: 20,
    bottom: 30,
    left: 40,
  }
  const newSameValue: Sides = {
    top: 10,
    right: 20,
    bottom: 30,
    left: 40,
  }
  const newDifferentValue: Sides = {
    top: 100,
    right: 20,
    bottom: 30,
    left: 40,
  }

  it('same reference returns the same reference', () => {
    const result = SidesKeepDeepEquality(oldValue, oldValue)
    expect(result.value).toBe(oldValue)
    expect(result.areEqual).toEqual(true)
  })
  it('same value returns the same reference', () => {
    const result = SidesKeepDeepEquality(oldValue, newSameValue)
    expect(result.value).toBe(oldValue)
    expect(result.areEqual).toEqual(true)
  })
  it('different but similar value handled appropriately', () => {
    const result = SidesKeepDeepEquality(oldValue, newDifferentValue)
    expect(result.value).toBe(newDifferentValue)
    expect(result.areEqual).toEqual(false)
  })
})

describe('ImportInfoKeepDeepEquality', () => {
  const originalFilePath = '/utopia/storyboard.js'
  const oldNotImportedValue: ImportInfo = createNotImported(originalFilePath, 'Card')
  const newSameNotImportedValue: ImportInfo = createNotImported(originalFilePath, 'Card')

  const oldValue: ImportInfo = createImportedFrom('old', 'old', 'old')
  const newSameValue: ImportInfo = createImportedFrom('old', 'old', 'old')
  const newDifferentValue: ImportInfo = createImportedFrom('new', 'old', 'old')

  it('same reference returns the same reference', () => {
    const result = ImportInfoKeepDeepEquality(oldValue, oldValue)
    expect(result.value).toBe(oldValue)
    expect(result.areEqual).toEqual(true)

    const resultNotImported = ImportInfoKeepDeepEquality(oldNotImportedValue, oldNotImportedValue)
    expect(resultNotImported.value).toBe(oldNotImportedValue)
    expect(resultNotImported.areEqual).toEqual(true)
  })
  it('same value returns the same reference', () => {
    const result = ImportInfoKeepDeepEquality(oldValue, newSameValue)
    expect(result.value).toBe(oldValue)
    expect(result.areEqual).toEqual(true)

    const resultNotImported = ImportInfoKeepDeepEquality(
      oldNotImportedValue,
      newSameNotImportedValue,
    )
    expect(resultNotImported.value).toBe(oldNotImportedValue)
    expect(resultNotImported.areEqual).toEqual(true)
  })
  it('different but similar value handled appropriately', () => {
    const result = ImportInfoKeepDeepEquality(oldValue, newDifferentValue)
    expect(result.value).toEqual(newDifferentValue)
    expect(result.areEqual).toEqual(false)
  })
})

describe('SpecialSizeMeasurementsKeepDeepEquality', () => {
  const oldValue: SpecialSizeMeasurements = {
    offset: {
      x: 10,
      y: 20,
    } as LocalPoint,
    coordinateSystemBounds: canvasRectangle({
      x: 10,
      y: 20,
      width: 100,
      height: 200,
    }),
    immediateParentBounds: canvasRectangle({
      x: 100,
      y: 200,
      width: 1000,
      height: 2000,
    }),
    globalFrameWithTextContent: canvasRectangle({
      x: 10,
      y: 20,
      width: 100,
      height: 200,
    }),
    immediateParentProvidesLayout: false,
    closestOffsetParentPath: EP.fromString('some/dummy/path'),
    usesParentBounds: false,
    parentLayoutSystem: 'flex',
    layoutSystemForChildren: 'flex',
    providesBoundsForAbsoluteChildren: true,
    display: 'flex',
    position: 'absolute',
    margin: {
      top: 1,
      right: 2,
      bottom: 3,
      left: 4,
    },
    padding: {
      top: 10,
      right: 20,
      bottom: 30,
      left: 40,
    },
    naturalWidth: 100,
    naturalHeight: 200,
    clientWidth: 300,
    clientHeight: 400,
    parentFlexDirection: 'row',
    parentJustifyContent: null,
    parentFlexGap: 0,
    parentPadding: {
      top: undefined,
      right: 0,
      bottom: 0,
      left: 0,
    },
    parentHugsOnMainAxis: false,
    gap: 11,
    flexDirection: 'column',
    justifyContent: 'center',
    alignContent: null,
    alignItems: 'auto',
    htmlElementName: 'div',
    renderedChildrenCount: 10,
    globalContentBoxForChildren: canvasRectangle({
      x: 20,
      y: 40,
      width: 60,
      height: 80,
    }),
    float: 'none',
    hasPositionOffset: false,
    parentTextDirection: 'ltr',
    hasTransform: false,
    borderRadius: {
      top: 10,
      right: 20,
      bottom: 30,
      left: 40,
    },
    fontSize: '16',
    fontWeight: '400',
    fontStyle: 'normal',
    textDecorationLine: 'none',
    textBounds: null,
    computedHugProperty: {
      width: null,
      height: null,
    },
    containerGridProperties: {
      gridTemplateColumns: null,
      gridTemplateRows: null,
      gridAutoColumns: null,
      gridAutoRows: null,
      gridAutoFlow: null,
    },
    elementGridProperties: {
      gridColumnStart: null,
      gridColumnEnd: null,
      gridRowStart: null,
      gridRowEnd: null,
    },
    containerGridPropertiesFromProps: {
      gridTemplateColumns: null,
      gridTemplateRows: null,
      gridAutoColumns: null,
      gridAutoRows: null,
      gridAutoFlow: null,
    },
    elementGridPropertiesFromProps: {
      gridColumnStart: null,
      gridColumnEnd: null,
      gridRowStart: null,
      gridRowEnd: null,
    },
    rowGap: null,
    columnGap: null,
  }

  const newDifferentValue: SpecialSizeMeasurements = {
    offset: {
      x: 10,
      y: 20,
    } as LocalPoint,
    coordinateSystemBounds: canvasRectangle({
      x: 10,
      y: 20,
      width: 100,
      height: 200,
    }),
    immediateParentBounds: canvasRectangle({
      x: 100,
      y: 200,
      width: 1000,
      height: 2000,
    }),
    globalFrameWithTextContent: canvasRectangle({
      x: 10,
      y: 20,
      width: 100,
      height: 200,
    }),
    immediateParentProvidesLayout: true,
    closestOffsetParentPath: EP.fromString('some/dummy/path'),
    usesParentBounds: false,
    parentLayoutSystem: 'flex',
    layoutSystemForChildren: 'flex',
    providesBoundsForAbsoluteChildren: true,
    display: 'flex',
    position: 'absolute',
    margin: {
      top: 1,
      right: 2,
      bottom: 3,
      left: 4,
    },
    padding: {
      top: 10,
      right: 20,
      bottom: 30,
      left: 40,
    },
    naturalWidth: 100,
    naturalHeight: 200,
    clientWidth: 300,
    clientHeight: 400,
    parentFlexDirection: 'row',
    parentJustifyContent: null,
    parentFlexGap: 0,
    parentPadding: {
      top: undefined,
      right: 0,
      bottom: 0,
      left: 0,
    },
    parentHugsOnMainAxis: false,
    gap: 11,
    flexDirection: 'column',
    justifyContent: 'center',
    alignContent: null,
    alignItems: 'auto',
    htmlElementName: 'div',
    renderedChildrenCount: 10,
    globalContentBoxForChildren: canvasRectangle({
      x: 20,
      y: 40,
      width: 60,
      height: 0,
    }),
    float: 'none',
    hasPositionOffset: false,
    parentTextDirection: 'ltr',
    hasTransform: false,
    borderRadius: {
      top: 10,
      right: 20,
      bottom: 30,
      left: 40,
    },
    fontSize: '16',
    fontWeight: '400',
    fontStyle: 'normal',
    textDecorationLine: 'none',
    textBounds: null,
    computedHugProperty: {
      width: null,
      height: null,
    },
    containerGridProperties: {
      gridTemplateColumns: null,
      gridTemplateRows: null,
      gridAutoColumns: null,
      gridAutoRows: null,
      gridAutoFlow: null,
    },
    elementGridProperties: {
      gridColumnStart: null,
      gridColumnEnd: null,
      gridRowStart: null,
      gridRowEnd: null,
    },
    containerGridPropertiesFromProps: {
      gridTemplateColumns: null,
      gridTemplateRows: null,
      gridAutoColumns: null,
      gridAutoRows: null,
      gridAutoFlow: null,
    },
    elementGridPropertiesFromProps: {
      gridColumnStart: null,
      gridColumnEnd: null,
      gridRowStart: null,
      gridRowEnd: null,
    },
    rowGap: null,
    columnGap: null,
  }

  it('same reference returns the same reference', () => {
    const result = SpecialSizeMeasurementsKeepDeepEquality()(oldValue, oldValue)
    expect(result.value).toBe(oldValue)
    expect(result.areEqual).toEqual(true)
  })
  it('different but similar value handled appropriately', () => {
    const result = SpecialSizeMeasurementsKeepDeepEquality()(oldValue, newDifferentValue)
    expect(result.value.offset).toBe(oldValue.offset)
    expect(result.value.coordinateSystemBounds).toBe(oldValue.coordinateSystemBounds)
    expect(result.value.immediateParentBounds).toBe(oldValue.immediateParentBounds)
    expect(result.value.immediateParentProvidesLayout).toBe(
      newDifferentValue.immediateParentProvidesLayout,
    )
    expect(result.value.closestOffsetParentPath).toBe(oldValue.closestOffsetParentPath)
    expect(result.value.usesParentBounds).toBe(oldValue.usesParentBounds)
    expect(result.value.parentLayoutSystem).toBe(oldValue.parentLayoutSystem)
    expect(result.value.layoutSystemForChildren).toBe(oldValue.layoutSystemForChildren)
    expect(result.value.providesBoundsForAbsoluteChildren).toBe(
      oldValue.providesBoundsForAbsoluteChildren,
    )
    expect(result.value.display).toBe(oldValue.display)
    expect(result.value.position).toBe(oldValue.position)
    expect(result.value.margin).toBe(oldValue.margin)
    expect(result.value.padding).toBe(oldValue.padding)
    expect(result.value.naturalWidth).toBe(oldValue.naturalWidth)
    expect(result.value.naturalHeight).toBe(oldValue.naturalHeight)
    expect(result.value.clientWidth).toBe(oldValue.clientWidth)
    expect(result.value.clientHeight).toBe(oldValue.clientHeight)
    expect(result.value.parentFlexDirection).toBe(oldValue.parentFlexDirection)
    expect(result.value.flexDirection).toBe(oldValue.flexDirection)
    expect(result.value.htmlElementName).toBe(oldValue.htmlElementName)
    expect(result.value.renderedChildrenCount).toBe(oldValue.renderedChildrenCount)
    expect(result.value).toEqual(newDifferentValue)
    expect(result.areEqual).toEqual(false)
  })
})

describe('ElementInstanceMetadataKeepDeepEquality', () => {
  const oldValue: ElementInstanceMetadata = {
    elementPath: EP.elementPath([['scene'], ['aaa', 'bbb']]),
    element: left('div'),
    globalFrame: canvasRectangle({
      x: 10,
      y: 20,
      width: 100,
      height: 200,
    }),
    nonRoundedGlobalFrame: canvasRectangle({
      x: 10,
      y: 20,
      width: 100,
      height: 200,
    }),
    componentInstance: true,
    isEmotionOrStyledComponent: false,
    specialSizeMeasurements: {
      offset: {
        x: 10,
        y: 20,
      } as LocalPoint,
      coordinateSystemBounds: canvasRectangle({
        x: 10,
        y: 20,
        width: 100,
        height: 200,
      }),
      immediateParentBounds: canvasRectangle({
        x: 100,
        y: 200,
        width: 1000,
        height: 2000,
      }),
      globalFrameWithTextContent: canvasRectangle({
        x: 10,
        y: 20,
        width: 100,
        height: 200,
      }),
      immediateParentProvidesLayout: false,
      closestOffsetParentPath: EP.fromString('some/dummy/path'),
      usesParentBounds: false,
      parentLayoutSystem: 'flex',
      layoutSystemForChildren: 'flex',
      providesBoundsForAbsoluteChildren: true,
      display: 'flex',
      position: 'absolute',
      margin: {
        top: 1,
        right: 2,
        bottom: 3,
        left: 4,
      },
      padding: {
        top: 10,
        right: 20,
        bottom: 30,
        left: 40,
      },
      naturalWidth: 100,
      naturalHeight: 200,
      clientWidth: 300,
      clientHeight: 400,
      parentFlexDirection: 'row',
      parentJustifyContent: null,
      parentFlexGap: 0,
      parentPadding: {
        top: undefined,
        right: 0,
        bottom: 0,
        left: 0,
      },
      parentHugsOnMainAxis: false,
      gap: 11,
      flexDirection: 'column',
      justifyContent: 'center',
      alignContent: null,
      alignItems: 'auto',
      htmlElementName: 'div',
      renderedChildrenCount: 10,
      globalContentBoxForChildren: canvasRectangle({
        x: 20,
        y: 40,
        width: 60,
        height: 80,
      }),
      float: 'none',
      hasPositionOffset: false,
      parentTextDirection: 'ltr',
      hasTransform: false,
      borderRadius: {
        top: 10,
        right: 20,
        bottom: 30,
        left: 40,
      },
      fontSize: '16',
      fontWeight: '400',
      fontStyle: 'normal',
      textDecorationLine: 'none',
      textBounds: null,
      computedHugProperty: {
        width: null,
        height: null,
      },
      containerGridProperties: {
        gridTemplateColumns: null,
        gridTemplateRows: null,
        gridAutoColumns: null,
        gridAutoRows: null,
        gridAutoFlow: null,
      },
      elementGridProperties: {
        gridColumnStart: null,
        gridColumnEnd: null,
        gridRowStart: null,
        gridRowEnd: null,
      },
      containerGridPropertiesFromProps: {
        gridTemplateColumns: null,
        gridTemplateRows: null,
        gridAutoColumns: null,
        gridAutoRows: null,
        gridAutoFlow: null,
      },
      elementGridPropertiesFromProps: {
        gridColumnStart: null,
        gridColumnEnd: null,
        gridRowStart: null,
        gridRowEnd: null,
      },
      rowGap: null,
      columnGap: null,
    },
    computedStyle: {
      a: 'a',
      b: 'b',
    },
    attributeMetadata: {
      a: {
        fromStyleSheet: false,
      },
    },
    label: 'label',
    importInfo: createImportedFrom('old', 'old', 'old'),
    conditionValue: 'not-a-conditional',
    textContent: null,
    earlyReturn: null,
    assignedToProp: null,
  }
  const newDifferentValue: ElementInstanceMetadata = {
    elementPath: EP.elementPath([['scene'], ['aaa', 'bbb']]),
    element: left('div'),
    globalFrame: canvasRectangle({
      x: 10,
      y: 20,
      width: 100,
      height: 200,
    }),
    nonRoundedGlobalFrame: canvasRectangle({
      x: 10,
      y: 20,
      width: 100,
      height: 200,
    }),
    componentInstance: true,
    isEmotionOrStyledComponent: false,
    specialSizeMeasurements: {
      offset: {
        x: 10,
        y: 20,
      } as LocalPoint,
      coordinateSystemBounds: canvasRectangle({
        x: 10,
        y: 20,
        width: 100,
        height: 200,
      }),
      immediateParentBounds: canvasRectangle({
        x: 100,
        y: 200,
        width: 1000,
        height: 2000,
      }),
      globalFrameWithTextContent: canvasRectangle({
        x: 10,
        y: 20,
        width: 100,
        height: 200,
      }),
      immediateParentProvidesLayout: false,
      closestOffsetParentPath: EP.fromString('some/dummy/path'),
      usesParentBounds: false,
      parentLayoutSystem: 'flex',
      layoutSystemForChildren: 'flex',
      providesBoundsForAbsoluteChildren: true,
      display: 'flex',
      position: 'absolute',
      margin: {
        top: 1,
        right: 2,
        bottom: 3,
        left: 4,
      },
      padding: {
        top: 10,
        right: 20,
        bottom: 30,
        left: 40,
      },
      naturalWidth: 100,
      naturalHeight: 200,
      clientWidth: 300,
      clientHeight: 400,
      parentFlexDirection: 'row',
      parentJustifyContent: null,
      parentFlexGap: 0,
      parentPadding: {
        top: undefined,
        right: 0,
        bottom: 0,
        left: 0,
      },
      parentHugsOnMainAxis: false,
      gap: 11,
      flexDirection: 'column',
      justifyContent: 'center',
      alignContent: null,
      alignItems: 'auto',
      htmlElementName: 'div',
      renderedChildrenCount: 10,
      globalContentBoxForChildren: canvasRectangle({
        x: 20,
        y: 40,
        width: 60,
        height: 80,
      }),
      float: 'none',
      hasPositionOffset: false,
      parentTextDirection: 'ltr',
      hasTransform: false,
      borderRadius: {
        top: 10,
        right: 20,
        bottom: 30,
        left: 40,
      },
      fontSize: '16',
      fontWeight: '400',
      fontStyle: 'normal',
      textDecorationLine: 'none',
      textBounds: null,
      computedHugProperty: {
        width: null,
        height: null,
      },
      containerGridProperties: {
        gridTemplateColumns: null,
        gridTemplateRows: null,
        gridAutoColumns: null,
        gridAutoRows: null,
        gridAutoFlow: null,
      },
      elementGridProperties: {
        gridColumnStart: null,
        gridColumnEnd: null,
        gridRowStart: null,
        gridRowEnd: null,
      },
      containerGridPropertiesFromProps: {
        gridTemplateColumns: null,
        gridTemplateRows: null,
        gridAutoColumns: null,
        gridAutoRows: null,
        gridAutoFlow: null,
      },
      elementGridPropertiesFromProps: {
        gridColumnStart: null,
        gridColumnEnd: null,
        gridRowStart: null,
        gridRowEnd: null,
      },
      rowGap: null,
      columnGap: null,
    },
    computedStyle: {
      a: 'a',
      b: 'b',
    },
    attributeMetadata: {
      a: {
        fromStyleSheet: false,
      },
    },
    label: 'new-label',
    importInfo: createImportedFrom('old', 'old', 'old'),
    conditionValue: 'not-a-conditional',
    textContent: null,
    earlyReturn: null,
    assignedToProp: null,
  }

  it('same reference returns the same reference', () => {
    const result = ElementInstanceMetadataKeepDeepEquality(oldValue, oldValue)
    expect(result.value).toBe(oldValue)
    expect(result.areEqual).toEqual(true)
  })
  it('different but similar value handled appropriately', () => {
    const result = ElementInstanceMetadataKeepDeepEquality(oldValue, newDifferentValue)
    expect(result.value.elementPath).toBe(oldValue.elementPath)
    expect(result.value.element).toBe(oldValue.element)
    expect(result.value.globalFrame).toBe(oldValue.globalFrame)
    expect(result.value.componentInstance).toBe(oldValue.componentInstance)
    expect(result.value.isEmotionOrStyledComponent).toBe(oldValue.isEmotionOrStyledComponent)
    expect(result.value.specialSizeMeasurements).toBe(oldValue.specialSizeMeasurements)
    expect(result.value.computedStyle).toBe(oldValue.computedStyle)
    expect(result.value.attributeMetadata).toBe(oldValue.attributeMetadata)
    expect(result.value.label).toBe(newDifferentValue.label)
    expect(result.value.importInfo).toBe(oldValue.importInfo)
    expect(result.value).toEqual(newDifferentValue)
    expect(result.areEqual).toEqual(false)
  })
})

describe('ElementInstanceMetadataMapKeepDeepEquality', () => {
  const oldValue: ElementInstanceMetadataMap = {
    elem: {
      elementPath: EP.elementPath([['scene'], ['aaa', 'bbb']]),
      element: left('div'),
      globalFrame: canvasRectangle({
        x: 10,
        y: 20,
        width: 100,
        height: 200,
      }),
      nonRoundedGlobalFrame: canvasRectangle({
        x: 10,
        y: 20,
        width: 100,
        height: 200,
      }),
      componentInstance: true,
      isEmotionOrStyledComponent: false,
      specialSizeMeasurements: {
        offset: {
          x: 10,
          y: 20,
        } as LocalPoint,
        coordinateSystemBounds: canvasRectangle({
          x: 10,
          y: 20,
          width: 100,
          height: 200,
        }),
        immediateParentBounds: canvasRectangle({
          x: 100,
          y: 200,
          width: 1000,
          height: 2000,
        }),
        globalFrameWithTextContent: canvasRectangle({
          x: 10,
          y: 20,
          width: 100,
          height: 200,
        }),
        immediateParentProvidesLayout: false,
        closestOffsetParentPath: EP.fromString('some/dummy/path'),
        usesParentBounds: false,
        parentLayoutSystem: 'flex',
        layoutSystemForChildren: 'flex',
        providesBoundsForAbsoluteChildren: true,
        display: 'flex',
        position: 'absolute',
        margin: {
          top: 1,
          right: 2,
          bottom: 3,
          left: 4,
        },
        padding: {
          top: 10,
          right: 20,
          bottom: 30,
          left: 40,
        },
        naturalWidth: 100,
        naturalHeight: 200,
        clientWidth: 300,
        clientHeight: 400,
        parentFlexDirection: 'row',
        parentJustifyContent: null,
        parentFlexGap: 0,
        parentPadding: {
          top: undefined,
          right: 0,
          bottom: 0,
          left: 0,
        },
        parentHugsOnMainAxis: false,
        gap: 11,
        flexDirection: 'column',
        justifyContent: 'center',
        alignContent: null,
        alignItems: 'auto',
        htmlElementName: 'div',
        renderedChildrenCount: 10,
        globalContentBoxForChildren: canvasRectangle({
          x: 20,
          y: 40,
          width: 60,
          height: 80,
        }),
        float: 'none',
        hasPositionOffset: false,
        parentTextDirection: 'ltr',
        hasTransform: false,
        borderRadius: {
          top: 10,
          right: 20,
          bottom: 30,
          left: 40,
        },
        fontSize: '16',
        fontWeight: '400',
        fontStyle: 'normal',
        textDecorationLine: 'none',
        textBounds: null,
        computedHugProperty: {
          width: null,
          height: null,
        },
        containerGridProperties: {
          gridTemplateColumns: null,
          gridTemplateRows: null,
          gridAutoColumns: null,
          gridAutoRows: null,
          gridAutoFlow: null,
        },
        elementGridProperties: {
          gridColumnStart: null,
          gridColumnEnd: null,
          gridRowStart: null,
          gridRowEnd: null,
        },
        containerGridPropertiesFromProps: {
          gridTemplateColumns: null,
          gridTemplateRows: null,
          gridAutoColumns: null,
          gridAutoRows: null,
          gridAutoFlow: null,
        },
        elementGridPropertiesFromProps: {
          gridColumnStart: null,
          gridColumnEnd: null,
          gridRowStart: null,
          gridRowEnd: null,
        },
        rowGap: null,
        columnGap: null,
      },
      computedStyle: {
        a: 'a',
        b: 'b',
      },
      attributeMetadata: {
        a: {
          fromStyleSheet: false,
        },
      },
      label: 'label',
      importInfo: createImportedFrom('old', 'old', 'old'),
      conditionValue: 'not-a-conditional',
      textContent: null,
      earlyReturn: null,
      assignedToProp: null,
    },
  }
  const newSameValue: ElementInstanceMetadataMap = {
    elem: {
      elementPath: EP.elementPath([['scene'], ['aaa', 'bbb']]),
      element: left('div'),
      globalFrame: canvasRectangle({
        x: 10,
        y: 20,
        width: 100,
        height: 200,
      }),
      nonRoundedGlobalFrame: canvasRectangle({
        x: 10,
        y: 20,
        width: 100,
        height: 200,
      }),
      componentInstance: true,
      isEmotionOrStyledComponent: false,
      specialSizeMeasurements: {
        offset: {
          x: 10,
          y: 20,
        } as LocalPoint,
        coordinateSystemBounds: canvasRectangle({
          x: 10,
          y: 20,
          width: 100,
          height: 200,
        }),
        immediateParentBounds: canvasRectangle({
          x: 100,
          y: 200,
          width: 1000,
          height: 2000,
        }),
        globalFrameWithTextContent: canvasRectangle({
          x: 10,
          y: 20,
          width: 100,
          height: 200,
        }),
        immediateParentProvidesLayout: false,
        closestOffsetParentPath: EP.fromString('some/dummy/path'),
        usesParentBounds: false,
        parentLayoutSystem: 'flex',
        parentFlexGap: 0,
        layoutSystemForChildren: 'flex',
        providesBoundsForAbsoluteChildren: true,
        display: 'flex',
        position: 'absolute',
        margin: {
          top: 1,
          right: 2,
          bottom: 3,
          left: 4,
        },
        padding: {
          top: 10,
          right: 20,
          bottom: 30,
          left: 40,
        },
        naturalWidth: 100,
        naturalHeight: 200,
        clientWidth: 300,
        clientHeight: 400,
        parentFlexDirection: 'row',
        parentJustifyContent: null,
        parentPadding: {
          top: undefined,
          right: 0,
          bottom: 0,
          left: 0,
        },
        parentHugsOnMainAxis: false,
        gap: 11,
        flexDirection: 'column',
        justifyContent: 'center',
        alignContent: null,
        alignItems: 'auto',
        htmlElementName: 'div',
        renderedChildrenCount: 10,
        globalContentBoxForChildren: canvasRectangle({
          x: 20,
          y: 40,
          width: 60,
          height: 80,
        }),
        float: 'none',
        hasPositionOffset: false,
        parentTextDirection: 'ltr',
        hasTransform: false,
        borderRadius: {
          top: 10,
          right: 20,
          bottom: 30,
          left: 40,
        },
        fontSize: '16',
        fontWeight: '400',
        fontStyle: 'normal',
        textDecorationLine: 'none',
        textBounds: null,
        computedHugProperty: {
          width: null,
          height: null,
        },
        containerGridProperties: {
          gridTemplateColumns: null,
          gridTemplateRows: null,
          gridAutoColumns: null,
          gridAutoRows: null,
          gridAutoFlow: null,
        },
        elementGridProperties: {
          gridColumnStart: null,
          gridColumnEnd: null,
          gridRowStart: null,
          gridRowEnd: null,
        },
        containerGridPropertiesFromProps: {
          gridTemplateColumns: null,
          gridTemplateRows: null,
          gridAutoColumns: null,
          gridAutoRows: null,
          gridAutoFlow: null,
        },
        elementGridPropertiesFromProps: {
          gridColumnStart: null,
          gridColumnEnd: null,
          gridRowStart: null,
          gridRowEnd: null,
        },
        rowGap: null,
        columnGap: null,
      },
      computedStyle: {
        a: 'a',
        b: 'b',
      },
      attributeMetadata: {
        a: {
          fromStyleSheet: false,
        },
      },
      label: 'label',
      importInfo: createImportedFrom('old', 'old', 'old'),
      conditionValue: 'not-a-conditional',
      textContent: null,
      earlyReturn: null,
      assignedToProp: null,
    },
  }
  const newDifferentValue: ElementInstanceMetadataMap = {
    elem: {
      elementPath: EP.elementPath([['scene'], ['aaa', 'bbb']]),
      element: left('div'),
      globalFrame: canvasRectangle({
        x: 10,
        y: 20,
        width: 100,
        height: 200,
      }),
      nonRoundedGlobalFrame: canvasRectangle({
        x: 10,
        y: 20,
        width: 100,
        height: 200,
      }),
      componentInstance: true,
      isEmotionOrStyledComponent: false,
      specialSizeMeasurements: {
        offset: {
          x: 10,
          y: 20,
        } as LocalPoint,
        coordinateSystemBounds: canvasRectangle({
          x: 10,
          y: 20,
          width: 100,
          height: 200,
        }),
        immediateParentBounds: canvasRectangle({
          x: 100,
          y: 200,
          width: 1000,
          height: 2000,
        }),
        globalFrameWithTextContent: canvasRectangle({
          x: 10,
          y: 20,
          width: 100,
          height: 200,
        }),
        immediateParentProvidesLayout: false,
        closestOffsetParentPath: EP.fromString('some/dummy/path'),
        usesParentBounds: false,
        parentLayoutSystem: 'flex',
        layoutSystemForChildren: 'flex',
        providesBoundsForAbsoluteChildren: true,
        display: 'flex',
        position: 'absolute',
        margin: {
          top: 1,
          right: 2,
          bottom: 3,
          left: 4,
        },
        padding: {
          top: 10,
          right: 20,
          bottom: 30,
          left: 40,
        },
        naturalWidth: 100,
        naturalHeight: 200,
        clientWidth: 300,
        clientHeight: 400,
        parentFlexDirection: 'row',
        parentJustifyContent: null,
        parentFlexGap: 0,
        parentPadding: {
          top: undefined,
          right: 0,
          bottom: 0,
          left: 0,
        },
        parentHugsOnMainAxis: false,
        gap: 11,
        flexDirection: 'column',
        justifyContent: 'center',
        alignContent: null,
        alignItems: 'auto',
        htmlElementName: 'div',
        renderedChildrenCount: 10,
        globalContentBoxForChildren: canvasRectangle({
          x: 20,
          y: 40,
          width: 60,
          height: 80,
        }),
        float: 'none',
        hasPositionOffset: false,
        parentTextDirection: 'ltr',
        hasTransform: false,
        borderRadius: {
          top: 10,
          right: 20,
          bottom: 30,
          left: 40,
        },
        fontSize: '16',
        fontWeight: '400',
        fontStyle: 'normal',
        textDecorationLine: 'none',
        textBounds: null,
        computedHugProperty: {
          width: null,
          height: null,
        },
        containerGridProperties: {
          gridTemplateColumns: null,
          gridTemplateRows: null,
          gridAutoColumns: null,
          gridAutoRows: null,
          gridAutoFlow: null,
        },
        elementGridProperties: {
          gridColumnStart: null,
          gridColumnEnd: null,
          gridRowStart: null,
          gridRowEnd: null,
        },
        containerGridPropertiesFromProps: {
          gridTemplateColumns: null,
          gridTemplateRows: null,
          gridAutoColumns: null,
          gridAutoRows: null,
          gridAutoFlow: null,
        },
        elementGridPropertiesFromProps: {
          gridColumnStart: null,
          gridColumnEnd: null,
          gridRowStart: null,
          gridRowEnd: null,
        },
        rowGap: null,
        columnGap: null,
      },
      computedStyle: {
        a: 'a',
        b: 'b',
      },
      attributeMetadata: {
        a: {
          fromStyleSheet: false,
        },
      },
      label: 'new-label',
      importInfo: createImportedFrom('old', 'old', 'old'),
      conditionValue: 'not-a-conditional',
      textContent: 'hello',
      earlyReturn: null,
      assignedToProp: null,
    },
  }

  it('same reference returns the same reference', () => {
    const result = ElementInstanceMetadataMapKeepDeepEquality(oldValue, oldValue)
    expect(result.value).toBe(oldValue)
    expect(result.areEqual).toEqual(true)
  })
  it('same value returns the same reference', () => {
    const result = ElementInstanceMetadataMapKeepDeepEquality(oldValue, newSameValue)
    expect(result.value).toStrictEqual(oldValue)
    expect(result.areEqual).toEqual(true)
  })
  it('different but similar value handled appropriately', () => {
    const result = ElementInstanceMetadataMapKeepDeepEquality(oldValue, newDifferentValue)
    expect(result.value.elem.elementPath).toBe(oldValue.elem.elementPath)
    expect(result.value.elem.element).toBe(oldValue.elem.element)
    expect(result.value.elem.globalFrame).toBe(oldValue.elem.globalFrame)
    expect(result.value.elem.componentInstance).toBe(oldValue.elem.componentInstance)
    expect(result.value.elem.isEmotionOrStyledComponent).toBe(
      oldValue.elem.isEmotionOrStyledComponent,
    )
    expect(result.value.elem.specialSizeMeasurements).toBe(oldValue.elem.specialSizeMeasurements)
    expect(result.value.elem.computedStyle).toBe(oldValue.elem.computedStyle)
    expect(result.value.elem.attributeMetadata).toBe(oldValue.elem.attributeMetadata)
    expect(result.value.elem.label).toBe(newDifferentValue.elem.label)
    expect(result.value.elem.importInfo).toBe(oldValue.elem.importInfo)
    expect(result.value).toEqual(newDifferentValue)
    expect(result.areEqual).toEqual(false)
  })
})

describe('DropTargetHintKeepDeepEquality', () => {
  const oldValue: DropTargetHint = {
    displayAtEntry: regularNavigatorEntry(EP.elementPath([['scene'], ['aaa', 'bbb']])),
    targetParent: regularNavigatorEntry(EP.elementPath([['scene'], ['aaa', 'bbb']])),
    type: 'before',
    targetIndexPosition: { type: 'front' },
  }
  const newSameValue: DropTargetHint = {
    displayAtEntry: regularNavigatorEntry(EP.elementPath([['scene'], ['aaa', 'bbb']])),
    targetParent: regularNavigatorEntry(EP.elementPath([['scene'], ['aaa', 'bbb']])),
    type: 'before',
    targetIndexPosition: { type: 'front' },
  }
  const newDifferentValue: DropTargetHint = {
    displayAtEntry: regularNavigatorEntry(EP.elementPath([['scene'], ['aaa', 'bbb']])),
    targetParent: regularNavigatorEntry(EP.elementPath([['scene'], ['aaa', 'bbb']])),
    type: 'after',
    targetIndexPosition: { type: 'front' },
  }

  it('same reference returns the same reference', () => {
    const result = DropTargetHintKeepDeepEquality(oldValue, oldValue)
    expect(result.value).toBe(oldValue)
    expect(result.areEqual).toEqual(true)
  })
  it('same value returns the same reference', () => {
    const result = DropTargetHintKeepDeepEquality(oldValue, newSameValue)
    expect(result.value).toBe(oldValue)
    expect(result.areEqual).toEqual(true)
  })
  it('different but similar value handled appropriately', () => {
    const result = DropTargetHintKeepDeepEquality(oldValue, newDifferentValue)
    expect(result.value.displayAtEntry).toBe(oldValue.displayAtEntry)
    expect(result.value.type).toBe(newDifferentValue.type)
    expect(result.value).toEqual(newDifferentValue)
    expect(result.areEqual).toEqual(false)
  })
})

describe('NavigatorStateKeepDeepEquality', () => {
  const oldValue: NavigatorState = {
    minimised: false,
    dropTargetHint: {
      displayAtEntry: regularNavigatorEntry(EP.elementPath([['scene'], ['aaa', 'bbb']])),
      targetParent: regularNavigatorEntry(EP.elementPath([['scene'], ['aaa', 'bbb']])),
      type: 'before',
      targetIndexPosition: { type: 'front' },
    },
    collapsedViews: [EP.elementPath([['scene'], ['aaa', 'bbb']])],
    renamingTarget: EP.elementPath([['scene'], ['aaa', 'bbb']]),
    highlightedTargets: [],
    hiddenInNavigator: [],
  }
  const newSameValue: NavigatorState = {
    minimised: false,
    dropTargetHint: {
      displayAtEntry: regularNavigatorEntry(EP.elementPath([['scene'], ['aaa', 'bbb']])),
      targetParent: regularNavigatorEntry(EP.elementPath([['scene'], ['aaa', 'bbb']])),
      type: 'before',
      targetIndexPosition: { type: 'front' },
    },
    collapsedViews: [EP.elementPath([['scene'], ['aaa', 'bbb']])],
    renamingTarget: EP.elementPath([['scene'], ['aaa', 'bbb']]),
    highlightedTargets: [],
    hiddenInNavigator: [],
  }
  const newDifferentValue: NavigatorState = {
    minimised: true,
    dropTargetHint: {
      displayAtEntry: regularNavigatorEntry(EP.elementPath([['scene'], ['aaa', 'bbb']])),
      targetParent: regularNavigatorEntry(EP.elementPath([['scene'], ['aaa', 'bbb']])),
      type: 'before',
      targetIndexPosition: { type: 'front' },
    },
    collapsedViews: [EP.elementPath([['scene'], ['aaa', 'bbb']])],
    renamingTarget: EP.elementPath([['scene'], ['aaa', 'bbb']]),
    highlightedTargets: [],
    hiddenInNavigator: [],
  }

  it('same reference returns the same reference', () => {
    const result = NavigatorStateKeepDeepEquality(oldValue, oldValue)
    expect(result.value).toBe(oldValue)
    expect(result.areEqual).toEqual(true)
  })
  it('same value returns the same reference', () => {
    const result = NavigatorStateKeepDeepEquality(oldValue, newSameValue)
    expect(result.value).toBe(oldValue)
    expect(result.areEqual).toEqual(true)
  })
  it('different but similar value handled appropriately', () => {
    const result = NavigatorStateKeepDeepEquality(oldValue, newDifferentValue)
    expect(result.value.minimised).toBe(newDifferentValue.minimised)
    expect(result.value.dropTargetHint).toBe(oldValue.dropTargetHint)
    expect(result.value.collapsedViews).toBe(oldValue.collapsedViews)
    expect(result.value.renamingTarget).toBe(oldValue.renamingTarget)
    expect(result.value).toEqual(newDifferentValue)
    expect(result.areEqual).toEqual(false)
  })
})
