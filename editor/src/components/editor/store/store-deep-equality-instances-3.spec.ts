import { Sides } from 'utopia-api/core'
import { left, Right } from '../../../core/shared/either'
import {
  createImportedFrom,
  createNotImported,
  ElementInstanceMetadata,
  ElementInstanceMetadataMap,
  FoundImportInfo,
  ImportInfo,
  SpecialSizeMeasurements,
} from '../../../core/shared/element-template'
import {
  canvasRectangle,
  CanvasRectangle,
  LocalPoint,
  localRectangle,
  LocalRectangle,
} from '../../../core/shared/math-utils'
import {
  CanvasRectangleKeepDeepEquality,
  ElementInstanceMetadataKeepDeepEquality,
  ElementInstanceMetadataMapKeepDeepEquality,
  ImportInfoKeepDeepEquality,
  LocalPointKeepDeepEquality,
  LocalRectangleKeepDeepEquality,
  SidesKeepDeepEquality,
  SpecialSizeMeasurementsKeepDeepEquality,
} from './store-deep-equality-instances'
import * as EP from '../../../core/shared/element-path'
import { ElementProps } from './editor-state'

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
  const oldNotImportedValue: ImportInfo = createNotImported()
  const newSameNotImportedValue: ImportInfo = createNotImported()

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
    expect((result.value as Right<FoundImportInfo>).value.variableName).toBe(
      (newDifferentValue as Right<FoundImportInfo>).value.variableName,
    )
    expect((result.value as Right<FoundImportInfo>).value.originalName).toBe(
      (oldValue as Right<FoundImportInfo>).value.originalName,
    )
    expect((result.value as Right<FoundImportInfo>).value.path).toBe(
      (oldValue as Right<FoundImportInfo>).value.path,
    )
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
    immediateParentProvidesLayout: false,
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
    flexDirection: 'column',
    htmlElementName: 'div',
    renderedChildrenCount: 10,
    globalContentBox: canvasRectangle({
      x: 20,
      y: 40,
      width: 60,
      height: 80,
    }),
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
    immediateParentProvidesLayout: true,
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
    flexDirection: 'column',
    htmlElementName: 'div',
    renderedChildrenCount: 10,
    globalContentBox: canvasRectangle({
      x: 20,
      y: 40,
      width: 60,
      height: 0,
    }),
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
    localFrame: localRectangle({
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
      immediateParentProvidesLayout: false,
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
      flexDirection: 'column',
      htmlElementName: 'div',
      renderedChildrenCount: 10,
      globalContentBox: canvasRectangle({
        x: 20,
        y: 40,
        width: 60,
        height: 80,
      }),
    },
    computedStyle: {
      a: 'a',
      b: 'b',
    },
    attributeMetadatada: {
      a: {
        fromStyleSheet: false,
      },
    },
    label: 'label',
    importInfo: createImportedFrom('old', 'old', 'old'),
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
    localFrame: localRectangle({
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
      immediateParentProvidesLayout: false,
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
      flexDirection: 'column',
      htmlElementName: 'div',
      renderedChildrenCount: 10,
      globalContentBox: canvasRectangle({
        x: 20,
        y: 40,
        width: 60,
        height: 80,
      }),
    },
    computedStyle: {
      a: 'a',
      b: 'b',
    },
    attributeMetadatada: {
      a: {
        fromStyleSheet: false,
      },
    },
    label: 'new-label',
    importInfo: createImportedFrom('old', 'old', 'old'),
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
    expect(result.value.localFrame).toBe(oldValue.localFrame)
    expect(result.value.componentInstance).toBe(oldValue.componentInstance)
    expect(result.value.isEmotionOrStyledComponent).toBe(oldValue.isEmotionOrStyledComponent)
    expect(result.value.specialSizeMeasurements).toBe(oldValue.specialSizeMeasurements)
    expect(result.value.computedStyle).toBe(oldValue.computedStyle)
    expect(result.value.attributeMetadatada).toBe(oldValue.attributeMetadatada)
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
      localFrame: localRectangle({
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
        immediateParentProvidesLayout: false,
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
        flexDirection: 'column',
        htmlElementName: 'div',
        renderedChildrenCount: 10,
        globalContentBox: canvasRectangle({
          x: 20,
          y: 40,
          width: 60,
          height: 80,
        }),
      },
      computedStyle: {
        a: 'a',
        b: 'b',
      },
      attributeMetadatada: {
        a: {
          fromStyleSheet: false,
        },
      },
      label: 'label',
      importInfo: createImportedFrom('old', 'old', 'old'),
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
      localFrame: localRectangle({
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
        immediateParentProvidesLayout: false,
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
        flexDirection: 'column',
        htmlElementName: 'div',
        renderedChildrenCount: 10,
        globalContentBox: canvasRectangle({
          x: 20,
          y: 40,
          width: 60,
          height: 80,
        }),
      },
      computedStyle: {
        a: 'a',
        b: 'b',
      },
      attributeMetadatada: {
        a: {
          fromStyleSheet: false,
        },
      },
      label: 'label',
      importInfo: createImportedFrom('old', 'old', 'old'),
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
      localFrame: localRectangle({
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
        immediateParentProvidesLayout: false,
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
        flexDirection: 'column',
        htmlElementName: 'div',
        renderedChildrenCount: 10,
        globalContentBox: canvasRectangle({
          x: 20,
          y: 40,
          width: 60,
          height: 80,
        }),
      },
      computedStyle: {
        a: 'a',
        b: 'b',
      },
      attributeMetadatada: {
        a: {
          fromStyleSheet: false,
        },
      },
      label: 'new-label',
      importInfo: createImportedFrom('old', 'old', 'old'),
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
    expect(result.value.elem.localFrame).toBe(oldValue.elem.localFrame)
    expect(result.value.elem.componentInstance).toBe(oldValue.elem.componentInstance)
    expect(result.value.elem.isEmotionOrStyledComponent).toBe(
      oldValue.elem.isEmotionOrStyledComponent,
    )
    expect(result.value.elem.specialSizeMeasurements).toBe(oldValue.elem.specialSizeMeasurements)
    expect(result.value.elem.computedStyle).toBe(oldValue.elem.computedStyle)
    expect(result.value.elem.attributeMetadatada).toBe(oldValue.elem.attributeMetadatada)
    expect(result.value.elem.label).toBe(newDifferentValue.elem.label)
    expect(result.value.elem.importInfo).toBe(oldValue.elem.importInfo)
    expect(result.value).toEqual(newDifferentValue)
    expect(result.areEqual).toEqual(false)
  })
})
