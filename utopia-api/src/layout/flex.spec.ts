import type { FlexElementProps, FlexParentProps } from './flex'
import {
  FlexDirection,
  FlexAlignment,
  getFlexStretchForChild,
  getMarginProps,
  getTLBRProps,
  getFlexSize,
  flexWidthValueToUse,
  flexHeightValueToUse,
} from './flex'

describe('flexWidthValueToUse', () => {
  const crossBasis = 7

  it('returns the cross basis if the cross axis is horizontal, and it does not stretch', () => {
    expect(flexWidthValueToUse(crossBasis, false, 'horizontal')).toBe(crossBasis)
  })

  it('returns undefined if the cross axis is horizontal, and it stretches', () => {
    expect(flexWidthValueToUse(crossBasis, true, 'horizontal')).toBeUndefined()
  })
})

describe('flexHeightValueToUse', () => {
  const crossBasis = 7

  it('returns the cross basis if the cross axis is vertical, and it does not stretch', () => {
    expect(flexHeightValueToUse(crossBasis, false, 'vertical')).toBe(crossBasis)
  })

  it('returns undefined if the cross axis is vertical, and it stretches', () => {
    expect(flexHeightValueToUse(crossBasis, true, 'vertical')).toBeUndefined()
  })
})

describe('getFlexSize', () => {
  const flexBasis = 5
  const crossBasis = 7

  it('returns the flex basis and cross basis for height if the axis is horizontal', () => {
    const elem: FlexElementProps = {
      flexBasis: flexBasis,
      crossBasis: crossBasis,
    }
    const parent: FlexParentProps = { flexDirection: FlexDirection.Row }
    expect(getFlexSize(elem, parent)).toEqual({ flexBasis: flexBasis, height: crossBasis })
  })

  it('returns the flex basis and cross basis for width if the axis is vertical', () => {
    const elem: FlexElementProps = {
      flexBasis: flexBasis,
      crossBasis: crossBasis,
    }
    const parent: FlexParentProps = { flexDirection: FlexDirection.Column }
    expect(getFlexSize(elem, parent)).toEqual({ width: crossBasis, flexBasis: flexBasis })
  })

  it('drops cross basis if the axis is horizontal and it stretches', () => {
    const elem: FlexElementProps = {
      flexBasis: flexBasis,
      crossBasis: crossBasis,
    }
    const parent: FlexParentProps = {
      flexDirection: FlexDirection.Row,
      alignItems: FlexAlignment.Stretch,
    }
    expect(getFlexSize(elem, parent)).toEqual({ flexBasis: flexBasis })
  })

  it('drops cross basis if the axis is vertical and it stretches', () => {
    const elem: FlexElementProps = {
      flexBasis: flexBasis,
      crossBasis: crossBasis,
    }
    const parent: FlexParentProps = {
      flexDirection: FlexDirection.Column,
      alignItems: FlexAlignment.Stretch,
    }
    expect(getFlexSize(elem, parent)).toEqual({ flexBasis: flexBasis })
  })
})

describe('getTLBRProps', () => {
  const tlbr: FlexElementProps = { left: 10, right: 10, top: 10, bottom: 10 }

  it('returns undefined for all of the props if position is not defined', () => {
    expect(getTLBRProps(tlbr)).toEqual({
      left: undefined,
      right: undefined,
      top: undefined,
      bottom: undefined,
    })
  })

  it('returns the values if the position is defined', () => {
    expect(getTLBRProps({ ...tlbr, position: 'absolute' })).toEqual(tlbr)
    expect(getTLBRProps({ ...tlbr, position: 'relative' })).toEqual(tlbr)
  })
})

describe('getMarginProps', () => {
  const crossGap = 2
  const elementGap = 5
  const margin = 10
  const gapProps: FlexParentProps = { gapMain: elementGap * 2, gapCross: crossGap }
  const parentWithColumnAlignedGap: FlexParentProps = {
    ...gapProps,
    flexDirection: FlexDirection.Column,
  }
  const parentWithColumnReverseAlignedGap: FlexParentProps = {
    ...gapProps,
    flexDirection: FlexDirection.ColumnReverse,
  }
  const parentWithRowAlignedGap: FlexParentProps = { ...gapProps, flexDirection: FlexDirection.Row }
  const parentWithRowReverseAlignedGap: FlexParentProps = {
    ...gapProps,
    flexDirection: FlexDirection.RowReverse,
  }
  const parentWithDefaultAlignedGap: FlexParentProps = { ...gapProps }
  const noMarginProps: FlexElementProps = {}

  const marginLeft: FlexElementProps = { marginLeft: margin }
  const marginRight: FlexElementProps = { marginRight: margin }
  const marginTop: FlexElementProps = { marginTop: margin }
  const marginBottom: FlexElementProps = { marginBottom: margin }
  const marginAll: FlexElementProps = {
    marginLeft: margin,
    marginRight: margin,
    marginTop: margin,
    marginBottom: margin,
  }

  it('returns the margin for every side if one is defined', () => {
    expect(getMarginProps(marginAll, {}, 1, 2)).toEqual({
      marginLeft: margin,
      marginRight: margin,
      marginTop: margin,
      marginBottom: margin,
    })
  })

  it('returns the margin for each side it is defined on, and the gap for the remaining sides', () => {
    const expectedGaps = {
      marginLeft: crossGap,
      marginRight: crossGap,
      marginTop: elementGap,
      marginBottom: elementGap,
    }

    expect(getMarginProps(marginLeft, parentWithColumnAlignedGap, 1, 3)).toEqual({
      ...expectedGaps,
      marginLeft: margin,
    })
    expect(getMarginProps(marginRight, parentWithColumnAlignedGap, 1, 3)).toEqual({
      ...expectedGaps,
      marginRight: margin,
    })
    expect(getMarginProps(marginTop, parentWithColumnAlignedGap, 1, 3)).toEqual({
      ...expectedGaps,
      marginTop: margin,
    })
    expect(getMarginProps(marginBottom, parentWithColumnAlignedGap, 1, 3)).toEqual({
      ...expectedGaps,
      marginBottom: margin,
    })
  })

  it('for no margin returns undefined for no gaps', () => {
    expect(getMarginProps(noMarginProps, {}, 1, 3)).toEqual({
      marginLeft: undefined,
      marginRight: undefined,
      marginTop: undefined,
      marginBottom: undefined,
    })
  })

  it('for no margin uses the main gap for vertical props and cross gap for horizontal props in column aligned', () => {
    const expectedFront = {
      marginLeft: crossGap,
      marginRight: crossGap,
      marginTop: 0,
      marginBottom: elementGap,
    }

    const expectedMiddle = {
      marginLeft: crossGap,
      marginRight: crossGap,
      marginTop: elementGap,
      marginBottom: elementGap,
    }

    const expectedBack = {
      marginLeft: crossGap,
      marginRight: crossGap,
      marginTop: elementGap,
      marginBottom: 0,
    }

    expect(getMarginProps(noMarginProps, parentWithColumnAlignedGap, 0, 3)).toEqual(expectedFront)
    expect(getMarginProps(noMarginProps, parentWithColumnReverseAlignedGap, 0, 3)).toEqual(
      expectedBack,
    )

    expect(getMarginProps(noMarginProps, parentWithColumnAlignedGap, 1, 3)).toEqual(expectedMiddle)
    expect(getMarginProps(noMarginProps, parentWithColumnReverseAlignedGap, 1, 3)).toEqual(
      expectedMiddle,
    )

    expect(getMarginProps(noMarginProps, parentWithColumnAlignedGap, 2, 3)).toEqual(expectedBack)
    expect(getMarginProps(noMarginProps, parentWithColumnReverseAlignedGap, 2, 3)).toEqual(
      expectedFront,
    )
  })

  it('for no margin uses the main gap for horizontal props and cross gap for vertical props in row aligned', () => {
    const expectedFront = {
      marginLeft: 0,
      marginRight: elementGap,
      marginTop: crossGap,
      marginBottom: crossGap,
    }

    const expectedMiddle = {
      marginLeft: elementGap,
      marginRight: elementGap,
      marginTop: crossGap,
      marginBottom: crossGap,
    }

    const expectedBack = {
      marginLeft: elementGap,
      marginRight: 0,
      marginTop: crossGap,
      marginBottom: crossGap,
    }

    expect(getMarginProps(noMarginProps, parentWithRowAlignedGap, 0, 3)).toEqual(expectedFront)
    expect(getMarginProps(noMarginProps, parentWithRowReverseAlignedGap, 0, 3)).toEqual(
      expectedBack,
    )
    expect(getMarginProps(noMarginProps, parentWithDefaultAlignedGap, 0, 3)).toEqual(expectedFront)

    expect(getMarginProps(noMarginProps, parentWithRowAlignedGap, 1, 3)).toEqual(expectedMiddle)
    expect(getMarginProps(noMarginProps, parentWithRowReverseAlignedGap, 1, 3)).toEqual(
      expectedMiddle,
    )
    expect(getMarginProps(noMarginProps, parentWithDefaultAlignedGap, 1, 3)).toEqual(expectedMiddle)

    expect(getMarginProps(noMarginProps, parentWithRowAlignedGap, 2, 3)).toEqual(expectedBack)
    expect(getMarginProps(noMarginProps, parentWithRowReverseAlignedGap, 2, 3)).toEqual(
      expectedFront,
    )
    expect(getMarginProps(noMarginProps, parentWithDefaultAlignedGap, 2, 3)).toEqual(expectedBack)
  })
})

describe('getFlexStretchForChild', () => {
  const columnParent: FlexParentProps = { flexDirection: FlexDirection.Column }
  const columnReverseParent: FlexParentProps = { flexDirection: FlexDirection.ColumnReverse }
  const rowParent: FlexParentProps = { flexDirection: FlexDirection.Row }
  const rowReverseParent: FlexParentProps = { flexDirection: FlexDirection.RowReverse }

  const stretchColumnParent: FlexParentProps = {
    flexDirection: FlexDirection.Column,
    alignItems: FlexAlignment.Stretch,
  }
  const stretchColumnReverseParent: FlexParentProps = {
    flexDirection: FlexDirection.ColumnReverse,
    alignItems: FlexAlignment.Stretch,
  }
  const stretchRowParent: FlexParentProps = {
    flexDirection: FlexDirection.Row,
    alignItems: FlexAlignment.Stretch,
  }
  const stretchRowReverseParent: FlexParentProps = {
    flexDirection: FlexDirection.RowReverse,
    alignItems: FlexAlignment.Stretch,
  }

  it('stretches if the child has alignSelf set to stretch', () => {
    const child: FlexElementProps = { alignSelf: FlexAlignment.Stretch }
    expect(getFlexStretchForChild(columnParent, child)).toBe('horizontal')
    expect(getFlexStretchForChild(columnReverseParent, child)).toBe('horizontal')
    expect(getFlexStretchForChild(rowParent, child)).toBe('vertical')
    expect(getFlexStretchForChild(rowReverseParent, child)).toBe('vertical')
  })

  it('stretches if the child has alignSelf set to auto or empty and the parent stretches', () => {
    const autoChild: FlexElementProps = { alignSelf: FlexAlignment.Auto }

    expect(getFlexStretchForChild(columnParent, autoChild)).toBe('none')
    expect(getFlexStretchForChild(columnReverseParent, autoChild)).toBe('none')
    expect(getFlexStretchForChild(rowParent, autoChild)).toBe('none')
    expect(getFlexStretchForChild(rowReverseParent, autoChild)).toBe('none')

    expect(getFlexStretchForChild(stretchColumnParent, autoChild)).toBe('horizontal')
    expect(getFlexStretchForChild(stretchColumnReverseParent, autoChild)).toBe('horizontal')
    expect(getFlexStretchForChild(stretchRowParent, autoChild)).toBe('vertical')
    expect(getFlexStretchForChild(stretchRowReverseParent, autoChild)).toBe('vertical')

    const emptyChild: FlexElementProps = {}

    expect(getFlexStretchForChild(columnParent, emptyChild)).toBe('none')
    expect(getFlexStretchForChild(columnReverseParent, emptyChild)).toBe('none')
    expect(getFlexStretchForChild(rowParent, emptyChild)).toBe('none')
    expect(getFlexStretchForChild(rowReverseParent, emptyChild)).toBe('none')

    expect(getFlexStretchForChild(stretchColumnParent, emptyChild)).toBe('horizontal')
    expect(getFlexStretchForChild(stretchColumnReverseParent, emptyChild)).toBe('horizontal')
    expect(getFlexStretchForChild(stretchRowParent, emptyChild)).toBe('vertical')
    expect(getFlexStretchForChild(stretchRowReverseParent, emptyChild)).toBe('vertical')
  })
})
