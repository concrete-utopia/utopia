import type { PinFrameProps } from './frame'

export interface AxisCSSProps {
  start?: number | string
  end?: number | string
  size?: number | string
}

function addUnitIfMissing(input: number | string | undefined): string | undefined {
  if (input == null) {
    return undefined
  }
  const inputAsNumber = Number(input)
  if (isNaN(inputAsNumber)) {
    // input already contains non-number characters, likely intentionally, probably a unit
    return `${input}`
  } else {
    // input parsed correctly as number means it had no unit, which means it is a pixel value
    return `${input}px`
  }
}

export function axisPinsToAxisCSSProps(
  start: number | string | undefined,
  center: number | string | undefined,
  end: number | string | undefined,
  size: number | string | undefined,
): AxisCSSProps {
  function defaultResult(): AxisCSSProps {
    let result: AxisCSSProps = {}
    if (start != null) {
      result.start = start
    }
    if (end != null) {
      result.end = end
    }
    if (size != null) {
      result.size = size
    }
    return result
  }
  const nonCenterProps = (start == null ? 0 : 1) + (end == null ? 0 : 1) + (size == null ? 0 : 1)
  if (nonCenterProps >= 2 || center == null) {
    // If we have 2 or more of these we can just use those coordinates.
    // Also default back to this if we have no center value.
    return defaultResult()
  } else {
    const sizeWithUnit = addUnitIfMissing(size)
    const centerWithUnit = addUnitIfMissing(center)
    const startWithUnit = addUnitIfMissing(start)
    const endWithUnit = addUnitIfMissing(end)
    if (sizeWithUnit != null) {
      return {
        // L = (50% + Cx) - W/2
        start: `calc((50% + ${centerWithUnit}) - (${sizeWithUnit} / 2))`,
        size: size,
      }
    } else if (startWithUnit != null) {
      return {
        start: start,
        size: `calc(calc(calc(50% + ${centerWithUnit}) - ${startWithUnit}) * 2)`,
      }
    } else if (endWithUnit != null) {
      return {
        // R + W / 2 = 50% - Cx
        // W = (50% - Cx - R) * 2
        // L = R - W
        start: `calc(${endWithUnit} - calc(50% - ${centerWithUnit} - ${endWithUnit}) * 2)`,
        end: end,
      }
    } else {
      return defaultResult()
    }
  }
}

export type CSSFrame = Partial<
  Pick<React.CSSProperties, 'position' | 'left' | 'top' | 'right' | 'bottom' | 'width' | 'height'>
>
export function convertPinsToStyleProps(
  props: PinFrameProps & Pick<React.CSSProperties, 'position'>, // todo make me cleaner code
): CSSFrame {
  const {
    start: left,
    end: right,
    size: width,
  } = axisPinsToAxisCSSProps(props.left, props.centerX, props.right, props.width)
  const {
    start: top,
    end: bottom,
    size: height,
  } = axisPinsToAxisCSSProps(props.top, props.centerY, props.bottom, props.height)
  let returnStyle: CSSFrame = {}
  if (left != null) {
    returnStyle.left = left
  }
  if (right != null) {
    returnStyle.right = right
  }
  if (width != null) {
    returnStyle.width = width
  }
  if (top != null) {
    returnStyle.top = top
  }
  if (bottom != null) {
    returnStyle.bottom = bottom
  }
  if (height != null) {
    returnStyle.height = height
  }
  if (props.position != null) {
    returnStyle.position = props.position
  }
  return returnStyle
}

export function convertPinsToAbsoluteStyleProps(props: PinFrameProps): CSSFrame {
  return {
    position: 'absolute',
    ...convertPinsToStyleProps(props),
  }
}
