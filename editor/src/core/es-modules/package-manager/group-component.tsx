// TODO move me to utopia-api once stable!

import * as React from 'react'
import type { CSSProperties } from 'react'

const CSS_OM_SUPPORTED: boolean = document.querySelector('body')?.computedStyleMap != null

interface Pins {
  left: number
  top: number
  right: number
  bottom: number
  width: number
  height: number
}

interface Size {
  width: number
  height: number
}

function isAutoOrPercentage(typedOmValue: CSSStyleValue | undefined): boolean {
  if (typedOmValue == null) {
    return true
  }

  if (typedOmValue instanceof CSSUnitValue && typedOmValue.unit !== 'percent') {
    // for now the only happy case if the value is a CSSUnitValue and the unit is not percent
    return false
  }

  // TODO recurse into compound values and check for percents
  return true
}

function getPinPixelOrZeroIfAutoOrPercentage(
  computedValue: string,
  typedOmValue: CSSStyleValue | undefined,
): number {
  if (isAutoOrPercentage(typedOmValue)) {
    return 0
  }
  if (!computedValue.endsWith('px')) {
    return 0
  }
  return parseFloat(computedValue)
}

function getAllPinsSanitized(child: HTMLElement): Pins {
  const computedStyle = window.getComputedStyle(child)
  const computedStyleMapTyped = child.computedStyleMap()

  if (computedStyle.position !== 'absolute') {
    return { left: 0, top: 0, right: 0, bottom: 0, width: 0, height: 0 }
  }

  return {
    left: getPinPixelOrZeroIfAutoOrPercentage(
      computedStyle.left,
      computedStyleMapTyped.get('left'),
    ),
    top: getPinPixelOrZeroIfAutoOrPercentage(computedStyle.top, computedStyleMapTyped.get('top')),
    right: getPinPixelOrZeroIfAutoOrPercentage(
      computedStyle.right,
      computedStyleMapTyped.get('right'),
    ),
    bottom: getPinPixelOrZeroIfAutoOrPercentage(
      computedStyle.bottom,
      computedStyleMapTyped.get('bottom'),
    ),
    width: getPinPixelOrZeroIfAutoOrPercentage(
      computedStyle.width,
      computedStyleMapTyped.get('width'),
    ),
    height: getPinPixelOrZeroIfAutoOrPercentage(
      computedStyle.height,
      computedStyleMapTyped.get('height'),
    ),
  }
}

function getGroupSize(children: NodeListOf<ChildNode>): Size {
  let minimumAcceptableWidth = 0
  let minimumAcceptableHeight = 0

  children.forEach((child) => {
    if (!(child instanceof HTMLElement)) {
      return
    }

    const sanitizedPins = getAllPinsSanitized(child)

    const minimumGroupWidthForChild = sanitizedPins.left + sanitizedPins.width + sanitizedPins.right
    const minimumGroupHeightForChild =
      sanitizedPins.top + sanitizedPins.height + sanitizedPins.bottom

    minimumAcceptableWidth = Math.max(minimumAcceptableWidth, minimumGroupWidthForChild)
    minimumAcceptableHeight = Math.max(minimumAcceptableHeight, minimumGroupHeightForChild)
  })

  return {
    width: minimumAcceptableWidth,
    height: minimumAcceptableHeight,
  }
}

export const UtopiaApiGroup: React.FunctionComponent<
  React.PropsWithChildren<{ style?: CSSProperties }>
> = (props) => {
  const groupRef = React.useRef<HTMLDivElement>(null)
  const latestPropsRef = React.useRef(props)
  latestPropsRef.current = props

  function changeSizeToMatchChildren() {
    if (groupRef.current == null) {
      return
    }
    if (!CSS_OM_SUPPORTED) {
      return
    }
    const group = groupRef.current

    const children = group.childNodes
    const { width, height } = getGroupSize(children)

    if (latestPropsRef.current.style?.width == null) {
      group.style.width = width + 'px'
    }
    if (latestPropsRef.current.style?.height == null) {
      group.style.height = height + 'px'
    }
  }

  React.useLayoutEffect(() => {
    changeSizeToMatchChildren()
  }, [])

  React.useEffect(() => {
    const observer = new MutationObserver((e) => {
      changeSizeToMatchChildren()
    })
    observer.observe(document, {
      childList: true,
      subtree: true,
      attributes: true,
    })

    return function cleanup() {
      observer.disconnect()
    }
  }, [])

  return (
    <div
      {...props}
      ref={groupRef}
      style={{
        ...props.style,
        flex: '0 0 auto',
        contain: 'layout',
      }}
    >
      {props.children}
    </div>
  )
}
