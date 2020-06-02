import { defaultIfNull } from '../utils'
import * as React from 'react'
import { NormalisedFrame } from '../layout/frame'
import { LayoutSystem, LayoutProps, AllLayoutBasePropsKeys } from '../layout/layout'
import { CSSFrame, convertPinsToStyleProps, convertPinsToAbsoluteStyleProps } from '../layout/pins'
import {
  flexElementPropsToStyle,
  flexParentPropsToStyle,
  flexParentPropsToStyleWithoutDisplayFlex,
} from '../layout/flex'
import { LayoutableProps } from './layoutable'

export interface UtopiaComponentProps {
  'data-uid'?: string
  'data-label'?: string
  layout?: LayoutProps
  style?: React.CSSProperties
}

function computeNonGroupChildren(
  children: React.ReactNode,
  parentLayoutProps: LayoutProps,
): Array<React.CSSProperties> {
  const childCount = React.Children.count(children)
  const mappedChildren = React.Children.map(children, (child, index) => {
    if (React.isValidElement(child)) {
      const childLayout = { ...child.props.layout, ...child.props.style }
      const childFlexProps = flexElementPropsToStyle(
        childLayout,
        parentLayoutProps,
        index,
        childCount,
      )
      const childContainerProps = calculateContainerProps(
        parentLayoutProps.layoutSystem,
        child.props,
        child.props.children,
      )
      const childPinsProps =
        parentLayoutProps.layoutSystem === LayoutSystem.PinSystem
          ? convertPinsToAbsoluteStyleProps(childLayout)
          : convertPinsToStyleProps(childLayout)
      return {
        ...childFlexProps,
        ...childContainerProps,
        ...childPinsProps,
      }
    } else {
      return {}
    }
  })
  return mappedChildren ?? []
}

function computeGroupChildren(children: React.ReactNode): Array<NormalisedFrame | undefined> {
  let result: Array<NormalisedFrame | undefined> = []
  React.Children.forEach(children, (child) => {
    if (React.isValidElement(child)) {
      const style = { ...child.props.layout, ...child.props.style }
      const normalisedFrame = {
        left: style.left || 0,
        top: style.top || 0,
        width: style.width || 0,
        height: style.height || 0,
      }
      result.push(normalisedFrame)
    } else {
      result.push(undefined)
    }
  })
  return result
}

function boundingRectangle(first: NormalisedFrame, second: NormalisedFrame): NormalisedFrame {
  const firstTL: NormalisedFrame = first
  const firstBR = {
    left: first.left + first.width,
    top: first.top + first.height,
  }
  const secondTL: NormalisedFrame = second
  const secondBR = {
    left: second.left + second.width,
    top: second.top + second.height,
  }

  const newTL = {
    left: Math.min(firstTL.left, secondTL.left),
    top: Math.min(firstTL.top, secondTL.top),
  }
  const newBR = {
    left: Math.max(firstBR.left, secondBR.left),
    top: Math.max(firstBR.top, secondBR.top),
  }

  return {
    left: newTL.left,
    top: newTL.top,
    width: newBR.left - newTL.left,
    height: newBR.top - newTL.top,
  }
}

function boundingRectangleFromArray(
  rectangles: Array<NormalisedFrame | undefined>,
): NormalisedFrame {
  let frames: Array<NormalisedFrame> = []
  for (const rect of rectangles) {
    if (rect != null) {
      frames.push(rect)
    }
  }
  if (frames.length === 0) {
    return { left: 0, top: 0, width: 0, height: 0 }
  } else {
    const [firstRectangle, ...remainder] = frames
    return remainder.reduce(boundingRectangle, firstRectangle)
  }
}

function calculateGroupBounds(frames: Array<NormalisedFrame | undefined>): NormalisedFrame {
  return boundingRectangleFromArray(frames)
}

function shiftGroupChildren(
  parentFrame: NormalisedFrame,
  childFrames: Array<NormalisedFrame | undefined>,
): Array<NormalisedFrame | undefined> {
  return childFrames.map((childFrame) => {
    if (childFrame == null) {
      return childFrame
    } else {
      return {
        left: childFrame.left - parentFrame.left,
        top: childFrame.top - parentFrame.top,
        width: childFrame.width,
        height: childFrame.height,
      }
    }
  })
}

function getLayoutPropsFromLayoutAndStyleProps(style: React.CSSProperties, layout: LayoutProps) {
  let workingLayoutProps: React.CSSProperties = { ...layout }
  if (style.flexDirection !== null) {
    workingLayoutProps.flexDirection = style.flexDirection
  }
}

export function calculateChildStylesToPrepend(
  props: React.PropsWithChildren<UtopiaComponentProps>,
  children: React.ReactNode,
): Array<CSSFrame> {
  const layout = { ...props.layout, ...props.style } as LayoutProps

  switch (layout.layoutSystem) {
    case LayoutSystem.Flex:
    case LayoutSystem.PinSystem:
    case undefined:
      return computeNonGroupChildren(children, layout)
    case LayoutSystem.Group:
    case LayoutSystem.Flow:
      return []
    default:
      const _exhaustiveCheck: never = layout.layoutSystem
      throw new Error(`Unknown layout type ${JSON.stringify(layout.layoutSystem)}`)
  }
}

export function calculateChildStylesThatOverwriteStyle(
  props: React.PropsWithChildren<UtopiaComponentProps>,
  children: React.ReactNode,
): Array<CSSFrame> {
  const layout = { ...props.layout, ...props.style } as LayoutProps

  switch (layout.layoutSystem) {
    case LayoutSystem.Group:
      // Balazs: I'm keeping groups as special for now
      // WARNING: groups can only support T-L-W-H children
      const childFrames = computeGroupChildren(children)
      const selfFrame = calculateGroupBounds(childFrames)
      const shiftedChildFrames = shiftGroupChildren(selfFrame, childFrames)
      const childPositionStyles = shiftedChildFrames.map((childFrame) => {
        if (childFrame == null) {
          return {}
        } else {
          return convertPinsToAbsoluteStyleProps(childFrame)
        }
      })
      return childPositionStyles
    case LayoutSystem.Flex:
    case LayoutSystem.PinSystem:
    case LayoutSystem.Flow:
    case undefined:
      return []
    default:
      const _exhaustiveCheck: never = layout.layoutSystem
      throw new Error(`Unknown layout type ${JSON.stringify(layout.layoutSystem)}`)
  }
}

function calculateContainerProps(
  parentLayoutSystem: LayoutSystem | undefined,
  props: React.PropsWithChildren<UtopiaComponentProps>,
  children: React.ReactNode,
): CSSFrame {
  const layout = defaultIfNull({} as LayoutProps, props.layout)
  switch (layout.layoutSystem) {
    case LayoutSystem.Flex:
      return flexParentPropsToStyle({ ...props.layout, ...props.style } as LayoutProps)
    case LayoutSystem.Group:
      const childFrames = computeGroupChildren(children)
      const selfFrame = calculateGroupBounds(childFrames)
      return {
        position: 'absolute',
        ...selfFrame,
      }
    case LayoutSystem.PinSystem:
      return {
        position: parentLayoutSystem === LayoutSystem.PinSystem ? 'absolute' : 'relative',
      }
    case LayoutSystem.Flow:
    case undefined:
      return {}
    default:
      const _exhaustiveCheck: never = layout.layoutSystem
      throw new Error(`Unknown layout type ${JSON.stringify(layout.layoutSystem)}`)
  }
}

export function calculateOwnStyleProp(
  props: React.PropsWithChildren<UtopiaComponentProps>,
  children: React.ReactNode,
): CSSFrame {
  if (props.layout == null) {
    return {}
  } else {
    const layout = { ...props.layout, ...props.style } as LayoutProps
    switch (layout.layoutSystem) {
      case LayoutSystem.Flex:
        return {
          ...convertPinsToStyleProps(layout as any),
          // overwrite layout props with style props if available
          ...flexParentPropsToStyle({ ...props.layout, ...props.style } as LayoutProps),
        }
      case LayoutSystem.Group:
        // TODO more group stuff
        const childFrames = computeGroupChildren(children)
        const selfFrame = calculateGroupBounds(childFrames)
        return {
          position: 'absolute',
          ...selfFrame,
        }
      case LayoutSystem.PinSystem:
        return {
          ...flexElementPropsToStyle(layout, {}, 0, 1), // TODO instead of flexElementPropsToStyle, make a function that moves only the non-magic props to style
          ...convertPinsToStyleProps(layout as any),
          position: 'relative',
        }
      default:
        return {
          ...flexElementPropsToStyle(layout, {}, 0, 1), // TODO instead of flexElementPropsToStyle, make a function that moves only the non-magic props to style
          ...flexParentPropsToStyleWithoutDisplayFlex(layout),
          ...convertPinsToStyleProps(layout as any),
        }
    }
  }
}

export function addEventHandlersToDivProps(
  props: React.DetailedHTMLProps<React.HTMLAttributes<HTMLDivElement>, HTMLDivElement>,
): React.DetailedHTMLProps<React.HTMLAttributes<HTMLDivElement>, HTMLDivElement> {
  // Duplicate some handlers to cover ourselves.
  let updatedProps = {
    ...props,
  }
  const touchStart = props.onTouchStart
  if (touchStart != null) {
    updatedProps = {
      ...props,
      onMouseDown: () => touchStart({} as any),
    }
  }
  const touchEnd = props.onTouchEnd
  if (touchEnd != null) {
    updatedProps = {
      ...props,
      onMouseUp: () => touchEnd({} as any),
    }
  }

  return updatedProps
}

export function calculatePositionableStyle(
  props: React.PropsWithChildren<UtopiaComponentProps>,
): CSSFrame {
  if (props.layout == null) {
    return {}
  } else {
    const layout = props.layout
    return {
      position: 'absolute',
      left: layout.left,
      top: layout.top,
      ...flexParentPropsToStyle(layout),
    }
  }
}

export function calculateResizeableStyle(
  props: React.PropsWithChildren<UtopiaComponentProps>,
): CSSFrame {
  if (props.layout == null) {
    return {}
  } else {
    const layout = props.layout
    return {
      position: 'absolute',
      width: layout.width,
      height: layout.height,
      ...flexParentPropsToStyle(layout),
    }
  }
}

export function isLayoutWrapped(props: React.PropsWithChildren<LayoutableProps>): boolean {
  return props.wrappedComponent != null
}
