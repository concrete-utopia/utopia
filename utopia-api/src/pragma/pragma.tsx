/** @jsx jsx */
import { jsx as EmotionJsx } from '@emotion/react'
import * as React from 'react'
import {
  calculateChildStylesToPrepend,
  calculateOwnStyleProp,
  calculateChildStylesThatOverwriteStyle,
} from '../primitives/common'
import { LayoutProps } from '../layout/layout'

declare module 'react' {
  interface DOMAttributes<T> {
    layout?: LayoutProps
  }
}

const MAX_CACHE_SIZE = 10000
type HocCacheItem = { componentToCreateHOCFor: any; createdHOC: any }
const hocForTypeCache: HocCacheItem[] = []

function pushToHocCache(cache: HocCacheItem[], newCacheItem: HocCacheItem) {
  if (cache.length >= MAX_CACHE_SIZE) {
    cache.shift() // might need to use something like tiny-queue or a custom linked lsit to avoid the cost of shifting
  }
  return cache.push(newCacheItem)
}

function findHocForType(cache: HocCacheItem[], type: any): any | null {
  const found = cache.find((cacheItem) => {
    return cacheItem.componentToCreateHOCFor === type
  })
  if (found == null) {
    return null
  } else {
    return found.createdHOC
  }
}

function applyRefToProps(type: any, props: any, ref: any): any {
  // Attempt to change as little as possible.
  if (props == null) {
    if (ref == null) {
      return props
    } else {
      return { ref: ref }
    }
  } else {
    if (ref == null) {
      return props
    } else {
      return {
        ...props,
        ref: ref,
      }
    }
  }
}

export const jsx = (type: any, ...pragmaParams: any[]) => {
  if (typeof type !== 'function' && type === React.Fragment) {
    // early return, we are not wrapping Fragments
    return EmotionJsx(type, ...pragmaParams)
  }
  const foundHocForType = findHocForType(hocForTypeCache, type)
  if (foundHocForType != null) {
    return EmotionJsx(foundHocForType, ...pragmaParams)
  } else {
    const HOC = (propsWithoutRef: any, ref: any) => {
      const props = applyRefToProps(type, propsWithoutRef, ref)
      if (props == null || props.layout == null) {
        return EmotionJsx(type, props)
      }

      const reactChildren = props.children

      const { layout: passedLayout, ...originalProps } = props

      const childStyles = calculateChildStylesToPrepend(props, reactChildren)
      const ownStyle = calculateOwnStyleProp(props, reactChildren)
      const childStylesThatOverwriteStyle = calculateChildStylesThatOverwriteStyle(
        props,
        reactChildren,
      )

      const isGroup: boolean = passedLayout == null ? false : passedLayout.layoutSystem === 'group'
      const styleFromProps = isGroup
        ? filterFrameFromStyle(originalProps.style)
        : { ...originalProps.style }

      const styleIsDefined = styleFromProps != null || ownStyle != undefined
      const finalOwnProps = styleIsDefined
        ? {
            ...originalProps,
            style: {
              ...ownStyle,
              ...styleFromProps,
            },
          }
        : originalProps // we don't modify the props object in case there is no need to do so

      const mappedChildren = React.Children.map(reactChildren, (child, index) => {
        if (
          (childStyles[index] != null || childStylesThatOverwriteStyle[index] != null) &&
          React.isValidElement(child)
        ) {
          const removeLayoutPropFromReactBuiltins =
            typeof child.type === 'string' ? { layout: undefined } : {}

          const childProps: any = child.props
          const childStyleProps =
            typeof childProps === 'object' &&
            childProps != null &&
            typeof childProps.style === 'object'
              ? childProps.style
              : {}

          const layoutEnhancedStyleProp = {
            ...childStyles[index],
            ...childStyleProps,
            ...childStylesThatOverwriteStyle[index],
          }

          const updatedProps =
            (child.type as any)?.theOriginalType == React.Fragment
              ? (child.props as any)
              : {
                  ...removeLayoutPropFromReactBuiltins,
                  style: layoutEnhancedStyleProp,
                }
          return React.cloneElement(child as any, updatedProps)
        } else {
          return child
        }
      })

      return EmotionJsx(type, finalOwnProps, mappedChildren)
    }
    HOC.displayName = `UtopiaPragma(${getDisplayName(type)})`
    const HOCWithForwardRef = React.forwardRef(HOC)
    pushToHocCache(hocForTypeCache, {
      componentToCreateHOCFor: type,
      createdHOC: HOCWithForwardRef,
    })
    return EmotionJsx(HOCWithForwardRef, ...pragmaParams)
  }
}

function filterFrameFromStyle(style: React.CSSProperties | undefined): React.CSSProperties {
  if (style == null) {
    return {}
  } else {
    const { top, left, width, height, ...styleWithoutFrame } = style
    return styleWithoutFrame
  }
}

function getDisplayName(type: any) {
  // taken from https://github.com/facebook/react/blob/7e405d458d6481fb1c04dfca6afab0651e6f67cd/packages/react/src/ReactElement.js#L415
  if (typeof type === 'function') {
    return type.displayName || type.name || 'Unknown'
  } else if (typeof type === 'symbol') {
    return type.toString()
  } else if (typeof type === 'string') {
    return type
  } else {
    return 'Unknown'
  }
}
