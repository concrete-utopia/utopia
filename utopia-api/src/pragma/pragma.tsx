/** @jsx jsx */
import { jsx as EmotionJsx } from '@emotion/core'
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
    const HOC = React.forwardRef((propsWithoutRef: any, ref: any) => {
      const props = applyRefToProps(type, propsWithoutRef, ref)
      if (props == null || props.layout == null) {
        return EmotionJsx(type, props)
      }

      const reactChildren = props.children

      const { layout: passedLayout } = props

      const childStyles = calculateChildStylesToPrepend(props, reactChildren)
      const ownStyle = calculateOwnStyleProp(props, reactChildren)
      const childStylesThatOverwriteStyle = calculateChildStylesThatOverwriteStyle(
        props,
        reactChildren,
      )

      const isGroup: boolean = passedLayout == null ? false : passedLayout.layoutSystem === 'group'
      const styleFromProps = isGroup ? filterFrameFromStyle(props.style) : { ...props.style }

      const styleIsDefined = styleFromProps != null || ownStyle != undefined
      const finalOwnProps = styleIsDefined
        ? {
            ...props,
            style: {
              ...ownStyle,
              ...styleFromProps,
            },
          }
        : props // we don't modify the props object in case there is no need to do so

      const mappedChildren = React.Children.map(reactChildren, (child, index) => {
        if (
          (childStyles[index] != null || childStylesThatOverwriteStyle[index] != null) &&
          React.isValidElement(child)
        ) {
          const removeLayoutPropFromReactBuiltins = typeof child.type === 'string' ? {} : {} // do not remove layout prop

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
          return React.cloneElement(child as any, {
            ...removeLayoutPropFromReactBuiltins,
            style: layoutEnhancedStyleProp,
          })
        } else {
          return child
        }
      })

      return EmotionJsx(type, finalOwnProps, mappedChildren)
    })
    pushToHocCache(hocForTypeCache, { componentToCreateHOCFor: type, createdHOC: HOC })
    return EmotionJsx(HOC, ...pragmaParams)
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
