/** @jsx jsx */
import { jsx as EmotionJsx } from '@emotion/core'
import * as React from 'react'

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

      const { layout: passedLayout, ...originalProps } = props

      const finalOwnProps = { ...originalProps }

      return EmotionJsx(type, finalOwnProps, reactChildren)
    })
    pushToHocCache(hocForTypeCache, { componentToCreateHOCFor: type, createdHOC: HOC })
    return EmotionJsx(HOC, ...pragmaParams)
  }
}
