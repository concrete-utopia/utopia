// TODO move me to utopia-api once stable!

import * as React from 'react'
import { CSSProperties } from 'react'

interface Rectangle {
  left: number
  top: number
  right: number
  bottom: number
  width: number
  height: number
}

function getChildrenAABB(children: NodeListOf<ChildNode>): Rectangle {
  // thanks chatGPT!
  let minX = Infinity
  let minY = Infinity
  let maxX = -Infinity
  let maxY = -Infinity

  children.forEach((child) => {
    if (!(child instanceof HTMLElement)) {
      return
    }

    const rect = child.getBoundingClientRect()
    const x1 = rect.left
    const y1 = rect.top
    const x2 = rect.right
    const y2 = rect.bottom

    minX = Math.min(minX, x1)
    minY = Math.min(minY, y1)
    maxX = Math.max(maxX, x2)
    maxY = Math.max(maxY, y2)
  })

  return {
    left: minX,
    top: minY,
    right: maxX,
    bottom: maxY,
    width: maxX - minX,
    height: maxY - minY,
  }
}

export const UtopiaApiGroup = (props: React.PropsWithChildren<{ style?: CSSProperties }>) => {
  const groupRef = React.useRef<HTMLDivElement>(null)

  function changeSizeToMatchChildren() {
    if (groupRef.current == null) {
      return
    }
    const group = groupRef.current

    const children = group.childNodes
    const childrenAABB = getChildrenAABB(children)

    group.style.width = childrenAABB.width + 'px'
    group.style.height = childrenAABB.height + 'px'
    group.style.position = 'relative'
  }

  React.useLayoutEffect(() => {
    if (groupRef.current?.parentElement != null) {
      // const parentDisplayProp = window.getComputedStyle(groupRef.current?.parentElement).display
      // if (parentDisplayProp !== 'flex') {
      //   return
      // }
    }
    changeSizeToMatchChildren()
    return function cleanup() {
      // empty
    }
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
      ref={groupRef}
      style={{
        ...props.style,
      }}
    >
      {props.children}
    </div>
  )
}
