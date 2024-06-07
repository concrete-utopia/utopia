import React from 'react'
import * as ReactDOM from 'react-dom'
import { useHandleCloseOnESCOrEnter } from '../common/inspector-utils'
import { EditorID, PortalTargetID } from '../../../core/shared/utils'
import ResizeObserver from 'resize-observer-polyfill'
import { OnClickOutsideHOC } from '../../../uuiui'

export type InspectorModalProps = {
  offsetX: number
  offsetY: number
  portalTarget?: HTMLElement
  closePopup: () => void
  children: JSX.Element
  style?: React.CSSProperties
  closePopupOnUnmount?: boolean
  outsideClickIgnoreClass?: string
}

const padding = 16

export const InspectorModal: React.FunctionComponent<
  React.PropsWithChildren<InspectorModalProps>
> = ({
  offsetX,
  offsetY,
  portalTarget = document.getElementById(PortalTargetID) as HTMLElement,
  closePopup,
  children,
  style,
  outsideClickIgnoreClass,
  closePopupOnUnmount = true,
}) => {
  useHandleCloseOnESCOrEnter(closePopup)
  const outerElementRef = React.useRef<HTMLDivElement>(null)
  const wrapperRef = React.useRef<HTMLDivElement>(null)
  const [editorWidth, setEditorWidth] = React.useState(document.body.clientWidth)
  const [editorHeight, setEditorHeight] = React.useState(document.body.clientHeight)
  const [wrapperWidth, setWrapperWidth] = React.useState(0)
  const [wrapperHeight, setWrapperHeight] = React.useState(0)
  const [originLeft, setOriginLeft] = React.useState(0)
  const [originTop, setOriginTop] = React.useState(0)

  React.useEffect(
    () => () => {
      if (closePopupOnUnmount && closePopup != null) {
        closePopup()
      }
    },
    [closePopup, closePopupOnUnmount],
  )

  const updatePositionAndSize = React.useCallback(() => {
    setEditorWidth(document.body.clientWidth)
    setEditorHeight(document.body.clientHeight)
    if (wrapperRef.current != null) {
      setWrapperWidth(wrapperRef.current.children[0].clientWidth)
      setWrapperHeight(wrapperRef.current.children[0].clientHeight)
    }
    if (outerElementRef.current != null) {
      const boundingRect = outerElementRef.current.getBoundingClientRect()
      setOriginLeft(boundingRect.left)
      setOriginTop(boundingRect.top)
    }
  }, [])

  React.useLayoutEffect(() => {
    window.addEventListener('resize', updatePositionAndSize)
    const observer = new ResizeObserver(() => updatePositionAndSize())
    const wrapperRefCurrent = wrapperRef.current
    if (wrapperRefCurrent != null) {
      observer.observe(wrapperRefCurrent.children[0])
    }
    updatePositionAndSize()
    return () => {
      if (wrapperRefCurrent != null) {
        observer.unobserve(wrapperRefCurrent.children[0])
      }
      window.removeEventListener('resize', updatePositionAndSize)
    }
  }, [updatePositionAndSize])

  const cssOffset = {
    left: Math.min(Math.max(originLeft + offsetX, 0), editorWidth - wrapperWidth - padding),
    top: Math.min(Math.max(originTop + offsetY, 0), editorHeight - wrapperHeight - padding),
  }

  return (
    <div ref={outerElementRef}>
      {ReactDOM.createPortal(
        <OnClickOutsideHOC
          onClickOutside={closePopup}
          outsideClickIgnoreClass={outsideClickIgnoreClass}
        >
          <div
            style={{
              position: 'absolute',
              left: cssOffset.left,
              top: cssOffset.top,
              zIndex: 1,
              ...style,
            }}
          >
            <div ref={wrapperRef}>{children}</div>
          </div>
        </OnClickOutsideHOC>,
        portalTarget,
      )}
    </div>
  )
}
