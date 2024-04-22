import React from 'react'
import ReactDOM from 'react-dom'
import { arrayAccumulate } from '../../../core/shared/array-utils'
import type { ElementPath } from '../../../core/shared/project-file-types'
import { MetadataUtils } from '../../../core/model/element-metadata-utils'
import { Substores, useEditorState } from '../../editor/store/store-hook'
import { CanvasOffsetWrapper } from './canvas-offset-wrapper'
import { useBoundingBox } from './bounding-box-hooks'
import { isZeroSizedElement } from './outline-utils'
import * as EP from '../../../core/shared/element-path'
import { useDispatch } from '../../editor/store/dispatch-context'
import { openFloatingInsertMenu } from '../../editor/actions/action-creators'
import {
  ComponentPickerContextMenu,
  useShowComponentPickerContextMenu,
} from '../../navigator/navigator-item/component-picker-context-menu'
import { CanvasContextMenuPortalTargetID } from '../../../core/shared/utils'

export const PlaceholderChildrenOutlineControl = React.memo(() => {
  const placeholderChildren = useEditorState(
    Substores.metadata,
    (store) => {
      return arrayAccumulate<ElementPath>((childrenArray) => {
        for (const selectedView of store.editor.selectedViews) {
          const selectedViewChildren = MetadataUtils.getChildrenPathsOrdered(
            store.editor.jsxMetadata,
            store.editor.elementPathTree,
            selectedView,
          )
          for (const selectedViewChild of selectedViewChildren) {
            const metadata = MetadataUtils.findElementByElementPath(
              store.editor.jsxMetadata,
              selectedViewChild,
            )

            if (metadata?.isPlaceholder) {
              childrenArray.push(selectedViewChild)
            }
          }
        }
      })
    },
    'PlaceholderOutlineContol placeholderChildrenBoundingBox',
  )

  if (placeholderChildren.length === 0) {
    return null
  }
  return (
    <CanvasOffsetWrapper>
      {placeholderChildren.map((child) => (
        <PlaceholderOutlineControl
          key={`PlaceholderOutlineControl-${EP.toString(child)}`}
          target={child}
        />
      ))}
    </CanvasOffsetWrapper>
  )
})

interface PlaceholderOutlineControlProps {
  target: ElementPath
}

const SuppressEvent = (e: React.MouseEvent) => {
  e.stopPropagation()
}

export const PlaceholderOutlineControl = React.memo<PlaceholderOutlineControlProps>((props) => {
  const { target } = props
  const componentPickerContextMenuId = `component-picker-canvas-${EP.toString(target)}`
  const { showComponentPickerContextMenu } = useShowComponentPickerContextMenu(
    componentPickerContextMenuId,
  )

  const portalTarget = document.getElementById(CanvasContextMenuPortalTargetID)
  const dispatch = useDispatch()

  const insertElement: React.MouseEventHandler<HTMLDivElement> = React.useCallback(
    (event) => {
      event.stopPropagation()
      event.preventDefault()
      dispatch(
        [
          openFloatingInsertMenu({
            insertMenuMode: 'insert',
            parentPath: EP.parentPath(target),
            indexPosition: null,
          }),
        ],
        'canvas',
      )
    },
    [dispatch, target],
  )

  const outlineRef = useBoundingBox(
    [target],
    (ref, safeGappedBoundingBox, realBoundingBox, canvasScale) => {
      if (isZeroSizedElement(realBoundingBox)) {
        ref.current.style.display = 'none'
      } else {
        ref.current.style.display = 'block'
        ref.current.style.left = `${safeGappedBoundingBox.x - 0.5 / canvasScale}px`
        ref.current.style.top = `${safeGappedBoundingBox.y - 0.5 / canvasScale}px`
        ref.current.style.width = `${safeGappedBoundingBox.width + 1 / canvasScale}px`
        ref.current.style.height = `${safeGappedBoundingBox.height + 1 / canvasScale}px`
      }
    },
  )

  return (
    <>
      {portalTarget == null
        ? null
        : ReactDOM.createPortal(
            <ComponentPickerContextMenu
              target={EP.parentPath(target)}
              key={componentPickerContextMenuId}
              id={componentPickerContextMenuId}
              prop={undefined}
            />,
            portalTarget,
          )}
      <div
        data-testid={`PlaceholderOutlineControl-${EP.toString(target)}`}
        ref={outlineRef}
        style={{
          position: 'absolute',
          border: '1px solid #e0e0e0',
          borderRadius: '0 10% 0 10%',
          backgroundImage: 'linear-gradient(45deg, #EFEFEF 0%, #F8F8F8 100%)',
        }}
        onClick={showComponentPickerContextMenu}
        onMouseDown={SuppressEvent}
        onMouseUp={SuppressEvent}
      />
    </>
  )
})
