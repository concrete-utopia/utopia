import React from 'react'
import { moveInspectorStrategy } from '../../../../../components/canvas/canvas-strategies/strategies/shared-move-strategies-helpers'
import { MetadataUtils } from '../../../../../core/model/element-metadata-utils'
import type {
  CanvasRectangle,
  CanvasVector,
  LocalRectangle,
} from '../../../../../core/shared/math-utils'
import {
  canvasRectangle,
  canvasVector,
  isInfinityRectangle,
  localRectangle,
  zeroRectangle,
} from '../../../../../core/shared/math-utils'
import { assertNever } from '../../../../../core/shared/utils'
import { NumberInput } from '../../../../../uuiui'
import { resizeInspectorStrategy } from '../../../../canvas/canvas-strategies/strategies/keyboard-absolute-resize-strategy'
import {
  EdgePositionBottom,
  EdgePositionLeft,
  EdgePositionRight,
  EdgePositionTop,
  type EdgePosition,
} from '../../../../canvas/canvas-types'
import { InspectorContextMenuWrapper } from '../../../../context-menu-wrapper'
import { useDispatch } from '../../../../editor/store/dispatch-context'
import { Substores, useEditorState, useRefEditorState } from '../../../../editor/store/store-hook'
import { metadataSelector, selectedViewsSelector } from '../../../../inspector/inpector-selectors'
import { unsetPropertyMenuItem } from '../../../common/context-menu-items'
import {
  cssNumber,
  isEmptyInputValue,
  isUnknownInputValue,
  type CSSNumber,
  type UnknownOrEmptyInput,
} from '../../../common/css-utils'
import { useInspectorLayoutInfo } from '../../../common/property-path-hooks'
import { executeFirstApplicableStrategy } from '../../../inspector-strategies/inspector-strategy'
import { UIGridRow } from '../../../widgets/ui-grid-row'

type TLWH = 'top' | 'left' | 'width' | 'height'

function getTLWHEdgePosition(property: TLWH): EdgePosition {
  switch (property) {
    case 'top':
      return EdgePositionTop
    case 'height':
      return EdgePositionBottom
    case 'left':
      return EdgePositionLeft
    case 'width':
      return EdgePositionRight
    default:
      assertNever(property)
  }
}

function getMovementFromValues(property: TLWH, oldValue: number, newValue: number): CanvasVector {
  switch (property) {
    case 'left':
    case 'width':
      return canvasVector({ x: newValue - oldValue, y: 0 })
    case 'top':
    case 'height':
      return canvasVector({ x: 0, y: newValue - oldValue })
    default:
      assertNever(property)
  }
}

export const FrameUpdatingLayoutSection = React.memo(() => {
  const dispatch = useDispatch()
  const metadataRef = useRefEditorState(metadataSelector)
  const selectedViewsRef = useRefEditorState(selectedViewsSelector)
  const elementPathTreeRef = useRefEditorState((store) => store.editor.elementPathTree)
  const allElementPropsRef = useRefEditorState((store) => store.editor.allElementProps)
  const projectContentsRef = useRefEditorState((store) => store.editor.projectContents)
  const originalGlobalFrame: CanvasRectangle = useEditorState(
    Substores.metadata,
    (store) => {
      if (store.editor.selectedViews.length === 1) {
        const metadata = MetadataUtils.findElementByElementPath(
          store.editor.jsxMetadata,
          store.editor.selectedViews[0],
        )
        const maybeInfinityGlobalFrame = metadata?.globalFrame ?? canvasRectangle(zeroRectangle)
        return isInfinityRectangle(maybeInfinityGlobalFrame)
          ? canvasRectangle(zeroRectangle)
          : maybeInfinityGlobalFrame
      } else {
        return canvasRectangle(zeroRectangle)
      }
    },
    'SimplifiedLayoutSubsection originalGlobalFrame',
  )
  const originalLocalFrame: LocalRectangle = useEditorState(
    Substores.metadata,
    (store) => {
      if (store.editor.selectedViews.length === 1) {
        const metadata = MetadataUtils.findElementByElementPath(
          store.editor.jsxMetadata,
          store.editor.selectedViews[0],
        )
        const maybeInfinityLocalFrame = metadata?.localFrame ?? localRectangle(zeroRectangle)
        return isInfinityRectangle(maybeInfinityLocalFrame)
          ? localRectangle(zeroRectangle)
          : maybeInfinityLocalFrame
      } else {
        return localRectangle(zeroRectangle)
      }
    },
    'SimplifiedLayoutSubsection originalGlobalFrame',
  )

  const updateFrame = React.useCallback(
    (edgePosition: EdgePosition, movement: CanvasVector) => {
      if (edgePosition === EdgePositionTop || edgePosition === EdgePositionLeft) {
        executeFirstApplicableStrategy(
          dispatch,
          metadataRef.current,
          selectedViewsRef.current,
          elementPathTreeRef.current,
          allElementPropsRef.current,
          [moveInspectorStrategy(projectContentsRef.current, movement)],
        )
      } else {
        executeFirstApplicableStrategy(
          dispatch,
          metadataRef.current,
          selectedViewsRef.current,
          elementPathTreeRef.current,
          allElementPropsRef.current,
          [
            resizeInspectorStrategy(
              projectContentsRef.current,
              originalGlobalFrame,
              edgePosition,
              movement,
            ),
          ],
        )
      }
    },
    [
      allElementPropsRef,
      dispatch,
      elementPathTreeRef,
      metadataRef,
      originalGlobalFrame,
      projectContentsRef,
      selectedViewsRef,
    ],
  )

  return (
    <>
      <UIGridRow
        padded={false}
        variant='<--1fr--><--1fr-->'
        style={{ minHeight: undefined, gap: 4 }}
      >
        <FrameUpdatingLayoutControl
          property='left'
          label='L'
          updateFrame={updateFrame}
          currentValue={originalLocalFrame.x}
        />
        <FrameUpdatingLayoutControl
          property='top'
          label='T'
          updateFrame={updateFrame}
          currentValue={originalLocalFrame.y}
        />
      </UIGridRow>
      <UIGridRow
        padded={false}
        variant='<--1fr--><--1fr-->'
        style={{ minHeight: undefined, gap: 4 }}
      >
        <FrameUpdatingLayoutControl
          property='width'
          label='W'
          updateFrame={updateFrame}
          currentValue={originalLocalFrame.width}
        />
        <FrameUpdatingLayoutControl
          property='height'
          label='H'
          updateFrame={updateFrame}
          currentValue={originalLocalFrame.height}
        />
      </UIGridRow>
    </>
  )
})

interface LayoutPinPropertyControlProps {
  label: string
  property: TLWH
  currentValue: number
  updateFrame: (edgePosition: EdgePosition, movement: CanvasVector) => void
}

const FrameUpdatingLayoutControl = React.memo((props: LayoutPinPropertyControlProps) => {
  const { property, currentValue, updateFrame } = props
  const pointInfo = useInspectorLayoutInfo(props.property)

  const [mountCount, forceUpdate] = React.useReducer((c) => c + 1, 0)

  const onSubmitValue = React.useCallback(
    (newValue: UnknownOrEmptyInput<CSSNumber>) => {
      if (isUnknownInputValue(newValue)) {
        // Ignore right now.
      } else if (isEmptyInputValue(newValue)) {
        // Reset the NumberInput
        forceUpdate()
      } else {
        if (newValue.unit == null || newValue.unit === 'px') {
          const edgePosition = getTLWHEdgePosition(property)
          const movement = getMovementFromValues(property, currentValue, newValue.value)
          updateFrame(edgePosition, movement)
        } else {
          console.error('Attempting to use a value with a unit, which is invalid.')
        }
      }
    },
    [updateFrame, forceUpdate, property, currentValue],
  )

  return (
    <InspectorContextMenuWrapper
      id={`position-${props.property}-context-menu`}
      items={[unsetPropertyMenuItem(props.property, pointInfo.onUnsetValues)]}
      data={{}}
    >
      <NumberInput
        key={`pin-${props.property}-number-input-mount-${mountCount}`}
        data-controlstatus={pointInfo.controlStatus}
        value={cssNumber(props.currentValue)}
        id={`pin-${props.property}-number-input`}
        testId={`pin-${props.property}-number-input`}
        labelInner={props.label}
        onSubmitValue={onSubmitValue}
        onTransientSubmitValue={onSubmitValue}
        controlStatus={'simple'}
        numberType={'LengthPercent'}
        defaultUnitToHide={'px'}
      />
    </InspectorContextMenuWrapper>
  )
})
