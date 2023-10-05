import React from 'react'
import { FlexColumn, FlexRow, InspectorSubsectionHeader, NumberInput } from '../../../../../uuiui'
import { EditorContractDropdown } from '../../../editor-contract-section'
import { UIGridRow } from '../../../widgets/ui-grid-row'
import { FixedHugDropdown } from '../../../fill-hug-fixed-control'
import {
  useInspectorLayoutInfo,
  useInspectorStyleInfo,
  useIsSubSectionVisible,
} from '../../../common/property-path-hooks'
import { BooleanControl } from '../../../controls/boolean-control'
import { InspectorContextMenuWrapper } from '../../../../context-menu-wrapper'
import {
  isUnknownInputValue,
  type CSSNumber,
  type UnknownOrEmptyInput,
  isEmptyInputValue,
  cssNumber,
} from '../../../common/css-utils'
import { unsetPropertyMenuItem } from '../../../common/context-menu-items'
import { executeFirstApplicableStrategy } from '../../../inspector-strategies/inspector-strategy'
import { useDispatch } from '../../../../editor/store/dispatch-context'
import { Substores, useEditorState, useRefEditorState } from '../../../../editor/store/store-hook'
import { metadataSelector, selectedViewsSelector } from '../../../../inspector/inpector-selectors'
import { assertNever } from '../../../../../core/shared/utils'
import { resizeInspectorStrategy } from '../../../../canvas/canvas-strategies/strategies/keyboard-absolute-resize-strategy'
import {
  EdgePositionBottom,
  type EdgePosition,
  EdgePositionLeft,
  EdgePositionRight,
  EdgePositionTop,
} from '../../../../canvas/canvas-types'
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
import { MetadataUtils } from '../../../../../core/model/element-metadata-utils'
import { moveInspectorStrategy } from '../../../../../components/canvas/canvas-strategies/strategies/shared-move-strategies-helpers'
import { useForceUpdate } from '../../../../editor/hook-utils'

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

export interface LayoutPinPropertyControlProps {
  label: string
  property: TLWH
  currentValue: number
  updateFrame: (edgePosition: EdgePosition, movement: CanvasVector) => void
}

export const LayoutPinPropertyControl = React.memo((props: LayoutPinPropertyControlProps) => {
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

export const SimplifiedLayoutSubsection = React.memo(() => {
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
    <FlexColumn style={{ paddingBottom: 8 }}>
      <InspectorSubsectionHeader>
        <FlexRow
          style={{
            flexGrow: 1,
            gap: 8,
            height: 42,
          }}
        >
          <EditorContractDropdown />
        </FlexRow>
      </InspectorSubsectionHeader>
      <FlexColumn style={{ gap: 8, paddingLeft: 8, paddingRight: 8 }}>
        <UIGridRow
          padded={false}
          variant='<--1fr--><--1fr-->'
          style={{ minHeight: undefined, gap: 4 }}
        >
          <LayoutPinPropertyControl
            property='left'
            label='L'
            updateFrame={updateFrame}
            currentValue={originalLocalFrame.x}
          />
          <LayoutPinPropertyControl
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
          <LayoutPinPropertyControl
            property='width'
            label='W'
            updateFrame={updateFrame}
            currentValue={originalLocalFrame.width}
          />
          <LayoutPinPropertyControl
            property='height'
            label='H'
            updateFrame={updateFrame}
            currentValue={originalLocalFrame.height}
          />
        </UIGridRow>
        <UIGridRow
          padded={false}
          variant='<--1fr--><--1fr-->'
          style={{ minHeight: undefined, gap: 4 }}
        >
          <FixedHugDropdown dimension='width' />
          <FixedHugDropdown dimension='height' />
        </UIGridRow>
        <FlexRow style={{ minHeight: undefined, gap: 4 }}>
          <ClipContentControl />
        </FlexRow>
      </FlexColumn>
    </FlexColumn>
  )
})
SimplifiedLayoutSubsection.displayName = 'SimplifiedLayoutSubsection'

const ClipContentControl = React.memo(() => {
  const isVisible = useIsSubSectionVisible('overflow')

  const { value, controlStatus, controlStyles, onSubmitValue, onUnsetValues } =
    useInspectorStyleInfo(
      'overflow',
      (parsed) => !parsed.overflow,
      (clipContent: boolean) => ({
        overflow: !clipContent,
      }),
    )

  if (!isVisible) {
    return null
  }

  return (
    <>
      <BooleanControl
        key='clip-content-control'
        id='clip-content-control'
        testId='clip-content-control'
        value={value}
        controlStatus={controlStatus}
        controlStyles={controlStyles}
        onSubmitValue={onSubmitValue}
      />
      Clip content
    </>
  )
})
