import React from 'react'

import { useContextSelector } from 'use-context-selector'
import { LayoutSystem } from 'utopia-api/core'
import { MetadataUtils } from '../../../../../core/model/element-metadata-utils'
import { mapArrayToDictionary } from '../../../../../core/shared/array-utils'
import {
  DetectedLayoutSystem,
  SettableLayoutSystem,
} from '../../../../../core/shared/element-template'
import {
  clampValue,
  numberIsZero,
  roundTo,
  zeroRectIfNullOrInfinity,
} from '../../../../../core/shared/math-utils'
import { PropertyPath } from '../../../../../core/shared/project-file-types'
import { styleStringInArray } from '../../../../../utils/common-constants'
import { FunctionIcons, SquareButton } from '../../../../../uuiui'
import { oppositeEdgePiece } from '../../../../canvas/canvas-types'
import {
  adjustCssLengthProperty,
  AdjustCssLengthProperty,
} from '../../../../canvas/commands/adjust-css-length-command'
import { SubduedPaddingControl } from '../../../../canvas/controls/select-mode/subdued-padding-control'
import { pixelPaddingFromPadding } from '../../../../canvas/padding-utils'
import { InspectorContextMenuWrapper } from '../../../../context-menu-wrapper'
import { applyCommandsAction, switchLayoutSystem } from '../../../../editor/actions/action-creators'
import { useDispatch } from '../../../../editor/store/dispatch-context'
import { useRefEditorState } from '../../../../editor/store/store-hook'
import { optionalAddOnUnsetValues } from '../../../common/context-menu-items'
import {
  ControlStatus,
  ControlStyles,
  getControlStatusFromPropertyStatus,
  getControlStyles,
} from '../../../common/control-status'
import { useInspectorInfoLonghandShorthand } from '../../../common/longhand-shorthand-hooks'
import {
  InspectorCallbackContext,
  InspectorInfo,
  InspectorPropsContext,
  stylePropPathMappingFn,
  useInspectorContext,
  useInspectorInfoSimpleUntyped,
  useInspectorLayoutInfo,
  useInspectorStyleInfo,
} from '../../../common/property-path-hooks'
import { OptionChainControl } from '../../../controls/option-chain-control'
import { detectFillHugFixedState } from '../../../inspector-common'
import { PropertyLabel } from '../../../widgets/property-label'
import { UIGridRow } from '../../../widgets/ui-grid-row'
import {
  longhandShorthandEventHandler,
  SplitChainedEvent,
  splitChainedEventValueForProp,
  SplitChainedNumberInput,
  SplitControlValues,
} from './split-chained-number-input'

function useDefaultedLayoutSystemInfo(): {
  value: LayoutSystem | 'flow'
  controlStatus: ControlStatus
  controlStyles: ControlStyles
} {
  const styleDisplayMetadata = useInspectorStyleInfo('display')

  let metadataToUse: InspectorInfo<any> = styleDisplayMetadata
  if (styleDisplayMetadata.value === 'flex') {
    metadataToUse = styleDisplayMetadata
  }

  if (metadataToUse.value == null) {
    const updatedPropertyStatus = {
      ...metadataToUse.propertyStatus,
      set: true,
    }
    const controlStatus = getControlStatusFromPropertyStatus(updatedPropertyStatus)
    const controlStyles = getControlStyles(controlStatus)
    return {
      value: 'flow',
      controlStatus,
      controlStyles,
    }
  } else {
    return {
      value: metadataToUse.value,
      controlStatus: metadataToUse.controlStatus,
      controlStyles: metadataToUse.controlStyles,
    }
  }
}

export function useLayoutSystemData() {
  const dispatch = useDispatch()
  const targetPath = useContextSelector(InspectorPropsContext, (contextData) => {
    return contextData.targetPath
  })
  const onLayoutSystemChange = React.useCallback(
    (layoutSystem: SettableLayoutSystem) => {
      switch (layoutSystem) {
        case LayoutSystem.PinSystem:
        case 'flow':
        case 'flex':
          dispatch([switchLayoutSystem(layoutSystem, targetPath)], 'everyone')
          break
        case LayoutSystem.Group:
        case 'grid':
          // 'grid' and 'group' are not clickable buttons, they only have an indicative role
          break
        default:
          const _exhaustiveCheck: never = layoutSystem
          throw new Error(`Unknown layout system ${JSON.stringify(layoutSystem)}`)
      }
    },
    [dispatch, targetPath],
  )

  const { value, controlStatus, controlStyles } = useDefaultedLayoutSystemInfo()

  return {
    onSubmitValue: onLayoutSystemChange,
    value,
    controlStatus,
    controlStyles,
  }
}

interface LayoutSystemControlProps {
  layoutSystem: DetectedLayoutSystem | null
  providesCoordinateSystemForChildren: boolean
}

export const LayoutSystemControl = React.memo((props: LayoutSystemControlProps) => {
  const layoutSystemData = useLayoutSystemData()
  const detectedLayoutSystem = props.layoutSystem ?? layoutSystemData.value
  return (
    <OptionChainControl
      id={'layoutSystem'}
      key={'layoutSystem'}
      testId={'layoutSystem'}
      onSubmitValue={layoutSystemData.onSubmitValue}
      value={detectedLayoutSystem}
      options={layoutSystemOptions}
      controlStatus={layoutSystemData.controlStatus}
      controlStyles={layoutSystemData.controlStyles}
    />
  )
})

const layoutSystemOptions = [
  {
    value: 'flex',
    tooltip: 'Layout children with flexbox',
    label: 'Flex',
  },

  {
    value: 'grid',
    tooltip: 'Layout children with grid',
    label: 'Grid',
  },
]

export function buildPaddingPropsToUnset(
  propertyTarget: ReadonlyArray<string>,
): Array<PropertyPath> {
  return [
    stylePropPathMappingFn('padding', propertyTarget),
    stylePropPathMappingFn('paddingLeft', propertyTarget),
    stylePropPathMappingFn('paddingTop', propertyTarget),
    stylePropPathMappingFn('paddingRight', propertyTarget),
    stylePropPathMappingFn('paddingBottom', propertyTarget),
  ]
}

export const PaddingRow = React.memo(() => {
  const targetPath = useContextSelector(InspectorPropsContext, (contextData) => {
    return contextData.targetPath
  })
  const paddingPropsToUnset = React.useMemo(() => {
    return buildPaddingPropsToUnset(targetPath)
  }, [targetPath])
  const metadata = useInspectorInfoSimpleUntyped(
    paddingPropsToUnset,
    (v) => v,
    (v) => v,
  )

  const contextMenuLabel = React.useMemo(() => ['all paddings'], [])
  const contextMenuItems = React.useMemo(
    () =>
      optionalAddOnUnsetValues(
        metadata.propertyStatus.set,
        contextMenuLabel,
        metadata.onUnsetValues,
      ),
    [contextMenuLabel, metadata.propertyStatus.set, metadata.onUnsetValues],
  )

  return (
    <InspectorContextMenuWrapper
      id='padding-subsection-context-menu'
      items={contextMenuItems}
      data={null}
    >
      <UIGridRow tall padded={true} variant='<---1fr--->|------172px-------|'>
        <PropertyLabel
          target={paddingPropsToUnset}
          propNamesToUnset={contextMenuLabel}
          style={{
            paddingBottom: 20,
          }}
        >
          Padding
        </PropertyLabel>
        <PaddingControl />
      </UIGridRow>
    </InspectorContextMenuWrapper>
  )
})

export const PaddingControl = React.memo(() => {
  const { paddingTop, paddingRight, paddingBottom, paddingLeft } =
    useInspectorInfoLonghandShorthand(
      ['paddingTop', 'paddingRight', 'paddingBottom', 'paddingLeft'],
      'padding',
      stylePropPathMappingFn,
    )

  const shorthand = useInspectorLayoutInfo('padding')
  const dispatch = useDispatch()

  const { selectedViewsRef } = useInspectorContext()

  const canvasControlsForSides = React.useMemo(() => {
    return mapArrayToDictionary(
      ['top', 'right', 'bottom', 'left'],
      (k) => k,
      (side) => ({
        onHover: {
          control: SubduedPaddingControl,
          props: {
            side: side,
            hoveredOrFocused: 'hovered',
          },
          key: `subdued-padding-control-hovered-${side}`,
        },
        onFocus: {
          control: SubduedPaddingControl,
          props: {
            side: side,
            hoveredOrFocused: 'focused',
          },
          key: `subdued-padding-control-focused-${side}`,
        },
      }),
    )
  }, [])

  const metadataRef = useRefEditorState((store) => store.editor.jsxMetadata)
  const startingFrame = MetadataUtils.getFrameOrZeroRect(
    selectedViewsRef.current[0],
    metadataRef.current,
  )

  const onPaddingChange = React.useCallback(
    (e: SplitChainedEvent, aggregates: SplitControlValues) => {
      const selectedElement = selectedViewsRef.current[0]
      const metadata = metadataRef.current
      const targetFrame = MetadataUtils.getFrameOrZeroRect(selectedElement, metadata)

      const allChildPaths = MetadataUtils.getChildrenPaths(metadata, selectedElement)

      const nonAbsoluteChildrenPaths = allChildPaths.filter((childPath) =>
        MetadataUtils.targetParticipatesInAutoLayout(metadata, childPath),
      )

      const elementMetadata = MetadataUtils.findElementByElementPath(metadata, selectedElement)
      const elementParentBounds = elementMetadata?.specialSizeMeasurements.immediateParentBounds
      const elementParentFlexDirection =
        elementMetadata?.specialSizeMeasurements.parentFlexDirection

      const adjustSizeCommandForDimension = (
        dimension: 'horizontal' | 'vertical',
      ): AdjustCssLengthProperty | null => {
        const isHorizontal = dimension === 'horizontal'
        const dimensionKey = isHorizontal ? 'width' : 'height'
        const relevantParentSize = elementParentBounds?.[dimensionKey]

        const edgePieceToUse = dimension === 'horizontal' ? 'left' : 'top'
        const pixelPaddingSide1 = pixelPaddingFromPadding(
          splitChainedEventValueForProp(isHorizontal ? 'R' : 'T', e) ?? aggregates[edgePieceToUse],
          relevantParentSize ?? 0,
        )
        const pixelPaddingSide2 = pixelPaddingFromPadding(
          splitChainedEventValueForProp(isHorizontal ? 'L' : 'B', e) ??
            aggregates[oppositeEdgePiece(edgePieceToUse)],
          relevantParentSize ?? 0,
        )

        if (pixelPaddingSide1 == null || pixelPaddingSide2 == null) {
          return null
        }

        const combinedPaddingInDimension = pixelPaddingSide1 + pixelPaddingSide2

        const fixedSizeChildrenPaths = nonAbsoluteChildrenPaths.filter(
          (childPath) => detectFillHugFixedState(dimension, metadata, childPath)?.type === 'fixed',
        )
        const childrenBoundingFrameMaybeInfinite = MetadataUtils.getBoundingRectangleInCanvasCoords(
          fixedSizeChildrenPaths,
          metadata,
        )
        const childrenBoundingFrame = zeroRectIfNullOrInfinity(childrenBoundingFrameMaybeInfinite)

        const combinedContentSizeInDimension =
          combinedPaddingInDimension + childrenBoundingFrame[dimensionKey]

        // TODO We need a way to call the correct resizing strategy here, but they are all assuming
        // the drag originates from a given edge, whereas we want to pass in the desired delta to a
        // dimension and receive the required commands to resize the element
        const sizeDelta = combinedContentSizeInDimension - targetFrame[dimensionKey]

        // clamp the delta so that the resultant frame will never be smaller than the starting frame
        // when scrubbing
        const clampedSizeDelta = Math.max(
          roundTo(sizeDelta, 0),
          startingFrame[dimensionKey] - targetFrame[dimensionKey],
        )

        return numberIsZero(clampedSizeDelta)
          ? null
          : adjustCssLengthProperty(
              'always',
              selectedElement,
              stylePropPathMappingFn(dimensionKey, styleStringInArray),
              clampedSizeDelta,
              elementParentBounds?.[dimensionKey],
              elementParentFlexDirection ?? null,
              'do-not-create-if-doesnt-exist',
            )
      }

      const horizontalSizeAdjustment = adjustSizeCommandForDimension('horizontal')
      const verticalSizeAdjustment = adjustSizeCommandForDimension('vertical')

      // Check if child content size + padding exceeds parent size in each dimension
      let adjustLengthCommands: Array<AdjustCssLengthProperty> = []
      if (horizontalSizeAdjustment != null) {
        adjustLengthCommands.push(horizontalSizeAdjustment)
      }

      if (verticalSizeAdjustment != null) {
        adjustLengthCommands.push(verticalSizeAdjustment)
      }

      return adjustLengthCommands.length > 0 ? [applyCommandsAction(adjustLengthCommands)] : []
    },
    [metadataRef, selectedViewsRef, startingFrame],
  )

  return (
    <SplitChainedNumberInput
      controlModeOrder={['one-value', 'per-direction', 'per-side']}
      tooltips={{
        oneValue: 'Padding',
        perDirection: 'Padding per direction',
        perSide: 'Padding per side',
      }}
      selectedViews={selectedViewsRef.current}
      name='padding'
      defaultMode='per-direction'
      top={paddingTop}
      left={paddingLeft}
      bottom={paddingBottom}
      right={paddingRight}
      shorthand={shorthand}
      canvasControls={canvasControlsForSides}
      numberType={'LengthPercent'}
      eventHandler={longhandShorthandEventHandler(
        'padding',
        {
          T: 'paddingTop',
          R: 'paddingRight',
          B: 'paddingBottom',
          L: 'paddingLeft',
        },
        selectedViewsRef.current[0],
        dispatch,
        onPaddingChange,
      )}
    />
  )
})

function layoutSystemConfigPropertyPaths(
  propertyTarget: ReadonlyArray<string>,
): Array<PropertyPath> {
  return [
    stylePropPathMappingFn('alignContent', propertyTarget),
    stylePropPathMappingFn('alignItems', propertyTarget),
    stylePropPathMappingFn('display', propertyTarget),
    stylePropPathMappingFn('flexDirection', propertyTarget),
    stylePropPathMappingFn('flexWrap', propertyTarget),
    stylePropPathMappingFn('justifyContent', propertyTarget),
    stylePropPathMappingFn('marginBottom', propertyTarget),
    stylePropPathMappingFn('marginLeft', propertyTarget),
    stylePropPathMappingFn('marginRight', propertyTarget),
    stylePropPathMappingFn('marginTop', propertyTarget),
    stylePropPathMappingFn('margin', propertyTarget),
  ]
}

function useDeleteAllLayoutConfig() {
  const { onUnsetValue } = React.useContext(InspectorCallbackContext)
  const targetPath = useContextSelector(InspectorPropsContext, (contextData) => {
    return contextData.targetPath
  })
  return React.useCallback(() => {
    onUnsetValue(layoutSystemConfigPropertyPaths(targetPath), false)
  }, [onUnsetValue, targetPath])
}

export const DeleteAllLayoutSystemConfigButton = React.memo(() => {
  const onDeleteAllConfig = useDeleteAllLayoutConfig()

  return (
    <SquareButton highlight onClick={onDeleteAllConfig}>
      <FunctionIcons.Remove />
    </SquareButton>
  )
})
