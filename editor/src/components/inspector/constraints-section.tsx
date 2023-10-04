/** @jsxRuntime classic */
/** @jsx jsx */
import { jsx } from '@emotion/react'

import React from 'react'
import { useContextSelector } from 'use-context-selector'
import {
  HorizontalLayoutPinnedProps,
  VerticalLayoutPinnedProps,
  type LayoutPinnedProp,
} from '../../core/layout/layout-helpers-new'
import { MetadataUtils } from '../../core/model/element-metadata-utils'
import type { ElementInstanceMetadataMap } from '../../core/shared/element-template'
import { emptyComments, jsExpressionValue } from '../../core/shared/element-template'
import { nullIfInfinity } from '../../core/shared/math-utils'
import type { ElementPath } from '../../core/shared/project-file-types'
import * as PP from '../../core/shared/property-path'
import { NO_OP, assertNever } from '../../core/shared/utils'
import invariant from '../../third-party/remix/invariant'
import { when } from '../../utils/react-conditionals'
import { FlexColumn, FlexRow, InspectorSubsectionHeader, PopupList, UtopiaTheme } from '../../uuiui'
import type { SelectOption } from '../../uuiui-deps'
import { getControlStyles } from '../../uuiui-deps'
import { valueToUseForPin } from '../canvas/canvas-utils'
import { InspectorRowHoverCSS } from '../context-menu-wrapper'
import type { SetProp, UnsetProperty } from '../editor/action-types'
import { setProp_UNSAFE, unsetProperty } from '../editor/actions/action-creators'
import { useDispatch } from '../editor/store/dispatch-context'
import { Substores, useEditorState, useRefEditorState } from '../editor/store/store-hook'
import { getFullFrame } from '../frame'
import type { FramePinsInfo } from './common/layout-property-path-hooks'
import { InspectorPropsContext } from './common/property-path-hooks'
import { PinControl } from './controls/pin-control'
import {
  GroupChildPinControl,
  GroupConstraintSelect,
  allElementsAreGroupChildren,
  anySelectedElementGroupOrChildOfGroup,
} from './fill-hug-fixed-control'
import { selectedViewsSelector } from './inpector-selectors'
import { PinHeightSVG, PinWidthSVG } from './utility-controls/pin-control'
import { UIGridRow } from './widgets/ui-grid-row'
import { getFramePointsFromMetadata, getFramePointsFromMetadataTypeFixed } from './inspector-common'
import * as EP from '../../core/shared/element-path'
import { isCSSNumber, isCssNumberAndFixedSize, isCssNumberAndPercentage } from './common/css-utils'

export const ConstraintsSection = React.memo(() => {
  const noGroupOrGroupChildrenSelected = !useEditorState(
    Substores.metadata,
    anySelectedElementGroupOrChildOfGroup,
    'ConstraintsSection someGroupOrGroupChildrenSelected',
  )
  const onlyGroupChildrenSelected = useEditorState(
    Substores.metadata,
    allElementsAreGroupChildren,
    'ConstraintsSection onlyGroupChildrenSelected',
  )

  return (
    <React.Fragment>
      <InspectorSubsectionHeader>
        <FlexRow
          style={{
            flexGrow: 1,
            height: 42,
          }}
        >
          <span style={{ flex: 1 }}>Constraints</span>
        </FlexRow>
      </InspectorSubsectionHeader>
      {when(noGroupOrGroupChildrenSelected, <FrameChildConstraintsSection />)}
      {when(onlyGroupChildrenSelected, <GroupChildConstraintsSection />)}
    </React.Fragment>
  )
})

const GroupChildConstraintsSection = React.memo(() => {
  return (
    <FlexColumn css={{ paddingBottom: UtopiaTheme.layout.rowHorizontalPadding }}>
      <UIGridRow padded variant='<-auto-><----------1fr--------->'>
        <GroupChildPinControl />
        <FlexColumn css={{ gap: 0 }}>
          <FlexRow css={InspectorRowHoverCSS}>
            <PinWidthSVG />
            <GroupConstraintSelect dimension={'width'} />
          </FlexRow>
          <FlexRow css={InspectorRowHoverCSS}>
            <PinHeightSVG />
            <GroupConstraintSelect dimension={'height'} />
          </FlexRow>
        </FlexColumn>
      </UIGridRow>
    </FlexColumn>
  )
})

const FrameChildConstraintsSection = React.memo(() => {
  return (
    <FlexColumn css={{ paddingBottom: UtopiaTheme.layout.rowHorizontalPadding }}>
      <UIGridRow padded variant='<-auto-><----------1fr--------->'>
        <FrameChildPinControl />{' '}
        <FlexColumn css={{ gap: 0 }}>
          <FlexRow css={InspectorRowHoverCSS}>
            <PinWidthSVG />
            <FrameChildConstraintSelect dimension={'width'} />
          </FlexRow>
          <FlexRow css={InspectorRowHoverCSS}>
            <PinHeightSVG />
            <FrameChildConstraintSelect dimension={'height'} />
          </FlexRow>
        </FlexColumn>
      </UIGridRow>
    </FlexColumn>
  )
})

export const FrameChildPinControl = React.memo(() => {
  const dispatch = useDispatch()

  const propertyTarget = useContextSelector(InspectorPropsContext, (contextData) => {
    return contextData.targetPath
  })

  const selectedViewsRef = useRefEditorState(selectedViewsSelector)
  const allElementPropsRef = useRefEditorState((store) => store.editor.allElementProps)

  const onPinControlMouseDown = React.useCallback(
    (frameProp: LayoutPinnedProp, event: React.MouseEvent<Element, MouseEvent>) => {},
    [],
  )

  const framePoints: FramePinsInfo = React.useMemo(() => {
    const ignore = { isPrimaryPosition: false, isRelativePosition: false }
    return {
      left: {
        isPrimaryPosition: false,
        isRelativePosition: false,
      },
      top: {
        isPrimaryPosition: false,
        isRelativePosition: false,
      },
      bottom: {
        isPrimaryPosition: false,
        isRelativePosition: false,
      },
      right: {
        isPrimaryPosition: false,
        isRelativePosition: false,
      },
      width: ignore,
      height: ignore,
      centerX: ignore,
      centerY: ignore,
    }
  }, [])

  return (
    <PinControl
      handlePinMouseDown={NO_OP}
      framePoints={framePoints}
      controlStatus='simple'
      exclude={{ center: true }}
      name='group-child-controls'
    />
  )
})
GroupChildPinControl.displayName = 'GroupChildPinControl'

export const FrameChildConstraintSelect = React.memo((props: { dimension: 'width' | 'height' }) => {
  const { dimension } = props

  const dispatch = useDispatch()

  const propertyTarget = useContextSelector(InspectorPropsContext, (contextData) => {
    return contextData.targetPath
  })

  const editorRef = useRefEditorState((store) => ({
    selectedViews: store.editor.selectedViews,
    metadata: store.editor.jsxMetadata,
  }))

  const pins = useEditorState(
    Substores.metadata,
    (store) => detectPinsSet(store.editor.jsxMetadata, store.editor.selectedViews[0]),
    'FrameChildConstraintSelect pins',
  )

  const optionsToUse =
    dimension === 'width'
      ? Object.values(HorizontalPinChangeOptions)
      : Object.values(VerticalPinChangeOptions)

  const activeOption =
    dimension === 'width'
      ? HorizontalPinChangeOptionsIncludingMixed[pins.horizontal]
      : VerticalPinChangeOptionsIncludingMixed[pins.vertical]

  return (
    <PopupList
      id={`frame-child-constraint-${dimension}`}
      // eslint-disable-next-line react/jsx-no-bind
      onSubmitValue={(option) => {
        const requestedPins: RequestedPins = option.value
        dispatch(
          getFrameChangeActions(
            editorRef.current.metadata,
            propertyTarget,
            editorRef.current.selectedViews,
            requestedPins,
          ),
        )
      }}
      value={activeOption}
      options={optionsToUse}
      style={{
        position: 'relative',
      }}
      controlStyles={getControlStyles('simple')}
    />
  )
})

function detectPinsSet(
  metadata: ElementInstanceMetadataMap,
  target: ElementPath,
): { horizontal: HorizontalPinRequests | 'mixed'; vertical: VerticalPinRequests | 'mixed' } {
  const element = MetadataUtils.findElementByElementPath(metadata, target)
  if (element == null) {
    return { horizontal: 'mixed', vertical: 'mixed' }
  }

  const framePoints = getFramePointsFromMetadataTypeFixed(element)

  const horizontalPins: HorizontalPinRequests | 'mixed' = (() => {
    if (
      isCssNumberAndFixedSize(framePoints.left) &&
      framePoints.right == null &&
      isCssNumberAndFixedSize(framePoints.width)
    ) {
      return 'left-and-width'
    }
    if (
      framePoints.left == null &&
      isCssNumberAndFixedSize(framePoints.right) &&
      isCssNumberAndFixedSize(framePoints.width)
    ) {
      return 'right-and-width'
    }
    if (
      isCssNumberAndFixedSize(framePoints.left) &&
      isCssNumberAndFixedSize(framePoints.right) &&
      framePoints.width == null
    ) {
      return 'left-and-right'
    }
    if (
      isCssNumberAndPercentage(framePoints.left) &&
      framePoints.right == null &&
      isCssNumberAndPercentage(framePoints.width)
    ) {
      return 'scale-horizontal'
    }
    return 'mixed'
  })()

  const verticalPins: VerticalPinRequests | 'mixed' = (() => {
    if (
      isCssNumberAndFixedSize(framePoints.top) &&
      framePoints.bottom == null &&
      isCssNumberAndFixedSize(framePoints.height)
    ) {
      return 'top-and-height'
    }
    if (
      framePoints.top == null &&
      isCssNumberAndFixedSize(framePoints.bottom) &&
      isCssNumberAndFixedSize(framePoints.height)
    ) {
      return 'bottom-and-height'
    }
    if (
      isCssNumberAndFixedSize(framePoints.top) &&
      isCssNumberAndFixedSize(framePoints.bottom) &&
      framePoints.height == null
    ) {
      return 'top-and-bottom'
    }
    if (
      isCssNumberAndPercentage(framePoints.top) &&
      framePoints.bottom == null &&
      isCssNumberAndPercentage(framePoints.height)
    ) {
      return 'scale-vertical'
    }
    return 'mixed'
  })()

  return { horizontal: horizontalPins, vertical: verticalPins }
}

type HorizontalPinRequests =
  | 'left-and-width'
  | 'right-and-width'
  | 'left-and-right'
  | 'scale-horizontal'

type VerticalPinRequests =
  | 'top-and-height'
  | 'bottom-and-height'
  | 'top-and-bottom'
  | 'scale-vertical'

type RequestedPins = HorizontalPinRequests | VerticalPinRequests

const HorizontalPinChangeOptions: {
  [key in HorizontalPinRequests]: SelectOption & { value: HorizontalPinRequests }
} = {
  'left-and-width': {
    value: 'left-and-width',
    label: 'Left',
  },
  'right-and-width': {
    value: 'right-and-width',
    label: 'Right',
  },
  'left-and-right': {
    value: 'left-and-right',
    label: 'Left and Right',
  },
  'scale-horizontal': {
    value: 'scale-horizontal',
    label: 'Scale',
  },
} as const

const HorizontalPinChangeOptionsIncludingMixed = {
  ...HorizontalPinChangeOptions,
  mixed: {
    value: 'mixed',
    label: 'Mixed',
  },
} as const

const VerticalPinChangeOptions: {
  [key in VerticalPinRequests]: SelectOption & { value: VerticalPinRequests }
} = {
  'top-and-height': {
    value: 'top-and-height',
    label: 'Top',
  },
  'bottom-and-height': {
    value: 'bottom-and-height',
    label: 'Bottom',
  },
  'top-and-bottom': {
    value: 'top-and-bottom',
    label: 'Top and Bottom',
  },
  'scale-vertical': {
    value: 'scale-vertical',
    label: 'Scale',
  },
} as const

const VerticalPinChangeOptionsIncludingMixed = {
  ...VerticalPinChangeOptions,
  mixed: {
    value: 'mixed',
    label: 'Mixed',
  },
} as const

function getFrameChangeActions(
  metadata: ElementInstanceMetadataMap,
  propertyTarget: ReadonlyArray<string>,
  targets: Array<ElementPath>,
  requestedPins: RequestedPins,
): Array<SetProp | UnsetProperty> {
  const pinChange = getPinChanges(metadata, propertyTarget, targets)
  switch (requestedPins) {
    case 'left-and-width':
      return pinChange(['left', 'width'], 'horizontal', 'px')
    case 'right-and-width':
      return pinChange(['right', 'width'], 'horizontal', 'px')
    case 'left-and-right':
      return pinChange(['left', 'right'], 'horizontal', 'px')
    case 'scale-horizontal':
      return pinChange(['left', 'width'], 'horizontal', '%')

    case 'top-and-height':
      return pinChange(['top', 'height'], 'vertical', 'px')
    case 'bottom-and-height':
      return pinChange(['bottom', 'height'], 'vertical', 'px')
    case 'top-and-bottom':
      return pinChange(['top', 'bottom'], 'vertical', 'px')
    case 'scale-vertical':
      return pinChange(['top', 'height'], 'vertical', '%')

    default:
      assertNever(requestedPins)
  }
}

const getPinChanges =
  (
    metadata: ElementInstanceMetadataMap,
    propertyTarget: ReadonlyArray<string>,
    targets: Array<ElementPath>,
  ) =>
  (
    pinsToSet: Array<LayoutPinnedProp>,
    horizontal: 'horizontal' | 'vertical',
    setAsPercentage: 'px' | '%',
  ): Array<SetProp | UnsetProperty> => {
    const pinsToUnset =
      horizontal === 'horizontal' ? HorizontalLayoutPinnedProps : VerticalLayoutPinnedProps

    const unsetActions: Array<UnsetProperty> = targets.flatMap((target) =>
      pinsToUnset.map((pin) => unsetProperty(target, PP.create(...propertyTarget, pin))),
    )

    const setActions: Array<SetProp> = targets.flatMap((target) => {
      const localFrame = nullIfInfinity(
        MetadataUtils.findElementByElementPath(metadata, target)?.localFrame,
      )
      const coordinateSystemBounds = MetadataUtils.findElementByElementPath(metadata, target)
        ?.specialSizeMeasurements.coordinateSystemBounds

      invariant(localFrame != null, 'LocalFrame should never be null')
      invariant(coordinateSystemBounds != null, 'coordinateSystemBounds should never be null')

      const fullFrame = getFullFrame(localFrame)

      return pinsToSet.map((pin) =>
        setProp_UNSAFE(
          target,
          PP.create(...propertyTarget, pin),
          jsExpressionValue(
            valueToUseForPin(pin, fullFrame[pin], setAsPercentage === '%', coordinateSystemBounds),
            emptyComments,
          ),
        ),
      )
    })

    return [...unsetActions, ...setActions]
  }
