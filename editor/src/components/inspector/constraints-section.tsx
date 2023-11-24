/** @jsxRuntime classic */
/** @jsx jsx */
import { jsx } from '@emotion/react'

import React from 'react'
import { useContextSelector } from 'use-context-selector'
import {
  isHorizontalLayoutPinnedProp,
  type LayoutPinnedPropIncludingCenter,
} from '../../core/layout/layout-helpers-new'
import { NO_OP } from '../../core/shared/utils'
import { when } from '../../utils/react-conditionals'
import {
  FlexColumn,
  FlexRow,
  InspectorSubsectionHeader,
  PopupList,
  UtopiaTheme,
  useColorTheme,
} from '../../uuiui'
import type { SelectOption } from '../../uuiui-deps'
import { getControlStyles } from '../../uuiui-deps'
import { InspectorRowHoverCSS } from '../context-menu-wrapper'
import { useDispatch } from '../editor/store/dispatch-context'
import { Substores, useEditorState, useRefEditorState } from '../editor/store/store-hook'
import type { FramePinsInfo } from './common/layout-property-path-hooks'
import { InspectorPropsContext } from './common/property-path-hooks'
import { CombinedPinControl, PinControl } from './controls/pin-control'
import {
  allElementsAreGroupChildren,
  allElementsArePositionedAbsolutelySelector,
  anySelectedElementGroupOrChildOfGroup,
} from './fill-hug-fixed-control'
import { selectedViewsSelector } from './inpector-selectors'
import type { DetectedPins, RequestedPins } from './simplified-pinning-helpers'
import {
  DetectedFrameChildHorizontalPinChangeOptions,
  DetectedFrameChildVerticalPinChangeOptions,
  DetectedGroupChildHorizontalPinChangeOptions,
  DetectedGroupChildVerticalPinChangeOptions,
  FrameChildHorizontalPinChangeOptions,
  FrameChildVerticalPinChangeOptions,
  GroupChildHorizontalPinChangeOptions,
  GroupChildVerticalPinChangeOptions,
  getConstraintAndFrameChangeActionsForGroupChild,
  getFixedPointsForPinning,
  getFrameChangeActionsForFrameChild,
  useDetectedConstraints,
} from './simplified-pinning-helpers'
import { UIGridRow } from './widgets/ui-grid-row'
import { allSelectedElementsContractSelector } from './editor-contract-section'

export const InspectorSectionConstraintsTestId = 'inspector-section-constraints'

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
  const allElementsArePositionedAbsolutely = useEditorState(
    Substores.metadata,
    allElementsArePositionedAbsolutelySelector,
    'ConstraintsSection allElementsArePositionedAbsolutely',
  )

  const selectedElementContract = useEditorState(
    Substores.metadata,
    allSelectedElementsContractSelector,
    'EditorContractDropdown selectedElementContract',
  )

  const showSection = React.useMemo(() => {
    return (
      (selectedElementContract === 'frame' || selectedElementContract === 'group') &&
      allElementsArePositionedAbsolutely &&
      (noGroupOrGroupChildrenSelected || onlyGroupChildrenSelected)
    )
  }, [
    selectedElementContract,
    noGroupOrGroupChildrenSelected,
    onlyGroupChildrenSelected,
    allElementsArePositionedAbsolutely,
  ])

  if (!showSection) {
    return null
  }

  return (
    <React.Fragment>
      <InspectorSubsectionHeader>
        <FlexRow
          data-testid={InspectorSectionConstraintsTestId}
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
ConstraintsSection.displayName = 'ConstraintsSection'

const GroupChildConstraintsSection = React.memo(() => {
  const pins = useDetectedConstraints('group-child')
  const framePinsInfo: FramePinsInfo = React.useMemo(() => getFixedPointsForPinning(pins), [pins])

  return (
    <FlexColumn css={{ paddingBottom: UtopiaTheme.layout.rowHorizontalPadding }}>
      <UIGridRow padded variant='<-auto-><----------1fr--------->'>
        <ChildPinControl isGroupChild='group-child' pins={pins} framePinsInfo={framePinsInfo} />
        <FlexColumn css={{ gap: 8 }}>
          <ChildConstraintSelect isGroupChild='group-child' dimension={'width'} />
          <ChildConstraintSelect isGroupChild='group-child' dimension={'height'} />
        </FlexColumn>
      </UIGridRow>
    </FlexColumn>
  )
})
GroupChildConstraintsSection.displayName = 'GroupChildConstraintsSection'

const FrameChildConstraintsSection = React.memo(() => {
  const pins = useDetectedConstraints('frame-child')
  const framePinsInfo: FramePinsInfo = React.useMemo(() => getFixedPointsForPinning(pins), [pins])

  return (
    <FlexColumn css={{ paddingBottom: UtopiaTheme.layout.rowHorizontalPadding }}>
      <UIGridRow padded variant='<-auto-><----------1fr--------->'>
        <ChildPinControl isGroupChild='frame-child' pins={pins} framePinsInfo={framePinsInfo} />
        <FlexColumn css={{ gap: 8 }}>
          <ChildConstraintSelect isGroupChild='frame-child' dimension={'width'} />
          <ChildConstraintSelect isGroupChild='frame-child' dimension={'height'} />
        </FlexColumn>
      </UIGridRow>
    </FlexColumn>
  )
})
FrameChildConstraintsSection.displayName = 'FrameChildConstraintsSection'

export const ChildPinControl = React.memo(
  ({
    isGroupChild,
    pins,
    framePinsInfo,
  }: {
    isGroupChild: 'group-child' | 'frame-child'
    pins: DetectedPins
    framePinsInfo: FramePinsInfo
  }) => {
    const dispatch = useDispatch()

    const [activeHorizontalPins, setActiveHorizontalPins] = React.useState<
      Array<LayoutPinnedPropIncludingCenter>
    >([]) // TODO reset on selectedView change
    const [activeVerticalPins, setActiveVerticalPins] = React.useState<
      Array<LayoutPinnedPropIncludingCenter>
    >([]) // TODO reset on selectedView change

    const propertyTarget = useContextSelector(InspectorPropsContext, (contextData) => {
      return contextData.targetPath
    })

    const selectedViewsRef = useRefEditorState(selectedViewsSelector)
    const metadataRef = useRefEditorState((store) => store.editor.jsxMetadata)
    const allElementPropsRef = useRefEditorState((store) => store.editor.allElementProps)
    const elementPathTreesRef = useRefEditorState((store) => store.editor.elementPathTree)

    const requestedGroupPinChanges = React.useCallback(
      (frameProp: LayoutPinnedPropIncludingCenter): RequestedPins | 'no-op' => {
        if (frameProp === 'centerX' || isHorizontalLayoutPinnedProp(frameProp)) {
          const newActivePins = (() => {
            if (frameProp === 'centerX') {
              // clicking on scale-horizontal resets the array
              return []
            }
            const pinIndex = activeHorizontalPins.indexOf(frameProp)
            if (pinIndex === -1) {
              const updatedActivePins = [...activeHorizontalPins, frameProp]

              // If more than two of 'left', 'width', 'right' are active, discard the oldest one
              if (
                updatedActivePins.includes('left') &&
                updatedActivePins.includes('width') &&
                updatedActivePins.includes('right')
              ) {
                updatedActivePins.shift()
              }
              return updatedActivePins
            } else {
              // Pin is active, deactivate it
              const updatedHorizontalPins = [...activeHorizontalPins]
              updatedHorizontalPins.splice(pinIndex, 1)
              return updatedHorizontalPins
            }
          })()

          setActiveHorizontalPins(newActivePins)

          const hasLeft = newActivePins.includes('left')
          const hasWidth = newActivePins.includes('width')
          const hasRight = newActivePins.includes('right')
          const isEmpty = newActivePins.length === 0

          if (isEmpty) return 'scale-horizontal'
          if (hasLeft && hasRight) return 'left-and-right'
          if (hasLeft && hasWidth) return 'left-and-width'
          if (hasRight && hasWidth) return 'right-and-width'
          if (hasLeft) return 'left'
          if (hasRight) return 'right'
          if (hasWidth) return 'width'
          return 'no-op'
        } else {
          const newActivePins = (() => {
            if (frameProp === 'centerY') {
              // clicking on scale-vertical resets the array
              return []
            }
            const pinIndex = activeVerticalPins.indexOf(frameProp)
            if (pinIndex === -1) {
              const updatedActivePins = [...activeVerticalPins, frameProp]

              // If more than two of 'top', 'height', 'bottom' are active, discard the oldest one
              if (
                updatedActivePins.includes('top') &&
                updatedActivePins.includes('height') &&
                updatedActivePins.includes('bottom')
              ) {
                updatedActivePins.shift()
              }
              return updatedActivePins
            } else {
              // Pin is active, deactivate it
              const updatedVerticalPins = [...activeVerticalPins]
              updatedVerticalPins.splice(pinIndex, 1)
              return updatedVerticalPins
            }
          })()

          setActiveVerticalPins(newActivePins)

          const hasTop = newActivePins.includes('top')
          const hasHeight = newActivePins.includes('height')
          const hasBottom = newActivePins.includes('bottom')
          const isEmpty = newActivePins.length === 0

          if (isEmpty) return 'scale-vertical'
          if (hasTop && hasBottom) return 'top-and-bottom'
          if (hasTop && hasHeight) return 'top-and-height'
          if (hasBottom && hasHeight) return 'bottom-and-height'
          if (hasTop) return 'top'
          if (hasBottom) return 'bottom'
          if (hasHeight) return 'height'
          return 'no-op'
        }
      },
      [activeHorizontalPins, setActiveHorizontalPins, activeVerticalPins, setActiveVerticalPins],
    )

    const onPinControlMouseDown = React.useCallback(
      (
        frameProp: LayoutPinnedPropIncludingCenter,
        event: React.MouseEvent<Element, MouseEvent>,
      ) => {
        const cmdPressed = event.metaKey
        const requestedPinChange: RequestedPins | 'no-op' = (() => {
          if (isGroupChild === 'group-child') {
            return requestedGroupPinChanges(frameProp)
          }

          switch (frameProp) {
            case 'left': {
              if (cmdPressed && pins.horizontal === 'right-and-width') {
                return 'left-and-right'
              } else {
                return 'left-and-width'
              }
            }
            case 'right': {
              if (cmdPressed && pins.horizontal === 'left-and-width') {
                return 'left-and-right'
              } else {
                return 'right-and-width'
              }
            }
            case 'width': {
              if (pins.horizontal.includes('width')) {
                return 'no-op' // if Height is already pressed, we leave it as-is
              }
              return 'left-and-width'
            }
            case 'top': {
              if (cmdPressed && pins.vertical === 'bottom-and-height') {
                return 'top-and-bottom'
              } else {
                return 'top-and-height'
              }
            }
            case 'bottom': {
              if (cmdPressed && pins.vertical === 'top-and-height') {
                return 'top-and-bottom'
              } else {
                return 'bottom-and-height'
              }
            }
            case 'height': {
              if (pins.vertical.includes('height')) {
                return 'no-op' // if Height is already pressed, we leave it as-is
              }
              return 'top-and-height'
            }
            case 'centerX': {
              if (cmdPressed) {
                return 'scale-horizontal'
              } else {
                return 'no-op'
              }
            }
            case 'centerY': {
              if (cmdPressed) {
                return 'scale-vertical'
              } else {
                return 'no-op'
              }
            }
            default:
              const _exhaustiveCheck: never = frameProp
              throw new Error(`Unhandled frameProp: ${_exhaustiveCheck}`)
          }
        })()

        if (requestedPinChange === 'no-op') {
          // no-op, early return :)
          return
        }

        dispatch(
          isGroupChild === 'group-child'
            ? getConstraintAndFrameChangeActionsForGroupChild(
                metadataRef.current,
                allElementPropsRef.current,
                elementPathTreesRef.current,
                propertyTarget,
                selectedViewsRef.current,
                requestedPinChange,
              )
            : getFrameChangeActionsForFrameChild(
                metadataRef.current,
                elementPathTreesRef.current,
                propertyTarget,
                selectedViewsRef.current,
                requestedPinChange,
              ),
        )
      },
      [
        dispatch,
        isGroupChild,
        metadataRef,
        elementPathTreesRef,
        propertyTarget,
        selectedViewsRef,
        pins.horizontal,
        pins.vertical,
        allElementPropsRef,
        requestedGroupPinChanges,
      ],
    )

    return (
      <CombinedPinControl
        handlePinMouseDown={onPinControlMouseDown}
        pins={pins}
        framePinsInfo={framePinsInfo}
      />
    )
  },
)
ChildPinControl.displayName = 'ChildPinControl'

const ChildConstraintSelect = React.memo(
  (props: { dimension: 'width' | 'height'; isGroupChild: 'group-child' | 'frame-child' }) => {
    const { dimension, isGroupChild } = props

    const dispatch = useDispatch()

    const propertyTarget = useContextSelector(InspectorPropsContext, (contextData) => {
      return contextData.targetPath
    })

    const editorRef = useRefEditorState((store) => ({
      selectedViews: store.editor.selectedViews,
      metadata: store.editor.jsxMetadata,
      allElementProps: store.editor.allElementProps,
      elementPathTrees: store.editor.elementPathTree,
    }))

    const pins = useDetectedConstraints(isGroupChild)

    const optionsToUse = (() => {
      if (dimension === 'width') {
        if (isGroupChild === 'frame-child') {
          return Object.values(FrameChildHorizontalPinChangeOptions)
        } else {
          return Object.values(GroupChildHorizontalPinChangeOptions)
        }
      } else {
        if (isGroupChild === 'frame-child') {
          return Object.values(FrameChildVerticalPinChangeOptions)
        } else {
          return Object.values(GroupChildVerticalPinChangeOptions)
        }
      }
    })()

    const activeOption = (() => {
      if (dimension === 'width') {
        if (isGroupChild === 'frame-child') {
          return DetectedFrameChildHorizontalPinChangeOptions[pins.horizontal]
        } else {
          return DetectedGroupChildHorizontalPinChangeOptions[pins.horizontal]
        }
      } else {
        if (isGroupChild === 'frame-child') {
          return DetectedFrameChildVerticalPinChangeOptions[pins.vertical]
        } else {
          return DetectedGroupChildVerticalPinChangeOptions[pins.vertical]
        }
      }
    })()

    const onSubmit = React.useCallback(
      (option: SelectOption) => {
        const requestedPins: RequestedPins = option.value
        if (activeOption.label === option.label) {
          // using the same *label* as a noop to ensure consistency between the different dispatches
          return
        }
        dispatch(
          isGroupChild === 'group-child'
            ? getConstraintAndFrameChangeActionsForGroupChild(
                editorRef.current.metadata,
                editorRef.current.allElementProps,
                editorRef.current.elementPathTrees,
                propertyTarget,
                editorRef.current.selectedViews,
                requestedPins,
              )
            : getFrameChangeActionsForFrameChild(
                editorRef.current.metadata,
                editorRef.current.elementPathTrees,
                propertyTarget,
                editorRef.current.selectedViews,
                requestedPins,
              ),
        )
      },
      [dispatch, propertyTarget, editorRef, isGroupChild, activeOption],
    )

    const styles = React.useMemo(() => {
      return getControlStyles('simple')
    }, [])

    const colorTheme = useColorTheme()

    return (
      <PopupList
        id={`frame-child-constraint-${dimension}`}
        onSubmitValue={onSubmit}
        value={activeOption}
        options={optionsToUse}
        style={{
          position: 'relative',
        }}
        controlStyles={{
          ...styles,
          mainColor: activeOption.invalid ? colorTheme.error.value : styles.mainColor,
        }}
      />
    )
  },
)
