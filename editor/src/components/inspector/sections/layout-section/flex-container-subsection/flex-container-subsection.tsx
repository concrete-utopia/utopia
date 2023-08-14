import React from 'react'
import { useDispatch } from '../../../../editor/store/dispatch-context'
import { useRefEditorState } from '../../../../editor/store/store-hook'
import type { FlexDirection } from '../../../common/css-utils'
import { updateFlexDirectionStrategies } from '../../../inspector-strategies/inspector-strategies'
import { executeFirstApplicableStrategy } from '../../../inspector-strategies/inspector-strategy'
import { FlexWrap } from 'utopia-api/core'
import type { ControlStatus, ControlStyles } from '../../../common/control-status'
import { getControlStyles } from '../../../common/control-status'
import { useInspectorLayoutInfo } from '../../../common/property-path-hooks'
import { UIGridRow } from '../../../widgets/ui-grid-row'
import {
  FlexAlignContentControl,
  FlexAlignItemsControl,
  FlexGapControl,
  FlexWrapControl,
  FlexJustifyContentControl,
  FlexDirectionControl,
  getDirectionAwareLabels,
} from './flex-container-controls'
import { unless } from '../../../../../utils/react-conditionals'
import { isFeatureEnabled } from '../../../../../utils/feature-switches'

export const FlexContainerControls = React.memo<{ seeMoreVisible: boolean }>((props) => {
  // Right now flex layout isn't supported on groups, so just don't show the controls if a group is selected
  const flexWrap = useInspectorLayoutInfo('flexWrap')
  const flexDirection = useInspectorLayoutInfo('flexDirection')
  const alignItems = useInspectorLayoutInfo('alignItems')
  const alignContent = useInspectorLayoutInfo('alignContent')
  const justifyContent = useInspectorLayoutInfo('justifyContent')

  const {
    justifyFlexStart,
    justifyFlexEnd,
    alignDirection,
    alignItemsFlexStart,
    alignItemsFlexEnd,
    alignContentFlexStart,
    alignContentFlexEnd,
  } = getDirectionAwareLabels(flexWrap.value, flexDirection.value)

  const alignItemsControlStatus: ControlStatus =
    flexWrap.value === FlexWrap.NoWrap ? 'disabled' : alignItems.controlStatus
  const alignItemsControlStyles: ControlStyles =
    flexWrap.value === FlexWrap.NoWrap ? getControlStyles('disabled') : alignItems.controlStyles

  const editorStateRef = useRefEditorState((store) => {
    return {
      metadata: store.editor.jsxMetadata,
      selectedViews: store.editor.selectedViews,
      elementPathTree: store.editor.elementPathTree,
      allElementProps: store.editor.allElementProps,
    }
  })
  const dispatch = useDispatch()
  const updateFlexDirection = React.useCallback(
    (newFlexDirection: FlexDirection) => {
      executeFirstApplicableStrategy(
        dispatch,
        editorStateRef.current.metadata,
        editorStateRef.current.selectedViews,
        editorStateRef.current.elementPathTree,
        editorStateRef.current.allElementProps,
        updateFlexDirectionStrategies(newFlexDirection),
      )
    },
    [dispatch, editorStateRef],
  )

  return (
    <>
      {/* <UIGridRow
        padded={true}
        variant='<-auto-><----------1fr--------->'
        style={{ padding: '0 16px' }}
      >
        <FlexWrapControl
          value={flexWrap.value}
          onSubmitValue={flexWrap.onSubmitValue}
          onUnset={flexWrap.onUnsetValues}
          controlStatus={flexWrap.controlStatus}
          controlStyles={flexWrap.controlStyles}
        />
        <FlexAlignContentControl
          value={alignContent.value}
          onSubmitValue={alignContent.onSubmitValue}
          onUnset={alignContent.onUnsetValues}
          controlStatus={alignItemsControlStatus}
          controlStyles={alignItemsControlStyles}
          alignDirection={alignDirection}
          alignContentFlexStart={alignContentFlexStart}
          alignContentFlexEnd={alignContentFlexEnd}
        />
      </UIGridRow> */}
      {/* <UIGridRow padded variant='|--67px--|<--------1fr-------->'> */}
      <FlexWrapControl
        value={flexWrap.value}
        onSubmitValue={flexWrap.onSubmitValue}
        onUnset={flexWrap.onUnsetValues}
        controlStatus={flexWrap.controlStatus}
        controlStyles={flexWrap.controlStyles}
      />
      {/* </UIGridRow> */}
    </>
  )
})
