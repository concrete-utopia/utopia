import React from 'react'
import { useDispatch } from '../../../../editor/store/dispatch-context'
import { useRefEditorState } from '../../../../editor/store/store-hook'
import { FlexDirection } from '../../../common/css-utils'
import { updateFlexDirectionStrategies } from '../../../inspector-strategies/inspector-strategies'
import { executeFirstApplicableStrategy } from '../../../inspector-strategies/inspector-strategy'
import { FlexWrap } from 'utopia-api/core'
import { ControlStatus, ControlStyles, getControlStyles } from '../../../common/control-status'
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
    }
  })
  const dispatch = useDispatch()
  const updateFlexDirection = React.useCallback(
    (newFlexDirection: FlexDirection) => {
      executeFirstApplicableStrategy(
        dispatch,
        editorStateRef.current.metadata,
        editorStateRef.current.selectedViews,
        updateFlexDirectionStrategies(newFlexDirection),
      )
    },
    [dispatch, editorStateRef],
  )

  return (
    <>
      <UIGridRow tall padded={true} variant='<---1fr--->|------172px-------|'>
        <FlexDirectionControl
          value={flexDirection.value}
          controlStatus={flexDirection.controlStatus}
          controlStyles={flexDirection.controlStyles}
          onSubmitValue={updateFlexDirection}
          onUnset={flexDirection.onUnsetValues}
          flexWrap={flexWrap.value}
        />
        <FlexJustifyContentControl
          value={justifyContent.value}
          onSubmitValue={justifyContent.onSubmitValue}
          onUnset={justifyContent.onUnsetValues}
          controlStatus={justifyContent.controlStatus}
          controlStyles={justifyContent.controlStyles}
          flexDirection={flexDirection.value}
          justifyFlexStart={justifyFlexStart}
          justifyFlexEnd={justifyFlexEnd}
        />
      </UIGridRow>
      {unless(isFeatureEnabled('Nine block control'), <FlexGapControl />)}
      <FlexAlignItemsControl
        value={alignItems.value}
        controlStatus={alignItems.controlStatus}
        controlStyles={alignItems.controlStyles}
        onSubmitValue={alignItems.onSubmitValue}
        onUnset={alignItems.onUnsetValues}
        alignDirection={alignDirection}
        alignItemsFlexStart={alignItemsFlexStart}
        alignItemsFlexEnd={alignItemsFlexEnd}
      />
      <UIGridRow padded={true} variant='<---1fr--->|------172px-------|'>
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
      </UIGridRow>
    </>
  )
})
