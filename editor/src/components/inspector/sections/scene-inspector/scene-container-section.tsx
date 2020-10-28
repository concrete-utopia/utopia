import * as React from 'react'
import { betterReactMemo } from 'uuiui-deps'
import * as TP from '../../../../core/shared/template-path'
import * as PP from '../../../../core/shared/property-path'
import Utils from '../../../../utils/utils'
import { useWrappedEmptyOrUnknownOnSubmitValue, CheckboxInput } from '../../../../uuiui'
import { ControlStatus, ControlStyleDefaults, getControlStyles } from '../../common/control-status'
import { cssEmptyValues, layoutEmptyValues } from '../../common/css-utils'
import {
  useInspectorLayoutInfo,
  useInspectorStyleInfo,
  useInspectorInfoSimpleUntyped,
  InspectorInfo,
} from '../../common/property-path-hooks'
import { PropertyRow } from '../../widgets/property-row'
import {
  FlexAlignContentControl,
  FlexAlignItemsControl,
  FlexDirectionControl,
  FlexGapControl,
  FlexJustifyContentControl,
  FlexWrapControl,
  getDirectionAwareLabels,
} from '../layout-section/flex-container-subsection/flex-container-controls'
import { jsxAttributeValue, isJSXAttributeNotFound } from '../../../../core/shared/element-template'
import { useEditorState } from '../../../editor/store/store-hook'
import { MetadataUtils } from '../../../../core/model/element-metadata-utils'
import { isPercentPin } from 'utopia-api'
import { unsetSceneProp, setSceneProp } from '../../../editor/actions/actions'
import { createLayoutPropertyPath } from '../../../../core/layout/layout-helpers-new'
import {
  PathForResizeContent,
  isDynamicSceneChildWidthHeightPercentage,
  isSceneChildWidthHeightPercentage,
} from '../../../../core/model/scene-utils'
import { GridRow } from '../../widgets/grid-row'
import { WarningIcon } from '../../../../uuiui/warning-icon'
import { ChildWithPercentageSize } from '../../../common/size-warnings'
const simpleControlStatus: ControlStatus = 'simple'
const simpleControlStyles = getControlStyles(simpleControlStatus)

export const SceneFlexContainerSection = betterReactMemo('SceneFlexContainerSection', () => {
  const styleDisplayMetadata = useInspectorStyleInfo('display')
  const flexWrap = useInspectorLayoutInfo('flexWrap')
  const flexDirection = useInspectorLayoutInfo('flexDirection')
  const alignItems = useInspectorLayoutInfo('alignItems')
  const alignContent = useInspectorLayoutInfo('alignContent')
  const justifyContent = useInspectorLayoutInfo('justifyContent')
  const flexGapMain = useInspectorLayoutInfo('FlexGapMain')

  const wrappedFlexGapOnSubmitValue = useWrappedEmptyOrUnknownOnSubmitValue(
    flexGapMain.onSubmitValue,
    flexGapMain.onUnsetValues,
  )
  const wrappedFlexGapOnTransientSubmitValue = useWrappedEmptyOrUnknownOnSubmitValue(
    flexGapMain.onSubmitValue,
    flexGapMain.onUnsetValues,
  )

  if (styleDisplayMetadata.value === 'flex') {
    const flexDirectionValue = Utils.defaultIfNull(
      cssEmptyValues.flexDirection,
      flexDirection.value,
    )
    const alignItemsValue = Utils.defaultIfNull(cssEmptyValues.alignItems, alignItems.value)
    const flexWrapValue = Utils.defaultIfNull(cssEmptyValues.flexWrap, flexWrap.value)
    const justifyContentValue = Utils.defaultIfNull(
      cssEmptyValues.justifyContent,
      justifyContent.value,
    )
    const flexGapMainValue = Utils.defaultIfNull(layoutEmptyValues.gapMain, flexGapMain.value)
    const alignContentValue = Utils.defaultIfNull(cssEmptyValues.alignContent, alignContent.value)

    const {
      justifyFlexStart,
      justifyFlexEnd,
      alignDirection,
      alignItemsFlexStart,
      alignItemsFlexEnd,
      alignContentFlexStart,
      alignContentFlexEnd,
    } = getDirectionAwareLabels(flexWrapValue, flexDirectionValue)

    return (
      <>
        <PropertyRow>
          <FlexDirectionControl
            value={flexDirectionValue}
            controlStatus={simpleControlStatus}
            controlStyles={simpleControlStyles}
            onSubmitValue={flexDirection.onSubmitValue}
            onUnset={flexDirection.onUnsetValues}
            flexWrap={flexWrapValue}
          />
          <FlexAlignItemsControl
            value={alignItemsValue}
            controlStatus={simpleControlStatus}
            controlStyles={simpleControlStyles}
            onSubmitValue={alignItems.onSubmitValue}
            onUnset={alignItems.onUnsetValues}
            alignDirection={alignDirection}
            alignItemsFlexStart={alignItemsFlexStart}
            alignItemsFlexEnd={alignItemsFlexEnd}
          />
          <FlexWrapControl
            value={flexWrapValue}
            onSubmitValue={flexWrap.onSubmitValue}
            onUnset={flexWrap.onUnsetValues}
            controlStatus={simpleControlStatus}
            controlStyles={simpleControlStyles}
          />
          <FlexJustifyContentControl
            value={justifyContentValue}
            onSubmitValue={justifyContent.onSubmitValue}
            onUnset={justifyContent.onUnsetValues}
            controlStatus={simpleControlStatus}
            controlStyles={simpleControlStyles}
            flexDirection={flexDirectionValue}
            justifyFlexStart={justifyFlexStart}
            justifyFlexEnd={justifyFlexEnd}
          />
        </PropertyRow>
        <PropertyRow>
          <span>Gap</span>
          <FlexGapControl
            value={flexGapMainValue}
            onSubmitValue={wrappedFlexGapOnSubmitValue}
            onTransientSubmitValue={wrappedFlexGapOnTransientSubmitValue}
            onUnset={flexGapMain.onUnsetValues}
            controlStatus={simpleControlStatus}
            controlStyles={simpleControlStyles}
          />
        </PropertyRow>
        <div
          className='inspector-divider'
          style={{
            width: 'calc(100% - 32px)',
            height: 1,
            backgroundColor: ControlStyleDefaults.SetBorderColor,
            margin: '8px 16px 16px',
          }}
        />
        <PropertyRow style={{ marginTop: 16 }}>
          <FlexAlignContentControl
            value={alignContentValue}
            onSubmitValue={alignContent.onSubmitValue}
            onUnset={alignContent.onUnsetValues}
            controlStatus={simpleControlStatus}
            controlStyles={simpleControlStyles}
            alignDirection={alignDirection}
            alignContentFlexStart={alignContentFlexStart}
            alignContentFlexEnd={alignContentFlexEnd}
          />
        </PropertyRow>
      </>
    )
  } else {
    return null
  }
})

function useSceneType(): InspectorInfo<boolean> {
  return useInspectorInfoSimpleUntyped(
    [PathForResizeContent],
    (targets) => {
      const resizesContent = Utils.path(PP.getElements(PathForResizeContent), targets) ?? false
      if (isJSXAttributeNotFound(resizesContent)) {
        // OH MY GOD
        return false
      }
      return resizesContent
    },
    (resizesContent: unknown) => {
      const resizesConentBoolean = Boolean(resizesContent ?? false)
      return {
        [PP.toString(PathForResizeContent)]: jsxAttributeValue(resizesConentBoolean),
      }
    },
  )
}

function useIsSceneChildWidthHeightPercentage() {
  const { selectedViews, metadata } = useEditorState((state) => {
    return {
      selectedViews: state.editor.selectedViews,
      metadata: state.editor.jsxMetadataKILLME,
    }
  }, 'useIsSceneChildWidthHeightPercentage')

  const selectedScenePath = Utils.forceNotNull(
    'missing scene from scene section',
    selectedViews.find(TP.isScenePath),
  )
  const scene = MetadataUtils.findSceneByTemplatePath(metadata, selectedScenePath)
  if (scene != null) {
    return isSceneChildWidthHeightPercentage(scene)
  } else {
    return false
  }
}

export const SceneContainerSections = betterReactMemo('SceneContainerSections', () => {
  const { dispatch, metadata } = useEditorState(
    (store) => ({
      dispatch: store.dispatch,
      metadata: store.editor.jsxMetadataKILLME,
    }),
    'SceneContainerSections',
  )
  const selectedViews = useEditorState(
    (store) => store.editor.selectedViews,
    'SceneContainerSections selectedViews',
  )
  const selectedScene = Utils.forceNotNull(
    'Scene cannot be null in SceneContainerSection',
    React.useMemo(() => selectedViews.find(TP.isScenePath), [selectedViews]),
  )
  const scene = MetadataUtils.findSceneByTemplatePath(metadata, selectedScene)

  const sceneResizesContentInfo = useSceneType()
  let controlStatus: ControlStatus = simpleControlStatus
  const isDynamicSceneChildSizePercent = useIsSceneChildWidthHeightPercentage()
  if (isDynamicSceneChildSizePercent && !sceneResizesContentInfo.value) {
    controlStatus = 'disabled'
  }
  const onSubmitValue = React.useCallback(
    (event: React.ChangeEvent<HTMLInputElement>) => {
      const newSceneResizesContentValue = event.target.checked
      sceneResizesContentInfo.onSubmitValue(newSceneResizesContentValue)
      if (newSceneResizesContentValue === true) {
        dispatch(
          [
            unsetSceneProp(selectedScene, createLayoutPropertyPath('Width')),
            unsetSceneProp(selectedScene, createLayoutPropertyPath('Height')),
          ],
          'inspector',
        )
      } else if (scene != null) {
        const actions = [
          setSceneProp(
            selectedScene,
            createLayoutPropertyPath('Width'),
            jsxAttributeValue(scene.globalFrame?.width),
          ),
          setSceneProp(
            selectedScene,
            createLayoutPropertyPath('Height'),
            jsxAttributeValue(scene.globalFrame?.height),
          ),
        ]
        dispatch(actions, 'inspector')
      }
    },
    [dispatch, sceneResizesContentInfo, selectedScene, scene],
  )
  return (
    <>
      {!isDynamicSceneChildSizePercent ? null : (
        <GridRow padded type='<-auto-><----------1fr--------->'>
          <WarningIcon />
          <span style={{ whiteSpace: 'normal' }}>{ChildWithPercentageSize}</span>
        </GridRow>
      )}

      <GridRow padded type='<-auto-><----------1fr--------->'>
        <CheckboxInput
          id='resizeContentToggle'
          controlStatus={controlStatus}
          onChange={onSubmitValue}
          checked={sceneResizesContentInfo.value}
        />
        <label htmlFor='resizeContentToggle'>Resize Content</label>
      </GridRow>
      <SceneFlexContainerSection />
    </>
  )
})

SceneContainerSections.displayName = 'SceneContainerSections'
