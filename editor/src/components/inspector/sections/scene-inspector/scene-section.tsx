import * as React from 'react'
import * as TP from '../../../../core/shared/template-path'
import * as PP from '../../../../core/shared/property-path'
import {
  InspectorSectionHeader,
  InspectorSubsectionHeader,
  useWrappedEmptyOnSubmitValue,
  NumberInput,
  PopupList,
} from 'uuiui'
import { SelectControl, SelectOption } from '../../controls/select-control'
import { ControlStatus, getControlStyles } from '../../widgets/control-status'
import { PropertyRow } from '../../widgets/property-row'
import { useEditorState } from '../../../editor/store/store-hook'
import {
  getOpenUtopiaJSXComponentsFromState,
  getOpenFilename,
} from '../../../editor/store/editor-state'
import { NormalisedFrame, FramePoint } from 'utopia-api'
import { SimpleNumberInput } from 'uuiui'
import { ComponentSection } from '../component-section/component-section'
import { SceneContainerSections } from './scene-container-section'
import { betterReactMemo } from 'uuiui-deps'
import { pinnedPropForFramePoint } from '../../../../core/layout/layout-helpers-new'
import {
  EmptyInputValue,
  fallbackOnEmptyInputValueToCSSEmptyValue,
  CSSNumber,
} from '../../new-inspector/css-utils'
import { pinLabels } from '../layout-section/self-layout-subsection/gigantic-size-pins-subsection'
import {
  useInspectorLayoutInfo,
  InspectorInfo,
  useInspectorMetadataInfo,
  useInspectorInfoSimpleUntyped,
  useKeepReferenceEqualityIfPossible,
} from '../../new-inspector/new-inspector-hooks'
import {
  PathForSceneComponent,
  BakedInStoryboardVariableName,
} from '../../../../core/model/scene-utils'
import {
  jsxAttributeOtherJavaScript,
  isJSXAttributeOtherJavaScript,
} from '../../../../core/shared/element-template'
import { getWarningIconProps } from '../../../../uuiui/warning-icon'
import {
  defaultPropertiesForComponentInFile,
  findMissingDefaultsAndGetWarning,
} from '../../../../core/property-controls/property-controls-utils'
import { dropExtension } from '../../../../core/shared/string-utils'

const simpleControlStatus: ControlStatus = 'simple'
const simpleControlStyles = getControlStyles(simpleControlStatus)

function useSceneTarget() {
  return useInspectorInfoSimpleUntyped(
    [PathForSceneComponent],
    (targets) => {
      const attribute = targets['component']
      return isJSXAttributeOtherJavaScript(attribute) ? attribute.javascript : null
    },
    (target: string) => {
      return {
        component: jsxAttributeOtherJavaScript(target, `return ${target}`, [target], null),
      }
    },
  )
}

export const SceneSection = betterReactMemo('SceneSection', () => {
  const frameLeft = useInspectorLayoutInfo('left')
  const frameTop = useInspectorLayoutInfo('top')
  const frameWidth = useInspectorLayoutInfo('Width')
  const frameHeight = useInspectorLayoutInfo('Height')

  const sceneTarget = useSceneTarget()
  const { codeResultCache, components, openFileFullPath } = useKeepReferenceEqualityIfPossible(
    useEditorState((store) => {
      return {
        codeResultCache: store.editor.codeResultCache,
        components: getOpenUtopiaJSXComponentsFromState(store.editor),
        openFileFullPath: getOpenFilename(store.editor),
      }
    }),
  )

  const filteredComponents = components.filter(
    (component) => component.name !== BakedInStoryboardVariableName,
  ) // FIXME getOpenUtopiaJSXComponentsFromState shouldn't return that one

  const options: Array<SelectOption> =
    openFileFullPath == null
      ? []
      : filteredComponents.map((component) => {
          const componentName = component.name
          const defaultProps = defaultPropertiesForComponentInFile(
            componentName,
            dropExtension(openFileFullPath),
            codeResultCache,
          )
          const detectedProps = component.propsUsed
          const warningMessage = findMissingDefaultsAndGetWarning(detectedProps, defaultProps)
          const warningIconProps =
            warningMessage == null ? undefined : getWarningIconProps(warningMessage)
          return {
            label: componentName,
            value: componentName,
            icon: warningIconProps,
          } as SelectOption
        })

  const onSelect = React.useCallback(
    (selectOption: SelectOption) => {
      const value = selectOption.value
      sceneTarget.onSubmitValue(value)
    },
    [sceneTarget],
  )

  return (
    <>
      <InspectorSectionHeader>Scene</InspectorSectionHeader>
      <InspectorSubsectionHeader>Component</InspectorSubsectionHeader>
      <PropertyRow
        style={{
          minHeight: 0,
        }}
      >
        <PopupList
          id='target-component'
          key='target-component'
          onSubmitValue={onSelect}
          controlStyles={simpleControlStyles}
          value={
            {
              value: sceneTarget.value,
              label: sceneTarget.value,
            } as SelectOption
          }
          style={{ gridColumn: '1 / span 21' }}
          options={options}
        />
      </PropertyRow>
      <InspectorSubsectionHeader>Layout</InspectorSubsectionHeader>
      <PropertyRow style={{ gridColumnGap: 16 }}>
        <PositionWidgetForCSSNumber inspectorInfo={frameLeft} point={'left'} />
        <PositionWidgetForCSSNumber inspectorInfo={frameTop} point={'top'} />
      </PropertyRow>
      <PropertyRow style={{ gridColumnGap: 16 }}>
        <PositionWidget inspectorInfo={frameWidth} point={'width'} />
        <PositionWidget inspectorInfo={frameHeight} point={'height'} />
      </PropertyRow>
      <InspectorSubsectionHeader>Container</InspectorSubsectionHeader>
      <SceneContainerSections />
      <ComponentSection isScene={true} />
    </>
  )
})

const PositionWidget = betterReactMemo(
  'PositionWidget',
  (props: {
    inspectorInfo: InspectorInfo<string | number | undefined>
    point: keyof NormalisedFrame
  }) => {
    const { inspectorInfo, point } = props
    const pinnedProp = pinnedPropForFramePoint(point as FramePoint)
    const label = pinLabels[pinnedProp]
    const wrappedOnSubmitValue = useWrappedEmptyOnSubmitValue(
      inspectorInfo.onSubmitValue,
      inspectorInfo.onUnsetValues,
    )
    const wrappedOnTransientSubmitValue = useWrappedEmptyOnSubmitValue(
      inspectorInfo.onTransientSubmitValue,
      inspectorInfo.onUnsetValues,
    )
    return (
      <div style={{ gridColumn: 'span 2' }}>
        <div
          style={{
            gridColumn: '1 / span 4',
            gridRow: '1 / span 2',
          }}
        >
          <SimpleNumberInput
            value={inspectorInfo.value as any} // I will delete this code once the scenes are merged to components
            labelInner={label}
            id={label}
            onSubmitValue={wrappedOnSubmitValue}
            onTransientSubmitValue={wrappedOnTransientSubmitValue}
          />
        </div>
      </div>
    )
  },
)

const PositionWidgetForCSSNumber = betterReactMemo(
  'PositionWidget',
  (props: { inspectorInfo: InspectorInfo<CSSNumber>; point: keyof NormalisedFrame }) => {
    const { inspectorInfo, point } = props
    const pinnedProp = pinnedPropForFramePoint(point as FramePoint)
    const label = pinLabels[pinnedProp]
    const wrappedOnSubmitValue = useWrappedEmptyOnSubmitValue(
      inspectorInfo.onSubmitValue,
      inspectorInfo.onUnsetValues,
    )
    const wrappedOnTransientSubmitValue = useWrappedEmptyOnSubmitValue(
      inspectorInfo.onTransientSubmitValue,
      inspectorInfo.onUnsetValues,
    )
    return (
      <div style={{ gridColumn: 'span 2' }}>
        <div
          style={{
            gridColumn: '1 / span 4',
            gridRow: '1 / span 2',
          }}
        >
          <NumberInput
            numberType='Length'
            value={inspectorInfo.value}
            labelInner={label}
            id={label}
            onSubmitValue={wrappedOnSubmitValue}
            onTransientSubmitValue={wrappedOnTransientSubmitValue}
          />
        </div>
      </div>
    )
  },
)
