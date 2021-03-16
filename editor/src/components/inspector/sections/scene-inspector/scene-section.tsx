import * as React from 'react'
import { FramePoint, NormalisedFrame } from 'utopia-api'
import {
  InspectorInfo,
  useInspectorInfoSimpleUntyped,
  useInspectorLayoutInfo,
} from '../../common/property-path-hooks'
import { pinnedPropForFramePoint } from '../../../../core/layout/layout-helpers-new'
import {
  BakedInStoryboardVariableName,
  PathForSceneComponent,
} from '../../../../core/model/scene-utils'
import {
  defaultPropertiesForComponentInFile,
  findMissingDefaultsAndGetWarning,
} from '../../../../core/property-controls/property-controls-utils'
import {
  isJSXAttributeOtherJavaScript,
  jsxAttributeOtherJavaScript,
  jsxAttributeValue,
  jsxElementNameEquals,
} from '../../../../core/shared/element-template'
import { dropFileExtension } from '../../../../core/shared/file-utils'
import { getWarningIconProps } from '../../../../uuiui/warning-icon'
import {
  getOpenFilename,
  getOpenUtopiaJSXComponentsFromState,
} from '../../../editor/store/editor-state'
import { useEditorState } from '../../../editor/store/store-hook'
import { ControlStatus, getControlStyles } from '../../common/control-status'
import { CSSNumber } from '../../common/css-utils'
import { SelectOption } from '../../controls/select-control'
import { PropertyRow } from '../../widgets/property-row'
import { ComponentSection } from '../component-section/component-section'
import { pinLabels } from '../layout-section/self-layout-subsection/gigantic-size-pins-subsection'
import { SceneContainerSections } from './scene-container-section'
import {
  betterReactMemo,
  useKeepReferenceEqualityIfPossible,
} from '../../../../utils/react-performance'
import {
  InspectorSectionHeader,
  InspectorSubsectionHeader,
  PopupList,
  useWrappedEmptyOrUnknownOnSubmitValue,
  SimpleNumberInput,
  NumberInput,
} from '../../../../uuiui'
import {
  getComponentGroupsAsSelectOptions,
  InsertableComponent,
} from '../../../shared/project-components'
import { usePossiblyResolvedPackageDependencies } from '../../../editor/npm-dependency/npm-dependency'
import { addImports, setSceneProp } from '../../../editor/actions/action-creators'
import { ScenePath } from '../../../../core/shared/project-file-types'
import { emptyComments } from '../../../../core/workers/parser-printer/parser-printer-comments'

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

export interface SceneSectionProps {
  scenePath: ScenePath
}

export const SceneSection = betterReactMemo('SceneSection', (props: SceneSectionProps) => {
  const frameLeft = useInspectorLayoutInfo('left')
  const frameTop = useInspectorLayoutInfo('top')
  const frameWidth = useInspectorLayoutInfo('Width')
  const frameHeight = useInspectorLayoutInfo('Height')

  const sceneTarget = useSceneTarget()

  const {
    packageStatus,
    propertyControlsInfo,
    projectContents,
    dispatch,
    currentlyOpenFilename,
  } = useEditorState((store) => {
    return {
      packageStatus: store.editor.nodeModules.packageStatus,
      propertyControlsInfo: store.editor.propertyControlsInfo,
      projectContents: store.editor.projectContents,
      dispatch: store.dispatch,
      currentlyOpenFilename: store.editor.canvas.openFile?.filename ?? null,
    }
  }, 'SceneSection')

  const dependencies = usePossiblyResolvedPackageDependencies()

  const insertableComponents = React.useMemo(() => {
    return currentlyOpenFilename == null
      ? []
      : getComponentGroupsAsSelectOptions(
          packageStatus,
          propertyControlsInfo,
          projectContents,
          dependencies,
          currentlyOpenFilename,
        )
  }, [packageStatus, propertyControlsInfo, projectContents, dependencies, currentlyOpenFilename])

  const currentTargetComponentOption = React.useMemo(() => {
    function searchOptions(selectOptions: Array<SelectOption>): SelectOption | undefined {
      for (const innerOption of selectOptions) {
        const innerResult = searchOption(innerOption)
        if (innerResult != null) {
          return innerResult
        }
      }
      return undefined
    }
    function searchOption(selectOption: SelectOption): SelectOption | undefined {
      if (selectOption.value != null) {
        const value: InsertableComponent = selectOption.value
        if (value.name === sceneTarget.value) {
          return selectOption
        }
      }
      if (selectOption.options != null) {
        const optionsResult = searchOptions(selectOption.options)
        if (optionsResult != null) {
          return optionsResult
        }
      }
      return undefined
    }
    return searchOptions(insertableComponents)
  }, [insertableComponents, sceneTarget])

  const onSelect = React.useCallback(
    (selectOption: SelectOption) => {
      const value: InsertableComponent = selectOption.value
      dispatch(
        [
          addImports(value.importsToAdd),
          setSceneProp(
            props.scenePath,
            PathForSceneComponent,
            jsxAttributeOtherJavaScript(value.name, `return ${value.name}`, [value.name], null),
          ),
        ],
        'everyone',
      )
    },
    [props.scenePath, dispatch],
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
          value={currentTargetComponentOption}
          style={{ gridColumn: '1 / span 21' }}
          options={insertableComponents}
        />
      </PropertyRow>
      <InspectorSubsectionHeader>Layout</InspectorSubsectionHeader>
      <PropertyRow style={{ gridColumnGap: 16 }}>
        <PositionWidget inspectorInfo={frameLeft} point={'left'} />
        <PositionWidget inspectorInfo={frameTop} point={'top'} />
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
    inspectorInfo: InspectorInfo<CSSNumber> | InspectorInfo<CSSNumber | undefined>
    point: keyof NormalisedFrame
  }) => {
    const { inspectorInfo, point } = props
    const pinnedProp = pinnedPropForFramePoint(point as FramePoint)
    const label = pinLabels[pinnedProp]
    const wrappedOnSubmitValue = useWrappedEmptyOrUnknownOnSubmitValue(
      inspectorInfo.onSubmitValue,
      inspectorInfo.onUnsetValues,
    )
    const wrappedOnTransientSubmitValue = useWrappedEmptyOrUnknownOnSubmitValue(
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
            id={`scene-frame-${pinnedProp}-number-input`}
            testId={`scene-frame-${pinnedProp}-number-input`}
            onSubmitValue={wrappedOnSubmitValue}
            onTransientSubmitValue={wrappedOnTransientSubmitValue}
            defaultUnitToHide={'px'}
          />
        </div>
      </div>
    )
  },
)
