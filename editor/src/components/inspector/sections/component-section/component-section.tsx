import * as fastDeepEquals from 'fast-deep-equal'
import * as React from 'react'
import {
  InspectorSectionHeader,
  PopupList,
  SimpleNumberInput,
  useWrappedEmptyOrUnknownOnSubmitValue,
  colorTheme,
  Tooltip,
  FunctionIcons,
  SquareButton,
  Icons,
  SimpleFlexRow,
  UtopiaStyles,
  UtopiaTheme,
} from 'uuiui'
import { betterReactMemo, SliderControl } from 'uuiui-deps'
import { jsxAttributeValue } from '../../../../core/shared/element-template'
import { ControlDescription, BaseControlDescription, isBaseControlDescription } from 'utopia-api'
import { foldEither, right, Either } from '../../../../core/shared/either'
import Utils from '../../../../utils/utils'
import { InspectorContextMenuWrapper } from '../../../context-menu-wrapper'
import * as PP from '../../../../core/shared/property-path'
import { BooleanControl } from '../../controls/boolean-control'
import { ColorControl } from '../../controls/color-control'
import { OptionChainControl } from '../../controls/option-chain-control'
import { SelectControl, SelectOption } from '../../controls/select-control'
import { StringControl } from '../../controls/string-control'
import { addOnUnsetValues } from '../../common/context-menu-items'
import { CSSColor, parseColor, printColor } from '../../common/css-utils'
import {
  InspectorInfo,
  useInspectorInfoSimpleUntyped,
  useKeepReferenceEqualityIfPossible,
  useSelectedPropertyControls,
  useUsedPropsWithoutControls,
  useUsedPropsWithoutDefaults,
} from '../../common/property-path-hooks'
import { PropertyRow } from '../../widgets/property-row'
import { PathForSceneProps } from '../../../../core/model/scene-utils'
import { GridRow } from '../../widgets/grid-row'
import { PropertyLabel } from '../../widgets/property-label'
import { ParseError, getParseErrorDetails } from '../../../../utils/value-parser-utils'
import { InfoBox } from '../../../common/notices'
import { showContextMenu } from '../../../editor/actions/actions'
import { useEditorState } from '../../../editor/store/store-hook'
import { InstanceContextMenu } from '../../common/instance-context-menu'
import {
  getMissingDefaultsWarning,
  getMissingPropertyControlsWarning,
} from '../../../../core/property-controls/property-controls-utils'
import { getDescriptionUnsetOptionalFields } from '../../../../core/property-controls/property-controls-utils'
import { joinSpecial } from '../../../../core/shared/array-utils'
import { WarningIcon } from '../../../../uuiui/warning-icon'
import { useInspectorInfoForPropertyControl } from '../../common/property-controls-hooks'
import { PropertyPath } from '../../../../core/shared/project-file-types'

function useComponentPropsInspectorInfo(
  partialPath: PropertyPath,
  addPropsToPath: boolean,
  control: BaseControlDescription,
) {
  const propertyPath = addPropsToPath ? PP.append(PathForSceneProps, partialPath) : partialPath
  return useInspectorInfoForPropertyControl(propertyPath, control)
}

interface ControlForPropProps {
  propName: string
  controlDescription: ControlDescription | undefined
  propMetadata: InspectorInfo<any>
}

const ControlForProp = betterReactMemo('ControlForProp', (props: ControlForPropProps) => {
  const { propName, propMetadata, controlDescription } = props
  const wrappedOnSubmit = useWrappedEmptyOrUnknownOnSubmitValue(
    propMetadata.onSubmitValue,
    propMetadata.onUnsetValues,
  )
  const wrappedOnTransientSubmit = useWrappedEmptyOrUnknownOnSubmitValue(
    propMetadata.onTransientSubmitValue,
    propMetadata.onUnsetValues,
  )
  if (controlDescription == null) {
    return null
  } else {
    const controlId = `${propName}-property-control`
    const value = propMetadata.propertyStatus.set
      ? propMetadata.value
      : (controlDescription as any).defaultValue
    switch (controlDescription.type) {
      case 'string':
        const stringControlValue = Utils.defaultIfNull('', value)
        return (
          <StringControl
            key={propName}
            id={controlId}
            value={stringControlValue}
            onSubmitValue={propMetadata.onSubmitValue}
            controlStatus={propMetadata.controlStatus}
            controlStyles={propMetadata.controlStyles}
          />
        )
      case 'number':
        return (
          <SimpleNumberInput
            id={controlId}
            value={value}
            onSubmitValue={wrappedOnSubmit}
            onTransientSubmitValue={wrappedOnTransientSubmit}
            onForcedSubmitValue={wrappedOnSubmit}
            controlStatus={propMetadata.controlStatus}
            incrementControls={controlDescription.displayStepper}
            stepSize={controlDescription.step}
            minimum={controlDescription.min}
            maximum={controlDescription.max}
            labelInner={controlDescription.unit}
          />
        )
      case 'boolean':
        return (
          <BooleanControl
            key={propName}
            id={controlId}
            value={value}
            onSubmitValue={propMetadata.onSubmitValue}
            controlStatus={propMetadata.controlStatus}
            controlStyles={propMetadata.controlStyles}
          />
        )
      case 'enum':
        // TODO memoize this
        const options: Array<SelectOption> = controlDescription.options.map((option, index) => {
          return {
            value: option as string, // TODO cheating with type
            label:
              controlDescription.optionTitles == null ||
              typeof controlDescription.optionTitles === 'function'
                ? (option as string)
                : (controlDescription.optionTitles[index] as string),
          }
        })
        return (
          <SelectControl
            style={{
              fontWeight: 'normal',
              marginLeft: 4,
            }}
            key={propName}
            id={controlId}
            value={value}
            onSubmitValue={propMetadata.onSubmitValue}
            controlStatus={propMetadata.controlStatus}
            controlStyles={propMetadata.controlStyles}
            options={options}
          />
        )
      case 'slider': {
        return (
          <SliderControl
            key={propName}
            id={controlId}
            value={value}
            onTransientSubmitValue={propMetadata.onTransientSubmitValue}
            onForcedSubmitValue={propMetadata.onSubmitValue}
            onSubmitValue={propMetadata.onSubmitValue}
            controlStatus={propMetadata.controlStatus}
            controlStyles={propMetadata.controlStyles}
            DEPRECATED_controlOptions={{
              minimum: controlDescription.min,
              maximum: controlDescription.max,
              stepSize: controlDescription.step,
            }}
          />
        )
      }
      case 'popuplist': {
        function submitValue(option: SelectOption): void {
          propMetadata.onSubmitValue(option.value)
        }
        const currentValue = controlDescription.options.find((option) => {
          return fastDeepEquals(option.value, value)
        })

        return (
          <PopupList
            disabled={!propMetadata.controlStyles.interactive}
            value={currentValue}
            onSubmitValue={submitValue}
            options={controlDescription.options}
            containerMode={'default'}
          />
        )
      }
      case 'options':
        return (
          <OptionChainControl
            key={propName}
            id={controlId}
            value={value}
            controlStatus={propMetadata.controlStatus}
            controlStyles={propMetadata.controlStyles}
            onSubmitValue={propMetadata.onSubmitValue}
            options={controlDescription.options}
          />
        )
      case 'color':
        const parsedColor = parseColor(value)
        return foldEither(
          (failureReason) => {
            return <div>{failureReason}</div>
          },
          (validColor) => {
            function transientSubmitValue(color: CSSColor): void {
              propMetadata.onTransientSubmitValue(printColor(color))
            }
            function submitValue(color: CSSColor): void {
              propMetadata.onSubmitValue(printColor(color))
            }
            return (
              <ColorControl
                key={propName}
                id={controlId}
                value={validColor}
                controlStatus={propMetadata.controlStatus}
                controlStyles={propMetadata.controlStyles}
                onTransientSubmitValue={transientSubmitValue}
                onSubmitValue={submitValue}
              />
            )
          },
          parsedColor,
        )
      case 'componentinstance':
        return (
          <StringControl
            key={propName}
            id={controlId}
            value={Utils.defaultIfNull('', value)}
            onSubmitValue={
              propMetadata.controlStatus === 'controlled' ? Utils.NO_OP : propMetadata.onSubmitValue
            }
            controlStatus={propMetadata.controlStatus}
            controlStyles={{
              ...propMetadata.controlStyles,
              showContent: true,
            }}
          />
        )
      default:
        return <div>Not yet implemented control type.</div>
    }
  }
})

interface ParseErrorProps {
  parseError: ParseError
}

export const ParseErrorControl = betterReactMemo('ParseErrorControl', (props: ParseErrorProps) => {
  const details = getParseErrorDetails(props.parseError)
  return (
    <div>
      <Tooltip title={`${details.path}`}>
        <span>{details.description}</span>
      </Tooltip>
    </div>
  )
})

const WarningTooltip = betterReactMemo('WarningTooltip', ({ warning }: { warning: string }) => {
  return (
    <Tooltip title={warning}>
      <div
        style={{
          width: 5,
          height: 5,
          background: colorTheme.warningBgSolid.value,
          borderRadius: '50%',
          marginRight: 4,
        }}
      />
    </Tooltip>
  )
})

interface RowForInvalidControlProps {
  propName: string
  title: string
  propertyError: ParseError
  warningTooltip?: string
}

const RowForInvalidControl = betterReactMemo(
  'RowForInvalidControl',
  (props: RowForInvalidControlProps) => {
    const propPath = [PP.create([props.propName])]
    const warning =
      props.warningTooltip == null ? null : <WarningTooltip warning={props.warningTooltip} />
    return (
      <GridRow padded={true} type='<---1fr--->|------172px-------|'>
        <PropertyLabel target={propPath}>
          {warning}
          {props.title}
        </PropertyLabel>
        <ParseErrorControl parseError={props.propertyError} />
      </GridRow>
    )
  },
)

interface RowForPropProps {
  propName: string
  title: string
  controlDescription: ControlDescription
  isScene: boolean
  warningTooltip?: string
}

const RowForProp = betterReactMemo('RowForProp', (props: RowForPropProps) => {
  const { propName, title, controlDescription, isScene, warningTooltip } = props
  if (isBaseControlDescription(controlDescription)) {
    // TODO Use higher level controls to determine the layout of the Inspector
    const propPath = PP.create([props.propName])
    const propMetadata = useComponentPropsInspectorInfo(propPath, isScene, controlDescription)
    const contextMenuItems = Utils.stripNulls([
      addOnUnsetValues([propName], propMetadata.onUnsetValues),
    ])
    const warning = warningTooltip == null ? null : <WarningTooltip warning={warningTooltip} />
    return (
      <InspectorContextMenuWrapper
        id={`context-menu-for-${propName}`}
        items={contextMenuItems}
        data={null}
      >
        <GridRow padded={true} type='<---1fr--->|------172px-------|'>
          <PropertyLabel target={[propPath]}>
            {warning}
            {title}
          </PropertyLabel>
          <ControlForProp
            propName={propName}
            controlDescription={controlDescription}
            propMetadata={propMetadata}
          />
        </GridRow>
      </InspectorContextMenuWrapper>
    )
  } else {
    return null
  }
})

export interface ComponentSectionProps {
  isScene: boolean
}

export const ComponentSectionInner = betterReactMemo(
  'ComponentSectionInner',
  (props: ComponentSectionProps) => {
    const propertyControls = useKeepReferenceEqualityIfPossible(useSelectedPropertyControls(false))
    const propsUsedWithoutControls = useKeepReferenceEqualityIfPossible(
      useUsedPropsWithoutControls(),
    )
    const dispatch = useEditorState((state) => state.dispatch)
    const onResetClicked = React.useCallback(
      (event: React.MouseEvent<HTMLElement>) => {
        dispatch(
          [showContextMenu('context-menu-instance-inspector', event.nativeEvent)],
          'everyone',
        )
      },
      [dispatch],
    )
    const propsUsedWithoutDefaults = useKeepReferenceEqualityIfPossible(
      useUsedPropsWithoutDefaults(),
    )
    const missingControlsWarning = getMissingPropertyControlsWarning(propsUsedWithoutControls)
    const missingDefaultsWarning = getMissingDefaultsWarning(propsUsedWithoutDefaults)
    return foldEither(
      (rootParseError) => {
        return (
          <>
            <InspectorSectionHeader>Component props</InspectorSectionHeader>
            <ParseErrorControl parseError={rootParseError} />
          </>
        )
      },
      (rootParseSuccess) => {
        const propNames = Object.keys(rootParseSuccess)
        if (propNames.length > 0 || propsUsedWithoutControls.length > 0) {
          return (
            <>
              <InspectorSectionHeader>
                <SimpleFlexRow style={{ flexGrow: 1 }}>Component props</SimpleFlexRow>
                <SquareButton highlight onClick={onResetClicked}>
                  <InstanceContextMenu
                    propNames={propNames}
                    contextMenuInstance={'context-menu-instance-inspector'}
                  />
                  <FunctionIcons.Reset />
                </SquareButton>
              </InspectorSectionHeader>
              {missingControlsWarning == null ? null : (
                <InfoBox message={'Missing Property Controls'}>{missingControlsWarning}</InfoBox>
              )}
              {missingDefaultsWarning == null ? null : (
                <InfoBox message={'Missing Default Properties'}>{missingDefaultsWarning}</InfoBox>
              )}
              {propNames.map((propName) => {
                const propertyControl = rootParseSuccess[propName]
                if (propertyControl == null) {
                  return null
                } else {
                  return foldEither(
                    (propertyError) => {
                      return (
                        <RowForInvalidControl
                          key={propName}
                          title={propName}
                          propName={propName}
                          propertyError={propertyError}
                        />
                      )
                    },
                    (controlDescription) => {
                      let warningTooltip: string | undefined = undefined
                      const unsetOptionalFields = getDescriptionUnsetOptionalFields(
                        controlDescription,
                      )
                      if (unsetOptionalFields.length > 0) {
                        warningTooltip = `These optional fields are not set: ${joinSpecial(
                          unsetOptionalFields,
                          ', ',
                          ' and ',
                        )}`
                      }
                      return (
                        <RowForProp
                          key={propName}
                          title={Utils.defaultIfNull(propName, controlDescription.title)}
                          propName={propName}
                          controlDescription={controlDescription}
                          isScene={props.isScene}
                          warningTooltip={warningTooltip}
                        />
                      )
                    },
                    propertyControl,
                  )
                }
              })}
            </>
          )
        } else {
          return null
        }
      },
      propertyControls,
    )
  },
)

export interface ComponentSectionState {
  errorOccurred: boolean
}

export class ComponentSection extends React.Component<
  ComponentSectionProps,
  ComponentSectionState
> {
  constructor(props: ComponentSectionProps) {
    super(props)
    this.state = { errorOccurred: false }
  }

  static getDerivedStateFromError(error: Error): ComponentSectionState {
    return {
      errorOccurred: true,
    }
  }

  componentDidCatch(error: Error, errorInfo: React.ErrorInfo): void {
    console.error('Error occurred in component section.', error, errorInfo)
  }

  render() {
    if (this.state.errorOccurred) {
      return (
        <>
          <InspectorSectionHeader>Component props</InspectorSectionHeader>
          <PropertyRow
            style={{
              gridTemplateColumns: '2fr 4fr',
            }}
          >
            <span style={{ paddingTop: 4, color: colorTheme.errorForeground.value }}>
              Invalid propertyControls value
            </span>
          </PropertyRow>
        </>
      )
    } else {
      return <ComponentSectionInner {...this.props} />
    }
  }
}
