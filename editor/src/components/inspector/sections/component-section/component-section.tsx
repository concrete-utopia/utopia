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
import { betterReactMemo, SliderControl, getControlStyles } from 'uuiui-deps'
import { jsxAttributeValue } from '../../../../core/shared/element-template'
import {
  ControlDescription,
  BaseControlDescription,
  isBaseControlDescription,
  UnionControlDescription,
} from 'utopia-api'
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
import {
  useInspectorInfoForPropertyControl,
  useControlForUnionControl,
} from '../../common/property-controls-hooks'
import { PropertyPath } from '../../../../core/shared/project-file-types'
import { OptionsType } from 'react-select'

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
  controlDescription: BaseControlDescription | undefined
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

interface AbstractRowForControlProps {
  propPath: PropertyPath
  isScene: boolean
}

function titleForControl(propPath: PropertyPath, control: ControlDescription): string {
  return control.title ?? PP.lastPartToString(propPath)
}

interface RowForBaseControlProps extends AbstractRowForControlProps {
  label?: React.ComponentType<any>
  controlDescription: BaseControlDescription
}

const RowForBaseControl = betterReactMemo('RowForBaseControl', (props: RowForBaseControlProps) => {
  const { propPath, controlDescription, isScene } = props
  const title = titleForControl(propPath, controlDescription)
  const propName = `${PP.lastPart(propPath)}`

  let warningTooltip: string | undefined = undefined
  const unsetOptionalFields = getDescriptionUnsetOptionalFields(controlDescription)
  if (unsetOptionalFields.length > 0) {
    warningTooltip = `These optional fields are not set: ${joinSpecial(
      unsetOptionalFields,
      ', ',
      ' and ',
    )}`
  }

  const propMetadata = useComponentPropsInspectorInfo(propPath, isScene, controlDescription)
  const contextMenuItems = Utils.stripNulls([
    addOnUnsetValues([propName], propMetadata.onUnsetValues),
  ])
  const warning = warningTooltip == null ? null : <WarningTooltip warning={warningTooltip} />

  const propertyLabel =
    props.label == null ? (
      <PropertyLabel target={[propPath]} style={{ textTransform: 'capitalize' }}>
        {warning}
        {title}
      </PropertyLabel>
    ) : (
      <props.label />
    )

  return (
    <InspectorContextMenuWrapper
      id={`context-menu-for-${propName}`}
      items={contextMenuItems}
      data={null}
    >
      <GridRow padded={true} type='<---1fr--->|------172px-------|'>
        {propertyLabel}
        <ControlForProp
          propName={propName}
          controlDescription={controlDescription}
          propMetadata={propMetadata}
        />
      </GridRow>
    </InspectorContextMenuWrapper>
  )
})

interface RowForUnionControlProps extends AbstractRowForControlProps {
  controlDescription: UnionControlDescription
}

const RowForUnionControl = betterReactMemo(
  'RowForUnionControl',
  (props: RowForUnionControlProps) => {
    const { propPath, controlDescription } = props
    const title = titleForControl(propPath, controlDescription)

    const suitableControl = useControlForUnionControl(propPath, controlDescription)
    const [controlToUse, setControlToUse] = React.useState(suitableControl)

    const labelOptions: OptionsType<SelectOption> = controlDescription.controls.map((control) => {
      const label = control.title ?? control.type
      return {
        value: control,
        label: label,
      }
    })

    const labelValue: SelectOption = {
      value: controlToUse,
      label: title,
    }

    const onLabelChangeValue = React.useCallback(
      (option: SelectOption) => {
        if (option.value !== controlToUse) {
          setControlToUse(option.value)
        }
      },
      [controlToUse, setControlToUse],
    )

    const simpleControlStyles = getControlStyles('simple')

    const label = (
      <PopupList
        value={labelValue}
        options={labelOptions}
        onSubmitValue={onLabelChangeValue}
        containerMode='showBorderOnHover'
        controlStyles={simpleControlStyles}
        style={{
          maxWidth: '100%',
          overflow: 'hidden',
        }}
      />
    )

    const labelAsRenderProp = React.useCallback(() => label, [label])

    if (isBaseControlDescription(controlToUse)) {
      return (
        <RowForBaseControl {...props} label={labelAsRenderProp} controlDescription={controlToUse} />
      )
    } else {
      return (
        <>
          {label}
          <RowForControl {...props} controlDescription={controlToUse} />
        </>
      )
    }
  },
)

interface RowForControlProps extends AbstractRowForControlProps {
  controlDescription: ControlDescription
}

const RowForControl = betterReactMemo('RowForControl', (props: RowForControlProps) => {
  const { controlDescription } = props

  if (isBaseControlDescription(controlDescription)) {
    return <RowForBaseControl {...props} controlDescription={controlDescription} />
  } else {
    switch (controlDescription.type) {
      case 'array':
        return <div>Not yet implemented control type.</div>
      case 'object':
        return <div>Not yet implemented control type.</div>
      case 'union':
        return <RowForUnionControl {...props} controlDescription={controlDescription} />
      default:
        const _exhaustiveCheck: never = controlDescription
        throw new Error(`Unhandled control ${JSON.stringify(controlDescription)}`)
    }
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
                      return (
                        <RowForControl
                          key={propName}
                          propPath={PP.create([propName])}
                          controlDescription={controlDescription}
                          isScene={props.isScene}
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
          return (
            <>
              <InspectorSectionHeader>
                <SimpleFlexRow style={{ flexGrow: 1 }}>Component props</SimpleFlexRow>
              </InspectorSectionHeader>
              <InfoBox message={'No properties available to configure.'} />
              <InfoBox message={'Use code instead.'} />
            </>
          )
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
