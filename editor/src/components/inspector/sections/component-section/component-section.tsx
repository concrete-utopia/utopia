import * as fastDeepEquals from 'fast-deep-equal'
import * as React from 'react'
import {
  InspectorSectionHeader,
  PopupList,
  SimpleNumberInput,
  useWrappedEmptyOnSubmitValue,
  colorTheme,
  Tooltip,
  FunctionIcons,
  SquareButton,
  Icons,
} from 'uuiui'
import { betterReactMemo, SliderControl } from 'uuiui-deps'
import { jsxAttributeValue } from '../../../../core/shared/element-template'
import { ControlDescription, ControlType } from 'utopia-api'
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
import { CSSColor, parseCSSColor, printColor } from '../../common/css-utils'
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

function useComponentPropsInspectorInfo(propertyName: string, addPropsToPath: boolean) {
  // TODO useInspectorInfo can only work for props which have a parser pre-defined in css-utils.
  // we need to make a more generic hook that doesn't do parsing
  // For now this is just a copy pasted hack to make this work
  function transformValue(parsedValues: any) {
    return parsedValues[propertyName]
  }
  function untransformValue(transformedType: any) {
    return { [propertyName]: jsxAttributeValue(transformedType) }
  }

  const paths = addPropsToPath
    ? [PP.append(PathForSceneProps, PP.create([propertyName]))]
    : [PP.create([propertyName])]
  return useInspectorInfoSimpleUntyped(paths, transformValue, untransformValue)
}

interface ControlForPropProps {
  propName: string
  controlDescription: ControlDescription | undefined
  propMetadata: InspectorInfo<any>
}

const ControlForProp = betterReactMemo('ControlForProp', (props: ControlForPropProps) => {
  const { propName, propMetadata, controlDescription } = props
  const wrappedOnSubmit = useWrappedEmptyOnSubmitValue(
    propMetadata.onSubmitValue,
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
      case ControlType.String:
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
      case ControlType.Number:
        return (
          <SimpleNumberInput
            id={controlId}
            value={value}
            onSubmitValue={wrappedOnSubmit}
            controlStatus={propMetadata.controlStatus}
            incrementControls={controlDescription.displayStepper}
            stepSize={controlDescription.step}
            minimum={controlDescription.min}
            maximum={controlDescription.max}
            labelInner={controlDescription.unit}
          />
        )
      case ControlType.Boolean:
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
      case ControlType.Enum:
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
      case ControlType.Slider: {
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
            controlOptions={{
              minimum: controlDescription.min,
              maximum: controlDescription.max,
              stepSize: controlDescription.step,
            }}
          />
        )
      }
      case ControlType.PopUpList: {
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
      case ControlType.Options:
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
      case ControlType.Color:
        const parsedColor = parseCSSColor(value)
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
      case ControlType.ComponentInstance:
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
        <span style={{ paddingTop: 4, color: colorTheme.errorForeground.value }}>
          {details.description}
        </span>
      </Tooltip>
    </div>
  )
})

interface RowForPropProps {
  propName: string
  title: string
  controlDescription?: ControlDescription
  propertyError?: ParseError
  isScene: boolean
  warningTooltip?: string
}

const RowForProp = betterReactMemo('RowForProp', (props: RowForPropProps) => {
  const { propName } = props
  const propPath = [PP.create([props.propName])]
  const propMetadata = useComponentPropsInspectorInfo(propName, props.isScene)
  const contextMenuItems = Utils.stripNulls([
    addOnUnsetValues([propName], propMetadata.onUnsetValues),
  ])
  const warning =
    props.warningTooltip == null ? null : (
      <Tooltip title={props.warningTooltip}>
        <span>
          <WarningIcon style={{ position: 'relative', left: 2, top: 4 }} />
        </span>
      </Tooltip>
    )
  return (
    <InspectorContextMenuWrapper
      id={`context-menu-for-${props.propName}`}
      items={contextMenuItems}
      data={null}
    >
      <GridRow padded={true} type='<---1fr--->|------172px-------|'>
        <PropertyLabel target={propPath}>
          {props.title}
          {warning}
        </PropertyLabel>
        {props.propertyError == null ? (
          <ControlForProp
            propName={props.propName}
            controlDescription={props.controlDescription}
            propMetadata={propMetadata}
          />
        ) : (
          <ParseErrorControl parseError={props.propertyError} />
        )}
      </GridRow>
    </InspectorContextMenuWrapper>
  )
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
                Component props
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
                        <RowForProp
                          key={propName}
                          title={propName}
                          propName={propName}
                          propertyError={propertyError}
                          isScene={props.isScene}
                        />
                      )
                    },
                    (controlDescription) => {
                      let warningTooltip: string | undefined = undefined
                      const unsetOptionalFields = getDescriptionUnsetOptionalFields(
                        controlDescription,
                      )
                      if (unsetOptionalFields.length > 0) {
                        warningTooltip = `This control has the following unset optional fields: ${joinSpecial(
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
