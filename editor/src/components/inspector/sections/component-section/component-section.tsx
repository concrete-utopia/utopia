/**@jsx jsx */
import React from 'react'
import { css, jsx } from '@emotion/react'
import { OptionsType } from 'react-select'
import { animated } from 'react-spring'
import {
  ArrayControlDescription,
  BaseControlDescription,
  ControlDescription,
  FolderControlDescription,
  HigherLevelControlDescription,
  isBaseControlDescription,
  ObjectControlDescription,
  PropertyControls,
  RegularControlDescription,
  UnionControlDescription,
} from 'utopia-api'
import { PathForSceneProps } from '../../../../core/model/scene-utils'
import { mapToArray } from '../../../../core/shared/object-utils'
import { ElementPath, PropertyPath } from '../../../../core/shared/project-file-types'
import * as PP from '../../../../core/shared/property-path'
import * as EP from '../../../../core/shared/element-path'
import {
  betterReactMemo,
  useKeepReferenceEqualityIfPossible,
} from '../../../../utils/react-performance'
import Utils from '../../../../utils/utils'
import { getParseErrorDetails, ParseError, ParseResult } from '../../../../utils/value-parser-utils'
import {
  Tooltip,
  //TODO: switch last component to functional component and make use of 'useColorTheme':
  colorTheme as colorThemeConst,
  useColorTheme,
  UtopiaTheme,
  InspectorSectionHeader,
  SimpleFlexRow,
  SquareButton,
  PopupList,
  Icons,
  VerySubdued,
  FlexRow,
} from '../../../../uuiui'
import { CSSCursor, getControlStyles } from '../../../../uuiui-deps'
import { InspectorContextMenuWrapper } from '../../../context-menu-wrapper'
import { addOnUnsetValues } from '../../common/context-menu-items'
import {
  useControlForUnionControl,
  useGetPropertyControlsForSelectedComponents,
  useInspectorInfoForPropertyControl,
} from '../../common/property-controls-hooks'
import { ControlStyles, ControlStatus } from '../../common/control-status'
import { InspectorInfo } from '../../common/property-path-hooks'
import { useArraySuperControl } from '../../controls/array-supercontrol'
import { SelectOption } from '../../controls/select-control'
import { UIGridRow } from '../../widgets/ui-grid-row'
import { PropertyLabel } from '../../widgets/property-label'
import { PropertyRow } from '../../widgets/property-row'
import {
  ControlForBooleanProp,
  ControlForColorProp,
  ControlForEnumProp,
  ControlForEulerProp,
  ControlForExpressionEnumProp,
  ControlForImageProp,
  ControlForMatrix3Prop,
  ControlForMatrix4Prop,
  ControlForNumberProp,
  ControlForOptionsProp,
  ControlForPopupListProp,
  ControlForPropProps,
  ControlForQuaternionProp,
  ControlForRawJSProp,
  ControlForStringProp,
  ControlForVectorProp,
} from './property-control-controls'
import { ComponentInfoBox } from './component-info-box'
import { ExpandableIndicator } from '../../../navigator/navigator-item/expandable-indicator'
import { when } from '../../../../utils/react-conditionals'
import { PropertyControlsSection } from './property-controls-section'

function useComponentPropsInspectorInfo(
  partialPath: PropertyPath,
  addPropsToPath: boolean,
  control: RegularControlDescription,
) {
  const propertyPath = addPropsToPath ? PP.append(PathForSceneProps, partialPath) : partialPath
  return useInspectorInfoForPropertyControl(propertyPath, control)
}

const ControlForProp = betterReactMemo(
  'ControlForProp',
  (props: ControlForPropProps<BaseControlDescription>) => {
    const { controlDescription } = props
    if (controlDescription == null) {
      return null
    } else {
      switch (controlDescription.type) {
        case 'boolean':
          return <ControlForBooleanProp {...props} controlDescription={controlDescription} />
        case 'color':
          return <ControlForColorProp {...props} controlDescription={controlDescription} />
        case 'enum':
          return <ControlForEnumProp {...props} controlDescription={controlDescription} />
        case 'euler':
          return <ControlForEulerProp {...props} controlDescription={controlDescription} />
        case 'expression-enum':
          return <ControlForExpressionEnumProp {...props} controlDescription={controlDescription} />
        case 'ignore':
          return null
        case 'image':
          return <ControlForImageProp {...props} controlDescription={controlDescription} />
        case 'matrix3':
          return <ControlForMatrix3Prop {...props} controlDescription={controlDescription} />
        case 'matrix4':
          return <ControlForMatrix4Prop {...props} controlDescription={controlDescription} />
        case 'number':
          return <ControlForNumberProp {...props} controlDescription={controlDescription} />
        case 'options':
          return <ControlForOptionsProp {...props} controlDescription={controlDescription} />
        case 'popuplist':
          return <ControlForPopupListProp {...props} controlDescription={controlDescription} />
        case 'quaternion':
          return <ControlForQuaternionProp {...props} controlDescription={controlDescription} />
        case 'rawjs':
          return <ControlForRawJSProp {...props} controlDescription={controlDescription} />
        case 'string':
          return <ControlForStringProp {...props} controlDescription={controlDescription} />
        case 'vector2':
        case 'vector3':
        case 'vector4':
          return <ControlForVectorProp {...props} controlDescription={controlDescription} />
        // case 'styleobject':
        default:
          return null
      }
    }
  },
)
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
  const colorTheme = useColorTheme()
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

export const RowForInvalidControl = betterReactMemo(
  'RowForInvalidControl',
  (props: RowForInvalidControlProps) => {
    const propPath = [PP.create([props.propName])]
    const warning =
      props.warningTooltip == null ? null : <WarningTooltip warning={props.warningTooltip} />
    return (
      <UIGridRow padded={true} variant='<--1fr--><--1fr-->'>
        <PropertyLabel target={propPath}>
          {warning}
          {props.title}
        </PropertyLabel>
        <ParseErrorControl parseError={props.propertyError} />
      </UIGridRow>
    )
  },
)

interface AbstractRowForControlProps {
  propPath: PropertyPath
  isScene: boolean
  setGlobalCursor: (cursor: CSSCursor | null) => void
  indentationLevel: number
}

function titleForControl(propPath: PropertyPath, control: RegularControlDescription): string {
  return control.title ?? PP.lastPartToString(propPath)
}

function getLabelControlStyle(
  controlDescription: ControlDescription,
  propMetadata: InspectorInfo<any>,
): ControlStyles {
  if (
    controlDescription.type === 'expression-enum' &&
    propMetadata.controlStatus === 'controlled'
  ) {
    return getControlStyles('simple')
  } else {
    return propMetadata.controlStyles
  }
}

interface RowForBaseControlProps extends AbstractRowForControlProps {
  label?: React.ComponentType<any> // TODO Before Merge this probably should not be a component
  controlDescription: BaseControlDescription
}

const RowForBaseControl = betterReactMemo('RowForBaseControl', (props: RowForBaseControlProps) => {
  const { propPath, controlDescription, isScene } = props
  const title = titleForControl(propPath, controlDescription)
  const propName = `${PP.lastPart(propPath)}`
  const indentation = props.indentationLevel * 8

  const propMetadata = useComponentPropsInspectorInfo(propPath, isScene, controlDescription)
  const contextMenuItems = Utils.stripNulls([
    addOnUnsetValues([propName], propMetadata.onUnsetValues),
  ])

  const labelControlStyle = React.useMemo(
    () => getLabelControlStyle(controlDescription, propMetadata),
    [controlDescription, propMetadata],
  )

  const propertyLabel =
    props.label == null ? (
      <PropertyLabel
        controlStyles={labelControlStyle}
        target={[propPath]}
        style={{ textTransform: 'capitalize', paddingLeft: indentation }}
      >
        <Tooltip title={title}>
          <span>{title}</span>
        </Tooltip>
      </PropertyLabel>
    ) : (
      <props.label />
    )

  if (controlDescription.type === 'ignore') {
    // do not list anything for `ignore` controls
    return null
  }

  return (
    <InspectorContextMenuWrapper
      id={`context-menu-for-${propName}`}
      items={contextMenuItems}
      data={null}
    >
      <UIGridRow padded={false} style={{ paddingLeft: 0 }} variant='<--1fr--><--1fr-->'>
        {propertyLabel}
        <ControlForProp
          propPath={propPath}
          propName={propName}
          controlDescription={controlDescription}
          propMetadata={propMetadata}
          setGlobalCursor={props.setGlobalCursor}
        />
      </UIGridRow>
    </InspectorContextMenuWrapper>
  )
})

interface RowForArrayControlProps extends AbstractRowForControlProps {
  controlDescription: ArrayControlDescription
}

const RowForArrayControl = betterReactMemo(
  'RowForArrayControl',
  (props: RowForArrayControlProps) => {
    const { propPath, controlDescription, isScene } = props
    const title = titleForControl(propPath, controlDescription)
    const { value, onSubmitValue, propertyStatus } = useComponentPropsInspectorInfo(
      propPath,
      isScene,
      controlDescription,
    )

    const rowHeight = UtopiaTheme.layout.rowHeight.max
    const transformedValue = Array.isArray(value) ? value : [value]
    const { springs, bind } = useArraySuperControl(
      transformedValue,
      onSubmitValue,
      rowHeight,
      false,
    )
    const [insertingRow, setInsertingRow] = React.useState(false)

    const toggleInsertRow = React.useCallback(() => setInsertingRow((current) => !current), [])

    React.useEffect(() => setInsertingRow(false), [springs.length])

    return (
      <React.Fragment>
        <InspectorSectionHeader>
          <SimpleFlexRow style={{ flexGrow: 1 }}>
            <PropertyLabel target={[propPath]} style={{ textTransform: 'capitalize' }}>
              {title}
            </PropertyLabel>
            {propertyStatus.overwritable ? (
              <SquareButton highlight onMouseDown={toggleInsertRow}>
                {insertingRow ? (
                  <Icons.Minus
                    style={{ paddingTop: 1 }}
                    color={propertyStatus.controlled ? 'primary' : 'secondary'}
                    width={16}
                    height={16}
                  />
                ) : (
                  <Icons.Plus
                    style={{ paddingTop: 1 }}
                    color={propertyStatus.controlled ? 'primary' : 'secondary'}
                    width={16}
                    height={16}
                  />
                )}
              </SquareButton>
            ) : null}
          </SimpleFlexRow>
        </InspectorSectionHeader>
        <div
          style={{
            height: rowHeight * springs.length,
          }}
        >
          {springs.map((springStyle, index) => {
            return (
              <animated.div
                {...bind(index)}
                key={index}
                style={{
                  ...springStyle,
                  width: '100%',
                  position: 'absolute',
                  height: rowHeight,
                }}
              >
                <RowForControl
                  controlDescription={controlDescription.propertyControl}
                  isScene={isScene}
                  propPath={PP.appendPropertyPathElems(propPath, [index])}
                  setGlobalCursor={props.setGlobalCursor}
                  indentationLevel={1}
                />
              </animated.div>
            )
          })}
        </div>
        {insertingRow ? (
          <RowForControl
            controlDescription={controlDescription.propertyControl}
            isScene={isScene}
            propPath={PP.appendPropertyPathElems(propPath, [springs.length])}
            setGlobalCursor={props.setGlobalCursor}
            indentationLevel={1}
          />
        ) : null}
      </React.Fragment>
    )
  },
)

interface ObjectIndicatorProps {
  open: boolean
}

const ObjectIndicator = (props: ObjectIndicatorProps) => {
  const colorTheme = useColorTheme()
  return (
    <div
      style={{
        border: `1px solid ${colorTheme.bg3.value}`,
        paddingLeft: 2,
        paddingRight: 2,
        borderRadius: 4,
        lineHeight: 1,
        fontSize: 9,
        color: colorTheme.fg6.value,
        background: props.open ? 'transparent' : colorTheme.bg2.value,
      }}
    >
      ⋯
    </div>
  )
}

interface RowForObjectControlProps extends AbstractRowForControlProps {
  controlDescription: ObjectControlDescription
}

const RowForObjectControl = betterReactMemo(
  'RowForObjectControl',
  (props: RowForObjectControlProps) => {
    const [open, setOpen] = React.useState(true)
    const handleOnClick = React.useCallback(() => setOpen(!open), [setOpen, open])
    const { propPath, controlDescription, isScene } = props
    const title = titleForControl(propPath, controlDescription)
    const indentation = props.indentationLevel * 8

    return (
      <div
        css={{
          marginTop: 8,
          marginBottom: 8,
          '&:hover': {
            boxShadow: 'inset 1px 0px 0px 0px hsla(0,0%,0%,20%)',
            background: 'hsl(0,0%,0%,1%)',
          },
          '&:focus-within': {
            boxShadow: 'inset 1px 0px 0px 0px hsla(0,0%,0%,20%)',
            background: 'hsl(0,0%,0%,1%)',
          },
        }}
      >
        <div>
          <div onClick={handleOnClick}>
            <SimpleFlexRow style={{ flexGrow: 1 }}>
              <PropertyLabel
                target={[propPath]}
                style={{
                  textTransform: 'capitalize',
                  paddingLeft: indentation,
                  display: 'flex',
                  alignItems: 'center',
                  height: 34,
                  fontWeight: 500,
                  gap: 4,
                  cursor: 'pointer',
                }}
              >
                {title}
                <ObjectIndicator open={open} />
              </PropertyLabel>
            </SimpleFlexRow>
          </div>
          {when(
            open,
            mapToArray((innerControl: RegularControlDescription, prop: string) => {
              const innerPropPath = PP.appendPropertyPathElems(propPath, [prop])
              return (
                <RowForControl
                  key={`object-control-row-${PP.toString(innerPropPath)}`}
                  controlDescription={innerControl}
                  isScene={isScene}
                  propPath={innerPropPath}
                  setGlobalCursor={props.setGlobalCursor}
                  indentationLevel={props.indentationLevel + 1}
                />
              )
            }, controlDescription.object),
          )}
        </div>
      </div>
    )
  },
)

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

    const onLabelChangeValue = React.useCallback(
      (option: SelectOption) => {
        if (option.value !== controlToUse) {
          setControlToUse(option.value)
        }
      },
      [controlToUse, setControlToUse],
    )

    const simpleControlStyles = getControlStyles('simple')

    const label = React.useMemo(
      () => (
        <PopupList
          value={{
            value: controlToUse,
            label: title,
          }}
          options={labelOptions}
          onSubmitValue={onLabelChangeValue}
          containerMode='showBorderOnHover'
          controlStyles={simpleControlStyles}
          style={{
            maxWidth: '100%',
            overflow: 'hidden',
          }}
        />
      ),
      [controlToUse, labelOptions, onLabelChangeValue, simpleControlStyles, title],
    )

    const labelAsRenderProp = React.useCallback(() => label, [label])

    if (controlToUse == null) {
      return null
    } else if (isBaseControlDescription(controlToUse)) {
      return (
        <RowForBaseControl {...props} label={labelAsRenderProp} controlDescription={controlToUse} />
      )
    } else {
      return (
        <React.Fragment>
          {label}
          <RowForControl {...props} controlDescription={controlToUse} />
        </React.Fragment>
      )
    }
  },
)

interface RowForControlProps extends AbstractRowForControlProps {
  controlDescription: RegularControlDescription
}

export const RowForControl = betterReactMemo('RowForControl', (props: RowForControlProps) => {
  const { controlDescription } = props
  if (isBaseControlDescription(controlDescription)) {
    return <RowForBaseControl {...props} controlDescription={controlDescription} />
  } else {
    switch (controlDescription.type) {
      case 'array':
        return <RowForArrayControl {...props} controlDescription={controlDescription} />
      case 'object':
        return <RowForObjectControl {...props} controlDescription={controlDescription} />
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
    const colorTheme = useColorTheme()

    const propertyControlsAndTargets = useKeepReferenceEqualityIfPossible(
      useGetPropertyControlsForSelectedComponents(),
    )

    const [sectionExpanded, setSectionExpanded] = React.useState(true)
    const toggleSection = React.useCallback(() => {
      setSectionExpanded((currentlyExpanded) => !currentlyExpanded)
    }, [setSectionExpanded])

    return (
      <React.Fragment>
        <InspectorSectionHeader>
          <FlexRow style={{ flexGrow: 1, color: colorTheme.primary.value, gap: 8 }}>
            <Icons.Component color='primary' />
            <span>Component </span>
          </FlexRow>
          <SquareButton highlight onClick={toggleSection}>
            <ExpandableIndicator
              testId='component-section-expand'
              visible
              collapsed={!sectionExpanded}
              selected={false}
            />
          </SquareButton>
        </InspectorSectionHeader>
        {when(
          sectionExpanded,
          <React.Fragment>
            {/* Information about the component as a whole */}
            <ComponentInfoBox />
            {/* List of component props with controls */}
            {propertyControlsAndTargets.map((controlsAndTargets) => (
              <PropertyControlsSection
                key={EP.toString(controlsAndTargets.targets[0])}
                propertyControls={controlsAndTargets.controls}
                targets={controlsAndTargets.targets}
                isScene={props.isScene}
                detectedPropsAndValuesWithoutControls={
                  controlsAndTargets.detectedPropsAndValuesWithoutControls
                }
                detectedPropsWithNoValue={controlsAndTargets.detectedPropsWithNoValue}
                propsWithControlsButNoValue={controlsAndTargets.propsWithControlsButNoValue}
              />
            ))}
          </React.Fragment>,
        )}
      </React.Fragment>
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
        <React.Fragment>
          <InspectorSectionHeader>Component props</InspectorSectionHeader>

          <PropertyRow
            style={{
              gridTemplateColumns: '2fr 4fr',
            }}
          >
            <span style={{ paddingTop: 4, color: colorThemeConst.errorForeground.value }}>
              Invalid propertyControls value
            </span>
          </PropertyRow>
        </React.Fragment>
      )
    } else {
      return <ComponentSectionInner {...this.props} />
    }
  }
}
