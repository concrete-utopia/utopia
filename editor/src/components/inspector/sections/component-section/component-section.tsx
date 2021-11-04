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
  CheckboxPropertyControl,
  ColorPropertyControl,
  PopUpListPropertyControl,
  EulerPropertyControl,
  ExpressionPopUpListPropertyControl,
  Matrix3PropertyControl,
  Matrix4PropertyControl,
  NumberInputPropertyControl,
  RadioPropertyControl,
  ControlForPropProps,
  ExpressionInputPropertyControl,
  StringInputPropertyControl,
  VectorPropertyControl,
} from './property-control-controls'
import { ComponentInfoBox } from './component-info-box'
import { ExpandableIndicator } from '../../../navigator/navigator-item/expandable-indicator'
import { when } from '../../../../utils/react-conditionals'
import { PropertyControlsSection } from './property-controls-section'
import type { ReactEventHandlers } from 'react-use-gesture/dist/types'

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
      switch (controlDescription.control) {
        case 'checkbox':
          return <CheckboxPropertyControl {...props} controlDescription={controlDescription} />
        case 'color':
          return <ColorPropertyControl {...props} controlDescription={controlDescription} />
        case 'euler':
          return <EulerPropertyControl {...props} controlDescription={controlDescription} />
        case 'expression-input':
          return (
            <ExpressionInputPropertyControl {...props} controlDescription={controlDescription} />
          )
        case 'expression-popuplist':
          return (
            <ExpressionPopUpListPropertyControl
              {...props}
              controlDescription={controlDescription}
            />
          )
        case 'none':
          return null
        case 'matrix3':
          return <Matrix3PropertyControl {...props} controlDescription={controlDescription} />
        case 'matrix4':
          return <Matrix4PropertyControl {...props} controlDescription={controlDescription} />
        case 'number-input':
          return <NumberInputPropertyControl {...props} controlDescription={controlDescription} />
        case 'popuplist':
          return <PopUpListPropertyControl {...props} controlDescription={controlDescription} />
        case 'radio':
          return <RadioPropertyControl {...props} controlDescription={controlDescription} />
        case 'string-input':
          return <StringInputPropertyControl {...props} controlDescription={controlDescription} />
        case 'style-controls':
          return null
        case 'vector2':
        case 'vector3':
        case 'vector4':
          return <VectorPropertyControl {...props} controlDescription={controlDescription} />
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
  focusOnMount: boolean
}

function labelForControl(propPath: PropertyPath, control: RegularControlDescription): string {
  return control.label ?? PP.lastPartToString(propPath)
}

function getLabelControlStyle(
  controlDescription: ControlDescription,
  propMetadata: InspectorInfo<any>,
): ControlStyles {
  if (
    (controlDescription.control === 'expression-input' ||
      controlDescription.control === 'expression-popuplist') &&
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
  const title = labelForControl(propPath, controlDescription)
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

  if (controlDescription.control === 'none') {
    // do not list anything for `none` controls
    return null
  }

  return (
    <InspectorContextMenuWrapper
      id={`context-menu-for-${propName}`}
      items={contextMenuItems}
      data={null}
    >
      <UIGridRow
        padded={false}
        style={{ paddingLeft: 0, paddingRight: 8 }}
        variant='<--1fr--><--1fr-->'
      >
        {propertyLabel}
        <ControlForProp
          propPath={propPath}
          propName={propName}
          controlDescription={controlDescription}
          propMetadata={propMetadata}
          setGlobalCursor={props.setGlobalCursor}
          focusOnMount={props.focusOnMount}
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
    const title = labelForControl(propPath, controlDescription)
    const { value, onSubmitValue, propertyStatus } = useComponentPropsInspectorInfo(
      propPath,
      isScene,
      controlDescription,
    )

    const rowHeight = UtopiaTheme.layout.rowHeight.normal
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
          {springs.map((springStyle, index) => (
            <ArrayControlItem
              springStyle={springStyle}
              bind={bind}
              key={index} //FIXME this causes the row drag handle to jump after finishing the re-order
              index={index}
              propPath={propPath}
              isScene={props.isScene}
              controlDescription={controlDescription}
              focusOnMount={props.focusOnMount}
              setGlobalCursor={props.setGlobalCursor}
            />
          ))}
        </div>
        {insertingRow ? (
          <RowForControl
            controlDescription={controlDescription.propertyControl}
            isScene={isScene}
            propPath={PP.appendPropertyPathElems(propPath, [springs.length])}
            setGlobalCursor={props.setGlobalCursor}
            indentationLevel={1}
            focusOnMount={false}
          />
        ) : null}
      </React.Fragment>
    )
  },
)
interface ArrayControlItemProps {
  springStyle: { [x: string]: any; [x: number]: any; [x: symbol]: any }
  bind: (...args: any[]) => ReactEventHandlers
  propPath: PropertyPath
  index: number
  isScene: boolean
  controlDescription: ArrayControlDescription
  focusOnMount: boolean
  setGlobalCursor: (cursor: CSSCursor | null) => void
}

const ArrayControlItem = betterReactMemo('ArrayControlItem', (props: ArrayControlItemProps) => {
  const colorTheme = useColorTheme()
  const { bind, propPath, index, isScene, springStyle, controlDescription } = props
  const propPathWithIndex = PP.appendPropertyPathElems(propPath, [index])
  const propMetadata = useComponentPropsInspectorInfo(
    propPathWithIndex,
    isScene,
    controlDescription,
  )
  const contextMenuItems = Utils.stripNulls([addOnUnsetValues([index], propMetadata.onUnsetValues)])

  const rowHeight = UtopiaTheme.layout.rowHeight.normal
  return (
    <InspectorContextMenuWrapper
      id={`context-menu-for-${PP.toString(propPathWithIndex)}`}
      items={contextMenuItems}
      data={null}
      key={index}
    >
      <animated.div
        {...bind(index)}
        style={{
          ...springStyle,
          width: '100%',
          position: 'absolute',
          height: rowHeight,
        }}
        css={{
          '& > .handle': {
            opacity: 0,
          },
          '&:hover > .handle': {
            opacity: 1,
          },
        }}
      >
        <RowForControl
          controlDescription={controlDescription.propertyControl}
          isScene={isScene}
          propPath={PP.appendPropertyPathElems(propPath, [index])}
          setGlobalCursor={props.setGlobalCursor}
          indentationLevel={1}
          focusOnMount={props.focusOnMount && index === 0}
        />
        <div
          style={{
            position: 'absolute',
            top: 0,
            bottom: 0,
            display: 'flex',
            alignItems: 'center',
          }}
          className='handle'
        >
          <svg width='5px' height='23px' viewBox='0 0 4 23'>
            <g
              stroke={colorTheme.border3.value}
              strokeWidth='1'
              fill='none'
              fillRule='evenodd'
              strokeLinecap='round'
            >
              <line x1='1' y1='1.5' x2='1' y2='21'></line>
              <line x1='4' y1='1.5' x2='4' y2='21'></line>
            </g>
          </svg>
        </div>
      </animated.div>
    </InspectorContextMenuWrapper>
  )
})

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
    const title = labelForControl(propPath, controlDescription)
    const indentation = props.indentationLevel * 8

    const propMetadata = useComponentPropsInspectorInfo(propPath, isScene, controlDescription)
    const contextMenuItems = Utils.stripNulls([
      addOnUnsetValues([PP.lastPart(propPath)], propMetadata.onUnsetValues),
    ])

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
        <div onClick={handleOnClick}>
          <InspectorContextMenuWrapper
            id={`context-menu-for-${PP.toString(propPath)}`}
            items={contextMenuItems}
            data={null}
          >
            <SimpleFlexRow style={{ flexGrow: 1, paddingRight: 8 }}>
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
          </InspectorContextMenuWrapper>
        </div>
        {when(
          open,
          mapToArray((innerControl: RegularControlDescription, prop: string, index: number) => {
            const innerPropPath = PP.appendPropertyPathElems(propPath, [prop])
            return (
              <RowForControl
                key={`object-control-row-${PP.toString(innerPropPath)}`}
                controlDescription={innerControl}
                isScene={isScene}
                propPath={innerPropPath}
                setGlobalCursor={props.setGlobalCursor}
                indentationLevel={props.indentationLevel + 1}
                focusOnMount={props.focusOnMount && index === 0}
              />
            )
          }, controlDescription.object),
        )}
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
    const title = labelForControl(propPath, controlDescription)

    const suitableControl = useControlForUnionControl(propPath, controlDescription)
    const [controlToUse, setControlToUse] = React.useState(suitableControl)

    const labelOptions: OptionsType<SelectOption> = controlDescription.controls.map((control) => {
      const label = control.label ?? control.control
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
        <RowForBaseControl
          {...props}
          label={labelAsRenderProp}
          controlDescription={controlToUse}
          focusOnMount={false}
        />
      )
    } else {
      return (
        <React.Fragment>
          {label}
          <RowForControl {...props} controlDescription={controlToUse} focusOnMount={false} />
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
    switch (controlDescription.control) {
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
