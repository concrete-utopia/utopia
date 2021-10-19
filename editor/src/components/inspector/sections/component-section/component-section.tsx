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
import { filterSpecialProps } from '../../../../core/property-controls/property-controls-utils'
import {
  eitherToMaybe,
  foldEither,
  forEachRight,
  isLeft,
  right,
} from '../../../../core/shared/either'
import { mapToArray, mapValues } from '../../../../core/shared/object-utils'
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
import { setCursorOverlay } from '../../../editor/actions/action-creators'
import { useEditorState } from '../../../editor/store/store-hook'
import { addOnUnsetValues } from '../../common/context-menu-items'
import {
  useControlForUnionControl,
  useControlStatusForPaths,
  useInspectorInfoForPropertyControl,
} from '../../common/property-controls-hooks'
import { ControlStyles } from '../../common/control-status'
import {
  InspectorInfo,
  InspectorPropsContext,
  InspectorPropsContextData,
  useGivenPropsAndValuesWithoutControls,
  useGivenPropsWithoutControls,
  useSelectedPropertyControls,
  useUsedPropsWithoutControls,
} from '../../common/property-path-hooks'
import { useArraySuperControl } from '../../controls/array-supercontrol'
import { SelectOption } from '../../controls/select-control'
import { UIGridRow } from '../../widgets/ui-grid-row'
import { PropertyLabel } from '../../widgets/property-label'
import { PropertyRow } from '../../widgets/property-row'
import {
  ControlForBooleanProp,
  ControlForColorProp,
  ControlForComponentInstanceProp,
  ControlForEnumProp,
  ControlForEventHandlerProp,
  ControlForExpressionEnumProp,
  ControlForImageProp,
  ControlForNumberProp,
  ControlForOptionsProp,
  ControlForPopupListProp,
  ControlForPropProps,
  ControlForStringProp,
  ControlForVectorProp,
} from './property-control-controls'
import {
  filterNonUnsetAndEmptyControls,
  HiddenControls,
  useHiddenElements,
} from './hidden-controls-section'
import { ComponentInfoBox } from './component-info-box'
import {
  ParsedPropertyControls,
  parseStringValidateAsColor,
} from '../../../../core/property-controls/property-controls-parser'
import { getPropertyControlNames } from '../../../../core/property-controls/property-control-values'
import { ExpandableIndicator } from '../../../navigator/navigator-item/expandable-indicator'
import { when } from '../../../../utils/react-conditionals'
import { useContext } from 'use-context-selector'
import { getJSXAttribute, jsxAttributeValue } from '../../../../core/shared/element-template'
import { jsxSimpleAttributeToValue } from '../../../../core/shared/jsx-attributes'
import { UTOPIA_PATHS_KEY, UTOPIA_UIDS_KEY } from '../../../../core/model/utopia-constants'

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
        case 'componentinstance':
          return (
            <ControlForComponentInstanceProp {...props} controlDescription={controlDescription} />
          )
        case 'enum':
          return <ControlForEnumProp {...props} controlDescription={controlDescription} />
        case 'expression-enum':
          return <ControlForExpressionEnumProp {...props} controlDescription={controlDescription} />
        case 'eventhandler':
          return <ControlForEventHandlerProp {...props} controlDescription={controlDescription} />
        case 'ignore':
          return null
        case 'image':
          return <ControlForImageProp {...props} controlDescription={controlDescription} />
        case 'number':
          return <ControlForNumberProp {...props} controlDescription={controlDescription} />
        case 'options':
          return <ControlForOptionsProp {...props} controlDescription={controlDescription} />
        case 'popuplist':
          return <ControlForPopupListProp {...props} controlDescription={controlDescription} />
        case 'string':
          return <ControlForStringProp {...props} controlDescription={controlDescription} />
        case 'vector2':
        case 'vector3':
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

const RowForInvalidControl = betterReactMemo(
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
          <div onClick={() => setOpen(!open)}>
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

const RowForControl = betterReactMemo('RowForControl', (props: RowForControlProps) => {
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
      useSelectedPropertyControls(false),
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
              />
            ))}
          </React.Fragment>,
        )}
      </React.Fragment>
    )
  },
)

function useFilterPropsContext(paths: ElementPath[]): InspectorPropsContextData {
  const currentContext = useContext(InspectorPropsContext)
  const spiedProps = currentContext.spiedProps.filter((props) =>
    paths.some((path) => EP.toString(path) === props[UTOPIA_PATHS_KEY]),
  )
  const editedMultiSelectedProps = currentContext.editedMultiSelectedProps.filter((attributes) => {
    const dataUidAttribute = getJSXAttribute(attributes, UTOPIA_UIDS_KEY)
    if (dataUidAttribute != null) {
      const uid = eitherToMaybe(jsxSimpleAttributeToValue(dataUidAttribute))
      return paths.some((path) => EP.toUid(path) === uid)
    } else {
      return false
    }
  })

  return {
    ...currentContext,
    spiedProps,
    editedMultiSelectedProps,
    selectedViews: paths,
  }
}

interface PropertyControlsSectionProps {
  targets: ElementPath[]
  propertyControls: ParseResult<ParsedPropertyControls>
  isScene: boolean
}

const PropertyControlsSection = betterReactMemo(
  'PropertyControlsSection',
  (props: PropertyControlsSectionProps) => {
    const { targets, propertyControls } = props

    const detectedPropsWithoutControls = useKeepReferenceEqualityIfPossible(
      useGivenPropsWithoutControls(targets),
    )
    const detectedPropsWithNoValue = useKeepReferenceEqualityIfPossible(
      useUsedPropsWithoutControls(detectedPropsWithoutControls, targets),
    )
    const detectedPropsAndValuesWithoutControls = useKeepReferenceEqualityIfPossible(
      useGivenPropsAndValuesWithoutControls(targets),
    )

    const dispatch = useEditorState((state) => state.dispatch, 'ComponentSectionInner')

    const setGlobalCursor = React.useCallback(
      (cursor: CSSCursor | null) => {
        dispatch([setCursorOverlay(cursor)], 'everyone')
      },
      [dispatch],
    )

    const propPaths = React.useMemo(() => {
      return foldEither(
        () => [],
        (success) => {
          return filterSpecialProps(getPropertyControlNames(success)).map((name) => {
            return PP.create([name])
          })
        },
        propertyControls,
      )
    }, [propertyControls])

    const propertyControlsStatus = useControlStatusForPaths(propPaths)
    const [visibleEmptyControls, showHiddenControl] = useHiddenElements()

    const updatedContext = useKeepReferenceEqualityIfPossible(useFilterPropsContext(targets))

    return (
      <InspectorPropsContext.Provider value={updatedContext}>
        {foldEither(
          (rootParseError) => {
            return <ParseErrorControl parseError={rootParseError} />
          },
          (rootParseSuccess) => {
            const propNamesToDisplay = new Set(
              filterNonUnsetAndEmptyControls(
                filterSpecialProps(getPropertyControlNames(rootParseSuccess)),
                propertyControlsStatus,
                visibleEmptyControls,
              ),
            )

            return (
              <React.Fragment>
                {Object.keys(rootParseSuccess).map((propName) => {
                  const propertyControl = rootParseSuccess[propName]
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
                    (propertySuccess) => {
                      return (
                        <SectionRow
                          key={propName}
                          propPath={PP.create([propName])}
                          isScene={props.isScene}
                          setGlobalCursor={setGlobalCursor}
                          controlDescription={propertySuccess}
                          propNamesToDisplay={propNamesToDisplay}
                          indentationLevel={1}
                        />
                      )
                    },
                    propertyControl,
                  )
                })}
              </React.Fragment>
            )
          },
          propertyControls,
        )}
        {Object.keys(detectedPropsAndValuesWithoutControls).map((propName) => {
          const propValue = detectedPropsAndValuesWithoutControls[propName]
          const controlDescription: ControlDescription = inferControlTypeBasedOnValue(
            propValue,
            propName,
          )
          return (
            <RowForControl
              key={propName}
              propPath={PP.create([propName])}
              controlDescription={controlDescription}
              isScene={props.isScene}
              setGlobalCursor={setGlobalCursor}
              indentationLevel={1}
            />
          )
        })}
        <HiddenControls
          propertyControls={propertyControls}
          propertyControlsStatus={propertyControlsStatus}
          visibleEmptyControls={visibleEmptyControls}
          showHiddenControl={showHiddenControl}
          setGlobalCursor={setGlobalCursor}
        />
        {/** props set on the component instance and props used inside the component code */}
        {detectedPropsWithNoValue.length > 0 ? (
          <UIGridRow padded tall={false} variant={'<-------------1fr------------->'}>
            <div>
              <VerySubdued>{`Unused props: ${detectedPropsWithNoValue.join(', ')}.`}</VerySubdued>
            </div>
          </UIGridRow>
        ) : null}
      </InspectorPropsContext.Provider>
    )
  },
)

function inferControlTypeBasedOnValueInner(
  stackSize: number,
  propValue: any,
  propName?: string,
): RegularControlDescription {
  if (stackSize > 100) {
    // Prevent this blowing out on recursive structures
    return {
      type: 'ignore',
    }
  }

  switch (typeof propValue) {
    case 'number':
      return {
        type: 'number',
        title: propName,
      }
    case 'string': {
      const parsedAsColor = parseStringValidateAsColor(propValue)
      const controlType = isLeft(parsedAsColor) ? 'string' : 'color'
      return {
        type: controlType,
        title: propName,
      }
    }
    case 'boolean': {
      return {
        type: 'boolean',
        title: propName,
      }
    }
    case 'object': {
      if (propValue == null || React.isValidElement(propValue) || propName === 'style') {
        return {
          type: 'ignore',
        }
      } else if (Array.isArray(propValue)) {
        if (
          (propValue.length === 2 || propValue.length === 3) &&
          propValue.every((v) => typeof v === 'number')
        ) {
          // First we try to find Vectors
          if (propValue.length === 2) {
            return {
              type: 'vector2',
              title: propName,
            }
          } else {
            return {
              type: 'vector3',
              title: propName,
            }
          }
        } else if (propValue.length > 0) {
          // Otherwise we go with a regular array control
          return {
            type: 'array',
            title: propName,
            propertyControl: inferControlTypeBasedOnValueInner(stackSize + 1, propValue[0]),
          }
        } else {
          // We can't infer the underlying control type for empty arrays, so our hands are tied here
          return {
            type: 'ignore',
          }
        }
      } else {
        const controlsForKeys = mapValues(
          (v: unknown, key: string) => inferControlTypeBasedOnValueInner(stackSize + 1, v, key),
          propValue,
        )

        return {
          type: 'object',
          title: propName,
          object: controlsForKeys,
        }
      }
    }
    default:
      return {
        type: 'ignore',
      }
  }
}

export function inferControlTypeBasedOnValue(
  propValue: any,
  propName?: string,
): RegularControlDescription {
  return inferControlTypeBasedOnValueInner(0, propValue, propName)
}

type SectionRowProps = {
  propPath: PropertyPath
  isScene: boolean
  setGlobalCursor: (cursor: CSSCursor | null) => void
  controlDescription: ControlDescription
  propNamesToDisplay: Set<string>
  indentationLevel: number
}

export const SectionRow = betterReactMemo('SectionRow', (props: SectionRowProps) => {
  switch (props.controlDescription.type) {
    case 'folder':
      const innerProperties = getPropertyControlNames({ folder: right(props.controlDescription) })
      const anyInnerControlsToDisplay = innerProperties.some((controlForFolder) => {
        return props.propNamesToDisplay.has(controlForFolder)
      })
      if (anyInnerControlsToDisplay) {
        return <FolderSection {...props} controlDescription={props.controlDescription} />
      } else {
        return null
      }
    default:
      if (props.propNamesToDisplay.has(PP.toString(props.propPath))) {
        return (
          <UIGridRow
            padded
            tall={false}
            style={{ paddingLeft: 0 }}
            variant='<-------------1fr------------->'
          >
            <RowForControl
              propPath={props.propPath}
              controlDescription={props.controlDescription}
              isScene={props.isScene}
              setGlobalCursor={props.setGlobalCursor}
              indentationLevel={props.indentationLevel}
            />
          </UIGridRow>
        )
      } else {
        return null
      }
  }
})

interface ExpansionArrowSVGProps {
  style: React.CSSProperties
}

const ExpansionArrowSVG = betterReactMemo('ExpansionArrowSVG', (props: ExpansionArrowSVGProps) => {
  const colorTheme = useColorTheme()
  return (
    <svg width='7px' height='5px' viewBox='0 0 7 5' version='1.1' style={props.style}>
      <g
        strokeWidth='1'
        fillRule='evenodd'
        strokeLinecap='round'
        strokeLinejoin='round'
        id='expansion-triangle-open'
        transform='translate(-1.000000, 0.000000)'
        fill={colorTheme.textColor.value}
        stroke={colorTheme.textColor.value}
      >
        <polygon
          transform='translate(3.828427, 0.828427) rotate(-45.000000) translate(-3.828427, -0.828427) '
          points='1.82842712 -1.17157288 1.82842712 2.82842712 5.82842712 2.82842712'
        />
      </g>
    </svg>
  )
})

interface FolderSectionProps {
  propPath: PropertyPath
  isScene: boolean
  setGlobalCursor: (cursor: CSSCursor | null) => void
  controlDescription: FolderControlDescription
  propNamesToDisplay: Set<string>
  indentationLevel: number
}

const FolderSection = betterReactMemo('FolderSection', (props: FolderSectionProps) => {
  const [open, setOpen] = React.useState(true)
  const controls = props.controlDescription.controls
  const indentation = props.indentationLevel * 8
  return (
    <div
      css={{
        '&:hover': {
          boxShadow: 'inset 1px 0px 0px 0px hsla(0,0%,0%,30%)',
          background: 'hsl(0,0%,0%,1%)',
        },
        '&:focus-within': {
          boxShadow: 'inset 1px 0px 0px 0px hsla(0,0%,0%,30%)',
          background: 'hsl(0,0%,0%,1%)',
        },
      }}
    >
      {/* TODO BEFORE MERGE use something like the PropertyLabel */}
      <div
        style={{
          paddingLeft: indentation,
          display: 'flex',
          alignItems: 'center',
          height: 34,
          fontWeight: 500,
          gap: 4,
          cursor: 'pointer',
        }}
        onClick={() => setOpen(!open)}
      >
        <ExpansionArrowSVG
          style={{
            transform: open ? 'none' : 'rotate(-90deg)',
            transition: 'all linear .1s',
          }}
        />
        <span>{props.controlDescription.title ?? PP.toString(props.propPath)}</span>
      </div>
      {when(
        open,
        Object.keys(controls).map((propName) => {
          const controlDescription = controls[propName]
          return (
            <SectionRow
              key={`section-row-${propName}`}
              propPath={PP.create([propName])}
              controlDescription={controlDescription}
              isScene={props.isScene}
              setGlobalCursor={props.setGlobalCursor}
              propNamesToDisplay={props.propNamesToDisplay}
              indentationLevel={props.indentationLevel + 1}
            />
          )
        }),
      )}
    </div>
  )
})

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
