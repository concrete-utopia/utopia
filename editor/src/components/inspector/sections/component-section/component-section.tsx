/** @jsxRuntime classic */
/** @jsx jsx */
import React from 'react'
import { css, jsx } from '@emotion/react'
import type { OptionsType } from 'react-select'
import { animated } from 'react-spring'
import type {
  ArrayControlDescription,
  BaseControlDescription,
  ControlDescription,
  ObjectControlDescription,
  RegularControlDescription,
  TupleControlDescription,
  UnionControlDescription,
} from 'utopia-api/core'
import {
  FolderControlDescription,
  HigherLevelControlDescription,
  isBaseControlDescription,
  PropertyControls,
} from 'utopia-api/core'
import { PathForSceneProps } from '../../../../core/model/scene-utils'
import { mapToArray } from '../../../../core/shared/object-utils'
import type { PropertyPath } from '../../../../core/shared/project-file-types'
import { ElementPath } from '../../../../core/shared/project-file-types'
import * as PP from '../../../../core/shared/property-path'
import * as EP from '../../../../core/shared/element-path'
import { useKeepReferenceEqualityIfPossible } from '../../../../utils/react-performance'
import Utils from '../../../../utils/utils'
import type { ParseError } from '../../../../utils/value-parser-utils'
import { getParseErrorDetails, ParseResult } from '../../../../utils/value-parser-utils'
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
  Button,
  Icn,
} from '../../../../uuiui'
import type { CSSCursor } from '../../../../uuiui-deps'
import { getControlStyles } from '../../../../uuiui-deps'
import { InspectorContextMenuWrapper } from '../../../context-menu-wrapper'
import { addOnUnsetValues } from '../../common/context-menu-items'
import {
  useControlForUnionControl,
  useGetPropertyControlsForSelectedComponents,
  useInspectorInfoForPropertyControl,
} from '../../common/property-controls-hooks'
import type { ControlStyles } from '../../common/control-styles'
import { ControlStatus } from '../../common/control-status'
import type { InspectorInfo } from '../../common/property-path-hooks'
import { useArraySuperControl } from '../../controls/array-supercontrol'
import type { SelectOption } from '../../controls/select-control'
import { UIGridRow } from '../../widgets/ui-grid-row'
import { PropertyLabel } from '../../widgets/property-label'
import { PropertyRow } from '../../widgets/property-row'
import type { ControlForPropProps } from './property-control-controls'
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
  ExpressionInputPropertyControl,
  StringInputPropertyControl,
  VectorPropertyControl,
} from './property-control-controls'
import { ExpandableIndicator } from '../../../navigator/navigator-item/expandable-indicator'
import { unless, when } from '../../../../utils/react-conditionals'
import { PropertyControlsSection } from './property-controls-section'
import type { ReactEventHandlers } from 'react-use-gesture/dist/types'
import { normalisePathToUnderlyingTarget } from '../../../custom-code/code-file'
import { openCodeEditorFile, setProp_UNSAFE } from '../../../editor/actions/action-creators'
import { Substores, useEditorState, useRefEditorState } from '../../../editor/store/store-hook'
import { MetadataUtils } from '../../../../core/model/element-metadata-utils'
import { getFilePathForImportedComponent } from '../../../../core/model/project-file-utils'
import { safeIndex } from '../../../../core/shared/array-utils'
import { useDispatch } from '../../../editor/store/dispatch-context'
import { usePopper } from 'react-popper'
import {
  jsExpressionOtherJavaScript,
  jsExpressionOtherJavaScriptSimple,
} from '../../../../core/shared/element-template'
import { optionalMap } from '../../../../core/shared/optional-utils'

function useComponentPropsInspectorInfo(
  partialPath: PropertyPath,
  addPropsToPath: boolean,
  control: RegularControlDescription,
) {
  const propertyPath = addPropsToPath ? PP.append(PathForSceneProps, partialPath) : partialPath
  return useInspectorInfoForPropertyControl(propertyPath, control)
}

const ControlForProp = React.memo((props: ControlForPropProps<BaseControlDescription>) => {
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
        return <ExpressionInputPropertyControl {...props} controlDescription={controlDescription} />
      case 'expression-popuplist':
        return (
          <ExpressionPopUpListPropertyControl {...props} controlDescription={controlDescription} />
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
})
interface ParseErrorProps {
  parseError: ParseError
}

export const ParseErrorControl = React.memo((props: ParseErrorProps) => {
  const details = getParseErrorDetails(props.parseError)
  return (
    <div>
      <Tooltip title={`${details.path}`}>
        <span>{details.description}</span>
      </Tooltip>
    </div>
  )
})

const WarningTooltip = React.memo(({ warning }: { warning: string }) => {
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
  label?: React.ComponentType<React.PropsWithChildren<any>> // TODO Before Merge this probably should not be a component
  controlDescription: BaseControlDescription
}

const RowForBaseControl = React.memo((props: RowForBaseControlProps) => {
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
        style={{
          textTransform: 'capitalize',
          paddingLeft: indentation,
          alignSelf: 'flex-start',
        }}
      >
        <Tooltip title={title}>
          <span
            style={{
              marginTop: 3,
              lineHeight: `${UtopiaTheme.layout.inputHeight.default}px`,
            }}
          >
            {title}
          </span>
        </Tooltip>
      </PropertyLabel>
    ) : (
      <props.label />
    )

  const [referenceElement, setReferenceElement] = React.useState<HTMLDivElement | null>(null)
  const [popperElement, setPopperElement] = React.useState<HTMLDivElement | null>(null)
  const { styles, attributes } = usePopper(referenceElement, popperElement, {
    modifiers: [
      {
        name: 'offset',
        options: {
          offset: [0, 8],
        },
      },
    ],
  })

  const [popupIsOpen, setPopupIsOpen] = React.useState(false)
  const togglePopup = React.useCallback(() => setPopupIsOpen((v) => !v), [])
  const closePopup = React.useCallback(() => setPopupIsOpen(false), [])

  const selectedViewPathRef = useRefEditorState((store) => store.editor.selectedViews.at(0) ?? null)
  const variablesInScopeRef = useRefEditorState((store) => store.editor.variablesInScope)
  const dispatch = useDispatch()

  const onTweakProperty = React.useCallback(() => {
    if (selectedViewPathRef.current == null) {
      return
    }

    dispatch([
      setProp_UNSAFE(
        selectedViewPathRef.current,
        PP.create('text'),
        jsExpressionOtherJavaScriptSimple('alternateTitle', ['alternateTitle']),
      ),
    ])
  }, [dispatch, selectedViewPathRef])

  const onClick = React.useCallback(
    (e: React.MouseEvent) => {
      togglePopup()
      e.stopPropagation()
    },
    [togglePopup],
  )

  if (controlDescription.control === 'none') {
    // do not list anything for `none` controls
    return null
  }

  // the actual control
  return (
    <InspectorContextMenuWrapper
      id={`context-menu-for-${propName}`}
      testId={`context-menu-for-${propName}`}
      items={contextMenuItems}
      data={null}
    >
      {popupIsOpen ? (
        <div
          {...attributes.popper}
          style={{ ...styles.popper, border: '1px solid black', zIndex: 1 }}
          ref={setPopperElement}
        >
          <Button onClick={onTweakProperty}>Tweak prop</Button>
        </div>
      ) : null}
      <UIGridRow
        padded={false}
        style={{ paddingLeft: 0, paddingRight: 8, paddingTop: 3, paddingBottom: 3 }}
        variant='<--1fr--><--1fr-->|-18px-|'
      >
        {propertyLabel}
        <div ref={setReferenceElement}>
          <ControlForProp
            propPath={propPath}
            propName={propName}
            controlDescription={controlDescription}
            propMetadata={propMetadata}
            setGlobalCursor={props.setGlobalCursor}
            focusOnMount={props.focusOnMount}
          />
        </div>
        <Button onClick={onClick}>
          <Icn
            type='pipette'
            color='secondary'
            tooltipText={'Pick data source'}
            width={18}
            height={18}
          />
        </Button>
      </UIGridRow>
    </InspectorContextMenuWrapper>
  )
})

function getSectionHeight(controlDescription: ArrayControlDescription): number {
  const rowHeight = UtopiaTheme.layout.rowHeight.normal
  return getSectionHeightFromPropControl(rowHeight, rowHeight, controlDescription.propertyControl)
}

function getSectionHeightFromPropControl(
  accumulatedHeight: number,
  baseHeight: number,
  propertyControl: RegularControlDescription,
): number {
  if (propertyControl.control === 'object') {
    return Object.values(propertyControl.object).reduce<number>(
      (workingHeight, innerPropControl) =>
        getSectionHeightFromPropControl(workingHeight + baseHeight, baseHeight, innerPropControl),
      accumulatedHeight,
    )
  } else {
    return accumulatedHeight
  }
}

interface RowForArrayControlProps extends AbstractRowForControlProps {
  controlDescription: ArrayControlDescription
}

const RowForArrayControl = React.memo((props: RowForArrayControlProps) => {
  const { propPath, controlDescription, isScene } = props
  const title = labelForControl(propPath, controlDescription)
  const { value, onSubmitValue, propertyStatus } = useComponentPropsInspectorInfo(
    propPath,
    isScene,
    controlDescription,
  )

  const sectionHeight = React.useMemo(
    () => getSectionHeight(controlDescription),
    [controlDescription],
  )

  const [insertingRow, setInsertingRow] = React.useState(false)
  const toggleInsertRow = React.useCallback(() => setInsertingRow((current) => !current), [])

  // Ensure the value is an array, just in case.
  const transformedValue = React.useMemo(() => {
    return Array.isArray(value) ? value : [value]
  }, [value])

  // If we are inserting, extend the array with an `undefined` value and
  // then let the handling of the elements cater for that, so that there
  // is no need for special handling of the value(s) being inserted.
  const valueWithInsertingEntry = React.useMemo(() => {
    if (insertingRow) {
      return [...transformedValue, undefined]
    } else {
      return transformedValue
    }
  }, [transformedValue, insertingRow])
  React.useEffect(() => {
    setInsertingRow(false)
  }, [transformedValue.length])

  const { springs, bind } = useArraySuperControl(
    valueWithInsertingEntry,
    onSubmitValue,
    sectionHeight,
    false,
  )

  return (
    <React.Fragment>
      <InspectorSectionHeader>
        <SimpleFlexRow style={{ flexGrow: 1, gap: 5 }}>
          <PropertyLabel target={[propPath]} style={{ textTransform: 'capitalize', paddingTop: 2 }}>
            {title}
          </PropertyLabel>
          {propertyStatus.overwritable ? (
            <SquareButton
              highlight
              onMouseDown={toggleInsertRow}
              data-testid={`toggle-insert-${PP.toString(propPath)}`}
            >
              {insertingRow ? (
                <Icons.Minus
                  color={propertyStatus.controlled ? 'dynamic' : 'secondary'}
                  width={16}
                  height={16}
                />
              ) : (
                <Icons.Plus
                  color={propertyStatus.controlled ? 'dynamic' : 'secondary'}
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
          height: sectionHeight * springs.length,
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
    </React.Fragment>
  )
})
interface ArrayControlItemProps {
  springStyle: { [x: string]: any; [x: number]: any }
  bind: (...args: any[]) => ReactEventHandlers
  propPath: PropertyPath
  index: number
  isScene: boolean
  controlDescription: ArrayControlDescription
  focusOnMount: boolean
  setGlobalCursor: (cursor: CSSCursor | null) => void
}

const ArrayControlItem = React.memo((props: ArrayControlItemProps) => {
  const colorTheme = useColorTheme()
  const { bind, propPath, index, isScene, springStyle, controlDescription } = props
  const propPathWithIndex = PP.appendPropertyPathElems(propPath, [index])
  const propMetadata = useComponentPropsInspectorInfo(
    propPathWithIndex,
    isScene,
    controlDescription,
  )
  const contextMenuItems = Utils.stripNulls([addOnUnsetValues([index], propMetadata.onUnsetValues)])

  const contextMenuId = `context-menu-for-${PP.toString(propPathWithIndex)}`
  return (
    <InspectorContextMenuWrapper
      id={contextMenuId}
      items={contextMenuItems}
      data={null}
      key={contextMenuId}
    >
      <animated.div
        {...bind(index)}
        style={{
          ...springStyle,
          width: '100%',
          position: 'absolute',
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
          disableToggling={true}
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

interface RowForTupleControlProps extends AbstractRowForControlProps {
  controlDescription: TupleControlDescription
}

const RowForTupleControl = React.memo((props: RowForTupleControlProps) => {
  const { propPath, controlDescription, isScene } = props
  const title = labelForControl(propPath, controlDescription)
  const { value, onSubmitValue, propertyStatus } = useComponentPropsInspectorInfo(
    propPath,
    isScene,
    controlDescription,
  )

  const rowHeight = UtopiaTheme.layout.rowHeight.normal
  const transformedValue = Array.isArray(value) ? value : [value]
  const boundedTransformedValue = transformedValue.slice(
    0,
    controlDescription.propertyControls.length,
  )

  return (
    <React.Fragment>
      <InspectorSectionHeader>
        <SimpleFlexRow style={{ flexGrow: 1 }}>
          <PropertyLabel target={[propPath]} style={{ textTransform: 'capitalize' }}>
            {title}
          </PropertyLabel>
        </SimpleFlexRow>
      </InspectorSectionHeader>
      <div
        style={{
          height: rowHeight * boundedTransformedValue.length,
        }}
      >
        {boundedTransformedValue.map((_, index) => (
          <TupleControlItem
            key={index}
            index={index}
            propPath={propPath}
            isScene={props.isScene}
            controlDescription={controlDescription}
            setGlobalCursor={props.setGlobalCursor}
          />
        ))}
      </div>
    </React.Fragment>
  )
})

interface TupleControlItemProps {
  propPath: PropertyPath
  index: number
  isScene: boolean
  controlDescription: TupleControlDescription
  setGlobalCursor: (cursor: CSSCursor | null) => void
}

const TupleControlItem = React.memo((props: TupleControlItemProps) => {
  const { propPath, index, isScene, controlDescription } = props
  const propPathWithIndex = PP.appendPropertyPathElems(propPath, [index])
  const propMetadata = useComponentPropsInspectorInfo(
    propPathWithIndex,
    isScene,
    controlDescription,
  )
  const contextMenuItems = Utils.stripNulls([addOnUnsetValues([index], propMetadata.onUnsetValues)])

  return (
    <InspectorContextMenuWrapper
      id={`context-menu-for-${PP.toString(propPathWithIndex)}`}
      items={contextMenuItems}
      data={null}
      key={index}
    >
      <RowForControl
        controlDescription={controlDescription.propertyControls[index]}
        isScene={isScene}
        propPath={PP.appendPropertyPathElems(propPath, [index])}
        setGlobalCursor={props.setGlobalCursor}
        indentationLevel={1}
        focusOnMount={false}
      />
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
  disableToggling: boolean
}

const RowForObjectControl = React.memo((props: RowForObjectControlProps) => {
  const [open, setOpen] = React.useState(true)
  const handleOnClick = React.useCallback(() => {
    if (!props.disableToggling) {
      setOpen(!open)
    }
  }, [setOpen, open, props.disableToggling])
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
                cursor: props.disableToggling ? 'default' : 'pointer',
              }}
            >
              {title}
              {unless(props.disableToggling, <ObjectIndicator open={open} />)}
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
              disableToggling={props.disableToggling}
            />
          )
        }, controlDescription.object),
      )}
    </div>
  )
})

interface RowForUnionControlProps extends AbstractRowForControlProps {
  controlDescription: UnionControlDescription
}

const RowForUnionControl = React.memo((props: RowForUnionControlProps) => {
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
})

interface RowForControlProps extends AbstractRowForControlProps {
  controlDescription: RegularControlDescription
  disableToggling?: boolean
}

export const RowForControl = React.memo((props: RowForControlProps) => {
  const { controlDescription, disableToggling } = props
  if (isBaseControlDescription(controlDescription)) {
    return <RowForBaseControl {...props} controlDescription={controlDescription} />
  } else {
    switch (controlDescription.control) {
      case 'array':
        return <RowForArrayControl {...props} controlDescription={controlDescription} />
      case 'object':
        return (
          <RowForObjectControl
            {...props}
            controlDescription={controlDescription}
            disableToggling={disableToggling ?? false}
          />
        )
      case 'tuple':
        return <RowForTupleControl {...props} controlDescription={controlDescription} />
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

export const ComponentSectionInner = React.memo((props: ComponentSectionProps) => {
  const colorTheme = useColorTheme()

  const propertyControlsAndTargets = useKeepReferenceEqualityIfPossible(
    useGetPropertyControlsForSelectedComponents(),
  )

  const [sectionExpanded, setSectionExpanded] = React.useState(true)
  const toggleSection = React.useCallback(() => {
    setSectionExpanded((currentlyExpanded) => !currentlyExpanded)
  }, [setSectionExpanded])

  const dispatch = useDispatch()

  const selectedViews = useEditorState(
    Substores.selectedViews,
    (store) => store.editor.selectedViews,
    'ComponentInfoBox selectedViews',
  )

  const target = safeIndex(selectedViews, 0) ?? null

  const locationOfComponentInstance = useEditorState(
    Substores.fullStore,
    (state) => {
      const element = MetadataUtils.findElementByElementPath(state.editor.jsxMetadata, target)
      const importResult = getFilePathForImportedComponent(element)
      if (importResult == null) {
        const underlyingTarget = normalisePathToUnderlyingTarget(
          state.editor.projectContents,
          target,
        )

        return underlyingTarget.type === 'NORMALISE_PATH_SUCCESS' ? underlyingTarget.filePath : null
      } else {
        return importResult
      }
    },
    'ComponentSectionInner locationOfComponentInstance',
  )
  ComponentSectionInner.displayName = 'ComponentSectionInner'

  const OpenFile = React.useCallback(() => {
    if (locationOfComponentInstance != null) {
      dispatch([openCodeEditorFile(locationOfComponentInstance, true)])
    }
  }, [dispatch, locationOfComponentInstance])

  return (
    <React.Fragment>
      <FlexRow
        style={{
          padding: `0 ${UtopiaTheme.layout.inspectorXPadding}px`,
        }}
      >
        <FlexRow
          style={{
            flexGrow: 1,
            color: colorTheme.componentPurple.value,
            gap: 8,
            height: UtopiaTheme.layout.rowHeight.large,
          }}
        >
          <Icons.Component color='component' />
          <div
            onClick={OpenFile}
            style={{
              color: colorTheme.componentPurple.value,
              textDecoration: 'none',
              cursor: 'pointer',
              padding: '0 2px',
            }}
          >
            Component
          </div>
        </FlexRow>
        <SquareButton highlight onClick={toggleSection}>
          <ExpandableIndicator
            testId='component-section-expand'
            visible
            collapsed={!sectionExpanded}
            selected={false}
          />
        </SquareButton>
      </FlexRow>
      {when(
        sectionExpanded,
        <React.Fragment>
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
          <FlexRow
            style={{
              justifyContent: 'space-around',
              height: UtopiaTheme.layout.rowHeight.normal,
              position: 'sticky',
              top: 0,
              background: colorThemeConst.inspectorBackground.value,
              zIndex: 1,
            }}
          >
            Component props
          </FlexRow>

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
