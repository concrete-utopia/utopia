/** @jsxRuntime classic */
/** @jsx jsx */
import React from 'react'
import { jsx } from '@emotion/react'
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
} from '../../../custom-code/internal-property-controls'
import { isBaseControlDescription } from '../../../custom-code/internal-property-controls'
import { PathForSceneProps } from '../../../../core/model/scene-utils'
import { mapToArray } from '../../../../core/shared/object-utils'
import type { PropertyPath } from '../../../../core/shared/project-file-types'
import type { ElementPath } from '../../../../core/shared/project-file-types'
import * as PP from '../../../../core/shared/property-path'
import * as EP from '../../../../core/shared/element-path'
import { useKeepReferenceEqualityIfPossible } from '../../../../utils/react-performance'
import Utils from '../../../../utils/utils'
import type { ParseError } from '../../../../utils/value-parser-utils'
import { getParseErrorDetails } from '../../../../utils/value-parser-utils'
import {
  Tooltip,
  //TODO: switch last component to functional component and make use of 'useColorTheme':
  UtopiaTheme,
  InspectorSectionHeader,
  SimpleFlexRow,
  SquareButton,
  PopupList,
  Icons,
  FlexRow,
  Icn,
  iconForControlType,
  colorTheme,
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
  HtmlInputPropertyControl,
  JSXPropertyControl,
} from './property-control-controls'
import { ExpandableIndicator } from '../../../navigator/navigator-item/expandable-indicator'
import { unless, when } from '../../../../utils/react-conditionals'
import { PropertyControlsSection } from './property-controls-section'
import type { ReactEventHandlers } from 'react-use-gesture/dist/types'
import { normalisePathToUnderlyingTarget } from '../../../custom-code/code-file'
import { openCodeEditorFile, replaceElementInScope } from '../../../editor/actions/action-creators'
import { Substores, useEditorState } from '../../../editor/store/store-hook'
import { MetadataUtils } from '../../../../core/model/element-metadata-utils'
import { getFilePathForImportedComponent } from '../../../../core/model/project-file-utils'
import { safeIndex } from '../../../../core/shared/array-utils'
import { useDispatch } from '../../../editor/store/dispatch-context'
import { usePopper } from 'react-popper'
import type { JSExpressionOtherJavaScript } from '../../../../core/shared/element-template'
import {
  getJSXElementNameAsString,
  isImportedOrigin,
} from '../../../../core/shared/element-template'
import { optionalMap } from '../../../../core/shared/optional-utils'
import type { DataPickerCallback, VariableOption } from './data-picker-popup'
import { DataPickerPopup, DataPickerPreferredAllAtom } from './data-picker-popup'
import { jsxElementChildToText } from '../../../canvas/ui-jsx-canvas-renderer/jsx-element-child-to-text'
import { stopPropagation } from '../../common/inspector-utils'
import { IdentifierExpressionCartoucheControl } from './cartouche-control'
import { getRegisteredComponent } from '../../../../core/property-controls/property-controls-utils'
import type { EditorAction } from '../../../editor/action-types'
import { useVariablesInScopeForSelectedElement } from './variables-in-scope-utils'
import { useAtom } from 'jotai'

export const VariableFromScopeOptionTestId = (idx: string) => `variable-from-scope-${idx}`
export const DataPickerPopupButtonTestId = `data-picker-popup-button-test-id`
export const DataPickerPopupTestId = `data-picker-popup-test-id`

export interface PropertyLabelAndPlusButtonProps {
  title: string
  openPopup: () => void
  handleMouseEnter: () => void
  handleMouseLeave: () => void
  popupIsOpen: boolean
  isHovered: boolean
  isConnectedToData: boolean
}

export function PropertyLabelAndPlusButton(
  props: React.PropsWithChildren<PropertyLabelAndPlusButtonProps>,
) {
  const {
    title,
    openPopup,
    popupIsOpen,
    isHovered,
    isConnectedToData,
    handleMouseEnter,
    handleMouseLeave,
    children,
  } = props

  return (
    <Tooltip title={title}>
      <div
        onMouseEnter={handleMouseEnter}
        onMouseLeave={handleMouseLeave}
        style={{
          lineHeight: `${UtopiaTheme.layout.inputHeight.default}px`,
          display: 'flex',
          flexDirection: 'row',
          alignItems: 'center',
          gap: 6,
          color:
            popupIsOpen || isHovered || isConnectedToData
              ? colorTheme.dynamicBlue.value
              : undefined,
          cursor: 'pointer',
        }}
      >
        <span onClick={openPopup}>{title}</span>
        {children}
        <div
          onClick={openPopup}
          style={{
            opacity: isHovered || popupIsOpen ? 1 : 0,
          }}
        >
          <Icn
            category='semantic'
            type='plus-in-white-translucent-circle'
            color={'dynamic'}
            width={12}
            height={12}
          />
        </div>
      </div>
    </Tooltip>
  )
}

function useComponentPropsInspectorInfo(
  partialPath: PropertyPath,
  addPropsToPath: boolean,
  control: RegularControlDescription,
) {
  const propertyPath = addPropsToPath ? PP.append(PathForSceneProps, partialPath) : partialPath
  return useInspectorInfoForPropertyControl(propertyPath, control)
}

const ControlForProp = React.memo((props: ControlForPropProps<RegularControlDescription>) => {
  const { controlDescription, showHiddenControl } = props
  const onSubmitValue = props.propMetadata.onSubmitValue

  const isRequired: boolean = props.controlDescription.required ?? false
  const hasDefaultValue: boolean = 'defaultValue' in props.controlDescription
  const safeToDelete = !isRequired || hasDefaultValue

  const onDeleteCartouche = React.useCallback(() => {
    if (safeToDelete) {
      if (isRequired) {
        onSubmitValue(props.controlDescription.defaultValue, false)
      } else {
        onSubmitValue(null, false)
        showHiddenControl(PP.firstPartToString(props.propPath))
      }
    }
  }, [
    safeToDelete,
    isRequired,
    onSubmitValue,
    props.controlDescription.defaultValue,
    props.propPath,
    showHiddenControl,
  ])

  if (controlDescription == null) {
    return null
  }

  const attributeExpression = props.propMetadata.attributeExpression

  if (attributeExpression != null) {
    if (
      attributeExpression.type === 'JS_IDENTIFIER' ||
      attributeExpression.type === 'JS_PROPERTY_ACCESS' ||
      attributeExpression.type === 'JS_ELEMENT_ACCESS'
    ) {
      return (
        <IdentifierExpressionCartoucheControl
          contents={jsxElementChildToText(attributeExpression, null, null, 'jsx', 'inner')}
          icon={React.createElement(iconForControlType(props.controlDescription.control))}
          matchType='full'
          onOpenDataPicker={props.onOpenDataPicker}
          onDeleteCartouche={onDeleteCartouche}
          testId={`cartouche-${PP.toString(props.propPath)}`}
          safeToDelete={safeToDelete}
          propertyPath={props.propPath}
          elementPath={props.elementPath}
        />
      )
    }

    if (
      attributeExpression.type === 'ATTRIBUTE_OTHER_JAVASCRIPT' ||
      attributeExpression.type === 'JSX_MAP_EXPRESSION'
    ) {
      // If the parsed code is JSExpression but the value is JSX, we should show the inline jsx control instead
      if (controlDescription.control !== 'jsx') {
        return (
          <IdentifierExpressionCartoucheControl
            contents={'Expression'}
            icon={React.createElement(iconForControlType('none'))}
            matchType='partial'
            onOpenDataPicker={props.onOpenDataPicker}
            onDeleteCartouche={onDeleteCartouche}
            testId={`cartouche-${PP.toString(props.propPath)}`}
            propertyPath={props.propPath}
            safeToDelete={safeToDelete}
            elementPath={props.elementPath}
          />
        )
      }
    }
  }

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
    case 'html-input':
      return <HtmlInputPropertyControl {...props} controlDescription={controlDescription} />
    case 'style-controls':
      return null
    case 'vector2':
    case 'vector3':
    case 'vector4':
      return <VectorPropertyControl {...props} controlDescription={controlDescription} />
    case 'jsx':
      return <JSXPropertyControl {...props} controlDescription={controlDescription} />
    default:
      return null
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

interface AbstractRowForControlProps {
  propPath: PropertyPath
  isScene: boolean
  setGlobalCursor: (cursor: CSSCursor | null) => void
  indentationLevel: number
  focusOnMount: boolean
  showHiddenControl: (path: string) => void
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

const isBaseIndentationLevel = (props: AbstractRowForControlProps) => props.indentationLevel === 1

export function useDataPickerButton(
  variablesInScope: VariableOption[],
  onPropertyPicked: DataPickerCallback,
) {
  const [referenceElement, setReferenceElement] = React.useState<HTMLDivElement | null>(null)
  const [popperElement, setPopperElement] = React.useState<HTMLDivElement | null>(null)
  const popper = usePopper(referenceElement, popperElement, {
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
  const openPopup = React.useCallback(() => setPopupIsOpen(true), [])
  const closePopup = React.useCallback(() => setPopupIsOpen(false), [])

  const DataPickerComponent = React.useMemo(
    () => (
      <DataPickerPopup
        {...popper.attributes.popper}
        style={popper.styles.popper}
        closePopup={closePopup}
        ref={setPopperElement}
        onTweakProperty={onPropertyPicked}
        variablesInScope={variablesInScope}
      />
    ),
    [
      closePopup,
      onPropertyPicked,
      popper.attributes.popper,
      popper.styles.popper,
      variablesInScope,
    ],
  )

  return {
    popupIsOpen,
    DataPickerComponent,
    setReferenceElement,
    openPopup,
  }
}

interface RowForBaseControlProps extends AbstractRowForControlProps {
  label?: React.ComponentType<React.PropsWithChildren<any>> // TODO Before Merge this probably should not be a component
  controlDescription: BaseControlDescription
}

function setPropertyFromDataPickerActions(
  selectedViews: Array<ElementPath>,
  propertyPath: PropertyPath,
  expression: JSExpressionOtherJavaScript,
): Array<EditorAction> | null {
  const target = selectedViews.at(0)
  if (target == null) {
    return null
  }

  return [
    replaceElementInScope(target, {
      type: 'replace-property-value',
      propertyPath: propertyPath,
      replaceWith: expression,
    }),
  ]
}

function useDataPickerButtonInComponentSection(
  selectedViews: Array<ElementPath>,
  propertyPath: PropertyPath,
) {
  const dispatch = useDispatch()

  const [preferredAllState] = useAtom(DataPickerPreferredAllAtom)
  const variableNamesInScope = useVariablesInScopeForSelectedElement(
    selectedViews.at(0) ?? EP.emptyElementPath,
    propertyPath,
    preferredAllState,
  )

  const dataPickerButtonData = useDataPickerButton(variableNamesInScope, (e) =>
    optionalMap(dispatch, setPropertyFromDataPickerActions(selectedViews, propertyPath, e)),
  )

  return dataPickerButtonData
}

const RowForBaseControl = React.memo((props: RowForBaseControlProps) => {
  const { propPath, controlDescription, isScene } = props
  const title = labelForControl(propPath, controlDescription)
  const propName = `${PP.lastPart(propPath)}`
  const indentation = props.indentationLevel * 8

  const [isHovered, setIsHovered] = React.useState(false)

  const selectedViews = useEditorState(
    Substores.selectedViews,
    (store) => store.editor.selectedViews,
    'RowForBaseControl selectedViews',
  )

  const propMetadata = useComponentPropsInspectorInfo(propPath, isScene, controlDescription)
  const contextMenuItems = Utils.stripNulls([
    addOnUnsetValues([propName], propMetadata.onUnsetValues),
  ])

  const labelControlStyle = React.useMemo(
    () => getLabelControlStyle(controlDescription, propMetadata),
    [controlDescription, propMetadata],
  )

  const dataPickerButtonData = useDataPickerButtonInComponentSection(selectedViews, props.propPath)

  const handleMouseEnter = React.useCallback(() => {
    setIsHovered(true)
  }, [])

  const handleMouseLeave = React.useCallback(() => {
    setIsHovered(false)
  }, [])

  const isConnectedToData = React.useMemo(() => {
    return (
      propMetadata.propertyStatus.controlled &&
      propMetadata.attributeExpression?.type !== 'JSX_ELEMENT'
    )
  }, [propMetadata])

  const propertyLabel =
    props.label == null ? (
      <PropertyLabel
        controlStyles={labelControlStyle}
        target={[propPath]}
        style={{
          textTransform: 'capitalize',
          paddingLeft: indentation - 8,
          alignSelf: 'flex-start',
        }}
      >
        <PropertyLabelAndPlusButton
          title={title}
          openPopup={dataPickerButtonData.openPopup}
          handleMouseEnter={handleMouseEnter}
          handleMouseLeave={handleMouseLeave}
          popupIsOpen={dataPickerButtonData.popupIsOpen}
          isHovered={isHovered}
          isConnectedToData={isConnectedToData}
        />
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
      testId={`context-menu-for-${propName}`}
      items={contextMenuItems}
      data={null}
    >
      {dataPickerButtonData.popupIsOpen ? dataPickerButtonData.DataPickerComponent : null}
      <UIGridRow padded={false} style={{ padding: '3px 8px' }} variant='<--1fr--><--1fr-->'>
        {propertyLabel}
        <div
          style={{
            minWidth: 0, // this ensures that the div can never expand the allocated grid space
          }}
          ref={dataPickerButtonData.setReferenceElement}
        >
          <ControlForProp
            propPath={propPath}
            propName={propName}
            controlDescription={controlDescription}
            propMetadata={propMetadata}
            setGlobalCursor={props.setGlobalCursor}
            focusOnMount={props.focusOnMount}
            onOpenDataPicker={dataPickerButtonData.openPopup}
            showHiddenControl={props.showHiddenControl}
            elementPath={selectedViews.at(0) ?? EP.emptyElementPath}
          />
        </div>
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

  const propName = `${PP.lastPart(propPath)}`
  const propMetadata = useComponentPropsInspectorInfo(propPath, isScene, controlDescription)

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

  const selectedViews = useEditorState(
    Substores.selectedViews,
    (store) => store.editor.selectedViews,
    'RowForArrayControl selectedViews',
  )

  const dataPickerButtonData = useDataPickerButtonInComponentSection(selectedViews, props.propPath)

  return (
    <React.Fragment>
      {when(dataPickerButtonData.popupIsOpen, dataPickerButtonData.DataPickerComponent)}
      <div>
        <SimpleFlexRow
          style={{ gap: 5, justifyContent: 'space-between', flexGrow: 1, paddingRight: 3 }}
        >
          <FlexRow
            style={{ flex: 1, flexShrink: 0, gap: 5, justifyContent: 'space-between' }}
            ref={dataPickerButtonData.setReferenceElement}
          >
            <PropertyLabel target={[propPath]} style={objectPropertyLabelStyle}>
              {title}
            </PropertyLabel>
            {propertyStatus.overwritable && !propertyStatus.controlled ? (
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
            <ControlForProp
              propPath={propPath}
              propName={propName}
              controlDescription={controlDescription}
              propMetadata={propMetadata}
              setGlobalCursor={props.setGlobalCursor}
              focusOnMount={props.focusOnMount}
              onOpenDataPicker={dataPickerButtonData.openPopup}
              showHiddenControl={props.showHiddenControl}
              elementPath={selectedViews.at(0) ?? EP.emptyElementPath}
            />
          </FlexRow>
        </SimpleFlexRow>
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
              showHiddenControl={props.showHiddenControl}
            />
          ))}
        </div>
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
  showHiddenControl: (path: string) => void
}

const ArrayControlItem = React.memo((props: ArrayControlItemProps) => {
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
          indentationLevel={2}
          focusOnMount={props.focusOnMount && index === 0}
          disableToggling={true}
          showHiddenControl={props.showHiddenControl}
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
  const { value } = useComponentPropsInspectorInfo(propPath, isScene, controlDescription)

  const rowHeight = UtopiaTheme.layout.rowHeight.normal
  const transformedValue = Array.isArray(value) ? value : [value]
  const boundedTransformedValue = transformedValue.slice(
    0,
    controlDescription.propertyControls.length,
  )

  return (
    <React.Fragment>
      <InspectorSectionHeader>
        <SimpleFlexRow style={{ flexGrow: 1, flexShrink: 0 }}>
          {when(isBaseIndentationLevel(props), <div>I0</div>)}
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
            showHiddenControl={props.showHiddenControl}
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
  showHiddenControl: (path: string) => void
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
        showHiddenControl={props.showHiddenControl}
      />
    </InspectorContextMenuWrapper>
  )
})

interface ObjectIndicatorProps {
  open: boolean
}

const ObjectIndicator = (props: ObjectIndicatorProps) => {
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

  const propName = `${PP.lastPart(propPath)}`

  const propMetadata = useComponentPropsInspectorInfo(propPath, isScene, controlDescription)
  const contextMenuItems = Utils.stripNulls([
    addOnUnsetValues([PP.lastPart(propPath)], propMetadata.onUnsetValues),
  ])

  const selectedViews = useEditorState(
    Substores.selectedViews,
    (store) => store.editor.selectedViews,
    'RowForObjectControl selectedViews',
  )

  const dataPickerButtonData = useDataPickerButtonInComponentSection(selectedViews, props.propPath)

  const [isHovered, setIsHovered] = React.useState(false)

  const handleMouseEnter = React.useCallback(() => {
    setIsHovered(true)
  }, [])

  const handleMouseLeave = React.useCallback(() => {
    setIsHovered(false)
  }, [])

  const isConnectedToData = React.useMemo(() => {
    return (
      propMetadata.propertyStatus.controlled &&
      propMetadata.attributeExpression?.type !== 'JSX_ELEMENT'
    )
  }, [propMetadata])

  return (
    <div>
      <div>
        <InspectorContextMenuWrapper
          id={`context-menu-for-${PP.toString(propPath)}`}
          items={contextMenuItems}
          data={null}
        >
          {when(dataPickerButtonData.popupIsOpen, dataPickerButtonData.DataPickerComponent)}
          <FlexRow
            style={{ flexGrow: 1, justifyContent: 'space-between', paddingRight: 10 }}
            ref={dataPickerButtonData.setReferenceElement}
          >
            <SimpleFlexRow
              style={{
                minWidth: 0,
                flexGrow: 1,
                justifyContent: 'space-between',
              }}
              onClick={handleOnClick}
            >
              <PropertyLabel
                target={[propPath]}
                style={{
                  ...objectPropertyLabelStyle,
                  paddingLeft: indentation,
                  paddingRight: 6,
                  cursor: props.disableToggling ? 'default' : 'pointer',
                }}
              >
                <PropertyLabelAndPlusButton
                  title={title}
                  openPopup={dataPickerButtonData.openPopup}
                  handleMouseEnter={handleMouseEnter}
                  handleMouseLeave={handleMouseLeave}
                  popupIsOpen={dataPickerButtonData.popupIsOpen}
                  isHovered={isHovered}
                  isConnectedToData={isConnectedToData}
                >
                  {unless(props.disableToggling, <ObjectIndicator open={open} />)}
                </PropertyLabelAndPlusButton>
              </PropertyLabel>
              <div style={{ minWidth: 0 }} onClick={stopPropagation}>
                <ControlForProp
                  propPath={propPath}
                  propName={propName}
                  controlDescription={controlDescription}
                  propMetadata={propMetadata}
                  setGlobalCursor={props.setGlobalCursor}
                  focusOnMount={props.focusOnMount}
                  onOpenDataPicker={dataPickerButtonData.openPopup}
                  showHiddenControl={props.showHiddenControl}
                  elementPath={selectedViews.at(0) ?? EP.emptyElementPath}
                />
              </div>
            </SimpleFlexRow>
          </FlexRow>
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
              showHiddenControl={props.showHiddenControl}
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
        {when(isBaseIndentationLevel(props), <div>I0</div>)}
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

  const propertyControlsInfo = useEditorState(
    Substores.propertyControlsInfo,
    (store) => store.editor.propertyControlsInfo,
    'ComponentsectionInner propertyControlsInfo',
  )

  const componentData = useEditorState(
    Substores.metadata,
    (store) => {
      if (
        propertyControlsAndTargets.length === 0 ||
        propertyControlsAndTargets[0].targets.length !== 1
      ) {
        return null
      }

      const element = MetadataUtils.findElementByElementPath(
        store.editor.jsxMetadata,
        propertyControlsAndTargets[0].targets[0],
      )

      const targetJSXElement = MetadataUtils.getJSXElementFromElementInstanceMetadata(element)
      const elementImportInfo = element?.importInfo
      if (elementImportInfo == null || targetJSXElement == null) {
        return null
      }

      const elementName = getJSXElementNameAsString(targetJSXElement.name)

      const exportedName = isImportedOrigin(elementImportInfo)
        ? elementImportInfo.exportedName ?? elementName
        : elementName

      const registeredComponent = getRegisteredComponent(
        exportedName,
        elementImportInfo.filePath,
        propertyControlsInfo,
      )

      if (registeredComponent?.label == null) {
        return {
          displayName: elementName,
          isRegisteredComponent: registeredComponent != null,
        }
      }

      return {
        displayName: registeredComponent.label,
        isRegisteredComponent: registeredComponent != null,
        secondaryName: elementName,
      }
    },
    'ComponentSectionInner componentName',
  )

  return (
    <React.Fragment>
      <UIGridRow
        padded={false}
        variant='<----------1fr---------><-auto->'
        style={{
          borderTop: `1px solid ${colorTheme.seperator.value}`,
          padding: `0 ${UtopiaTheme.layout.inspectorXPadding}px`,
          alignItems: 'center',
          gap: 8,
          height: 38,
          flexShrink: 0,
        }}
      >
        <FlexRow
          onClick={OpenFile}
          style={{
            flexGrow: 1,
            height: UtopiaTheme.layout.rowHeight.large,
            fontWeight: 600,
            cursor: 'pointer',
            textOverflow: 'ellipsis',
            whiteSpace: 'nowrap',
            overflow: 'hidden',
            gap: 8,
          }}
        >
          {componentData != null ? (
            <React.Fragment>
              <span>{componentData.displayName}</span>
              {when(componentData.isRegisteredComponent, <span style={{ fontSize: 6 }}>◇</span>)}
              {componentData.secondaryName == null ? null : (
                <span style={{ opacity: 0.5, fontWeight: 'initial' }}>
                  {componentData.secondaryName}
                </span>
              )}
            </React.Fragment>
          ) : (
            <span>Component</span>
          )}
        </FlexRow>
        <SquareButton highlight style={{ width: 12 }} onClick={toggleSection}>
          <ExpandableIndicator
            testId='component-section-expand'
            visible
            collapsed={!sectionExpanded}
            selected={false}
          />
        </SquareButton>
      </UIGridRow>
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
              background: colorTheme.inspectorBackground.value,
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
            <span style={{ paddingTop: 4, color: colorTheme.errorForeground.value }}>
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

const objectPropertyLabelStyle = {
  textTransform: 'capitalize',
  paddingLeft: 8,
  display: 'flex',
  alignItems: 'center',
  height: 34,
  fontWeight: 500,
  gap: 4,
} as const
