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
  NoneControlDescription,
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
import { safeIndex } from '../../../../core/shared/array-utils'
import { useDispatch } from '../../../editor/store/dispatch-context'
import { usePopper } from 'react-popper'
import type {
  ElementInstanceMetadataMap,
  JSExpressionOtherJavaScript,
} from '../../../../core/shared/element-template'
import {
  getJSXElementNameAsString,
  isImportedOrigin,
  isJSXElement,
} from '../../../../core/shared/element-template'
import { optionalMap } from '../../../../core/shared/optional-utils'
import type { DataPickerCallback, DataPickerOption, ObjectPath } from './data-picker-utils'
import { jsxElementChildToValuePath } from './data-picker-utils'
import { stopPropagation } from '../../common/inspector-utils'
import { IdentifierExpressionCartoucheControl } from './cartouche-control'
import { getRegisteredComponent } from '../../../../core/property-controls/property-controls-utils'
import type { EditorAction } from '../../../editor/action-types'
import {
  getCartoucheDataTypeForExpression,
  matchForPropertyValue,
  usePropertyControlDescriptions,
  usePropertyValue,
  useVariablesInScopeForSelectedElement,
} from './variables-in-scope-utils'
import { DataSelectorModal } from './data-selector-modal'
import { getModifiableJSXAttributeAtPath } from '../../../../core/shared/jsx-attribute-utils'
import { isRight } from '../../../../core/shared/either'
import { useChildrenPropOverride } from './component-section-children'
import {
  childrenAreProbablyNumericExpression,
  replaceFirstChildAndDeleteSiblings,
} from '../../../editor/element-children'
import { getTextContentOfElement } from './data-reference-cartouche'

export interface PropertyLabelAndPlusButtonProps {
  title: string
  openPopup: () => void
  handleMouseEnter: () => void
  handleMouseLeave: () => void
  popupIsOpen: boolean
  isHovered: boolean
  isConnectedToData: boolean
  testId: string
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
    testId,
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
          data-testid={testId}
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
  elementPath: ElementPath,
  partialPath: PropertyPath,
  addPropsToPath: boolean,
  control: RegularControlDescription,
) {
  const propertyPath = addPropsToPath ? PP.append(PathForSceneProps, partialPath) : partialPath
  return useInspectorInfoForPropertyControl(elementPath, propertyPath, control)
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

  const attributeExpression = props.propMetadata.attributeExpression

  const datatypeForExpression = useEditorState(
    Substores.projectContentsAndMetadataAndVariablesInScope,
    (store) => {
      if (attributeExpression == null) {
        return 'unknown'
      }
      return getCartoucheDataTypeForExpression(
        props.elementPath,
        attributeExpression,
        store.editor.variablesInScope,
      )
    },
    'ControlForProp datatypeForExpression',
  )

  const childrenPropOverride = useChildrenPropOverride({
    ...props,
    onDeleteCartouche: onDeleteCartouche,
    safeToDelete: safeToDelete,
    dataTypeForExpression: datatypeForExpression,
  })
  if (childrenPropOverride != null) {
    return childrenPropOverride
  }

  if (controlDescription == null) {
    return null
  }

  if (attributeExpression != null) {
    if (
      attributeExpression.type === 'JS_IDENTIFIER' ||
      attributeExpression.type === 'JS_PROPERTY_ACCESS' ||
      attributeExpression.type === 'JS_ELEMENT_ACCESS'
    ) {
      return (
        <IdentifierExpressionCartoucheControl
          contents={getTextContentOfElement(attributeExpression, null)}
          icon={React.createElement(iconForControlType(props.controlDescription.control))}
          matchType='full'
          onOpenDataPicker={props.onOpenDataPicker}
          onDeleteCartouche={onDeleteCartouche}
          testId={`cartouche-${PP.toString(props.propPath)}`}
          safeToDelete={safeToDelete}
          propertyPath={props.propPath}
          elementPath={props.elementPath}
          datatype={datatypeForExpression}
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
            contents={getTextContentOfElement(attributeExpression, null)}
            icon={React.createElement(iconForControlType('none'))}
            matchType='partial'
            onOpenDataPicker={props.onOpenDataPicker}
            onDeleteCartouche={onDeleteCartouche}
            testId={`cartouche-${PP.toString(props.propPath)}`}
            propertyPath={props.propPath}
            safeToDelete={safeToDelete}
            elementPath={props.elementPath}
            datatype={datatypeForExpression}
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
ControlForProp.displayName = 'ControlForProp'

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
  shouldIncreaseIdentation: boolean
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
  variablesInScope: DataPickerOption[],
  onPropertyPicked: DataPickerCallback,
  currentSelectedValuePath: ObjectPath | null,
  lowestInsertionCeiling: ElementPath | null,
) {
  const [referenceElement, setReferenceElement] = React.useState<HTMLDivElement | null>(null)
  const [popperElement, setPopperElement] = React.useState<HTMLDivElement | null>(null)
  const popper = usePopper(referenceElement, popperElement, {
    modifiers: [
      {
        name: 'offset',
        options: {
          offset: [8, 8],
        },
      },
      {
        name: 'preventOverflow',
        options: {
          altAxis: true,
          padding: 20,
        },
      },
    ],
  })

  const [popupIsOpen, setPopupIsOpen] = React.useState(false)
  const openPopup = React.useCallback(() => setPopupIsOpen(true), [])
  const closePopup = React.useCallback(() => setPopupIsOpen(false), [])

  const DataPickerComponent = React.useMemo(
    () => (
      <DataSelectorModal
        {...popper.attributes.popper}
        style={popper.styles.popper}
        closePopup={closePopup}
        ref={setPopperElement}
        onPropertyPicked={onPropertyPicked}
        variablesInScope={variablesInScope}
        startingSelectedValuePath={currentSelectedValuePath}
        lowestInsertionCeiling={lowestInsertionCeiling}
      />
    ),
    [
      closePopup,
      currentSelectedValuePath,
      onPropertyPicked,
      popper.attributes.popper,
      popper.styles.popper,
      variablesInScope,
      lowestInsertionCeiling,
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
  metadata: ElementInstanceMetadataMap,
  selectedViews: Array<ElementPath>,
  propertyPath: PropertyPath,
  expression: JSExpressionOtherJavaScript,
): Array<EditorAction> | null {
  const target = selectedViews.at(0)
  if (target == null) {
    return null
  }

  // If the target replacement is the children property and the element has children,
  // replace the children directly instead of its prop.
  const isReplacingChildren =
    propertyPath.propertyElements.length === 1 && propertyPath.propertyElements[0] === 'children'
  if (isReplacingChildren) {
    // …and the element has children…
    const element = MetadataUtils.findElementByElementPath(metadata, target)
    const children =
      element != null && isRight(element.element) && isJSXElement(element.element.value)
        ? element.element.value.children
        : []
    return replaceFirstChildAndDeleteSiblings(target, children, expression)
  }

  // In all other cases, replace the prop.
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

  const elementPath = selectedViews.at(0) ?? EP.emptyElementPath

  const controlDescriptions = usePropertyControlDescriptions(propertyPath)
  const currentPropertyValue = usePropertyValue(elementPath, propertyPath)

  const variableNamesInScope = useVariablesInScopeForSelectedElement(
    selectedViews.at(0) ?? EP.emptyElementPath,
    matchForPropertyValue(
      controlDescriptions,
      currentPropertyValue,
      PP.lastPart(propertyPath).toString(),
    ),
  )

  const metadata = useEditorState(
    Substores.metadata,
    (store) => store.editor.jsxMetadata,
    'useDataPickerButtonInComponentSection metadata',
  )

  const pathToCurrentValue = useEditorState(
    Substores.metadata,
    (store) => {
      const [selectedView, ...rest] = selectedViews
      if (selectedView == null || rest.length > 0) {
        return null
      }

      const instance = MetadataUtils.findElementByElementPath(
        store.editor.jsxMetadata,
        selectedView,
      )
      if (
        instance == null ||
        instance.element.type !== 'RIGHT' ||
        instance.element.value.type !== 'JSX_ELEMENT'
      ) {
        return null
      }

      const prop = getModifiableJSXAttributeAtPath(instance.element.value.props, propertyPath)
      if (
        prop.type !== 'RIGHT' ||
        prop.value.type === 'PART_OF_ATTRIBUTE_VALUE' ||
        prop.value.type === 'ATTRIBUTE_NOT_FOUND'
      ) {
        return null
      }

      return jsxElementChildToValuePath(prop.value)
    },
    'useDataPickerButtonInComponentSection pathToCurrentValue',
  )

  const dataPickerButtonData = useDataPickerButton(
    variableNamesInScope,
    (e) =>
      optionalMap(
        dispatch,
        setPropertyFromDataPickerActions(metadata, selectedViews, propertyPath, e),
      ),
    pathToCurrentValue,
    selectedViews.at(0) ?? null,
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

  const propMetadata = useComponentPropsInspectorInfo(
    selectedViews[0] ?? EP.emptyElementPath,
    propPath,
    isScene,
    controlDescription,
  )
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

  const isConnectedToData = useEditorState(
    Substores.metadata,
    (store) => {
      if (propName === 'children') {
        // for children props, we need to drill down and look for the types of the elements
        return selectedViews.some((view) => {
          const element = MetadataUtils.findElementByElementPath(store.editor.jsxMetadata, view)
          if (element != null && isRight(element.element) && isJSXElement(element.element.value)) {
            const children = element.element.value.children
            return (
              (controlDescription.control === 'number-input' &&
                childrenAreProbablyNumericExpression(children)) ||
              children.some((child) => child.type === 'JS_IDENTIFIER')
            )
          }
          return false
        })
      }
      return (
        propMetadata.propertyStatus.controlled &&
        propMetadata.attributeExpression?.type !== 'JSX_ELEMENT'
      )
    },
    'RowForBaseControl isConnectedToData',
  )

  const propertyLabel =
    props.label == null ? (
      <PropertyLabel
        controlStyles={labelControlStyle}
        target={[propPath]}
        style={{
          textTransform: 'capitalize',
          paddingLeft: indentation,
          alignSelf: 'center',
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
          testId={`plus-button-${title}`}
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
      <UIGridRow
        padded={false}
        alignContent='center'
        style={{ padding: '3px 8px' }}
        variant='<--1fr--><--1fr-->'
      >
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
RowForBaseControl.displayName = 'RowForBaseControl'

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
  disableToggling: boolean
}

const RowForArrayControl = React.memo((props: RowForArrayControlProps) => {
  const { propPath, controlDescription, isScene } = props
  const title = labelForControl(propPath, controlDescription)

  const [open, setOpen] = React.useState(false)
  const handleOnClick = React.useCallback(() => {
    if (!props.disableToggling) {
      setOpen(!open)
    }
  }, [setOpen, open, props.disableToggling])

  const selectedViews = useEditorState(
    Substores.selectedViews,
    (store) => store.editor.selectedViews,
    'RowForArrayControl selectedViews',
  )

  const { value, onSubmitValue, propertyStatus } = useComponentPropsInspectorInfo(
    selectedViews[0] ?? EP.emptyElementPath,
    propPath,
    isScene,
    controlDescription,
  )

  const propName = `${PP.lastPart(propPath)}`
  const propMetadata = useComponentPropsInspectorInfo(
    selectedViews[0] ?? EP.emptyElementPath,
    propPath,
    isScene,
    controlDescription,
  )

  const sectionHeight = React.useMemo(
    () => getSectionHeight(controlDescription),
    [controlDescription],
  )

  const [insertingRow, setInsertingRow] = React.useState(false)
  const toggleInsertRow = React.useCallback((e: React.MouseEvent) => {
    e.stopPropagation()
    e.preventDefault()
    setInsertingRow((current) => !current)
  }, [])

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
    <React.Fragment>
      {when(dataPickerButtonData.popupIsOpen, dataPickerButtonData.DataPickerComponent)}
      <div>
        <UIGridRow
          padded={false}
          alignContent='center'
          style={{ padding: '3px 8px' }}
          variant='<--1fr--><--1fr-->'
          ref={dataPickerButtonData.setReferenceElement}
          onClick={handleOnClick}
          data-testid={`control-container-${title}`}
        >
          <PropertyLabelAndPlusButton
            title={title}
            openPopup={dataPickerButtonData.openPopup}
            handleMouseEnter={handleMouseEnter}
            handleMouseLeave={handleMouseLeave}
            popupIsOpen={dataPickerButtonData.popupIsOpen}
            isHovered={isHovered}
            isConnectedToData={isConnectedToData}
            testId={`plus-button-${title}`}
          >
            {unless(props.disableToggling, <ObjectIndicator open={open} toggle={handleOnClick} />)}
          </PropertyLabelAndPlusButton>
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
        </UIGridRow>
        {when(
          open,
          <div
            style={{
              height: sectionHeight * springs.length,
            }}
          >
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
                  indentationLevel={props.indentationLevel}
                />
              ))}
            </div>
          </div>,
        )}
      </div>
    </React.Fragment>
  )
})
RowForArrayControl.displayName = 'RowForArrayControl'

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
  indentationLevel: number
}

const ArrayControlItem = React.memo((props: ArrayControlItemProps) => {
  const { bind, propPath, index, isScene, springStyle, controlDescription } = props
  const propPathWithIndex = PP.appendPropertyPathElems(propPath, [index])

  const selectedViews = useEditorState(
    Substores.selectedViews,
    (store) => store.editor.selectedViews,
    'ArrayControlItem selectedViews',
  )
  const propMetadata = useComponentPropsInspectorInfo(
    selectedViews[0] ?? EP.emptyElementPath,
    propPathWithIndex,
    isScene,
    controlDescription,
  )
  const contextMenuItems = Utils.stripNulls([addOnUnsetValues([index], propMetadata.onUnsetValues)])

  const contextMenuId = `context-menu-for-${PP.toString(propPathWithIndex)}`

  if (controlDescription.propertyControl.control === 'none') {
    return null
  }

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
          shouldIncreaseIdentation={true}
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
ArrayControlItem.displayName = 'ArrayControlItem'

interface RowForTupleControlProps extends AbstractRowForControlProps {
  controlDescription: TupleControlDescription
}

const RowForTupleControl = React.memo((props: RowForTupleControlProps) => {
  const { propPath, controlDescription, isScene } = props
  const title = labelForControl(propPath, controlDescription)

  const selectedViews = useEditorState(
    Substores.selectedViews,
    (store) => store.editor.selectedViews,
    'RowForTupleControl selectedViews',
  )
  const { value } = useComponentPropsInspectorInfo(
    selectedViews[0] ?? EP.emptyElementPath,
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
RowForTupleControl.displayName = 'RowForTupleControl'

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

  const selectedViews = useEditorState(
    Substores.selectedViews,
    (store) => store.editor.selectedViews,
    'TupleControlItem selectedViews',
  )
  const propMetadata = useComponentPropsInspectorInfo(
    selectedViews[0] ?? EP.emptyElementPath,
    propPathWithIndex,
    isScene,
    controlDescription,
  )
  const contextMenuItems = Utils.stripNulls([addOnUnsetValues([index], propMetadata.onUnsetValues)])

  const indexedPropertyControls = controlDescription.propertyControls[index]

  if (indexedPropertyControls.control === 'none') {
    return null
  }

  return (
    <InspectorContextMenuWrapper
      id={`context-menu-for-${PP.toString(propPathWithIndex)}`}
      items={contextMenuItems}
      data={null}
      key={index}
    >
      <RowForControl
        controlDescription={indexedPropertyControls}
        isScene={isScene}
        propPath={PP.appendPropertyPathElems(propPath, [index])}
        setGlobalCursor={props.setGlobalCursor}
        indentationLevel={1}
        shouldIncreaseIdentation={true}
        focusOnMount={false}
        showHiddenControl={props.showHiddenControl}
      />
    </InspectorContextMenuWrapper>
  )
})
TupleControlItem.displayName = 'TupleControlItem'

interface ObjectIndicatorProps {
  open: boolean
  toggle: () => void
}

const ObjectIndicator = (props: ObjectIndicatorProps) => {
  return (
    <div
      onClick={props.toggle}
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
  const partOfBiggerObject = props.propPath.propertyElements.length > 1
  const [open, setOpen] = React.useState(partOfBiggerObject)
  const handleOnClick = React.useCallback(() => {
    if (!props.disableToggling) {
      setOpen(!open)
    }
  }, [setOpen, open, props.disableToggling])
  const { propPath, controlDescription, isScene } = props
  const title = labelForControl(propPath, controlDescription)
  const indentation = props.indentationLevel * 8

  const propName = `${PP.lastPart(propPath)}`

  const selectedViews = useEditorState(
    Substores.selectedViews,
    (store) => store.editor.selectedViews,
    'RowForObjectControl selectedViews',
  )

  const propMetadata = useComponentPropsInspectorInfo(
    selectedViews[0] ?? EP.emptyElementPath,
    propPath,
    isScene,
    controlDescription,
  )
  const contextMenuItems = Utils.stripNulls([
    addOnUnsetValues([PP.lastPart(propPath)], propMetadata.onUnsetValues),
  ])

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
          <UIGridRow
            padded={false}
            alignContent='center'
            style={{ padding: '3px 8px' }}
            variant='<--1fr--><--1fr-->'
            ref={dataPickerButtonData.setReferenceElement}
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
                testId={`plus-button-${title}`}
              >
                {unless(
                  props.disableToggling,
                  <ObjectIndicator open={open} toggle={handleOnClick} />,
                )}
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
          </UIGridRow>
        </InspectorContextMenuWrapper>
      </div>
      {when(
        open,
        mapToArray((innerControl: RegularControlDescription, prop: string, index: number) => {
          const innerPropPath = PP.appendPropertyPathElems(propPath, [prop])
          if (innerControl.control === 'none') {
            return null
          }
          return (
            <RowForControl
              key={`object-control-row-${PP.toString(innerPropPath)}`}
              controlDescription={innerControl}
              isScene={isScene}
              propPath={innerPropPath}
              setGlobalCursor={props.setGlobalCursor}
              indentationLevel={
                props.shouldIncreaseIdentation ? props.indentationLevel + 1 : props.indentationLevel
              }
              shouldIncreaseIdentation={props.shouldIncreaseIdentation}
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
RowForObjectControl.displayName = 'RowForObjectControl'

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
RowForUnionControl.displayName = 'RowForUnionControl'

interface RowForControlProps extends AbstractRowForControlProps {
  controlDescription: Exclude<RegularControlDescription, NoneControlDescription>
  disableToggling?: boolean
}

export const RowForControl = React.memo((props: RowForControlProps) => {
  const { controlDescription, disableToggling, propPath } = props
  if (isBaseControlDescription(controlDescription)) {
    return <RowForBaseControl {...props} controlDescription={controlDescription} />
  } else if (propPath.propertyElements[0] === 'children') {
    // just show a single element for arrays of children
    return (
      <RowForBaseControl
        {...props}
        controlDescription={{ control: 'jsx', preferredChildComponents: [], label: 'children' }}
      />
    )
  } else {
    switch (controlDescription.control) {
      case 'array':
        return (
          <RowForArrayControl
            {...props}
            disableToggling={disableToggling ?? false}
            controlDescription={controlDescription}
          />
        )
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
RowForControl.displayName = 'RowForControl'

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
    Substores.projectContents,
    (state) => {
      const underlyingTarget = normalisePathToUnderlyingTarget(state.editor.projectContents, target)
      return underlyingTarget.type === 'NORMALISE_PATH_SUCCESS' ? underlyingTarget.filePath : null
    },
    'ComponentSectionInner locationOfComponentInstance',
  )
  ComponentSectionInner.displayName = 'ComponentSectionInner'

  const openInstanceFile = React.useCallback(() => {
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

      const descriptorFile =
        registeredComponent?.source.type === 'DESCRIPTOR_FILE' ? registeredComponent.source : null

      if (registeredComponent?.label == null) {
        return {
          displayName: elementName,
          descriptorFile: descriptorFile,
          isRegisteredComponent: registeredComponent != null,
        }
      }

      return {
        displayName: registeredComponent.label,
        descriptorFile: descriptorFile,
        isRegisteredComponent: registeredComponent != null,
        secondaryName: elementName,
      }
    },
    'ComponentSectionInner componentName',
  )

  const openDescriptorFile = React.useCallback(() => {
    if (componentData?.descriptorFile != null) {
      dispatch([
        openCodeEditorFile(
          componentData.descriptorFile.sourceDescriptorFile,
          true,
          componentData.descriptorFile.bounds,
        ),
      ])
    }
  }, [dispatch, componentData?.descriptorFile])

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
          style={{
            flexGrow: 1,
            height: UtopiaTheme.layout.rowHeight.large,
            fontWeight: 600,
            textOverflow: 'ellipsis',
            whiteSpace: 'nowrap',
            overflow: 'hidden',
            gap: 8,
          }}
        >
          {componentData != null ? (
            <React.Fragment>
              <span
                onClick={openInstanceFile}
                style={{ cursor: 'pointer', textTransform: 'uppercase' }}
              >
                {componentData.displayName}
              </span>
              {when(
                componentData.isRegisteredComponent,
                <span onClick={openDescriptorFile} style={{ cursor: 'pointer', fontSize: 6 }}>
                  ◇
                </span>,
              )}
              {componentData.secondaryName == null ? null : (
                <span
                  onClick={openDescriptorFile}
                  style={{ cursor: 'pointer', opacity: 0.5, fontWeight: 'initial' }}
                >
                  {componentData.secondaryName}
                </span>
              )}
            </React.Fragment>
          ) : (
            <span onClick={openInstanceFile}>Component</span>
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
  display: 'flex',
  alignItems: 'center',
  height: 34,
  fontWeight: 500,
  gap: 4,
} as const
