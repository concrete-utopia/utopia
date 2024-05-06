/** @jsxRuntime classic */
/** @jsx jsx */ import { jsx } from '@emotion/react'
import createCachedSelector from 're-reselect'
import React from 'react'
import { usePopper } from 'react-popper'
import type { ArrayControlDescription } from 'utopia-api/core'
import { MetadataUtils } from '../../../../core/model/element-metadata-utils'
import { mapDropNulls } from '../../../../core/shared/array-utils'
import { isLeft } from '../../../../core/shared/either'
import * as EP from '../../../../core/shared/element-path'
import type {
  ElementInstanceMetadata,
  ElementInstanceMetadataMap,
  JSXMapExpression,
} from '../../../../core/shared/element-template'
import { isJSXMapExpression } from '../../../../core/shared/element-template'
import type { ElementPath } from '../../../../core/shared/project-file-types'
import { NO_OP, assertNever } from '../../../../core/shared/utils'
import type { JSXParsedValue } from '../../../../utils/value-parser-utils'
import {
  Button,
  FlexRow,
  Icn,
  InspectorSectionIcons,
  UtopiaTheme,
  useColorTheme,
} from '../../../../uuiui'
import { Substores, useEditorState } from '../../../editor/store/store-hook'
import type { MetadataSubstate } from '../../../editor/store/store-hook-substore-types'
import { UIGridRow } from '../../widgets/ui-grid-row'
import { DataPickerPopupButtonTestId } from '../component-section/component-section'
import type { VariableOption } from '../component-section/data-picker-popup'
import { DataPickerPopup, dataPickerForAnElement } from '../component-section/data-picker-popup'
import {
  DataCartoucheInner,
  getTextContentOfElement,
} from '../component-section/data-reference-cartouche'
import { JSXPropertyControlForListSection } from '../component-section/property-control-controls'
import { useVariablesInScopeForSelectedElement } from '../component-section/variables-in-scope-utils'

type MapExpression = JSXMapExpression | 'multiselect' | 'not-a-mapexpression'

const mapExpressionValueToMapSelector = createCachedSelector(
  (store: MetadataSubstate) => store.editor.jsxMetadata,
  (_store: MetadataSubstate, paths: ElementPath[]) => paths,
  (jsxMetadata: ElementInstanceMetadataMap, paths: ElementPath[]): MapExpression => {
    const element = getMapExpressionMetadata(jsxMetadata, paths)

    if (element === 'not-a-mapexpression' || element === 'multiselect') {
      return element
    }

    return element.element
  },
)((_, paths) => paths.map(EP.toString).join(','))

function getMapExpressionMetadata(
  jsxMetadata: ElementInstanceMetadataMap,
  paths: ElementPath[],
):
  | { metadata: ElementInstanceMetadata; element: JSXMapExpression }
  | 'multiselect'
  | 'not-a-mapexpression' {
  const elementMetadatas = mapDropNulls((path) => {
    const elementMetadata = MetadataUtils.findElementByElementPath(jsxMetadata, path)
    if (
      elementMetadata == null ||
      isLeft(elementMetadata.element) ||
      !isJSXMapExpression(elementMetadata.element.value)
    ) {
      return null
    }

    return {
      metadata: elementMetadata,
      element: elementMetadata.element.value,
    }
  }, paths)

  if (elementMetadatas.length === 0) {
    return 'not-a-mapexpression'
  }

  if (elementMetadatas.length > 1) {
    return 'multiselect'
  }

  return elementMetadatas.at(0) ?? 'not-a-mapexpression'
}

export const ListSectionTestId = 'list-section'

function filterVariableOption(option: VariableOption): VariableOption | null {
  switch (option.type) {
    case 'array':
      return {
        ...option,
        children: filterKeepArraysOnly(option.children),
        disabled: false,
      }
    case 'object':
      const chilren = filterKeepArraysOnly(option.children)
      if (chilren.length === 0) {
        // no array-valued children found
        return null
      }
      return {
        ...option,
        children: chilren,
        disabled: true,
      }
    case 'jsx':
    case 'primitive':
      return null
    default:
      assertNever(option)
  }
}

function filterKeepArraysOnly(options: VariableOption[]): VariableOption[] {
  return mapDropNulls((o) => filterVariableOption(o), options)
}

function useDataPickerButton(selectedElements: Array<ElementPath>) {
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
  const togglePopup = React.useCallback(() => setPopupIsOpen((v) => !v), [])
  const openPopup = React.useCallback(() => setPopupIsOpen(true), [])
  const closePopup = React.useCallback(() => setPopupIsOpen(false), [])

  const onClick = React.useCallback(
    (e: React.MouseEvent) => {
      e.stopPropagation()
      e.preventDefault()

      togglePopup()
    },
    [togglePopup],
  )

  const selectedElement = selectedElements.at(0) ?? EP.emptyElementPath

  const variablePickerButtonAvailable =
    useVariablesInScopeForSelectedElement(selectedElement, null, 'all').length > 0
  const variablePickerButtonTooltipText = variablePickerButtonAvailable
    ? 'Pick data source'
    : 'No data sources available'

  const pickerType = React.useMemo(() => {
    return dataPickerForAnElement(selectedElement)
  }, [selectedElement])

  const DataPickerComponent = React.useMemo(
    () => (
      <DataPickerPopup
        {...popper.attributes.popper}
        style={popper.styles.popper}
        closePopup={closePopup}
        ref={setPopperElement}
        pickerType={pickerType}
        customizeOptions={filterKeepArraysOnly}
      />
    ),
    [closePopup, pickerType, popper.attributes.popper, popper.styles.popper],
  )

  const DataPickerOpener = React.useMemo(
    () => (
      <Button
        onClick={onClick}
        data-testid={DataPickerPopupButtonTestId}
        disabled={!variablePickerButtonAvailable}
      >
        <Icn
          type='pipette'
          color='secondary'
          tooltipText={variablePickerButtonTooltipText}
          width={18}
          height={18}
        />
      </Button>
    ),
    [onClick, variablePickerButtonAvailable, variablePickerButtonTooltipText],
  )

  return {
    popupIsOpen,
    DataPickerOpener,
    DataPickerComponent,
    setReferenceElement,
    openPopup,
  }
}

export const ListSection = React.memo(({ paths }: { paths: ElementPath[] }) => {
  const colorTheme = useColorTheme()

  const originalMapExpression = useEditorState(
    Substores.metadata,
    (store) => mapExpressionValueToMapSelector(store, paths),
    'ConditionalSection condition expression',
  )

  const metadataForElement = useEditorState(
    Substores.metadata,
    (store) => MetadataUtils.findElementByElementPath(store.editor.jsxMetadata, paths.at(0)),
    'ConditionalSection metadata',
  )

  const { popupIsOpen, DataPickerOpener, DataPickerComponent, setReferenceElement, openPopup } =
    useDataPickerButton(paths)

  const mappedRootElementToDisplay = useEditorState(
    Substores.metadata,
    (store) => {
      const path = paths.at(0)
      const elementMetadata: ElementInstanceMetadata | null | undefined =
        path == null
          ? null
          : MetadataUtils.getChildrenOrdered(
              store.editor.jsxMetadata,
              store.editor.elementPathTree,
              path,
            ).at(0)

      return elementMetadata == null || isLeft(elementMetadata.element)
        ? null
        : getTextContentOfElement(elementMetadata.element.value, elementMetadata)
    },
    'ConditionalSection mappedRootElement',
  )

  if (originalMapExpression === 'multiselect' || originalMapExpression === 'not-a-mapexpression') {
    return null
  }

  const contentsToDisplay = getTextContentOfElement(
    originalMapExpression.valueToMap,
    metadataForElement,
  )

  return (
    <div style={{ paddingBottom: 8 }} data-testid={ListSectionTestId}>
      <FlexRow
        css={{
          height: UtopiaTheme.layout.rowHeight.large,
          label: 'subsection-header',
          padding: `0 ${UtopiaTheme.layout.inspectorXPadding}px`,
          transition: 'color .1s ease-in-out',
          color: colorTheme.fg1.value,
          '--buttonContentOpacity': 0.3,
          '&:hover': {
            color: colorTheme.fg1.value,
            '--buttonContentOpacity': 1,
          },
        }}
      >
        <FlexRow
          style={{
            flexGrow: 1,
            gap: 8,
            color: colorTheme.dynamicBlue.value,
            textTransform: 'uppercase',
          }}
        >
          <InspectorSectionIcons.Code style={{ width: 16, height: 16 }} color='dynamic' />
          <span>List</span>
        </FlexRow>
      </FlexRow>
      {popupIsOpen ? DataPickerComponent : null}
      <UIGridRow
        padded={false}
        style={{ paddingLeft: 8, paddingRight: 8, paddingTop: 3, paddingBottom: 3 }}
        variant='<--1fr--><--1fr-->|-18px-|'
      >
        List Source
        <div
          style={{
            display: 'flex',
            flexDirection: 'row',
            minWidth: 0,
          }}
          ref={setReferenceElement}
        >
          <DataCartoucheInner
            contentsToDisplay={contentsToDisplay}
            onClick={openPopup}
            onDoubleClick={NO_OP}
            selected={false}
          />
        </div>
        {DataPickerOpener}
      </UIGridRow>
      <UIGridRow
        padded={false}
        style={{ paddingLeft: 8, paddingRight: 8, paddingTop: 3, paddingBottom: 3 }}
        variant='<--1fr--><--1fr-->|-18px-|'
      >
        Contents
        <div
          style={{
            display: 'flex',
            flexDirection: 'row',
            minWidth: 0,
          }}
          ref={setReferenceElement}
        >
          <JSXPropertyControlForListSection
            value={
              {
                type: 'internal-component',
                name: mappedRootElementToDisplay?.label,
              } as JSXParsedValue
            }
          />
        </div>
      </UIGridRow>
    </div>
  )
})
