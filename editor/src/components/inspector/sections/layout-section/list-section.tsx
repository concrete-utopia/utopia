/** @jsxRuntime classic */
/** @jsx jsx */ import { jsx } from '@emotion/react'
import createCachedSelector from 're-reselect'
import React from 'react'
import type { ConditionalCase } from '../../../../core/model/conditionals'
import { getConditionalClausePath } from '../../../../core/model/conditionals'
import { MetadataUtils } from '../../../../core/model/element-metadata-utils'
import { mapDropNulls } from '../../../../core/shared/array-utils'
import {
  findUtopiaCommentFlag,
  isUtopiaCommentFlagConditional,
} from '../../../../core/shared/comment-flags'
import { foldEither, isLeft } from '../../../../core/shared/either'
import * as EP from '../../../../core/shared/element-path'
import type {
  ConditionValue,
  ElementInstanceMetadata,
  ElementInstanceMetadataMap,
  JSXConditionalExpression,
  JSXElementChild,
  JSXMapExpression,
} from '../../../../core/shared/element-template'
import {
  isJSXConditionalExpression,
  isJSXMapExpression,
  modifiableAttributeToValuePath,
} from '../../../../core/shared/element-template'
import { optionalMap } from '../../../../core/shared/optional-utils'
import type { ElementPath } from '../../../../core/shared/project-file-types'
import { unless } from '../../../../utils/react-conditionals'
import {
  Button,
  FlexColumn,
  FlexRow,
  Icn,
  iconForControlType,
  Icons,
  InspectorSectionIcons,
  SquareButton,
  StringInput,
  Tooltip,
  useColorTheme,
  UtopiaStyles,
  UtopiaTheme,
} from '../../../../uuiui'
import { isEntryAConditionalSlot } from '../../../canvas/canvas-utils'
import type { EditorAction } from '../../../editor/action-types'
import {
  setConditionalOverriddenCondition,
  switchConditionalBranches,
  updateConditionalExpression,
} from '../../../editor/actions/action-creators'
import { useDispatch } from '../../../editor/store/dispatch-context'
import type { NavigatorEntry } from '../../../editor/store/editor-state'
import { Substores, useEditorState } from '../../../editor/store/store-hook'
import type { MetadataSubstate } from '../../../editor/store/store-hook-substore-types'
import { LayoutIcon } from '../../../navigator/navigator-item/layout-icon'
import {
  getNavigatorEntryLabel,
  labelSelector,
} from '../../../navigator/navigator-item/navigator-item-wrapper'
import { getNavigatorTargets } from '../../../navigator/navigator-utils'
import type { ControlStatus } from '../../common/control-status'
import { getControlStyles } from '../../common/control-styles'
import { usePropControlledStateV2 } from '../../common/inspector-utils'
import { ConditionalOverrideControl } from '../../controls/conditional-override-control'
import { UIGridRow } from '../../widgets/ui-grid-row'
import { usePopper } from 'react-popper'
import type { ArrayControlDescription, RegularControlDescription } from 'utopia-api/core'
import { useVariablesInScopeForSelectedElement } from '../component-section/variables-in-scope-utils'
import { DataPickerPopupButtonTestId } from '../component-section/component-section'
import {
  dataPickerForAnElement,
  dataPickerForAProperty,
  DataPickerPopup,
} from '../component-section/data-picker-popup'
import { ConditionalsControlSectionExpressionTestId } from './conditional-section'
import { jsxElementChildToText } from '../../../canvas/ui-jsx-canvas-renderer/jsx-element-child-to-text'
import { NO_OP } from '../../../../core/shared/utils'
import { IdentifierExpressionCartoucheControl } from '../component-section/cartouche-control'
import { getTextContentOfElement } from '../component-section/data-reference-cartouche'

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

  return elementMetadatas[0]
}

export const ListSectionTestId = 'list-section'

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
    (store) => MetadataUtils.findElementByElementPath(store.editor.jsxMetadata, paths[0]),
    'ConditionalSection metadata',
  )

  const controlDescription: ArrayControlDescription = {
    control: 'array',
    propertyControl: {
      control: 'none',
    },
  }

  const { popupIsOpen, DataPickerOpener, DataPickerComponent, setReferenceElement, openPopup } =
    useDataPickerButton(paths)

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
      <FlexRow
        css={{
          height: UtopiaTheme.layout.rowHeight.large,
          padding: `0 ${UtopiaTheme.layout.inspectorXPadding}px`,
        }}
      >
        {popupIsOpen ? DataPickerComponent : null}
        <UIGridRow
          padded={false}
          style={{ paddingLeft: 0, paddingRight: 8, paddingTop: 3, paddingBottom: 3 }}
          variant='<--1fr--><--1fr-->|-18px-|'
        >
          Value To Map
          <div
            style={{
              display: 'flex',
              flexDirection: 'row',
              minWidth: 0,
            }}
            ref={setReferenceElement}
          >
            <IdentifierExpressionCartoucheControl
              contents={contentsToDisplay.label ?? ''}
              icon={React.createElement(iconForControlType(controlDescription.control))}
              matchType={contentsToDisplay.type === 'literal' ? 'none' : 'full'}
              onOpenDataPicker={openPopup}
              onDeleteCartouche={NO_OP}
              testId={`cartouche-map-expression-value-to-map`}
              safeToDelete={false}
            />
            {DataPickerOpener}
          </div>
        </UIGridRow>
      </FlexRow>
    </div>
  )
})
