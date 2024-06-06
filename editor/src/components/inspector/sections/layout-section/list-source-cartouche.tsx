/** @jsxRuntime classic */
/** @jsx jsx */ import { jsx } from '@emotion/react'
import React from 'react'
import { MetadataUtils } from '../../../../core/model/element-metadata-utils'
import type { ElementPath } from '../../../../core/shared/project-file-types'
import { NO_OP, assertNever } from '../../../../core/shared/utils'
import { Substores, useEditorState } from '../../../editor/store/store-hook'
import {
  DataCartoucheInner,
  getTextContentOfElement,
} from '../component-section/data-reference-cartouche'
import { mapExpressionValueToMapSelector } from './list-section'
import { useDataPickerButton } from '../component-section/component-section'
import { useDispatch } from '../../../editor/store/dispatch-context'
import type { JSExpressionOtherJavaScript } from '../../../../core/shared/element-template'
import { replaceElementInScope } from '../../../editor/actions/action-creators'
import { useAtom } from 'jotai'
import type { DataPickerOption } from '../component-section/data-picker-utils'
import {
  DataPickerPreferredAllAtom,
  jsxElementChildToValuePath,
} from '../component-section/data-picker-utils'
import {
  getCartoucheDataTypeForExpression,
  useVariablesInScopeForSelectedElement,
} from '../component-section/variables-in-scope-utils'
import { mapDropNulls } from '../../../../core/shared/array-utils'
import { traceDataFromElement, dataPathSuccess } from '../../../../core/data-tracing/data-tracing'
import type { CartoucheDataType } from '../component-section/cartouche-ui'

function filterVariableOption(option: DataPickerOption): DataPickerOption | null {
  switch (option.type) {
    case 'array':
      return {
        ...option,
        children: filterKeepArraysOnly(option.children),
        disabled: false,
      }
    case 'object':
      const children = filterKeepArraysOnly(option.children)
      if (children.length === 0) {
        // no array-valued children found
        return null
      }
      return {
        ...option,
        children: children,
        disabled: true,
      }
    case 'jsx':
    case 'primitive':
      return null
    default:
      assertNever(option)
  }
}

function filterKeepArraysOnly(options: DataPickerOption[]): DataPickerOption[] {
  return mapDropNulls((o) => filterVariableOption(o), options)
}

interface MapListSourceCartoucheProps {
  target: ElementPath
  selected: boolean
  openOn: 'single-click' | 'double-click'
}

export const MapListSourceCartouche = React.memo((props: MapListSourceCartoucheProps) => {
  const { target, openOn } = props

  const originalMapExpression = useEditorState(
    Substores.metadata,
    (store) => mapExpressionValueToMapSelector(store, [target]),
    'ConditionalSection condition expression',
  )

  const metadataForElement = useEditorState(
    Substores.metadata,
    (store) => MetadataUtils.findElementByElementPath(store.editor.jsxMetadata, target),
    'ConditionalSection metadata',
  )

  const dispatch = useDispatch()

  const onPickMappedElement = React.useCallback(
    (expression: JSExpressionOtherJavaScript) => {
      dispatch([
        replaceElementInScope(target, {
          type: 'update-map-expression',
          valueToMap: expression,
        }),
      ])
    },
    [dispatch, target],
  )

  const [preferredAllState] = useAtom(DataPickerPreferredAllAtom)

  const variableNamesInScope = useVariablesInScopeForSelectedElement(
    target,
    null,
    preferredAllState,
  )

  const filteredVariableNamesInScope = React.useMemo(
    () => filterKeepArraysOnly(variableNamesInScope),
    [variableNamesInScope],
  )

  const pathToMappedExpression = React.useMemo(
    () =>
      originalMapExpression === 'multiselect' || originalMapExpression === 'not-a-mapexpression'
        ? null
        : jsxElementChildToValuePath(originalMapExpression.valueToMap),
    [originalMapExpression],
  )

  const { popupIsOpen, DataPickerComponent, setReferenceElement, openPopup } = useDataPickerButton(
    filteredVariableNamesInScope,
    onPickMappedElement,
    pathToMappedExpression,
    target,
  )

  const onClick = React.useCallback(() => {
    if (openOn === 'single-click') {
      openPopup()
    }
  }, [openOn, openPopup])

  const onDoubleClick = React.useCallback(() => {
    if (openOn === 'double-click') {
      openPopup()
    }
  }, [openOn, openPopup])

  const isDataComingFromHookResult = useEditorState(
    Substores.projectContentsAndMetadata,
    (store) => {
      if (
        originalMapExpression === 'multiselect' ||
        originalMapExpression === 'not-a-mapexpression'
      ) {
        return false
      } else {
        const traceDataResult = traceDataFromElement(
          originalMapExpression.valueToMap,
          target,
          store.editor.jsxMetadata,
          store.editor.projectContents,
          dataPathSuccess([]),
        )
        return traceDataResult.type === 'hook-result'
      }
    },
    'ListSection isDataComingFromHookResult',
  )

  const cartoucheDataType: CartoucheDataType = useEditorState(
    Substores.projectContentsAndMetadataAndVariablesInScope,
    (store) => {
      if (
        originalMapExpression === 'multiselect' ||
        originalMapExpression === 'not-a-mapexpression'
      ) {
        return 'unknown'
      }
      return getCartoucheDataTypeForExpression(
        target,
        originalMapExpression.valueToMap,
        store.editor.variablesInScope,
      )
    },
    'MapListSourceCartouche cartoucheDataType',
  )

  if (originalMapExpression === 'multiselect' || originalMapExpression === 'not-a-mapexpression') {
    return null
  }

  const contentsToDisplay = getTextContentOfElement(
    originalMapExpression.valueToMap,
    metadataForElement,
  )

  return (
    <div
      style={{
        display: 'flex',
        flexDirection: 'row',
        justifyContent: 'flex-end',
        minWidth: 0,
      }}
    >
      {popupIsOpen ? DataPickerComponent : null}
      <DataCartoucheInner
        ref={setReferenceElement}
        contentsToDisplay={contentsToDisplay}
        onClick={onClick}
        onDoubleClick={onDoubleClick}
        onDelete={NO_OP}
        selected={props.selected}
        safeToDelete={false}
        testId='list-source-cartouche'
        contentIsComingFromServer={isDataComingFromHookResult}
        datatype={cartoucheDataType}
      />
    </div>
  )
})
