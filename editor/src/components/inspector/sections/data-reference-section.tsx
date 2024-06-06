/** @jsxRuntime classic */
/** @jsx jsx */ import { jsx } from '@emotion/react'
import React from 'react'
import { FlexRow, UtopiaTheme, useColorTheme } from '../../../uuiui'
import { Substores, useEditorState } from '../../editor/store/store-hook'
import { MetadataUtils } from '../../../core/model/element-metadata-utils'
import type { ElementPath } from '../../../core/shared/project-file-types'
import { UIGridRow } from '../widgets/ui-grid-row'
import { getElementFromProjectContents } from '../../editor/store/editor-state'
import { mapDropNulls } from '../../../core/shared/array-utils'
import {
  DataCartoucheInner,
  getTextContentOfElement,
} from './component-section/data-reference-cartouche'
import { useDataPickerButton } from './component-section/component-section'
import { DataPickerPreferredAllAtom } from './component-section/data-picker-popup'
import { useAtom } from 'jotai'
import * as EP from '../../../core/shared/element-path'
import { useDispatch } from '../../editor/store/dispatch-context'
import { replaceElementInScope } from '../../editor/actions/action-creators'
import { NO_OP } from '../../../core/shared/utils'
import { dataPathSuccess, traceDataFromElement } from '../../../core/data-tracing/data-tracing'
import {
  getCartoucheDataTypeForExpression,
  useVariablesInScopeForSelectedElement,
} from './component-section/variables-in-scope-utils'
import { jsxElementChildToValuePath } from './component-section/data-picker-utils'

export const DataReferenceSectionId = 'code-element-section-test-id'

export const DataReferenceSection = React.memo(({ paths }: { paths: ElementPath[] }) => {
  const colorTheme = useColorTheme()
  const dispatch = useDispatch()

  const dataPickerWrapperRef = React.useRef<HTMLDivElement | null>(null)

  const [preferredAllState] = useAtom(DataPickerPreferredAllAtom)

  const [elementPathForDataPicker, setElementPathForDataPicker] =
    React.useState<ElementPath | null>(null)

  const elements = useEditorState(
    Substores.projectContentsAndMetadataAndVariablesInScope,
    (store) => {
      return mapDropNulls((path) => {
        const element = getElementFromProjectContents(path, store.editor.projectContents)
        if (element == null) {
          return null
        }

        const elementMetadata = MetadataUtils.findElementByElementPath(
          store.editor.jsxMetadata,
          path,
        )

        const isDataComingFromServer = traceDataFromElement(
          element,
          EP.parentPath(path),
          store.editor.jsxMetadata,
          store.editor.projectContents,
          dataPathSuccess([]),
        )

        return {
          element: element,
          textContent: getTextContentOfElement(element, elementMetadata),
          path: path,
          contentIsComingFromServer: isDataComingFromServer.type === 'hook-result',
          datatype: getCartoucheDataTypeForExpression(path, element, store.editor.variablesInScope),
        }
      }, paths)
    },
    'DataReferenceSection elements',
  )

  const isDataReference = React.useMemo(() => {
    return elements.every((element) => MetadataUtils.isElementDataReference(element.element))
  }, [elements])

  const isText = React.useMemo(() => {
    return elements.every((element) => element.element?.type === 'JSX_TEXT_BLOCK')
  }, [elements])

  const varsInScope = useVariablesInScopeForSelectedElement(
    elementPathForDataPicker,
    null,
    preferredAllState,
  )

  const pathToCurrenlySelectedValue = React.useMemo(() => {
    const element = elements.find((e) => EP.pathsEqual(e.path, elementPathForDataPicker))
    return jsxElementChildToValuePath(element?.element ?? null)
  }, [elements, elementPathForDataPicker])

  const dataPickerButtonData = useDataPickerButton(
    varsInScope,
    (expression) => {
      if (elementPathForDataPicker == null) {
        return
      }
      dispatch([
        replaceElementInScope(EP.parentPath(elementPathForDataPicker), {
          type: 'replace-child-with-uid',
          uid: EP.toUid(elementPathForDataPicker),
          replaceWith: expression,
        }),
      ])
    },
    pathToCurrenlySelectedValue,
    elementPathForDataPicker,
  )

  const openPicker = React.useCallback(
    (path: ElementPath | null) => () => {
      if (path == null) {
        return
      }
      dataPickerButtonData.setReferenceElement(dataPickerWrapperRef.current)
      dataPickerButtonData.openPopup()
      setElementPathForDataPicker(path)
    },
    [dataPickerButtonData],
  )

  if (!isDataReference) {
    return null
  }

  return (
    <div data-testid={DataReferenceSectionId}>
      <div ref={dataPickerWrapperRef} style={{ width: 0 }}>
        {dataPickerButtonData.popupIsOpen ? dataPickerButtonData.DataPickerComponent : null}
      </div>

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
            textTransform: 'uppercase',
          }}
        >
          <span>{isText ? 'Text' : 'Data'}</span>
        </FlexRow>
      </FlexRow>

      {elements.map((element) => {
        return (
          <UIGridRow
            padded={true}
            variant='<--------auto-------->|167px|'
            key={`inspector-data-reference-row-${EP.toString(element.path)}`}
          >
            <span>Value</span>

            <DataCartoucheInner
              onClick={openPicker(element.path)}
              onDoubleClick={NO_OP}
              selected={false}
              inverted={false}
              contentsToDisplay={element.textContent}
              safeToDelete={false}
              onDelete={NO_OP}
              testId={`inspector-data-cartouche-${EP.toString(element.path)}`}
              contentIsComingFromServer={element.contentIsComingFromServer}
              datatype={element.datatype}
            />
          </UIGridRow>
        )
      })}
    </div>
  )
})
