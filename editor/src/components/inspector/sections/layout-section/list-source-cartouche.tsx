/** @jsxRuntime classic */
/** @jsx jsx */ import { jsx } from '@emotion/react'
import React from 'react'
import { MetadataUtils } from '../../../../core/model/element-metadata-utils'
import type { ElementPath } from '../../../../core/shared/project-file-types'
import { NO_OP } from '../../../../core/shared/utils'
import { Substores, useEditorState } from '../../../editor/store/store-hook'
import {
  DataCartoucheInner,
  getTextContentOfElement,
} from '../component-section/data-reference-cartouche'
import { mapExpressionValueToMapSelector, useDataPickerButtonListSource } from './list-section'
import { traceDataFromElement } from '../../../../core/data-tracing/data-tracing'

interface MapListSourceCartoucheProps {
  target: ElementPath
  inverted: boolean
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

  const { popupIsOpen, DataPickerComponent, setReferenceElement, openPopup } =
    useDataPickerButtonListSource([target])

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
          [],
        )
        return traceDataResult.type === 'hook-result'
      }
    },
    'ListSection isDataComingFromHookResult',
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
        inverted={props.inverted}
        safeToDelete={false}
        testId='list-source-cartouche'
        contentIsComingFromServer={isDataComingFromHookResult}
      />
    </div>
  )
})
