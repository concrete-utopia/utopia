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
import { NO_OP } from '../../../../core/shared/utils'
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
import { DataPickerPopup, dataPickerForAnElement } from '../component-section/data-picker-popup'
import {
  DataCartoucheInner,
  getTextContentOfElement,
} from '../component-section/data-reference-cartouche'
import { JSXPropertyControlForListSection } from '../component-section/property-control-controls'
import { useVariablesInScopeForSelectedElement } from '../component-section/variables-in-scope-utils'
import { mapExpressionValueToMapSelector, useDataPickerButtonListSource } from './list-section'
import { NavigatorEntry } from '../../../editor/store/editor-state'

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
      />
    </div>
  )
})
