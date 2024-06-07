/** @jsxRuntime classic */
/** @jsx jsx */ import { jsx } from '@emotion/react'
import createCachedSelector from 're-reselect'
import React from 'react'
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
import type { JSXParsedValue } from '../../../../utils/value-parser-utils'
import { FlexRow, InspectorSectionIcons, UtopiaTheme, useColorTheme } from '../../../../uuiui'
import { Substores, useEditorState } from '../../../editor/store/store-hook'
import type { MetadataSubstate } from '../../../editor/store/store-hook-substore-types'
import { UIGridRow } from '../../widgets/ui-grid-row'
import { getTextContentOfElement } from '../component-section/data-reference-cartouche'
import { JSXPropertyControlForListSection } from '../component-section/property-control-controls'
import { MapListSourceCartouche } from './list-source-cartouche'

type MapExpression = JSXMapExpression | 'multiselect' | 'not-a-mapexpression'

export const mapExpressionValueToMapSelector = createCachedSelector(
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

export const ListSection = React.memo(({ paths }: { paths: ElementPath[] }) => {
  const target = paths.at(0)
  const colorTheme = useColorTheme()

  const originalMapExpression = useEditorState(
    Substores.metadata,
    (store) => mapExpressionValueToMapSelector(store, paths),
    'ConditionalSection condition expression',
  )

  const mappedRootElementToDisplay = useEditorState(
    Substores.metadata,
    (store) => {
      const path = target
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

  if (
    originalMapExpression === 'multiselect' ||
    originalMapExpression === 'not-a-mapexpression' ||
    target == null
  ) {
    return null
  }

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
            textTransform: 'uppercase',
          }}
        >
          <InspectorSectionIcons.Code style={{ width: 16, height: 16 }} color='main' />
          <span>List</span>
        </FlexRow>
      </FlexRow>
      <UIGridRow padded variant='<--1fr--><--1fr-->'>
        List Source
        <MapListSourceCartouche target={target} openOn='double-click' selected={false} />
      </UIGridRow>
      <UIGridRow padded variant='<--1fr--><--1fr-->'>
        Contents
        <div
          style={{
            display: 'flex',
            flexDirection: 'row',
            justifyContent: 'flex-end',
            minWidth: 0,
          }}
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
