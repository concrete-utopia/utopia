/** @jsxRuntime classic */
/** @jsx jsx */ import { jsx } from '@emotion/react'
import React from 'react'
import {
  FlexRow,
  InspectorSectionHeader,
  InspectorSectionIcons,
  UtopiaTheme,
  useColorTheme,
} from '../../../uuiui'
import { Substores, useEditorState } from '../../editor/store/store-hook'
import { MetadataUtils } from '../../../core/model/element-metadata-utils'
import type { ElementPath } from '../../../core/shared/project-file-types'
import { UIGridRow } from '../widgets/ui-grid-row'
import { getElementFromProjectContents } from '../../editor/store/editor-state'
import { mapDropNulls } from '../../../core/shared/array-utils'
import { getTextContentOfElement } from './component-section/data-reference-cartouche'

export const DataReferenceSectionId = 'code-element-section-test-id'

export const DataReferenceSection = React.memo(({ paths }: { paths: ElementPath[] }) => {
  const colorTheme = useColorTheme()

  const elements = useEditorState(
    Substores.projectContents,
    (store) => {
      return paths.map((path) => {
        return {
          path: path,
          element: getElementFromProjectContents(path, store.editor.projectContents),
        }
      })
    },
    'DataReferenceSection elements',
  )

  const isDataReference = React.useMemo(() => {
    return elements.every((element) => MetadataUtils.isElementDataReference(element.element))
  }, [elements])

  const jsxMetadata = useEditorState(
    Substores.metadata,
    (store) => store.editor.jsxMetadata,
    'DataReferenceSection metadata',
  )

  const labels = React.useMemo(() => {
    if (!isDataReference) {
      return []
    }
    return mapDropNulls((element) => {
      if (element.element == null) {
        return null
      }
      const elementMetadata = MetadataUtils.findElementByElementPath(jsxMetadata, element.path)
      return getTextContentOfElement(element.element, elementMetadata)
    }, elements)
  }, [isDataReference, elements, jsxMetadata])

  if (!isDataReference) {
    return null
  }

  return (
    <div data-testid={DataReferenceSectionId}>
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
          <span>Data Reference</span>
        </FlexRow>
      </FlexRow>
      {labels.map((label, idx) => {
        return (
          <UIGridRow
            padded={true}
            variant='<--------auto-------->|167px|'
            key={`label-${label.type}-${idx}`}
          >
            Value
            <div
              style={{
                backgroundColor: colorTheme.bg3.value,
                fontFamily: 'monospace',
                padding: 3,
                borderRadius: 2,
                textAlign: 'center',
              }}
            >
              {label.label}
            </div>
          </UIGridRow>
        )
      })}
    </div>
  )
})
