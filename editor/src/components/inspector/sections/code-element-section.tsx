/** @jsxRuntime classic */
/** @jsx jsx */ import { jsx } from '@emotion/react'
import React from 'react'
import {
  FlexRow,
  FormButton,
  InspectorSectionIcons,
  UtopiaTheme,
  useColorTheme,
} from '../../../uuiui'
import { Substores, useEditorState } from '../../editor/store/store-hook'
import { MetadataUtils } from '../../../core/model/element-metadata-utils'
import type { ElementPath } from '../../../core/shared/project-file-types'
import { UIGridRow } from '../widgets/ui-grid-row'
import { useDispatch } from '../../editor/store/dispatch-context'
import { openCodeEditor } from '../../editor/actions/action-creators'

export const CodeElementSectionTestId = 'code-element-section-test-id'

export const CodeElementSection = React.memo(({ paths }: { paths: ElementPath[] }) => {
  const dispatch = useDispatch()
  const colorTheme = useColorTheme()
  const isCodeElement = useEditorState(
    Substores.metadata,
    (store) => {
      return paths.every((path) => {
        return MetadataUtils.isExpressionOtherJavascript(path, store.editor.jsxMetadata)
      })
    },
    'CodeElementSection isCodeElement',
  )

  const dispatchOpenCodeEditor = React.useCallback(() => dispatch([openCodeEditor()]), [dispatch])

  if (!isCodeElement) {
    return null
  }

  return (
    <div data-testid={CodeElementSectionTestId}>
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
          }}
        >
          <InspectorSectionIcons.Code style={{ width: 16, height: 16 }} />
          <span>Code</span>
        </FlexRow>
      </FlexRow>
      <UIGridRow padded={true} variant='<--1fr--><--1fr--><--1fr-->'>
        <div />
        <FormButton onClick={dispatchOpenCodeEditor}>Show Code</FormButton>
        <div />
      </UIGridRow>
    </div>
  )
})
