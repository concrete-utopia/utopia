/** @jsxRuntime classic */
/** @jsx jsx */ import { jsx } from '@emotion/react'
import React from 'react'
import { MetadataUtils } from '../../../../core/model/element-metadata-utils'
import * as EP from '../../../../core/shared/element-path'
import { ElementPath } from '../../../../core/shared/project-file-types'
import {
  Button,
  FlexRow,
  InspectorSectionHeader,
  InspectorSectionIcons,
  useColorTheme,
} from '../../../../uuiui'
import { updateConditionals } from '../../../editor/actions/action-creators'
import { useDispatch } from '../../../editor/store/dispatch-context'
import { Substores, useEditorState } from '../../../editor/store/store-hook'
import { UIGridRow } from '../../widgets/ui-grid-row'

export const ConditionalSection = React.memo(({ paths }: { paths: ElementPath[] }) => {
  const dispatch = useDispatch()
  const colorTheme = useColorTheme()
  const jsxMetadata = useEditorState(
    Substores.metadata,
    (store) => store.editor.jsxMetadata,
    'Metadata',
  )
  const conditionals = useEditorState(
    Substores.restOfEditor,
    (store) => store.editor.conditionals,
    'Conditionals',
  )
  const condition = React.useMemo(() => {
    if (paths.length !== 1) {
      return true
    }
    return conditionals[EP.toString(paths[0])] === false ? false : true
  }, [conditionals, paths])

  const setCondition = React.useCallback(
    (value: boolean) => () => {
      if (paths.length !== 1) {
        return
      }
      dispatch([updateConditionals(paths[0], value)], 'everyone')
    },
    [dispatch, paths],
  )

  if (paths.length !== 1) {
    return null
  }
  const path = paths[0]
  if (!MetadataUtils.isElementPathConditionalFromMetadata(jsxMetadata, path)) {
    return null
  }

  return (
    <div
      style={{
        marginTop: 10,
        paddingBottom: 10,
        borderBottom: `1px solid ${colorTheme.subduedBorder.value}`,
      }}
    >
      <InspectorSectionHeader
        css={{
          marginTop: 8,
          transition: 'color .1s ease-in-out',
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
            alignSelf: 'stretch',
            gap: 8,
          }}
        >
          <InspectorSectionIcons.Conditionals />
          <span>Conditional</span>
        </FlexRow>
      </InspectorSectionHeader>
      <React.Fragment>
        <UIGridRow padded={true} variant='<--1fr--><--1fr-->'>
          Branch
          <FlexRow style={{ flexGrow: 1, gap: 4 }}>
            <Button
              style={{ flex: 1 }}
              spotlight={condition}
              highlight
              onMouseDown={setCondition(true)}
            >
              True
            </Button>
            <Button
              style={{ flex: 1 }}
              spotlight={!condition}
              highlight
              onMouseDown={setCondition(false)}
            >
              False
            </Button>
          </FlexRow>
        </UIGridRow>
      </React.Fragment>
    </div>
  )
})
