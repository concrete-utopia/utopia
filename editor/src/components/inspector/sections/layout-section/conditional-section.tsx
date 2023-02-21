/** @jsxRuntime classic */
/** @jsx jsx */ import { jsx } from '@emotion/react'
import React from 'react'
import { MetadataUtils } from '../../../../core/model/element-metadata-utils'
import { getUtopiaID } from '../../../../core/model/element-template-utils'
import { isRight } from '../../../../core/shared/either'
import * as EP from '../../../../core/shared/element-path'
import {
  childOrBlockIsChild,
  isJSXConditionalExpression,
} from '../../../../core/shared/element-template'
import { ElementPath } from '../../../../core/shared/project-file-types'
import {
  Button,
  FlexRow,
  InspectorSectionHeader,
  InspectorSectionIcons,
  useColorTheme,
} from '../../../../uuiui'
import { EditorAction } from '../../../editor/action-types'
import { setElementsToRerender, updateConditionals } from '../../../editor/actions/action-creators'
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

  const path = React.useMemo(() => {
    if (paths.length !== 1) {
      return null
    }
    return paths[0]
  }, [paths])

  const element = React.useMemo(() => {
    if (path == null) {
      return
    }
    const elementMetadata = MetadataUtils.findElementByElementPath(jsxMetadata, path)
    if (
      elementMetadata == null ||
      !isRight(elementMetadata.element) ||
      !isJSXConditionalExpression(elementMetadata.element.value)
    ) {
      return null
    }
    return elementMetadata.element.value
  }, [jsxMetadata, path])

  const condition = React.useMemo(() => {
    if (element == null) {
      return true
    }
    return conditionals[element.uniqueID] !== false
  }, [conditionals, element])

  const setCondition = React.useCallback(
    (value: boolean) => () => {
      if (path == null || element == null) {
        return
      }

      let actions: EditorAction[] = [updateConditionals(element.uniqueID, value)]

      const branch = value ? element.whenTrue : element.whenFalse
      if (!childOrBlockIsChild(branch)) {
        return
      }
      const branchElements = EP.appendToPath(path, getUtopiaID(branch))
      actions.push(setElementsToRerender([branchElements]))

      dispatch(actions, 'everyone')
    },
    [dispatch, path, element],
  )

  if (element == null) {
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
