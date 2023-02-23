/** @jsxRuntime classic */
/** @jsx jsx */ import { jsx } from '@emotion/react'
import React from 'react'
import { MetadataUtils } from '../../../../core/model/element-metadata-utils'
import { findUtopiaCommentFlag } from '../../../../core/shared/comment-flags'
import { isRight } from '../../../../core/shared/either'
import { isJSXConditionalExpression } from '../../../../core/shared/element-template'
import { ElementPath } from '../../../../core/shared/project-file-types'
import {
  Button,
  FlexRow,
  InspectorSectionIcons,
  InspectorSubsectionHeader,
  useColorTheme,
} from '../../../../uuiui'
import { setConditionalOverriddenCondition } from '../../../editor/actions/action-creators'
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
      return null
    }
    return findUtopiaCommentFlag(element.comments, 'conditional')?.value ?? null
  }, [element])

  const setCondition = React.useCallback(
    (value: boolean) => () => {
      if (path == null || element == null) {
        return
      }
      dispatch([setConditionalOverriddenCondition(path, value)])
    },
    [dispatch, path, element],
  )

  if (element == null) {
    return null
  }

  return (
    <React.Fragment>
      <InspectorSubsectionHeader>
        <FlexRow
          style={{
            flexGrow: 1,
            gap: 8,
          }}
        >
          <InspectorSectionIcons.Conditionals style={{ width: 16, height: 16 }} />
          <span>Conditional</span>
        </FlexRow>
      </InspectorSubsectionHeader>
      <UIGridRow
        padded={true}
        variant='<---1fr--->|------172px-------|'
        style={{ color: condition != null ? colorTheme.primary.value : 'inherit' }}
      >
        Branch
        <FlexRow style={{ flexGrow: 1, gap: 4 }}>
          <Button
            style={{ flex: 1 }}
            spotlight={condition !== false}
            highlight
            onClick={setCondition(true)}
            data-testid='conditionals-control-true'
          >
            True
          </Button>
          <Button
            style={{ flex: 1 }}
            spotlight={condition === false}
            highlight
            onClick={setCondition(false)}
            data-testid='conditionals-control-false'
          >
            False
          </Button>
        </FlexRow>
      </UIGridRow>
    </React.Fragment>
  )
})
