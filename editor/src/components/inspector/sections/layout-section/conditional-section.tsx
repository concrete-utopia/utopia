/** @jsxRuntime classic */
/** @jsx jsx */ import { jsx } from '@emotion/react'
import React from 'react'
import { MetadataUtils } from '../../../../core/model/element-metadata-utils'
import { findUtopiaCommentFlag } from '../../../../core/shared/comment-flags'
import { isRight } from '../../../../core/shared/either'
import { isJSXConditionalExpression } from '../../../../core/shared/element-template'
import { ElementPath } from '../../../../core/shared/project-file-types'
import { when } from '../../../../utils/react-conditionals'
import {
  Button,
  FlexRow,
  FunctionIcons,
  Icons,
  InspectorSectionIcons,
  InspectorSubsectionHeader,
  SquareButton,
  useColorTheme,
} from '../../../../uuiui'
import { setConditionalOverriddenCondition } from '../../../editor/actions/action-creators'
import { useDispatch } from '../../../editor/store/dispatch-context'
import { Substores, useEditorState } from '../../../editor/store/store-hook'
import { UIGridRow } from '../../widgets/ui-grid-row'

export const ConditionalsControlSectionOpenTestId = 'conditionals-control-section-open'
export const ConditionalsControlSectionCloseTestId = 'conditionals-control-section-close'
export const ConditionalsControlToggleTrueTestId = 'conditionals-control-toggle-true'
export const ConditionalsControlToggleFalseTestId = 'conditionals-control-toggle-false'

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
    (value: boolean | null) => () => {
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
      <InspectorSubsectionHeader
        css={{
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
          <InspectorSectionIcons.Conditionals style={{ width: 16, height: 16 }} />
          <span>Conditional</span>
        </FlexRow>
        {condition != null ? (
          <SquareButton
            highlight
            onClick={setCondition(null)}
            data-testid={ConditionalsControlSectionCloseTestId}
          >
            <FunctionIcons.Delete />
          </SquareButton>
        ) : (
          <SquareButton
            highlight
            onClick={setCondition(true)}
            data-testid={ConditionalsControlSectionOpenTestId}
          >
            <Icons.Plus style={{ opacity: 'var(--buttonContentOpacity)' }} />
          </SquareButton>
        )}
      </InspectorSubsectionHeader>
      {when(
        condition != null,
        <UIGridRow
          padded={true}
          variant='<---1fr--->|------172px-------|'
          style={{ color: condition != null ? colorTheme.brandNeonPink.value : 'inherit' }}
        >
          Branch
          <FlexRow style={{ flexGrow: 1, gap: 4 }}>
            <Button
              style={{ flex: 1 }}
              spotlight={condition !== false}
              highlight
              onClick={setCondition(true)}
              data-testid={ConditionalsControlToggleTrueTestId}
            >
              True
            </Button>
            <Button
              style={{ flex: 1 }}
              spotlight={condition === false}
              highlight
              onClick={setCondition(false)}
              data-testid={ConditionalsControlToggleFalseTestId}
            >
              False
            </Button>
          </FlexRow>
        </UIGridRow>,
      )}
    </React.Fragment>
  )
})
