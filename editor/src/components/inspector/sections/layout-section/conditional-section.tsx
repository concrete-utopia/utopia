/** @jsxRuntime classic */
/** @jsx jsx */ import { jsx } from '@emotion/react'
import createCachedSelector from 're-reselect'
import React from 'react'
import { createSelector } from 'reselect'
import { MetadataUtils } from '../../../../core/model/element-metadata-utils'
import { mapDropNulls } from '../../../../core/shared/array-utils'
import { findUtopiaCommentFlag } from '../../../../core/shared/comment-flags'
import { isRight } from '../../../../core/shared/either'
import * as EP from '../../../../core/shared/element-path'
import {
  ElementInstanceMetadataMap,
  isJSXConditionalExpression,
} from '../../../../core/shared/element-template'
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
import { EditorAction } from '../../../editor/action-types'
import { setConditionalOverriddenCondition } from '../../../editor/actions/action-creators'
import { useDispatch } from '../../../editor/store/dispatch-context'
import { Substores, useEditorState } from '../../../editor/store/store-hook'
import { MetadataSubstate } from '../../../editor/store/store-hook-substore-types'
import { UIGridRow } from '../../widgets/ui-grid-row'

export const ConditionalsControlSectionOpenTestId = 'conditionals-control-section-open'
export const ConditionalsControlSectionCloseTestId = 'conditionals-control-section-close'
export const ConditionalsControlToggleTrueTestId = 'conditionals-control-toggle-true'
export const ConditionalsControlToggleFalseTestId = 'conditionals-control-toggle-false'

type Condition = boolean | 'mixed' | 'not-overridden' | 'not-conditional'

const conditionSelector = createCachedSelector(
  (store: MetadataSubstate) => store.editor.jsxMetadata,
  (_store: MetadataSubstate, paths: ElementPath[]) => paths,
  (jsxMetadata: ElementInstanceMetadataMap, paths: ElementPath[]): Condition => {
    const elements = mapDropNulls((path) => {
      const elementMetadata = MetadataUtils.findElementByElementPath(jsxMetadata, path)
      if (
        elementMetadata == null ||
        !isRight(elementMetadata.element) ||
        !isJSXConditionalExpression(elementMetadata.element.value)
      ) {
        return null
      }

      return elementMetadata.element.value
    }, paths)

    if (elements.length === 0) {
      return 'not-conditional'
    }

    let conditions = new Set<boolean | null>()
    elements.forEach((element) => {
      const flag = findUtopiaCommentFlag(element.comments, 'conditional')
      conditions.add(flag?.value ?? null)
    })

    switch (conditions.size) {
      case 0:
        return 'not-overridden'
      case 1:
        return conditions.values().next().value ?? 'not-overridden'
      default:
        return 'mixed'
    }
  },
)((_, paths) => paths.map(EP.toString).join(','))

export const ConditionalSection = React.memo(({ paths }: { paths: ElementPath[] }) => {
  const dispatch = useDispatch()
  const colorTheme = useColorTheme()

  const condition = useEditorState(
    Substores.metadata,
    (store) => conditionSelector(store, paths),
    'ConditionalSection condition',
  )

  const setCondition = React.useCallback(
    (value: boolean | null) => () => {
      const actions: EditorAction[] = paths.map((path) =>
        setConditionalOverriddenCondition(path, value),
      )
      dispatch(actions)
    },
    [dispatch, paths],
  )

  if (condition === 'not-conditional') {
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
        {condition != 'not-overridden' ? (
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
        condition != 'not-overridden',
        <UIGridRow
          padded={true}
          variant='<---1fr--->|------172px-------|'
          style={{ color: condition != null ? colorTheme.brandNeonPink.value : 'inherit' }}
        >
          Branch
          <FlexRow style={{ flexGrow: 1, gap: 4 }}>
            <Button
              style={{ flex: 1 }}
              spotlight={condition === true}
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
