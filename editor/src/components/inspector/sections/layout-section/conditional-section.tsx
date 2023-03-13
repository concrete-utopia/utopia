/** @jsxRuntime classic */
/** @jsx jsx */ import { jsx } from '@emotion/react'
import createCachedSelector from 're-reselect'
import React from 'react'
import { MetadataUtils } from '../../../../core/model/element-metadata-utils'
import { mapDropNulls } from '../../../../core/shared/array-utils'
import { findUtopiaCommentFlag } from '../../../../core/shared/comment-flags'
import { isLeft } from '../../../../core/shared/either'
import * as EP from '../../../../core/shared/element-path'
import {
  ElementInstanceMetadataMap,
  isJSXConditionalExpression,
  JSXAttribute,
} from '../../../../core/shared/element-template'
import { ElementPath } from '../../../../core/shared/project-file-types'
import { unless } from '../../../../utils/react-conditionals'
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

type ConditionOverride = boolean | 'mixed' | 'not-overridden' | 'not-conditional'
type ConditionExpression = string | 'multiselect' | 'not-conditional'

const conditionOverrideSelector = createCachedSelector(
  (store: MetadataSubstate) => store.editor.jsxMetadata,
  (_store: MetadataSubstate, paths: ElementPath[]) => paths,
  (jsxMetadata: ElementInstanceMetadataMap, paths: ElementPath[]): ConditionOverride => {
    const elements = mapDropNulls((path) => {
      const elementMetadata = MetadataUtils.findElementByElementPath(jsxMetadata, path)
      if (
        elementMetadata == null ||
        isLeft(elementMetadata.element) ||
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

const conditionExpressionSelector = createCachedSelector(
  (store: MetadataSubstate) => store.editor.jsxMetadata,
  (_store: MetadataSubstate, paths: ElementPath[]) => paths,
  (jsxMetadata: ElementInstanceMetadataMap, paths: ElementPath[]): ConditionExpression => {
    const elements = mapDropNulls((path) => {
      const elementMetadata = MetadataUtils.findElementByElementPath(jsxMetadata, path)
      if (
        elementMetadata == null ||
        isLeft(elementMetadata.element) ||
        !isJSXConditionalExpression(elementMetadata.element.value)
      ) {
        return null
      }

      return {
        element: elementMetadata.element.value,
        conditionValue: elementMetadata.conditionalValue,
      }
    }, paths)

    if (elements.length === 0) {
      return 'not-conditional'
    }

    if (elements.length > 1) {
      return 'multiselect'
    }

    const element = elements[0]

    return jsxAttributeToString(element.element.condition)
  },
)((_, paths) => paths.map(EP.toString).join(','))

export const ConditionalSection = React.memo(({ paths }: { paths: ElementPath[] }) => {
  const dispatch = useDispatch()
  const colorTheme = useColorTheme()

  const conditionOverride = useEditorState(
    Substores.metadata,
    (store) => conditionOverrideSelector(store, paths),
    'ConditionalSection condition override',
  )

  const conditionExpression = useEditorState(
    Substores.metadata,
    (store) => conditionExpressionSelector(store, paths),
    'ConditionalSection condition expression',
  )

  const setConditionOverride = React.useCallback(
    (value: boolean | null) => () => {
      const actions: EditorAction[] = paths.map((path) =>
        setConditionalOverriddenCondition(path, value),
      )

      dispatch(actions)
    },
    [dispatch, paths],
  )

  const toggleConditionOverride = React.useCallback(
    (whichButton: boolean) => () => {
      const newCond = whichButton === conditionOverride ? null : whichButton
      setConditionOverride(newCond)()
    },
    [conditionOverride, setConditionOverride],
  )

  if (conditionOverride === 'not-conditional' || conditionExpression === 'not-conditional') {
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
        {conditionOverride != 'not-overridden' ? (
          <SquareButton
            highlight
            onClick={setConditionOverride(null)}
            data-testid={ConditionalsControlSectionCloseTestId}
          >
            <FunctionIcons.Delete />
          </SquareButton>
        ) : (
          <SquareButton
            highlight
            onClick={setConditionOverride(true)}
            data-testid={ConditionalsControlSectionOpenTestId}
          >
            <Icons.Plus style={{ opacity: 'var(--buttonContentOpacity)' }} />
          </SquareButton>
        )}
      </InspectorSubsectionHeader>
      {unless(
        conditionExpression === 'multiselect',
        <UIGridRow
          padded={true}
          variant='<---1fr--->|------172px-------|'
          style={{
            color: conditionOverride != null ? colorTheme.brandNeonPink.value : 'inherit',
          }}
        >
          Condition
          <FlexRow style={{ flexGrow: 1, gap: 4 }}>
            <span style={{ flex: 1, textAlign: 'center' }}>{conditionExpression}</span>
          </FlexRow>
        </UIGridRow>,
      )}
      <UIGridRow
        padded={true}
        variant='<---1fr--->|------172px-------|'
        style={{ color: conditionOverride != null ? colorTheme.brandNeonPink.value : 'inherit' }}
      >
        Override
        <FlexRow style={{ flexGrow: 1, gap: 4 }}>
          <Button
            style={{ flex: 1 }}
            spotlight={conditionOverride === true}
            highlight
            onClick={toggleConditionOverride(true)}
            data-testid={ConditionalsControlToggleTrueTestId}
          >
            True
          </Button>
          <Button
            style={{ flex: 1 }}
            spotlight={conditionOverride === false}
            highlight
            onClick={toggleConditionOverride(false)}
            data-testid={ConditionalsControlToggleFalseTestId}
          >
            False
          </Button>
        </FlexRow>
      </UIGridRow>
    </React.Fragment>
  )
})

function jsxAttributeToString(attribute: JSXAttribute): string {
  switch (attribute.type) {
    case 'ATTRIBUTE_VALUE':
      if (typeof attribute.value === 'string') {
        return attribute.value
      } else {
        return attribute.value.toString()
      }
    case 'ATTRIBUTE_OTHER_JAVASCRIPT':
      return attribute.javascript
    default:
      return 'Not supported yet'
  }
}
