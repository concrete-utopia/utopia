/** @jsxRuntime classic */
/** @jsx jsx */ import { jsx } from '@emotion/react'
import createCachedSelector from 're-reselect'
import React from 'react'
import { MetadataUtils } from '../../../../core/model/element-metadata-utils'
import { mapDropNulls } from '../../../../core/shared/array-utils'
import {
  findUtopiaCommentFlag,
  isUtopiaCommentFlagConditional,
} from '../../../../core/shared/comment-flags'
import { isLeft } from '../../../../core/shared/either'
import * as EP from '../../../../core/shared/element-path'
import {
  ElementInstanceMetadataMap,
  isJSXConditionalExpression,
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
  StringInput,
  useColorTheme,
  UtopiaStyles,
} from '../../../../uuiui'
import { EditorAction } from '../../../editor/action-types'
import {
  setConditionalOverriddenCondition,
  switchConditionalBranches,
  updateConditionalExpression,
} from '../../../editor/actions/action-creators'
import { useDispatch } from '../../../editor/store/dispatch-context'
import { Substores, useEditorState } from '../../../editor/store/store-hook'
import { MetadataSubstate } from '../../../editor/store/store-hook-substore-types'
import { ControlStatus, getControlStyles } from '../../common/control-status'
import { usePropControlledStateV2 } from '../../common/inspector-utils'
import { ConditionalOverrideControl } from '../../controls/conditional-override-control'
import { UIGridRow } from '../../widgets/ui-grid-row'

export const ConditionalsControlSectionOpenTestId = 'conditionals-control-section-open'
export const ConditionalsControlSectionCloseTestId = 'conditionals-control-section-close'
export const ConditionalsControlSectionExpressionTestId = 'conditionals-control-expression'
export const ConditionalsControlSwitchBranches = 'conditionals-control-switch=branches'

export type ConditionOverride = boolean | 'mixed' | 'not-overridden' | 'not-conditional'
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
      if (isUtopiaCommentFlagConditional(flag)) {
        conditions.add(flag.value)
      }
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

const conditionOverrideToControlStatus = (conditionOverride: ConditionOverride): ControlStatus => {
  // TODO: we don't have multiselect support yet, that is why mixed is here
  if (conditionOverride === 'not-conditional' || conditionOverride === 'mixed') {
    return 'off'
  }
  if (conditionOverride === 'not-overridden') {
    return 'simple'
  }
  return 'overridden'
}

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
        conditionValue: elementMetadata.conditionValue,
      }
    }, paths)

    if (elements.length === 0) {
      return 'not-conditional'
    }

    if (elements.length > 1) {
      return 'multiselect'
    }

    const element = elements[0]

    return element.element.originalConditionString
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

  const originalConditionExpression = useEditorState(
    Substores.metadata,
    (store) => conditionExpressionSelector(store, paths),
    'ConditionalSection condition expression',
  )

  const [conditionExpression, setConditionExpression] = usePropControlledStateV2(
    originalConditionExpression,
  )

  const setConditionOverride = React.useCallback(
    (value: boolean | null) => {
      const actions: EditorAction[] = paths.map((path) =>
        setConditionalOverriddenCondition(path, value),
      )

      dispatch(actions)
    },
    [dispatch, paths],
  )

  const replaceBranches = React.useCallback(() => {
    const actions: EditorAction[] = paths.map((path) => switchConditionalBranches(path))

    dispatch(actions)
  }, [dispatch, paths])

  const onUpdateExpression = React.useCallback(() => {
    if (paths.length !== 1) {
      return
    }
    let expression = conditionExpression
    if (conditionExpression.trim().length === 0) {
      setConditionExpression(originalConditionExpression)
      return
    }
    dispatch([updateConditionalExpression(paths[0], expression)], 'everyone')
  }, [paths, conditionExpression, setConditionExpression, originalConditionExpression, dispatch])

  function onExpressionChange(e: React.ChangeEvent<HTMLInputElement>) {
    setConditionExpression(e.target.value)
  }

  function onExpressionKeyUp(e: React.KeyboardEvent) {
    if (e.key === 'Enter') {
      e.nativeEvent.stopImmediatePropagation()
      e.preventDefault()
      onUpdateExpression()
    }
  }

  if (
    conditionOverride === 'not-conditional' ||
    originalConditionExpression === 'not-conditional'
  ) {
    return null
  }

  const controlStatus = conditionOverrideToControlStatus(conditionOverride)
  const controlStyles = getControlStyles(controlStatus)

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
      </InspectorSubsectionHeader>
      {unless(
        originalConditionExpression === 'multiselect',
        <UIGridRow padded={true} variant='<-auto-><----------1fr--------->'>
          Condition
          <StringInput
            testId={ConditionalsControlSectionExpressionTestId}
            value={conditionExpression}
            onChange={onExpressionChange}
            onKeyUp={onExpressionKeyUp}
            onBlur={onUpdateExpression}
            css={{
              ...UtopiaStyles.fontStyles.monospaced,
              textAlign: 'center',
              fontWeight: 600,
            }}
          />
        </UIGridRow>,
      )}
      {unless(
        originalConditionExpression === 'multiselect',
        <UIGridRow padded={true} variant='<-------------1fr------------->'>
          <Button
            style={{ flex: 1 }}
            highlight
            spotlight
            onClick={replaceBranches}
            data-testid={ConditionalsControlSwitchBranches}
          >
            Switch branches
          </Button>
        </UIGridRow>,
      )}
      <ConditionalOverrideControl
        controlStatus={controlStatus}
        controlStyles={controlStyles}
        setConditionOverride={setConditionOverride}
        conditionOverride={conditionOverride}
      />
    </React.Fragment>
  )
})
