/** @jsxRuntime classic */
/** @jsx jsx */ import { jsx } from '@emotion/react'
import createCachedSelector from 're-reselect'
import React from 'react'
import type { ConditionalCase } from '../../../../core/model/conditionals'
import { getConditionalClausePath } from '../../../../core/model/conditionals'
import { MetadataUtils } from '../../../../core/model/element-metadata-utils'
import { mapDropNulls } from '../../../../core/shared/array-utils'
import {
  findUtopiaCommentFlag,
  isUtopiaCommentFlagConditional,
} from '../../../../core/shared/comment-flags'
import { isLeft } from '../../../../core/shared/either'
import * as EP from '../../../../core/shared/element-path'
import type {
  ConditionValue,
  ElementInstanceMetadata,
  ElementInstanceMetadataMap,
  JSXConditionalExpression,
  JSXElementChild,
} from '../../../../core/shared/element-template'
import { isJSXConditionalExpression } from '../../../../core/shared/element-template'
import { optionalMap } from '../../../../core/shared/optional-utils'
import type { ElementPath } from '../../../../core/shared/project-file-types'
import { unless } from '../../../../utils/react-conditionals'
import {
  FlexColumn,
  FlexRow,
  Icons,
  InspectorSectionIcons,
  SquareButton,
  StringInput,
  Tooltip,
  useColorTheme,
  UtopiaStyles,
  UtopiaTheme,
} from '../../../../uuiui'
import { isEntryAConditionalSlot } from '../../../canvas/canvas-utils'
import type { EditorAction } from '../../../editor/action-types'
import {
  setConditionalOverriddenCondition,
  switchConditionalBranches,
  updateConditionalExpression,
} from '../../../editor/actions/action-creators'
import { useDispatch } from '../../../editor/store/dispatch-context'
import type { NavigatorEntry } from '../../../editor/store/editor-state'
import { Substores, useEditorState } from '../../../editor/store/store-hook'
import type {
  MetadataSubstate,
  ProjectContentAndMetadataSubstate,
} from '../../../editor/store/store-hook-substore-types'
import { LayoutIcon } from '../../../navigator/navigator-item/layout-icon'
import {
  getNavigatorEntryLabel,
  labelSelector,
} from '../../../navigator/navigator-item/navigator-item-wrapper'
import { getNavigatorTargets } from '../../../navigator/navigator-utils'
import type { ControlStatus } from '../../common/control-status'
import { getControlStyles } from '../../common/control-styles'
import { usePropControlledStateV2 } from '../../common/inspector-utils'
import { ConditionalOverrideControl } from '../../controls/conditional-override-control'
import { UIGridRow } from '../../widgets/ui-grid-row'

export const ConditionalsControlSectionOpenTestId = 'conditionals-control-section-open'
export const ConditionalsControlSectionCloseTestId = 'conditionals-control-section-close'
export const ConditionalsControlSectionExpressionTestId = 'conditionals-control-expression'
export const ConditionalsControlSwitchBranchesTestId = 'conditionals-control-switch-branches'
export const ConditionalsControlBranchTrueTestId = 'conditionals-control-branch-true'
export const ConditionalsControlBranchFalseTestId = 'conditionals-control-branch-false'

export type ConditionOverride = boolean | 'mixed' | 'not-overridden' | 'not-a-conditional'
type ConditionExpression = string | 'multiselect' | 'not-a-conditional'

type BranchNavigatorEntries = {
  true: NavigatorEntry | null
  false: NavigatorEntry | null
}

const branchNavigatorEntriesSelector = createCachedSelector(
  (store: ProjectContentAndMetadataSubstate) => store.editor.jsxMetadata,
  (store: ProjectContentAndMetadataSubstate) => store.editor.elementPathTree,
  (store: ProjectContentAndMetadataSubstate) => store.editor.projectContents,
  (_store: ProjectContentAndMetadataSubstate, paths: ElementPath[]) => paths,
  (jsxMetadata, elementPathTree, projectContents, paths): BranchNavigatorEntries | null => {
    if (paths.length !== 1) {
      return null
    }
    const elementMetadata = MetadataUtils.findElementByElementPath(jsxMetadata, paths[0])
    if (
      elementMetadata == null ||
      isLeft(elementMetadata.element) ||
      !isJSXConditionalExpression(elementMetadata.element.value)
    ) {
      return null
    }

    const conditional = elementMetadata.element.value

    const navigatorEntries = getNavigatorTargets(
      jsxMetadata,
      elementPathTree,
      [],
      [],
      {},
      projectContents,
    ).navigatorTargets

    function getNavigatorEntry(clause: JSXElementChild): NavigatorEntry | null {
      return (
        navigatorEntries.find((entry) =>
          EP.pathsEqual(entry.elementPath, getConditionalClausePath(paths[0], clause)),
        ) ?? null
      )
    }

    return {
      true: getNavigatorEntry(conditional.whenTrue),
      false: getNavigatorEntry(conditional.whenFalse),
    }
  },
)((_, paths) => paths.map(EP.toString).join(','))

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
      return 'not-a-conditional'
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
  if (conditionOverride === 'not-a-conditional' || conditionOverride === 'mixed') {
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
    const element = getConditionalMetadata(jsxMetadata, paths)

    if (element === 'not-a-conditional' || element === 'multiselect') {
      return element
    }

    return element.element.originalConditionString
  },
)((_, paths) => paths.map(EP.toString).join(','))

const conditionValueSelector = createCachedSelector(
  (store: MetadataSubstate) => store.editor.jsxMetadata,
  (_store: MetadataSubstate, paths: ElementPath[]) => paths,
  (
    jsxMetadata: ElementInstanceMetadataMap,
    paths: ElementPath[],
  ): ConditionValue | 'multiselect' => {
    const element = getConditionalMetadata(jsxMetadata, paths)

    if (element === 'not-a-conditional' || element === 'multiselect') {
      return element
    }

    return element.metadata.conditionValue
  },
)((_, paths) => paths.map(EP.toString).join(','))

function getConditionalMetadata(
  jsxMetadata: ElementInstanceMetadataMap,
  paths: ElementPath[],
):
  | { metadata: ElementInstanceMetadata; element: JSXConditionalExpression }
  | 'multiselect'
  | 'not-a-conditional' {
  const elementMetadatas = mapDropNulls((path) => {
    const elementMetadata = MetadataUtils.findElementByElementPath(jsxMetadata, path)
    if (
      elementMetadata == null ||
      isLeft(elementMetadata.element) ||
      !isJSXConditionalExpression(elementMetadata.element.value)
    ) {
      return null
    }

    return {
      metadata: elementMetadata,
      element: elementMetadata.element.value,
    }
  }, paths)

  if (elementMetadatas.length === 0) {
    return 'not-a-conditional'
  }

  if (elementMetadatas.length > 1) {
    return 'multiselect'
  }

  return elementMetadatas[0]
}

export const ConditionalSectionTestId = 'conditional-section-test-id'

export const ConditionalSection = React.memo(({ paths }: { paths: ElementPath[] }) => {
  const dispatch = useDispatch()
  const colorTheme = useColorTheme()

  const conditionOverride = useEditorState(
    Substores.metadata,
    (store) => conditionOverrideSelector(store, paths),
    'ConditionalSection condition override',
  )

  const conditionValue = useEditorState(
    Substores.metadata,
    (store) => conditionValueSelector(store, paths),
    'ConditionalSection condition value',
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

  const branchNavigatorEntries = useEditorState(
    Substores.projectContentsAndMetadata,
    (store) => branchNavigatorEntriesSelector(store, paths),
    'ConditionalSection branches',
  )

  const branchLabels = useEditorState(
    Substores.projectContentsAndMetadata,
    (store) => {
      function getLabel(entry: NavigatorEntry | null) {
        if (entry == null) {
          return null
        }
        return labelSelector(store, entry)
      }
      return {
        true: getLabel(branchNavigatorEntries?.true ?? null),
        false: getLabel(branchNavigatorEntries?.false ?? null),
      }
    },
    'NavigatorItemWrapper labelSelector',
  )

  const onExpressionChange = React.useCallback(
    (e: React.ChangeEvent<HTMLInputElement>) => {
      setConditionExpression(e.target.value)
      setConditionOverride(null)
    },
    [setConditionExpression, setConditionOverride],
  )

  const onExpressionKeyUp = React.useCallback(
    (e: React.KeyboardEvent) => {
      if (e.key === 'Enter') {
        e.nativeEvent.stopImmediatePropagation()
        e.preventDefault()
        onUpdateExpression()
      }
    },
    [onUpdateExpression],
  )

  if (
    conditionOverride === 'not-a-conditional' ||
    originalConditionExpression === 'not-a-conditional' ||
    conditionValue === 'not-a-conditional'
  ) {
    return null
  }

  const controlStatus = conditionOverrideToControlStatus(conditionOverride)
  const controlStyles = getControlStyles(controlStatus)

  return (
    <div style={{ paddingBottom: 8 }} data-testid={ConditionalSectionTestId}>
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
          <InspectorSectionIcons.Conditionals style={{ width: 16, height: 16 }} color='main' />
          <span>Conditional</span>
        </FlexRow>
      </FlexRow>
      {unless(
        originalConditionExpression === 'multiselect',
        <React.Fragment>
          <UIGridRow padded={true} variant='<--------auto-------->|167px|'>
            Condition
            <StringInput
              testId={ConditionalsControlSectionExpressionTestId}
              value={conditionExpression}
              onChange={onExpressionChange}
              onKeyUp={onExpressionKeyUp}
              onBlur={onUpdateExpression}
              css={{
                textAlign: 'left',
                fontWeight: 600,
                height: 26,
                ...UtopiaStyles.fontStyles.monospaced,
              }}
            />
          </UIGridRow>
          {conditionValue !== 'multiselect' ? (
            <ConditionalOverrideControl
              controlStatus={controlStatus}
              controlStyles={controlStyles}
              setConditionOverride={setConditionOverride}
              conditionValue={conditionValue}
            />
          ) : null}
        </React.Fragment>,
      )}
      <FlexRow style={{ paddingRight: '8px' }}>
        <FlexColumn style={{ flexGrow: 2 }}>
          <BranchRow
            label={branchLabels.true}
            navigatorEntry={branchNavigatorEntries?.true ?? null}
            conditionalCase='true-case'
          />
          <BranchRow
            label={branchLabels.false}
            navigatorEntry={branchNavigatorEntries?.false ?? null}
            conditionalCase='false-case'
          />
        </FlexColumn>
        <Tooltip title={'Switch branches'}>
          <SquareButton
            onClick={replaceBranches}
            data-testid={ConditionalsControlSwitchBranchesTestId}
          >
            <Icons.Flip category={'element'} width={18} height={18} />
          </SquareButton>
        </Tooltip>
      </FlexRow>
    </div>
  )
})

const BranchRow = ({
  label,
  navigatorEntry,
  conditionalCase,
}: {
  label: string | null
  navigatorEntry: NavigatorEntry | null
  conditionalCase: ConditionalCase
}) => {
  const colorTheme = useColorTheme()

  const isSlot = useEditorState(
    Substores.metadata,
    (store) =>
      optionalMap((e) => isEntryAConditionalSlot(store.editor.jsxMetadata, e), navigatorEntry) ??
      false,
    'NavigatorItem parentElement',
  )

  if (label == null || navigatorEntry == null) {
    return null
  }

  return (
    <UIGridRow
      padded={false}
      variant='<--------1fr-------->|145px|'
      style={{ padding: '0px 0px 0px 8px' }}
    >
      <div>{conditionalCase === 'true-case' ? 'True' : 'False'}</div>
      <div
        style={{
          borderRadius: 2,
          padding: '4px 0px 4px 6px',
          background: colorTheme.bg2.value,
          fontWeight: 600,
          display: 'flex',
          justifyContent: 'flex-start',
          alignItems: 'center',
          gap: 6,
          overflowX: 'scroll',
          whiteSpace: 'nowrap',
        }}
      >
        {isSlot ? (
          <div
            style={{
              padding: '0px 6px',
              textTransform: 'lowercase',
              color: colorTheme.unavailableGrey.value,
              fontWeight: 500,
            }}
          >
            Empty
          </div>
        ) : (
          <React.Fragment>
            <LayoutIcon
              key={`layout-type-${label}`}
              navigatorEntry={navigatorEntry}
              color='main'
              warningText={null}
            />
            <span
              data-testid={
                conditionalCase === 'true-case'
                  ? ConditionalsControlBranchTrueTestId
                  : ConditionalsControlBranchFalseTestId
              }
            >
              {getNavigatorEntryLabel(navigatorEntry, label)}
            </span>
          </React.Fragment>
        )}
      </div>
    </UIGridRow>
  )
}
