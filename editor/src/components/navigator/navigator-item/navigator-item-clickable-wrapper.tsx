import React from 'react'
import type { ConditionalCase } from '../../../core/model/conditionals'
import {
  getConditionalCaseCorrespondingToBranchPath,
  isActiveBranchOfConditional,
  isDefaultBranchOfConditional,
  isOverriddenConditional,
} from '../../../core/model/conditionals'
import { MetadataUtils } from '../../../core/model/element-metadata-utils'
import * as EP from '../../../core/shared/element-path'
import type { EditorAction } from '../../editor/action-types'
import * as EditorActions from '../../editor/actions/action-creators'
import type { NavigatorEntry } from '../../editor/store/editor-state'
import {
  getElementFromProjectContents,
  isConditionalClauseNavigatorEntry,
} from '../../editor/store/editor-state'
import { Substores, useEditorState, useRefEditorState } from '../../editor/store/store-hook'
import { getHighlightBoundsForProject } from '../../../core/model/project-file-utils'
import {
  selectedElementChangedMessageFromHighlightBounds,
  sendMessage,
} from '../../../core/vscode/vscode-bridge'
import { toVSCodeExtensionMessage } from 'utopia-vscode-common'
import { isRegulaNavigatorRow, type NavigatorRow } from '../navigator-row'
import { useDispatch } from '../../editor/store/dispatch-context'
import { isRight } from '../../../core/shared/either'
import type { ElementPath, ProjectContentTreeRoot } from 'utopia-shared/src/types'
import { assertNever } from '../../../core/shared/utils'
import type {
  ElementInstanceMetadata,
  ElementInstanceMetadataMap,
} from '../../../core/shared/element-template'

export const NavigatorRowClickableWrapper = React.memo(
  (props: { row: NavigatorRow; children: React.ReactNode }) => {
    const dispatch = useDispatch()

    const selectedViews = useRefEditorState((store) => store.editor.selectedViews)

    const targetPath = React.useMemo(() => {
      return getRowPath(props.row)
    }, [props.row])

    const selected = React.useMemo(() => {
      return selectedViews.current.some((view) => EP.pathsEqual(targetPath, view))
    }, [selectedViews, targetPath])

    const getActions = useGetNavigatorClickActions(targetPath, selected, props.row)

    const onClick = React.useCallback(
      (e: React.MouseEvent) => {
        e.stopPropagation()
        e.preventDefault()

        const actions = getActions(e)
        dispatch(actions)
      },
      [dispatch, getActions],
    )

    return (
      <div style={{ display: 'flex', alignItems: 'center', flex: 1 }} onClick={onClick}>
        {props.children}
      </div>
    )
  },
)
NavigatorRowClickableWrapper.displayName = 'NavigatorRowClickableWrapper'

export function useGetNavigatorClickActions(
  targetPath: ElementPath,
  selected: boolean,
  row: NavigatorRow,
) {
  const navigatorTargets = useRefEditorState((store) => store.derived.navigatorTargets)
  const selectedViews = useRefEditorState((store) => store.editor.selectedViews)
  const collapsedViews = useRefEditorState((store) => store.editor.navigator.collapsedViews)
  const projectContents = useRefEditorState((store) => store.editor.projectContents)
  const jsxMetadata = useRefEditorState((store) => store.editor.jsxMetadata)

  const highlightBounds = useHighlightBounds(targetPath)
  const conditionalOverrideUpdate = useConditionalOverrideUpdate(row)

  return React.useCallback(
    (e: React.MouseEvent) => {
      if (e.metaKey && !e.shiftKey) {
        return actionsForAddToSelection(targetPath)
      } else if (e.shiftKey) {
        return actionsForRangeSelection(
          targetPath,
          navigatorTargets.current,
          selectedViews.current,
          collapsedViews.current,
          projectContents.current,
          jsxMetadata.current,
        )
      } else {
        // when we click on an already selected item we should force vscode to navigate there
        if (selected && highlightBounds != null) {
          sendMessage(
            toVSCodeExtensionMessage(
              selectedElementChangedMessageFromHighlightBounds(highlightBounds, 'force-navigation'),
            ),
          )
        }
        return actionsForSingleSelection(targetPath, row, conditionalOverrideUpdate)
      }
    },
    [
      row,
      selectedViews,
      projectContents,
      conditionalOverrideUpdate,
      highlightBounds,
      jsxMetadata,
      selected,
      targetPath,
      collapsedViews,
      navigatorTargets,
    ],
  )
}

function actionsForAddToSelection(targetPath: ElementPath): EditorAction[] {
  return [EditorActions.selectComponents([targetPath], true)]
}

export function actionsForRangeSelection(
  targetPath: ElementPath,
  navigatorTargets: NavigatorEntry[],
  selectedViews: ElementPath[],
  collapsedViews: ElementPath[],
  projectContents: ProjectContentTreeRoot,
  jsxMetadata: ElementInstanceMetadataMap,
): EditorAction[] {
  const selectableTargets = navigatorTargets
    .filter((target) => target.type !== 'SYNTHETIC')
    .map((target) => target.elementPath)

  // boundaries of the current selection
  let selectionTop: number = Infinity
  let selectionBottom: number = -Infinity

  // index of the row being clicked
  let targetIndex: number = Infinity

  // populate the indexes by matching rows, selected views, and the target path
  for (let i = 0; i < selectableTargets.length; i++) {
    const target = selectableTargets[i]
    if (
      selectedViews.some((path) => {
        if (EP.pathsEqual(target, path)) {
          return true
        }

        const element = getElementFromProjectContents(path, projectContents)
        if (MetadataUtils.isElementDataReference(element)) {
          return EP.isParentOf(target, path)
        }

        return false
      })
    ) {
      selectionTop = Math.min(selectionTop, i)
      selectionBottom = Math.max(selectionBottom, i)
    }
    if (EP.pathsEqual(target, targetPath)) {
      targetIndex = Math.min(targetIndex, i)
    }
  }

  // derive the slice indexes
  const sliceFrom = Math.min(selectionTop, targetIndex)
  const sliceTo = Math.max(selectionBottom, targetIndex)

  const selectionPaths = selectableTargets
    // get the targets inside the new selection bounds
    .slice(sliceFrom, sliceTo + 1)
    // get their paths
    .flatMap((target) => {
      let paths = [target]
      // if a collapsed view is included in the range, get its children paths
      if (collapsedViews.some((view) => EP.pathsEqual(view, target))) {
        const children = MetadataUtils.getChildrenUnordered(jsxMetadata, target)
        paths.push(...children.map((c) => c.elementPath))
      }
      return paths
    })

  const selection = selectionPaths
    // filter out conditional branches rows
    .filter((path) => {
      const parent = MetadataUtils.findElementByElementPath(jsxMetadata, EP.parentPath(path))
      const isConditionalExpression =
        parent != null &&
        isRight(parent.element) &&
        parent.element.value.type === 'JSX_CONDITIONAL_EXPRESSION'

      return !isConditionalExpression
    })

  return [EditorActions.selectComponents(selection, false)]
}

function actionsForSingleSelection(
  targetPath: ElementPath,
  row: NavigatorRow,
  conditionalOverrideUpdate: ConditionalOverrideUpdate,
): EditorAction[] {
  let actions: EditorAction[] = []

  if (isRegulaNavigatorRow(row)) {
    const conditionalOverrideActions = isConditionalClauseNavigatorEntry(row.entry)
      ? getConditionalOverrideActions(targetPath, conditionalOverrideUpdate)
      : getConditionalOverrideActions(EP.parentPath(targetPath), conditionalOverrideUpdate)
    actions.push(...conditionalOverrideActions)
  }

  const isNotSelectable = isRegulaNavigatorRow(row) && row.entry.type === 'CONDITIONAL_CLAUSE'
  if (!isNotSelectable) {
    actions.push(EditorActions.selectComponents([targetPath], false))
  }

  return actions
}

function getRowPath(row: NavigatorRow): ElementPath {
  return isRegulaNavigatorRow(row) ? row.entry.elementPath : row.entries[0].elementPath
}

type ConditionalOverrideUpdate = ConditionalCase | 'clear-override' | 'no-update'

export function getConditionalOverrideActions(
  targetPath: ElementPath,
  conditionalOverrideUpdate: ConditionalOverrideUpdate,
): Array<EditorAction> {
  switch (conditionalOverrideUpdate) {
    case 'no-update':
      return []
    case 'clear-override':
      return [EditorActions.setConditionalOverriddenCondition(targetPath, null)]
    case 'true-case':
      return [EditorActions.setConditionalOverriddenCondition(targetPath, true)]
    case 'false-case':
      return [EditorActions.setConditionalOverriddenCondition(targetPath, false)]
    default:
      assertNever(conditionalOverrideUpdate)
  }
}

function useConditionalOverrideUpdate(row: NavigatorRow) {
  return useEditorState(
    Substores.metadata,
    (store): ConditionalOverrideUpdate => {
      if (!isRegulaNavigatorRow(row)) {
        return 'no-update'
      }
      const navigatorEntry = row.entry
      const path = navigatorEntry.elementPath
      const metadata = store.editor.jsxMetadata
      const elementMetadata = MetadataUtils.findElementByElementPath(
        store.editor.jsxMetadata,
        navigatorEntry.elementPath,
      )
      if (isConditionalClauseNavigatorEntry(navigatorEntry)) {
        return conditionalOverrideUpdateForClause(navigatorEntry.clause, elementMetadata)
      } else {
        return conditionalOverrideUpdateForPath(path, metadata)
      }
    },
    'useConditionalOverrideUpdate conditionalOverrideUpdate',
  )
}

function conditionalOverrideUpdateForClause(
  clause: ConditionalCase,
  elementMetadata: ElementInstanceMetadata | null,
) {
  if (isActiveBranchOfConditional(clause, elementMetadata)) {
    if (isOverriddenConditional(elementMetadata)) {
      return 'clear-override'
    } else {
      return clause
    }
  } else {
    return clause
  }
}

export function conditionalOverrideUpdateForPath(
  path: ElementPath,
  metadata: ElementInstanceMetadataMap,
) {
  const conditionalCase = getConditionalCaseCorrespondingToBranchPath(path, metadata)
  if (conditionalCase != null) {
    const parentPath = EP.parentPath(path)
    const parentMetadata = MetadataUtils.findElementByElementPath(metadata, parentPath)
    if (isActiveBranchOfConditional(conditionalCase, parentMetadata)) {
      return 'no-update'
    } else if (isDefaultBranchOfConditional(conditionalCase, parentMetadata)) {
      return 'clear-override'
    } else {
      return conditionalCase
    }
  }

  return 'no-update'
}

function useHighlightBounds(path: ElementPath) {
  return useEditorState(
    Substores.projectContents,
    (store) => {
      const staticPath = EP.dynamicPathToStaticPath(path)
      if (staticPath != null) {
        const bounds = getHighlightBoundsForProject(store.editor.projectContents)
        if (bounds != null) {
          const highlightedUID = EP.toUid(staticPath)
          return bounds[highlightedUID]
        }
      }

      return null
    },
    'useHighlightBounds highlightBounds',
  )
}
