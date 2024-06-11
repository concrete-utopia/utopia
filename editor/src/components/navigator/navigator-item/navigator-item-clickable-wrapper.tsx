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
import type {
  ElementPath,
  HighlightBoundsWithFile,
  ProjectContentTreeRoot,
} from 'utopia-shared/src/types'
import { assertNever } from '../../../core/shared/utils'
import type { ElementInstanceMetadataMap } from '../../../core/shared/element-template'

export const NavigatorItemClickableWrapper = React.memo(
  (props: { row: NavigatorRow; children: React.ReactNode }) => {
    const dispatch = useDispatch()

    const rows = useRefEditorState((store) => store.derived.navigatorRows)
    const selectedViews = useRefEditorState((store) => store.editor.selectedViews)
    const projectContents = useRefEditorState((store) => store.editor.projectContents)
    const jsxMetadata = useRefEditorState((store) => store.editor.jsxMetadata)

    const conditionalOverrideUpdate = useConditionalOverrideUpdate(props.row)

    const targetPath = React.useMemo(() => {
      return getRowPath(props.row)
    }, [props.row])

    const highlightBounds = useHighlightBounds(targetPath)

    const selected = React.useMemo(() => {
      return selectedViews.current.some((view) => EP.pathsEqual(targetPath, view))
    }, [selectedViews, targetPath])

    const onClick = React.useCallback(
      (e: React.MouseEvent) => {
        function getActions() {
          if (e.metaKey && !e.shiftKey) {
            return actionsForAddToSelection(targetPath)
          } else if (e.shiftKey) {
            return actionsForRangeSelection(
              targetPath,
              rows.current,
              selectedViews.current,
              projectContents.current,
              jsxMetadata.current,
            )
          } else {
            return actionsForSingleSelection(
              targetPath,
              selected,
              props.row,
              highlightBounds,
              conditionalOverrideUpdate,
            )
          }
        }

        const actions = getActions()
        if (actions.length > 0) {
          e.stopPropagation()
          e.preventDefault()

          dispatch(actions)
        }
      },
      [
        props.row,
        dispatch,
        rows,
        selectedViews,
        projectContents,
        conditionalOverrideUpdate,
        highlightBounds,
        jsxMetadata,
        selected,
        targetPath,
      ],
    )

    return (
      <div style={{ display: 'flex', alignItems: 'center', flex: 1 }} onClick={onClick}>
        {props.children}
      </div>
    )
  },
)
NavigatorItemClickableWrapper.displayName = 'NavigatorItemClickable'

function actionsForAddToSelection(targetPath: ElementPath): EditorAction[] {
  return [EditorActions.selectComponents([targetPath], true)]
}

function actionsForRangeSelection(
  targetPath: ElementPath,
  rows: NavigatorRow[],
  selectedViews: ElementPath[],
  projectContents: ProjectContentTreeRoot,
  jsxMetadata: ElementInstanceMetadataMap,
) {
  // boundaries of the current selection
  let selectionTop: number = Infinity
  let selectionBottom: number = -Infinity

  // index of the row being clicked
  let targetIndex: number = Infinity

  // populate the indexes by matching rows, selected views, and the target path
  for (let i = 0; i < rows.length; i++) {
    const rowTarget = getRowPath(rows[i])
    if (
      selectedViews.some((path) => {
        if (EP.pathsEqual(rowTarget, path)) {
          return true
        }
        const element = getElementFromProjectContents(path, projectContents)
        if (MetadataUtils.isElementDataReference(element)) {
          return EP.isParentOf(rowTarget, path)
        }
        return false
      })
    ) {
      selectionTop = Math.min(selectionTop, i)
      selectionBottom = Math.max(selectionBottom, i)
    }
    if (EP.pathsEqual(rowTarget, targetPath)) {
      targetIndex = Math.min(targetIndex, i)
    }
  }

  // derive the slice indexes
  const from = Math.min(selectionTop, targetIndex)
  const to = Math.max(selectionBottom, targetIndex)

  const selection = rows
    .slice(from, to + 1)
    // filter out unselectable rows
    .filter((row) => {
      if (isRegulaNavigatorRow(row)) {
        return row.entry.type !== 'SLOT'
      }
      return true
    })
    .map(getRowPath)
    // filter out conditional branches rows
    .filter((path) => {
      const parent = MetadataUtils.findElementByElementPath(jsxMetadata, EP.parentPath(path))
      if (
        parent != null &&
        isRight(parent.element) &&
        parent.element.value.type === 'JSX_CONDITIONAL_EXPRESSION'
      ) {
        return false
      }
      return true
    })

  return [EditorActions.selectComponents(selection, false)]
}

function actionsForSingleSelection(
  targetPath: ElementPath,
  selected: boolean,
  row: NavigatorRow,
  highlightBounds: HighlightBoundsWithFile | null,
  conditionalOverrideUpdate: ConditionalOverrideUpdate,
): EditorAction[] {
  let actions: EditorAction[] = [EditorActions.selectComponents([targetPath], false)]
  if (isRegulaNavigatorRow(row)) {
    const conditionalOverrideActions = isConditionalClauseNavigatorEntry(row.entry)
      ? getConditionalOverrideActions(targetPath, conditionalOverrideUpdate)
      : getConditionalOverrideActions(EP.parentPath(targetPath), conditionalOverrideUpdate)
    actions.push(...conditionalOverrideActions)
  }
  // when we click on an already selected item we should force vscode to navigate there
  if (selected && highlightBounds != null) {
    sendMessage(
      toVSCodeExtensionMessage(
        selectedElementChangedMessageFromHighlightBounds(highlightBounds, 'force-navigation'),
      ),
    )
  }
  return actions
}

function getRowPath(row: NavigatorRow): ElementPath {
  return isRegulaNavigatorRow(row) ? row.entry.elementPath : row.entries[0].elementPath
}

type ConditionalOverrideUpdate = ConditionalCase | 'clear-override' | 'no-update'

function getConditionalOverrideActions(
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
        if (isActiveBranchOfConditional(navigatorEntry.clause, elementMetadata)) {
          if (isOverriddenConditional(elementMetadata)) {
            return 'clear-override'
          } else {
            return navigatorEntry.clause
          }
        } else {
          return navigatorEntry.clause
        }
      } else {
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
    },
    'useConditionalOverrideUpdate conditionalOverrideUpdate',
  )
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
