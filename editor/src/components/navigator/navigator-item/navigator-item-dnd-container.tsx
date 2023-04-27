import React from 'react'
import { DropTargetMonitor, useDrag, useDrop } from 'react-dnd'
import { ElementPath } from '../../../core/shared/project-file-types'
import { EditorDispatch } from '../../editor/action-types'
import * as EditorActions from '../../editor/actions/action-creators'
import * as MetaActions from '../../editor/actions/meta-actions'
import * as EP from '../../../core/shared/element-path'
import {
  placeComponentsAfter,
  placeComponentsBefore,
  reparentComponents,
  showNavigatorDropTargetHint,
} from '../actions'
import { ExpansionArrowWidth } from './expandable-indicator'
import { BasePaddingUnit, NavigatorItem, ParentOutline } from './navigator-item'
import {
  NavigatorHintBottom,
  NavigatorHintCircleDiameter,
  NavigatorHintTop,
} from './navigator-item-components'
import {
  ConditionalClauseNavigatorEntry,
  DropTargetHint,
  DropTargetType,
  EditorState,
  isConditionalClauseNavigatorEntry,
  isRegularNavigatorEntry,
  isSyntheticNavigatorEntry,
  navigatorEntriesEqual,
  NavigatorEntry,
  regularNavigatorEntry,
  varSafeNavigatorEntryToKey,
} from '../../editor/store/editor-state'
import {
  Substores,
  useEditorState,
  useRefEditorState,
} from '../../../components/editor/store/store-hook'
import { isAllowedToReparent } from '../../canvas/canvas-strategies/strategies/reparent-helpers/reparent-helpers'
import { MetadataUtils } from '../../../core/model/element-metadata-utils'
import { getEmptyImage } from 'react-dnd-html5-backend'
import { when } from '../../../utils/react-conditionals'
import { metadataSelector } from '../../inspector/inpector-selectors'
import { navigatorDepth } from '../navigator-utils'
import {
  ElementInstanceMetadataMap,
  JSXConditionalExpression,
  isJSXArbitraryBlock,
  isJSXElementLike,
  isNullJSXAttributeValue,
} from '../../../core/shared/element-template'
import {
  ConditionalCase,
  findMaybeConditionalExpression,
  getConditionalBranch,
  getConditionalCaseCorrespondingToBranchPath,
  isNonEmptyConditionalBranch,
  maybeBranchConditionalCase,
} from '../../../core/model/conditionals'
import { isRight } from '../../../core/shared/either'

export const TopDropTargetLineTestId = (safeComponentId: string): string =>
  `navigator-item-drop-before-${safeComponentId}`

export const BottomDropTargetLineTestId = (safeComponentId: string): string =>
  `navigator-item-drop-after-${safeComponentId}`

export const DragItemTestId = (safeComponentId: string): string =>
  `navigator-item-drag-${safeComponentId}`

const BaseRowHeight = 35
const PreviewIconSize = BaseRowHeight

export interface DragSelection {
  elementPath: ElementPath
  index: number
}

export interface NavigatorItemDragAndDropWrapperProps {
  index: number
  navigatorEntry: NavigatorEntry
  entryDepth: number
  appropriateDropTargetHint: DropTargetHint | null
  editorDispatch: EditorDispatch
  selected: boolean
  highlighted: boolean // TODO are we sure about this?
  collapsed: boolean // TODO are we sure about this?
  getCurrentlySelectedEntries: () => Array<NavigatorEntry>
  getSelectedViewsInRange: (index: number) => Array<ElementPath> // TODO remove me
  canReparentInto: boolean
  noOfChildren: number
  label: string
  isElementVisible: boolean
  renamingTarget: ElementPath | null
  windowStyle: React.CSSProperties
  visibleNavigatorTargets: Array<NavigatorEntry>
}

function notDescendant(
  draggedOnto: NavigatorItemDragAndDropWrapperProps,
  draggedItem: ElementPath,
): boolean {
  return !EP.isDescendantOfOrEqualTo(draggedOnto.navigatorEntry.elementPath, draggedItem)
}

function canDrop(
  editorState: EditorState,
  draggedItem: NavigatorItemDragAndDropWrapperProps,
  draggedOnto: NavigatorItemDragAndDropWrapperProps,
  dropTarget: 'top' | 'bottom' | 'hover',
): boolean {
  const isReparentTarget = draggedItem.appropriateDropTargetHint?.type === 'reparent'
  const targetSupportsChildren = MetadataUtils.targetSupportsChildren(
    editorState.projectContents,
    editorState.jsxMetadata,
    editorState.nodeModules.files,
    editorState.canvas.openFile?.filename,
    draggedOnto.navigatorEntry.elementPath,
  )
  if (isConditionalRoot(draggedOnto.navigatorEntry, editorState.jsxMetadata) && isReparentTarget) {
    // reparent target is the conditional root
    return false
  } else if (
    isConditionalClauseNavigatorEntry(draggedOnto.navigatorEntry) &&
    !canReparentIntoConditionalClause(draggedOnto.navigatorEntry, editorState.jsxMetadata)
  ) {
    // target is a conditional clause entry, non-empty
    return false
  } else if (
    !isConditionalClauseNavigatorEntry(draggedOnto.navigatorEntry) &&
    isNonEmptyConditionalBranch(draggedOnto.navigatorEntry.elementPath, editorState.jsxMetadata) &&
    !targetSupportsChildren
  ) {
    // target is a direct conditional branch, non-empty
    return false
  } else if (
    (dropTarget === 'bottom' || dropTarget === 'top') &&
    isInsideConditional(draggedOnto.navigatorEntry, editorState.jsxMetadata)
  ) {
    // do nothing when trying to drop next to an element in a branch of a conditional
    return false
  } else {
    const childrenSupportedIfRequired =
      !isReparentTarget ||
      isConditionalClauseNavigatorEntry(draggedOnto.navigatorEntry) ||
      (isRegularNavigatorEntry(draggedOnto.navigatorEntry) && targetSupportsChildren)
    const notSelectedItem = draggedItem.getCurrentlySelectedEntries().every((selection) => {
      return notDescendant(draggedOnto, selection.elementPath)
    })
    return childrenSupportedIfRequired && notSelectedItem
  }
}

function onDrop(
  propsOfDraggedItem: NavigatorItemDragAndDropWrapperProps,
  propsOfDropTargetItem: NavigatorItemDragAndDropWrapperProps,
  moveToEntry: NavigatorEntry,
  dropTargetHintType: DropTargetType,
): void {
  const dragSelections = propsOfDraggedItem.getCurrentlySelectedEntries()
  const filteredSelections = dragSelections.filter((selection) =>
    notDescendant(propsOfDropTargetItem, selection.elementPath),
  )
  const draggedElements = filteredSelections.map((selection) => selection.elementPath)
  const clearHintAction = showNavigatorDropTargetHint(null, null, null)

  switch (dropTargetHintType) {
    case 'before':
      propsOfDraggedItem.editorDispatch(
        [placeComponentsBefore(draggedElements, moveToEntry.elementPath), clearHintAction],
        'everyone',
      )
      break
    case 'after':
      propsOfDraggedItem.editorDispatch(
        [placeComponentsAfter(draggedElements, moveToEntry.elementPath), clearHintAction],
        'everyone',
      )
      break
    case 'reparent':
      const target =
        moveToEntry.type === 'SYNTHETIC' ? propsOfDropTargetItem.navigatorEntry : moveToEntry
      propsOfDraggedItem.editorDispatch(
        [reparentComponents(draggedElements, target), clearHintAction],
        'everyone',
      )
      break
    default:
      propsOfDraggedItem.editorDispatch([clearHintAction], 'everyone')
      break
  }
}

function getHintPaddingForDepth(depth: number): number {
  return (
    depth * BasePaddingUnit +
    ExpansionArrowWidth +
    PreviewIconSize / 2 -
    NavigatorHintCircleDiameter
  )
}

function onHoverDropTargetLine(
  propsOfDraggedItem: NavigatorItemDragAndDropWrapperProps,
  propsOfDropTargetItem: NavigatorItemDragAndDropWrapperProps,
  monitor: DropTargetMonitor | null,
  position: 'before' | 'after',
  metadata: ElementInstanceMetadataMap,
): void {
  if (
    monitor == null ||
    !propsOfDraggedItem
      .getCurrentlySelectedEntries()
      .every((selection) => notDescendant(propsOfDropTargetItem, selection.elementPath)) ||
    EP.pathsEqual(
      propsOfDraggedItem.navigatorEntry.elementPath,
      propsOfDropTargetItem.navigatorEntry.elementPath,
    ) ||
    isHintDisallowed(propsOfDropTargetItem.navigatorEntry, metadata)
  ) {
    return propsOfDraggedItem.editorDispatch(
      [showNavigatorDropTargetHint(null, null, null)],
      'leftpane',
    )
  }

  const cursor = monitor.getClientOffset()
  const cursorDelta = monitor.getDifferenceFromInitialOffset()
  const targetAction = propsOfDraggedItem.highlighted
    ? []
    : [EditorActions.setHighlightedView(propsOfDraggedItem.navigatorEntry.elementPath)]

  if (cursor == null || cursorDelta == null) {
    return propsOfDraggedItem.editorDispatch(
      [showNavigatorDropTargetHint(null, null, null)],
      'leftpane',
    )
  }

  const targetEntryWithReparentWiggle: NavigatorEntry | null = (() => {
    if (cursorDelta.x >= -BasePaddingUnit) {
      return null
    }

    const maximumTargetDepth = propsOfDropTargetItem.entryDepth
    const cursorTargetDepth = 1 + Math.floor(Math.abs(cursorDelta.x) / BasePaddingUnit)

    const targetDepth = Math.min(cursorTargetDepth, maximumTargetDepth)

    return regularNavigatorEntry(
      EP.dropNPathParts(propsOfDropTargetItem.navigatorEntry.elementPath, targetDepth),
    )
  })()

  if (targetEntryWithReparentWiggle != null) {
    return propsOfDraggedItem.editorDispatch([
      ...targetAction,
      showNavigatorDropTargetHint(
        'reparent',
        targetEntryWithReparentWiggle,
        propsOfDropTargetItem.navigatorEntry,
      ),
    ])
  }

  if (
    propsOfDraggedItem.appropriateDropTargetHint?.type !== position ||
    !navigatorEntriesEqual(
      propsOfDraggedItem.appropriateDropTargetHint?.displayAtEntry,
      propsOfDropTargetItem.navigatorEntry,
    )
  ) {
    return propsOfDraggedItem.editorDispatch(
      [
        ...targetAction,
        showNavigatorDropTargetHint(
          position,
          propsOfDropTargetItem.navigatorEntry,
          propsOfDropTargetItem.navigatorEntry,
        ),
      ],
      'leftpane',
    )
  }

  return propsOfDraggedItem.editorDispatch(
    [showNavigatorDropTargetHint(null, null, null)],
    'leftpane',
  )
}

function onHoverParentOutline(
  propsOfDraggedItem: NavigatorItemDragAndDropWrapperProps,
  propsOfDropTargetItem: NavigatorItemDragAndDropWrapperProps,
  monitor: DropTargetMonitor | null,
): void {
  if (
    monitor == null ||
    !propsOfDraggedItem
      .getCurrentlySelectedEntries()
      .every((selection) => notDescendant(propsOfDropTargetItem, selection.elementPath)) ||
    navigatorEntriesEqual(propsOfDraggedItem.navigatorEntry, propsOfDropTargetItem.navigatorEntry)
  ) {
    return propsOfDraggedItem.editorDispatch(
      [showNavigatorDropTargetHint(null, null, null)],
      'leftpane',
    )
  }

  const cursor = monitor.getClientOffset()
  const cursorDelta = monitor.getDifferenceFromInitialOffset()
  const targetAction = propsOfDraggedItem.highlighted
    ? []
    : [EditorActions.setHighlightedView(propsOfDraggedItem.navigatorEntry.elementPath)]

  if (cursor == null || cursorDelta == null) {
    return propsOfDraggedItem.editorDispatch(
      [showNavigatorDropTargetHint(null, null, null)],
      'leftpane',
    )
  }

  const { collapsed, canReparentInto } = propsOfDropTargetItem

  if (!collapsed && canReparentInto) {
    return propsOfDraggedItem.editorDispatch([
      ...targetAction,
      showNavigatorDropTargetHint(
        'reparent',
        propsOfDropTargetItem.navigatorEntry,
        propsOfDropTargetItem.navigatorEntry,
      ),
    ])
  }

  return propsOfDraggedItem.editorDispatch(
    [showNavigatorDropTargetHint(null, null, null)],
    'leftpane',
  )
}

function beginDrag(
  props: NavigatorItemDragAndDropWrapperProps,
): NavigatorItemDragAndDropWrapperProps {
  if (!props.selected && isRegularNavigatorEntry(props.navigatorEntry)) {
    props.editorDispatch(
      MetaActions.selectComponents([props.navigatorEntry.elementPath], false),
      'leftpane',
    )
  }
  return props
}

interface DropCollectedProps {
  isOver: boolean
  canDrop: boolean
}

function canReparentIntoConditionalClause(
  entry: ConditionalClauseNavigatorEntry,
  jsxMetadata: ElementInstanceMetadataMap,
) {
  if (entry == null || !isConditionalClauseNavigatorEntry(entry)) {
    return false
  }
  const conditional = findMaybeConditionalExpression(entry.elementPath, jsxMetadata)
  if (conditional == null) {
    return false
  }
  const branch = getConditionalBranch(conditional, entry.clause)
  return isNullJSXAttributeValue(branch)
}

function isConditionalRoot(entry: NavigatorEntry | null, jsxMetadata: ElementInstanceMetadataMap) {
  return (
    entry != null &&
    !isConditionalClauseNavigatorEntry(entry) &&
    findMaybeConditionalExpression(entry.elementPath, jsxMetadata) != null
  )
}

function isInsideConditional(
  entry: NavigatorEntry | null,
  jsxMetadata: ElementInstanceMetadataMap,
) {
  return (
    entry != null &&
    findMaybeConditionalExpression(EP.parentPath(entry.elementPath), jsxMetadata) != null
  )
}

function isHintDisallowed(
  navigatorEntry: NavigatorEntry | null,
  metadata: ElementInstanceMetadataMap,
) {
  return (
    navigatorEntry == null ||
    isConditionalClauseNavigatorEntry(navigatorEntry) || // don't show top / bottom hints on TRUE/FALSE entries
    isSyntheticNavigatorEntry(navigatorEntry) || // don't show top / bottom hints on empty slots
    isInsideConditional(navigatorEntry, metadata) // don't show top / bottom hints on elements that are the root of a conditional branch
  )
}

export const NavigatorItemContainer = React.memo((props: NavigatorItemDragAndDropWrapperProps) => {
  const editorStateRef = useRefEditorState((store) => store.editor)

  const [, drag, preview] = useDrag(
    () => ({
      type: 'NAVIGATOR_ITEM',
      collect: (monitor) => ({
        isDragging: monitor.isDragging(),
      }),
      item: props,
      beginDrag: beginDrag,
      canDrag: () => {
        const editorState = editorStateRef.current
        const regularCanReparent =
          isRegularNavigatorEntry(props.navigatorEntry) &&
          isAllowedToReparent(
            editorState.projectContents,
            editorState.jsxMetadata,
            props.navigatorEntry.elementPath,
          )
        const syntheticCanReparent =
          isSyntheticNavigatorEntry(props.navigatorEntry) &&
          !isJSXArbitraryBlock(props.navigatorEntry.childOrAttribute)
        return regularCanReparent || syntheticCanReparent
      },
    }),
    [props],
  )

  const metadata = useEditorState(
    Substores.metadata,
    metadataSelector,
    'NavigatorItemContainer metadata',
  )

  const dropTarget = React.useMemo(() => {
    const fixedProps = { ...props }
    const { elementPath } = props.navigatorEntry
    const parentPath = EP.parentPath(elementPath)
    const conditionalParent = findMaybeConditionalExpression(EP.parentPath(elementPath), metadata)
    if (conditionalParent != null && !isConditionalClauseNavigatorEntry(props.navigatorEntry)) {
      const clause = maybeBranchConditionalCase(parentPath, conditionalParent, elementPath)
      if (clause != null) {
        fixedProps.navigatorEntry = {
          type: 'CONDITIONAL_CLAUSE',
          elementPath: parentPath,
          clause: clause,
        }
      }
    }
    return fixedProps
  }, [props, metadata])

  const moveToEntry = useEditorState(
    Substores.navigator,
    (store) => store.editor.navigator.dropTargetHint.moveToEntry,
    'NavigatorItemDndWrapper moveToElementPath',
  )

  const dropTargetHintType = useEditorState(
    Substores.navigator,
    (store) => store.editor.navigator.dropTargetHint.type,
    'NavigatorItemDndWrapper dropTargetHintType',
  )

  const [{ isOver: isOverBottomHint }, bottomDropRef] = useDrop<
    NavigatorItemDragAndDropWrapperProps,
    unknown,
    DropCollectedProps
  >(
    () => ({
      accept: 'NAVIGATOR_ITEM',
      collect: (monitor) => ({
        isOver: monitor.isOver(),
        canDrop: monitor.canDrop(),
      }),
      hover: (item: NavigatorItemDragAndDropWrapperProps, monitor) => {
        onHoverDropTargetLine(item, props, monitor, 'after', metadata)
      },
      drop: (item: NavigatorItemDragAndDropWrapperProps) => {
        if (moveToEntry != null) {
          onDrop(item, dropTarget, moveToEntry, dropTargetHintType)
        }
      },
      canDrop: (item: NavigatorItemDragAndDropWrapperProps) => {
        const editorState = editorStateRef.current
        return canDrop(editorState, item, props, 'bottom')
      },
    }),
    [props],
  )

  const [{ isOver: isOverTopHint }, topDropRef] = useDrop<
    NavigatorItemDragAndDropWrapperProps,
    unknown,
    DropCollectedProps
  >(
    () => ({
      accept: 'NAVIGATOR_ITEM',
      collect: (monitor) => ({
        isOver: monitor.isOver(),
        canDrop: monitor.canDrop(),
      }),
      hover: (item: NavigatorItemDragAndDropWrapperProps, monitor) => {
        onHoverDropTargetLine(item, props, monitor, 'before', metadata)
      },
      drop: (item: NavigatorItemDragAndDropWrapperProps) => {
        if (moveToEntry != null) {
          onDrop(item, dropTarget, moveToEntry, dropTargetHintType)
        }
      },
      canDrop: (item: NavigatorItemDragAndDropWrapperProps) => {
        const editorState = editorStateRef.current
        return canDrop(editorState, item, props, 'top')
      },
    }),
    [props],
  )

  const [, reparentDropRef] = useDrop<
    NavigatorItemDragAndDropWrapperProps,
    unknown,
    DropCollectedProps
  >(
    () => ({
      accept: 'NAVIGATOR_ITEM',
      collect: (monitor) => ({
        isOver: monitor.isOver(),
        canDrop: monitor.canDrop(),
      }),
      hover: (item: NavigatorItemDragAndDropWrapperProps, monitor) => {
        onHoverParentOutline(item, props, monitor)
      },
      drop: (item: NavigatorItemDragAndDropWrapperProps) => {
        if (moveToEntry != null) {
          onDrop(item, dropTarget, moveToEntry, dropTargetHintType)
        }
      },
      canDrop: (item: NavigatorItemDragAndDropWrapperProps, monitor) => {
        const editorState = editorStateRef.current
        return canDrop(editorState, item, props, 'hover')
      },
    }),
    [props],
  )

  const safeComponentId = varSafeNavigatorEntryToKey(props.navigatorEntry)

  React.useEffect(() => {
    preview(getEmptyImage(), { captureDraggingState: true })
  })

  const shouldShowTopHint = isOverTopHint && !isHintDisallowed(moveToEntry, metadata)

  const isConditionalEntry = MetadataUtils.isConditionalFromMetadata(
    MetadataUtils.findElementByElementPath(metadata, props.navigatorEntry.elementPath),
  )

  const shouldShowBottomHint =
    isOverBottomHint &&
    !isHintDisallowed(moveToEntry, metadata) &&
    (isConditionalEntry ? props.collapsed : true)

  const appropriateDropTargetHintDepth = useEditorState(
    Substores.metadata,
    (store) => {
      if (props.appropriateDropTargetHint?.moveToEntry == null) {
        return 0
      } else {
        return navigatorDepth(
          regularNavigatorEntry(props.appropriateDropTargetHint.moveToEntry.elementPath),
          store.editor.jsxMetadata,
        )
      }
    },
    'NavigatorItemDndWrapper appropriateDropTargetHintDepth',
  )

  const margin = (() => {
    if (
      props.appropriateDropTargetHint?.type === 'reparent' &&
      props.appropriateDropTargetHint.moveToEntry != null
    ) {
      return getHintPaddingForDepth(appropriateDropTargetHintDepth)
    }
    if (
      props.appropriateDropTargetHint?.type != null &&
      props.appropriateDropTargetHint.moveToEntry != null
    ) {
      return getHintPaddingForDepth(appropriateDropTargetHintDepth - 1)
    }

    return 0
  })()

  const parentOutline = React.useMemo((): ParentOutline => {
    if (moveToEntry == null) {
      return 'none'
    }

    const equalEntries = navigatorEntriesEqual(props.navigatorEntry, moveToEntry)

    if (
      props.navigatorEntry.type === 'SYNTHETIC' &&
      equalEntries &&
      isNullJSXAttributeValue(props.navigatorEntry.childOrAttribute)
    ) {
      return 'child'
    }

    if (dropTargetHintType !== 'reparent') {
      const wouldBeParentPath = EP.parentPath(moveToEntry.elementPath)
      if (
        MetadataUtils.isConditionalFromMetadata(
          MetadataUtils.findElementByElementPath(metadata, wouldBeParentPath),
        )
      ) {
        return 'none'
      }

      return EP.pathsEqual(wouldBeParentPath, props.navigatorEntry.elementPath) &&
        moveToEntry.type === 'REGULAR'
        ? 'solid'
        : 'none'
    }

    const parentPath = EP.parentPath(props.navigatorEntry.elementPath)
    const parentConditional = findMaybeConditionalExpression(parentPath, metadata)

    if (moveToEntry != null && isConditionalClauseNavigatorEntry(moveToEntry)) {
      // it's a conditional clause entry
      const canReparent = canReparentIntoConditionalClause(moveToEntry, metadata)
      const conditional = findMaybeConditionalExpression(moveToEntry.elementPath, metadata)
      if (canReparent && conditional != null) {
        const branch = getConditionalBranch(conditional, moveToEntry.clause)
        const branchPath = EP.appendToPath(moveToEntry.elementPath, branch.uid)
        if (
          EP.pathsEqual(parentPath, moveToEntry.elementPath) &&
          EP.pathsEqual(branchPath, props.navigatorEntry.elementPath)
        ) {
          return 'child'
        }
      }

      return 'none'
    } else if (parentConditional != null && equalEntries) {
      // it's a conditional branch item
      const element = MetadataUtils.findElementByElementPath(
        metadata,
        props.navigatorEntry.elementPath,
      )
      // if element is null than this is an empty slot
      if (element == null) {
        return 'child'
      }
      return 'solid'
    }

    if (moveToEntry != null && isRegularNavigatorEntry(moveToEntry) && equalEntries) {
      return 'solid'
    }

    return 'none'
  }, [dropTargetHintType, moveToEntry, metadata, props.navigatorEntry])

  const isFirstSibling = React.useMemo(() => {
    // FIXME: Performance: This is retrieving everything ordered and then getting just the siblings,
    // for every single navigator item.
    const siblings = MetadataUtils.getSiblingsOrdered(metadata, props.navigatorEntry.elementPath)
    const firstSibling = siblings.at(0)
    if (firstSibling == null) {
      return false
    }

    return EP.pathsEqual(firstSibling.elementPath, props.navigatorEntry.elementPath)
  }, [metadata, props.navigatorEntry])

  // Drop target lines should only intercept mouse events if a drag session is in progress
  const shouldDropLinesInterceptMouseEvents = dropTargetHintType != null

  return (
    <div
      data-testid={DragItemTestId(safeComponentId)}
      ref={drag}
      style={{
        ...props.windowStyle,
      }}
    >
      {when(
        isFirstSibling,
        <NavigatorHintTop
          testId={TopDropTargetLineTestId(safeComponentId)}
          ref={topDropRef}
          shouldBeShown={shouldShowTopHint}
          shouldAcceptMouseEvents={shouldDropLinesInterceptMouseEvents}
          margin={margin}
        />,
      )}
      <div
        ref={reparentDropRef}
        key='navigatorItem'
        id={`navigator-item-${safeComponentId}`}
        data-testid={`navigator-item-${safeComponentId}`}
      >
        <NavigatorItem
          navigatorEntry={props.navigatorEntry}
          index={props.index}
          getSelectedViewsInRange={props.getSelectedViewsInRange}
          noOfChildren={props.noOfChildren}
          label={props.label}
          dispatch={props.editorDispatch}
          isHighlighted={props.highlighted}
          isElementVisible={props.isElementVisible}
          renamingTarget={props.renamingTarget}
          collapsed={props.collapsed}
          selected={props.selected}
          parentOutline={parentOutline}
          visibleNavigatorTargets={props.visibleNavigatorTargets}
        />
      </div>
      <NavigatorHintBottom
        testId={BottomDropTargetLineTestId(safeComponentId)}
        ref={bottomDropRef}
        shouldBeShown={shouldShowBottomHint}
        shouldAcceptMouseEvents={shouldDropLinesInterceptMouseEvents}
        margin={margin}
      />
    </div>
  )
})
