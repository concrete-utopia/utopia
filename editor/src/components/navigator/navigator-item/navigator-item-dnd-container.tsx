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
  isRegularNavigatorEntry,
  navigatorEntriesEqual,
  NavigatorEntry,
  regularNavigatorEntry,
  syntheticNavigatorEntry,
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
import { ElementInstanceMetadataMap, JSXElementChild } from '../../../core/shared/element-template'
import {
  findMaybeConditionalExpression,
  isEmptyConditionalBranch,
  isNonEmptyConditionalBranch,
} from '../../../core/model/conditionals'

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

export interface NavigatorItemDragAndDropWrapperPropsBase {
  index: number
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

export interface NavigatorItemDragAndDropWrapperProps
  extends NavigatorItemDragAndDropWrapperPropsBase {
  elementPath: ElementPath
}

export interface SyntheticNavigatorItemContainerProps
  extends NavigatorItemDragAndDropWrapperPropsBase {
  elementPath: ElementPath
  childOrAttribute: JSXElementChild
}

export interface ConditionalClauseNavigatorItemContainerProps
  extends NavigatorItemDragAndDropWrapperPropsBase {
  navigatorEntry: ConditionalClauseNavigatorEntry
}

function notDescendant(
  draggedOnto: NavigatorItemDragAndDropWrapperProps,
  draggedItem: ElementPath,
): boolean {
  return !EP.isDescendantOfOrEqualTo(draggedOnto.elementPath, draggedItem)
}

function canDrop(
  editorState: EditorState,
  draggedItem: NavigatorItemDragAndDropWrapperProps,
  draggedOnto: NavigatorItemDragAndDropWrapperProps,
): boolean {
  const isReparentTarget = draggedOnto.appropriateDropTargetHint?.type === 'reparent'
  const targetSupportsChildren = MetadataUtils.targetSupportsChildren(
    editorState.projectContents,
    editorState.jsxMetadata,
    editorState.nodeModules.files,
    editorState.canvas.openFile?.filename,
    draggedOnto.elementPath,
  )

  const childrenSupportedIfRequired = isReparentTarget ? targetSupportsChildren : true

  const notSelectedItem = draggedItem.getCurrentlySelectedEntries().every((selection) => {
    return notDescendant(draggedOnto, selection.elementPath)
  })

  return childrenSupportedIfRequired && notSelectedItem
}

function canDropNextTo(
  draggedItem: NavigatorItemDragAndDropWrapperProps,
  draggedOnto: NavigatorItemDragAndDropWrapperProps,
): boolean {
  const notSelectedItem = draggedItem.getCurrentlySelectedEntries().every((selection) => {
    return notDescendant(draggedOnto, selection.elementPath)
  })

  const targetNotRootOfInstance = !EP.isRootElementOfInstance(draggedOnto.elementPath)

  return notSelectedItem && targetNotRootOfInstance
}

function onDrop(
  propsOfDraggedItem: NavigatorItemDragAndDropWrapperProps,
  propsOfDropTargetItem: NavigatorItemDragAndDropWrapperProps,
  moveToElementPath: ElementPath,
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
        [placeComponentsBefore(draggedElements, moveToElementPath), clearHintAction],
        'everyone',
      )
      break
    case 'after':
      propsOfDraggedItem.editorDispatch(
        [placeComponentsAfter(draggedElements, moveToElementPath), clearHintAction],
        'everyone',
      )
      break
    case 'reparent':
      propsOfDraggedItem.editorDispatch(
        [reparentComponents(draggedElements, moveToElementPath), clearHintAction],
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
    EP.pathsEqual(propsOfDraggedItem.elementPath, propsOfDropTargetItem.elementPath) ||
    isHintDisallowed(propsOfDropTargetItem.elementPath, metadata)
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
    : [EditorActions.setHighlightedView(propsOfDraggedItem.elementPath)]

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

    return regularNavigatorEntry(EP.dropNPathParts(propsOfDropTargetItem.elementPath, targetDepth))
  })()

  if (targetEntryWithReparentWiggle != null) {
    return propsOfDraggedItem.editorDispatch([
      ...targetAction,
      showNavigatorDropTargetHint(
        'reparent',
        targetEntryWithReparentWiggle,
        regularNavigatorEntry(propsOfDropTargetItem.elementPath),
      ),
    ])
  }

  if (
    propsOfDraggedItem.appropriateDropTargetHint?.type !== position ||
    !navigatorEntriesEqual(
      propsOfDraggedItem.appropriateDropTargetHint?.displayAtEntry,
      regularNavigatorEntry(propsOfDropTargetItem.elementPath),
    )
  ) {
    return propsOfDraggedItem.editorDispatch(
      [
        ...targetAction,
        showNavigatorDropTargetHint(
          position,
          regularNavigatorEntry(propsOfDropTargetItem.elementPath),
          regularNavigatorEntry(propsOfDropTargetItem.elementPath),
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
    EP.pathsEqual(propsOfDraggedItem.elementPath, propsOfDropTargetItem.elementPath)
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
    : [EditorActions.setHighlightedView(propsOfDraggedItem.elementPath)]

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
        regularNavigatorEntry(propsOfDropTargetItem.elementPath),
        regularNavigatorEntry(propsOfDropTargetItem.elementPath),
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
  if (!props.selected) {
    props.editorDispatch(MetaActions.selectComponents([props.elementPath], false), 'leftpane')
  }
  return props
}

interface DropCollectedProps {
  isOver: boolean
  canDrop: boolean
}

function isInsideConditional(elementPath: ElementPath, jsxMetadata: ElementInstanceMetadataMap) {
  return findMaybeConditionalExpression(EP.parentPath(elementPath), jsxMetadata) != null
}

function isHintDisallowed(elementPath: ElementPath | null, metadata: ElementInstanceMetadataMap) {
  return elementPath == null || isInsideConditional(elementPath, metadata) // don't show top / bottom hints on elements that are the root of a conditional branch
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
        return isAllowedToReparent(
          editorState.projectContents,
          editorState.jsxMetadata,
          props.elementPath,
        )
      },
    }),
    [props],
  )

  const metadata = useEditorState(
    Substores.metadata,
    metadataSelector,
    'NavigatorItemContainer metadata',
  )

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

  const [{ isOver: isOverBottomHint, canDrop: canDropOnBottomHint }, bottomDropRef] = useDrop<
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
        if (monitor.canDrop()) {
          onHoverDropTargetLine(item, props, monitor, 'after', editorStateRef.current.jsxMetadata)
        }
      },
      drop: (item: NavigatorItemDragAndDropWrapperProps) => {
        if (moveToEntry != null) {
          onDrop(item, props, moveToEntry.elementPath, dropTargetHintType)
        }
      },
      canDrop: (item: NavigatorItemDragAndDropWrapperProps) => {
        return canDropNextTo(item, props)
      },
    }),
    [props, moveToEntry, dropTargetHintType],
  )

  const [{ isOver: isOverTopHint, canDrop: canDropOnTopHint }, topDropRef] = useDrop<
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
        if (monitor.canDrop()) {
          onHoverDropTargetLine(item, props, monitor, 'before', editorStateRef.current.jsxMetadata)
        }
      },
      drop: (item: NavigatorItemDragAndDropWrapperProps) => {
        if (moveToEntry != null) {
          onDrop(item, props, moveToEntry.elementPath, dropTargetHintType)
        }
      },
      canDrop: (item: NavigatorItemDragAndDropWrapperProps) => {
        return canDropNextTo(item, props)
      },
    }),
    [props, moveToEntry, dropTargetHintType],
  )

  const [{ canDrop: canDropParentOutline }, reparentDropRef] = useDrop<
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
        if (monitor.canDrop()) {
          onHoverParentOutline(item, props, monitor)
        }
      },
      drop: (item: NavigatorItemDragAndDropWrapperProps, monitor) => {
        if (monitor.canDrop() && moveToEntry != null) {
          onDrop(item, props, moveToEntry.elementPath, dropTargetHintType)
        }
      },
      canDrop: (item: NavigatorItemDragAndDropWrapperProps) => {
        const editorState = editorStateRef.current
        return canDrop(editorState, item, props)
      },
    }),
    [props, moveToEntry],
  )

  const safeComponentId = varSafeNavigatorEntryToKey(regularNavigatorEntry(props.elementPath))

  React.useEffect(() => {
    preview(getEmptyImage(), { captureDraggingState: true })
  })

  const shouldShowTopHint =
    isOverTopHint &&
    canDropOnTopHint &&
    !isHintDisallowed(moveToEntry?.elementPath ?? null, metadata)

  const isConditionalEntry = MetadataUtils.isConditionalFromMetadata(
    MetadataUtils.findElementByElementPath(metadata, props.elementPath),
  )

  const isCollapsedCondtionalEntry = isConditionalEntry ? props.collapsed : true

  const shouldShowBottomHint =
    isOverBottomHint &&
    canDropOnBottomHint &&
    !isHintDisallowed(props.elementPath, metadata) &&
    isCollapsedCondtionalEntry

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
    if (moveToEntry == null || !canDropParentOutline) {
      return 'none'
    }

    const equalEntries = navigatorEntriesEqual(
      regularNavigatorEntry(props.elementPath),
      moveToEntry,
    )

    if (dropTargetHintType !== 'reparent') {
      const wouldBeParentPath = EP.parentPath(moveToEntry.elementPath)
      if (
        MetadataUtils.isConditionalFromMetadata(
          MetadataUtils.findElementByElementPath(metadata, wouldBeParentPath),
        )
      ) {
        return 'none'
      }

      return EP.pathsEqual(wouldBeParentPath, props.elementPath) && moveToEntry.type === 'REGULAR'
        ? 'solid'
        : 'none'
    }

    if (moveToEntry != null && isRegularNavigatorEntry(moveToEntry) && equalEntries) {
      return 'solid'
    }

    return 'none'
  }, [moveToEntry, canDropParentOutline, props.elementPath, dropTargetHintType, metadata])

  const isFirstSibling = React.useMemo(() => {
    // FIXME: Performance: This is retrieving everything ordered and then getting just the siblings,
    // for every single navigator item.
    const siblings = MetadataUtils.getSiblingsOrdered(metadata, props.elementPath)
    const firstSibling = siblings.at(0)
    if (firstSibling == null) {
      return false
    }

    return EP.pathsEqual(firstSibling.elementPath, props.elementPath)
  }, [metadata, props.elementPath])

  // Drop target lines should only intercept mouse events if a drag session is in progress
  const isDragSessionInProgress = dropTargetHintType != null
  const shouldTopDropLineInterceptMouseEvents = isDragSessionInProgress

  // in addition, if this entry is a conditional, the bottom drop target line should only be active when
  // the it's toggled closed (since otherwise the drop line would show up between the entry and the TRUE
  // entry underneath)
  const shouldBottomDropLineInterceptMouseEvents =
    isDragSessionInProgress && isCollapsedCondtionalEntry

  const navigatorEntry = React.useMemo(
    () => regularNavigatorEntry(props.elementPath),
    [props.elementPath],
  )

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
          shouldAcceptMouseEvents={shouldTopDropLineInterceptMouseEvents}
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
          navigatorEntry={navigatorEntry}
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
        shouldAcceptMouseEvents={shouldBottomDropLineInterceptMouseEvents}
        margin={margin}
      />
    </div>
  )
})

export const SyntheticNavigatorItemContainer = React.memo(
  (props: SyntheticNavigatorItemContainerProps) => {
    const editorStateRef = useRefEditorState((store) => store.editor)

    const navigatorEntry = React.useMemo(
      () => syntheticNavigatorEntry(props.elementPath, props.childOrAttribute),
      [props.childOrAttribute, props.elementPath],
    )

    const [{ isOver }, reparentDropRef] = useDrop<
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
        drop: (item: NavigatorItemDragAndDropWrapperProps): void => {
          onDrop(item, props, props.elementPath, 'reparent')
        },
        canDrop: () => {
          const metadata = editorStateRef.current.jsxMetadata
          return isEmptyConditionalBranch(props.elementPath, metadata)
        },
      }),
      [props, navigatorEntry],
    )

    const [, drag, preview] = useDrag(
      () => ({
        type: 'NAVIGATOR_ITEM',
        collect: (monitor) => ({
          isDragging: monitor.isDragging(),
        }),
        item: props,
        beginDrag: beginDrag,
        canDrag: () =>
          isNonEmptyConditionalBranch(props.elementPath, editorStateRef.current.jsxMetadata),
      }),
      [props],
    )

    React.useEffect(() => {
      preview(getEmptyImage(), { captureDraggingState: true })
    })

    const parentOutline = isOver ? 'child' : 'none'
    const safeComponentId = varSafeNavigatorEntryToKey(navigatorEntry)
    return (
      <div
        data-testid={DragItemTestId(safeComponentId)}
        ref={drag}
        style={{
          ...props.windowStyle,
        }}
      >
        <div
          ref={reparentDropRef}
          key='navigatorItem'
          id={`navigator-item-${safeComponentId}`}
          data-testid={`navigator-item-${safeComponentId}`}
        >
          <NavigatorItem
            navigatorEntry={navigatorEntry}
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
      </div>
    )
  },
)

export const ConditionalClauseNavigatorItemContainer = React.memo(
  (props: ConditionalClauseNavigatorItemContainerProps) => {
    const safeComponentId = varSafeNavigatorEntryToKey(props.navigatorEntry)
    return (
      <div
        style={{
          ...props.windowStyle,
        }}
      >
        <div
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
            parentOutline={'none'}
            visibleNavigatorTargets={props.visibleNavigatorTargets}
          />
        </div>
      </div>
    )
  },
)
