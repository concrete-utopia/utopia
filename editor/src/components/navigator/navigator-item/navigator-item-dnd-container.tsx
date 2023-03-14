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
import { BasePaddingUnit, NavigatorItem } from './navigator-item'
import {
  NavigatorHintBottom,
  NavigatorHintCircleDiameter,
  NavigatorHintTop,
} from './navigator-item-components'
import {
  DropTargetHint,
  EditorState,
  ElementWarnings,
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
import { childOrBlockIsChild } from '../../../core/shared/element-template'

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
): boolean {
  const isReparentTarget = draggedItem.appropriateDropTargetHint?.type === 'reparent'
  const childrenSupportedIfRequired =
    !isReparentTarget ||
    isConditionalClauseNavigatorEntry(draggedOnto.navigatorEntry) ||
    (isRegularNavigatorEntry(draggedOnto.navigatorEntry) &&
      MetadataUtils.targetSupportsChildren(
        editorState.projectContents,
        editorState.jsxMetadata,
        draggedOnto.navigatorEntry.elementPath,
      ))
  const notSelectedItem = draggedItem.getCurrentlySelectedEntries().every((selection) => {
    return notDescendant(draggedOnto, selection.elementPath)
  })
  const result = childrenSupportedIfRequired && notSelectedItem
  return result
}

function onDrop(
  propsOfDraggedItem: NavigatorItemDragAndDropWrapperProps,
  propsOfDropTargetItem: NavigatorItemDragAndDropWrapperProps,
  monitor: DropTargetMonitor,
): void {
  if (monitor == null) {
    return
  }
  const dragSelections = propsOfDraggedItem.getCurrentlySelectedEntries()
  const filteredSelections = dragSelections.filter((selection) =>
    notDescendant(propsOfDropTargetItem, selection.elementPath),
  )
  const draggedElements = filteredSelections.map((selection) => selection.elementPath)
  const clearHintAction = showNavigatorDropTargetHint(null, null, null)
  const target =
    propsOfDropTargetItem.appropriateDropTargetHint?.moveToEntry ??
    propsOfDropTargetItem.navigatorEntry

  switch (propsOfDropTargetItem.appropriateDropTargetHint?.type) {
    case 'before':
      propsOfDraggedItem.editorDispatch(
        [placeComponentsBefore(draggedElements, target.elementPath), clearHintAction],
        'everyone',
      )
      break
    case 'after':
      propsOfDraggedItem.editorDispatch(
        [placeComponentsAfter(draggedElements, target.elementPath), clearHintAction],
        'everyone',
      )
      break
    case 'reparent':
      propsOfDraggedItem.editorDispatch(
        [
          reparentComponents(draggedElements, propsOfDropTargetItem.navigatorEntry),
          clearHintAction,
        ],
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
): void {
  if (
    monitor == null ||
    !propsOfDraggedItem
      .getCurrentlySelectedEntries()
      .every((selection) => notDescendant(propsOfDropTargetItem, selection.elementPath)) ||
    EP.pathsEqual(
      propsOfDraggedItem.navigatorEntry.elementPath,
      propsOfDropTargetItem.navigatorEntry.elementPath,
    )
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

  const targetEntryWithReparentWiggle: NavigatorEntry = (() => {
    if (
      cursorDelta.x >= -BasePaddingUnit ||
      EP.parentPath(propsOfDraggedItem.navigatorEntry.elementPath) == null
    ) {
      return propsOfDropTargetItem.navigatorEntry
    }

    const maximumTargetDepth = propsOfDropTargetItem.entryDepth - 1
    const cursorTargetDepth = Math.floor(Math.abs(cursorDelta.x) / BasePaddingUnit)

    const targetDepth = Math.min(cursorTargetDepth, maximumTargetDepth)

    return regularNavigatorEntry(
      EP.dropNPathParts(propsOfDropTargetItem.navigatorEntry.elementPath, targetDepth),
    )
  })()

  const { collapsed, canReparentInto } = propsOfDropTargetItem

  if (!collapsed && canReparentInto && cursorDelta.x >= BasePaddingUnit) {
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
          targetEntryWithReparentWiggle,
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
      canDrag: (monitor) => {
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
          childOrBlockIsChild(props.navigatorEntry.childOrAttribute)
        return regularCanReparent || syntheticCanReparent
      },
    }),
    [props],
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
        onHoverDropTargetLine(item, props, monitor, 'after')
      },
      drop: (item: NavigatorItemDragAndDropWrapperProps, monitor) => {
        onDrop(item, props, monitor)
      },
      canDrop: (item: NavigatorItemDragAndDropWrapperProps, monitor) => {
        const editorState = editorStateRef.current
        return canDrop(editorState, item, props)
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
        onHoverDropTargetLine(item, props, monitor, 'before')
      },
      drop: (item: NavigatorItemDragAndDropWrapperProps, monitor) => {
        onDrop(item, props, monitor)
      },
      canDrop: (item: NavigatorItemDragAndDropWrapperProps, monitor) => {
        const editorState = editorStateRef.current
        return canDrop(editorState, item, props)
      },
    }),
    [props],
  )

  const [{ isOver: isOverParentOutline }, reparentDropRef] = useDrop<
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
      drop: (item: NavigatorItemDragAndDropWrapperProps, monitor) => {
        onDrop(item, props, monitor)
      },
      canDrop: (item: NavigatorItemDragAndDropWrapperProps, monitor) => {
        const editorState = editorStateRef.current
        return canDrop(editorState, item, props)
      },
    }),
    [props],
  )

  const safeComponentId = varSafeNavigatorEntryToKey(props.navigatorEntry)

  React.useEffect(() => {
    preview(getEmptyImage(), { captureDraggingState: true })
  })

  const moveToElementPath = useEditorState(
    Substores.navigator,
    (store) => store.editor.navigator.dropTargetHint.moveToEntry,
    'NavigatorItemDndWrapper moveToElementPath',
  )

  const dropTargetHintType = useEditorState(
    Substores.navigator,
    (store) => store.editor.navigator.dropTargetHint.type,
    'NavigatorItemDndWrapper dropTargetHintType',
  )

  const shouldShowBottomHint =
    isOverBottomHint &&
    (props.appropriateDropTargetHint?.type === 'after' ||
      props.appropriateDropTargetHint?.type === 'reparent')

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

  const shouldShowParentOutline =
    dropTargetHintType === 'reparent'
      ? isOverBottomHint || isOverParentOutline
      : moveToElementPath != null &&
        (isRegularNavigatorEntry(moveToElementPath) ||
          isConditionalClauseNavigatorEntry(moveToElementPath)) &&
        navigatorEntriesEqual(props.navigatorEntry, moveToElementPath)

  const metadata = useEditorState(
    Substores.metadata,
    metadataSelector,
    'NavigatorItemContainer metadata',
  )

  const isFirstSibling = React.useMemo(() => {
    if (!isRegularNavigatorEntry(props.navigatorEntry)) {
      return false
    }

    // FIXME: Performance: This is retrieving everything ordered and then getting just the siblings,
    // for every single navigator item.
    const siblings = MetadataUtils.getSiblingsOrdered(metadata, props.navigatorEntry.elementPath)
    const firstSibling = siblings.at(0)
    if (firstSibling == null) {
      return false
    }

    return EP.pathsEqual(firstSibling.elementPath, props.navigatorEntry.elementPath)
  }, [metadata, props.navigatorEntry])

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
          shouldBeShown={isOverTopHint}
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
          shouldShowParentOutline={shouldShowParentOutline}
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
