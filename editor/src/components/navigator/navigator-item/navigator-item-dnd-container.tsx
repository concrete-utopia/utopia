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
  ElementWarnings,
  isRegularNavigatorEntry,
  NavigatorEntry,
  navigatorEntryToKey,
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
  getDragSelections: () => Array<DragSelection>
  getSelectedViewsInRange: (index: number) => Array<ElementPath> // TODO remove me
  canReparentInto: boolean
  noOfChildren: number
  label: string
  isElementVisible: boolean
  renamingTarget: ElementPath | null
  elementWarnings: ElementWarnings
  windowStyle: React.CSSProperties
  visibleNavigatorTargets: Array<NavigatorEntry>
}

function canDrop(props: NavigatorItemDragAndDropWrapperProps, dropSource: ElementPath): boolean {
  return !EP.isDescendantOfOrEqualTo(props.navigatorEntry.elementPath, dropSource)
}

function onDrop(
  propsOfDraggedItem: NavigatorItemDragAndDropWrapperProps,
  propsOfDropTargetItem: NavigatorItemDragAndDropWrapperProps,
  monitor: DropTargetMonitor,
): void {
  if (monitor == null) {
    return
  }
  const dragSelections = propsOfDraggedItem.getDragSelections()
  const filteredSelections = dragSelections.filter((selection) =>
    canDrop(propsOfDropTargetItem, selection.elementPath),
  )
  const draggedElements = filteredSelections.map((selection) => selection.elementPath)
  const clearHintAction = showNavigatorDropTargetHint(null, null, null)
  const target =
    propsOfDropTargetItem.appropriateDropTargetHint?.moveToElementPath ??
    propsOfDropTargetItem.navigatorEntry.elementPath

  switch (propsOfDropTargetItem.appropriateDropTargetHint?.type) {
    case 'before':
      propsOfDraggedItem.editorDispatch(
        [placeComponentsBefore(draggedElements, target), clearHintAction],
        'everyone',
      )
      break
    case 'after':
      propsOfDraggedItem.editorDispatch(
        [placeComponentsAfter(draggedElements, target), clearHintAction],
        'everyone',
      )
      break
    case 'reparent':
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
): void {
  if (
    monitor == null ||
    !propsOfDraggedItem
      .getDragSelections()
      .every((selection) => canDrop(propsOfDropTargetItem, selection.elementPath)) ||
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

  const canReparentInto = propsOfDropTargetItem.canReparentInto

  if (cursor == null || cursorDelta == null) {
    return propsOfDraggedItem.editorDispatch(
      [showNavigatorDropTargetHint(null, null, null)],
      'leftpane',
    )
  }

  const targetPathWithReparentWiggle = (() => {
    if (
      cursorDelta.x >= -BasePaddingUnit ||
      EP.parentPath(propsOfDraggedItem.navigatorEntry.elementPath) == null
    ) {
      return propsOfDropTargetItem.navigatorEntry.elementPath
    }

    const maximumTargetDepth = propsOfDropTargetItem.entryDepth - 1
    const cursorTargetDepth = Math.floor(Math.abs(cursorDelta.x) / BasePaddingUnit)

    const targetDepth = Math.min(cursorTargetDepth, maximumTargetDepth)

    return EP.dropNPathParts(propsOfDropTargetItem.navigatorEntry.elementPath, targetDepth)
  })()

  if (propsOfDraggedItem.noOfChildren === 0 || propsOfDraggedItem.collapsed) {
    if (canReparentInto && cursorDelta.x >= BasePaddingUnit) {
      return propsOfDraggedItem.editorDispatch([
        ...targetAction,
        showNavigatorDropTargetHint(
          'reparent',
          targetPathWithReparentWiggle,
          propsOfDropTargetItem.navigatorEntry.elementPath,
        ),
      ])
    }
  }

  if (
    propsOfDraggedItem.appropriateDropTargetHint?.type !== position ||
    !EP.pathsEqual(
      propsOfDraggedItem.appropriateDropTargetHint?.displayAtElementPath,
      propsOfDropTargetItem.navigatorEntry.elementPath,
    )
  ) {
    return propsOfDraggedItem.editorDispatch(
      [
        ...targetAction,
        showNavigatorDropTargetHint(
          position,
          targetPathWithReparentWiggle,
          propsOfDropTargetItem.navigatorEntry.elementPath,
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
      .getDragSelections()
      .every((selection) => canDrop(propsOfDropTargetItem, selection.elementPath)) ||
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

  const canReparentInto = propsOfDropTargetItem.canReparentInto

  if (cursor == null || cursorDelta == null) {
    return propsOfDraggedItem.editorDispatch(
      [showNavigatorDropTargetHint(null, null, null)],
      'leftpane',
    )
  }

  if (propsOfDraggedItem.noOfChildren === 0 || propsOfDraggedItem.collapsed) {
    if (canReparentInto) {
      return propsOfDraggedItem.editorDispatch([
        ...targetAction,
        showNavigatorDropTargetHint(
          'reparent',
          propsOfDropTargetItem.navigatorEntry.elementPath,
          propsOfDropTargetItem.navigatorEntry.elementPath,
        ),
      ])
    }
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
        const result =
          isRegularNavigatorEntry(props.navigatorEntry) &&
          isAllowedToReparent(
            editorState.projectContents,
            editorState.jsxMetadata,
            props.navigatorEntry.elementPath,
          )
        return result
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
        const isReparentTarget = item.appropriateDropTargetHint?.type === 'reparent'
        const childrenSupportedIfRequired =
          !isReparentTarget ||
          (isRegularNavigatorEntry(props.navigatorEntry) &&
            MetadataUtils.targetSupportsChildren(
              editorState.projectContents,
              editorState.jsxMetadata,
              props.navigatorEntry.elementPath,
            ))
        const notSelectedItem = item.getDragSelections().every((selection) => {
          return canDrop(props, selection.elementPath)
        })
        return childrenSupportedIfRequired && notSelectedItem
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
        const isReparentTarget = item.appropriateDropTargetHint?.type === 'reparent'
        const childrenSupportedIfRequired =
          !isReparentTarget ||
          (isRegularNavigatorEntry(props.navigatorEntry) &&
            MetadataUtils.targetSupportsChildren(
              editorState.projectContents,
              editorState.jsxMetadata,
              props.navigatorEntry.elementPath,
            ))
        const notSelectedItem = item.getDragSelections().every((selection) => {
          return canDrop(props, selection.elementPath)
        })
        return childrenSupportedIfRequired && notSelectedItem
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
        const isReparentTarget = item.appropriateDropTargetHint?.type === 'reparent'
        const childrenSupportedIfRequired =
          !isReparentTarget ||
          (isRegularNavigatorEntry(props.navigatorEntry) &&
            MetadataUtils.targetSupportsChildren(
              editorState.projectContents,
              editorState.jsxMetadata,
              props.navigatorEntry.elementPath,
            ))
        const notSelectedItem = item.getDragSelections().every((selection) => {
          return canDrop(props, selection.elementPath)
        })
        return childrenSupportedIfRequired && notSelectedItem
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
    (store) => store.editor.navigator.dropTargetHint.moveToElementPath,
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
      if (props.appropriateDropTargetHint?.moveToElementPath == null) {
        return 0
      } else {
        return navigatorDepth(
          regularNavigatorEntry(props.appropriateDropTargetHint.moveToElementPath),
          store.editor.jsxMetadata,
        )
      }
    },
    'NavigatorItemDndWrapper appropriateDropTargetHintDepth',
  )

  const margin = (() => {
    if (
      props.appropriateDropTargetHint?.type === 'reparent' &&
      props.appropriateDropTargetHint.moveToElementPath != null
    ) {
      return getHintPaddingForDepth(appropriateDropTargetHintDepth)
    }
    if (
      props.appropriateDropTargetHint?.type != null &&
      props.appropriateDropTargetHint.moveToElementPath != null
    ) {
      return getHintPaddingForDepth(appropriateDropTargetHintDepth - 1)
    }

    return 0
  })()

  const shouldShowParentOutline =
    dropTargetHintType === 'reparent'
      ? isOverBottomHint || isOverParentOutline
      : moveToElementPath != null &&
        EP.pathsEqual(props.navigatorEntry.elementPath, EP.parentPath(moveToElementPath))

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
          elementWarnings={props.elementWarnings}
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
