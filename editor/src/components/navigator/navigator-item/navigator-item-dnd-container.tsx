/** @jsxRuntime classic */
/** @jsx jsx */
import { jsx } from '@emotion/react'
import React from 'react'
import { PureComponent } from 'react'
import { DropTargetMonitor, useDrag, useDrop } from 'react-dnd'
import * as ReactDOM from 'react-dom'
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
import { CollectResults, isCursorInBottomArea, isCursorInTopArea } from '../drag-and-drop-utils'
import { ExpansionArrowWidth } from './expandable-indicator'
import { BasePaddingUnit, NavigatorItem } from './navigator-item'
import {
  NavigatorHintBottom,
  NavigatorHintCircleDiameter,
  NavigatorHintTop,
} from './navigator-item-components'
import { DropTargetHint, ElementWarnings } from '../../editor/store/editor-state'
import {
  Substores,
  useEditorState,
  useRefEditorState,
} from '../../../components/editor/store/store-hook'
import { isAllowedToReparent } from '../../canvas/canvas-strategies/strategies/reparent-helpers/reparent-helpers'
import { MetadataUtils } from '../../../core/model/element-metadata-utils'
import { getEmptyImage } from 'react-dnd-html5-backend'
import { when } from '../../../utils/react-conditionals'

const BaseRowHeight = 35
const PreviewIconSize = BaseRowHeight

export interface DragSelection {
  elementPath: ElementPath
  index: number
}

export interface NavigatorItemDragAndDropWrapperProps {
  index: number
  elementPath: ElementPath
  appropriateDropTargetHint: DropTargetHint | null
  editorDispatch: EditorDispatch
  selected: boolean
  highlighted: boolean // TODO are we sure about this?
  collapsed: boolean // TODO are we sure about this?
  getDragSelections: () => Array<DragSelection>
  getSelectedViewsInRange: (index: number) => Array<ElementPath> // TODO remove me
  supportsChildren: boolean
  noOfChildren: number
  label: string
  isElementVisible: boolean
  renamingTarget: ElementPath | null
  elementWarnings: ElementWarnings
  windowStyle: React.CSSProperties
  visibleNavigatorTargets: Array<ElementPath>
}

function canDrop(props: NavigatorItemDragAndDropWrapperProps, dropSource: ElementPath): boolean {
  return !EP.isDescendantOfOrEqualTo(props.elementPath, dropSource)
}

function onDrop(
  propsOfDraggedItem: NavigatorItemDragAndDropWrapperProps,
  propsOfDropTargetItem: NavigatorItemDragAndDropWrapperProps,
  monitor: DropTargetMonitor,
  component: HTMLDivElement | null,
): void {
  if (monitor != null && component != null) {
    const dragSelections = propsOfDraggedItem.getDragSelections()
    const filteredSelections = dragSelections.filter((selection) =>
      canDrop(propsOfDropTargetItem, selection.elementPath),
    )
    const draggedElements = filteredSelections.map((selection) => selection.elementPath)
    const clearHintAction = showNavigatorDropTargetHint(null, null, null)
    const target =
      propsOfDropTargetItem.appropriateDropTargetHint?.moveToElementPath ??
      propsOfDropTargetItem.elementPath

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
}

function getHintPadding(path: ElementPath, visibleNavigatorTargets: Array<ElementPath>): number {
  return (
    EP.navigatorDepth(path) * BasePaddingUnit +
    ExpansionArrowWidth +
    PreviewIconSize / 2 -
    NavigatorHintCircleDiameter
  )
}

function onHover(
  propsOfDraggedItem: NavigatorItemDragAndDropWrapperProps,
  propsOfDropTargetItem: NavigatorItemDragAndDropWrapperProps,
  monitor: DropTargetMonitor | null,
  component: HTMLDivElement | null,
  indexInParent: number,
  visibleNavigatorTargets: Array<ElementPath>,
): void {
  if (
    monitor != null &&
    component != null &&
    propsOfDraggedItem
      .getDragSelections()
      .every((selection) => canDrop(propsOfDropTargetItem, selection.elementPath))
  ) {
    // React DnD necessitates the two divs around the actual navigator item,
    // so we need to drill down to the navigator elements themselves which have real dimensions.
    const dropDomNode = ReactDOM.findDOMNode(component)
    const dragDomNode = dropDomNode?.firstChild
    const domNode = dragDomNode?.firstChild
    if (domNode == null || typeof domNode === 'string') {
      return
    }
    const dropTargetRectangle = (domNode as HTMLElement).getBoundingClientRect()
    const cursor = monitor.getClientOffset()
    const cursorDelta = monitor.getDifferenceFromInitialOffset()
    const targetAction = propsOfDraggedItem.highlighted
      ? []
      : [EditorActions.setHighlightedView(propsOfDraggedItem.elementPath)]

    const canReparent = propsOfDropTargetItem.supportsChildren
    // if it's the first item, we allow both the top and the bottom hint
    // if reparenting is possible, hovering the middle of an item shows the reparent outline
    const numberOfAreasToCut = (indexInParent === 0 ? 2 : 1) + (canReparent ? 1 : 0)

    if (cursor == null || cursorDelta == null) {
      return
    }

    const targetPathWithReparentWiggle = (() => {
      if (
        cursorDelta.x >= -BasePaddingUnit ||
        EP.parentPath(propsOfDraggedItem.elementPath) == null
      ) {
        return propsOfDropTargetItem.elementPath
      }

      const maximumTargetDepth = EP.navigatorDepth(propsOfDropTargetItem.elementPath)
      const cursorTargetDepth = Math.floor(Math.abs(cursorDelta.x) / BasePaddingUnit)

      const targetDepth = Math.min(cursorTargetDepth, maximumTargetDepth)

      return EP.dropNPathParts(propsOfDropTargetItem.elementPath, targetDepth)
    })()

    if (
      indexInParent === 0 &&
      isCursorInTopArea(dropTargetRectangle, cursor.y, numberOfAreasToCut) &&
      propsOfDraggedItem.appropriateDropTargetHint?.type !== 'before'
    ) {
      return propsOfDraggedItem.editorDispatch(
        [
          ...targetAction,
          showNavigatorDropTargetHint(
            'before',
            targetPathWithReparentWiggle,
            propsOfDropTargetItem.elementPath,
          ),
        ],
        'leftpane',
      )
    }

    if (
      isCursorInBottomArea(dropTargetRectangle, cursor.y, numberOfAreasToCut) &&
      (propsOfDraggedItem.noOfChildren === 0 || propsOfDraggedItem.collapsed)
    ) {
      if (
        propsOfDraggedItem.appropriateDropTargetHint?.type !== 'after' ||
        !EP.pathsEqual(
          propsOfDraggedItem.appropriateDropTargetHint?.displayAtElementPath,
          propsOfDropTargetItem.elementPath,
        )
      ) {
        return propsOfDraggedItem.editorDispatch(
          [
            ...targetAction,
            showNavigatorDropTargetHint(
              'after',
              targetPathWithReparentWiggle,
              propsOfDropTargetItem.elementPath,
            ),
          ],
          'leftpane',
        )
      }
    }

    if (canReparent && propsOfDraggedItem.appropriateDropTargetHint?.type !== 'reparent') {
      return propsOfDraggedItem.editorDispatch(
        [
          ...targetAction,
          showNavigatorDropTargetHint(
            'reparent',
            propsOfDropTargetItem.elementPath,
            propsOfDropTargetItem.elementPath,
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
}

function beginDrag(
  props: NavigatorItemDragAndDropWrapperProps,
): NavigatorItemDragAndDropWrapperProps {
  if (!props.selected) {
    props.editorDispatch(MetaActions.selectComponents([props.elementPath], false), 'leftpane')
  }
  return props
}

interface NavigatorItemDndWrapperProps {
  indexInParent: number
}

export const NavigatorItemDndWrapper = React.memo<
  NavigatorItemDragAndDropWrapperProps & CollectResults & NavigatorItemDndWrapperProps
>((props) => {
  const safeComponentId = EP.toVarSafeComponentId(props.elementPath)

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

  const margin =
    props.isOver &&
    moveToElementPath != null &&
    props.appropriateDropTargetHint?.type !== 'reparent'
      ? getHintPadding(moveToElementPath, props.visibleNavigatorTargets)
      : 0

  const shouldShowParentOutline =
    dropTargetHintType == null
      ? false
      : dropTargetHintType === 'reparent'
      ? props.isOver
      : moveToElementPath != null &&
        EP.pathsEqual(props.elementPath, EP.parentPath(moveToElementPath))

  return (
    <div
      key='navigatorItem'
      id={`navigator-item-${safeComponentId}`}
      data-testid={`navigator-item-${safeComponentId}`}
    >
      <NavigatorItem
        elementPath={props.elementPath}
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
      {when(
        props.indexInParent === 0,
        <NavigatorHintTop
          shouldBeShown={props.isOver && props.appropriateDropTargetHint?.type === 'before'}
          margin={margin}
        />,
      )}
      <NavigatorHintBottom
        shouldBeShown={props.isOver && props.appropriateDropTargetHint?.type === 'after'}
        margin={margin}
      />
    </div>
  )
})

interface DropCollectedProps {
  isOver: boolean
  canDrop: boolean
}

export const NavigatorItemContainer = React.memo((props: NavigatorItemDragAndDropWrapperProps) => {
  const metadata = useEditorState(Substores.metadata, (store) => store.editor.jsxMetadata, 'aaa')

  const indexInParent = React.useMemo(() => {
    const parentPath = EP.parentPath(props.elementPath)
    const pathString = EP.toString(props.elementPath)
    return MetadataUtils.getChildrenPaths(metadata, parentPath)
      .map((path) => EP.toString(path))
      .findIndex((path) => path === pathString)
  }, [metadata, props.elementPath])

  const editorStateRef = useRefEditorState((store) => store.editor)

  const [{ isDragging }, drag, preview] = useDrag(
    () => ({
      type: 'NAVIGATOR_ITEM',
      collect: (monitor) => ({
        isDragging: monitor.isDragging(),
      }),
      item: props,
      beginDrag: beginDrag,
      canDrag: (monitor) => {
        const editorState = editorStateRef.current
        const result = isAllowedToReparent(
          editorState.projectContents,
          editorState.jsxMetadata,
          props.elementPath,
        )
        return result
      },
    }),
    [props],
  )

  const dropRef = React.useRef<HTMLDivElement | null>(null)

  const [{ isOver }, drop] = useDrop<
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
        onHover(item, props, monitor, dropRef.current, indexInParent, props.visibleNavigatorTargets)
      },
      drop: (item: NavigatorItemDragAndDropWrapperProps, monitor) => {
        onDrop(item, props, monitor, dropRef.current)
      },
      canDrop: (item: NavigatorItemDragAndDropWrapperProps, monitor) => {
        const editorState = editorStateRef.current
        const isReparentTarget = item.appropriateDropTargetHint?.type === 'reparent'
        const childrenSupportedIfRequired =
          !isReparentTarget ||
          MetadataUtils.targetSupportsChildren(
            editorState.projectContents,
            editorState.jsxMetadata,
            props.elementPath,
          )
        const notSelectedItem = item.getDragSelections().every((selection) => {
          return canDrop(props, selection.elementPath)
        })
        return childrenSupportedIfRequired && notSelectedItem
      },
    }),
    [props],
  )

  const attachDrop = React.useCallback(
    (domElement: HTMLDivElement) => {
      drop(domElement)
      dropRef.current = domElement
    },
    [drop, dropRef],
  )

  const safeComponentId = EP.toVarSafeComponentId(props.elementPath)

  React.useEffect(() => {
    preview(getEmptyImage(), { captureDraggingState: true })
  })

  return (
    <div
      style={{
        ...props.windowStyle,
      }}
    >
      <div ref={drag} data-testid={`navigator-item-drag-${safeComponentId}`}>
        <div ref={attachDrop} data-testid={`navigator-item-drop-${safeComponentId}`}>
          <NavigatorItemDndWrapper
            {...props}
            isOver={isOver}
            isDragging={isDragging}
            indexInParent={indexInParent}
          />
        </div>
      </div>
    </div>
  )
})
