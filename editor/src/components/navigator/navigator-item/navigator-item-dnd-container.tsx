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
import { BasePaddingUnit, getElementPadding, NavigatorItem } from './navigator-item'
import { NavigatorHintBottom, NavigatorHintTop } from './navigator-item-components'
import { JSXElementName } from '../../../core/shared/element-template'
import { DropTargetHint, ElementWarnings } from '../../editor/store/editor-state'
import { useRefEditorState } from '../../../components/editor/store/store-hook'
import { isAllowedToReparent } from '../../canvas/canvas-strategies/strategies/reparent-helpers/reparent-helpers'
import { MetadataUtils } from '../../../core/model/element-metadata-utils'

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
  getMaximumDistance: (componentId: string, initialDistance: number) => number
  getSelectedViewsInRange: (index: number) => Array<ElementPath> // TODO remove me
  supportsChildren: boolean
  noOfChildren: number
  staticElementName: JSXElementName | null
  label: string
  isElementVisible: boolean
  renamingTarget: ElementPath | null
  elementWarnings: ElementWarnings
  windowStyle: React.CSSProperties
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
    const clearHintAction = showNavigatorDropTargetHint(null, null)
    const target = propsOfDropTargetItem.elementPath

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

function getHintPadding(elementPath: ElementPath): number {
  return getElementPadding(elementPath) + ExpansionArrowWidth + PreviewIconSize / 2
}

function isCursorInLeftAreaOfItem(x: number, elementPath: ElementPath) {
  return x < getHintPadding(elementPath)
}

function getTargetDepthFromMousePosition(x: number, elementPath: ElementPath) {
  return Math.floor((getHintPadding(elementPath) - x) / BasePaddingUnit)
}

function onHover(
  propsOfDraggedItem: NavigatorItemDragAndDropWrapperProps,
  propsOfDropTargetItem: NavigatorItemDragAndDropWrapperProps,
  monitor: DropTargetMonitor | null,
  component: HTMLDivElement | null,
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
    const targetAction = propsOfDraggedItem.highlighted
      ? []
      : [EditorActions.setHighlightedView(propsOfDraggedItem.elementPath)]
    const canReparent = propsOfDropTargetItem.supportsChildren
    const numberOfAreasToCut = canReparent ? 3 : 2

    if (cursor == null) {
      return
    }

    if (isCursorInTopArea(dropTargetRectangle, cursor.y, numberOfAreasToCut)) {
      if (propsOfDraggedItem.appropriateDropTargetHint?.type !== 'before') {
        propsOfDraggedItem.editorDispatch(
          [
            ...targetAction,
            showNavigatorDropTargetHint('before', propsOfDropTargetItem.elementPath),
          ],
          'leftpane',
        )
      }
    } else if (
      isCursorInBottomArea(dropTargetRectangle, cursor.y, numberOfAreasToCut) &&
      (propsOfDraggedItem.noOfChildren === 0 || propsOfDraggedItem.collapsed)
    ) {
      if (
        isCursorInLeftAreaOfItem(cursor.x, propsOfDraggedItem.elementPath) &&
        EP.parentPath(propsOfDraggedItem.elementPath) != null
      ) {
        const maximumTargetDepth = propsOfDraggedItem.getMaximumDistance(
          EP.toComponentId(propsOfDraggedItem.elementPath),
          0,
        )
        const cursorTargetDepth = getTargetDepthFromMousePosition(
          cursor.x,
          propsOfDraggedItem.elementPath,
        )
        const targetDistance = Math.min(cursorTargetDepth, maximumTargetDepth)
        const targetTP = EP.getNthParent(propsOfDraggedItem.elementPath, targetDistance)
        if (
          propsOfDraggedItem.appropriateDropTargetHint?.type !== 'after' ||
          !EP.pathsEqual(propsOfDraggedItem.appropriateDropTargetHint?.target, targetTP)
        ) {
          propsOfDraggedItem.editorDispatch(
            [...targetAction, showNavigatorDropTargetHint('after', targetTP)],
            'leftpane',
          )
        }
      } else if (
        propsOfDraggedItem.appropriateDropTargetHint?.type !== 'after' ||
        !EP.pathsEqual(
          propsOfDraggedItem.appropriateDropTargetHint?.target,
          propsOfDropTargetItem.elementPath,
        )
      ) {
        propsOfDraggedItem.editorDispatch(
          [
            ...targetAction,
            showNavigatorDropTargetHint('after', propsOfDropTargetItem.elementPath),
          ],
          'leftpane',
        )
      }
    } else if (canReparent) {
      if (propsOfDraggedItem.appropriateDropTargetHint?.type !== 'reparent') {
        propsOfDraggedItem.editorDispatch(
          [
            ...targetAction,
            showNavigatorDropTargetHint('reparent', propsOfDropTargetItem.elementPath),
          ],
          'leftpane',
        )
      }
    } else if (propsOfDraggedItem.appropriateDropTargetHint?.type !== null) {
      propsOfDraggedItem.editorDispatch([showNavigatorDropTargetHint(null, null)], 'leftpane')
    }
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

export class NavigatorItemDndWrapper extends PureComponent<
  NavigatorItemDragAndDropWrapperProps & CollectResults
> {
  constructor(props: NavigatorItemDragAndDropWrapperProps & CollectResults) {
    super(props)
  }

  getMarginForHint = (): number => {
    if (
      this.props.isOver &&
      this.props.appropriateDropTargetHint?.target != null &&
      this.props.appropriateDropTargetHint?.type !== 'reparent'
    ) {
      return getHintPadding(this.props.appropriateDropTargetHint.target)
    } else {
      return 0
    }
  }

  render(): React.ReactElement {
    const props = this.props
    const safeComponentId = EP.toVarSafeComponentId(this.props.elementPath)

    return (
      <div
        key='navigatorItem'
        id={`navigator-item-${safeComponentId}`}
        data-testid={`navigator-item-${safeComponentId}`}
        style={{
          ...props.windowStyle,
        }}
      >
        <NavigatorItem
          elementPath={this.props.elementPath}
          index={this.props.index}
          getSelectedViewsInRange={this.props.getSelectedViewsInRange}
          noOfChildren={this.props.noOfChildren}
          staticElementName={this.props.staticElementName}
          label={this.props.label}
          dispatch={this.props.editorDispatch}
          isHighlighted={this.props.highlighted}
          isElementVisible={this.props.isElementVisible}
          renamingTarget={this.props.renamingTarget}
          collapsed={this.props.collapsed}
          selected={this.props.selected}
          elementWarnings={this.props.elementWarnings}
        />
        <NavigatorHintTop
          shouldBeShown={
            this.props.isOver && this.props.appropriateDropTargetHint?.type === 'before'
          }
          getMarginForHint={this.getMarginForHint}
        />
        <NavigatorHintBottom
          shouldBeShown={
            this.props.isOver && this.props.appropriateDropTargetHint?.type === 'after'
          }
          getMarginForHint={this.getMarginForHint}
        />
      </div>
    )
  }
}

interface DropCollectedProps {
  isOver: boolean
  canDrop: boolean
}

export const NavigatorItemContainer = React.memo((props: NavigatorItemDragAndDropWrapperProps) => {
  const editorStateRef = useRefEditorState('fullOldStore')((store) => store.editor)
  const [{ isDragging }, drag] = useDrag(
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
        onHover(item, props, monitor, dropRef.current)
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

  return (
    <div ref={attachDrop} data-testid={`navigator-item-drop-${safeComponentId}`}>
      <div ref={drag} data-testid={`navigator-item-drag-${safeComponentId}`}>
        <NavigatorItemDndWrapper {...props} isOver={isOver} isDragging={isDragging} />
      </div>
    </div>
  )
})
