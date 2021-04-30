/** @jsx jsx */
import { jsx } from '@emotion/react'
import * as React from 'react'
import { PureComponent } from 'react'
import { DragSource, DropTarget, DropTargetMonitor } from 'react-dnd'
import * as ReactDOM from 'react-dom'
import { ElementPath, ElementOriginType, Imports } from '../../../core/shared/project-file-types'
import { EditorDispatch } from '../../editor/action-types'
import * as EditorActions from '../../editor/actions/action-creators'
import * as EP from '../../../core/shared/element-path'
import {
  placeComponentsAfter,
  placeComponentsBefore,
  reparentComponents,
  showNavigatorDropTargetHint,
} from '../actions'
import {
  cardSource,
  CollectResults,
  connectDragSource,
  connectDropTarget,
  isCursorInBottomArea,
  isCursorInTopArea,
  onDragActions,
} from '../drag-and-drop-utils'
import { DropTargetHint } from '../navigator'
import { ExpansionArrowWidth } from './expandable-indicator'
import { BasePaddingUnit, getElementPadding, NavigatorItem } from './navigator-item'
import { NavigatorHintBottom, NavigatorHintTop } from './navigator-item-components'
import { JSXElementName } from '../../../core/shared/element-template'
import { ElementWarnings } from '../../editor/store/editor-state'

const BaseRowHeight = 35
const PreviewIconSize = BaseRowHeight

export interface DragSelection {
  elementPath: ElementPath
  index: number
}

export interface NavigatorItemDragAndDropWrapperProps {
  index: number
  elementPath: ElementPath
  dropTargetHint: DropTargetHint
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
  elementOriginType: ElementOriginType
  isElementVisible: boolean
  renamingTarget: ElementPath | null
  elementWarnings: ElementWarnings
  windowStyle: React.CSSProperties
}

function canDrop(props: NavigatorItemDragAndDropWrapperProps, dropSource: ElementPath): boolean {
  return (
    EP.pathsEqual(props.elementPath, dropSource) ||
    !EP.isDescendantOfOrEqualTo(props.elementPath, dropSource)
  )
}

function onDrop(
  props: NavigatorItemDragAndDropWrapperProps,
  monitor?: DropTargetMonitor,
  component?: unknown,
) {
  if (monitor != null && component != null) {
    const dragSelections = props.getDragSelections()
    const filteredSelections = dragSelections.filter((selection) =>
      canDrop(props, selection.elementPath),
    )
    const draggedElements = filteredSelections.map((selection) => selection.elementPath)
    const clearHintAction = showNavigatorDropTargetHint(null, null)
    const target =
      props.dropTargetHint.target != null ? props.dropTargetHint.target : props.elementPath

    switch (props.dropTargetHint.type) {
      case 'before':
        return props.editorDispatch(
          [placeComponentsBefore(draggedElements, target), clearHintAction],
          'leftpane',
        )
      case 'after':
        return props.editorDispatch(
          [placeComponentsAfter(draggedElements, target), clearHintAction],
          'leftpane',
        )
      case 'reparent':
        return props.editorDispatch(
          [reparentComponents(draggedElements, target), clearHintAction],
          'leftpane',
        )
      default:
        return props.editorDispatch([clearHintAction], 'leftpane')
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
  props: NavigatorItemDragAndDropWrapperProps,
  monitor?: DropTargetMonitor,
  component?: React.Component<NavigatorItemDragAndDropWrapperProps, any>,
): void {
  if (
    monitor != null &&
    component != null &&
    props.getDragSelections().some((selection) => canDrop(props, selection.elementPath))
  ) {
    const domNode = ReactDOM.findDOMNode(component)
    if (domNode == null || typeof domNode === 'string') {
      return
    }
    const dropTargetRectangle = (domNode as HTMLElement).getBoundingClientRect()
    const cursor = monitor.getClientOffset()
    const targetAction = props.highlighted
      ? []
      : [EditorActions.setHighlightedView(props.elementPath)]
    const canReparent = props
      .getDragSelections()
      .every(
        (dragSelectedItem: DragSelection) =>
          !EP.pathsEqual(props.elementPath, dragSelectedItem.elementPath) && props.supportsChildren,
      )
    const numberOfAreasToCut = canReparent ? 3 : 2

    if (cursor == null) {
      return
    }

    if (isCursorInTopArea(dropTargetRectangle, cursor.y, numberOfAreasToCut)) {
      if (props.dropTargetHint.type !== 'after') {
        props.editorDispatch(
          [...targetAction, showNavigatorDropTargetHint('after', component.props.elementPath)],
          'leftpane',
        )
      }
    } else if (
      isCursorInBottomArea(dropTargetRectangle, cursor.y, numberOfAreasToCut) &&
      (props.noOfChildren === 0 || props.collapsed)
    ) {
      if (
        isCursorInLeftAreaOfItem(cursor.x, props.elementPath) &&
        EP.parentPath(props.elementPath) != null
      ) {
        const maximumTargetDepth = props.getMaximumDistance(EP.toComponentId(props.elementPath), 0)
        const cursorTargetDepth = getTargetDepthFromMousePosition(cursor.x, props.elementPath)
        const targetDistance = Math.min(cursorTargetDepth, maximumTargetDepth)
        const targetTP = EP.getNthParent(props.elementPath, targetDistance)
        if (
          props.dropTargetHint.type !== 'before' ||
          !EP.pathsEqual(props.dropTargetHint.target, targetTP)
        ) {
          props.editorDispatch(
            [...targetAction, showNavigatorDropTargetHint('before', targetTP)],
            'leftpane',
          )
        }
      } else if (
        props.dropTargetHint.type !== 'before' ||
        !EP.pathsEqual(props.dropTargetHint.target, component.props.elementPath)
      ) {
        props.editorDispatch(
          [...targetAction, showNavigatorDropTargetHint('before', component.props.elementPath)],
          'leftpane',
        )
      }
    } else if (canReparent) {
      if (props.dropTargetHint.type !== 'reparent') {
        props.editorDispatch(
          [...targetAction, showNavigatorDropTargetHint('reparent', component.props.elementPath)],
          'leftpane',
        )
      }
    } else if (props.dropTargetHint.type !== null) {
      props.editorDispatch([showNavigatorDropTargetHint(null, null)], 'leftpane')
    }
  }
}

function clearDropTargetHint(props: NavigatorItemDragAndDropWrapperProps) {
  props.editorDispatch([showNavigatorDropTargetHint(null, null)], 'leftpane')
}

function beginDrag(
  props: NavigatorItemDragAndDropWrapperProps,
): NavigatorItemDragAndDropWrapperProps {
  if (!props.selected) {
    props.editorDispatch([EditorActions.selectComponents([props.elementPath], false)], 'leftpane')
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
      this.props.dropTargetHint.target != null &&
      this.props.dropTargetHint.type !== 'reparent'
    ) {
      return getHintPadding(this.props.dropTargetHint.target)
    } else {
      return 0
    }
  }

  render(): React.ReactElement {
    const props = this.props

    const navigatorItemContainer = (
      <div
        key='navigatorItem'
        id={`navigator-item-${EP.toVarSafeComponentId(this.props.elementPath)}`}
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
          elementOriginType={this.props.elementOriginType}
          elementWarnings={this.props.elementWarnings}
        />
        <NavigatorHintTop
          isOver={this.props.isOver}
          dropTargetType={this.props.dropTargetHint.type}
          getMarginForHint={this.getMarginForHint}
        />
        <NavigatorHintBottom
          isOver={this.props.isOver}
          dropTargetType={this.props.dropTargetHint.type}
          getMarginForHint={this.getMarginForHint}
        />
      </div>
    )

    return props.connectDropTarget(props.connectDragSource(navigatorItemContainer))
  }
}

export const NavigatorItemContainer = DropTarget(
  'navigator-item',
  onDragActions(onDrop, onHover),
  connectDropTarget,
)(
  DragSource<NavigatorItemDragAndDropWrapperProps>(
    'navigator-item',
    cardSource(beginDrag, clearDropTargetHint),
    connectDragSource,
  )(
    NavigatorItemDndWrapper as any, // TODO DragSource type is shit
  ),
)
