/** @jsx jsx */
import { jsx } from '@emotion/core'
import * as React from 'react'
import { PureComponent } from 'react'
import { DragSource, DropTarget, DropTargetMonitor } from 'react-dnd'
import * as ReactDOM from 'react-dom'
import { TemplatePath, ElementOriginType, Imports } from '../../../core/shared/project-file-types'
import { EditorDispatch } from '../../editor/action-types'
import * as EditorActions from '../../editor/actions/actions'
import * as TP from '../../../core/shared/template-path'
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
import { ElementInstanceMetadata } from '../../../core/shared/element-template'

const BaseRowHeight = 35
const PreviewIconSize = BaseRowHeight

export interface DragSelection {
  templatePath: TemplatePath
  index: number
}

export interface NavigatorItemDragAndDropWrapperProps {
  index: number
  templatePath: TemplatePath
  dropTargetHint: DropTargetHint
  editorDispatch: EditorDispatch
  ancestorCollapsed: boolean
  selected: boolean
  highlighted: boolean // TODO are we sure about this?
  collapsed: boolean // TODO are we sure about this?
  getDragSelections: () => Array<DragSelection>
  getMaximumDistance: (componentId: string, initialDistance: number) => number
  getSelectedViewsInRange: (index: number) => Array<TemplatePath> // TODO remove me
  supportsChildren: boolean
  noOfChildren: number
  element: ElementInstanceMetadata | null
  elementOriginType: ElementOriginType
  name: string
  isAutosizingView: boolean
  isElementVisible: boolean
  renamingTarget: TemplatePath | null
  imports: Imports
}

function canDrop(props: NavigatorItemDragAndDropWrapperProps, dropSource: TemplatePath): boolean {
  return (
    TP.pathsEqual(props.templatePath, dropSource) ||
    !TP.isAncestorOf(props.templatePath, dropSource)
  )
}

function onDrop(
  props: NavigatorItemDragAndDropWrapperProps,
  monitor?: DropTargetMonitor,
  component?: {},
) {
  if (monitor != null && component != null) {
    const dragSelections = props.getDragSelections()
    const filteredSelections = dragSelections.filter((selection) =>
      canDrop(props, selection.templatePath),
    )
    const draggedElements = filteredSelections.map((selection) => selection.templatePath)
    const clearHintAction = showNavigatorDropTargetHint(null, null)
    const target =
      props.dropTargetHint.target != null ? props.dropTargetHint.target : props.templatePath

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

function getHintPadding(templatePath: TemplatePath): number {
  return getElementPadding(templatePath) + ExpansionArrowWidth + PreviewIconSize / 2
}

function isCursorInLeftAreaOfItem(x: number, templatePath: TemplatePath) {
  return x < getHintPadding(templatePath)
}

function getTargetDepthFromMousePosition(x: number, templatePath: TemplatePath) {
  return Math.floor((getHintPadding(templatePath) - x) / BasePaddingUnit)
}

function onHover(
  props: NavigatorItemDragAndDropWrapperProps,
  monitor?: DropTargetMonitor,
  component?: React.Component<NavigatorItemDragAndDropWrapperProps, any>,
): void {
  if (
    monitor != null &&
    component != null &&
    props.getDragSelections().some((selection) => canDrop(props, selection.templatePath))
  ) {
    const domNode = ReactDOM.findDOMNode(component)
    if (domNode == null || typeof domNode === 'string') {
      return
    }
    const dropTargetRectangle = (domNode as Element).getBoundingClientRect()
    const cursor = monitor.getClientOffset()
    const targetAction = props.highlighted
      ? []
      : [EditorActions.setHighlightedView(props.templatePath)]
    const canReparent = props
      .getDragSelections()
      .every(
        (dragSelectedItem: DragSelection) =>
          !TP.pathsEqual(props.templatePath, dragSelectedItem.templatePath) &&
          props.supportsChildren,
      )
    const numberOfAreasToCut = canReparent ? 3 : 2

    if (cursor == null) {
      return
    }

    if (isCursorInTopArea(dropTargetRectangle, cursor.y, numberOfAreasToCut)) {
      if (props.dropTargetHint.type !== 'after') {
        props.editorDispatch(
          [...targetAction, showNavigatorDropTargetHint('after', component.props.templatePath)],
          'leftpane',
        )
      }
    } else if (
      isCursorInBottomArea(dropTargetRectangle, cursor.y, numberOfAreasToCut) &&
      (props.noOfChildren === 0 || props.collapsed)
    ) {
      if (
        isCursorInLeftAreaOfItem(cursor.x, props.templatePath) &&
        TP.parentPath(props.templatePath) != null
      ) {
        const maximumTargetDepth = props.getMaximumDistance(TP.toComponentId(props.templatePath), 0)
        const cursorTargetDepth = getTargetDepthFromMousePosition(cursor.x, props.templatePath)
        const targetDistance = Math.min(cursorTargetDepth, maximumTargetDepth)
        const targetTP = TP.getNthParent(props.templatePath, targetDistance)
        if (
          props.dropTargetHint.type !== 'before' ||
          !TP.pathsEqual(props.dropTargetHint.target, targetTP)
        ) {
          props.editorDispatch(
            [...targetAction, showNavigatorDropTargetHint('before', targetTP)],
            'leftpane',
          )
        }
      } else if (
        props.dropTargetHint.type !== 'before' ||
        !TP.pathsEqual(props.dropTargetHint.target, component.props.templatePath)
      ) {
        props.editorDispatch(
          [...targetAction, showNavigatorDropTargetHint('before', component.props.templatePath)],
          'leftpane',
        )
      }
    } else if (canReparent) {
      if (props.dropTargetHint.type !== 'reparent') {
        props.editorDispatch(
          [...targetAction, showNavigatorDropTargetHint('reparent', component.props.templatePath)],
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
    props.editorDispatch([EditorActions.selectComponents([props.templatePath], false)], 'leftpane')
  }
  return props
}

export class NavigatorItemDndWrapper extends PureComponent<
  NavigatorItemDragAndDropWrapperProps & CollectResults,
  {}
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

  render() {
    const props = this.props

    const navigatorItemContainer = (
      <div
        key='navigatorItem'
        style={{
          position: 'relative',
          width: '100%',
          display: props.ancestorCollapsed ? 'none' : 'initial',
        }}
      >
        <NavigatorItem
          templatePath={this.props.templatePath}
          index={this.props.index}
          getSelectedViewsInRange={this.props.getSelectedViewsInRange}
          noOfChildren={this.props.noOfChildren}
          isAutosizingView={this.props.isAutosizingView}
          name={this.props.name}
          element={this.props.element}
          dispatch={this.props.editorDispatch}
          isHighlighted={this.props.highlighted}
          isElementVisible={this.props.isElementVisible}
          renamingTarget={this.props.renamingTarget}
          collapsed={this.props.collapsed}
          selected={this.props.selected}
          imports={this.props.imports}
          elementOriginType={this.props.elementOriginType}
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
