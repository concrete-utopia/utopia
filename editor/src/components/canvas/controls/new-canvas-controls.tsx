/** @jsx jsx */
import { jsx } from '@emotion/react'
import * as React from 'react'
import * as EP from '../../../core/shared/element-path'
import Utils from '../../../utils/utils'
import { CanvasPoint } from '../../../core/shared/math-utils'
import { EditorDispatch } from '../../editor/action-types'
import {
  DerivedState,
  EditorState,
  getMetadata,
  EditorStore,
  getOpenUIJSFileKey,
  TransientCanvasState,
  TransientFilesState,
} from '../../editor/store/editor-state'
import { ElementPath, NodeModules } from '../../../core/shared/project-file-types'
import { CanvasPositions, CSSCursor } from '../canvas-types'
import { SelectModeControlContainer } from './select-mode-control-container'
import { InsertModeControlContainer } from './insert-mode-control-container'
import { HighlightControl } from './highlight-control'
import { TextEditor } from '../../editor/text-editor'
import { useEditorState, useRefEditorState } from '../../editor/store/store-hook'
import {
  ElementInstanceMetadataMap,
  UtopiaJSXComponent,
} from '../../../core/shared/element-template'
import { MetadataUtils } from '../../../core/model/element-metadata-utils'
import { isAspectRatioLockedNew } from '../../aspect-ratio'
import { ElementContextMenu } from '../../element-context-menu'
import { isLiveMode, EditorModes, isSelectLiteMode } from '../../editor/editor-modes'
import { DropTargetHookSpec, ConnectableElement, useDrop } from 'react-dnd'
import { FileBrowserItemProps } from '../../filebrowser/fileitem'
import { forceNotNull } from '../../../core/shared/optional-utils'
import { flatMapArray } from '../../../core/shared/array-utils'
import { targetRespectsLayout } from '../../../core/layout/layout-helpers'
import { createSelector } from 'reselect'
import { PropertyControlsInfo, ResolveFn } from '../../custom-code/code-file'
import { colorTheme } from '../../../uuiui'
import { betterReactMemo } from '../../../uuiui-deps'
import {
  isDragging,
  isResizing,
  pickSelectionEnabled,
  useMaybeHighlightElement,
  useSelectAndHover,
  useStartDragStateAfterDragExceedsThreshold,
} from './select-mode/select-mode-hooks'
import { NO_OP } from '../../../core/shared/utils'
import { usePropControlledStateV2 } from '../../inspector/common/inspector-utils'
import { ProjectContentTreeRoot } from '../../assets'

export const CanvasControlsContainerID = 'new-canvas-controls-container'

export type ResizeStatus = 'disabled' | 'noninteractive' | 'enabled'

function useLocalSelectedHighlightedViews(
  transientCanvasState: TransientCanvasState,
): {
  localSelectedViews: ElementPath[]
  localHighlightedViews: ElementPath[]
  setSelectedViewsLocally: (newSelectedViews: Array<ElementPath>) => void
} {
  const [localSelectedViews, setLocalSelectedViews] = usePropControlledStateV2(
    transientCanvasState.selectedViews,
  )
  const [localHighlightedViews, setLocalHighlightedViews] = usePropControlledStateV2(
    transientCanvasState.highlightedViews,
  )

  const setSelectedViewsLocally = React.useCallback(
    (newSelectedViews: Array<ElementPath>) => {
      setLocalSelectedViews(newSelectedViews)
      setLocalHighlightedViews([])
    },
    [setLocalSelectedViews, setLocalHighlightedViews],
  )
  return { localSelectedViews, localHighlightedViews, setSelectedViewsLocally }
}

export interface ControlProps {
  selectedViews: Array<ElementPath>
  highlightedViews: Array<ElementPath>
  componentMetadata: ElementInstanceMetadataMap
  projectContents: ProjectContentTreeRoot
  nodeModules: NodeModules
  openFile: string | null
  hiddenInstances: Array<ElementPath>
  focusedElementPath: ElementPath | null
  highlightsEnabled: boolean
  canvasOffset: CanvasPoint
  scale: number
  dispatch: EditorDispatch
  resizeStatus: ResizeStatus
  elementAspectRatioLocked: boolean
  imageMultiplier: number | null
  windowToCanvasPosition: (event: MouseEvent) => CanvasPositions
  cmdKeyPressed: boolean
  showAdditionalControls: boolean
  maybeClearHighlightsOnHoverEnd: () => void
  transientState: TransientCanvasState
  resolve: ResolveFn
}

interface NewCanvasControlsProps {
  windowToCanvasPosition: (event: MouseEvent) => CanvasPositions
  cursor: CSSCursor
}

export const NewCanvasControls = betterReactMemo(
  'NewCanvasControls',
  (props: NewCanvasControlsProps) => {
    const canvasControlProps = useEditorState(
      (store) => ({
        dispatch: store.dispatch,
        editor: store.editor,
        derived: store.derived,
        canvasOffset: store.editor.canvas.roundedCanvasOffset,
        animationEnabled:
          (store.editor.canvas.dragState == null || store.editor.canvas.dragState.start == null) &&
          store.editor.canvas.animationsEnabled,

        controls: store.derived.canvas.controls,
        scale: store.editor.canvas.scale,
        focusedPanel: store.editor.focusedPanel,
        transientCanvasState: store.derived.canvas.transientState,
      }),
      'NewCanvasControls',
    )

    const {
      localSelectedViews,
      localHighlightedViews,
      setSelectedViewsLocally,
    } = useLocalSelectedHighlightedViews(canvasControlProps.transientCanvasState)

    const canvasScrollAnimation = useEditorState(
      (store) => store.editor.canvas.scrollAnimation,
      'NewCanvasControls scrollAnimation',
    )

    // Somehow this being setup and hooked into the div makes the `onDrop` call
    // work properly in `editor-canvas.ts`. I blame React DnD for this.
    const dropSpec: DropTargetHookSpec<FileBrowserItemProps, 'CANVAS', unknown> = {
      accept: 'filebrowser',
      canDrop: () => true,
    }

    const [_, drop] = useDrop(dropSpec)

    const forwardedRef = React.useCallback(
      (node: ConnectableElement) => {
        return drop(node)
      },
      [drop],
    )

    if (isLiveMode(canvasControlProps.editor.mode) && !canvasControlProps.editor.keysPressed.cmd) {
      return null
    } else {
      return (
        <div
          key='canvas-controls'
          ref={forwardedRef}
          className={
            canvasControlProps.focusedPanel === 'canvas'
              ? '  canvas-controls focused '
              : ' canvas-controls '
          }
          id='canvas-controls'
          style={{
            pointerEvents: 'initial',
            position: 'absolute',
            top: 0,
            left: 0,
            transform: 'translate3d(0, 0, 0)',
            width: `100%`,
            height: `100%`,
            zoom: canvasControlProps.scale >= 1 ? `${canvasControlProps.scale * 100}%` : 1,
            cursor: props.cursor,
            visibility: canvasScrollAnimation ? 'hidden' : 'initial',
          }}
        >
          <div
            style={{
              position: 'absolute',
              top: 0,
              left: 0,
              width: `${canvasControlProps.scale < 1 ? 100 / canvasControlProps.scale : 100}%`,
              height: `${canvasControlProps.scale < 1 ? 100 / canvasControlProps.scale : 100}%`,
              transformOrigin: 'top left',
              transform: canvasControlProps.scale < 1 ? `scale(${canvasControlProps.scale}) ` : '',
            }}
          >
            <NewCanvasControlsInner
              windowToCanvasPosition={props.windowToCanvasPosition}
              localSelectedViews={localSelectedViews}
              localHighlightedViews={localHighlightedViews}
              setLocalSelectedViews={setSelectedViewsLocally}
              {...canvasControlProps}
            />
          </div>
          <ElementContextMenu contextMenuInstance='context-menu-canvas' />
        </div>
      )
    }
  },
)
NewCanvasControls.displayName = 'NewCanvasControls'

interface NewCanvasControlsInnerProps {
  editor: EditorState
  derived: DerivedState
  dispatch: EditorDispatch
  canvasOffset: CanvasPoint
  animationEnabled: boolean
  windowToCanvasPosition: (event: MouseEvent) => CanvasPositions
  localSelectedViews: Array<ElementPath>
  localHighlightedViews: Array<ElementPath>
  setLocalSelectedViews: (newSelectedViews: ElementPath[]) => void
}

const NewCanvasControlsInner = (props: NewCanvasControlsInnerProps) => {
  const startDragStateAfterDragExceedsThreshold = useStartDragStateAfterDragExceedsThreshold()

  const { localSelectedViews, localHighlightedViews, setLocalSelectedViews } = props
  const cmdKeyPressed = props.editor.keysPressed['cmd'] ?? false

  const componentMetadata = getMetadata(props.editor)

  const resizing = isResizing(props.editor.canvas.dragState)
  const dragging = isDragging(props.editor.canvas.dragState)
  const selectionEnabled = pickSelectionEnabled(props.editor.canvas, props.editor.keysPressed)
  const draggingEnabled = !isSelectLiteMode(props.editor.mode)
  const contextMenuEnabled = !isSelectLiteMode(props.editor.mode)

  const { maybeHighlightOnHover, maybeClearHighlightsOnHoverEnd } = useMaybeHighlightElement()

  const { onMouseMove, onMouseDown } = useSelectAndHover(cmdKeyPressed, setLocalSelectedViews)

  const getResizeStatus = () => {
    const selectedViews = localSelectedViews
    if (props.editor.canvas.textEditor != null || props.editor.keysPressed['z']) {
      return 'disabled'
    }
    if (cmdKeyPressed) {
      return 'noninteractive'
    }
    const anyIncomprehensibleElementsSelected = selectedViews.some((selectedView) => {
      const possibleMetadata = MetadataUtils.findElementByElementPath(
        componentMetadata,
        selectedView,
      )
      return possibleMetadata == null
    })
    if (anyIncomprehensibleElementsSelected) {
      return 'disabled'
    }
    return 'enabled'
  }

  const renderModeControlContainer = () => {
    const elementAspectRatioLocked = localSelectedViews.every((target) => {
      const possibleElement = MetadataUtils.findElementByElementPath(componentMetadata, target)
      if (possibleElement == null) {
        return false
      } else {
        return isAspectRatioLockedNew(possibleElement)
      }
    })
    const imageMultiplier: number | null = MetadataUtils.getImageMultiplier(
      componentMetadata,
      localSelectedViews,
    )
    const controlProps: ControlProps = {
      selectedViews: localSelectedViews,
      highlightedViews: localHighlightedViews,
      componentMetadata: componentMetadata,
      hiddenInstances: props.editor.hiddenInstances,
      focusedElementPath: props.editor.focusedElementPath,
      highlightsEnabled: props.editor.canvas.highlightsEnabled,
      canvasOffset: props.canvasOffset,
      scale: props.editor.canvas.scale,
      dispatch: props.dispatch,
      resizeStatus: getResizeStatus(),
      elementAspectRatioLocked: elementAspectRatioLocked,
      imageMultiplier: imageMultiplier,
      windowToCanvasPosition: props.windowToCanvasPosition,
      cmdKeyPressed: cmdKeyPressed,
      showAdditionalControls: props.editor.interfaceDesigner.additionalControls,
      maybeClearHighlightsOnHoverEnd: maybeClearHighlightsOnHoverEnd,
      projectContents: props.editor.projectContents,
      nodeModules: props.editor.nodeModules.files,
      openFile: props.editor.canvas.openFile?.filename ?? null,
      transientState: props.derived.canvas.transientState,
      resolve: props.editor.codeResultCache.resolve,
    }
    const dragState = props.editor.canvas.dragState
    switch (props.editor.mode.type) {
      case 'select':
      case 'select-lite':
      case 'live': {
        return (
          <SelectModeControlContainer
            {...controlProps}
            startDragStateAfterDragExceedsThreshold={startDragStateAfterDragExceedsThreshold}
            setSelectedViewsLocally={setLocalSelectedViews}
            keysPressed={props.editor.keysPressed}
            windowToCanvasPosition={props.windowToCanvasPosition}
            isDragging={dragging}
            isResizing={resizing}
            selectionEnabled={selectionEnabled}
            draggingEnabled={draggingEnabled}
            contextMenuEnabled={contextMenuEnabled}
            maybeHighlightOnHover={maybeHighlightOnHover}
            maybeClearHighlightsOnHoverEnd={maybeClearHighlightsOnHoverEnd}
            duplicationState={props.editor.canvas.duplicationState}
            dragState={
              dragState?.type === 'MOVE_DRAG_STATE' || dragState?.type === 'RESIZE_DRAG_STATE'
                ? dragState
                : null
            }
            showAdditionalControls={props.editor.interfaceDesigner.additionalControls}
          />
        )
      }
      case 'insert': {
        return (
          <InsertModeControlContainer
            {...controlProps}
            mode={props.editor.mode}
            keysPressed={props.editor.keysPressed}
            windowToCanvasPosition={props.windowToCanvasPosition}
            dragState={
              dragState != null && dragState.type === 'INSERT_DRAG_STATE' ? dragState : null
            }
            canvasOffset={props.editor.canvas.realCanvasOffset /* maybe roundedCanvasOffset? */}
            scale={props.editor.canvas.scale}
          />
        )
      }
      default: {
        const _exhaustiveCheck: never = props.editor.mode
        throw new Error(`Unhandled editor mode ${JSON.stringify(props.editor.mode)}`)
      }
    }
  }

  const renderHighlightControls = () => {
    return selectionEnabled
      ? localHighlightedViews.map((path) => {
          const frame = MetadataUtils.getFrameInCanvasCoords(path, componentMetadata)
          if (frame == null) {
            return null
          }
          const isFocusableComponent = MetadataUtils.isFocusableComponent(path, componentMetadata)
          const isFocusedComponent = EP.isFocused(props.editor.focusedElementPath, path)
          const color =
            isFocusableComponent || isFocusedComponent
              ? colorTheme.canvasSelectionIsolatedComponent.value
              : colorTheme.canvasSelectionPrimaryOutline.value
          return (
            <HighlightControl
              key={`highlight-control-${EP.toComponentId(path)}`}
              color={color}
              frame={frame}
              scale={props.editor.canvas.scale}
              canvasOffset={props.canvasOffset}
            />
          )
        })
      : []
  }

  const renderTextEditor = (target: ElementPath) => {
    const dragState = props.editor.canvas.dragState
    const selectedViews = localSelectedViews
    if (dragState != null || selectedViews.length !== 1) {
      return null
    } else {
      const element = MetadataUtils.findElementByElementPath(componentMetadata, target)
      const canAnimate =
        MetadataUtils.isParentYogaLayoutedContainerAndElementParticipatesInLayout(
          target,
          componentMetadata,
        ) && props.animationEnabled
      const frame = MetadataUtils.getFrameInCanvasCoords(target, componentMetadata)

      if (frame == null || element == null) {
        // If we have no frame at all we can't do anything, so fail to open the text editor
        return null
      }

      const offset = Utils.scaleVector(
        Utils.offsetPoint(props.editor.canvas.roundedCanvasOffset, frame),
        props.editor.canvas.scale,
      )

      const textStyle =
        element.computedStyle?.textSizing == 'auto'
          ? {
              ...element.computedStyle,
              top: 0,
              left: 0,
              visibility: 'visible',
            }
          : {
              ...element.computedStyle,
              top: 0,
              left: 0,
              width: frame.width,
              height: frame.height,
              visibility: 'visible',
            }

      return (
        <TextEditor
          key={'text-editor'}
          target={target}
          triggerMousePosition={
            props.editor.canvas.textEditor != null
              ? props.editor.canvas.textEditor.triggerMousePosition
              : null
          }
          dispatch={props.dispatch}
          text={element.props.text}
          style={textStyle}
          css={element.props.css}
          className={canAnimate ? 'yoga-element-transition' : ''}
          rawTextStyle={element.props.textstyle}
          textSizing={element.props.textSizing}
          scale={props.editor.canvas.scale}
          deleteWhenEmpty={true}
          offset={offset}
        />
      )
    }
  }

  const textEditor =
    props.editor.canvas.textEditor != null
      ? renderTextEditor(props.editor.canvas.textEditor.elementPath)
      : null

  return (
    <div
      id={CanvasControlsContainerID}
      data-testid={CanvasControlsContainerID}
      className='new-canvas-controls-container'
      style={{
        pointerEvents: 'initial',
        position: 'relative',
        width: '100%',
        height: '100%',
      }}
      onMouseDown={onMouseDown}
      onMouseMove={onMouseMove}
    >
      {renderModeControlContainer()}
      {renderHighlightControls()}
      {textEditor}
    </div>
  )
}
