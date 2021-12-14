/** @jsx jsx */
import { jsx } from '@emotion/react'
import React from 'react'
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
  ResizeOptions,
} from '../../editor/store/editor-state'
import { ElementPath, NodeModules } from '../../../core/shared/project-file-types'
import { CanvasPositions, CSSCursor } from '../canvas-types'
import { SelectModeControlContainer } from './select-mode-control-container'
import { InsertModeControlContainer } from './insert-mode-control-container'
import { HighlightControl } from './highlight-control'
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
import { useColorTheme } from '../../../uuiui'
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
import { LayoutParentControl } from './layout-parent-control'
import { when } from '../../../utils/react-conditionals'
import { isFeatureEnabled } from '../../../utils/feature-switches'
import { shallowEqual } from '../../../core/shared/equality-utils'
import { KeysPressed } from '../../../utils/keyboard'
import { usePrevious } from '../../editor/hook-utils'
import { LayoutTargetableProp } from '../../../core/layout/layout-helpers-new'
import { getDragStateStart } from '../canvas-utils'

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
  resizeOptions: ResizeOptions
}

interface NewCanvasControlsProps {
  windowToCanvasPosition: (event: MouseEvent) => CanvasPositions
  cursor: CSSCursor
}

export const NewCanvasControls = React.memo((props: NewCanvasControlsProps) => {
  const canvasControlProps = useEditorState(
    (store) => ({
      dispatch: store.dispatch,
      editor: store.editor,
      derived: store.derived,
      canvasOffset: store.editor.canvas.roundedCanvasOffset,
      animationEnabled:
        (store.editor.canvas.dragState == null ||
          getDragStateStart(store.editor.canvas.dragState, store.editor.canvas.resizeOptions) ==
            null) &&
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
})
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
  const colorTheme = useColorTheme()
  const startDragStateAfterDragExceedsThreshold = useStartDragStateAfterDragExceedsThreshold()

  const { localSelectedViews, localHighlightedViews, setLocalSelectedViews } = props
  const cmdKeyPressed = props.editor.keysPressed['cmd'] ?? false

  const componentMetadata = getMetadata(props.editor)

  const resizing = isResizing(props.editor)
  const dragging = isDragging(props.editor)
  const selectionEnabled = pickSelectionEnabled(props.editor.canvas, props.editor.keysPressed)
  const draggingEnabled = !isSelectLiteMode(props.editor.mode)
  const contextMenuEnabled = !isLiveMode(props.editor.mode)

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
    const resolveFn = props.editor.codeResultCache.curriedResolveFn(props.editor.projectContents)
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
      resolve: resolveFn,
      resizeOptions: props.editor.canvas.resizeOptions,
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
      <LayoutParentControl />
    </div>
  )
}
