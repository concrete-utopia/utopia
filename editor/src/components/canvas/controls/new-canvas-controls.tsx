/* eslint-disable @typescript-eslint/ban-types */
/** @jsxRuntime classic */
/** @jsx jsx */
/** @jsxFrag React.Fragment */
import { jsx } from '@emotion/react'
import React from 'react'
import * as EP from '../../../core/shared/element-path'
import { CanvasPoint } from '../../../core/shared/math-utils'
import { EditorDispatch } from '../../editor/action-types'
import {
  EditorState,
  getMetadata,
  TransientCanvasState,
  ResizeOptions,
  AllElementProps,
} from '../../editor/store/editor-state'
import { ElementPath, NodeModules } from '../../../core/shared/project-file-types'
import { CanvasPositions, CSSCursor } from '../canvas-types'
import { HighlightControl } from './highlight-control'
import { useEditorState } from '../../editor/store/store-hook'
import { ElementInstanceMetadataMap } from '../../../core/shared/element-template'
import { MetadataUtils } from '../../../core/model/element-metadata-utils'
import { isAspectRatioLockedNew } from '../../aspect-ratio'
import { ElementContextMenu } from '../../element-context-menu'
import {
  isLiveMode,
  isSelectMode,
  isTextEditMode,
  isTextEditModeWithActiveTextEditing,
  Mode,
} from '../../editor/editor-modes'
import { DropTargetHookSpec, ConnectableElement, useDrop, DndProvider } from 'react-dnd'
import { FileBrowserItemProps } from '../../filebrowser/fileitem'
import { ResolveFn } from '../../custom-code/code-file'
import { useColorTheme } from '../../../uuiui'
import {
  isDragging,
  isResizing,
  pickSelectionEnabled,
  useMaybeHighlightElement,
  useSelectAndHover,
  useStartDragStateAfterDragExceedsThreshold,
} from './select-mode/select-mode-hooks'
import { usePropControlledStateV2 } from '../../inspector/common/inspector-utils'
import { ProjectContentTreeRoot } from '../../assets'
import { LayoutParentControl } from './layout-parent-control'
import { unless, when } from '../../../utils/react-conditionals'
import { isFeatureEnabled } from '../../../utils/feature-switches'
import { useGetApplicableStrategyControls } from '../canvas-strategies/canvas-strategies'
import { MultiSelectOutlineControl } from './select-mode/simple-outline-control'
import { GuidelineControls } from './guideline-controls'
import { showContextMenu } from '../../editor/actions/action-creators'
import { InsertionControls } from './insertion-plus-button'
import { DistanceGuidelineControl } from './select-mode/distance-guideline-control'
import { SceneLabelControl } from './select-mode/scene-label'
import { PinLines } from './position-outline'
import { CursorComponent } from './select-mode/cursor-component'
import { ControlForStrategy, ControlWithProps } from '../canvas-strategies/canvas-strategy-types'
import { useKeepShallowReferenceEquality } from '../../../utils/react-performance'
import { shallowEqual } from '../../../core/shared/equality-utils'
import { ZeroSizedElementControls } from './zero-sized-element-controls'
import { DRAW_TO_INSERT_TEXT_STRATEGY_ID } from '../canvas-strategies/strategies/draw-to-insert-text-strategy'
import { TextEditableControl } from './text-editable-control'
import { useDispatch } from '../../editor/store/dispatch-context'

export const CanvasControlsContainerID = 'new-canvas-controls-container'

export type ResizeStatus = 'disabled' | 'noninteractive' | 'enabled'

function useLocalSelectedHighlightedViews(
  editorSelectedViews: ElementPath[],
  editorHighlightedViews: ElementPath[],
  transientCanvasState: TransientCanvasState,
): {
  localSelectedViews: ElementPath[]
  localHighlightedViews: ElementPath[]
  setSelectedViewsLocally: (newSelectedViews: Array<ElementPath>) => void
} {
  const [localSelectedViews, setLocalSelectedViews] = usePropControlledStateV2(
    transientCanvasState.selectedViews ?? editorSelectedViews,
  )
  const [localHighlightedViews, setLocalHighlightedViews] = usePropControlledStateV2(
    transientCanvasState.highlightedViews ?? editorHighlightedViews,
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
  allElementProps: AllElementProps
}

interface NewCanvasControlsProps {
  windowToCanvasPosition: (event: MouseEvent) => CanvasPositions
  cursor: CSSCursor
}

export const NewCanvasControls = React.memo((props: NewCanvasControlsProps) => {
  const dispatch = useDispatch()
  const canvasControlProps = useEditorState(
    'fullOldStore',
    (store) => ({
      editor: store.editor,
      derived: store.derived,
      canvasOffset: store.editor.canvas.roundedCanvasOffset,
      controls: store.derived.controls,
      scale: store.editor.canvas.scale,
      focusedPanel: store.editor.focusedPanel,
      transientCanvasState: store.derived.transientState,
    }),
    'NewCanvasControls',
  )

  const { localSelectedViews, localHighlightedViews, setSelectedViewsLocally } =
    useLocalSelectedHighlightedViews(
      canvasControlProps.editor.selectedViews,
      canvasControlProps.editor.highlightedViews,
      canvasControlProps.transientCanvasState,
    )

  const canvasScrollAnimation = useEditorState(
    'canvas',
    (store) => store.editor.canvas.scrollAnimation,
    'NewCanvasControls scrollAnimation',
  )

  // Somehow this being setup and hooked into the div makes the `onDrop` call
  // work properly in `editor-canvas.ts`. I blame React DnD for this.
  const dropSpec: DropTargetHookSpec<FileBrowserItemProps, 'CANVAS', unknown> = {
    accept: 'files',
    canDrop: () => true,
  }

  const [_, drop] = useDrop(dropSpec)

  const forwardedRef = React.useCallback(
    (node: ConnectableElement) => {
      return drop(node)
    },
    [drop],
  )

  if (
    isTextEditModeWithActiveTextEditing(canvasControlProps.editor.mode) ||
    (isLiveMode(canvasControlProps.editor.mode) && !canvasControlProps.editor.keysPressed.cmd)
  ) {
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
            editor={canvasControlProps.editor}
            transientState={canvasControlProps.transientCanvasState}
            dispatch={dispatch}
            canvasOffset={canvasControlProps.canvasOffset}
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
  transientState: TransientCanvasState
  dispatch: EditorDispatch
  canvasOffset: CanvasPoint
  windowToCanvasPosition: (event: MouseEvent) => CanvasPositions
  localSelectedViews: Array<ElementPath>
  localHighlightedViews: Array<ElementPath>
  setLocalSelectedViews: (newSelectedViews: ElementPath[]) => void
}

const NewCanvasControlsInner = (props: NewCanvasControlsInnerProps) => {
  const colorTheme = useColorTheme()
  const strategyControls = useGetApplicableStrategyControls()

  const anyStrategyActive = useEditorState(
    'restOfStore',
    (store) => store.strategyState.currentStrategy != null,
    'currentStrategy',
  )
  const strategy = useEditorState('restOfStore', (store) => store.strategyState, 'strategy')

  const { localSelectedViews, localHighlightedViews, setLocalSelectedViews } = props
  const cmdKeyPressed = props.editor.keysPressed['cmd'] ?? false

  const componentMetadata = getMetadata(props.editor)

  const dragging = isDragging(props.editor)
  const selectionEnabled = pickSelectionEnabled(props.editor.canvas, props.editor.keysPressed)
  const contextMenuEnabled = !isLiveMode(props.editor.mode)

  const { maybeHighlightOnHover, maybeClearHighlightsOnHoverEnd } = useMaybeHighlightElement()

  const { onMouseMove, onMouseDown, onMouseUp } = useSelectAndHover(
    cmdKeyPressed,
    setLocalSelectedViews,
  )

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

  const onContextMenu = React.useCallback(
    (event: React.MouseEvent<HTMLDivElement>): void => {
      switch (props.editor.mode.type) {
        case 'select':
        case 'live': {
          event.stopPropagation()
          event.preventDefault()
          if (contextMenuEnabled && localSelectedViews.length > 0) {
            props.dispatch([showContextMenu('context-menu-canvas', event.nativeEvent)], 'canvas')
          }
          break
        }
        default:
          break
      }
    },
    [contextMenuEnabled, localSelectedViews, props],
  )

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

  const renderTextEditableControls = () => {
    if (strategy?.currentStrategy !== DRAW_TO_INSERT_TEXT_STRATEGY_ID) {
      return []
    }
    return Object.keys(props.editor.allElementProps)
      .filter((p) => {
        const metadata = componentMetadata[p]
        if (metadata == null) {
          return false
        }
        return (
          MetadataUtils.targetTextEditable(componentMetadata, EP.fromString(p)) &&
          ['hasOnlyTextChildren', 'supportsChildren'].includes(
            MetadataUtils.targetElementSupportsChildrenAlsoText(
              props.editor.projectContents,
              metadata,
            ),
          )
        )
      })
      .map((p) => {
        const elementPath = EP.fromString(p)
        const frame = MetadataUtils.getFrameInCanvasCoords(elementPath, componentMetadata)
        if (frame == null) {
          return null
        }
        return (
          <TextEditableControl
            key={`text-editable-control-${EP.toComponentId(elementPath)}`}
            frame={frame}
            scale={props.editor.canvas.scale}
            canvasOffset={props.canvasOffset}
          />
        )
      })
  }

  const resizeStatus = getResizeStatus()

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
      onContextMenu={onContextMenu}
      onMouseDown={onMouseDown}
      onMouseMove={onMouseMove}
      onMouseUp={onMouseUp}
    >
      {when(
        isSelectMode(props.editor.mode),
        <SceneLabelControl
          maybeHighlightOnHover={maybeHighlightOnHover}
          maybeClearHighlightsOnHoverEnd={maybeClearHighlightsOnHoverEnd}
        />,
      )}
      {when(
        resizeStatus !== 'disabled',
        <>
          {when(isSelectMode(props.editor.mode) && !anyStrategyActive, <PinLines />)}
          {when(isSelectMode(props.editor.mode), <InsertionControls />)}
          {renderHighlightControls()}
          {renderTextEditableControls()}
          {unless(dragging, <LayoutParentControl />)}
          <MultiSelectOutlineControl localSelectedElements={localSelectedViews} />
          <GuidelineControls />
          <ZeroSizedElementControls.control showAllPossibleElements={false} />
          {when(
            isSelectOrInsertMode(props.editor.mode),
            <>
              {strategyControls.map((c) => (
                <RenderControlMemoized
                  key={c.key}
                  control={c.control.control}
                  propsForControl={c.props}
                />
              ))}
            </>,
          )}
          {when(isSelectMode(props.editor.mode), <DistanceGuidelineControl />)}
        </>,
      )}
      <CursorComponent />
    </div>
  )
}

function isSelectOrInsertMode(mode: Mode): boolean {
  return mode.type === 'select' || mode.type === 'insert'
}

interface RenderControlMemoizedProps {
  control: React.FC
  propsForControl: any
}

const RenderControlMemoized = React.memo(
  ({ control, propsForControl }: RenderControlMemoizedProps) => {
    const ControlToRender = control

    return <ControlToRender {...propsForControl} />
  },
  (prevProps, nextProps) => {
    return (
      prevProps.control === nextProps.control &&
      shallowEqual(prevProps.propsForControl, nextProps.propsForControl)
    )
  },
)
