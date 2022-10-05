/** @jsxRuntime classic */
/** @jsx jsx */
/** @jsxFrag React.Fragment */
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
  getOpenUIJSFileKey,
  TransientCanvasState,
  TransientFilesState,
  ResizeOptions,
  AllElementProps,
} from '../../editor/store/editor-state'
import { ElementPath, NodeModules } from '../../../core/shared/project-file-types'
import { CanvasPositions, CSSCursor } from '../canvas-types'
import { SelectModeControlContainer } from './select-mode-control-container'
import { HighlightControl } from './highlight-control'
import { useEditorState, useRefEditorState } from '../../editor/store/store-hook'
import {
  ElementInstanceMetadataMap,
  UtopiaJSXComponent,
} from '../../../core/shared/element-template'
import { MetadataUtils } from '../../../core/model/element-metadata-utils'
import { isAspectRatioLockedNew } from '../../aspect-ratio'
import { ElementContextMenu } from '../../element-context-menu'
import { isLiveMode, EditorModes, Mode } from '../../editor/editor-modes'
import { DropTargetHookSpec, ConnectableElement, useDrop, DndProvider } from 'react-dnd'
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
import { unless, when } from '../../../utils/react-conditionals'
import { isFeatureEnabled } from '../../../utils/feature-switches'
import { shallowEqual } from '../../../core/shared/equality-utils'
import { KeysPressed } from '../../../utils/keyboard'
import { usePrevious } from '../../editor/hook-utils'
import { LayoutTargetableProp } from '../../../core/layout/layout-helpers-new'
import { getDragStateStart } from '../canvas-utils'
import { useGetApplicableStrategyControls } from '../canvas-strategies/canvas-strategies'
import { FlexResizeControl } from './select-mode/flex-resize-control'
import { MultiSelectOutlineControl } from './select-mode/simple-outline-control'
import { GuidelineControls } from './guideline-controls'
import { showContextMenu } from '../../editor/actions/action-creators'
import { HTML5Backend } from 'react-dnd-html5-backend'
import { OutlineHighlightControl } from './select-mode/outline-highlight-control'
import { InsertionControls } from './insertion-plus-button'
import { DistanceGuidelineControl } from './select-mode/distance-guideline-control'
import { SceneLabelControl } from './select-mode/scene-label'
import { PinLines } from './position-outline'
import { CursorOverlay } from './select-mode/cursor-overlay'
import { FlexReparentTargetIndicator } from './select-mode/flex-reparent-target-indicator'
import { ControlWithProps } from '../canvas-strategies/canvas-strategy-types'
import { useKeepShallowReferenceEquality } from '../../../utils/react-performance'

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
  const canvasControlProps = useEditorState(
    (store) => ({
      dispatch: store.dispatch,
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
      <DndProvider backend={HTML5Backend}>
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
              dispatch={canvasControlProps.dispatch}
              canvasOffset={canvasControlProps.canvasOffset}
            />
          </div>
          <ElementContextMenu contextMenuInstance='context-menu-canvas' />
        </div>
      </DndProvider>
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
  const startDragStateAfterDragExceedsThreshold = useStartDragStateAfterDragExceedsThreshold()
  const strategyControls = useGetApplicableStrategyControls()

  const anyStrategyActive = useEditorState(
    (store) => store.strategyState.currentStrategy != null,
    'currentStrategy',
  )

  const { localSelectedViews, localHighlightedViews, setLocalSelectedViews } = props
  const cmdKeyPressed = props.editor.keysPressed['cmd'] ?? false

  const componentMetadata = getMetadata(props.editor)

  const resizing = isResizing(props.editor)
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
      if (isFeatureEnabled('Canvas Strategies')) {
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
      }
    },
    [contextMenuEnabled, localSelectedViews, props],
  )

  const renderModeControlContainer = () => {
    const elementAspectRatioLocked = localSelectedViews.every((target) => {
      const possibleElement = MetadataUtils.findElementByElementPath(componentMetadata, target)
      const elementProps = props.editor.allElementProps[EP.toString(target)]
      if (possibleElement == null || elementProps == null) {
        return false
      } else {
        return isAspectRatioLockedNew(possibleElement, elementProps)
      }
    })
    const imageMultiplier: number | null = MetadataUtils.getImageMultiplier(
      componentMetadata,
      localSelectedViews,
      props.editor.allElementProps,
    )
    const resolveFn = props.editor.codeResultCache.curriedResolveFn(props.editor.projectContents)
    const controlProps: ControlProps = {
      selectedViews: localSelectedViews,
      highlightedViews: localHighlightedViews,
      componentMetadata: componentMetadata,
      hiddenInstances: props.editor.hiddenInstances,
      focusedElementPath: props.editor.focusedElementPath,
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
      transientState: props.transientState,
      resolve: resolveFn,
      resizeOptions: props.editor.canvas.resizeOptions,
      allElementProps: props.editor.allElementProps,
    }
    const dragState = props.editor.canvas.dragState

    if (isFeatureEnabled('Canvas Strategies')) {
      return null
    } else {
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
      {renderModeControlContainer()}
      {when(
        isFeatureEnabled('Canvas Strategies') && props.editor.mode.type === 'select',
        <SceneLabelControl
          maybeHighlightOnHover={maybeHighlightOnHover}
          maybeClearHighlightsOnHoverEnd={maybeClearHighlightsOnHoverEnd}
        />,
      )}
      {when(
        resizeStatus !== 'disabled',
        <>
          {when(
            isCanvasStrategyOnAndSelectMode(props.editor.mode) && !anyStrategyActive,
            <PinLines />,
          )}
          {when(isCanvasStrategyOnAndSelectMode(props.editor.mode), <DistanceGuidelineControl />)}
          {when(
            isFeatureEnabled('Canvas Strategies') &&
              isFeatureEnabled('Insertion Plus Button') &&
              props.editor.mode.type === 'select',
            <InsertionControls />,
          )}
          {renderHighlightControls()}
          {unless(dragging, <LayoutParentControl />)}
          {when(
            isFeatureEnabled('Canvas Strategies'),
            <MultiSelectOutlineControl localSelectedElements={localSelectedViews} />,
          )}
          {when(isFeatureEnabled('Canvas Strategies'), <GuidelineControls />)}
          <OutlineHighlightControl />
          {when(
            isCanvasStrategyOnAndSelectOrInsertMode(props.editor.mode),
            <>
              {strategyControls.map((c) => (
                <RenderControlMemoized key={c.key} control={c} />
              ))}
            </>,
          )}
        </>,
      )}
      <CursorOverlay />
    </div>
  )
}

function isCanvasStrategyOnAndSelectMode(mode: Mode): boolean {
  return isFeatureEnabled('Canvas Strategies') && mode.type === 'select'
}

function isCanvasStrategyOnAndSelectOrInsertMode(mode: Mode): boolean {
  return isFeatureEnabled('Canvas Strategies') && (mode.type === 'select' || mode.type === 'insert')
}

const RenderControlMemoized = React.memo(({ control }: { control: ControlWithProps<any> }) => {
  const ControlToRender = control.control.control

  const propsMemoized = useKeepShallowReferenceEquality(control.props)

  return <ControlToRender {...propsMemoized} />
})
