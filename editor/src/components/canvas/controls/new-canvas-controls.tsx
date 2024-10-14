/* eslint-disable @typescript-eslint/ban-types */
/** @jsxRuntime classic */
/** @jsx jsx */
/** @jsxFrag React.Fragment */
import { jsx } from '@emotion/react'
import React from 'react'
import * as EP from '../../../core/shared/element-path'
import type { CanvasPoint, WindowRectangle } from '../../../core/shared/math-utils'
import { isInfinityRectangle } from '../../../core/shared/math-utils'
import type { EditorDispatch } from '../../editor/action-types'
import type {
  TransientCanvasState,
  ResizeOptions,
  AllElementProps,
} from '../../editor/store/editor-state'
import { getMetadata } from '../../editor/store/editor-state'
import type { ElementPath, NodeModules } from '../../../core/shared/project-file-types'
import type { CSSCursor, CanvasPositions } from '../canvas-types'
import { HighlightControl } from './highlight-control'
import { Substores, useEditorState } from '../../editor/store/store-hook'
import type { ElementInstanceMetadataMap } from '../../../core/shared/element-template'
import { MetadataUtils } from '../../../core/model/element-metadata-utils'
import { ElementContextMenu } from '../../element-context-menu'
import type { Mode } from '../../editor/editor-modes'
import {
  isCommentMode,
  isInsertMode,
  isLiveMode,
  isSelectMode,
  isSelectModeWithArea,
  isTextEditMode,
} from '../../editor/editor-modes'
import type { DropTargetHookSpec, ConnectableElement } from 'react-dnd'
import { useDrop } from 'react-dnd'
import type { FileBrowserItemProps } from '../../filebrowser/fileitem'
import type { ResolveFn } from '../../custom-code/code-file'
import { useColorTheme } from '../../../uuiui'
import {
  isDragInteractionActive,
  isGridDragInteractionActive,
  pickSelectionEnabled,
  useMaybeHighlightElement,
  useSelectAndHover,
} from './select-mode/select-mode-hooks'
import { usePropControlledStateV2 } from '../../inspector/common/inspector-utils'
import type { ProjectContentTreeRoot } from '../../assets'
import { LayoutParentControl } from './layout-parent-control'
import { unless, when } from '../../../utils/react-conditionals'
import { useGetApplicableStrategyControls } from '../canvas-strategies/canvas-strategies'
import {
  DataReferenceParentOutline,
  MultiSelectOutlineControl,
} from './select-mode/simple-outline-control'
import { GuidelineControls } from './guideline-controls'
import { showContextMenu } from '../../editor/actions/action-creators'
import { InsertionControls } from './insertion-plus-button'
import { DistanceGuidelineControl } from './select-mode/distance-guideline-control'
import { SceneLabelControl } from './select-mode/scene-label'
import { PinLines } from './position-outline'
import { shallowEqual } from '../../../core/shared/equality-utils'
import { ZeroSizedElementControls } from './zero-sized-element-controls'
import { DRAW_TO_INSERT_TEXT_STRATEGY_ID } from '../canvas-strategies/strategies/draw-to-insert-text-strategy'
import { TextEditableControl } from './text-editable-control'
import { TextEditCanvasOverlay } from './text-edit-mode/text-edit-canvas-overlay'
import { useDispatch } from '../../editor/store/dispatch-context'
import { AbsoluteChildrenOutline } from './absolute-children-outline'
import { useAtom } from 'jotai'
import {
  InspectorFocusedCanvasControls,
  InspectorHoveredCanvasControls,
} from '../../inspector/common/inspector-atoms'
import { useSelectionArea } from './selection-area-hooks'
import { RemixSceneLabelControl } from './select-mode/remix-scene-label'
import { NO_OP } from '../../../core/shared/utils'
import { useIsMyProject } from '../../editor/store/collaborative-editing'
import { MultiplayerWrapper } from '../../../utils/multiplayer-wrapper'
import { MultiplayerPresence } from '../multiplayer-presence'
import { GridMeasurementHelper } from './grid-controls'
import { GridControlKey, useGridData } from './grid-controls-for-strategies'

export const CanvasControlsContainerID = 'new-canvas-controls-container'

export type ResizeStatus = 'disabled' | 'noninteractive' | 'enabled'

function useLocalSelectedHighlightedViews(
  editorSelectedViews: ElementPath[],
  editorHighlightedViews: ElementPath[],
): {
  localSelectedViews: ElementPath[]
  localHighlightedViews: ElementPath[]
  setSelectedViewsLocally: (newSelectedViews: Array<ElementPath>) => void
  setLocalHighlightedViews: (newHighlightedViews: Array<ElementPath>) => void
} {
  const [localSelectedViews, setLocalSelectedViews] = usePropControlledStateV2(editorSelectedViews)
  const [localHighlightedViews, setLocalHighlightedViews] =
    usePropControlledStateV2(editorHighlightedViews)

  const setSelectedViewsLocally = React.useCallback(
    (newSelectedViews: Array<ElementPath>) => {
      setLocalSelectedViews(newSelectedViews)
    },
    [setLocalSelectedViews],
  )
  return {
    localSelectedViews,
    localHighlightedViews,
    setSelectedViewsLocally,
    setLocalHighlightedViews,
  }
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
    Substores.fullStore,
    (store) => ({
      keysPressed: store.editor.keysPressed,
      editorMode: store.editor.mode,
      controls: store.derived.controls,
      scale: store.editor.canvas.scale,
      focusedPanel: store.editor.focusedPanel,
      selectedViews: store.editor.selectedViews,
      highlightedViews: store.editor.highlightedViews,
      canvasScrollAnimation: store.editor.canvas.scrollAnimation,
    }),
    'NewCanvasControls',
  )

  const {
    localSelectedViews,
    localHighlightedViews,
    setSelectedViewsLocally,
    setLocalHighlightedViews,
  } = useLocalSelectedHighlightedViews(
    canvasControlProps.selectedViews,
    canvasControlProps.highlightedViews,
  )

  // Somehow this being setup and hooked into the div makes the `onDrop` call
  // work properly in `editor-canvas.ts`. I blame React DnD for this.
  const dropSpec: DropTargetHookSpec<FileBrowserItemProps, 'CANVAS', unknown> = {
    accept: 'files',
    canDrop: () => true,
  }

  const [, drop] = useDrop(dropSpec)

  const forwardedRef = React.useCallback(
    (node: ConnectableElement) => {
      return drop(node)
    },
    [drop],
  )

  const ref = React.useRef<HTMLDivElement | null>(null)

  if (isLiveMode(canvasControlProps.editorMode) && !canvasControlProps.keysPressed.cmd) {
    return (
      <div
        style={{
          pointerEvents: 'none',
          position: 'absolute',
          top: 0,
          left: 0,
          width: `${canvasControlProps.scale < 1 ? 100 / canvasControlProps.scale : 100}%`,
          height: `${canvasControlProps.scale < 1 ? 100 / canvasControlProps.scale : 100}%`,
          transformOrigin: 'top left',
          transform: canvasControlProps.scale < 1 ? `scale(${canvasControlProps.scale}) ` : '',
          cursor: props.cursor,
          visibility: canvasControlProps.canvasScrollAnimation ? 'hidden' : 'initial',
        }}
      >
        <div style={{ pointerEvents: 'none', position: 'relative', width: '100%', height: '100%' }}>
          <RemixSceneLabelControl
            maybeHighlightOnHover={NO_OP}
            maybeClearHighlightsOnHoverEnd={NO_OP}
          />
        </div>
      </div>
    )
  } else if (isTextEditMode(canvasControlProps.editorMode)) {
    return <TextEditCanvasOverlay cursor={props.cursor} />
  } else {
    return (
      <>
        <div
          key='canvas-controls'
          ref={forwardedRef}
          className={
            canvasControlProps.focusedPanel === 'canvas'
              ? '  canvas-controls focused '
              : ' canvas-controls '
          }
          id='canvas-controls'
          data-testid='canvas-controls'
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
            visibility: canvasControlProps.canvasScrollAnimation ? 'hidden' : 'initial',
          }}
        >
          <div
            ref={ref}
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
              setLocalHighlightedViews={setLocalHighlightedViews}
            />
          </div>

          <ElementContextMenu contextMenuInstance='context-menu-canvas' />
          <ElementContextMenu contextMenuInstance='context-menu-canvas-no-selection' />
        </div>
        <MultiplayerWrapper errorFallback={null} suspenseFallback={null}>
          <MultiplayerPresence />
        </MultiplayerWrapper>
      </>
    )
  }
})
NewCanvasControls.displayName = 'NewCanvasControls'

interface NewCanvasControlsInnerProps {
  windowToCanvasPosition: (event: MouseEvent) => CanvasPositions
  localSelectedViews: Array<ElementPath>
  localHighlightedViews: Array<ElementPath>
  setLocalSelectedViews: (newSelectedViews: ElementPath[]) => void
  setLocalHighlightedViews: (newHighlightedViews: ElementPath[]) => void
}

const NewCanvasControlsInner = (props: NewCanvasControlsInnerProps) => {
  const {
    localSelectedViews,
    localHighlightedViews,
    setLocalSelectedViews,
    setLocalHighlightedViews,
  } = props

  const dispatch = useDispatch()
  const colorTheme = useColorTheme()
  const strategyControls = useGetApplicableStrategyControls(localSelectedViews)
  const [inspectorHoveredControls] = useAtom(InspectorHoveredCanvasControls)
  const [inspectorFocusedControls] = useAtom(InspectorFocusedCanvasControls)

  const anyStrategyActive = useEditorState(
    Substores.restOfStore,
    (store) => store.strategyState.currentStrategy != null,
    'currentStrategy',
  )
  const strategy = useEditorState(Substores.restOfStore, (store) => store.strategyState, 'strategy')

  const [selectionAreaRectangle, setSelectionAreaRectangle] =
    React.useState<WindowRectangle | null>(null)

  const {
    keysPressed,
    componentMetadata,
    dragInteractionActive,
    gridDragInteractionActive,
    selectionEnabled,
    textEditor,
    editorMode,
    canvasOffset,
    scale,
    focusedElementPath,
    projectContents,
    pathTrees,
    autoFocusedPaths,
    filePathMappings,
    propertyControlsInfo,
  } = useEditorState(
    Substores.fullStore,
    (store) => {
      return {
        keysPressed: store.editor.keysPressed,
        componentMetadata: getMetadata(store.editor),
        dragInteractionActive: isDragInteractionActive(store.editor),
        gridDragInteractionActive: isGridDragInteractionActive(store.editor),
        selectionEnabled: pickSelectionEnabled(store.editor.canvas, store.editor.keysPressed),
        editorMode: store.editor.mode,
        textEditor: store.editor.canvas.textEditor,
        canvasOffset: store.editor.canvas.roundedCanvasOffset,
        scale: store.editor.canvas.scale,
        focusedElementPath: store.editor.focusedElementPath,
        allElementProps: store.editor.allElementProps,
        projectContents: store.editor.projectContents,
        pathTrees: store.editor.elementPathTree,
        autoFocusedPaths: store.derived.autoFocusedPaths,
        filePathMappings: store.derived.filePathMappings,
        propertyControlsInfo: store.editor.propertyControlsInfo,
      }
    },
    'NewCanvasControlsInner',
  )

  const grids = useAllGrids(componentMetadata)

  const cmdKeyPressed = keysPressed['cmd'] ?? false

  const contextMenuEnabled = !isLiveMode(editorMode)
  const { maybeHighlightOnHover, maybeClearHighlightsOnHoverEnd } = useMaybeHighlightElement()

  const ref = React.useRef<HTMLDivElement | null>(null)

  const localSelectedViewsRef = React.useRef(localSelectedViews)
  localSelectedViewsRef.current = localSelectedViews
  const setLocalSelectedViewsRef = React.useCallback(
    (newSelectedViews: ElementPath[]) => {
      localSelectedViewsRef.current = newSelectedViews
      setLocalSelectedViews(newSelectedViews)
    },
    [setLocalSelectedViews],
  )

  const selectModeHooks = useSelectAndHover(cmdKeyPressed, setLocalSelectedViewsRef)

  const areaSelectionHooks = useSelectionArea(
    ref,
    localHighlightedViews,
    localSelectedViews,
    setSelectionAreaRectangle,
    setLocalHighlightedViews,
  )

  const onMouseDown = React.useCallback(
    (e: React.MouseEvent<HTMLDivElement>) => {
      if (areaSelectionHooks.onMouseDown(e)) {
        return
      }
      selectModeHooks.onMouseDown(e)
    },
    [areaSelectionHooks, selectModeHooks],
  )

  const onMouseUp = React.useCallback(
    (e: React.MouseEvent<HTMLDivElement>) => {
      if (!isSelectModeWithArea(editorMode)) {
        selectModeHooks.onMouseUp(e)
      }
    },
    [editorMode, selectModeHooks],
  )

  const onMouseMove = React.useCallback(
    (e: React.MouseEvent<HTMLDivElement>) => {
      if (!isSelectModeWithArea(editorMode)) {
        selectModeHooks.onMouseMove(e)
      }
    },
    [editorMode, selectModeHooks],
  )

  const getResizeStatus = () => {
    const selectedViews = localSelectedViews
    if (textEditor != null || keysPressed['z']) {
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
      switch (editorMode.type) {
        case 'select':
        case 'live': {
          event.stopPropagation()
          event.preventDefault()
          if (contextMenuEnabled) {
            selectModeHooks.onMouseDown(event)
            if (localSelectedViewsRef.current.length > 0) {
              dispatch([showContextMenu('context-menu-canvas', event.nativeEvent)], 'canvas')
            } else {
              dispatch([showContextMenu('context-menu-canvas-no-selection', event.nativeEvent)])
            }
          }
          break
        }
        default:
          break
      }
    },
    [contextMenuEnabled, editorMode.type, dispatch, selectModeHooks],
  )

  const renderHighlightControls = () => {
    return selectionEnabled
      ? localHighlightedViews.map((path) => {
          // Do not display the highlight controls if this element is selected.
          if (EP.containsPath(path, localSelectedViews)) {
            return null
          }
          const frame = MetadataUtils.getFrameInCanvasCoords(path, componentMetadata)
          if (frame == null || isInfinityRectangle(frame)) {
            return null
          }
          const isAutomaticOrManuallyFocusableComponent =
            MetadataUtils.isAutomaticOrManuallyFocusableComponent(
              path,
              componentMetadata,
              autoFocusedPaths,
              filePathMappings,
              propertyControlsInfo,
              projectContents,
            )
          const isFocusedComponent = EP.isFocused(focusedElementPath, path)
          const color =
            isAutomaticOrManuallyFocusableComponent || isFocusedComponent
              ? colorTheme.canvasSelectionIsolatedComponent.value
              : colorTheme.canvasSelectionPrimaryOutline.value
          return (
            <HighlightControl
              key={`highlight-control-${EP.toComponentId(path)}`}
              color={color}
              frame={frame}
              scale={scale}
              canvasOffset={canvasOffset}
              displayZeroSized={'display-zero-sized'}
            />
          )
        })
      : []
  }

  const renderTextEditableControls = () => {
    if (strategy?.currentStrategy !== DRAW_TO_INSERT_TEXT_STRATEGY_ID) {
      return []
    }
    return Object.keys(componentMetadata)
      .filter((p) => {
        return (
          MetadataUtils.targetTextEditableAndHasText(
            componentMetadata,
            pathTrees,
            EP.fromString(p),
          ) &&
          ['hasOnlyTextChildren', 'supportsChildren', 'conditionalWithText'].includes(
            MetadataUtils.targetElementSupportsChildrenAlsoText(
              projectContents,
              EP.fromString(p),
              componentMetadata,
              pathTrees,
              propertyControlsInfo,
            ),
          )
        )
      })
      .map((p) => {
        const elementPath = EP.fromString(p)
        const frame = MetadataUtils.getFrameInCanvasCoords(elementPath, componentMetadata)
        if (frame == null || isInfinityRectangle(frame)) {
          return null
        }
        return (
          <TextEditableControl
            key={`text-editable-control-${EP.toComponentId(elementPath)}`}
            frame={frame}
            scale={scale}
            canvasOffset={canvasOffset}
          />
        )
      })
  }

  const isMyProject = useIsMyProject()

  const resizeStatus = getResizeStatus()

  return (
    <>
      <div
        ref={ref}
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
        onMouseMove={onMouseMove}
        onMouseDown={onMouseDown}
        onMouseUp={onMouseUp}
      >
        {when(
          isSelectMode(editorMode) || isCommentMode(editorMode),
          <>
            <SceneLabelControl
              maybeHighlightOnHover={maybeHighlightOnHover}
              maybeClearHighlightsOnHoverEnd={maybeClearHighlightsOnHoverEnd}
            />
            <DataReferenceParentOutline
              selectedViews={localSelectedViews}
              highlightedViews={localHighlightedViews}
            />
          </>,
        )}
        {when(
          resizeStatus !== 'disabled',
          <>
            {renderHighlightControls()}
            {unless(dragInteractionActive, <LayoutParentControl />)}
            {unless(
              gridDragInteractionActive,
              <MultiSelectOutlineControl localSelectedElements={localSelectedViews} />,
            )}
            <ZeroSizedElementControls.control showAllPossibleElements={false} />

            {when(
              isSelectMode(editorMode) || isCommentMode(editorMode),
              <RemixSceneLabelControl
                maybeHighlightOnHover={maybeHighlightOnHover}
                maybeClearHighlightsOnHoverEnd={maybeClearHighlightsOnHoverEnd}
              />,
            )}

            {when(
              isMyProject,
              <>
                {inspectorFocusedControls.map((c) => (
                  <RenderControlMemoized
                    key={c.key}
                    control={c.control}
                    propsForControl={c.props}
                  />
                ))}
                {inspectorHoveredControls.map((c) => (
                  <RenderControlMemoized
                    key={c.key}
                    control={c.control}
                    propsForControl={c.props}
                  />
                ))}
                {when(isSelectMode(editorMode) && !anyStrategyActive, <PinLines />)}
                {when(isSelectMode(editorMode), <InsertionControls />)}
                {renderTextEditableControls()}
                {when(isSelectMode(editorMode), <AbsoluteChildrenOutline />)}
                {when(
                  isSelectOrInsertMode(editorMode) &&
                    !EP.multiplePathsAllWithTheSameUID(localSelectedViews),
                  <>
                    {when(
                      isSelectMode(editorMode) || isInsertMode(editorMode),
                      <React.Fragment>
                        {grids.map((grid) => {
                          return (
                            <GridMeasurementHelper
                              key={GridControlKey(grid.elementPath)}
                              grid={grid}
                            />
                          )
                        })}
                      </React.Fragment>,
                    )}
                    {strategyControls.map((c) => (
                      <RenderControlMemoized
                        key={c.key}
                        control={c.control.control}
                        propsForControl={c.props}
                      />
                    ))}
                  </>,
                )}
                {when(isSelectMode(editorMode), <DistanceGuidelineControl />)}
                <GuidelineControls />
              </>,
            )}
          </>,
        )}
      </div>
      <SelectionAreaRectangle rectangle={selectionAreaRectangle} />
    </>
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

const SelectionAreaRectangle = React.memo(
  ({ rectangle }: { rectangle: WindowRectangle | null }) => {
    const colorTheme = useColorTheme()

    const canvasScale = useEditorState(
      Substores.canvas,
      (store) => store.editor.canvas.scale,
      'SelectionAreaRectangle canvasScale',
    )

    if (rectangle == null) {
      return null
    }

    return (
      <div
        style={{
          border: `1px solid ${colorTheme.primary.value}`,
          background: colorTheme.primary10.value,
          position: 'absolute',
          pointerEvents: 'none',
          width: rectangle.width,
          height: rectangle.height,
          left: rectangle.x / canvasScale,
          top: rectangle.y / canvasScale,
          transformOrigin: 'top left',
          transform: `scale(${1 / canvasScale})`,
        }}
      />
    )
  },
)

function useAllGrids(metadata: ElementInstanceMetadataMap) {
  const grids = React.useMemo(() => {
    return MetadataUtils.getAllGrids(metadata)
  }, [metadata])

  return useGridData(grids)
}

SelectionAreaRectangle.displayName = 'SelectionAreaRectangle'
