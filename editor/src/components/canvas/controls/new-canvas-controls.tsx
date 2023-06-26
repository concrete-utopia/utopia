/* eslint-disable @typescript-eslint/ban-types */
/** @jsxRuntime classic */
/** @jsx jsx */
/** @jsxFrag React.Fragment */
import { jsx } from '@emotion/react'
import React from 'react'
import * as EP from '../../../core/shared/element-path'
import { CanvasPoint, WindowRectangle, isInfinityRectangle } from '../../../core/shared/math-utils'
import { EditorDispatch } from '../../editor/action-types'
import {
  getMetadata,
  TransientCanvasState,
  ResizeOptions,
  AllElementProps,
} from '../../editor/store/editor-state'
import { ElementPath, NodeModules } from '../../../core/shared/project-file-types'
import { CanvasPositions, CSSCursor } from '../canvas-types'
import { HighlightControl } from './highlight-control'
import { Substores, useEditorState } from '../../editor/store/store-hook'
import { ElementInstanceMetadataMap } from '../../../core/shared/element-template'
import { MetadataUtils } from '../../../core/model/element-metadata-utils'
import { ElementContextMenu } from '../../element-context-menu'
import {
  isLiveMode,
  isSelectMode,
  isSelectModeWithArea,
  isTextEditMode,
  Mode,
} from '../../editor/editor-modes'
import { DropTargetHookSpec, ConnectableElement, useDrop } from 'react-dnd'
import { FileBrowserItemProps } from '../../filebrowser/fileitem'
import { ResolveFn } from '../../custom-code/code-file'
import { useColorTheme } from '../../../uuiui'
import {
  isDragging,
  pickSelectionEnabled,
  useMaybeHighlightElement,
  useSelectAndHover,
} from './select-mode/select-mode-hooks'
import { usePropControlledStateV2 } from '../../inspector/common/inspector-utils'
import { ProjectContentTreeRoot } from '../../assets'
import { LayoutParentControl } from './layout-parent-control'
import { unless, when } from '../../../utils/react-conditionals'
import { useGetApplicableStrategyControls } from '../canvas-strategies/canvas-strategies'
import { MultiSelectOutlineControl } from './select-mode/simple-outline-control'
import { GuidelineControls } from './guideline-controls'
import { scrollToElement, showContextMenu } from '../../editor/actions/action-creators'
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
import {
  ElementOutsideVisibleAreaIndicator,
  useElementsOutsideVisibleArea,
} from './elements-outside-visible-area-hooks'

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
  setLocalHighlightedViews: (newHighlightedViews: Array<ElementPath>) => void
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
      transientCanvasState: store.derived.transientState,
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
    canvasControlProps.transientCanvasState,
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

  const ref = React.useRef<HTMLDivElement | null>(null)

  const elementsOutsideVisibleAreaIndicators = useElementsOutsideVisibleArea(
    ref,
    localHighlightedViews,
    localSelectedViews,
  )

  if (isLiveMode(canvasControlProps.editorMode) && !canvasControlProps.keysPressed.cmd) {
    return null
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
        </div>
        <ElementsOutsideVisibleAreaIndicators indicators={elementsOutsideVisibleAreaIndicators} />
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
  const dispatch = useDispatch()
  const colorTheme = useColorTheme()
  const strategyControls = useGetApplicableStrategyControls()
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
    dragging,
    selectionEnabled,
    textEditor,
    editorMode,
    canvasOffset,
    scale,
    focusedElementPath,
    projectContents,
    pathTrees,
  } = useEditorState(
    Substores.fullStore,
    (store) => {
      return {
        keysPressed: store.editor.keysPressed,
        componentMetadata: getMetadata(store.editor),
        dragging: isDragging(store.editor),
        selectionEnabled: pickSelectionEnabled(store.editor.canvas, store.editor.keysPressed),
        editorMode: store.editor.mode,
        textEditor: store.editor.canvas.textEditor,
        canvasOffset: store.editor.canvas.roundedCanvasOffset,
        scale: store.editor.canvas.scale,
        focusedElementPath: store.editor.focusedElementPath,
        allElementProps: store.editor.allElementProps,
        projectContents: store.editor.projectContents,
        pathTrees: store.editor.elementPathTree,
      }
    },
    'NewCanvasControlsInner',
  )

  const {
    localSelectedViews,
    localHighlightedViews,
    setLocalSelectedViews,
    setLocalHighlightedViews,
  } = props
  const cmdKeyPressed = keysPressed['cmd'] ?? false

  const contextMenuEnabled = !isLiveMode(editorMode)
  const { maybeHighlightOnHover, maybeClearHighlightsOnHoverEnd } = useMaybeHighlightElement()

  const ref = React.useRef<HTMLDivElement | null>(null)

  const selectModeHooks = useSelectAndHover(cmdKeyPressed, setLocalSelectedViews)

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
          if (contextMenuEnabled && localSelectedViews.length > 0) {
            dispatch([showContextMenu('context-menu-canvas', event.nativeEvent)], 'canvas')
          }
          break
        }
        default:
          break
      }
    },
    [contextMenuEnabled, localSelectedViews, editorMode.type, dispatch],
  )

  const renderHighlightControls = () => {
    return selectionEnabled
      ? localHighlightedViews.map((path) => {
          const frame = MetadataUtils.getFrameInCanvasCoords(path, componentMetadata)
          if (frame == null || isInfinityRectangle(frame)) {
            return null
          }
          const isFocusableComponent = MetadataUtils.isFocusableComponent(path, componentMetadata)
          const isFocusedComponent = EP.isFocused(focusedElementPath, path)
          const color =
            isFocusableComponent || isFocusedComponent
              ? colorTheme.canvasSelectionIsolatedComponent.value
              : colorTheme.canvasSelectionPrimaryOutline.value
          return (
            <HighlightControl
              key={`highlight-control-${EP.toComponentId(path)}`}
              color={color}
              frame={frame}
              scale={scale}
              canvasOffset={canvasOffset}
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
          isSelectMode(editorMode),
          <SceneLabelControl
            maybeHighlightOnHover={maybeHighlightOnHover}
            maybeClearHighlightsOnHoverEnd={maybeClearHighlightsOnHoverEnd}
          />,
        )}
        {when(
          resizeStatus !== 'disabled',
          <>
            {inspectorFocusedControls.map((c) => (
              <RenderControlMemoized key={c.key} control={c.control} propsForControl={c.props} />
            ))}
            {inspectorHoveredControls.map((c) => (
              <RenderControlMemoized key={c.key} control={c.control} propsForControl={c.props} />
            ))}
            {when(isSelectMode(editorMode) && !anyStrategyActive, <PinLines />)}
            {when(isSelectMode(editorMode), <InsertionControls />)}
            {renderHighlightControls()}
            {renderTextEditableControls()}
            {unless(dragging, <LayoutParentControl />)}
            {when(isSelectMode(editorMode), <AbsoluteChildrenOutline />)}
            <MultiSelectOutlineControl localSelectedElements={localSelectedViews} />
            <ZeroSizedElementControls.control showAllPossibleElements={false} />
            {when(
              isSelectOrInsertMode(editorMode),
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
            {when(isSelectMode(editorMode), <DistanceGuidelineControl />)}
            <GuidelineControls />
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

SelectionAreaRectangle.displayName = 'SelectionAreaRectangle'

const ElementsOutsideVisibleAreaIndicators = React.memo(
  ({ indicators }: { indicators: ElementOutsideVisibleAreaIndicator[] }) => {
    const dispatch = useDispatch()
    const colorTheme = useColorTheme()

    const scrollTo = React.useCallback(
      (path: ElementPath) => () => {
        dispatch([scrollToElement(path, false)])
      },
      [dispatch],
    )

    function getClusterLabel(indicator: ElementOutsideVisibleAreaIndicator): string {
      return indicator.cluster > 10 ? '10+' : `${indicator.cluster}`
    }

    const indicatorSize = 22

    return (
      <>
        {indicators.map((indicator, index) => {
          const color = colorTheme.dynamicBlue.value

          return (
            <div
              key={`indicator-${index}`}
              title='Scroll to element'
              style={{
                position: 'absolute',
                left: indicator.position.x,
                top: indicator.position.y,
                transform: `rotate(${indicator.angle}rad)`,
                color: color,
                fontWeight: 'bolder',
                display: 'flex',
                alignItems: 'center',
                justifyContent: 'center',
                backgroundColor: 'transparent',
                width: indicatorSize,
                height: indicatorSize,
              }}
              onClick={scrollTo(indicator.path)}
            >
              <div
                style={{
                  display: 'flex',
                  position: 'relative',
                  alignItems: 'center',
                  justifyContent: 'center',
                  cursor: 'pointer',
                }}
              >
                <div
                  style={{
                    fontSize: 15,
                    width: 17,
                    height: 17,
                    display: 'flex',
                    marginRight: 2,
                    alignItems: 'center',
                    justifyContent: 'center',
                    paddingBottom: 1,
                    borderRadius: '100%',
                    boxShadow: `${colorTheme.canvasControlsSizeBoxShadowColor20.value} 0px 0px 3px`,
                  }}
                  css={{
                    backgroundColor: colorTheme.bg0.value,
                    '&:hover': {
                      color: 'white',
                      backgroundColor: color,
                    },
                  }}
                >
                  ←
                </div>
                {when(
                  indicator.cluster > 1,
                  <div
                    style={{
                      position: 'absolute',
                      right: -(indicator.cluster > 9 ? 20 : 9),
                      transform: `rotate(${Math.PI * 2 - indicator.angle}rad)`, // rotate the label back so it always "faces" the reading direction
                    }}
                  >
                    {getClusterLabel(indicator)}
                  </div>,
                )}
              </div>
            </div>
          )
        })}
      </>
    )
  },
)

ElementsOutsideVisibleAreaIndicators.displayName = 'ElementsOutsideVisibleAreaIndicators'
