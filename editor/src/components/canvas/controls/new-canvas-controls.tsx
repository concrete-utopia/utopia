import * as React from 'react'
import * as TP from '../../../core/shared/template-path'
import Utils from '../../../utils/utils'
import { CanvasPoint, CanvasVector, WindowPoint } from '../../../core/shared/math-utils'
import { EditorDispatch } from '../../editor/action-types'
import {
  DerivedState,
  EditorState,
  getOpenUtopiaJSXComponentsFromState,
  getOpenImportsFromState,
  getMetadata,
} from '../../editor/store/editor-state'
import {
  TemplatePath,
  InstancePath,
  Imports,
  ScenePath,
} from '../../../core/shared/project-file-types'
import { CanvasPositions, ReparentTargetIndicatorPosition } from '../canvas-types'
import { SelectModeControlContainer } from './select-mode-control-container'
import { InsertModeControlContainer } from './insert-mode-control-container'
import { HighlightControl } from './highlight-control'
import { DeselectControl } from './deselect-control'
import { colorTheme } from 'uuiui'
import { TextEditor } from '../../editor/text-editor'
import {
  setHighlightedView,
  clearHighlightedViews,
  insertDroppedImage,
  switchEditorMode,
} from '../../editor/actions/actions'
import { useEditorState } from '../../editor/store/store-hook'
import { ComponentMetadata, UtopiaJSXComponent } from '../../../core/shared/element-template'
import { MetadataUtils } from '../../../core/model/element-metadata-utils'
import { isAspectRatioLockedNew } from '../../aspect-ratio'
import { ElementContextMenu } from '../../element-context-menu'
import { CSSCursor, betterReactMemo } from 'uuiui-deps'
import { editor } from 'monaco-editor'
import { isLiveMode, EditorModes } from '../../editor/editor-modes'
import { DropTargetHookSpec, ConnectableElement, useDrop } from 'react-dnd'
import { FileBrowserItemProps } from '../../filebrowser/fileitem'
import { forceNotNull } from '../../../core/shared/optional-utils'
import { flatMapArray } from '../../../core/shared/array-utils'
import { targetRespectsLayout } from '../../../core/layout/layout-helpers'
import { isFeatureEnabled } from '../../../utils/feature-switches'
import { useRecoilState } from 'recoil'
import { layoutHoveredState } from '../../../core/shared/inspector-recoil'
import { MiniNavigator } from '../../mini-navigator/mini-navigator'
import { shallowEqual } from '../../../core/shared/equality-utils'
import { usePrevious } from '../../editor/hook-utils'
import { KeysPressed } from '../../../utils/keyboard'
import { LayoutTargetableProp } from '../../../core/layout/layout-helpers-new'
import utils from '../../../utils/utils'
import useSize from '@react-hook/size'
import { TextEditingArea } from './text-editing-input-area'

export type ResizeStatus = 'disabled' | 'noninteractive' | 'enabled'

export interface ControlProps {
  selectedViews: Array<TemplatePath>
  highlightedViews: Array<TemplatePath>
  rootComponents: Array<UtopiaJSXComponent>
  componentMetadata: ComponentMetadata[]
  imports: Imports
  hiddenInstances: Array<TemplatePath>
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
  elementsThatRespectLayout: Array<TemplatePath>
  propertyTargetOptions: Array<LayoutTargetableProp>
  propertyTargetSelectedIndex: number
  setTargetOptionsArray: (newArray: Array<LayoutTargetableProp>) => void
}

interface NewCanvasControlsProps {
  windowToCanvasPosition: (event: MouseEvent) => CanvasPositions
  cursor: CSSCursor
}

function useArrayAndIndex(defaultTargets: LayoutTargetableProp[]) {
  const [targets, setTargets] = React.useState<LayoutTargetableProp[]>(defaultTargets)
  const [targetIndex, setTargetIndex] = React.useState(0)

  function incrementTargetIndex() {
    if (targetIndex < targets.length - 1) {
      setTargetIndex(targetIndex + 1)
    } else {
      setTargetIndex(0)
    }
  }

  function setTargetsResetIndex(newTargets: LayoutTargetableProp[]) {
    if (!shallowEqual(targets, newTargets)) {
      setTargets(newTargets)
      setTargetIndex(0)
    }
  }

  return [targets, targetIndex, setTargetsResetIndex, incrementTargetIndex] as const
}

function useTargetSelector(defaultTargets: LayoutTargetableProp[], keysPressed: KeysPressed) {
  const [targets, targetIndex, setTargets, incrementTargetIndex] = useArrayAndIndex(defaultTargets)

  const shiftPressed = keysPressed.shift
  const previousShiftPressed = usePrevious(shiftPressed)

  if (shiftPressed && !previousShiftPressed) {
    incrementTargetIndex()
  }

  return [targets, targetIndex, setTargets] as const
}

export const NewCanvasControls = betterReactMemo(
  'NewCanvasControls',
  (props: NewCanvasControlsProps) => {
    const canvasControlProps = useEditorState((store) => ({
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
    }))

    const [targets, targetIndex, setTargetOptionsArray] = useTargetSelector(
      ['Width', 'minWidth', 'maxWidth'],
      canvasControlProps.editor.keysPressed,
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

    const containerRef = React.useRef(null)
    const [width, height] = useSize(containerRef)

    const selectedViews = useEditorState((store) => store.editor.selectedViews)
    const componentMetadata = useEditorState((store) => store.editor.jsxMetadataKILLME)
    const selectedScene = selectedViews.length > 0 ? TP.scenePathForPath(selectedViews[0]) : null
    const sceneSize =
      selectedScene == null
        ? null
        : MetadataUtils.getFrameInCanvasCoords(selectedScene, componentMetadata)
    const codePaneVisible = useEditorState(
      (store) => !store.editor.interfaceDesigner.codePaneVisible,
    )

    if (isLiveMode(canvasControlProps.editor.mode) && !canvasControlProps.editor.keysPressed.cmd) {
      return null
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
              position: 'absolute',
              top: 0,
              left: 0,
              transform: 'translate3d(0, 0, 0)',
              width: `100%`,
              height: `100%`,
              zoom: canvasControlProps.scale >= 1 ? `${canvasControlProps.scale * 100}%` : 1,
              cursor: props.cursor,
            }}
          >
            <div
              ref={containerRef}
              style={{
                position: 'absolute',
                top: 0,
                left: 0,
                width: `${canvasControlProps.scale < 1 ? 100 / canvasControlProps.scale : 100}%`,
                height: `${canvasControlProps.scale < 1 ? 100 / canvasControlProps.scale : 100}%`,
                transformOrigin: 'top left',
                transform:
                  canvasControlProps.scale < 1 ? `scale(${canvasControlProps.scale}) ` : '',
              }}
            >
              <NewCanvasControlsClass
                windowToCanvasPosition={props.windowToCanvasPosition}
                {...canvasControlProps}
                propertyTargetOptions={targets}
                propertyTargetSelectedIndex={targetIndex}
                setTargetOptionsArray={setTargetOptionsArray}
                xrayView={false}
                selectedScene={selectedScene}
              />
            </div>
            <ElementContextMenu contextMenuInstance='context-menu-canvas' />
          </div>
          {isFeatureEnabled('Mini Navigator') ? <MiniNavigator /> : null}
          {isFeatureEnabled('Hierarchy View') && codePaneVisible && (
            <div
              key={'xrayview'}
              style={{
                position: 'absolute',
                bottom: 0,
                right: 0,
                width: width / 2,
                height: height,
                backgroundColor: 'white',
                transformOrigin: 'top left',
                outline: '1px solid #e6e6e6',
                transform:
                  canvasControlProps.scale < 1 ? `scale(${canvasControlProps.scale}) ` : '',
                overflow: 'hidden',
              }}
            >
              ☠︎ X-RAY Mode ☠︎
              <NewCanvasControlsClass
                key={'xrayview-controls'}
                // eslint-disable-next-line react/jsx-no-bind
                windowToCanvasPosition={() => ({
                  windowPosition: utils.zeroPoint as WindowPoint,
                  canvasPositionRaw: utils.zeroPoint as CanvasPoint,
                  canvasPositionRounded: utils.zeroPoint as CanvasPoint,
                })}
                {...canvasControlProps}
                canvasOffset={canvasControlProps.canvasOffset}
                xrayView={true}
                selectedScene={selectedScene}
                scale={canvasControlProps.scale}
                propertyTargetOptions={[]}
                propertyTargetSelectedIndex={0}
                setTargetOptionsArray={Utils.NO_OP}
              />
            </div>
          )}
        </>
      )
    }
  },
)
NewCanvasControls.displayName = 'NewCanvasControls'

interface NewCanvasControlsClassProps {
  editor: EditorState
  derived: DerivedState
  dispatch: EditorDispatch
  canvasOffset: CanvasPoint
  animationEnabled: boolean
  windowToCanvasPosition: (event: MouseEvent) => CanvasPositions
  propertyTargetOptions: Array<LayoutTargetableProp>
  propertyTargetSelectedIndex: number
  setTargetOptionsArray: (newArray: Array<LayoutTargetableProp>) => void
  xrayView: boolean
  selectedScene: ScenePath | null
  scale: number
}

export type SelectModeState =
  | 'move'
  | 'translate'
  | 'resize'
  | 'reparentMove'
  | 'reparentGlobal'
  | 'reparentLocal'

const NewCanvasControlsClass = (props: NewCanvasControlsClassProps) => {
  const [layoutSectionHovered] = useRecoilState(layoutHoveredState)
  const [selectModeState, setSelectModeState] = React.useState<SelectModeState>('move')

  const selectionEnabled =
    props.editor.canvas.selectionControlsVisible &&
    !props.editor.keysPressed['z'] &&
    props.editor.canvas.textEditor == null

  const componentMetadata = getMetadata(props.editor)
  const { dispatch } = props
  const isResizing =
    props.editor.canvas.dragState != null &&
    props.editor.canvas.dragState.type === 'RESIZE_DRAG_STATE' &&
    props.editor.canvas.dragState.drag != null
  const isDragging =
    props.editor.canvas.dragState != null &&
    props.editor.canvas.dragState.type === 'MOVE_DRAG_STATE' &&
    props.editor.canvas.dragState.drag != null
  const maybeHighlightOnHover = React.useCallback(
    (target: TemplatePath): void => {
      if (selectionEnabled && !isDragging && !isResizing) {
        dispatch([setHighlightedView(target)], 'canvas')
      }
    },
    [dispatch, selectionEnabled, isDragging, isResizing],
  )

  const maybeClearHighlightsOnHoverEnd = React.useCallback((): void => {
    if (selectionEnabled && !isDragging && !isResizing) {
      dispatch([clearHighlightedViews()], 'canvas')
    }
  }, [selectionEnabled, isDragging, isResizing, dispatch])

  const getResizeStatus = () => {
    const selectedViews = props.derived.canvas.transientState.selectedViews
    if (props.editor.canvas.textEditor != null || props.editor.keysPressed['z']) {
      return 'disabled'
    }
    if (props.editor.keysPressed['cmd'] === true) {
      return 'noninteractive'
    }
    const anyIncomprehensibleElementsSelected = selectedViews.some((selectedView) => {
      if (TP.isScenePath(selectedView)) {
        return false
      }

      const possibleMetadata = MetadataUtils.getElementByInstancePathMaybe(
        componentMetadata,
        selectedView,
      )
      return (
        possibleMetadata == null ||
        MetadataUtils.dynamicPathToStaticPath(componentMetadata, selectedView) == null
      )
    })
    if (anyIncomprehensibleElementsSelected) {
      return 'disabled'
    }
    return 'enabled'
  }

  const elementsThatRespectLayout = useEditorState((store) => {
    return flatMapArray((view) => {
      if (TP.isScenePath(view)) {
        const scene = MetadataUtils.findSceneByTemplatePath(store.editor.jsxMetadataKILLME, view)
        if (scene != null) {
          return [view, ...scene.rootElements.map((e) => e.templatePath)]
        } else {
          return [view]
        }
      } else {
        return [view]
      }
    }, store.derived.navigatorTargets).filter((view) => targetRespectsLayout(view, store.editor))
  })

  const renderModeControlContainer = () => {
    const fallbackTransientState = props.derived.canvas.transientState
    const targets = fallbackTransientState.selectedViews
    const elementAspectRatioLocked = targets.every((target) => {
      if (TP.isScenePath(target)) {
        return false
      }
      const possibleElement = MetadataUtils.getElementByInstancePathMaybe(componentMetadata, target)
      if (possibleElement == null) {
        return false
      } else {
        return isAspectRatioLockedNew(possibleElement)
      }
    })
    const imports = getOpenImportsFromState(props.editor)
    const imageMultiplier: number | null = MetadataUtils.getImageMultiplier(
      imports,
      componentMetadata,
      targets,
    )
    const rootComponents = getOpenUtopiaJSXComponentsFromState(props.editor)
    const controlProps: ControlProps = {
      selectedViews: targets,
      highlightedViews: fallbackTransientState.highlightedViews,
      rootComponents: rootComponents,
      componentMetadata: componentMetadata,
      imports: imports,
      hiddenInstances: props.editor.hiddenInstances,
      highlightsEnabled: props.editor.canvas.highlightsEnabled,
      canvasOffset: props.canvasOffset,
      scale: props.scale,
      dispatch: props.dispatch,
      resizeStatus: getResizeStatus(),
      elementAspectRatioLocked: elementAspectRatioLocked,
      imageMultiplier: imageMultiplier,
      windowToCanvasPosition: props.windowToCanvasPosition,
      cmdKeyPressed: props.editor.keysPressed['cmd'] ?? false,
      showAdditionalControls: props.editor.interfaceDesigner.additionalControls,
      elementsThatRespectLayout: elementsThatRespectLayout,
      propertyTargetOptions: props.propertyTargetOptions,
      propertyTargetSelectedIndex: props.propertyTargetSelectedIndex,
      setTargetOptionsArray: props.setTargetOptionsArray,
    }
    const dragState = props.editor.canvas.dragState
    const reparentTargetPositions: Array<ReparentTargetIndicatorPosition> =
      props.derived.canvas.transientState.reparentTargetPositions

    if (props.xrayView) {
      return (
        <SelectModeControlContainer
          {...controlProps}
          keysPressed={props.editor.keysPressed}
          windowToCanvasPosition={props.windowToCanvasPosition}
          isDragging={false}
          isResizing={false}
          selectionEnabled={selectionEnabled}
          maybeHighlightOnHover={maybeHighlightOnHover}
          maybeClearHighlightsOnHoverEnd={maybeClearHighlightsOnHoverEnd}
          duplicationState={null}
          dragState={null}
          showAdditionalControls={props.editor.interfaceDesigner.additionalControls}
          xrayMode={true}
          selectedScene={props.selectedScene}
          layoutInspectorSectionHovered={false}
          selectModeState={selectModeState}
          setSelectModeState={setSelectModeState}
          reparentTargetPositions={reparentTargetPositions}
        />
      )
    }
    switch (props.editor.mode.type) {
      case 'select':
      case 'live': {
        return (
          <SelectModeControlContainer
            {...controlProps}
            keysPressed={props.editor.keysPressed}
            windowToCanvasPosition={props.windowToCanvasPosition}
            isDragging={isDragging}
            isResizing={isResizing}
            selectionEnabled={selectionEnabled}
            maybeHighlightOnHover={maybeHighlightOnHover}
            maybeClearHighlightsOnHoverEnd={maybeClearHighlightsOnHoverEnd}
            duplicationState={props.editor.canvas.duplicationState}
            dragState={
              dragState?.type === 'MOVE_DRAG_STATE' || dragState?.type === 'RESIZE_DRAG_STATE'
                ? dragState
                : null
            }
            showAdditionalControls={props.editor.interfaceDesigner.additionalControls}
            layoutInspectorSectionHovered={layoutSectionHovered}
            selectModeState={selectModeState}
            setSelectModeState={setSelectModeState}
            xrayMode={false}
            selectedScene={null}
            reparentTargetPositions={reparentTargetPositions}
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
            projectId={props.editor.id}
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
    const highlightedViews = props.derived.canvas.transientState.highlightedViews
    return selectionEnabled
      ? highlightedViews.map((path) => {
          const frame = MetadataUtils.getFrameInCanvasCoords(path, componentMetadata)
          if (frame == null) {
            return null
          }
          let striped = false
          let color = colorTheme.canvasSelectionPrimaryOutline.value
          if (
            props.editor.canvas.dragState?.type === 'MOVE_DRAG_STATE' &&
            props.editor.canvas.dragState.reparent &&
            isFeatureEnabled('Nearby Reparent Target Highlight')
          ) {
            const isNewParent = props.derived.canvas.transientState.selectedViews.some((view) =>
              TP.pathsEqual(TP.parentPath(view), path),
            )
            if (isNewParent) {
              striped = true
              color = colorTheme.red.o(70).value
            } else {
              color = colorTheme.red.o(50).value
            }
          } else {
            color = TP.isScenePath(path)
              ? colorTheme.canvasSelectionSceneOutline.value
              : colorTheme.canvasSelectionPrimaryOutline.value
          }
          const zOffset =
            isFeatureEnabled('Hierarchy View') && !props.editor.interfaceDesigner.codePaneVisible
              ? (TP.depth(path) - 1) * 25
              : null

          return (
            <HighlightControl
              key={`highlight-control-${TP.toComponentId(path)}`}
              color={color}
              frame={frame}
              scale={props.editor.canvas.scale}
              canvasOffset={props.canvasOffset}
              striped={striped}
              zOffset={zOffset}
            />
          )
        })
      : []
  }

  const renderTextEditor = (target: InstancePath) => {
    const dragState = props.editor.canvas.dragState
    const selectedViews = props.derived.canvas.transientState.selectedViews
    if (dragState != null || selectedViews.length !== 1) {
      return null
    } else {
      const element = MetadataUtils.getElementByInstancePathMaybe(componentMetadata, target)
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

  const renderDeselectControl = () => {
    return selectionEnabled ? (
      <DeselectControl mode={props.editor.mode} dispatch={props.dispatch} />
    ) : null
  }

  const buttonStyle = (active: boolean) => ({
    borderRadius: 5,
    border: `1px solid ${colorTheme.neutralBorder.value}`,
    backgroundColor: active ? colorTheme.primary.value : '#fefefe',
  })

  const renderToolbar = () => {
    return (
      <div
        style={{
          position: 'absolute',
          bottom: 40,
          left: 0,
          width: 110,
        }}
      >
        <div
          style={buttonStyle(selectModeState === 'move')}
          onClick={() => setSelectModeState('move')}
        >
          → move
        </div>
        <div
          style={buttonStyle(selectModeState === 'translate')}
          onClick={() => setSelectModeState('translate')}
        >
          ↯ translate
        </div>
        <div
          style={buttonStyle(selectModeState === 'resize')}
          onClick={() => setSelectModeState('resize')}
        >
          ↕️ resize
        </div>
        <div
          style={buttonStyle(selectModeState === 'reparentMove')}
          onClick={() => setSelectModeState('reparentMove')}
        >
          ↲ reparent(move)
        </div>
        <div
          style={buttonStyle(selectModeState === 'reparentGlobal')}
          onClick={() => setSelectModeState('reparentGlobal')}
        >
          ↲ reparent(global)
        </div>
        <div
          style={buttonStyle(selectModeState === 'reparentLocal')}
          onClick={() => setSelectModeState('reparentLocal')}
        >
          ↲ reparent(local)
        </div>
      </div>
    )
  }

  const textEditor =
    props.editor.canvas.textEditor != null
      ? renderTextEditor(props.editor.canvas.textEditor.templatePath)
      : null

  return (
    <div
      className='new-canvas-controls-container'
      style={{
        position: 'relative',
        width: '100%',
        height: '100%',
        transform: props.xrayView ? 'rotateY(35deg) rotateX(15deg) rotateZ(0deg)' : 'none',
        transformStyle: props.xrayView ? 'preserve-3d' : undefined,
      }}
    >
      {renderDeselectControl()}
      {renderModeControlContainer()}
      {renderHighlightControls()}
      {textEditor}
      {isFeatureEnabled('Toolbar For Controls') ? renderToolbar() : null}
      {isFeatureEnabled('Edit Simple Text') ? <TextEditingArea /> : null}
    </div>
  )
}
