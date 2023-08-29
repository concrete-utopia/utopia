import React from 'react'
import Draggable from 'react-draggable'
import type { DraggableEventHandler } from 'react-draggable'
import { CodeEditorPane, ResizableRightPane } from './design-panel-root'
import { LeftPaneComponent } from '../navigator/left-pane'
import {
  AlwaysTrue,
  atomWithPubSub,
  usePubSubAtom,
  usePubSubAtomReadOnly,
} from '../../core/shared/atom-with-pub-sub'
import type { Size, WindowPoint, WindowRectangle } from '../../core/shared/math-utils'
import {
  rectContainsPoint,
  rectContainsPointInclusive,
  rectanglesEqual,
  windowPoint,
  windowRectangle,
} from '../../core/shared/math-utils'
import { CanvasSizeAtom, LeftPaneDefaultWidth } from '../editor/store/editor-state'
import { mapArrayToDictionary, mapDropNulls, stripNulls } from '../../core/shared/array-utils'
import { UtopiaTheme, useColorTheme } from '../../uuiui'
import { when } from '../../utils/react-conditionals'
import type { Direction } from 're-resizable/lib/resizer'

const TitleHeight = 28
export type Menu = 'inspector' | 'navigator'
export type Pane = 'code-editor' | 'preview'
export const GapBetweenPanels = 10

const SizeConstraints: {
  [key: string]: {
    resize: {
      minWidth: number
      maxWidth: number
      snap: {
        x?: number[]
        y?: number[]
      } | null
    } | null
    defaultSize: Size
  }
} = {
  inspector: {
    resize: {
      minWidth: UtopiaTheme.layout.inspectorSmallWidth,
      maxWidth: UtopiaTheme.layout.inspectorLargeWidth,
      snap: { x: [UtopiaTheme.layout.inspectorSmallWidth, UtopiaTheme.layout.inspectorLargeWidth] },
    },

    defaultSize: { width: UtopiaTheme.layout.inspectorSmallWidth, height: 0 },
  },
  navigator: {
    resize: {
      minWidth: 240,
      maxWidth: 300,
      snap: null,
    },
    defaultSize: { width: LeftPaneDefaultWidth, height: 0 },
  },
  'code-editor': {
    resize: null,
    defaultSize: {
      width: 500,
      height: 600,
    },
  },
}

type PanelName = 'leftMenu1' | 'leftMenu2' | 'rightMenu1' | 'rightMenu2'
const Panels: Array<PanelName> = ['leftMenu1', 'leftMenu2', 'rightMenu1', 'rightMenu2']

// TODO store menu/pane height
interface PanelData {
  menuOrPane: Menu | Pane
  height: number | null
}
const DefaultPanels: {
  [key in PanelName]: Array<PanelData>
} = {
  leftMenu1: [{ menuOrPane: 'code-editor', height: null }],
  leftMenu2: [{ menuOrPane: 'navigator', height: null }],
  rightMenu1: [{ menuOrPane: 'inspector', height: null }],
  rightMenu2: [],
}

const DefaultSizes: { [key in PanelName]: WindowRectangle } = {
  leftMenu1: windowRectangle({ x: 0, y: 0, width: 500, height: 600 }),
  leftMenu2: windowRectangle({ x: 0, y: 0, width: LeftPaneDefaultWidth, height: 0 }),
  rightMenu1: windowRectangle({ x: 0, y: 0, width: 255, height: 0 }),
  rightMenu2: windowRectangle({ x: 0, y: 0, width: 0, height: 0 }),
}
export const FloatingPanelSizesAtom = atomWithPubSub({
  key: 'FloatingPanelSizesAtom',
  defaultValue: DefaultSizes,
})

function isMenuContainingPanel(menusOrPanes: Array<PanelData>): boolean {
  return menusOrPanes.some(
    (value) => value.menuOrPane === 'inspector' || value.menuOrPane === 'navigator',
  )
}

export const FloatingPanelsContainer = React.memo(() => {
  const [panelsData, setPanelsData] =
    React.useState<{ [key in PanelName]: Array<PanelData> }>(DefaultPanels)
  const canvasSize = usePubSubAtomReadOnly(CanvasSizeAtom, AlwaysTrue)
  const [panelFrames, setPanelFrames] =
    usePubSubAtom<{ [key in PanelName]: WindowRectangle }>(FloatingPanelSizesAtom)

  const getUpdatedPanelSizes = React.useCallback(
    (
      currentPanelsData: { [key in PanelName]: Array<PanelData> },
      currentFrames: { [key in PanelName]: WindowRectangle },
    ) => {
      return mapArrayToDictionary(
        Panels,
        (p) => p,
        (p, i) => {
          let height = canvasSize.height
          if (!isMenuContainingPanel(currentPanelsData[p]) && currentPanelsData[p].length === 1) {
            // code pane!
            const codeEditorHeight = currentPanelsData[p][0].height ?? canvasSize.height
            height = Math.min(canvasSize.height, codeEditorHeight)
          }
          let width = currentFrames[p].width
          if (currentPanelsData[p].length === 0) {
            width = 0
          } else {
            if (currentFrames[p].width === 0) {
              // give default size to panel
              width = SizeConstraints[currentPanelsData[p][0].menuOrPane].defaultSize.width
            } else {
              const possibleConstraints = mapDropNulls(
                (v) => SizeConstraints[v.menuOrPane].resize,
                currentPanelsData[p],
              )
              const minWidth = Math.max(...possibleConstraints.map((v) => v.minWidth))
              const maxWidth = Math.min(...possibleConstraints.map((v) => v.maxWidth))
              if (currentFrames[p].width > maxWidth || currentFrames[p].width < minWidth) {
                width = maxWidth // TODO maybe some of the default widths?
              }
            }
          }

          return windowRectangle({
            x: currentFrames[p].x,
            y: currentFrames[p].y,
            width: width,
            height: height,
          })
        },
      )
    },
    [canvasSize],
  )

  const getUpdatedPanelPositions = React.useCallback(
    (
      currentPanelsData: { [key in PanelName]: Array<PanelData> },
      currentFrames: { [key in PanelName]: WindowRectangle },
    ) => {
      const withPanelSizeUpdates = getUpdatedPanelSizes(currentPanelsData, currentFrames)
      return mapArrayToDictionary(
        Panels,
        (p) => p,
        (p, i) => {
          let x = 0
          switch (p) {
            case 'leftMenu2':
              x =
                withPanelSizeUpdates.leftMenu1.width +
                (currentPanelsData.leftMenu1.length > 0 ? GapBetweenPanels : 0)
              break
            case 'rightMenu1':
              x =
                canvasSize.width -
                withPanelSizeUpdates.rightMenu1.width -
                withPanelSizeUpdates.rightMenu2.width -
                (currentPanelsData.rightMenu2.length > 0 ? GapBetweenPanels : 0)
              break
            case 'rightMenu2':
              x = canvasSize.width - withPanelSizeUpdates.rightMenu2.width
              break
            default:
              break
          }

          return windowRectangle({
            x: x,
            y: 0,
            width: withPanelSizeUpdates[p].width,
            height: withPanelSizeUpdates[p].height,
          })
        },
      )
    },
    [canvasSize, getUpdatedPanelSizes],
  )

  const updateColumn = React.useCallback(
    (menuOrPane: Menu | Pane, currentPanel: PanelName, newPosition: WindowPoint) => {
      const { newPanel, switchWithPanel } = ((): {
        newPanel: PanelName | null
        switchWithPanel: PanelName | null
      } => {
        const EdgeDropAreaWidth = 60
        const outerDropAreas: Array<{
          targetPanel: PanelName
          frame: WindowRectangle
        }> = [
          // between the left edge and the first left menu
          {
            targetPanel: 'leftMenu1',
            frame: windowRectangle({
              x: 0,
              y: 0,
              width: EdgeDropAreaWidth, //GapBetweenPanels,
              height: canvasSize.height,
            }),
          },
          // right side of the left menu
          {
            targetPanel: 'leftMenu2',
            frame: windowRectangle({
              x: panelFrames.leftMenu2.x + panelFrames.leftMenu2.width,
              y: 0,
              width: 80,
              height: canvasSize.height,
            }),
          },
          // left side of the right menu
          {
            targetPanel: 'rightMenu1',
            frame: windowRectangle({
              x: panelFrames.rightMenu1.x - 80,
              y: 0,
              width: 80,
              height: canvasSize.height,
            }),
          },
          // between the right edge and the last right menu
          {
            targetPanel: 'rightMenu2',
            frame: windowRectangle({
              x: panelFrames.rightMenu2.x + panelFrames.rightMenu2.width - EdgeDropAreaWidth, //GapBetweenPanels,
              y: 0,
              width:
                canvasSize.width -
                panelFrames.rightMenu2.x +
                panelFrames.rightMenu2.width +
                EdgeDropAreaWidth, //GapBetweenPanels,
              height: canvasSize.height,
            }),
          },
        ]

        const droppedToOutsideOfAColumn = outerDropAreas.find((area) =>
          rectContainsPointInclusive(area.frame, newPosition),
        )

        // maybe switch with existing panels if dragging to edge or when dropped to the side of a column
        let shouldSwitchPanel: PanelName | null = null
        if (droppedToOutsideOfAColumn != null) {
          if (
            droppedToOutsideOfAColumn.targetPanel === 'leftMenu1' &&
            panelsData.leftMenu1.length > 0 &&
            (panelsData.leftMenu2.length === 0 ||
              (currentPanel === 'leftMenu2' && panelsData.leftMenu2.length === 1))
          ) {
            // dragging to far left when something is already there
            shouldSwitchPanel = 'leftMenu2'
          }
          if (
            droppedToOutsideOfAColumn.targetPanel === 'leftMenu2' &&
            panelsData.leftMenu2.length > 0 &&
            (panelsData.leftMenu1.length === 0 ||
              (currentPanel === 'leftMenu1' && panelsData.leftMenu1.length === 1))
          ) {
            // dragging to the right of the left panel 2
            shouldSwitchPanel = 'leftMenu1'
          }

          if (
            droppedToOutsideOfAColumn.targetPanel === 'rightMenu1' &&
            panelsData.rightMenu1.length > 0 &&
            (panelsData.rightMenu2.length === 0 ||
              (currentPanel === 'rightMenu2' && panelsData.rightMenu2.length === 1))
          ) {
            // dragging to the left of the right panel 1
            shouldSwitchPanel = 'rightMenu2'
          }

          if (
            droppedToOutsideOfAColumn.targetPanel === 'rightMenu2' &&
            panelsData.rightMenu2.length > 0 &&
            (panelsData.rightMenu1.length === 0 ||
              (currentPanel === 'rightMenu1' && panelsData.rightMenu1.length === 1))
          ) {
            // dragging to far right when something is already there
            shouldSwitchPanel = 'rightMenu1'
          }

          return {
            newPanel: droppedToOutsideOfAColumn.targetPanel,
            switchWithPanel: shouldSwitchPanel,
          }
        }

        return {
          newPanel:
            (Panels.reverse() as Array<PanelName>).find((name) =>
              rectContainsPoint(panelFrames[name], newPosition),
            ) ?? null,
          switchWithPanel: null,
        }
      })()

      if (newPanel != null && (currentPanel != newPanel || switchWithPanel != null)) {
        const remainingMenusAndPanes = panelsData[currentPanel]
          .filter((panelData) => panelData.menuOrPane !== menuOrPane)
          .map((v) => ({ ...v, height: null }))

        if (switchWithPanel != null) {
          const newPanelsData = {
            ...panelsData,
            [currentPanel]: remainingMenusAndPanes,
            [switchWithPanel]: panelsData[newPanel].filter(
              (panelData) => panelData.menuOrPane !== menuOrPane,
            ),
            [newPanel]: [{ menuOrPane: menuOrPane, height: null }],
          }
          setPanelsData(newPanelsData)

          const newFrameData = {
            ...panelFrames,
            [switchWithPanel]: panelFrames[newPanel],
            [newPanel]: panelFrames[switchWithPanel],
          }
          const updatedPanelPositions = getUpdatedPanelPositions(newPanelsData, newFrameData)
          setPanelFrames(updatedPanelPositions)
        } else {
          const newPanelsData = {
            ...panelsData,
            [newPanel]: [
              ...panelsData[newPanel].map((v) => ({ menuOrPane: v.menuOrPane, height: null })),
              { menuOrPane: menuOrPane, height: null },
            ],
            [currentPanel]: remainingMenusAndPanes,
          }
          setPanelsData(newPanelsData)
        }
      }
    },
    [panelFrames, panelsData, setPanelsData, canvasSize, setPanelFrames, getUpdatedPanelPositions],
  )

  const updateSize = React.useCallback(
    (
      menuOrPane: Menu | Pane,
      currentPanel: PanelName,
      direction: Direction,
      width: number,
      height: number,
    ) => {
      if (direction === 'left' || direction === 'right') {
        let frame = panelFrames[currentPanel]
        frame.width = width
        const updatedPanelFrames = {
          ...panelFrames,
          [currentPanel]: frame,
        }
        const withUpdatedPositions = getUpdatedPanelPositions(panelsData, updatedPanelFrames)
        setPanelFrames(withUpdatedPositions)
      } else {
        const newPanelsData = {
          ...panelsData,
          [currentPanel]: panelsData[currentPanel].map((v) => {
            if (menuOrPane === v.menuOrPane) {
              return { menuOrPane: menuOrPane, height: height }
            } else {
              const remainingSpace =
                canvasSize.height -
                height -
                (panelsData[currentPanel].length + 1) * GapBetweenPanels
              const newHeight = remainingSpace / (panelsData[currentPanel].length - 1)
              return {
                menuOrPane: v.menuOrPane,
                height: newHeight,
              }
            }
          }),
        }
        setPanelsData(newPanelsData)
      }
    },
    [panelFrames, panelsData, setPanelFrames, setPanelsData, getUpdatedPanelPositions, canvasSize],
  )

  React.useEffect(() => {
    const framesWithUpdatedPosition = getUpdatedPanelPositions(panelsData, panelFrames)
    if (
      (Object.keys(framesWithUpdatedPosition) as Array<PanelName>).some(
        (name) => !rectanglesEqual(framesWithUpdatedPosition[name], panelFrames[name]),
      )
    ) {
      setPanelFrames(framesWithUpdatedPosition)
    }
  }, [canvasSize, panelsData, panelFrames, getUpdatedPanelPositions, setPanelFrames])

  return (
    <>
      <FloatingPanel
        key={'leftMenu1'}
        panelName={'leftMenu1'}
        frame={panelFrames.leftMenu1}
        menusAndPanes={panelsData.leftMenu1}
        onResizeStop={updateSize}
        updateColumn={updateColumn}
        alignment='left'
      />
      <FloatingPanel
        key={'leftMenu2'}
        panelName={'leftMenu2'}
        frame={panelFrames.leftMenu2}
        menusAndPanes={panelsData.leftMenu2}
        onResizeStop={updateSize}
        updateColumn={updateColumn}
        alignment='left'
      />
      <FloatingPanel
        key={'rightMenu1'}
        panelName={'rightMenu1'}
        frame={panelFrames.rightMenu1}
        menusAndPanes={panelsData.rightMenu1}
        onResizeStop={updateSize}
        updateColumn={updateColumn}
        alignment='right'
      />
      <FloatingPanel
        key={'rightMenu2'}
        panelName={'rightMenu2'}
        frame={panelFrames.rightMenu2}
        menusAndPanes={panelsData.rightMenu2}
        onResizeStop={updateSize}
        updateColumn={updateColumn}
        alignment='right'
      />
      {/* {Panels.map((key, i) => (
        <FloatingPanel
          key={key}
          panelName={key}
          position={positions[key]}
          menusAndPanes={panelsData[key].menusAndPanes}
          size={panelsData[key].size}
          updateMenuSize={updateSize}
          updateColumn={updateColumn}
        />
      ))} */}
    </>
  )
})

interface FloatingPanelProps {
  panelName: PanelName
  menusAndPanes: Array<PanelData>
  frame: WindowRectangle
  alignment: 'left' | 'right'
  updateColumn: (menuOrPane: Menu | Pane, currentPanel: PanelName, newPosition: WindowPoint) => void
  onResizeStop: (
    menuOrPane: Menu | Pane,
    currentPanel: PanelName,
    direction: Direction,
    width: number,
    height: number,
  ) => void
}

export const FloatingPanel = React.memo<FloatingPanelProps>((props) => {
  const { alignment, panelName, menusAndPanes, frame, updateColumn, onResizeStop } = props
  const canvasSize = usePubSubAtomReadOnly(CanvasSizeAtom, AlwaysTrue)

  const horizontalPosition = React.useMemo(() => {
    if (alignment === 'right') {
      return { right: canvasSize.width - frame.x - frame.width }
    }
    return { left: frame.x }
  }, [canvasSize, frame, alignment])

  const panelHeight = React.useMemo(() => {
    if (isMenuContainingPanel(menusAndPanes)) {
      return `calc(100% - ${GapBetweenPanels * 2}px)`
    } else {
      return `min(calc(100% - ${GapBetweenPanels * 2}px), ${frame.height}px)`
    }
  }, [menusAndPanes, frame])

  const [isDraggingOrResizing, setIsDraggingOrResizing] = React.useState<Menu | Pane | null>(null)
  const onDragStart = React.useCallback<(menuOrPane: Menu | Pane) => DraggableEventHandler>(
    (menuOrPane: Menu | Pane) => {
      return (e, data) => {
        setIsDraggingOrResizing(menuOrPane)
      }
    },
    [setIsDraggingOrResizing],
  )

  const onDragStop = React.useCallback<(menuOrPane: Menu | Pane) => DraggableEventHandler>(
    (menuOrPane: Menu | Pane) => {
      return (e, data) => {
        updateColumn(
          menuOrPane,
          panelName,
          windowPoint({ x: (e as any).clientX, y: (e as any).clientY }),
        )
        setIsDraggingOrResizing(null)
      }
    },
    [panelName, updateColumn, setIsDraggingOrResizing],
  )

  const resizeMinMaxSnap = React.useMemo(() => {
    const possibleConstraints = mapDropNulls(
      (v) => SizeConstraints[v.menuOrPane].resize,
      menusAndPanes,
    )
    const minWidth = Math.max(...possibleConstraints.map((v) => v.minWidth), 20)
    const maxWidth = Math.min(...possibleConstraints.map((v) => v.maxWidth), canvasSize.width)
    const snap = stripNulls(possibleConstraints.map((v) => v.snap))[0] // TODO what happens if there are multiple conflicting snapping menus

    return { minWidth, maxWidth, snap }
  }, [menusAndPanes, canvasSize])

  const resizeStopEventHandler = React.useCallback<
    (menuOrPane: Menu | Pane, direction: Direction, width: number, height: number) => void
  >(
    (menuOrPane: Menu | Pane, direction: Direction, width: number, height: number) =>
      onResizeStop(menuOrPane, panelName, direction, width, height),
    [panelName, onResizeStop],
  )

  const menuHeight = React.useMemo(
    () =>
      (canvasSize.height - (menusAndPanes.length + 1) * GapBetweenPanels) / menusAndPanes.length,
    [menusAndPanes, canvasSize],
  )

  const resizeConfig = React.useMemo(
    () => ({
      enable: {
        left: alignment === 'right',
        right: alignment === 'left',
        bottom: !isMenuContainingPanel(menusAndPanes) || menusAndPanes.length > 1,
      },
      minWidth: resizeMinMaxSnap.minWidth,
      maxWidth: resizeMinMaxSnap.maxWidth,
      minHeight: TitleHeight,
      maxHeight: canvasSize.height - menusAndPanes.length * TitleHeight,
      snap: resizeMinMaxSnap.snap,
    }),
    [alignment, canvasSize, resizeMinMaxSnap, menusAndPanes],
  )

  const draggableCommonProps = React.useMemo(
    () => ({
      position: { x: 0, y: 0 }, // this is needed to control the position
      handle: '.handle',
    }),
    [],
  )

  return (
    <>
      <div
        className={panelName}
        id={`floating-panel-${panelName}`}
        style={{
          position: 'absolute',
          height: panelHeight,
          top: frame.y,
          margin: 10,
          ...horizontalPosition, // left: x or right: y
        }}
      >
        {menusAndPanes.map((value, i) => {
          switch (value.menuOrPane) {
            case 'code-editor':
              return (
                <Draggable
                  {...draggableCommonProps}
                  key='code-editor'
                  onStop={onDragStop('code-editor')}
                  onStart={onDragStart('code-editor')}
                >
                  <div
                    style={{
                      width: '100%',
                      height: value.height ?? menuHeight,
                      marginTop: i >= 1 ? GapBetweenPanels : 0,
                      position: isDraggingOrResizing === 'code-editor' ? 'relative' : undefined,
                      zIndex: isDraggingOrResizing === 'code-editor' ? 999 : undefined,
                    }}
                  >
                    <CodeEditorPane
                      resizableConfig={resizeConfig}
                      width={frame.width}
                      height={value.height ?? menuHeight}
                      onResizeStop={resizeStopEventHandler}
                      setIsResizing={setIsDraggingOrResizing}
                      small={isMenuContainingPanel(menusAndPanes)}
                    />
                  </div>
                </Draggable>
              )
            case 'inspector':
              return (
                <Draggable
                  {...draggableCommonProps}
                  key='inspector'
                  onStop={onDragStop('inspector')}
                  onStart={onDragStart('inspector')}
                >
                  <div
                    style={{
                      width: '100%',
                      height: value.height ?? menuHeight,
                      marginTop: i >= 1 ? GapBetweenPanels : 0,
                      position: isDraggingOrResizing === 'inspector' ? 'relative' : undefined,
                      zIndex: isDraggingOrResizing === 'inspector' ? 999 : undefined,
                    }}
                  >
                    <ResizableRightPane
                      resizableConfig={resizeConfig}
                      onResizeStop={resizeStopEventHandler}
                      setIsResizing={setIsDraggingOrResizing}
                      width={frame.width}
                      height={value.height ?? menuHeight}
                    />
                  </div>
                </Draggable>
              )
            case 'navigator':
              return (
                <Draggable
                  {...draggableCommonProps}
                  key='navigator'
                  onStop={onDragStop('navigator')}
                  onStart={onDragStart('navigator')}
                >
                  <div
                    style={{
                      width: '100%',
                      height: value.height ?? menuHeight,
                      marginTop: i >= 1 ? GapBetweenPanels : 0,
                      position: isDraggingOrResizing === 'navigator' ? 'relative' : undefined,
                      zIndex: isDraggingOrResizing === 'navigator' ? 999 : undefined,
                    }}
                  >
                    <LeftPaneComponent
                      resizableConfig={resizeConfig}
                      onResizeStop={resizeStopEventHandler}
                      setIsResizing={setIsDraggingOrResizing}
                      width={frame.width}
                      height={value.height ?? menuHeight}
                    />
                  </div>
                </Draggable>
              )
            default:
              return null
          }
        })}
      </div>
      {when(
        isDraggingOrResizing != null,
        <style>{`
          body * {
            pointer-events: none !important;
          };
        `}</style>,
      )}
    </>
  )
})

export const PanelTitleBar = React.memo(() => {
  const colorTheme = useColorTheme()
  return (
    <div
      className='handle'
      style={{
        height: TitleHeight,
        width: '100%',
        backgroundColor: colorTheme.fg8.value,
        lineHeight: `${TitleHeight}px`,
        paddingLeft: 10,
      }}
    >
      draggable title
    </div>
  )
})
