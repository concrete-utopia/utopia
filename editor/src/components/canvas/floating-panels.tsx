import React from 'react'
import Draggable from 'react-draggable'
import type { DraggableEventHandler } from 'react-draggable'
import { CodeEditorPane, ResizableRightPane } from './design-panel-root'
import { LeftPaneComponent } from '../navigator/left-pane'
import { AlwaysTrue, usePubSubAtomReadOnly } from '../../core/shared/atom-with-pub-sub'
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
import { UtopiaTheme } from '../../uuiui'
// import type { ResizeCallback, ResizeDirection } from 're-resizable'
// import { Resizable } from 're-resizable'

type Menu = 'inspector' | 'navigator'
type Pane = 'code-editor' | 'preview'

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

const DefaultPanels: { [key in PanelName]: Array<Menu | Pane> } = {
  leftMenu1: ['code-editor'],
  leftMenu2: ['navigator'],
  rightMenu1: ['inspector'],
  rightMenu2: [],
}

const DefaultSizes: { [key in PanelName]: WindowRectangle } = {
  leftMenu1: windowRectangle({ x: 0, y: 0, width: 500, height: 600 }),
  leftMenu2: windowRectangle({ x: 0, y: 0, width: LeftPaneDefaultWidth, height: 0 }),
  rightMenu1: windowRectangle({ x: 0, y: 0, width: 255, height: 0 }),
  rightMenu2: windowRectangle({ x: 0, y: 0, width: 0, height: 0 }),
}

function isMenuContainingPanel(menusOrPanes: Array<Menu | Pane>): boolean {
  return menusOrPanes.some((value) => value === 'inspector' || value === 'navigator')
}

export const FloatingPanelsContainer = React.memo(() => {
  const [panelsData, setPanelsData] =
    React.useState<{ [key in PanelName]: Array<Menu | Pane> }>(DefaultPanels)
  const canvasSize = usePubSubAtomReadOnly(CanvasSizeAtom, AlwaysTrue)

  const [panelFrames, setPanelFrames] =
    React.useState<{ [key in PanelName]: WindowRectangle }>(DefaultSizes)

  const getUpdatedPanelSizes = React.useCallback(
    (
      currentPanelsData: { [key in PanelName]: Array<Menu | Pane> },
      currentFrames: { [key in PanelName]: WindowRectangle },
    ) => {
      return mapArrayToDictionary(
        Panels,
        (p) => p,
        (p, i) => {
          let height = Math.min(canvasSize.height, 600) // code pane height!!!
          if (isMenuContainingPanel(currentPanelsData[p])) {
            height = canvasSize.height
          }
          let width = currentFrames[p].width
          if (currentPanelsData[p].length === 0) {
            width = 0
          } else {
            if (currentFrames[p].width === 0) {
              // give default size to panel
              width = SizeConstraints[currentPanelsData[p][0]].defaultSize.width
            } else {
              const possibleConstraints = mapDropNulls(
                (v) => SizeConstraints[v].resize,
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
      currentPanelsData: { [key in PanelName]: Array<Menu | Pane> },
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
              x = withPanelSizeUpdates.leftMenu1.width
              break
            case 'rightMenu1':
              x =
                canvasSize.width -
                withPanelSizeUpdates.rightMenu1.width -
                withPanelSizeUpdates.rightMenu2.width
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
              width: 10, // TODO FIX, now this is a padding value // panelFrames.leftMenu1.x,
              height: canvasSize.height,
            }),
          },
          // right side of the left menu
          {
            targetPanel: 'leftMenu2',
            frame: windowRectangle({
              x: panelFrames.leftMenu2.x + panelFrames.leftMenu2.width,
              y: 0,
              width: 25,
              height: canvasSize.height,
            }),
          },
          // left side of the right menu
          {
            targetPanel: 'rightMenu1',
            frame: windowRectangle({
              x: panelFrames.rightMenu1.x - 25,
              y: 0,
              width: 25,
              height: canvasSize.height,
            }),
          },
          // between the right edge and the last right menu
          {
            targetPanel: 'rightMenu2',
            frame: windowRectangle({
              x: panelFrames.rightMenu2.x + panelFrames.rightMenu2.width - 10, // TODO FIX, now this is a padding value
              y: 0,
              width:
                canvasSize.width - panelFrames.rightMenu2.x + panelFrames.rightMenu2.width + 10, // TODO FIX, now this is a padding value
              height: canvasSize.height,
            }),
          },
        ]

        // TODO THIS SHOULD BE NOT BASED ON THE CURSOR X,Y BUT WHERE THE FRAME TOP/LEFT IS
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
              (currentPanel === 'leftMenu2' && panelsData.leftMenu1.length === 1))
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
        const remainingMenusAndPanes = panelsData[currentPanel].filter(
          (name) => name !== menuOrPane,
        )

        if (switchWithPanel != null) {
          const newPanelsData = {
            ...panelsData,
            [currentPanel]: remainingMenusAndPanes,
            [switchWithPanel]: panelsData[newPanel].filter((name) => name !== menuOrPane),
            [newPanel]: [menuOrPane],
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
            [newPanel]: [...panelsData[newPanel], menuOrPane],
            [currentPanel]: remainingMenusAndPanes,
          }
          setPanelsData(newPanelsData)
        }
      }
    },
    [panelFrames, panelsData, setPanelsData, canvasSize, setPanelFrames, getUpdatedPanelPositions],
  )

  const updateSize = React.useCallback(
    (menuOrPane: Menu | Pane, currentPanel: PanelName, width: number) => {
      let frame = panelFrames[currentPanel]
      frame.width = width
      const updatedPanelFrames = {
        ...panelFrames,
        [currentPanel]: frame,
      }
      const withUpdatedPositions = getUpdatedPanelPositions(panelsData, updatedPanelFrames)
      setPanelFrames(withUpdatedPositions)
    },
    [panelFrames, panelsData, setPanelFrames, getUpdatedPanelPositions],
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
  menusAndPanes: Array<Menu | Pane>
  frame: WindowRectangle
  alignment: 'left' | 'right'
  updateColumn: (menuOrPane: Menu | Pane, currentPanel: PanelName, newPosition: WindowPoint) => void
  onResizeStop: (menuOrPane: Menu | Pane, currentPanel: PanelName, width: number) => void
}

export const FloatingPanel = React.memo<FloatingPanelProps>((props) => {
  const { panelName, menusAndPanes, frame, alignment, updateColumn, onResizeStop } = props
  const canvasSize = usePubSubAtomReadOnly(CanvasSizeAtom, AlwaysTrue)

  const leftOrRightPosition = React.useMemo(() => {
    if (alignment === 'right') {
      return canvasSize.width - frame.x - frame.width
    }
    return frame.x
  }, [canvasSize, frame, alignment])

  const height = React.useMemo(() => {
    if (isMenuContainingPanel(menusAndPanes)) {
      return 'calc(100% - 20px)'
    } else {
      return frame.height
    }
  }, [menusAndPanes, frame])

  const dragStopEventHandler = React.useCallback<
    (menuOrPane: Menu | Pane) => DraggableEventHandler
  >(
    (menuOrPane: Menu | Pane) => {
      return (e, data) => {
        updateColumn(
          menuOrPane,
          panelName,
          windowPoint({ x: (e as any).clientX, y: (e as any).clientY }),
        )
      }
    },
    [panelName, updateColumn],
  )

  const resizeMinMaxSnap = React.useMemo(() => {
    const possibleConstraints = mapDropNulls((v) => SizeConstraints[v].resize, menusAndPanes)
    const minWidth = Math.max(...possibleConstraints.map((v) => v.minWidth))
    const maxWidth = Math.min(...possibleConstraints.map((v) => v.maxWidth))
    const snap = stripNulls(possibleConstraints.map((v) => v.snap))[0] // TODO what happens if there are multiple conflicting snapping menus

    return { minWidth, maxWidth, snap }
  }, [menusAndPanes])

  const resizeStopEventHandler = React.useCallback<
    (menuOrPane: Menu | Pane, width: number) => void
  >(
    (menuOrPane: Menu | Pane, width: number) => onResizeStop(menuOrPane, panelName, width),
    [panelName, onResizeStop],
  )

  return (
    <div
      className={panelName}
      style={{
        position: 'absolute',
        height: height,
        [props.alignment]: leftOrRightPosition,
        top: frame.y,
        margin: 10,
        border: '1px solid green',
      }}
    >
      <style>{`
.${panelName} > .react-draggable { height: ${100 / menusAndPanes.length}%; width: 100%}`}</style>
      {menusAndPanes.map((value) => {
        switch (value) {
          case 'code-editor':
            return (
              <Draggable
                key='code-editor'
                handle='.handle'
                onStop={dragStopEventHandler('code-editor')}
              >
                <div>
                  <CodeEditorPane
                    resizableConfig={{
                      enable: {
                        left: props.alignment === 'right',
                        right: props.alignment === 'left',
                      },
                      minWidth: resizeMinMaxSnap.minWidth,
                      maxWidth: resizeMinMaxSnap.maxWidth,
                      snap: resizeMinMaxSnap.snap,
                    }}
                    width={frame.width}
                    onResizeStop={resizeStopEventHandler}
                    small={isMenuContainingPanel(menusAndPanes)}
                  />
                </div>
              </Draggable>
            )
          case 'inspector':
            return (
              <Draggable
                key='inspector'
                onStop={dragStopEventHandler('inspector')}
                handle='.handle'
              >
                <div>
                  <ResizableRightPane
                    resizableConfig={{
                      enable: {
                        left: props.alignment === 'right',
                        right: props.alignment === 'left',
                      },
                      minWidth: resizeMinMaxSnap.minWidth,
                      maxWidth: resizeMinMaxSnap.maxWidth,
                      snap: resizeMinMaxSnap.snap,
                    }}
                    onResizeStop={resizeStopEventHandler}
                    width={frame.width}
                  />
                </div>
              </Draggable>
            )
          case 'navigator':
            return (
              <Draggable
                key='navigator'
                onStop={dragStopEventHandler('navigator')}
                handle='.handle'
              >
                <div>
                  <LeftPaneComponent
                    resizableConfig={{
                      enable: {
                        left: props.alignment === 'right',
                        right: props.alignment === 'left',
                      },
                      minWidth: resizeMinMaxSnap.minWidth,
                      maxWidth: resizeMinMaxSnap.maxWidth,
                      snap: resizeMinMaxSnap.snap,
                    }}
                    onResizeStop={resizeStopEventHandler}
                    width={frame.width}
                  />
                </div>
              </Draggable>
            )
          default:
            return null
        }
      })}
    </div>
  )
})
