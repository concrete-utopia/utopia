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
  boundingRectangleArray,
  rectContainsPoint,
  rectContainsPointInclusive,
  rectanglesEqual,
  windowPoint,
  windowRectangle,
  zeroWindowRect,
} from '../../core/shared/math-utils'
import { CanvasSizeAtom, LeftPaneDefaultWidth } from '../editor/store/editor-state'
import { mapDropNulls, stripNulls } from '../../core/shared/array-utils'
import { UtopiaTheme, useColorTheme } from '../../uuiui'
import { when } from '../../utils/react-conditionals'
import type { Direction } from 're-resizable/lib/resizer'
import { usePrevious } from '../editor/hook-utils'
import { TitleHeight } from '../titlebar/title-bar'

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
      maxWidth: 350,
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
interface PanelData {
  name: Menu | Pane
  type: 'menu' | 'pane'
  frame: WindowRectangle
  location: PanelName
  locationInColumn: number
}

const DefaultPanels: Array<PanelData> = [
  {
    name: 'code-editor',
    type: 'pane',
    frame: windowRectangle({ x: 0, y: 0, width: 500, height: 600 }),
    location: 'leftMenu1',
    locationInColumn: 0,
  },
  {
    name: 'navigator',
    type: 'menu',
    frame: windowRectangle({ x: 0, y: 0, width: LeftPaneDefaultWidth, height: 0 }),
    location: 'leftMenu1',
    locationInColumn: 1,
  },
  {
    name: 'inspector',
    type: 'menu',
    frame: windowRectangle({ x: 0, y: 0, width: 255, height: 0 }),
    location: 'rightMenu1',
    locationInColumn: 0,
  },
]

const DefaultSizes = {
  left: 0,
  right: 0,
}
export const FloatingPanelSizesAtom = atomWithPubSub({
  key: 'FloatingPanelSizesAtom',
  defaultValue: DefaultSizes,
})

function isMenuContainingPanel(panelData: Array<PanelData>): boolean {
  return panelData.some((value) => value.type === 'menu')
}

function findDropAreaBeforeAfterColumn(
  newPosition: WindowPoint,
  panelsData: Array<PanelData>,
  canvasSize: Size,
  draggedPanel: PanelName,
): { newPanel: PanelName; switchWithPanel: PanelName | null } | null {
  const EdgeDropAreaWidth = 60
  const leftColumn = panelsData.filter((data) => data.location === 'leftMenu1')
  const leftColumn2 = panelsData.filter((data) => data.location === 'leftMenu2')
  const rightColumn = panelsData.filter((data) => data.location === 'rightMenu1')
  const rightColumn2 = panelsData.filter((data) => data.location === 'rightMenu2')

  const leftColumnFrame =
    boundingRectangleArray(leftColumn.map((data) => data.frame)) ?? zeroWindowRect
  const leftColumn2Frame =
    boundingRectangleArray(leftColumn2.map((data) => data.frame)) ?? zeroWindowRect
  const rightColumnFrame =
    boundingRectangleArray(rightColumn.map((data) => data.frame)) ??
    windowRectangle({ x: canvasSize.width, y: 0, width: 0, height: canvasSize.height })
  const rightColumn2Frame =
    boundingRectangleArray(rightColumn2.map((data) => data.frame)) ??
    windowRectangle({ x: canvasSize.width, y: 0, width: 0, height: canvasSize.height })

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
        x: leftColumnFrame.x + leftColumnFrame.width,
        y: 0,
        width: 80,
        height: canvasSize.height,
      }),
    },
    // left side of the right menu
    {
      targetPanel: 'rightMenu1',
      frame: windowRectangle({
        x: rightColumnFrame.x - 80,
        y: 0,
        width: 80,
        height: canvasSize.height,
      }),
    },
    // between the right edge and the last right menu
    {
      targetPanel: 'rightMenu2',
      frame: windowRectangle({
        x: rightColumn2Frame.x + rightColumn2Frame.width - EdgeDropAreaWidth, //GapBetweenPanels,
        y: 0,
        width: canvasSize.width - rightColumn2Frame.x + rightColumn2Frame.width + EdgeDropAreaWidth, //GapBetweenPanels,
        height: canvasSize.height,
      }),
    },
  ]

  const dropTargetOutside =
    outerDropAreas.find((area) => rectContainsPointInclusive(area.frame, newPosition))
      ?.targetPanel ?? null
  if (dropTargetOutside != null) {
    return {
      newPanel: dropTargetOutside,
      switchWithPanel: shouldSwitchColumnsOnDropOutsideColumn(
        dropTargetOutside,
        draggedPanel,
        panelsData,
      ),
    }
  }
  return null
}

function shouldSwitchColumnsOnDropOutsideColumn(
  dropTargetPanel: PanelName | null,
  draggedPanel: PanelName,
  panelsData: Array<PanelData>,
): PanelName | null {
  // maybe switch with existing panels if dragging to edge or when dropped to the side of a column

  if (dropTargetPanel != null) {
    const leftColumn = panelsData.filter((data) => data.location === 'leftMenu1')
    const leftColumn2 = panelsData.filter((data) => data.location === 'leftMenu2')
    const rightColumn = panelsData.filter((data) => data.location === 'rightMenu1')
    const rightColumn2 = panelsData.filter((data) => data.location === 'rightMenu2')
    if (
      dropTargetPanel === 'leftMenu1' &&
      leftColumn.length > 0 &&
      (leftColumn2.length === 0 || (draggedPanel === 'leftMenu2' && leftColumn2.length === 1))
    ) {
      // dragging to far left when something is already there
      return 'leftMenu2'
    }
    if (
      dropTargetPanel === 'leftMenu2' &&
      leftColumn2.length > 0 &&
      (leftColumn.length === 0 || (draggedPanel === 'leftMenu1' && leftColumn.length === 1))
    ) {
      // dragging to the right of the left panel 2
      return 'leftMenu1'
    }

    if (
      dropTargetPanel === 'rightMenu1' &&
      rightColumn.length > 0 &&
      (rightColumn2.length === 0 || (draggedPanel === 'rightMenu2' && rightColumn2.length === 1))
    ) {
      // dragging to the left of the right panel 1
      return 'rightMenu2'
    }

    if (
      dropTargetPanel === 'rightMenu2' &&
      rightColumn2.length > 0 &&
      (rightColumn.length === 0 || (draggedPanel === 'rightMenu1' && rightColumn.length === 1))
    ) {
      // dragging to far right when something is already there
      return 'rightMenu1'
    }
  }
  return null
}

function findDropAreaInsideColumn(
  newPosition: WindowPoint,
  panelsData: Array<PanelData>,
): PanelName | null {
  return (
    panelsData.find((panel) => {
      if (panel.frame == null) {
        return null
      }
      return rectContainsPoint(panel.frame, newPosition)
    })?.location ?? null
  )
}

export const FloatingPanelsContainer = React.memo(() => {
  const [panelsData, setPanelsData] = React.useState<Array<PanelData>>(DefaultPanels)
  const [highlight, setHighlight] = React.useState<WindowRectangle | null>(null)

  const canvasSize = usePubSubAtomReadOnly(CanvasSizeAtom, AlwaysTrue)
  const prevCanvasSize = usePrevious(canvasSize)

  // TODO fix this to have correct canvas toolbar and arrow positions!
  const [columnFrames, setColumnFrames] = usePubSubAtom<{ left: number; right: number }>(
    FloatingPanelSizesAtom,
  )
  const colorTheme = useColorTheme()

  const setColumnFramesFromPanelsData = React.useCallback(
    (latestPanelsData: Array<PanelData>) => {
      const leftColumnFrame =
        boundingRectangleArray(
          latestPanelsData
            .filter((data) => data.location === 'leftMenu1')
            .map((data) => data.frame),
        ) ?? zeroWindowRect
      const left2ColumnFrame =
        boundingRectangleArray(
          latestPanelsData
            .filter((data) => data.location === 'leftMenu2')
            .map((data) => data.frame),
        ) ?? zeroWindowRect
      const rightColumnFrame =
        boundingRectangleArray(
          latestPanelsData
            .filter((data) => data.location === 'rightMenu1')
            .map((data) => data.frame),
        ) ?? zeroWindowRect
      const right2ColumnFrame =
        boundingRectangleArray(
          latestPanelsData
            .filter((data) => data.location === 'rightMenu2')
            .map((data) => data.frame),
        ) ?? zeroWindowRect
      setColumnFrames({
        left: leftColumnFrame.x + leftColumnFrame.width + left2ColumnFrame.width + GapBetweenPanels,
        right: rightColumnFrame.width + right2ColumnFrame.width + GapBetweenPanels,
      })
    },
    [setColumnFrames],
  )

  const getUpdatedPanelSizes = React.useCallback(
    (currentPanelsData: Array<PanelData>) => {
      return currentPanelsData.map((panel) => {
        const panelsInColumn = currentPanelsData.filter((d) => d.location === panel.location)
        let height = canvasSize.height
        let width = panel.frame.width

        if (!isMenuContainingPanel(panelsInColumn) && panelsInColumn.length === 1) {
          height = Math.min(canvasSize.height, SizeConstraints['code-editor'].defaultSize.height)
          width = SizeConstraints['code-editor'].defaultSize.width
        }
        if (isMenuContainingPanel(panelsInColumn)) {
          height =
            (canvasSize.height - GapBetweenPanels * (panelsInColumn.length + 1)) /
            panelsInColumn.length
        }

        if (panelsInColumn.find((d) => d.name === 'inspector') != null) {
          width = SizeConstraints['inspector'].defaultSize.width
        } else if (panelsInColumn.find((d) => d.name === 'navigator') != null) {
          width = SizeConstraints['navigator'].defaultSize.width
        }

        return {
          ...panel,
          frame: windowRectangle({
            x: panel.frame.x,
            y: panel.frame.y,
            width: width,
            height: height,
          }),
        }
      })
    },
    [canvasSize],
  )

  const getUpdatedPanelPositions = React.useCallback(
    (currentPanelsData: Array<PanelData>) => {
      return currentPanelsData.map((panel) => {
        const panelsInColumn = currentPanelsData.filter((d) => d.location === panel.location)
        const y = panelsInColumn.reduce((working, current) => {
          if (current.locationInColumn < panel.locationInColumn) {
            return (
              working + current.frame.height + GapBetweenPanels * (current.locationInColumn + 1)
            )
          }
          return working
        }, 0)

        let x = 0
        switch (panel.location) {
          case 'leftMenu1':
            x = GapBetweenPanels
            break
          case 'leftMenu2':
            x =
              (currentPanelsData.find((v) => v.location === 'leftMenu1')?.frame.width ?? 0) +
              GapBetweenPanels * 2
            break
          case 'rightMenu1':
            const otherRightMenu = currentPanelsData.find((v) => v.location === 'rightMenu2')
            x =
              canvasSize.width -
              panel.frame.width -
              GapBetweenPanels * 2 -
              (otherRightMenu?.frame.width ?? 0)
            break
          case 'rightMenu2':
            x = canvasSize.width - panel.frame.width - GapBetweenPanels
            break
          default:
            break
        }

        return {
          ...panel,
          frame: windowRectangle({
            x: x,
            y: y,
            width: panel.frame.width,
            height: panel.frame.height,
          }),
        }
      })
    },
    [canvasSize],
  )

  const getUpdatedPanelSizesAndPositions = React.useCallback(
    (currentPanelsData: Array<PanelData>) => {
      const withPanelSizeUpdates = getUpdatedPanelSizes(currentPanelsData)
      const withPositionsUpdated = getUpdatedPanelPositions(withPanelSizeUpdates)
      return withPositionsUpdated
    },
    [getUpdatedPanelSizes, getUpdatedPanelPositions],
  )

  const updateColumn = React.useCallback(
    (draggedMenuOrPane: Menu | Pane, draggedPanel: PanelName, newPosition: WindowPoint) => {
      const { newPanel, switchWithPanel } = ((): {
        newPanel: PanelName | null
        switchWithPanel: PanelName | null
      } => {
        setHighlight(null)
        const droppedToOutsideOfAColumn = findDropAreaBeforeAfterColumn(
          newPosition,
          panelsData,
          canvasSize,
          draggedPanel,
        )
        if (droppedToOutsideOfAColumn != null) {
          return {
            newPanel: droppedToOutsideOfAColumn.newPanel,
            switchWithPanel: droppedToOutsideOfAColumn.switchWithPanel,
          }
        }

        return {
          newPanel: findDropAreaInsideColumn(newPosition, panelsData),
          switchWithPanel: null,
        }
      })()

      const currentPanelData = panelsData.find((v) => v.name === draggedMenuOrPane)
      if (
        currentPanelData != null &&
        newPanel != null &&
        (draggedPanel != newPanel || switchWithPanel != null)
      ) {
        const columnData = panelsData.filter((d) => d.location === newPanel)
        let updatedPanelData = panelsData.map((panel) => {
          if (panel.name === draggedMenuOrPane) {
            return {
              ...panel,
              location: newPanel,
              locationInColumn: columnData.length,
            }
          } else if (panel.location === newPanel && switchWithPanel != null) {
            return { ...panel, location: switchWithPanel }
          } else if (draggedPanel === panel.location) {
            return {
              ...panel,
              locationInColumn:
                panel.locationInColumn > currentPanelData.locationInColumn
                  ? panel.locationInColumn - 1
                  : panel.locationInColumn,
            }
          } else {
            return panel
          }
        })

        // Readjust the locationInColumn values to have them start at zero and increment by 1.
        updatedPanelData.sort(
          (firstPanel, secondPanel) => firstPanel.locationInColumn - secondPanel.locationInColumn,
        )
        let locationInColumnMap: Map<PanelName, number> = new Map()
        updatedPanelData = updatedPanelData.map((panelData) => {
          const currentLocationInColumnValue = locationInColumnMap.get(panelData.location)
          const newLocationInColumn =
            currentLocationInColumnValue == null ? 0 : currentLocationInColumnValue + 1
          locationInColumnMap.set(panelData.location, newLocationInColumn)
          if (panelData.locationInColumn === newLocationInColumn) {
            return panelData
          } else {
            return {
              ...panelData,
              locationInColumn: newLocationInColumn,
            }
          }
        })

        // Put the entries in the same order by name.
        updatedPanelData = [
          ...updatedPanelData.filter((p) => p.name === 'code-editor'),
          ...updatedPanelData.filter((p) => p.name === 'navigator'),
          ...updatedPanelData.filter((p) => p.name === 'inspector'),
          ...updatedPanelData.filter((p) => p.name === 'preview'),
        ]

        const panelDataWithPositionsUpdated = getUpdatedPanelSizesAndPositions(updatedPanelData)
        setPanelsData(panelDataWithPositionsUpdated)
        setColumnFramesFromPanelsData(panelDataWithPositionsUpdated)
      }
    },
    [
      canvasSize,
      panelsData,
      setPanelsData,
      getUpdatedPanelSizesAndPositions,
      setColumnFramesFromPanelsData,
    ],
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
        const newPanelsData = panelsData.map((data) => {
          if (currentPanel === data.location) {
            return {
              ...data,
              frame: windowRectangle({
                x: data.frame.x,
                y: data.frame.y,
                width: width,
                height: data.frame.height,
              }),
            }
          } else {
            return data
          }
        })
        const withAdjustedPositions = getUpdatedPanelPositions(newPanelsData)
        setPanelsData(withAdjustedPositions)
        setColumnFramesFromPanelsData(withAdjustedPositions)
      } else {
        const panelsInColumn = panelsData.filter((d) => d.location === currentPanel)
        const newPanelsData = panelsData.map((data) => {
          if (currentPanel === data.location) {
            if (menuOrPane === data.name) {
              return {
                ...data,
                frame: windowRectangle({
                  x: data.frame.x,
                  y: data.frame.y,
                  width: data.frame.width,
                  height: height,
                }),
              }
            } else {
              // fill remaining space
              const remainingSpace =
                canvasSize.height - height - (panelsInColumn.length + 1) * GapBetweenPanels
              const newHeight = remainingSpace / (panelsInColumn.length - 1)
              return {
                ...data,
                frame: windowRectangle({
                  x: data.frame.x,
                  y: data.frame.y,
                  width: data.frame.width,
                  height: newHeight,
                }),
              }
            }
          } else {
            return data
          }
        })
        const withAdjustedPositions = getUpdatedPanelPositions(newPanelsData)
        setPanelsData(withAdjustedPositions)
        setColumnFramesFromPanelsData(withAdjustedPositions)
      }
    },
    [
      panelsData,
      setPanelsData,
      canvasSize,
      getUpdatedPanelPositions,
      setColumnFramesFromPanelsData,
    ],
  )

  const showHighlight = React.useCallback(
    (newPosition: WindowPoint, draggedPanel: PanelName) => {
      const shouldDropOutsideOfColumn = findDropAreaBeforeAfterColumn(
        newPosition,
        panelsData,
        canvasSize,
        draggedPanel,
      )

      if (
        shouldDropOutsideOfColumn != null &&
        draggedPanel === 'leftMenu1' &&
        shouldDropOutsideOfColumn.newPanel === 'leftMenu1' &&
        shouldDropOutsideOfColumn.switchWithPanel != null
      ) {
        const targetFrame = panelsData.find(
          (v) => v.location === shouldDropOutsideOfColumn.newPanel,
        )?.frame
        if (targetFrame != null) {
          const x = shouldDropOutsideOfColumn.newPanel.includes('1')
            ? targetFrame.x
            : targetFrame.x + targetFrame.width
          setHighlight(
            windowRectangle({
              x: x - GapBetweenPanels / 2,
              y: GapBetweenPanels,
              width: 2,
              height: canvasSize.height - GapBetweenPanels,
            }),
          )
          return
        }
      }

      // TODO fix this!
      // const isAboveColumn = findDropAreaInsideColumn(newPosition, panelsData)
      // if (isAboveColumn != null && draggedPanel != isAboveColumn) {
      //   const lastPanel = panelsData
      //     .filter((v) => v.location === isAboveColumn)
      //     .sort((first, second) => second.locationInColumn - first.locationInColumn)[0]
      //   if (lastPanel != null) {
      //     setHighlight(
      //       windowRectangle({
      //         x: lastPanel.frame.x,
      //         y: lastPanel.frame.y + lastPanel.frame.height + GapBetweenPanels / 2,
      //         width: lastPanel.frame.width,
      //         height: 2,
      //       }),
      //     )
      //   }
      // }
    },
    [panelsData, canvasSize, setHighlight],
  )

  // menus fill the available space
  React.useEffect(() => {
    if (
      prevCanvasSize?.width !== canvasSize.width ||
      prevCanvasSize?.height !== canvasSize.height
    ) {
      const updatedPanelsData = getUpdatedPanelSizesAndPositions(panelsData)
      if (
        updatedPanelsData.some((data, i) => {
          const originalFrame = panelsData[i].frame
          if (data.frame == null || originalFrame == null) {
            return true
          }
          return !rectanglesEqual(data.frame, originalFrame)
        })
      ) {
        setPanelsData(updatedPanelsData)
        setColumnFramesFromPanelsData(updatedPanelsData)
      }
    }
  }, [
    canvasSize,
    prevCanvasSize,
    panelsData,
    setPanelsData,
    getUpdatedPanelSizesAndPositions,
    setColumnFramesFromPanelsData,
  ])

  return (
    <>
      {panelsData.map((data, i) => (
        <FloatingPanel
          key={data.name}
          type={data.type}
          name={data.name}
          frame={data.frame}
          panelLocation={data.location}
          columnData={panelsData.filter((d) => d.location === data.location)}
          onResize={updateSize}
          updateColumn={updateColumn}
          onMenuDrag={showHighlight}
        />
      ))}
      {when(
        highlight != null,
        <div
          style={{
            position: 'absolute',
            top: highlight?.y,
            left: highlight?.x,
            width: highlight?.width,
            height: highlight?.height,
            backgroundColor: colorTheme.brandNeonGreen.value,
          }}
        ></div>,
      )}
    </>
  )
})

interface FloatingPanelProps {
  name: Menu | Pane
  type: 'menu' | 'pane'
  frame: WindowRectangle
  panelLocation: PanelName
  updateColumn: (menuOrPane: Menu | Pane, currentPanel: PanelName, newPosition: WindowPoint) => void
  onResize: (
    menuOrPane: Menu | Pane,
    currentPanel: PanelName,
    direction: Direction,
    width: number,
    height: number,
  ) => void
  onMenuDrag: (newPosition: WindowPoint, draggedPanel: PanelName) => void
  columnData: Array<PanelData>
}

export const FloatingPanel = React.memo<FloatingPanelProps>((props) => {
  const { columnData, panelLocation, name, frame, updateColumn, onResize, onMenuDrag } = props
  const canvasSize = usePubSubAtomReadOnly(CanvasSizeAtom, AlwaysTrue)

  const [isDraggingOrResizing, setIsDraggingOrResizing] = React.useState<Menu | Pane | null>(null)
  const onDragStart = React.useCallback<(menuOrPane: Menu | Pane) => DraggableEventHandler>(
    (menuOrPane: Menu | Pane) => {
      return (e, data) => {
        setIsDraggingOrResizing(menuOrPane)
      }
    },
    [setIsDraggingOrResizing],
  )
  const onDrag = React.useCallback<(menuOrPane: Menu | Pane) => DraggableEventHandler>(
    (menuOrPane: Menu | Pane) => {
      return (e, data) => onMenuDrag(windowPoint({ x: data.x, y: data.y }), panelLocation)
    },
    [onMenuDrag, panelLocation],
  )

  const onDragStop = React.useCallback<(menuOrPane: Menu | Pane) => DraggableEventHandler>(
    (menuOrPane: Menu | Pane) => {
      return (e, data) => {
        updateColumn(
          menuOrPane,
          panelLocation,
          windowPoint({ x: (e as any).clientX, y: (e as any).clientY }),
        )
        setIsDraggingOrResizing(null)
      }
    },
    [panelLocation, updateColumn, setIsDraggingOrResizing],
  )

  const resizeMinMaxSnap = React.useMemo(() => {
    const possibleConstraints = mapDropNulls((v) => SizeConstraints[v.name].resize, columnData)
    const minWidth = Math.max(...possibleConstraints.map((v) => v.minWidth), 20)
    const maxWidth = Math.min(...possibleConstraints.map((v) => v.maxWidth), canvasSize.width)
    const snap = stripNulls(possibleConstraints.map((v) => v.snap))[0] // TODO what happens if there are multiple conflicting snapping menus

    return { minWidth, maxWidth, snap }
  }, [columnData, canvasSize])

  const resizeEventHandler = React.useCallback<
    (menuOrPane: Menu | Pane, direction: Direction, width: number, height: number) => void
  >(
    (menuOrPane: Menu | Pane, direction: Direction, width: number, height: number) =>
      onResize(menuOrPane, panelLocation, direction, width, height),
    [panelLocation, onResize],
  )

  const defaultMenuHeight = React.useMemo(
    () => (canvasSize.height - (columnData.length + 1) * GapBetweenPanels) / columnData.length,
    [columnData, canvasSize],
  )

  const alignment = React.useMemo(
    () => (panelLocation === 'leftMenu1' || panelLocation === 'leftMenu2' ? 'left' : 'right'),
    [panelLocation],
  )

  const resizeConfig = React.useMemo(() => {
    return {
      enable: {
        left: alignment === 'right',
        right: alignment === 'left',
        bottom: !isMenuContainingPanel(columnData) || columnData.length > 1,
      },
      minWidth: resizeMinMaxSnap.minWidth,
      maxWidth: resizeMinMaxSnap.maxWidth,
      minHeight: TitleHeight,
      maxHeight:
        !isMenuContainingPanel(columnData) || columnData.length > 1
          ? canvasSize.height - columnData.length * TitleHeight
          : undefined,
      snap: resizeMinMaxSnap.snap,
    }
  }, [alignment, canvasSize, resizeMinMaxSnap, columnData])

  const draggablePanelComponent = (() => {
    switch (name) {
      case 'code-editor':
        return (
          <CodeEditorPane
            resizableConfig={resizeConfig}
            width={frame.width}
            height={frame.height ?? defaultMenuHeight}
            onResize={resizeEventHandler}
            setIsResizing={setIsDraggingOrResizing}
            small={isMenuContainingPanel(columnData)}
          />
        )
      case 'inspector':
        return (
          <ResizableRightPane
            resizableConfig={resizeConfig}
            onResize={resizeEventHandler}
            setIsResizing={setIsDraggingOrResizing}
            width={frame.width}
            height={frame.height ?? defaultMenuHeight}
          />
        )
      case 'navigator':
        return (
          <LeftPaneComponent
            resizableConfig={resizeConfig}
            onResize={resizeEventHandler}
            setIsResizing={setIsDraggingOrResizing}
            width={frame.width}
            height={frame.height ?? defaultMenuHeight}
          />
        )
      default:
        return null
    }
  })()

  return (
    <>
      <Draggable
        handle='.handle'
        position={{ x: frame.x, y: frame.y }}
        key={name}
        onStop={onDragStop(name)}
        onDrag={onDrag(name)}
        onStart={onDragStart(name)}
      >
        <div
          style={{
            position: 'absolute',
            marginTop: GapBetweenPanels,
            zIndex: isDraggingOrResizing === name ? 999 : undefined,
          }}
        >
          {draggablePanelComponent}
        </div>
      </Draggable>
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
