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
import type { WindowPoint, WindowRectangle } from '../../core/shared/math-utils'
import {
  boundingRectangleArray,
  rectanglesEqual,
  windowPoint,
  windowRectangle,
  zeroWindowRect,
} from '../../core/shared/math-utils'
import { CanvasSizeAtom, LeftPanelMinWidth } from '../editor/store/editor-state'
import { mapDropNulls, stripNulls } from '../../core/shared/array-utils'
import { UtopiaTheme, useColorTheme } from '../../uuiui'
import { when } from '../../utils/react-conditionals'
import type { Direction } from 're-resizable/lib/resizer'
import { usePrevious } from '../editor/hook-utils'
import { TitleHeight } from '../titlebar/title-bar'
import type { Menu, Pane, PanelColumn, PanelName, PanelState } from './floating-panels-state'
import { getOrderedPanelsForRendering } from './floating-panels-state'
import { findDropAreaBeforeAfterColumn } from './floating-panels-state'
import { dragPaneToNewPosition } from './floating-panels-state'
import { updateSizeOfPanel } from './floating-panels-state'
import { updatePanelPositionsBasedOnLocationAndSize } from './floating-panels-state'
import { isMenuContainingPanel } from './floating-panels-state'
import { updatePanelsToDefaultSizes } from './floating-panels-state'
import { DefaultPanels } from './floating-panels-state'
import { DefaultSizes } from './floating-panels-state'
import { GapBetweenPanels, SizeConstraints } from './floating-panels-state'
import { NO_OP } from '../../core/shared/utils'

export const FloatingPanelSizesAtom = atomWithPubSub({
  key: 'FloatingPanelSizesAtom',
  defaultValue: DefaultSizes,
})

export const FloatingPanelsContainer = React.memo(() => {
  const [panelsData, setPanelsData] = React.useState<PanelState>(DefaultPanels)

  const orderedPanels = React.useMemo(() => {
    return getOrderedPanelsForRendering(panelsData)
  }, [panelsData])

  return (
    <div
      data-testid='floating-panels-container'
      style={{
        position: 'absolute',
        display: 'grid',
        width: '100%',
        height: '100%',
        gridTemplateColumns: '260px 260px 1fr 260px 260px',
        gridTemplateRows: 'repeat(12, 1fr)',
      }}
    >
      {orderedPanels.map((pane) => {
        return (
          <FloatingPanel
            key={pane.name}
            type={pane.type}
            name={pane.name}
            frame={pane.frame}
            panelLocation={pane.location}
            columnData={pane.column}
            onResize={NO_OP}
            updateColumn={NO_OP}
            onMenuDrag={NO_OP}
          />
        )
      })}
    </div>
  )
})

export const FloatingPanelsContainerOld = React.memo(() => {
  const [panelsData, setPanelsData] = React.useState<PanelState>(DefaultPanels)
  const [highlight, setHighlight] = React.useState<WindowRectangle | null>(null)

  const canvasSize = usePubSubAtomReadOnly(CanvasSizeAtom, AlwaysTrue)
  const prevCanvasSize = usePrevious(canvasSize)

  const [columnFrames, setColumnFrames] = usePubSubAtom<{ left: number; right: number }>(
    FloatingPanelSizesAtom,
  )
  const colorTheme = useColorTheme()

  // sets the left and right edges for other canvas elements like canvas toolbar
  const setColumnFramesFromPanelsData = React.useCallback(
    (latestPanelsData: PanelState) => {
      const leftColumnFrame =
        boundingRectangleArray(
          (latestPanelsData.panelContent['leftMenu1'] ?? []).map((data) => data.frame),
        ) ?? zeroWindowRect
      const left2ColumnFrame =
        boundingRectangleArray(
          (latestPanelsData.panelContent['leftMenu2'] ?? []).map((data) => data.frame),
        ) ?? zeroWindowRect
      const rightColumnFrame =
        boundingRectangleArray(
          (latestPanelsData.panelContent['rightMenu1'] ?? []).map((data) => data.frame),
        ) ?? zeroWindowRect
      const right2ColumnFrame =
        boundingRectangleArray(
          (latestPanelsData.panelContent['rightMenu2'] ?? []).map((data) => data.frame),
        ) ?? zeroWindowRect
      setColumnFrames({
        left: leftColumnFrame.x + leftColumnFrame.width + left2ColumnFrame.width + GapBetweenPanels,
        right: rightColumnFrame.width + right2ColumnFrame.width + GapBetweenPanels,
      })
    },
    [setColumnFrames],
  )

  // update panel size to default, for example on drop and on browser resize
  const getUpdatedPanelSizes = React.useCallback(
    (currentPanelsData: PanelState) => {
      return updatePanelsToDefaultSizes(currentPanelsData, canvasSize)
    },
    [canvasSize],
  )

  // update menu positions based on their sizes and locations
  const getUpdatedPanelPositions = React.useCallback(
    (currentPanelsData: PanelState) => {
      return updatePanelPositionsBasedOnLocationAndSize(currentPanelsData, canvasSize)
    },
    [canvasSize],
  )

  const getUpdatedPanelSizesAndPositions = React.useCallback(
    (currentPanelsData: PanelState) => {
      const withPanelSizeUpdates = getUpdatedPanelSizes(currentPanelsData)
      const withPositionsUpdated = getUpdatedPanelPositions(withPanelSizeUpdates)
      return withPositionsUpdated
    },
    [getUpdatedPanelSizes, getUpdatedPanelPositions],
  )

  // when a menu is dropped in or next to a column
  const updateColumn = React.useCallback(
    (draggedMenuOrPane: Menu | Pane, draggedPanel: PanelName, newPosition: WindowPoint) => {
      setHighlight(null)
      const panelDataWithPositionsUpdated = dragPaneToNewPosition(
        panelsData,
        canvasSize,
        draggedMenuOrPane,
        draggedPanel,
        newPosition,
      )
      setPanelsData(panelDataWithPositionsUpdated)
      setColumnFramesFromPanelsData(panelDataWithPositionsUpdated)
    },
    [canvasSize, panelsData, setPanelsData, setColumnFramesFromPanelsData],
  )

  // when resizing a menu it effects other elements in the same column and the top/left position in the next column
  const updateSize = React.useCallback(
    (
      menuOrPane: Menu | Pane,
      currentPanel: PanelName,
      direction: Direction,
      width: number,
      height: number,
    ) => {
      return updateSizeOfPanel(
        panelsData,
        canvasSize,
        menuOrPane,
        currentPanel,
        direction,
        width,
        height,
      )
    },
    [panelsData, canvasSize],
  )

  // TODO this is really not ready here, only works when dragging from left menu to outside of left menu
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
        const targetFrame = panelsData.panelContent[shouldDropOutsideOfColumn.newPanel][0]?.frame
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
      function shouldUpdatePanelsData(): boolean {
        for (const panelColumnName of Object.keys(updatedPanelsData.panelContent)) {
          const updatedColumn = updatedPanelsData.panelContent[panelColumnName]
          const oldColumn = panelsData.panelContent[panelColumnName] ?? []
          if (updatedColumn.length !== oldColumn.length) {
            return true
          }
          for (let columnIndex = 0; columnIndex < updatedColumn.length; columnIndex++) {
            const updatedPanel = updatedColumn[columnIndex]
            const oldPanel = oldColumn[columnIndex]
            if (updatedPanel.frame == null || oldPanel.frame == null) {
              return true
            }
            if (!rectanglesEqual(oldPanel.frame, updatedPanel.frame)) {
              return true
            }
          }
        }
        return false
      }
      if (shouldUpdatePanelsData()) {
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

  const orderedPanels = React.useMemo(() => {
    return getOrderedPanelsForRendering(panelsData)
  }, [panelsData])

  return (
    <>
      {orderedPanels.map((pane) => {
        return (
          <FloatingPanel
            key={pane.name}
            type={pane.type}
            name={pane.name}
            frame={pane.frame}
            panelLocation={pane.location}
            columnData={pane.column}
            onResize={updateSize}
            updateColumn={updateColumn}
            onMenuDrag={showHighlight}
          />
        )
      })}
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
  columnData: PanelColumn
}

export const FloatingPanel = React.memo<FloatingPanelProps>((props) => {
  const { columnData, panelLocation, name, frame, updateColumn, onResize, onMenuDrag } = props

  const draggablePanelComponent = (() => {
    switch (name) {
      case 'code-editor':
        return (
          <CodeEditorPane
            small={false}
            width={0}
            height={0}
            onResize={NO_OP}
            setIsResizing={NO_OP}
            resizableConfig={{
              enable: {
                right: true,
              },
            }}
          />
        )
      case 'inspector':
        return (
          <ResizableRightPane
            width={0}
            height={0}
            onResize={NO_OP}
            setIsResizing={NO_OP}
            resizableConfig={{
              snap: {
                x: [UtopiaTheme.layout.inspectorSmallWidth, UtopiaTheme.layout.inspectorLargeWidth],
              },
              enable: {
                left: true,
              },
            }}
          />
        )
      case 'navigator':
        return (
          <LeftPaneComponent
            width={0}
            height={0}
            onResize={NO_OP}
            setIsResizing={NO_OP}
            resizableConfig={{
              minWidth: LeftPanelMinWidth,
              enable: {
                right: true,
              },
            }}
          />
        )
      default:
        return null
    }
  })()

  return (
    <>
      <div
        style={{
          gridRow: '1 / span 12',
          display: 'flex',
          flexDirection: 'column',
          contain: 'layout',
        }}
      >
        {draggablePanelComponent}
      </div>
    </>
  )
})

export const FloatingPanelOld = React.memo<FloatingPanelProps>((props) => {
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
          ? canvasSize.height - (columnData.length * TitleHeight) / 2 // what happens here?
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
