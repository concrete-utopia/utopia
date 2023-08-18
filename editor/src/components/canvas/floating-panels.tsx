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
  rectanglesEqual,
  windowPoint,
  windowRectangle,
} from '../../core/shared/math-utils'
import { CanvasSizeAtom, LeftPaneDefaultWidth } from '../editor/store/editor-state'
import { mapArrayToDictionary } from '../../core/shared/array-utils'
// import type { ResizeCallback, ResizeDirection } from 're-resizable'
// import { Resizable } from 're-resizable'

type Menu = 'inspector' | 'navigator'
type Pane = 'code-editor' | 'preview'

const ResizeConstraints: {
  [key: string]: { minWidth: number; maxWidth: number; snap: Array<number> | null }
} = {
  inspector: {
    minWidth: 255,
    maxWidth: 280,
    snap: [255, 280],
  },
  navigator: {
    minWidth: 240,
    maxWidth: 280,
    snap: null,
  },
}

type PanelName = 'leftMenu1' | 'leftMenu2' | 'rightMenu1' | 'rightMenu2'
const Panels: Array<PanelName> = ['leftMenu1', 'leftMenu2', 'rightMenu1', 'rightMenu2']

const DefaultPanels: { [key in PanelName]: Array<Menu | Pane> } = {
  leftMenu1: ['navigator'],
  leftMenu2: ['code-editor'],
  rightMenu1: ['inspector'],
  rightMenu2: [],
}

const DefaultSizes: { [key in PanelName]: WindowRectangle } = {
  leftMenu1: windowRectangle({ x: 0, y: 0, width: LeftPaneDefaultWidth, height: 0 }),
  leftMenu2: windowRectangle({ x: 0, y: 0, width: 500, height: 600 }),
  rightMenu1: windowRectangle({ x: 0, y: 0, width: 255, height: 0 }),
  rightMenu2: windowRectangle({ x: 0, y: 0, width: 0, height: 0 }),
}

export const FloatingPanelsAtom = atomWithPubSub({
  key: 'FloatingPanelsAtom',
  defaultValue: DefaultPanels,
})

function isOnlyMenuContainingPanel(menusOrPanes: Array<Menu | Pane>): boolean {
  return menusOrPanes.some((value) => value === 'inspector' || value === 'navigator')
}

export const FloatingPanelsContainer = React.memo(() => {
  const [panelsData, setPanelsData] = usePubSubAtom(FloatingPanelsAtom)
  const canvasSize = usePubSubAtomReadOnly(CanvasSizeAtom, AlwaysTrue)

  const [panelFrames, setPanelFrames] =
    React.useState<{ [key in PanelName]: WindowRectangle }>(DefaultSizes)

  const updateSize = React.useCallback(
    (menuOrPane: Menu | Pane, panel: PanelName, newSize: Size) => {
      setPanelFrames({
        ...panelFrames,
        [panel]: windowRectangle({
          x: panelFrames[panel].x,
          y: panelFrames[panel].y,
          width: newSize.width,
          height: newSize.height,
        }),
      })
    },
    [panelFrames, setPanelFrames],
  )
  const updateColumn = React.useCallback(
    (menuOrPane: Menu | Pane, currentPanel: PanelName, newPosition: WindowPoint) => {
      // TODO SWITCH PANELS
      const newPanel = (() => {
        if (newPosition.x <= 10) {
          return 'leftMenu1'
        } else if (newPosition.x <= panelFrames.leftMenu2.width + 10) {
          return 'leftMenu2'
        } else if (newPosition.x >= canvasSize.width - 10) {
          return 'rightMenu2'
        } else if (newPosition.x >= panelFrames.rightMenu1.x - 10) {
          return 'rightMenu1'
        } else {
          return (Panels.reverse() as Array<PanelName>).find((name) =>
            rectContainsPoint(panelFrames[name], newPosition),
          )
        }
      })()

      if (newPanel != null && currentPanel != newPanel) {
        const remainingMenusAndPanes = panelsData[currentPanel].filter(
          (name) => name !== menuOrPane,
        )
        const newPanelsData = {
          ...panelsData,
          [newPanel]: [...panelsData[newPanel], menuOrPane],
          [currentPanel]: remainingMenusAndPanes,
        }
        setPanelsData(newPanelsData)
        if (remainingMenusAndPanes.length === 0) {
          updateSize(menuOrPane, currentPanel, { width: 0, height: 0 })
        }
        if (panelFrames[newPanel].width === 0) {
          updateSize(menuOrPane, newPanel, { width: 255, height: 0 }) // TODO USE MENU SIZE
        }
      }
    },
    [panelFrames, panelsData, setPanelsData, updateSize, canvasSize],
  )

  React.useEffect(() => {
    const framesWithUpdatedPosition: { [key in PanelName]: WindowRectangle } = mapArrayToDictionary(
      Panels,
      (p) => p,
      (p, i) => {
        let height = 600 // canvasSize.height // code pane height!!!
        if (isOnlyMenuContainingPanel(panelsData[p])) {
          height = canvasSize.height
        }

        let x = 0
        switch (p) {
          case 'leftMenu2':
            x = panelFrames.leftMenu1.width
            break
          case 'rightMenu1':
            x = canvasSize.width - panelFrames.rightMenu1.width - panelFrames.rightMenu2.width - 20
            break
          case 'rightMenu2':
            x = canvasSize.width - panelFrames.rightMenu2.width - 20
            break
          default:
            break
        }

        return windowRectangle({ x: x, y: 0, width: panelFrames[p].width, height: height })
      },
    )
    if (
      (Object.keys(framesWithUpdatedPosition) as Array<PanelName>).some(
        (name) => !rectanglesEqual(framesWithUpdatedPosition[name], panelFrames[name]),
      )
    ) {
      setPanelFrames(framesWithUpdatedPosition)
    }
  }, [canvasSize, panelsData, panelFrames, setPanelFrames])

  return (
    <>
      <FloatingPanel
        key={'leftMenu1'}
        panelName={'leftMenu1'}
        frame={panelFrames.leftMenu1}
        menusAndPanes={panelsData.leftMenu1}
        updateMenuSize={updateSize}
        updateColumn={updateColumn}
      />
      <FloatingPanel
        key={'leftMenu2'}
        panelName={'leftMenu2'}
        frame={panelFrames.leftMenu2}
        menusAndPanes={panelsData.leftMenu2}
        updateMenuSize={updateSize}
        updateColumn={updateColumn}
      />
      <FloatingPanel
        key={'rightMenu1'}
        panelName={'rightMenu1'}
        frame={panelFrames.rightMenu1}
        menusAndPanes={panelsData.rightMenu1}
        updateMenuSize={updateSize}
        updateColumn={updateColumn}
      />
      <FloatingPanel
        key={'rightMenu2'}
        panelName={'rightMenu2'}
        frame={panelFrames.rightMenu2}
        menusAndPanes={panelsData.rightMenu2}
        updateMenuSize={updateSize}
        updateColumn={updateColumn}
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
  updateMenuSize: (menuOrPane: Menu | Pane, currentPanel: PanelName, newSize: Size) => void
  updateColumn: (menuOrPane: Menu | Pane, currentPanel: PanelName, newPosition: WindowPoint) => void
}

export const FloatingPanel = React.memo<FloatingPanelProps>((props) => {
  // TODO RESIZE
  // const resizableRef = React.useRef<Resizable>(null)
  // const [width, setWidth] = React.useState<number>(DefaultWidth)

  // const onResize = React.useCallback(() => {
  //   const newWidth = resizableRef.current?.size.width
  //   if (newWidth != null) {
  //     // we have to use the instance ref to directly access the get size() getter, because re-resize's API only wants to tell us deltas, but we need the snapped width
  //     setWidth(newWidth)
  //   }
  // }, [setWidth])

  const { panelName, menusAndPanes, frame, updateMenuSize, updateColumn } = props

  // TODO RESIZE
  const enabledResizeDirections = React.useMemo(() => {
    if (isOnlyMenuContainingPanel(menusAndPanes)) {
      return ['width']
    } else {
      return ['width', 'height']
    }
  }, [menusAndPanes])

  const height = React.useMemo(() => {
    if (isOnlyMenuContainingPanel(menusAndPanes)) {
      return 'calc(100% - 20px)'
    } else {
      return frame.height
    }
  }, [menusAndPanes, frame])

  const dragEventHandler = React.useCallback<DraggableEventHandler>((e, data) => {
    // ha valami folott vagyunk akkor highlight az oszlopot
    if ((e as any).clientX < 400) {
      // console.log('NA HELLO')
    }
  }, [])
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

  return (
    <div
      className={panelName}
      style={{
        position: 'absolute',
        height: height,
        width: frame.width,
        left: frame.x,
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
              <Draggable handle='.handle' onStop={dragStopEventHandler('code-editor')}>
                <div>
                  <CodeEditorPane />
                </div>
              </Draggable>
            )
          case 'inspector':
            return (
              <Draggable
                onDrag={dragEventHandler}
                onStop={dragStopEventHandler('inspector')}
                handle='.handle'
                position={{ x: 0, y: 0 }}
              >
                <div>
                  <ResizableRightPane />
                </div>
              </Draggable>
            )
          case 'navigator':
            return (
              <Draggable
                onDrag={dragEventHandler}
                onStop={dragStopEventHandler('navigator')}
                handle='.handle'
                position={{ x: 0, y: 0 }}
              >
                <div>
                  <LeftPaneComponent />
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
