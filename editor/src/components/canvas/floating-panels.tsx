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
import {
  CanvasSizeAtom,
  LeftPaneDefaultWidth,
  LeftPanelWidthAtom,
} from '../editor/store/editor-state'
import { mapArrayToDictionary } from '../../core/shared/array-utils'
import { InspectorWidthAtom } from '../inspector/common/inspector-atoms'
import { UtopiaTheme } from '../../uuiui'
import { useAtom } from 'jotai'
import { Substores, useEditorState } from '../editor/store/store-hook'
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
  const navigatorSize = usePubSubAtomReadOnly(LeftPanelWidthAtom, AlwaysTrue)
  const [inspectorSize, _] = useAtom(InspectorWidthAtom)
  const codeEditorWidth = useEditorState(
    Substores.restOfEditor,
    (store) =>
      store.editor.interfaceDesigner.codePaneVisible
        ? store.editor.interfaceDesigner.codePaneWidth + 10
        : 0,
    'CanvasWrapperComponent codeEditorWidth',
  )

  const [panelFrames, setPanelFrames] =
    React.useState<{ [key in PanelName]: WindowRectangle }>(DefaultSizes)

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
        } else {
          const newPanelsData = {
            ...panelsData,
            [newPanel]: [...panelsData[newPanel], menuOrPane],
            [currentPanel]: remainingMenusAndPanes,
          }
          setPanelsData(newPanelsData)
        }

        // TODO MOVE THIS SOMEWHERE ELSE!!
        let newPanelSize = panelFrames[newPanel]
        switch (menuOrPane) {
          case 'navigator':
            newPanelSize.width = Math.max(newPanelSize.width, navigatorSize)
            break
          case 'code-editor':
            newPanelSize.width = Math.max(newPanelSize.width, codeEditorWidth)
            break
          case 'inspector':
            newPanelSize.width =
              inspectorSize === 'wide'
                ? UtopiaTheme.layout.inspectorLargeWidth
                : UtopiaTheme.layout.inspectorSmallWidth
            break
          case 'preview':
          default:
            break
        }
        setPanelFrames({
          ...panelFrames,
          [newPanel]: newPanelSize,
        })
      }
    },
    [
      panelFrames,
      panelsData,
      setPanelsData,
      canvasSize,
      navigatorSize,
      inspectorSize,
      codeEditorWidth,
    ],
  )

  React.useEffect(() => {
    const framesWithUpdatedPosition: { [key in PanelName]: WindowRectangle } = mapArrayToDictionary(
      Panels,
      (p) => p,
      (p, i) => {
        let height = 600 // canvasSize.height // code pane height!!!
        if (isMenuContainingPanel(panelsData[p])) {
          height = canvasSize.height
        }
        let width = panelFrames[p].width
        if (panelsData[p].length === 0) {
          width = 0
        }

        let x = 0
        switch (p) {
          case 'leftMenu2':
            x = panelFrames.leftMenu1.width
            break
          case 'rightMenu1':
            x = canvasSize.width - panelFrames.rightMenu1.width - panelFrames.rightMenu2.width
            break
          case 'rightMenu2':
            x = canvasSize.width - panelFrames.rightMenu2.width
            break
          default:
            break
        }

        return windowRectangle({ x: x, y: 0, width: width, height: height })
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
        updateColumn={updateColumn}
        alignment='left'
      />
      <FloatingPanel
        key={'leftMenu2'}
        panelName={'leftMenu2'}
        frame={panelFrames.leftMenu2}
        menusAndPanes={panelsData.leftMenu2}
        updateColumn={updateColumn}
        alignment='left'
      />
      <FloatingPanel
        key={'rightMenu1'}
        panelName={'rightMenu1'}
        frame={panelFrames.rightMenu1}
        menusAndPanes={panelsData.rightMenu1}
        updateColumn={updateColumn}
        alignment='right'
      />
      <FloatingPanel
        key={'rightMenu2'}
        panelName={'rightMenu2'}
        frame={panelFrames.rightMenu2}
        menusAndPanes={panelsData.rightMenu2}
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

  const { panelName, menusAndPanes, frame, alignment, updateColumn } = props
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
                  <CodeEditorPane small={isMenuContainingPanel(menusAndPanes)} />
                </div>
              </Draggable>
            )
          case 'inspector':
            return (
              <Draggable
                key='inspector'
                onStop={dragStopEventHandler('inspector')}
                handle='.handle'
                position={{ x: 0, y: 0 }}
              >
                <div>
                  <ResizableRightPane
                    enabledDirection={{
                      left: props.alignment === 'right',
                      right: props.alignment === 'left',
                    }}
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
                position={{ x: 0, y: 0 }}
              >
                <div>
                  <LeftPaneComponent
                    enabledDirection={{
                      left: props.alignment === 'right',
                      right: props.alignment === 'left',
                    }}
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
