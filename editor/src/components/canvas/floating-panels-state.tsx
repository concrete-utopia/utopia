import type { Direction } from 're-resizable/lib/resizer'
import { eitherToMaybe, foldEither } from '../../core/shared/either'
import { modify, set, toFirst } from '../../core/shared/optics/optic-utilities'
import type { Size, WindowPoint, WindowRectangle } from '../../core/shared/math-utils'
import { rectContainsPoint } from '../../core/shared/math-utils'
import {
  boundingRectangleArray,
  rectContainsPointInclusive,
  zeroWindowRect,
} from '../../core/shared/math-utils'
import { windowRectangle } from '../../core/shared/math-utils'
import {
  filtered,
  fromArrayIndex,
  fromField,
  fromObjectField,
  objectValues,
  traverseArray,
} from '../../core/shared/optics/optic-creators'
import type { Optic } from '../../core/shared/optics/optics'
import { UtopiaTheme } from '../../uuiui'
import { LeftPaneDefaultWidth } from '../editor/store/editor-state'

export type Menu = 'inspector' | 'navigator'
export type Pane = 'code-editor' | 'preview'

export const allMenusAndPanels: Array<Menu | Pane> = [
  'navigator',
  'code-editor',
  'inspector',
  'preview',
]

export type PanelName = 'leftMenu1' | 'leftMenu2' | 'rightMenu1' | 'rightMenu2'

export interface PanelData {
  name: Menu | Pane
  type: 'menu' | 'pane'
  frame: WindowRectangle
}

export interface PanelDataWithLocation extends PanelData {
  location: PanelName
  column: PanelColumn
}

export type PanelColumn = Array<PanelData>

export type PanelContent = { [location: string]: PanelColumn }

export interface PanelState {
  panelContent: PanelContent
}

export const GapBetweenPanels = 10

export const panelStateToPanelContentOptic: Optic<PanelState, PanelContent> =
  fromField('panelContent')

export const panelStateToPanelColumns: Optic<PanelState, PanelColumn> =
  panelStateToPanelContentOptic.compose(objectValues())

export const panelStateToPanelDataOptic: Optic<PanelState, PanelData> =
  panelStateToPanelColumns.compose(traverseArray())

export function panelStateToColumnOptic(panelColumn: PanelName): Optic<PanelState, PanelColumn> {
  return panelStateToPanelContentOptic.compose(fromObjectField(panelColumn))
}

export function panelStateToNamedPanelOptic(
  panelName: PanelData['name'],
): Optic<PanelState, PanelData> {
  return panelStateToPanelDataOptic.compose(filtered((panel) => panel.name === panelName))
}

export interface SizeConstraintsResizeValue {
  minWidth: number
  maxWidth: number
  snap: {
    x?: number[]
    y?: number[]
  } | null
}

export interface SizeConstraintsValue {
  resize: SizeConstraintsResizeValue | null
  defaultSize: Size
}

export const SizeConstraints: {
  [key: string]: SizeConstraintsValue
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

export function emptyPanels(): PanelState {
  return {
    panelContent: {
      leftMenu1: [],
      leftMenu2: [],
      rightMenu1: [],
      rightMenu2: [],
    },
  }
}

export const DefaultPanels: PanelState = {
  panelContent: {
    leftMenu1: [
      {
        name: 'navigator',
        type: 'menu',
        frame: windowRectangle({ x: 0, y: 0, width: LeftPaneDefaultWidth, height: 0 }),
      },
      {
        name: 'code-editor',
        type: 'pane',
        frame: windowRectangle({ x: 0, y: 0, width: 500, height: 600 }),
      },
    ],
    leftMenu2: [],
    rightMenu1: [
      {
        name: 'inspector',
        type: 'menu',
        frame: windowRectangle({ x: 0, y: 0, width: 255, height: 0 }),
      },
    ],
    rightMenu2: [],
  },
}

export const DefaultSizes = {
  left: 0,
  right: 0,
}

export function isMenuContainingPanel(panelColumn: PanelColumn): boolean {
  return Object.values(panelColumn).some((value) => value.type === 'menu')
}

export function updatePanelsToDefaultSizes(
  currentPanelState: PanelState,
  canvasSize: Size,
): PanelState {
  let newPanelContent: PanelContent = emptyPanels().panelContent
  for (const [columnName, columnContent] of Object.entries(currentPanelState.panelContent)) {
    for (const panel of columnContent) {
      const gapsAndMargins = GapBetweenPanels * (columnContent.length + 1)
      const canvasHeightWithoutMargins = canvasSize.height - gapsAndMargins
      let height = canvasSize.height
      let width = panel.frame.width

      if (!isMenuContainingPanel(columnContent) && columnContent.length === 1) {
        if (columnName === 'leftMenu1' || columnName === 'rightMenu2') {
          height = canvasHeightWithoutMargins / columnContent.length
        } else {
          height = Math.min(
            canvasHeightWithoutMargins / columnContent.length,
            SizeConstraints['code-editor'].defaultSize.height,
          )
        }
        width = SizeConstraints['code-editor'].defaultSize.width
      }
      if (isMenuContainingPanel(columnContent)) {
        if (columnContent.length == 2) {
          const panelNamesInColumn = columnContent.map((v) => v.name)
          if (
            panelNamesInColumn.includes('navigator') &&
            panelNamesInColumn.includes('code-editor')
          ) {
            if (panel.name === 'code-editor') {
              height = canvasHeightWithoutMargins / 3
            } else {
              height = canvasHeightWithoutMargins * (2 / 3)
            }
          } else if (
            panelNamesInColumn.includes('navigator') &&
            panelNamesInColumn.includes('inspector')
          ) {
            if (panel.name === 'navigator') {
              height = canvasHeightWithoutMargins / 3
            } else {
              height = canvasHeightWithoutMargins * (2 / 3)
            }
          } else {
            if (panel.name === 'code-editor') {
              height = canvasHeightWithoutMargins / 3
            } else {
              height = canvasHeightWithoutMargins * (2 / 3)
            }
          }
        } else {
          height = canvasHeightWithoutMargins / columnContent.length
        }
      }

      if (columnContent.find((d) => d.name === 'inspector') != null) {
        width = SizeConstraints['inspector'].defaultSize.width
      } else if (columnContent.find((d) => d.name === 'navigator') != null) {
        width = SizeConstraints['navigator'].defaultSize.width
      }

      let columnToAddTo: PanelColumn
      if (columnName in newPanelContent) {
        columnToAddTo = newPanelContent[columnName]
      } else {
        columnToAddTo = []
        newPanelContent[columnName] = columnToAddTo
      }
      columnToAddTo.push({
        ...panel,
        frame: windowRectangle({
          x: panel.frame.x,
          y: panel.frame.y,
          width: width,
          height: height,
        }),
      })
    }
  }
  return {
    panelContent: newPanelContent,
  }
}

export function updatePanelPositionsBasedOnLocationAndSize(
  currentPanelsData: PanelState,
  canvasSize: Size,
): PanelState {
  let newPanelContent: PanelContent = emptyPanels().panelContent
  for (const [columnName, columnContent] of Object.entries(currentPanelsData.panelContent)) {
    columnContent.forEach((panel, panelIndex) => {
      const y = columnContent.reduce((working, current, currentIndex) => {
        if (currentIndex < panelIndex) {
          return working + current.frame.height + GapBetweenPanels
        }
        return working
      }, 0)

      let x = 0
      switch (columnName) {
        case 'leftMenu1':
          x = GapBetweenPanels
          break
        case 'leftMenu2':
          const leftMenu1FirstPanel = toFirst(
            panelStateToColumnOptic('leftMenu1').compose(fromArrayIndex(0)),
            currentPanelsData,
          )
          x = foldEither(
            () => {
              return GapBetweenPanels
            },
            (firstPanel) => {
              return firstPanel.frame.width + GapBetweenPanels * 2
            },
            leftMenu1FirstPanel,
          )
          break
        case 'rightMenu1':
          const rightMenu2FirstPanel = eitherToMaybe(
            toFirst(
              panelStateToColumnOptic('rightMenu2').compose(fromArrayIndex(0)),
              currentPanelsData,
            ),
          )
          x =
            canvasSize.width -
            panel.frame.width -
            (rightMenu2FirstPanel == null ? GapBetweenPanels : GapBetweenPanels * 2) -
            (rightMenu2FirstPanel?.frame.width ?? 0)
          break
        case 'rightMenu2':
          x = canvasSize.width - panel.frame.width - GapBetweenPanels
          break
        default:
          break
      }

      let columnToAddTo: PanelColumn
      if (columnName in newPanelContent) {
        columnToAddTo = newPanelContent[columnName]
      } else {
        columnToAddTo = []
        newPanelContent[columnName] = columnToAddTo
      }
      columnToAddTo.push({
        ...panel,
        frame: windowRectangle({
          x: x,
          y: y,
          width: panel.frame.width,
          height: panel.frame.height,
        }),
      })
    })
  }
  return {
    panelContent: newPanelContent,
  }
}

export function updateSizeOfPanel(
  panelsData: PanelState,
  canvasSize: Size,
  menuOrPane: Menu | Pane,
  currentPanel: PanelName,
  direction: Direction,
  width: number,
  height: number,
): PanelState {
  if (direction === 'left' || direction === 'right') {
    const newPanelsData = modify(
      panelStateToColumnOptic(currentPanel).compose(traverseArray()),
      (data) => {
        return {
          ...data,
          frame: windowRectangle({
            x: data.frame.x,
            y: data.frame.y,
            width: width,
            height: data.frame.height,
          }),
        }
      },
      panelsData,
    )
    const withAdjustedPositions = updatePanelPositionsBasedOnLocationAndSize(
      newPanelsData,
      canvasSize,
    )
    return withAdjustedPositions
  } else {
    const newPanelsData = modify(
      panelStateToColumnOptic(currentPanel),
      (panelColumn) => {
        return panelColumn.map((data) => {
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
              canvasSize.height - height - (panelColumn.length + 1) * GapBetweenPanels
            const newHeight = remainingSpace / (panelColumn.length - 1)
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
        })
      },
      panelsData,
    )
    const withAdjustedPositions = updatePanelPositionsBasedOnLocationAndSize(
      newPanelsData,
      canvasSize,
    )
    return withAdjustedPositions
  }
}

export function findDropAreaInsideColumn(
  newPosition: WindowPoint,
  panelState: PanelState,
): PanelName | null {
  for (const [location, panels] of Object.entries(panelState.panelContent)) {
    for (const panel of panels) {
      if (rectContainsPoint(panel.frame, newPosition)) {
        return location as PanelName
      }
    }
  }
  return null
}

export function shouldSwitchColumnsOnDropOutsideColumn(
  dropTargetPanel: PanelName | null,
  draggedPanel: PanelName,
  panelState: PanelState,
): PanelName | null {
  // maybe switch with existing panels if dragging to edge or when dropped to the side of a column

  if (dropTargetPanel != null) {
    const leftColumn = panelState.panelContent['leftMenu1'] ?? []
    const leftColumn2 = panelState.panelContent['leftMenu2'] ?? []
    const rightColumn = panelState.panelContent['rightMenu1'] ?? []
    const rightColumn2 = panelState.panelContent['rightMenu2'] ?? []
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

export function findDropAreaBeforeAfterColumn(
  newPosition: WindowPoint,
  panelState: PanelState,
  canvasSize: Size,
  draggedPanel: PanelName,
): { newPanel: PanelName; switchWithPanel: PanelName | null } | null {
  const EdgeDropAreaWidth = 60
  const leftColumn = panelState.panelContent['leftMenu1'] ?? []
  const leftColumn2 = panelState.panelContent['leftMenu2'] ?? []
  const rightColumn = panelState.panelContent['rightMenu1'] ?? []
  const rightColumn2 = panelState.panelContent['rightMenu2'] ?? []

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
        panelState,
      ),
    }
  }
  return null
}

export function dragPaneToNewPosition(
  currentPanelsData: PanelState,
  canvasSize: Size,
  draggedMenuOrPane: Menu | Pane,
  draggedPanel: PanelName,
  newPosition: WindowPoint,
): PanelState {
  // Determine where the panel has landed.
  const droppedToOutsideOfAColumn = findDropAreaBeforeAfterColumn(
    newPosition,
    currentPanelsData,
    canvasSize,
    draggedPanel,
  )
  let newPanel: PanelName | null = null
  let switchWithPanel: PanelName | null = null
  if (droppedToOutsideOfAColumn == null) {
    newPanel = findDropAreaInsideColumn(newPosition, currentPanelsData)
  } else {
    newPanel = droppedToOutsideOfAColumn.newPanel
    switchWithPanel = droppedToOutsideOfAColumn.switchWithPanel
  }
  const currentPanelData = eitherToMaybe(
    toFirst(panelStateToNamedPanelOptic(draggedMenuOrPane), currentPanelsData),
  )

  // If there are any changes to make...
  if (
    currentPanelData != null &&
    newPanel != null &&
    (draggedPanel != newPanel || switchWithPanel != null)
  ) {
    let updatedPanelState: PanelState = currentPanelsData
    // Move what is dragged.
    const draggedContent = eitherToMaybe(
      toFirst(panelStateToNamedPanelOptic(draggedMenuOrPane), updatedPanelState),
    )
    if (switchWithPanel != null) {
      // Swapping this content out with another panel.
      let updatedPanelContent: PanelContent = { ...updatedPanelState.panelContent }
      const toSwitchWithCurrentColumn = updatedPanelContent[switchWithPanel] ?? []
      updatedPanelContent[switchWithPanel] = updatedPanelContent[newPanel] ?? []
      updatedPanelContent[newPanel] = toSwitchWithCurrentColumn
      updatedPanelState = set(fromField('panelContent'), updatedPanelContent, updatedPanelState)
    }
    if (draggedContent != null) {
      // Remove the entry from where it was...
      updatedPanelState = modify(
        panelStateToPanelColumns,
        (panelColumn) => {
          return panelColumn.filter((content) => content !== draggedContent)
        },
        updatedPanelState,
      )
      // ...Add the entry to where it should be.
      updatedPanelState = modify(
        panelStateToColumnOptic(newPanel),
        (panelColumn) => {
          return [...panelColumn, draggedContent]
        },
        updatedPanelState,
      )
    }

    const panelDataWithPositionsUpdated = updatePanelPositionsBasedOnLocationAndSize(
      updatePanelsToDefaultSizes(updatedPanelState, canvasSize),
      canvasSize,
    )
    return panelDataWithPositionsUpdated
  } else {
    return currentPanelsData
  }
}

export function getOrderedPanelsForRendering(panelState: PanelState): Array<PanelDataWithLocation> {
  let result: Array<PanelDataWithLocation> = []
  function addPanel(panelName: PanelData['name']): void {
    for (const [panelColumnKey, panelColumnValue] of Object.entries(panelState.panelContent)) {
      for (const panel of panelColumnValue) {
        if (panel.name === panelName) {
          result.push({
            ...panel,
            location: panelColumnKey as PanelName,
            column: panelColumnValue,
          })
          return
        }
      }
    }
  }
  for (const panelName of allMenusAndPanels) {
    addPanel(panelName)
  }
  return result
}
