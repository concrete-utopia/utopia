import { ControlProps } from './new-canvas-controls'
import * as React from 'react'
import { MetadataUtils } from '../../../core/model/element-metadata-utils'
import { useEditorState } from '../../editor/store/store-hook'
import uuid from 'uuid'
import { IndexPosition } from '../../../utils/utils'
import { ElementPath } from '../../../core/shared/project-file-types'
import * as EP from '../../../core/shared/element-path'
import { openFloatingInsertMenu } from '../../editor/actions/action-creators'
import { betterReactMemo } from '../../../utils/react-performance'
import { useColorTheme } from '../../../uuiui/styles/theme'

const InsertionButtonOffset = 10

interface ButtonControlProps {
  key: string
  positionX: number
  positionY: number
  lineEndX: number
  lineEndY: number
  isHorizontalLine: boolean
  parentPath: ElementPath
  indexPosition: IndexPosition
}

export const InsertionControls: React.FunctionComponent<ControlProps> = betterReactMemo(
  'InsertionControls',
  (props: ControlProps): React.ReactElement | null => {
    if (props.selectedViews.length !== 1) {
      return null
    }
    const selectedView = props.selectedViews[0]
    const parentPath = EP.parentPath(selectedView)
    const parentFrame =
      parentPath != null
        ? MetadataUtils.getFrameInCanvasCoords(parentPath, props.componentMetadata)
        : null

    const parentElement = MetadataUtils.getParent(props.componentMetadata, selectedView)
    if (parentPath == null || parentFrame == null || parentElement == null) {
      return null
    }
    const children = MetadataUtils.getChildrenHandlingGroups(
      props.componentMetadata,
      parentPath,
      false,
    )
    let controlProps: ButtonControlProps[] = []
    children.forEach((child, index) => {
      const childFrame = MetadataUtils.getFrameInCanvasCoords(
        child.elementPath,
        props.componentMetadata,
      )
      if (child.specialSizeMeasurements.position !== 'absolute' && childFrame != null) {
        let direction: 'row' | 'column' = 'column'
        if (child.specialSizeMeasurements.parentLayoutSystem === 'flex') {
          switch (child.specialSizeMeasurements.parentFlexDirection) {
            case 'row':
            case 'column':
              direction = child.specialSizeMeasurements.parentFlexDirection
              break
            default:
            // Ignore any other values.
          }
        }
        const positionX =
          direction == 'column'
            ? parentFrame.x - InsertionButtonOffset + props.canvasOffset.x
            : childFrame.x + childFrame.width + props.canvasOffset.x
        const positionY =
          direction == 'column'
            ? childFrame.y + childFrame.height + props.canvasOffset.y
            : parentFrame.y - InsertionButtonOffset + props.canvasOffset.y

        const lineEndX =
          direction == 'column'
            ? parentFrame.x + parentFrame.width + props.canvasOffset.x
            : childFrame.x + childFrame.width + props.canvasOffset.x
        const lineEndY =
          direction == 'column'
            ? childFrame.y + childFrame.height + props.canvasOffset.y
            : parentFrame.y + parentFrame.height + props.canvasOffset.y
        controlProps.push({
          key: EP.toString(child.elementPath),
          positionX: positionX,
          positionY: positionY,
          lineEndX: lineEndX,
          lineEndY: lineEndY,
          isHorizontalLine: direction === 'column',
          parentPath: parentPath,
          indexPosition: {
            type: 'absolute',
            index: index + 1,
          },
        })
        // first element has a plus button before the element too
        if (index === 0) {
          const beforeX =
            direction == 'column'
              ? parentFrame.x - InsertionButtonOffset + props.canvasOffset.x
              : childFrame.x + props.canvasOffset.x
          const beforeY =
            direction == 'column'
              ? childFrame.y + props.canvasOffset.y
              : parentFrame.y - InsertionButtonOffset + props.canvasOffset.y
          const beforeLineEndX =
            direction == 'column'
              ? parentFrame.x + parentFrame.width + props.canvasOffset.x
              : childFrame.x + props.canvasOffset.x
          const beforeLineEndY =
            direction == 'column'
              ? childFrame.y + props.canvasOffset.y
              : parentFrame.y + parentFrame.height + props.canvasOffset.y
          controlProps.push({
            key: EP.toString(child.elementPath) + '0',
            positionX: beforeX,
            positionY: beforeY,
            lineEndX: beforeLineEndX,
            lineEndY: beforeLineEndY,
            isHorizontalLine: direction === 'column',
            parentPath: parentPath,
            indexPosition: {
              type: 'absolute',
              index: 0,
            },
          })
        }
      }
    })
    return (
      <>
        {controlProps.map((control) => (
          <InsertionButtonContainer {...control} key={control.key} />
        ))}
      </>
    )
  },
)

const InsertionButtonContainer = betterReactMemo(
  'InsertionButtonContainer',
  (props: ButtonControlProps) => {
    const [plusVisible, setPlusVisible] = React.useState(false)

    const onMouseEnter = () => setPlusVisible(true)
    const onMouseLeave = () => setPlusVisible(false)

    return (
      <div
        key={props.key}
        onMouseEnter={onMouseEnter}
        onMouseLeave={onMouseLeave}
        style={{
          position: 'absolute',
          left: props.positionX,
          top: props.positionY,
        }}
      >
        {plusVisible ? (
          <>
            <Line {...props} />
            <PlusButton {...props} />
          </>
        ) : (
          <BlueDot {...props} />
        )}
      </div>
    )
  },
)

const BlueDotSize = 7
const BlueDot = betterReactMemo('BlueDot', (props: ButtonControlProps) => {
  const colorTheme = useColorTheme()
  return (
    <div
      style={{
        marginLeft: -BlueDotSize / 2,
        marginTop: -BlueDotSize / 2,
        backgroundColor: 'white',
        width: BlueDotSize,
        height: BlueDotSize,
        borderRadius: '50%',
        display: 'flex',
        alignItems: 'center',
        justifyContent: 'center',
      }}
    >
      <div
        style={{
          width: BlueDotSize - 2,
          height: BlueDotSize - 2,
          backgroundColor: colorTheme.canvasSelectionPrimaryOutline.value,
          borderRadius: '50%',
        }}
      />
    </div>
  )
})

const ButtonSize = 12
const PlusButton = betterReactMemo('PlusButton', (props: ButtonControlProps) => {
  const dispatch = useEditorState((store) => store.dispatch, 'PlusButton dispatch')
  const colorTheme = useColorTheme()
  const { parentPath, indexPosition } = props
  const insertElement = React.useCallback(
    (event) => {
      event.stopPropagation()
      event.preventDefault()
      dispatch(
        [
          openFloatingInsertMenu({
            insertMenuMode: 'insert',
            parentPath: parentPath,
            indexPosition: indexPosition,
          }),
        ],
        'canvas',
      )
    },
    [dispatch, parentPath, indexPosition],
  )
  return (
    <div
      style={{
        position: 'absolute',
        top: -ButtonSize / 2,
        left: -ButtonSize / 2,
        backgroundColor: 'white',
        width: ButtonSize,
        height: ButtonSize,
        borderRadius: '50%',
        display: 'flex',
        alignItems: 'center',
        justifyContent: 'center',
      }}
      onMouseDown={insertElement}
    >
      <div
        style={{
          width: ButtonSize - 2,
          height: ButtonSize - 2,
          backgroundColor: colorTheme.canvasSelectionPrimaryOutline.value,
          borderRadius: '50%',
          display: 'flex',
          alignItems: 'center',
          justifyContent: 'center',
        }}
      >
        <div style={{ color: 'white', fontSize: 10, marginBottom: 2 }}>+</div>
      </div>
    </div>
  )
})

const Line = betterReactMemo('Line', (props: ButtonControlProps) => {
  const colorTheme = useColorTheme()
  return (
    <div
      style={{
        position: 'absolute',
        top: -1,
        left: -1,
        width: props.isHorizontalLine ? props.lineEndX - props.positionX : 1,
        height: props.isHorizontalLine ? 1 : props.lineEndY - props.positionY,
        backgroundColor: colorTheme.canvasSelectionPrimaryOutline.value,
      }}
    />
  )
})
