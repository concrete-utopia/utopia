import React from 'react'
import { MetadataUtils } from '../../../core/model/element-metadata-utils'
import { useEditorState } from '../../editor/store/store-hook'
import uuid from 'uuid'
import { IndexPosition } from '../../../utils/utils'
import { ElementPath } from '../../../core/shared/project-file-types'
import * as EP from '../../../core/shared/element-path'
import { openFloatingInsertMenu } from '../../editor/actions/action-creators'
import { useColorTheme } from '../../../uuiui/styles/theme'
import { CanvasOffsetWrapper } from './canvas-offset-wrapper'

const InsertionButtonOffset = 10

interface ButtonControlProps {
  key: string
  scale: number
  positionX: number
  positionY: number
  lineEndX: number
  lineEndY: number
  isHorizontalLine: boolean
  parentPath: ElementPath
  indexPosition: IndexPosition
}

export const InsertionControls: React.FunctionComponent = React.memo(
  (): React.ReactElement | null => {
    const isInteractionActive = useEditorState(
      (store) => store.editor.canvas.interactionSession != null,
      'DistanceGuidelineControl isInteractionActive',
    )
    const selectedViews = useEditorState(
      (store) => store.editor.selectedViews,
      'InsertionControls selectedViews',
    )
    const jsxMetadata = useEditorState(
      (store) => store.editor.jsxMetadata,
      'InsertionControls jsxMetadata',
    )
    const scale = useEditorState((store) => store.editor.canvas.scale, 'InsertionControls scale')
    if (selectedViews.length !== 1 || isInteractionActive) {
      return null
    }
    const selectedView = selectedViews[0]
    const parentPath = selectedView
    const parentFrame =
      parentPath != null ? MetadataUtils.getFrameInCanvasCoords(parentPath, jsxMetadata) : null

    const parentElement = MetadataUtils.findElementByElementPath(jsxMetadata, selectedView)
    if (parentPath == null || parentFrame == null || parentElement == null) {
      return null
    }
    const children = MetadataUtils.getChildren(jsxMetadata, parentPath)
    let controlProps: ButtonControlProps[] = []

    if (
      children.length === 0 &&
      parentElement.specialSizeMeasurements.layoutSystemForChildren === 'flex'
    ) {
      let direction: 'row' | 'column' | null = null

      switch (parentElement.specialSizeMeasurements.flexDirection) {
        case 'row':
        case 'column':
          direction = parentElement.specialSizeMeasurements.flexDirection
          break
        default:
        // Ignore any other values.
      }

      const beforeX = direction == 'column' ? parentFrame.x - InsertionButtonOffset : parentFrame.x
      const beforeY = direction == 'column' ? parentFrame.y : parentFrame.y - InsertionButtonOffset
      const beforeLineEndX =
        direction == 'column' ? parentFrame.x + parentFrame.width : parentFrame.x
      const beforeLineEndY =
        direction == 'column' ? parentFrame.y : parentFrame.y + parentFrame.height

      if (direction != null) {
        controlProps.push({
          key: 'parent-0',
          scale: scale,
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

    children.forEach((child, index) => {
      const childFrame = MetadataUtils.getFrameInCanvasCoords(child.elementPath, jsxMetadata)
      if (child.specialSizeMeasurements.position !== 'absolute' && childFrame != null) {
        let direction: 'row' | 'column' | null = null
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
        if (direction != null) {
          const positionX =
            direction == 'column'
              ? parentFrame.x - InsertionButtonOffset
              : childFrame.x + childFrame.width
          const positionY =
            direction == 'column'
              ? childFrame.y + childFrame.height
              : parentFrame.y - InsertionButtonOffset

          const lineEndX =
            direction == 'column'
              ? parentFrame.x + parentFrame.width
              : childFrame.x + childFrame.width
          const lineEndY =
            direction == 'column'
              ? childFrame.y + childFrame.height
              : parentFrame.y + parentFrame.height
          controlProps.push({
            key: EP.toString(child.elementPath),
            scale: scale,
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
              direction == 'column' ? parentFrame.x - InsertionButtonOffset : childFrame.x
            const beforeY =
              direction == 'column' ? childFrame.y : parentFrame.y - InsertionButtonOffset
            const beforeLineEndX =
              direction == 'column' ? parentFrame.x + parentFrame.width : childFrame.x
            const beforeLineEndY =
              direction == 'column' ? childFrame.y : parentFrame.y + parentFrame.height
            controlProps.push({
              key: EP.toString(child.elementPath) + '0',
              scale: scale,
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
      }
    })
    return (
      <CanvasOffsetWrapper>
        {controlProps.map((control) => (
          <InsertionButtonContainer {...control} key={control.key} />
        ))}
      </CanvasOffsetWrapper>
    )
  },
)

const InsertionButtonContainer = React.memo((props: ButtonControlProps) => {
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
})

const BlueDot = React.memo((props: ButtonControlProps) => {
  const colorTheme = useColorTheme()
  const BlueDotSize = 7 / props.scale
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
          width: BlueDotSize - 2 / props.scale,
          height: BlueDotSize - 2 / props.scale,
          backgroundColor: colorTheme.canvasSelectionPrimaryOutline.value,
          borderRadius: '50%',
        }}
      />
    </div>
  )
})

const PlusButton = React.memo((props: ButtonControlProps) => {
  const dispatch = useEditorState((store) => store.dispatch, 'PlusButton dispatch')
  const colorTheme = useColorTheme()
  const { parentPath, indexPosition } = props
  const insertElement: React.MouseEventHandler<HTMLDivElement> = React.useCallback(
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

  const ButtonSize = 12

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
        transform: `scale(${1 / props.scale})`,
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
        <div
          style={{
            color: 'white',
            fontSize: 10,
            marginBottom: 2,
          }}
        >
          +
        </div>
      </div>
    </div>
  )
})

const Line = React.memo((props: ButtonControlProps) => {
  const colorTheme = useColorTheme()

  const LineWidth = 1 / props.scale

  return (
    <div
      style={{
        position: 'absolute',
        top: -LineWidth,
        left: -LineWidth,
        width: props.isHorizontalLine ? props.lineEndX - props.positionX : LineWidth,
        height: props.isHorizontalLine ? LineWidth : props.lineEndY - props.positionY,
        backgroundColor: colorTheme.canvasSelectionPrimaryOutline.value,
      }}
    />
  )
})
