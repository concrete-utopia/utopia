import { ControlProps } from './new-canvas-controls'
import * as React from 'react'
import { MetadataUtils } from '../../../core/model/element-metadata-utils'
import * as TP from '../../../core/shared/template-path'
import { colorTheme } from '../../../uuiui'
import { useEditorState } from '../../editor/store/store-hook'
import { insertJSXElement } from '../../editor/actions/actions'
import { TemplatePath } from '../../../core/shared/project-file-types'
import { defaultInsertableDivElement } from '../../editor/defaults'
import uuid = require('uuid')
import { IndexPosition } from '../../../utils/utils'

const InsertionButtonOffset = 10

interface ButtonControlProps {
  key: string
  positionX: number
  positionY: number
  lineEndX: number
  lineEndY: number
  isHorizontalLine: boolean
  parentPath: TemplatePath
  indexPosition: IndexPosition
}

export const InsertionControls = (props: ControlProps): JSX.Element | null => {
  if (props.selectedViews.length !== 1) {
    return null
  }
  const selectedView = props.selectedViews[0]
  const parentPath = TP.parentPath(selectedView)
  const parentFrame =
    parentPath != null
      ? MetadataUtils.getFrameInCanvasCoords(parentPath, props.componentMetadata)
      : null
  const parentElement =
    parentPath != null
      ? MetadataUtils.getElementByTemplatePathMaybe(props.componentMetadata.elements, parentPath)
      : null
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
      child.templatePath,
      props.componentMetadata,
    )
    if (child.specialSizeMeasurements.position !== 'absolute' && childFrame != null) {
      let direction: 'row' | 'column' =
        child.specialSizeMeasurements.parentLayoutSystem === 'flex'
          ? parentElement.props?.style?.flexDirection || 'row'
          : 'column'
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
        key: TP.toString(child.templatePath),
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
          key: TP.toString(child.templatePath) + '0',
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
}

const InsertionButtonContainer = (props: ButtonControlProps) => {
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
}
const BlueDotSize = 7
const BlueDot = (props: ButtonControlProps) => {
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
}

const ButtonSize = 12
const PlusButton = (props: ButtonControlProps) => {
  const dispatch = useEditorState((store) => store.dispatch, 'plus-button')
  const { parentPath, indexPosition } = props
  const insertElement = React.useCallback(() => {
    dispatch(
      [insertJSXElement(defaultInsertableDivElement(uuid()), parentPath, {}, indexPosition)],
      'canvas',
    )
  }, [dispatch, parentPath, indexPosition])
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
}

const Line = (props: ButtonControlProps) => {
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
}
