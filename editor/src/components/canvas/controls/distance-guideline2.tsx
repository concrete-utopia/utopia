import styled from '@emotion/styled'
import { jsx, ThemeProvider } from '@emotion/react'

import React from 'react'
import { ElementPath } from '../../../core/shared/project-file-types'
import { useColorTheme } from '../../../uuiui'
import { useEditorState } from '../../editor/store/store-hook'
import { useClosestDistanceGuideline } from './distance-guideline-hooks'

interface DistanceGuidelineProps {
  localSelectedElements: Array<ElementPath>
}

const FontSize = 11
const LineEndSegmentSize = 3.5
const StrokeWidth = 1
export const DistanceGuideline = React.memo<DistanceGuidelineProps>((props) => {
  const isInteraction = useEditorState(
    (store) =>
      store.editor.canvas.dragState != null || store.editor.canvas.interactionSession != null,
    'DistanceGuideline isInteraction',
  )
  const isAltPressed = useEditorState(
    (store) => store.editor.keysPressed['alt'],
    'DistanceGuideline isAltPressed',
  )

  const colorTheme = useColorTheme()

  if (props.localSelectedElements.length > 0) {
    return (
      <ThemeProvider theme={colorTheme}>
        <div
          className='role-distance-guideline'
          style={{
            position: 'absolute',
            display: 'block',
          }}
        >
          <GuidelineHorizontal localSelectedElements={props.localSelectedElements} />
          <GuidelineVertical localSelectedElements={props.localSelectedElements} />
        </div>
      </ThemeProvider>
    )
  } else {
    return null
  }
})

const GuidelineHorizontal = React.memo<DistanceGuidelineProps>((props) => {
  const guidelineHorizontal = useClosestDistanceGuideline(
    props.localSelectedElements,
    'XAxisGuideline',
    (ref, guidelineRect, distance) => {
      if (guidelineRect == null || distance == null) {
        //
      } else {
        ref.current.style.setProperty('left', `${guidelineRect.x}px`)
        ref.current.style.setProperty('top', `${guidelineRect.y}px`)
        ref.current.style.setProperty(`width`, `${guidelineRect.width}px`)
      }
    },
  )
  const guidelineHorizontalText = useClosestDistanceGuideline(
    props.localSelectedElements,
    'XAxisGuideline',
    (ref, guidelineRect, distance) => {
      if (guidelineRect == null || distance == null) {
        //
      } else {
        ref.current.style.setProperty(`left`, `${guidelineRect.x}px`)
        ref.current.style.setProperty(`top`, `${guidelineRect.y}px`)
        ref.current.style.setProperty(`width`, `${guidelineRect.width}px`)

        ref.current.innerHTML = `${distance}`
      }
    },
  )

  return (
    <>
      <GuidelineHorizontalBase ref={guidelineHorizontal}>
        <GuidelineHorizontalStart />
        <GuidelineHorizontalLine />
        <GuidelineHorizontalEnd />
      </GuidelineHorizontalBase>
      <GuidelineHorizontalText ref={guidelineHorizontalText} />
    </>
  )
})

const GuidelineHorizontalBase = styled.div`
  position: absolute;
  pointer-events: none;
  transform: translate(-${StrokeWidth / 2}px, ${StrokeWidth / 2}px);
`
const GuidelineHorizontalStart = styled.div`
  position: relative;
  display: inline-block;
  background-color: ${(props) => props.theme.canvasLayoutStroke.value};
  top: calc(${-LineEndSegmentSize}px / 1);
  height: calc(${LineEndSegmentSize * 2}px / 1);
  width: calc(${StrokeWidth}px / 1);
`
const GuidelineHorizontalLine = styled.div`
  position: relative;
  display: inline-block;
  background-color: ${(props) => props.theme.canvasLayoutStroke.value};
  width: 100%;
  top: calc(${-LineEndSegmentSize * 2}px / 1);
  height: calc(${StrokeWidth}px / 1);
`
const GuidelineHorizontalEnd = styled.div`
  position: relative;
  display: inline-block;
  background-color: ${(props) => props.theme.canvasLayoutStroke.value};
  top: calc(${-LineEndSegmentSize}px / 1);
  height: calc(${LineEndSegmentSize * 2}px / 1);
  width: calc(${StrokeWidth}px / 1);
`

const GuidelineHorizontalText = styled.div`
  position: absolute;
  pointer-events: none;
  text-align: center;
  color: ${(props) => props.theme.canvasLayoutStroke.value};
  font-size: calc(${FontSize}px / 1);
  transform: translate(${StrokeWidth / 2}px, ${FontSize / 2}px);
`

const GuidelineVertical = React.memo<DistanceGuidelineProps>((props) => {
  const guidelineVertical = useClosestDistanceGuideline(
    props.localSelectedElements,
    'YAxisGuideline',
    (ref, guidelineRect, distance) => {
      if (guidelineRect == null || distance == null) {
        //
      } else {
        ref.current.style.setProperty('left', `${guidelineRect.x}px`)
        ref.current.style.setProperty('top', `${guidelineRect.y}px`)
        ref.current.style.setProperty('height', `${guidelineRect.height}px`)
      }
    },
  )
  const guidelineVerticalText = useClosestDistanceGuideline(
    props.localSelectedElements,
    'YAxisGuideline',
    (ref, guidelineRect, distance) => {
      if (guidelineRect == null || distance == null) {
        //
      } else {
        ref.current.style.setProperty('left', `${guidelineRect.x}px`)
        ref.current.style.setProperty('top', `${guidelineRect.y}px`)
        ref.current.style.setProperty('height', `${guidelineRect.height}px`)
        ref.current.innerHTML = `${distance}`
      }
    },
  )

  return (
    <>
      <GuidelineVerticalBase ref={guidelineVertical}>
        <GuidelineVerticalStart />
        <GuidelineVerticalLine />
        <GuidelineVerticalEnd />
      </GuidelineVerticalBase>
      <GuidelineVerticalText ref={guidelineVerticalText} />
    </>
  )
})

const GuidelineVerticalBase = styled.div`
  position: absolute;
  pointer-events: none;
`
const GuidelineVerticalStart = styled.div`
  position: relative;
  background-color: ${(props) => props.theme.canvasLayoutStroke.value};
  left: calc(${-LineEndSegmentSize}px / 1);
  width: calc(${LineEndSegmentSize * 2}px / 1);
  height: calc(${StrokeWidth}px / 1);
`
const GuidelineVerticalLine = styled.div`
  position: relative;
  background-color: ${(props) => props.theme.canvasLayoutStroke.value};
  height: 100%;
  left: calc(${-StrokeWidth / 2}px / 1);
  width: calc(${StrokeWidth}px / 1);
`
const GuidelineVerticalEnd = styled.div`
  position: relative;
  background-color: ${(props) => props.theme.canvasLayoutStroke.value};
  left: calc(${-LineEndSegmentSize}px / 1);
  width: calc(${LineEndSegmentSize * 2}px / 1);
  height: calc(${StrokeWidth}px / 1);
`

const GuidelineVerticalText = styled.div`
  position: absolute;
  pointer-events: none;
  text-align: center;
  color: ${(props) => props.theme.canvasLayoutStroke.value};
  font-size: calc(${FontSize}px / 1);
`
