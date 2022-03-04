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
        <style>{`
          .utopia-canvas-guideline: {
            --utopia-canvas-guideline-x: 0px;
            --utopia-canvas-guideline-y: 0px;
            --utopia-canvas-guideline-width: 0px;
            --utopia-canvas-guideline-height: 0px;
            --utopia-canvas-guideline-display: 'none';
          }
        `}</style>
        <div
          className='role-distance-guideline'
          style={{
            position: 'absolute',
            display:
              // isAltPressed && !isInteraction ? `var(--utopia-canvas-guideline-display)` : 'none',
              `var(--utopia-canvas-guideline-display)`,
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
        ref.current.style.setProperty(`--utopia-canvas-guideline-display`, 'none')
      } else {
        ref.current.style.setProperty(`--utopia-canvas-guideline-x`, `${guidelineRect.x}px`)
        ref.current.style.setProperty(`--utopia-canvas-guideline-y`, `${guidelineRect.y}px`)
        ref.current.style.setProperty(`--utopia-canvas-guideline-width`, `${guidelineRect.width}px`)
        ref.current.style.setProperty(
          `--utopia-canvas-guideline-display`,
          distance > 0 ? 'block' : 'none',
        )
      }
    },
  )
  const guidelineHorizontalText = useClosestDistanceGuideline(
    props.localSelectedElements,
    'XAxisGuideline',
    (ref, guidelineRect, distance) => {
      if (guidelineRect == null || distance == null) {
        ref.current.style.setProperty(`--utopia-canvas-guideline-display`, 'none')
      } else {
        ref.current.style.setProperty(`--utopia-canvas-guideline-x`, `${guidelineRect.x}px`)
        ref.current.style.setProperty(`--utopia-canvas-guideline-y`, `${guidelineRect.y}px`)
        ref.current.style.setProperty(`--utopia-canvas-guideline-width`, `${guidelineRect.width}px`)
        ref.current.style.setProperty(
          `--utopia-canvas-guideline-display`,
          distance > 0 ? 'block' : 'none',
        )
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
  background-color: calc(var(--utopia-canvas-guideline-display));
  display: var(--utopia-canvas-guideline-display);
  left: calc(var(--utopia-canvas-guideline-x) - ${StrokeWidth / 2}px / var(--utopia-canvas-scale));
  top: calc(var(--utopia-canvas-guideline-y) + ${StrokeWidth / 2}px / var(--utopia-canvas-scale));
  width: calc(
    var(--utopia-canvas-guideline-width) - ${StrokeWidth / 2}px / var(--utopia-canvas-scale) * 3
  );
`
const GuidelineHorizontalStart = styled.div`
  position: relative;
  display: inline-block;
  background-color: ${(props) => props.theme.canvasLayoutStroke.value};
  top: calc(${-LineEndSegmentSize}px / var(--utopia-canvas-scale));
  height: calc(${LineEndSegmentSize * 2}px / var(--utopia-canvas-scale));
  width: calc(${StrokeWidth}px / var(--utopia-canvas-scale));
`
const GuidelineHorizontalLine = styled.div`
  position: relative;
  display: inline-block;
  background-color: ${(props) => props.theme.canvasLayoutStroke.value};
  width: 100%;
  top: calc(${-LineEndSegmentSize * 2}px / var(--utopia-canvas-scale));
  height: calc(${StrokeWidth}px / var(--utopia-canvas-scale));
`
const GuidelineHorizontalEnd = styled.div`
  position: relative;
  display: inline-block;
  background-color: ${(props) => props.theme.canvasLayoutStroke.value};
  top: calc(${-LineEndSegmentSize}px / var(--utopia-canvas-scale));
  height: calc(${LineEndSegmentSize * 2}px / var(--utopia-canvas-scale));
  width: calc(${StrokeWidth}px / var(--utopia-canvas-scale));
`

const GuidelineHorizontalText = styled.div`
  position: absolute;
  pointer-events: none;
  text-align: center;
  color: ${(props) => props.theme.canvasLayoutStroke.value};
  display: var(--utopia-canvas-guideline-display);
  font-size: calc(${FontSize}px / var(--utopia-canvas-scale));
  left: calc(var(--utopia-canvas-guideline-x) + ${StrokeWidth / 2}px / var(--utopia-canvas-scale));
  top: calc(var(--utopia-canvas-guideline-y) + ${FontSize / 2}px / var(--utopia-canvas-scale));
  width: calc(
    var(--utopia-canvas-guideline-width) - ${StrokeWidth / 2}px / var(--utopia-canvas-scale) * 3
  );
`

const GuidelineVertical = React.memo<DistanceGuidelineProps>((props) => {
  const guidelineVertical = useClosestDistanceGuideline(
    props.localSelectedElements,
    'YAxisGuideline',
    (ref, guidelineRect, distance) => {
      if (guidelineRect == null || distance == null) {
        ref.current.style.setProperty(`--utopia-canvas-guideline-display`, 'none')
      } else {
        ref.current.style.setProperty(`--utopia-canvas-guideline-x`, `${guidelineRect.x}px`)
        ref.current.style.setProperty(`--utopia-canvas-guideline-y`, `${guidelineRect.y}px`)
        ref.current.style.setProperty(
          `--utopia-canvas-guideline-height`,
          `${guidelineRect.height}px`,
        )
        ref.current.style.setProperty(
          `--utopia-canvas-guideline-display`,
          distance > 0 ? 'block' : 'none',
        )
      }
    },
  )
  const guidelineVerticalText = useClosestDistanceGuideline(
    props.localSelectedElements,
    'YAxisGuideline',
    (ref, guidelineRect, distance) => {
      if (guidelineRect == null || distance == null) {
        ref.current.style.setProperty(`--utopia-canvas-guideline-display`, 'none')
      } else {
        ref.current.style.setProperty(`--utopia-canvas-guideline-x`, `${guidelineRect.x}px`)
        ref.current.style.setProperty(`--utopia-canvas-guideline-y`, `${guidelineRect.y}px`)
        ref.current.style.setProperty(
          `--utopia-canvas-guideline-height`,
          `${guidelineRect.height}px`,
        )
        ref.current.style.setProperty(
          `--utopia-canvas-guideline-display`,
          distance > 0 ? 'block' : 'none',
        )
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
  display: var(--utopia-canvas-guideline-display);
  left: calc(var(--utopia-canvas-guideline-x) + ${StrokeWidth / 2}px / var(--utopia-canvas-scale));
  top: calc(var(--utopia-canvas-guideline-y) - ${StrokeWidth / 2}px / var(--utopia-canvas-scale));
  height: calc(
    var(--utopia-canvas-guideline-height) - ${StrokeWidth / 2}px / var(--utopia-canvas-scale) * 3
  );
`
const GuidelineVerticalStart = styled.div`
  position: relative;
  background-color: ${(props) => props.theme.canvasLayoutStroke.value};
  left: calc(${-LineEndSegmentSize}px / var(--utopia-canvas-scale));
  width: calc(${LineEndSegmentSize * 2}px / var(--utopia-canvas-scale));
  height: calc(${StrokeWidth}px / var(--utopia-canvas-scale));
`
const GuidelineVerticalLine = styled.div`
  position: relative;
  background-color: ${(props) => props.theme.canvasLayoutStroke.value};
  height: 100%;
  left: calc(${-StrokeWidth / 2}px / var(--utopia-canvas-scale));
  width: calc(${StrokeWidth}px / var(--utopia-canvas-scale));
`
const GuidelineVerticalEnd = styled.div`
  position: relative;
  background-color: ${(props) => props.theme.canvasLayoutStroke.value};
  left: calc(${-LineEndSegmentSize}px / var(--utopia-canvas-scale));
  width: calc(${LineEndSegmentSize * 2}px / var(--utopia-canvas-scale));
  height: calc(${StrokeWidth}px / var(--utopia-canvas-scale));
`

const GuidelineVerticalText = styled.div`
  position: absolute;
  pointer-events: none;
  text-align: center;
  color: ${(props) => props.theme.canvasLayoutStroke.value};
  display: var(--utopia-canvas-guideline-display);
  font-size: calc(${FontSize}px / var(--utopia-canvas-scale));
  left: calc(var(--utopia-canvas-guideline-x) + ${FontSize / 2}px / var(--utopia-canvas-scale));
  top: calc(var(--utopia-canvas-guideline-y) + ${StrokeWidth / 2}px / var(--utopia-canvas-scale));
  height: calc(
    var(--utopia-canvas-guideline-height) - ${StrokeWidth / 2}px / var(--utopia-canvas-scale) * 3
  );
  line-height: calc(
    var(--utopia-canvas-guideline-height) - ${StrokeWidth / 2}px / var(--utopia-canvas-scale) * 3
  );
`
