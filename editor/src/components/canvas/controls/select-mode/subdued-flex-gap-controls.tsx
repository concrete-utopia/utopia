import React from 'react'
import { useColorTheme } from '../../../../uuiui'
import { Substores, useEditorState, useRefEditorState } from '../../../editor/store/store-hook'
import { useBoundingBox } from '../bounding-box-hooks'
import { CanvasOffsetWrapper } from '../canvas-offset-wrapper'
import type { FlexGapData, PathWithBounds } from '../../gap-utils'
import {
  gapControlBoundsFromMetadata,
  maybeFlexGapData,
  recurseIntoChildrenOfMapOrFragment,
} from '../../gap-utils'
import type { ElementPath } from 'utopia-shared/src/types'
import type { ElementInstanceMetadata } from '../../../../core/shared/element-template'
import { getActivePluginFromEditorState } from '../../plugins/style-plugins'
import { MetadataUtils } from '../../../../core/model/element-metadata-utils'

export interface SubduedFlexGapControlProps {
  hoveredOrFocused: 'hovered' | 'focused'
}

export const SubduedFlexGapControl = React.memo<SubduedFlexGapControlProps>((props) => {
  const { hoveredOrFocused } = props
  const targets = useEditorState(
    Substores.selectedViews,
    (store) => store.editor.selectedViews,
    'SubduedFlexGapControl selectedViews',
  )
  const metadata = useRefEditorState((store) => store.editor.jsxMetadata)
  const elementPathTrees = useRefEditorState((store) => store.editor.elementPathTree)
  const allElementProps = useRefEditorState((store) => store.editor.allElementProps)
  const selectedElement = targets[0]

  const children = recurseIntoChildrenOfMapOrFragment(
    metadata.current,
    allElementProps.current,
    elementPathTrees.current,
    targets[0],
  )

  const flexGap = useEditorState(
    Substores.fullStore,
    (store) =>
      maybeFlexGapData(
        getActivePluginFromEditorState(store.editor).styleInfoFactory({
          projectContents: store.editor.projectContents,
          metadata: store.editor.jsxMetadata,
          elementPathTree: store.editor.elementPathTree,
        })(selectedElement),
        MetadataUtils.findElementByElementPath(store.editor.jsxMetadata, selectedElement),
      ),
    'FlexGapControl flexGap',
  )

  if (flexGap == null) {
    return null
  }

  const flexGapValue = flexGap.value

  const controlBounds = gapControlBoundsFromMetadata(
    metadata.current,
    selectedElement,
    children.map((c) => c.elementPath),
    flexGapValue.renderedValuePx,
    flexGap.direction,
  )

  return (
    <>
      {controlBounds.map((controlBound, i) => (
        <FlexGapControl
          key={i}
          flexGap={flexGap}
          selectedElement={selectedElement}
          hoveredOrFocused={hoveredOrFocused}
          controlBound={controlBound}
          flexChildren={children}
        />
      ))}
    </>
  )
})

function FlexGapControl({
  flexGap,
  selectedElement,
  hoveredOrFocused,
  controlBound,
  flexChildren,
}: {
  selectedElement: ElementPath
  hoveredOrFocused: 'hovered' | 'focused'
  controlBound: PathWithBounds
  flexChildren: Array<ElementInstanceMetadata>
  flexGap: FlexGapData
}) {
  const metadata = useRefEditorState((store) => store.editor.jsxMetadata)

  const sideRef = useBoundingBox([controlBound.path], (ref) => {
    const flexGapValue = flexGap.value

    const controlBounds = gapControlBoundsFromMetadata(
      metadata.current,
      selectedElement,
      flexChildren.map((c) => c.elementPath),
      flexGapValue.renderedValuePx,
      flexGap.direction,
    )

    const bound = controlBounds.find((c) => c.path === controlBound.path)
    if (bound == null) {
      return
    }

    ref.current.style.display = 'block'
    ref.current.style.left = `${bound.bounds.x}px`
    ref.current.style.top = `${bound.bounds.y}px`
    ref.current.style.height = numberToPxValue(bound.bounds.height)
    ref.current.style.width = numberToPxValue(bound.bounds.width)
  })

  const color = useColorTheme().brandNeonPink.value

  const solidOrDashed = hoveredOrFocused === 'focused' ? 'solid' : 'dashed'

  return (
    <CanvasOffsetWrapper>
      <div
        ref={sideRef}
        style={{
          position: 'absolute',
          border: `1px ${solidOrDashed} ${color}`,
        }}
        data-testid={getSubduedFlexGaplTestID(hoveredOrFocused)}
      />
    </CanvasOffsetWrapper>
  )
}

export function getSubduedFlexGaplTestID(hoveredOrFocused: 'hovered' | 'focused'): string {
  return `SubduedFlexGapControl-${hoveredOrFocused}`
}

const numberToPxValue = (n: number) => n + 'px'
