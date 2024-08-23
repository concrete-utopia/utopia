import { MetadataUtils } from '../../../../core/model/element-metadata-utils'
import type { CanvasPoint } from '../../../../core/shared/math-utils'
import {
  boundingRectangleArray,
  canvasPoint,
  canvasRectangle,
  getRectCenter,
  zeroRectIfNullOrInfinity,
} from '../../../../core/shared/math-utils'
import { forceNotNull } from '../../../../core/shared/optional-utils'
import { mouseMoveToPoint } from '../../event-helpers.test-utils'
import type { EditorRenderResult } from '../../ui-jsx.test-utils'
import { FlexGapControlHandleTestId, FlexGapControlTestId } from './flex-gap-control'
import * as EP from '../../../../core/shared/element-path'
import { getDomRectCenter } from '../../../../core/shared/dom-utils'
import { isReversedFlexDirection } from '../../../../core/model/flex-utils'
import { reverse } from '../../../../core/shared/array-utils'

function mainAxisCenter(frontStart: number, frontSize: number, backStart: number): number {
  // Dependent on the width of the front element.
  const frontEnd = frontStart + frontSize
  const actualGapSize = backStart - frontEnd
  return frontEnd + actualGapSize / 2
}

export async function checkFlexGapHandlesPositionedCorrectly(
  renderResult: EditorRenderResult,
  targetTestId: string,
): Promise<void> {
  const editorState = renderResult.getEditorState().editor
  // Currently the flex gap controls are only shown if a single element is selected.
  if (editorState.selectedViews.length === 1) {
    const targetPath = editorState.selectedViews[0]
    const selectedElement = forceNotNull(
      'Should be able to get metadata for selected element.',
      MetadataUtils.findElementByElementPath(editorState.jsxMetadata, targetPath),
    )
    const localFrame = MetadataUtils.getLocalFrame(
      selectedElement.elementPath,
      editorState.jsxMetadata,
    )
    const selectedElementFrame = zeroRectIfNullOrInfinity(localFrame)
    // If this is a flex element and it has a gap specified.
    if (
      selectedElement.specialSizeMeasurements.layoutSystemForChildren === 'flex' &&
      selectedElement.specialSizeMeasurements.gap != null
    ) {
      const flexDirection = selectedElement.specialSizeMeasurements.flexDirection
      const isHorizontal = flexDirection?.startsWith('row')
      // Move the mouse pointer to the center of the flex element.
      const flexGapControl = await renderResult.renderedDOM.findByTestId(FlexGapControlTestId)
      const flexGapControlBounds = flexGapControl.getBoundingClientRect()
      // Need to use the width and height from the selected element not the control...
      const flexElementDomBounds = canvasRectangle({
        x: flexGapControlBounds.x,
        y: flexGapControlBounds.y,
        width: selectedElementFrame.width,
        height: selectedElementFrame.height,
      })
      // ...as the control has no size.
      const flexGapControlCenter = getRectCenter(flexElementDomBounds)
      await mouseMoveToPoint(flexGapControl, flexGapControlCenter)
      await renderResult.getDispatchFollowUpActionsFinished()

      // Find the element that we're targeting in the canvas.
      const targetFlexContainer = await renderResult.renderedDOM.findByTestId(targetTestId)

      // Get the children of the target element.
      let children = MetadataUtils.getChildrenOrdered(
        editorState.jsxMetadata,
        editorState.elementPathTree,
        targetPath,
      )
      // The children of the flex container on the canvas.
      let childDOMElements = Array.from(targetFlexContainer.children)

      // When the flex direction is reversed, the gap handling needs to be similarly
      // reversed as the DOM entries are reversed visually but not in the DOM itself.
      if (flexDirection != null && isReversedFlexDirection(flexDirection)) {
        children = reverse(children)
        childDOMElements = reverse(childDOMElements)
      }

      const totalChildElementBounds = zeroRectIfNullOrInfinity(
        boundingRectangleArray(
          childDOMElements.map((elem) => {
            return canvasRectangle(elem.getBoundingClientRect())
          }),
        ),
      )

      // Check that we have the same number of children as child DOM elements.
      expect(childDOMElements).toHaveLength(children.length)

      // Check the various pairs of children...
      for (
        let frontChildIndex: number = 0;
        frontChildIndex < childDOMElements.length - 1;
        frontChildIndex++
      ) {
        const frontChild = childDOMElements[frontChildIndex]
        const frontChildBounds = frontChild.getBoundingClientRect()
        const backChild = childDOMElements[frontChildIndex + 1]
        const backChildBounds = backChild.getBoundingClientRect()
        const expectedHandleCenter: CanvasPoint = canvasPoint({
          x: isHorizontal
            ? mainAxisCenter(frontChildBounds.x, frontChildBounds.width, backChildBounds.x)
            : totalChildElementBounds.x + totalChildElementBounds.width / 2,
          y: isHorizontal
            ? totalChildElementBounds.y + totalChildElementBounds.height / 2
            : mainAxisCenter(frontChildBounds.y, frontChildBounds.height, backChildBounds.y),
        })

        const frontChildPath = children[frontChildIndex].elementPath

        // eslint-disable-next-line no-await-in-loop
        const handleElement = await renderResult.renderedDOM.findByTestId(
          `${FlexGapControlHandleTestId}-${EP.toString(frontChildPath)}`,
        )
        const handleRect = handleElement.getBoundingClientRect()
        const handleCenter = getDomRectCenter(handleRect)
        // Check the handle center is close to the center of the child pair.
        expect(handleCenter.x).toBeGreaterThanOrEqual(expectedHandleCenter.x - 1)
        expect(handleCenter.x).toBeLessThanOrEqual(expectedHandleCenter.x + 1)
        expect(handleCenter.y).toBeGreaterThanOrEqual(expectedHandleCenter.y - 1)
        expect(handleCenter.y).toBeLessThanOrEqual(expectedHandleCenter.y + 1)
      }
    }
  }
}
