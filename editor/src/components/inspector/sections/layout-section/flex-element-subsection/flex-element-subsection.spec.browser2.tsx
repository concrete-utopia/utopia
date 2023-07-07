import { CanvasControlsContainerID } from '../../../../../components/canvas/controls/new-canvas-controls'
import { mouseClickAtPoint } from '../../../../../components/canvas/event-helpers.test-utils'
import { cmdModifier } from '../../../../../utils/modifiers'
import type { EditorRenderResult } from '../../../../../components/canvas/ui-jsx.test-utils'
import {
  getPrintedUiJsCode,
  renderTestEditorWithCode,
} from '../../../../../components/canvas/ui-jsx.test-utils'
import type {
  FlexDirection,
  LengthPercentUnit,
} from '../../../../../components/inspector/common/css-utils'
import {
  AbsoluteLengthUnits,
  AllFlexDirections,
  LengthPercentUnits,
  LengthUnit,
  LengthUnits,
} from '../../../../../components/inspector/common/css-utils'
import { act, fireEvent } from '@testing-library/react'
import { applyPrettier } from 'utopia-vscode-common'
import { wait } from '../../../../../utils/utils.test-utils'

function getCrossCapitalDimension(flexDirection: FlexDirection): string {
  return flexDirection.startsWith('row') ? 'Height' : 'Width'
}

function createProject(
  flexDirection: FlexDirection,
  width: string | number,
  height: string | number,
): string {
  const crossCapitalDimension = getCrossCapitalDimension(flexDirection)
  return applyPrettier(
    `import * as React from 'react'
  import { Storyboard } from 'utopia-api'
  
  export var storyboard = (
    <Storyboard data-uid='storyboard'>
      <div
        style={{
          backgroundColor: 'grey',
          position: 'absolute',
          left: 111,
          top: 121,
          width: 469,
          height: 310,
          display: 'flex',
          flexDirection: '${flexDirection}',
          paddingTop: 0,
        }}
        data-testid='flexparent'
        data-uid='flexparent'
      >
        <div
          style={{
            backgroundColor: 'blue',
            height: ${JSON.stringify(height)},
            width: ${JSON.stringify(width)},
            min${crossCapitalDimension}: 40,
            max${crossCapitalDimension}: 100,
          }}
          data-testid='flexchild'
          data-uid='flexchild'
        />
      </div>
    </Storyboard>
  )
  `,
    false,
  ).formatted
}

function createNonFlexProject(
  flexDirection: FlexDirection,
  width: string | number,
  height: string | number,
): string {
  const crossCapitalDimension = getCrossCapitalDimension(flexDirection)
  return applyPrettier(
    `import * as React from 'react'
  import { Storyboard } from 'utopia-api'
  
  export var storyboard = (
    <Storyboard data-uid='storyboard'>
      <div
        style={{
          backgroundColor: 'grey',
          position: 'absolute',
          left: 111,
          top: 121,
          width: 469,
          height: 310,
          paddingTop: 0,
        }}
        data-testid='flexparent'
        data-uid='flexparent'
      >
        <div
          style={{
            backgroundColor: 'blue',
            height: ${JSON.stringify(height)},
            width: ${JSON.stringify(width)},
            min${crossCapitalDimension}: 40,
            max${crossCapitalDimension}: 100,
          }}
          data-testid='flexchild'
          data-uid='flexchild'
        />
      </div>
    </Storyboard>
  )
  `,
    false,
  ).formatted
}

function createProjectWithoutMinAndMax(
  flexDirection: FlexDirection,
  width: string | number,
  height: string | number,
): string {
  return applyPrettier(
    `import * as React from 'react'
  import { Storyboard } from 'utopia-api'
  
  export var storyboard = (
    <Storyboard data-uid='storyboard'>
      <div
        style={{
          backgroundColor: 'grey',
          position: 'absolute',
          left: 111,
          top: 121,
          width: 469,
          height: 310,
          display: 'flex',
          flexDirection: '${flexDirection}',
          paddingTop: 0,
        }}
        data-testid='flexparent'
        data-uid='flexparent'
      >
        <div
          style={{
            backgroundColor: 'blue',
            height: ${JSON.stringify(height)},
            width: ${JSON.stringify(width)},
          }}
          data-testid='flexchild'
          data-uid='flexchild'
        />
      </div>
    </Storyboard>
  )
  `,
    false,
  ).formatted
}

function getCrossDimensionTestId(flexDirection: FlexDirection): string {
  return flexDirection.startsWith('row')
    ? 'position-height-number-input'
    : 'position-width-number-input'
}

async function changeDimensionValue(
  editor: EditorRenderResult,
  flexDirection: FlexDirection,
  newValue: string | number,
): Promise<void> {
  // Select the flex child.
  const canvasControlsLayer = editor.renderedDOM.getByTestId(CanvasControlsContainerID)
  const div = editor.renderedDOM.getByTestId('flexchild')
  const divBounds = div.getBoundingClientRect()
  const divCorner = {
    x: divBounds.x + 50,
    y: divBounds.y + 40,
  }

  await act(async () => {
    await mouseClickAtPoint(canvasControlsLayer, divCorner, { modifiers: cmdModifier })
  })

  // Modify the value.
  const dimensionInput = editor.renderedDOM.getByTestId(
    getCrossDimensionTestId(flexDirection),
  ) as HTMLInputElement
  const dimensionInputBounds = dimensionInput.getBoundingClientRect()
  const dimensionInputCenter = {
    x: dimensionInputBounds.x + dimensionInputBounds.width / 2,
    y: dimensionInputBounds.y + dimensionInputBounds.height / 2,
  }

  await act(async () => {
    await mouseClickAtPoint(dimensionInput, dimensionInputCenter, {})
  })

  act(() => {
    fireEvent.change(dimensionInput, { target: { value: newValue } })
    fireEvent.blur(dimensionInput)
  })

  await editor.getDispatchFollowUpActionsFinished()
}

// These flex dimenson controls no longer exist
xdescribe('flex dimension controls', () => {
  // For every unit added on the end and for when no unit is explicitly supplied...
  for (const lengthUnit of ['fr', '%', null] as const) {
    // Common values relating to the length unit.
    const unitText = lengthUnit == null ? 'with no unit' : `with a unit of ${lengthUnit}`
    const newValue = lengthUnit == null ? 90 : `90${lengthUnit}`
    const isFixedUnit =
      lengthUnit == null
        ? true
        : (AbsoluteLengthUnits as Array<LengthPercentUnit>).includes(lengthUnit)
    // eslint-disable-next-line jest/valid-title
    describe(unitText, () => {
      // For every possible flex direction...
      for (const flexDirection of AllFlexDirections) {
        // Common properties relating to the flex direction.
        const isRow = flexDirection.startsWith('row')
        const crossCapitalDimension = getCrossCapitalDimension(flexDirection)
        if (isFixedUnit) {
          // If the unit is a fixed dimension, remove the additional properties.
          it(`with a direction of ${flexDirection} when the ${crossCapitalDimension.toLowerCase()} is changed min${crossCapitalDimension} and max${crossCapitalDimension} are removed`, async () => {
            const editor = await renderTestEditorWithCode(
              createProject(flexDirection, 100, 120),
              'await-first-dom-report',
            )
            await changeDimensionValue(editor, flexDirection, newValue)

            expect(getPrintedUiJsCode(editor.getEditorState())).toEqual(
              createProjectWithoutMinAndMax(
                flexDirection,
                isRow ? 100 : newValue,
                isRow ? newValue : 120,
              ),
            )
          })
        } else {
          // If it is a proportional dimension, the minX and maxX properties remain.
          it(`with a direction of ${flexDirection} when the ${crossCapitalDimension.toLowerCase()} is changed min${crossCapitalDimension} and max${crossCapitalDimension} are not removed`, async () => {
            const editor = await renderTestEditorWithCode(
              createProject(flexDirection, 100, 120),
              'await-first-dom-report',
            )
            await changeDimensionValue(editor, flexDirection, newValue)

            expect(getPrintedUiJsCode(editor.getEditorState())).toEqual(
              createProject(flexDirection, isRow ? 100 : newValue, isRow ? newValue : 120),
            )
          })
        }
      }
    })
  }

  it("for non-flex child, the control doesn't show up", async () => {
    const editor = await renderTestEditorWithCode(
      createNonFlexProject('row', 100, 120),
      'await-first-dom-report',
    )

    expect(editor.renderedDOM.queryByTestId(getCrossDimensionTestId('row'))).toBeNull()
  })
})
