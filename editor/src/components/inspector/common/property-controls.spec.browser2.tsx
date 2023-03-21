import { selectComponents } from '../../editor/actions/action-creators'
import { createModifiedProject } from '../../../sample-projects/sample-project-utils.test-utils'
import { EditorRenderResult, renderTestEditorWithModel } from '../../canvas/ui-jsx.test-utils'
import {
  StoryboardFilePath,
  withUnderlyingTargetFromEditorState,
} from '../../editor/store/editor-state'
import * as EP from '../../../core/shared/element-path'
import { ElementPath } from '../../../core/shared/project-file-types'
import { fireEvent } from '@testing-library/react'
import { act } from 'react-dom/test-utils'
import {
  clearExpressionUniqueIDs,
  emptyComments,
  getJSXAttribute,
  isJSXElement,
  jsExpressionValue,
} from '../../../core/shared/element-template'

function exampleProject(): string {
  return `import * as React from "react";
import { Scene, Storyboard, jsx } from "utopia-api";
export var App = (props) => {
  return (
    <div
      data-uid="other-app-root"
      data-testid="other-app-root"
      style={{
        width: "100%",
        height: "100%",
        backgroundColor: "#FFFFFF",
        position: "relative",
      }}
    >
      {JSON.stringify(props.cards)}
    </div>
  );
};
export var storyboard = (
  <Storyboard data-uid="storyboard">
    <Scene
      data-uid="scene"
      data-testid="scene"
      style={{ position: "absolute", left: 0, top: 0, width: 375, height: 812 }}
    >
      <App
        data-uid="app"
        data-testid="app"
        cards={[
          { hello: 'bello', n: 1 },
          { hello: 'yes', n: 5 },
        ]}
      />
    </Scene>
  </Storyboard>
);
`
}

function createAndRenderModifiedProject(modifiedFiles: {
  [filename: string]: string
}): Promise<EditorRenderResult> {
  const project = createModifiedProject(modifiedFiles)
  return renderTestEditorWithModel(project, 'await-first-dom-report')
}

function createExampleProject(): Promise<EditorRenderResult> {
  return createAndRenderModifiedProject({
    [StoryboardFilePath]: exampleProject(),
  })
}

const appElementPath: ElementPath = EP.elementPath([['storyboard', 'scene', 'app']])

function downClickUp(targetElement: HTMLElement, mouseEventInit: MouseEventInit) {
  fireEvent(targetElement, new MouseEvent('mousedown', mouseEventInit))
  fireEvent(targetElement, new MouseEvent('click', mouseEventInit))
  fireEvent(targetElement, new MouseEvent('mouseup', mouseEventInit))
}

describe('Automatically derived property controls', () => {
  it('should support adding an additional entry', async () => {
    // Setup the editor.
    const renderResult = await createExampleProject()
    await renderResult.dispatch([selectComponents([appElementPath], false)], true)

    // Add another entry using the plus button.
    const toggleInsertCardsButton = await renderResult.renderedDOM.findByTestId(
      'toggle-insert-cards',
    )
    const toggleInsertCardsButtonBounds = toggleInsertCardsButton.getBoundingClientRect()
    act(() => {
      downClickUp(toggleInsertCardsButton, {
        detail: 1,
        bubbles: true,
        cancelable: true,
        clientX: toggleInsertCardsButtonBounds.x + 3,
        clientY: toggleInsertCardsButtonBounds.y + 3,
        buttons: 1,
      })
    })

    // Assume the third one of these is the just added input for the "hello" field.
    const allInputsForHello = await renderResult.renderedDOM.findAllByTestId(
      'hello-string-input-property-control',
    )
    expect(allInputsForHello.length).toEqual(3)
    const latestHelloInput = allInputsForHello[2]
    const latestHelloInputBounds = latestHelloInput.getBoundingClientRect()

    // Click inside the input box and enter the value "x" into the input box.
    act(() => {
      downClickUp(latestHelloInput, {
        detail: 1,
        bubbles: true,
        cancelable: true,
        clientX: latestHelloInputBounds.x + latestHelloInputBounds.width / 2,
        clientY: latestHelloInputBounds.y + latestHelloInputBounds.height / 2,
        buttons: 1,
      })
      fireEvent.change(latestHelloInput, { target: { value: 'abc' } })
      fireEvent.blur(latestHelloInput)
    })

    // Assume the third one of these is the just added input for the "n" field.
    const allInputsForN = await renderResult.renderedDOM.findAllByTestId(
      'n-number-input-property-control',
    )
    expect(allInputsForN.length).toEqual(3)
    const latestNInput = allInputsForN[2]
    const latestNInputBounds = latestNInput.getBoundingClientRect()

    // Click inside the input box.
    act(() => {
      downClickUp(latestNInput, {
        detail: 1,
        bubbles: true,
        cancelable: true,
        clientX: latestNInputBounds.x + latestNInputBounds.width / 2,
        clientY: latestNInputBounds.y + latestNInputBounds.height / 2,
        buttons: 1,
      })
    })

    // Assume the third one of these is the just added input for the "n" field, which should
    // still exist at this point.
    const allInputsForNSecondTime = await renderResult.renderedDOM.findAllByTestId(
      'n-number-input-property-control',
    )
    expect(allInputsForNSecondTime.length).toEqual(3)
    const latestNInputForTheSecondTime = allInputsForNSecondTime[2]

    // Change the content of the 'n' field.
    act(() => {
      fireEvent.change(latestNInputForTheSecondTime, { target: { value: '50' } })
      fireEvent.blur(latestNInputForTheSecondTime)
    })

    // Check the result that has been set into
    const element = withUnderlyingTargetFromEditorState(
      EP.fromString('storyboard/scene/app'),
      renderResult.getEditorState().editor,
      null,
      (_success, underlyingElement) => {
        return underlyingElement
      },
    )
    if (element == null) {
      throw new Error('Element could not be found.')
    } else if (isJSXElement(element)) {
      const cardsAttribute = getJSXAttribute(element.props, 'cards')
      if (cardsAttribute == null) {
        throw new Error("The 'cards' attribute does not exist.")
      } else {
        expect(clearExpressionUniqueIDs(cardsAttribute)).toEqual(
          jsExpressionValue(
            [
              { hello: 'bello', n: 1 },
              { hello: 'yes', n: 5 },
              { hello: 'abc', n: 50 },
            ],
            emptyComments,
            '',
          ),
        )
      }
    } else {
      throw new Error('Was not a JSXElement.')
    }
  })
})
