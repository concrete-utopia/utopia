/// <reference types="karma-viewport" />
import { act, fireEvent } from '@testing-library/react'
import { BakedInStoryboardUID } from '../../../../core/model/scene-utils'
import { canvasPoint } from '../../../../core/shared/math-utils'
import * as EP from '../../../../core/shared/element-path'
import {
  makeTestProjectCodeWithSnippet,
  renderTestEditorWithCode,
  TestAppUID,
  TestScenePath,
  TestSceneUID,
} from '../../ui-jsx.test-utils'
import {
  selectComponents,
  setCursorOverlay,
  setFocusedElement,
} from '../../../editor/actions/action-creators'
import CanvasActions from '../../canvas-actions'
import { CanvasControlsContainerID } from '../new-canvas-controls'
import { SceneLabelTestID } from './scene-label'
import { CSSCursor } from '../../../../uuiui-deps'

function fireSingleClickEvents(target: HTMLElement, clientX: number, clientY: number) {
  fireEvent(
    target,
    new MouseEvent('mousemove', {
      bubbles: true,
      cancelable: true,
      clientX: clientX,
      clientY: clientY,
    }),
  )
  fireEvent(
    target,
    new MouseEvent('mousedown', {
      detail: 1,
      bubbles: true,
      cancelable: true,
      metaKey: false,
      clientX: clientX,
      clientY: clientY,
      buttons: 1,
    }),
  )
  fireEvent(
    target,
    new MouseEvent('mouseup', {
      detail: 1,
      bubbles: true,
      cancelable: true,
      metaKey: false,
      clientX: clientX,
      clientY: clientY,
      buttons: 1,
    }),
  )
  fireEvent(
    target,
    new MouseEvent('click', {
      detail: 1,
      bubbles: true,
      cancelable: true,
      metaKey: false,
      clientX: clientX,
      clientY: clientY,
      buttons: 1,
    }),
  )
}

function fireDoubleClickEvents(target: HTMLElement, clientX: number, clientY: number) {
  fireEvent(
    target,
    new MouseEvent('mousemove', {
      bubbles: true,
      cancelable: true,
      clientX: clientX,
      clientY: clientY,
    }),
  )
  fireEvent(
    target,
    new MouseEvent('mousedown', {
      detail: 1,
      bubbles: true,
      cancelable: true,
      metaKey: false,
      clientX: clientX,
      clientY: clientY,
      buttons: 1,
    }),
  )
  fireEvent(
    target,
    new MouseEvent('mouseup', {
      detail: 1,
      bubbles: true,
      cancelable: true,
      metaKey: false,
      clientX: clientX,
      clientY: clientY,
      buttons: 1,
    }),
  )
  fireEvent(
    target,
    new MouseEvent('click', {
      detail: 1,
      bubbles: true,
      cancelable: true,
      metaKey: false,
      clientX: clientX,
      clientY: clientY,
      buttons: 1,
    }),
  )
  fireEvent(
    target,
    new MouseEvent('mousedown', {
      detail: 2,
      bubbles: true,
      cancelable: true,
      metaKey: false,
      clientX: clientX,
      clientY: clientY,
      buttons: 1,
    }),
  )
  fireEvent(
    target,
    new MouseEvent('mouseup', {
      detail: 2,
      bubbles: true,
      cancelable: true,
      metaKey: false,
      clientX: clientX,
      clientY: clientY,
      buttons: 1,
    }),
  )
  fireEvent(
    target,
    new MouseEvent('click', {
      detail: 2,
      bubbles: true,
      cancelable: true,
      metaKey: false,
      clientX: clientX,
      clientY: clientY,
      buttons: 1,
    }),
  )
  fireEvent(
    target,
    new MouseEvent('dblclick', {
      detail: 2,
      bubbles: true,
      cancelable: true,
      metaKey: false,
      clientX: clientX,
      clientY: clientY,
      buttons: 1,
    }),
  )
}

describe('Select Mode Selection', () => {
  before(() => {
    viewport.set(2200, 1000)
  })

  it('keep double clicking on a children eventually selects it – even if it is out of bounds of the parents', async () => {
    const renderResult = await renderTestEditorWithCode(
      makeTestProjectCodeWithSnippet(`
          <div data-uid='a' style={{ ...props.style }}>
            <div
              data-uid='b'
              style={{
                backgroundColor: '#0091FFAA',
                position: 'absolute',
                overflow: 'visible',
                left: 50,
                top: 50,
                height: 120,
                width: 120,
              }}
            >
              <div
              data-uid='c'
                style={{
                  backgroundColor: '#0091FFAA',
                  position: 'absolute',
                  left: 30,
                  top: 30,
                  height: 120,
                  width: 120,
                }}
              >
                <div
                  data-uid='d'
                  style={{
                    backgroundColor: '#0091FFAA',
                    position: 'absolute',
                    height: 120,
                    width: 120,
                    left: 30,
                    top: 30,
                  }}
                >
                  <div
                    data-uid='e'
                    style={{
                      backgroundColor: '#0091FFAA',
                      position: 'absolute',
                      left: 30,
                      top: 30,
                      height: 120,
                      width: 120,
                    }}
                  >
                    <div
                      data-uid='targetdiv'
                      data-testid='targetdiv'
                      style={{
                        backgroundColor: '#0091FFAA',
                        position: 'absolute',
                        left: 30,
                        top: 30,
                        height: 120,
                        width: 120,
                      }}
                    />
                  </div>
                </div>
              </div>
            </div>
          </div>
      `),
      'await-first-dom-report',
    )

    const areaControl = renderResult.renderedDOM.getByTestId('targetdiv')
    const areaControlBounds = areaControl.getBoundingClientRect()

    const canvasControlsLayer = renderResult.renderedDOM.getByTestId(CanvasControlsContainerID)

    const doubleClick = async () => {
      await act(async () => {
        const dispatchDone = renderResult.getDispatchFollowUpActionsFinished()
        fireDoubleClickEvents(
          canvasControlsLayer,
          areaControlBounds.left + 20,
          areaControlBounds.top + 20,
        )

        await dispatchDone
      })
    }

    await doubleClick()

    const selectedViews2 = renderResult.getEditorState().editor.selectedViews
    expect(selectedViews2).toEqual([
      EP.elementPath([[BakedInStoryboardUID, TestSceneUID, TestAppUID]]),
    ])

    await doubleClick()

    const selectedViews3 = renderResult.getEditorState().editor.selectedViews
    expect(selectedViews3).toEqual([EP.appendNewElementPath(TestScenePath, ['a'])])

    await doubleClick()

    const selectedViews4 = renderResult.getEditorState().editor.selectedViews
    expect(selectedViews4).toEqual([EP.appendNewElementPath(TestScenePath, ['a', 'b'])])

    await doubleClick()

    const selectedViews5 = renderResult.getEditorState().editor.selectedViews
    expect(selectedViews5).toEqual([EP.appendNewElementPath(TestScenePath, ['a', 'b', 'c'])])

    await doubleClick()

    const selectedViews6 = renderResult.getEditorState().editor.selectedViews
    expect(selectedViews6).toEqual([EP.appendNewElementPath(TestScenePath, ['a', 'b', 'c', 'd'])])

    await doubleClick()

    const selectedViews7 = renderResult.getEditorState().editor.selectedViews
    expect(selectedViews7).toEqual([
      EP.appendNewElementPath(TestScenePath, ['a', 'b', 'c', 'd', 'e']),
    ])

    await doubleClick()

    // after 8 "double clicks", the `targetdiv` div should be selected
    const selectedViews8 = renderResult.getEditorState().editor.selectedViews
    expect(selectedViews8).toEqual([
      EP.appendNewElementPath(TestScenePath, ['a', 'b', 'c', 'd', 'e', 'targetdiv']),
    ])
  })

  it('Clicking a scene label selects the scene', async () => {
    const renderResult = await renderTestEditorWithCode(
      `
        import * as React from 'react'
        import {
          Scene,
          Storyboard,
        } from 'utopia-api'
        export var App = (props) => {
          return (
            <div />
          )
        }
        export var storyboard = (props) => {
          return (
            <Storyboard data-uid={'${BakedInStoryboardUID}'}>
              <Scene
                style={{ position: 'relative', left: 0, top: 0, width: 375, height: 812 }}
                data-uid={'${TestSceneUID}'}
              >
                <App
                  data-uid='${TestAppUID}' 
                  style={{ position: 'absolute', bottom: 0, left: 0, right: 0, top: 0 }}
                />
              </Scene>
            </Storyboard>
          )
        }
      `,
      'await-first-dom-report',
    )

    const sceneLabel = renderResult.renderedDOM.getByTestId(SceneLabelTestID)
    const sceneLabelBounds = sceneLabel.getBoundingClientRect()

    await act(async () => {
      const dispatchDone = renderResult.getDispatchFollowUpActionsFinished()
      fireSingleClickEvents(sceneLabel, sceneLabelBounds.left + 5, sceneLabelBounds.top + 5)
      await dispatchDone
    })
    await waitForAnimationFrame()

    expect(renderResult.getEditorState().editor.selectedViews).toEqual([
      EP.fromString(`${BakedInStoryboardUID}/${TestSceneUID}`),
    ])
  })
  it('Doubleclick on a child element selects it, with custom cursors set', async () => {
    const targetElementUid = 'aaa'
    const renderResult = await renderTestEditorWithCode(
      makeTestProjectCodeWithSnippet(`
        <div data-uid='aaa' data-testid='${targetElementUid}' style={{ position: 'absolute', width: 200, height: 200 }}>
          <div data-uid='bbb' data-testid='bbb' style={{ width: 100, height: 100 }}></div>
        </div>
      `),
      'await-first-dom-report',
    )

    const appElementPath = EP.elementPath([[BakedInStoryboardUID, TestSceneUID, TestAppUID]])
    await renderResult.dispatch(
      [setCursorOverlay(CSSCursor.Move), selectComponents([appElementPath], false)],
      true,
    )

    const areaControl = renderResult.renderedDOM.getByTestId(targetElementUid)
    const areaControlBounds = areaControl.getBoundingClientRect()

    const canvasControlsLayer = renderResult.renderedDOM.getByTestId(CanvasControlsContainerID)

    await act(async () => {
      const dispatchDone = renderResult.getDispatchFollowUpActionsFinished()
      fireDoubleClickEvents(
        canvasControlsLayer,
        areaControlBounds.left + 20,
        areaControlBounds.top + 20,
      )

      await dispatchDone
    })
    await waitForAnimationFrame()

    const selectedViews = renderResult.getEditorState().editor.selectedViews
    expect(selectedViews).toEqual([EP.appendNewElementPath(appElementPath, [targetElementUid])])
  })
})

describe('Select Mode Advanced Cases', () => {
  before(() => {
    viewport.set(2200, 1000)
  })

  it('Can cmd-click to select Button on a Card Scene Root', async () => {
    const renderResult = await renderTestEditorWithCode(
      TestProjectAlpineClimb,
      'await-first-dom-report',
    )

    const cardSceneRoot = renderResult.renderedDOM.getByTestId('card-scene')
    const cardSceneRootBounds = cardSceneRoot.getBoundingClientRect()

    const canvasControlsLayer = renderResult.renderedDOM.getByTestId(CanvasControlsContainerID)

    await act(async () => {
      const dispatchDone = renderResult.getDispatchFollowUpActionsFinished()
      fireEvent(
        canvasControlsLayer,
        new MouseEvent('mousedown', {
          detail: 1,
          bubbles: true,
          cancelable: true,
          metaKey: true,
          clientX: cardSceneRootBounds.left + 130,
          clientY: cardSceneRootBounds.top + 220,
          buttons: 1,
        }),
      )
      await dispatchDone
    })
    await waitForAnimationFrame()

    expect(renderResult.getEditorState().editor.selectedViews).toEqual([
      EP.fromString('sb/scene-2/Card-instance:Card-Root/Card-Row-Buttons/Card-Button-3'),
    ])
  })

  it('Five double clicks to select Button on a Card Scene Root', async () => {
    const renderResult = await renderTestEditorWithCode(
      TestProjectAlpineClimb,
      'await-first-dom-report',
    )

    const cardSceneRoot = renderResult.renderedDOM.getByTestId('card-scene')
    const cardSceneRootBounds = cardSceneRoot.getBoundingClientRect()

    const canvasControlsLayer = renderResult.renderedDOM.getByTestId(CanvasControlsContainerID)

    const doubleClick = async () => {
      await act(async () => {
        const dispatchDone = renderResult.getDispatchFollowUpActionsFinished()
        fireDoubleClickEvents(
          canvasControlsLayer,
          cardSceneRootBounds.left + 130,
          cardSceneRootBounds.top + 220,
        )
        await dispatchDone
      })
      await waitForAnimationFrame()
    }

    await doubleClick()
    await doubleClick()
    await doubleClick()
    await doubleClick()
    await doubleClick()

    expect(renderResult.getEditorState().editor.selectedViews).toEqual([
      EP.fromString('sb/scene-2/Card-instance:Card-Root/Card-Row-Buttons/Card-Button-3'),
    ])
  })

  it('Keep double clicking to select Button inside a focused generated Card', async () => {
    const renderResult = await renderTestEditorWithCode(
      TestProjectAlpineClimb,
      'await-first-dom-report',
    )

    const canvasControlsLayer = renderResult.renderedDOM.getByTestId(CanvasControlsContainerID)

    await act(async () => {
      await renderResult.dispatch(
        [
          setFocusedElement(
            EP.elementPath([
              ['sb', 'scene-CardList', 'CardList-instance'],
              ['CardList-Root', 'CardList-Col', 'CardList-Card~~~1'],
            ]),
          ),
          CanvasActions.scrollCanvas(canvasPoint({ x: 430, y: 390 })),
        ],
        true,
      )
    })

    const cardSceneRoot = renderResult.renderedDOM.getByTestId('generated-card-1')
    const cardSceneRootBounds = cardSceneRoot.getBoundingClientRect()

    const doubleClick = async () => {
      await act(async () => {
        const dispatchDone = renderResult.getDispatchFollowUpActionsFinished()
        fireDoubleClickEvents(
          canvasControlsLayer,
          cardSceneRootBounds.left + 130,
          cardSceneRootBounds.top + 220,
        )
        await dispatchDone
      })
      await waitForAnimationFrame()
    }

    await doubleClick()
    await doubleClick()
    await doubleClick()
    await doubleClick()
    await doubleClick()
    await doubleClick()
    await doubleClick()
    await doubleClick()

    expect(renderResult.getEditorState().editor.selectedViews).toEqual([
      EP.fromString(
        'sb/scene-CardList/CardList-instance:CardList-Root/CardList-Col/CardList-Card~~~1:Card-Root/Card-Row-Buttons/Card-Button-3',
      ),
    ])
  })
})
function waitForAnimationFrame(): Promise<void> {
  return new Promise<void>((resolve, reject) => {
    requestAnimationFrame(() => {
      requestAnimationFrame(() => {
        requestAnimationFrame(() => {
          resolve()
        })
      })
    })
  })
}

const TestProjectAlpineClimb = `
import * as React from "react";
import { Scene, Storyboard } from "utopia-api";
import styled from "@emotion/styled";

export const Col = (props) => (
  <div
    style={{
      display: "flex",
      flexDirection: "column",
      gap: 34,
      padding: 12,
      alignItems: "stretch",
      backgroundColor: "yellow",
    }}
    data-uid="Col-Root"
  >
    {props.children}
  </div>
);

export const LabelRow = (props) => (
  <div
    style={{
      color: "white",
      display: "flex",
      alignItems: "center",
      fontFamily: "sans-serif",
      backdropFilter: "blur(6px) brightness(120%)",
      paddingLeft: 12,
      paddingRight: 12,
      position: "absolute",
      bottom: 0,
      left: 0,
      height: 34,
      right: 0,
    }}
    data-uid="LabelRow-Root"
  >
    <div style={{ flex: "1 0 150px" }} data-uid="LabelRow-div">
      Beautiful Hackney Street Arrrrt
    </div>
    <Button data-uid="LabelRow-Button">Hello</Button>
  </div>
);

export const Button = styled.button({
  minWidth: 40,
  minHeight: 22,
  boxShadow: "1px 1px 0px 1px black",
  backgroundColor: "#00FFAA",
  color: "black",
});

export const CardList = (props) => {
  cards = [1, 2, 3, 4, 5];

  return (
    <div data-uid="CardList-Root">
      <h2 data-uid="CardList-h2">List of available street art</h2>
      <Col data-uid="CardList-Col">
        {cards.map((card) => (
          <Card data-uid="CardList-Card" testid={'generated-card-'+ card} />
        ))}
      </Col>
    </div>
  );
};

export const ManualCardList = (props) => {
  return (
    <div data-uid="ManualCardList-root">
      <h2 data-uid="ManualCardList-h2">List of available street art</h2>
      <Col data-uid="ManualCardList-Col">
        <Card data-uid="ManualCardList-Card-1" />
        <Card data-uid="ManualCardList-Card-2" />
      </Col>
    </div>
  );
};

export const Card = (props) => (
  <div
    style={{
      display: "flex",
      flexDirection: "column",
      width: 364,
      height: 250,
      backgroundColor: "hsl(0,0%,95%)",
      boxShadow: "0px 0px 0px 1px black",
    }}
    data-testid={props.testid}
    data-uid="Card-Root"
  >
    <Row
      style={{ minHeight: 200, position: "relative", overflow: "hidden" }}
      data-uid="Card-Row"
    >
      <img
        src="https://www.hackneycitizen.co.uk/wp-content/uploads/nerone-1-620.jpg"
        data-uid="Card-img"
      />
      <LabelRow data-uid="Card-LabelRow" />
    </Row>
    <Row style={{ minHeight: 40, gap: 12 }} data-uid="Card-Row-Buttons">
      <Button data-uid="Card-Button-1">Hello</Button>
      <Button data-uid="Card-Button-2">Button</Button>
      <Button data-uid="Card-Button-3">Button</Button>
    </Row>
  </div>
);

export const FlexRow = styled.div({
  display: "flex",
  alignItems: "center",
});

export const Row = styled(FlexRow)({});
export const UIRow = styled.div({
  minHeight: 34,
  paddingLeft: 8,
  paddingRight: 8,
  display: "flex",
  alignItems: "center",
});

export const UIListRow = styled(UIRow)({
  height: 27,
  minHeight: "initial",
  "&:hover": {
    color: "white",
    background: "#007AFF",
  },
});
export const UIListGridRow = styled(UIRow)({
  height: 27,
  minHeight: "initial",
  display: "grid",
  gridTemplateColumns: "28px 1fr",
  minHeight: "initial",
  "&:hover": {
    color: "white",
    background: "#007AFF",
  },
});

export const UIContextMenuRow = styled(UIRow)({
  height: 22,
  borderRadius: 2,
  minHeight: "initial",
  display: "grid",
  gridTemplateColumns: "1fr auto",
  minHeight: "initial",
  "&:hover": {
    color: "white",
    background: "#007AFF",
  },
});

export const ContextMenu = (props) => {
  return (
    <div
      style={{
        borderRadius: 4,
        padding: 4,
        backgroundColor: "hsl(0,0%,95%)",
        paddingTop: 4,
        paddingBottom: 4,
        width: 202,
        fontFamily: "Inter",
        fontSize: 11,
        fontWeight: 400,
        boxShadow:
          "0px 2px 7px rgb(0, 0, 0, 0.12), 0px 0px 0px 1px rgb(0, 0, 0, 0.12)",
      }}
      data-uid="ContextMenu-Root"
    >
      <UIContextMenuRow data-uid="ContextMenu-Copy">
        <span data-uid="918">Copy</span>
        <span style={{ color: "hsl(0,0%,60%)" }} data-uid="f49">
          ⌘+C
        </span>
      </UIContextMenuRow>
      <UIContextMenuRow data-uid="ContextMenu-Cut">
        <span data-uid="a3e">Cut</span>
        <span style={{ color: "hsl(0,0%,60%)" }} data-uid="b34">
          ⌘+X
        </span>
      </UIContextMenuRow>
      <UIContextMenuRow data-uid="ContextMenu-Paste">
        <span data-uid="09d">Paste</span>
        <span style={{ color: "hsl(0,0%,60%)" }} data-uid="706">
          ⌘+V
        </span>
      </UIContextMenuRow>
      <UIContextMenuRow data-uid="ContextMenu-">
        <span data-uid="821">Paste</span>
        <span style={{ color: "hsl(0,0%,60%)" }} data-uid="46d">
          ⌘+V
        </span>
      </UIContextMenuRow>
      <hr
        style={{
          color: "yello",
          background: "purple",
          stroke: "grey",
          backgroundColor: "/*rgb(128, 0, 128, 1)*/",
          border: "0.5px solid rgb(255, 20, 20, 1)",
          height: 1,
        }}
        data-uid="ContextMenu-hr"
      />
      <UIContextMenuRow data-uid="ContextMenu-Backward">
        <span data-uid="ab9">Bring Backward</span>
        <span style={{ color: "hsl(0,0%,60%)" }} data-uid="c2b">
          ⌘+V
        </span>
      </UIContextMenuRow>
      <UIContextMenuRow data-uid="ContextMenu-Forward">
        <span data-uid="cb2">Bring Forward</span>
        <span style={{ color: "hsl(0,0%,60%)" }} data-uid="07c">
          ⌘+V
        </span>
      </UIContextMenuRow>
    </div>
  );
};

export var App = (props) => {
  return (
    <div
      style={{
        padding: 20,
        width: "100%",
        height: "100%",
        backgroundColor: "#FFFFFF",
        position: "relative",
        fontFamily: "Inter",
        fontSize: 11,
        fontWeight: 400,
        WebkitTextRendering: "subpixel-antialiased",
      }}
      data-uid="App-root"
    >
      <div
        style={{
          width: 270,
          height: 215,
          background: "hsl(0,0%,97%)",
          backgroundColor: "rgb(247, 247, 247, 1)",
          boxShadow:
            "0px 2px 7px rgb(0, 0, 0, 0.12), 0px 0px 0px 1px rgb(0, 0, 0, 0.12)",
          borderRadius: 3,
        }}
        data-uid="App-div"
      >
        <div
          style={{
            paddingLeft: 8,
            paddingRight: 8,
            fontFamily: "Inter",
            fontSize: 11,
            color: "hsl(0,0%,10%)",
            display: "flex",
            alignItems: "center",
            height: 34,
          }}
          data-uid="App-div-div"
        >
          <span style={{ fontWeight: 600 }} data-uid="App-div-div-span">
            Popup Menu
          </span>
        </div>
        <div
          style={{
            display: "flex",
            flexDirection: "column",
            paddingLeft: 8,
            paddingRight: 8,
          }}
          data-uid="57e"
        >
          <UIListGridRow
            style={{ gap: 8, borderRadius: 2, flexGrow: 1 }}
            data-uid="App-Select"
          >
            <Row data-uid="681" />
            <Row data-uid="04c">Select Elements</Row>
          </UIListGridRow>
          <UIListGridRow
            style={{ gap: 8, borderRadius: 2, flexGrow: 1 }}
            data-uid="App-Copy"
          >
            <div data-uid="2a2" />
            <div data-uid="329">Copy</div>
          </UIListGridRow>
          <UIListGridRow
            style={{ gap: 8, borderRadius: 2, flexGrow: 1 }}
            data-uid="App-Pase"
          >
            <div data-uid="2ce" />
            <div data-uid="1a3">Paste</div>
          </UIListGridRow>
          <UIListGridRow data-uid="App-Check">
            <Row style={{ justifyContent: "center" }} data-uid="a1d">
              ✓
            </Row>
            <Row data-uid="2d9">A little label</Row>
          </UIListGridRow>
          <UIListGridRow
            style={{ gap: 8, borderRadius: 2, flexGrow: 1 }}
            data-uid="App-Magic"
          >
            <div data-uid="765" />
            <div data-uid="a20">Magic</div>
          </UIListGridRow>
        </div>
      </div>
    </div>
  );
};
export var storyboard = (
  <Storyboard data-uid="sb" >
    <Scene
      style={{ position: "absolute", left: 0, top: 0, width: 313, height: 261 }}
      data-uid="scene-App"
    >
      <App data-uid="App-instance" />
    </Scene>
    <Scene
      data-label="Scene 1"
      style={{
        position: "absolute",
        padding: 20,
        left: 0,
        top: 299,
        width: 280,
        height: 196,
      }}
      data-uid="scene-1"
    >
      <ContextMenu data-uid="ContextMenu-instance" />
    </Scene>
    <Scene
      data-label="Scene 2"
      style={{
        position: "absolute",
        padding: 20,
        left: 420,
        top: -19,
        width: 400,
        height: 300,
      }}
      data-uid="scene-2"
    >
      <Card data-uid="Card-instance" testid="card-scene" />
    </Scene>
    <Scene
      data-label="List of Cards"
      style={{
        position: "absolute",
        padding: 20,
        left: 420,
        top: 350,
        width: 500,
        height: 400,
      }}
      data-uid="scene-CardList"
    >
      <CardList data-uid="CardList-instance" />
    </Scene>
    <Scene
      data-label="Card component out of place for focus mode"
      style={{
        position: "absolute",
        padding: 20,
        left: 1060,
        top: -31,
        width: 500,
        height: 400,
      }}
      data-uid="scene-ManualCardList"
    >
      <ManualCardList data-uid="ManualCardList-uid" />
    </Scene>
  </Storyboard>
);
`
