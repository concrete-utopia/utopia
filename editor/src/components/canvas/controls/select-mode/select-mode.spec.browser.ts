import { act, fireEvent } from '@testing-library/react'
import * as TP from '../../../../core/shared/template-path'
import { setElectronWindow } from '../../../../core/shared/test-setup.test-utils'
import { wait } from '../../../../utils/utils.test-utils'
import { makeTestProjectCodeWithSnippet, renderTestEditorWithCode } from '../../ui-jsx.test-utils'
import { CanvasControlsContainerID } from '../new-canvas-controls'

describe('Select Mode Selection', () => {
  beforeAll(setElectronWindow)

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
    )

    const areaControl = renderResult.renderedDOM.getByTestId('targetdiv')
    const areaControlBounds = areaControl.getBoundingClientRect()

    const canvasControlsLayer = renderResult.renderedDOM.getByTestId(CanvasControlsContainerID)

    await act(async () => {
      const domFinished = renderResult.getDomReportDispatched()
      const dispatchDone = renderResult.getDispatchFollowUpactionsFinished()
      fireEvent(
        canvasControlsLayer,
        new MouseEvent('mousedown', {
          detail: 1,
          bubbles: true,
          cancelable: true,
          metaKey: false,
          clientX: areaControlBounds.left + 20,
          clientY: areaControlBounds.top + 20,
          buttons: 1,
        }),
      )
      fireEvent(
        canvasControlsLayer,
        new MouseEvent('mousedown', {
          detail: 2,
          bubbles: true,
          cancelable: true,
          metaKey: false,
          clientX: areaControlBounds.left + 20,
          clientY: areaControlBounds.top + 20,
          buttons: 1,
        }),
      )
      await domFinished
      await dispatchDone
    })
    await waitForAnimationFrame()

    const selectedViews1 = renderResult.getEditorState().editor.selectedViews
    expect(selectedViews1).toEqual([
      TP.instancePath(TP.scenePath([['utopia-storyboard-uid', 'scene-aaa']]), ['a']),
    ])

    await act(async () => {
      const domFinished = renderResult.getDomReportDispatched()
      const dispatchDone = renderResult.getDispatchFollowUpactionsFinished()
      fireEvent(
        canvasControlsLayer,
        new MouseEvent('mousedown', {
          detail: 1,
          bubbles: true,
          cancelable: true,
          metaKey: false,
          clientX: areaControlBounds.left + 20,
          clientY: areaControlBounds.top + 20,
          buttons: 1,
        }),
      )
      fireEvent(
        canvasControlsLayer,
        new MouseEvent('mousedown', {
          detail: 2,
          bubbles: true,
          cancelable: true,
          metaKey: false,
          clientX: areaControlBounds.left + 20,
          clientY: areaControlBounds.top + 20,
          buttons: 1,
        }),
      )
      await domFinished
      await dispatchDone
    })
    await waitForAnimationFrame()

    const selectedViews2 = renderResult.getEditorState().editor.selectedViews
    expect(selectedViews2).toEqual([
      TP.instancePath(TP.scenePath([['utopia-storyboard-uid', 'scene-aaa']]), ['a', 'b']),
    ])

    await act(async () => {
      const domFinished = renderResult.getDomReportDispatched()
      const dispatchDone = renderResult.getDispatchFollowUpactionsFinished()
      fireEvent(
        canvasControlsLayer,
        new MouseEvent('mousedown', {
          detail: 1,
          bubbles: true,
          cancelable: true,
          metaKey: false,
          clientX: areaControlBounds.left + 20,
          clientY: areaControlBounds.top + 20,
          buttons: 1,
        }),
      )
      fireEvent(
        canvasControlsLayer,
        new MouseEvent('mousedown', {
          detail: 2,
          bubbles: true,
          cancelable: true,
          metaKey: false,
          clientX: areaControlBounds.left + 20,
          clientY: areaControlBounds.top + 20,
          buttons: 1,
        }),
      )
      await domFinished
      await dispatchDone
    })
    await waitForAnimationFrame()

    const selectedViews3 = renderResult.getEditorState().editor.selectedViews
    expect(selectedViews3).toEqual([
      TP.instancePath(TP.scenePath([['utopia-storyboard-uid', 'scene-aaa']]), ['a', 'b', 'c']),
    ])

    await act(async () => {
      const domFinished = renderResult.getDomReportDispatched()
      const dispatchDone = renderResult.getDispatchFollowUpactionsFinished()
      fireEvent(
        canvasControlsLayer,
        new MouseEvent('mousedown', {
          detail: 1,
          bubbles: true,
          cancelable: true,
          metaKey: false,
          clientX: areaControlBounds.left + 20,
          clientY: areaControlBounds.top + 20,
          buttons: 1,
        }),
      )
      fireEvent(
        canvasControlsLayer,
        new MouseEvent('mousedown', {
          detail: 2,
          bubbles: true,
          cancelable: true,
          metaKey: false,
          clientX: areaControlBounds.left + 20,
          clientY: areaControlBounds.top + 20,
          buttons: 1,
        }),
      )
      await domFinished
      await dispatchDone
    })
    await waitForAnimationFrame()

    const selectedViews4 = renderResult.getEditorState().editor.selectedViews
    expect(selectedViews4).toEqual([
      TP.instancePath(TP.scenePath([['utopia-storyboard-uid', 'scene-aaa']]), ['a', 'b', 'c', 'd']),
    ])

    await act(async () => {
      const domFinished = renderResult.getDomReportDispatched()
      const dispatchDone = renderResult.getDispatchFollowUpactionsFinished()
      fireEvent(
        canvasControlsLayer,
        new MouseEvent('mousedown', {
          detail: 1,
          bubbles: true,
          cancelable: true,
          metaKey: false,
          clientX: areaControlBounds.left + 20,
          clientY: areaControlBounds.top + 20,
          buttons: 1,
        }),
      )
      fireEvent(
        canvasControlsLayer,
        new MouseEvent('mousedown', {
          detail: 2,
          bubbles: true,
          cancelable: true,
          metaKey: false,
          clientX: areaControlBounds.left + 20,
          clientY: areaControlBounds.top + 20,
          buttons: 1,
        }),
      )
      await domFinished
      await dispatchDone
    })
    await waitForAnimationFrame()

    const selectedViews5 = renderResult.getEditorState().editor.selectedViews
    expect(selectedViews5).toEqual([
      TP.instancePath(TP.scenePath([['utopia-storyboard-uid', 'scene-aaa']]), [
        'a',
        'b',
        'c',
        'd',
        'e',
      ]),
    ])

    await act(async () => {
      const domFinished = renderResult.getDomReportDispatched()
      const dispatchDone = renderResult.getDispatchFollowUpactionsFinished()
      fireEvent(
        canvasControlsLayer,
        new MouseEvent('mousedown', {
          detail: 1,
          bubbles: true,
          cancelable: true,
          metaKey: false,
          clientX: areaControlBounds.left + 20,
          clientY: areaControlBounds.top + 20,
          buttons: 1,
        }),
      )
      fireEvent(
        canvasControlsLayer,
        new MouseEvent('mousedown', {
          detail: 2,
          bubbles: true,
          cancelable: true,
          metaKey: false,
          clientX: areaControlBounds.left + 20,
          clientY: areaControlBounds.top + 20,
          buttons: 1,
        }),
      )
      await domFinished
      await dispatchDone
    })
    await waitForAnimationFrame()

    // after 6 "double clicks", the `targetdiv` div should be selected
    const selectedViews6 = renderResult.getEditorState().editor.selectedViews
    expect(selectedViews6).toEqual([
      TP.instancePath(TP.scenePath([['utopia-storyboard-uid', 'scene-aaa']]), [
        'a',
        'b',
        'c',
        'd',
        'e',
        'targetdiv',
      ]),
    ])
  })
})

describe('Select Mode Advanced Cases', () => {
  beforeAll(setElectronWindow)

  it('Can cmd-click to select Button on a Card Scene Root', async () => {
    const renderResult = await renderTestEditorWithCode(TestProjectAlpineClimb)

    const cardSceneRoot = renderResult.renderedDOM.getByTestId('card-scene')
    const cardSceneRootBounds = cardSceneRoot.getBoundingClientRect()

    const canvasControlsLayer = renderResult.renderedDOM.getByTestId(CanvasControlsContainerID)

    await act(async () => {
      const domFinished = renderResult.getDomReportDispatched()
      const dispatchDone = renderResult.getDispatchFollowUpactionsFinished()
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
      await domFinished
      await dispatchDone
    })
    await waitForAnimationFrame()

    expect(renderResult.getEditorState().editor.selectedViews).toEqual([
      TP.fromString('sb/scene-2:Card-Root/d63/Card-Button-3'),
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
/** @jsx jsx */
import * as React from "react";
import { Scene, Storyboard, jsx } from "utopia-api";
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
          <Card data-uid="CardList-Card" />
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
    <Row style={{ minHeight: 40, gap: 12 }} data-uid="Card-Row">
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
      component={App}
      props={{}}
      style={{ position: "absolute", left: 0, top: 0, width: 313, height: 261 }}
      data-uid="scene-App"
    />
    <Scene
      data-label="Scene 1"
      component={ContextMenu}
      resizeContent
      style={{
        position: "absolute",
        padding: 20,
        left: 0,
        top: 299,
        width: 280,
        height: 196,
      }}
      data-uid="scene-1"
    />
    <Scene
      data-label="Scene 2"
      component={Card}
      resizeContent
      style={{
        position: "absolute",
        padding: 20,
        left: 420,
        top: -19,
        width: 400,
        height: 300,
      }}
      props={{
        testid: 'card-scene'
      }}
      data-uid="scene-2"
    />
    <Scene
      data-label="List of Cards"
      component={CardList}
      resizeContent
      style={{
        position: "absolute",
        padding: 20,
        left: 420,
        top: 350,
        width: 500,
        height: 400,
      }}
      data-uid="scene-CardList"
    />
    <Scene
      data-label="Card component out of place for focus mode"
      component={ManualCardList}
      resizeContent
      style={{
        position: "absolute",
        padding: 20,
        left: 1060,
        top: -31,
        width: 500,
        height: 400,
      }}
      data-uid="scene-ManualCardList"
    />
  </Storyboard>
);
`
