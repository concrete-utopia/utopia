import { formatTestProjectCode, renderTestEditorWithCode } from '../canvas/ui-jsx.test-utils'
import {
  resizeInterfaceDesignerCodePane,
  setPanelVisibility,
} from '../editor/actions/action-creators'

import { MIN_CODE_PANE_REOPEN_WIDTH } from '../editor/actions/actions'

describe('Code editor', () => {
  describe('reopening size', () => {
    describe('width = 0', () => {
      it('reopens with min size', async () => {
        const result = await resizeAndReopen(-9999999)
        expect(result.widthAfterResize).toEqual(0)
        expect(result.widthAfterReopen).toEqual(MIN_CODE_PANE_REOPEN_WIDTH)
      })
    })
    describe('0 < width < min', () => {
      it('reopens with min size', async () => {
        const result = await resizeAndReopen(-450)
        expect(result.widthAfterResize).toEqual(50)
        expect(result.widthAfterReopen).toEqual(MIN_CODE_PANE_REOPEN_WIDTH)
      })
    })
    describe('width > min', () => {
      it('reopens with the old size', async () => {
        const result = await resizeAndReopen(50)
        expect(result.widthAfterResize).toEqual(550)
        expect(result.widthAfterReopen).toEqual(550)
      })
    })
  })
})

interface ResizeResult {
  widthAfterResize: number
  widthAfterReopen: number
}

async function resizeAndReopen(resizeDelta: number): Promise<ResizeResult> {
  let result: ResizeResult = {
    widthAfterResize: -1,
    widthAfterReopen: -1,
  }
  const editor = await renderTestEditorWithCode(emptyProject, 'await-first-dom-report')
  await editor.dispatch([setPanelVisibility('codeEditor', true)], true)
  await editor.getDispatchFollowUpActionsFinished()

  await editor.dispatch([resizeInterfaceDesignerCodePane(resizeDelta)], true)
  await editor.getDispatchFollowUpActionsFinished()
  result.widthAfterResize = getPanelWidth()

  await editor.dispatch([setPanelVisibility('codeEditor', false)], true)
  await editor.getDispatchFollowUpActionsFinished()

  await editor.dispatch([setPanelVisibility('codeEditor', true)], true)
  await editor.getDispatchFollowUpActionsFinished()

  result.widthAfterReopen = getPanelWidth()

  return result
}

const emptyProject = formatTestProjectCode(`import * as React from 'react'
import { Storyboard } from 'utopia-api'


export var storyboard = (
  <Storyboard data-uid='sb' />
)
`)

function getPanelWidth(): number {
  const elements = document.getElementsByClassName('resizableFlexColumnCanvasCode')
  if (elements.length < 1) {
    return -1
  }
  const panel = elements[0] as HTMLElement
  return panel.getBoundingClientRect().width
}
