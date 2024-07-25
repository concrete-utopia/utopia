import { within } from '@testing-library/react'
import userEvent from '@testing-library/user-event'
import { mouseClickAtPoint } from '../canvas/event-helpers.test-utils'
import type { EditorRenderResult } from '../canvas/ui-jsx.test-utils'
import {
  AddRemoveLayoutSystemControlTestId,
  AddFlexLayoutOptionId,
  AddGridLayoutOptionId,
} from './add-remove-layout-system-control'

async function openLayoutDropdown(editor: EditorRenderResult) {
  const flexDirectionToggle = editor.renderedDOM.getAllByTestId(
    AddRemoveLayoutSystemControlTestId(),
  )[0]
  await userEvent.click(within(flexDirectionToggle).getByRole('button'))
}

export async function addFlexLayout(editor: EditorRenderResult) {
  await openLayoutDropdown(editor)
  const flexOption = editor.renderedDOM.getByTestId(AddFlexLayoutOptionId)
  await userEvent.click(flexOption)
}

export async function addGridLayout(editor: EditorRenderResult) {
  await openLayoutDropdown(editor)
  const gridOption = editor.renderedDOM.getByTestId(AddGridLayoutOptionId)
  await userEvent.click(gridOption)
}

export async function removeLayout(editor: EditorRenderResult) {
  const flexDirectionToggle = editor.renderedDOM.getAllByTestId(
    AddRemoveLayoutSystemControlTestId(),
  )[0]
  await mouseClickAtPoint(flexDirectionToggle, { x: 2, y: 2 })
}
