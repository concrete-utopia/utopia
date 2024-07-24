import { within } from '@testing-library/react'
import userEvent from '@testing-library/user-event'
import { ItemContainerTestId } from '../../uuiui/radix-components'
import { mouseClickAtPoint } from '../canvas/event-helpers.test-utils'
import type { EditorRenderResult } from '../canvas/ui-jsx.test-utils'
import {
  AddRemoveLayoutSystemControlTestId,
  AddFlexLayoutOptionId,
} from './add-remove-layout-system-control'

export async function addFlexLayout(editor: EditorRenderResult) {
  const flexDirectionToggle = editor.renderedDOM.getAllByTestId(
    AddRemoveLayoutSystemControlTestId(),
  )[0]
  await userEvent.click(within(flexDirectionToggle).getByRole('button'))
  const flexOption = editor.renderedDOM.getByTestId(ItemContainerTestId(AddFlexLayoutOptionId))
  await userEvent.click(flexOption)
}

export async function removeFlexLayout(editor: EditorRenderResult) {
  const flexDirectionToggle = editor.renderedDOM.getAllByTestId(
    AddRemoveLayoutSystemControlTestId(),
  )[0]
  await mouseClickAtPoint(flexDirectionToggle, { x: 2, y: 2 })
}
