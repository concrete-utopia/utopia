import { within } from '@testing-library/react'
import userEvent from '@testing-library/user-event'
import { ItemContainerTestId } from '../../uuiui/radix-components'
import type { EditorRenderResult } from '../canvas/ui-jsx.test-utils'
import {
  AddRemoveLayoutSystemControlTestId,
  AddFlexLayoutOptionId,
  AddGridLayoutOptionId,
  RemoveLayoutSystemOptionId,
} from './add-remove-layout-system-control'

async function clickLayoutDropdownOption(editor: EditorRenderResult, option: string) {
  const flexDirectionToggle = editor.renderedDOM.getAllByTestId(
    AddRemoveLayoutSystemControlTestId(),
  )[0]
  await userEvent.click(within(flexDirectionToggle).getByRole('button'))
  const flexOption = editor.renderedDOM.getByTestId(ItemContainerTestId(option))
  await userEvent.click(flexOption)
}

export async function addFlexLayout(editor: EditorRenderResult) {
  await clickLayoutDropdownOption(editor, AddFlexLayoutOptionId)
}

export async function addGridLayout(editor: EditorRenderResult) {
  await clickLayoutDropdownOption(editor, AddGridLayoutOptionId)
}

export async function removeLayout(editor: EditorRenderResult) {
  await clickLayoutDropdownOption(editor, RemoveLayoutSystemOptionId)
}
