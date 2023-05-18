import { EditorRenderResult } from '../../canvas/ui-jsx.test-utils'
import { EditorState } from '../store/editor-state'

export interface StaticReparentTestCase {
  setup: () => Promise<EditorState>
  trigger: () => Promise<void>
  check: (renderResult: EditorRenderResult) => Promise<void>
}
