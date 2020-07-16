import { PRODUCTION_CONFIG } from '../core/shared/detect-env'
// Fire off server requests that later block, to improve initial load on slower connections. These will still block,
// but this gives us a chance to cache the result first
import { getLoginState } from '../common/server'
import { triggerHashedAssetsUpdate } from '../utils/hashed-assets'

getLoginState(PRODUCTION_CONFIG)
triggerHashedAssetsUpdate()

import { addStyleSheetToPage } from '../core/shared/dom-utils'

const editorCSS = [
  '/editor/editor.css',
  '/editor/cursors.css',
  '/editor/ReactContexify.css',
  '/editor/css/topmenu.css',
  '/editor/canvas.css',
  '/editor/spreadsheet-grid.css',
  '/editor/node-graph.css',
  '/editor/inspector.css',
  '/editor/show-hint.css',
  '/editor/css/react-spinner.css',
  '/editor/css/loadscreen.css',
  '/editor/css/light/monaco-overrides.css',
  '/editor/css/codicons.css',
  '/editor/css/control-iconcheckbox.css',
  '/editor/css/control-button.css',
  '/editor/css/control-slider.css',
  '/editor/css/control-toggle.css',
  '/editor/slider.css',
  '/editor/draft.css',
  '/editor/utopia-light.css',
  '/editor/preview.css',
]

const editorCSSDontHash = [
  'https://fonts.googleapis.com/css?family=Inter&display=swap',
  'https://fonts.googleapis.com/css2?family=Inconsolata&display=swap',
]

// Queue up further CSS downloads before going any further
editorCSS.forEach((url) => addStyleSheetToPage(url))
editorCSSDontHash.forEach((url) => addStyleSheetToPage(url, false))

import { Editor } from './editor'

// eslint-disable-next-line
const EditorRunner = new Editor()
