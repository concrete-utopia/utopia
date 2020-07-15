import { Editor } from './editor'
import { URL_HASH } from '../common/env-vars'

const editorCSS = [
  '/editor/editor.css',
  '/editor/cursors.css',
  '/editor/css/utopions.css',
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
  'https://fonts.googleapis.com/css?family=Arvo:400,400i,700,700i|EB+Garamond:400,400i,500,500i,600,600i,700,700i,800,800i|Lora:400,400i,700,700i|Noto+Sans+JP:100,300,400,500,700,900|Noto+Sans+KR:100,300,400,500,700,900|Noto+Sans+SC:100,300,400,500,700,900|Noto+Sans+TC:100,300,400,500,700,900|Noto+Sans:400,400i,700,700i|Noto+Serif+JP:200,300,400,500,600,700,900|Noto+Serif+KR:200,300,400,500,600,700,900|Noto+Serif+SC:200,300,400,500,600,700,900|Noto+Serif+TC:200,300,400,500,600,700,900|Noto+Serif:400,400i,700,700i|Open+Sans:300,300i,400,400i,600,600i,700,700i,800,800i|PT+Sans+Narrow:400,700|PT+Sans:400,400i,700,700i|PT+Serif:400,400i,700,700i|Playfair+Display:400,400i,700,700i,900,900i|Roboto+Condensed:300,300i,400,400i,700,700i|Roboto+Mono:100,100i,300,300i,400,400i,500,500i,700,700i|Roboto+Slab:100,300,400,700|Roboto:100,100i,300,300i,400,400i,500,500i,700,700i,900,900i|Sorts+Mill+Goudy:400,400i|Source+Code+Pro:200,300,400,500,600,700,900|Source+Sans+Pro:200,200i,300,300i,400,400i,600,600i,700,700i,900,900i|Source+Serif+Pro:400,600,700|ZCOOL+QingKe+HuangYou',
]

export function addStyleSheetToPage(url: string, shouldAppendHash: boolean = true) {
  const cssElement = document.createElement('link')
  cssElement.rel = 'stylesheet'
  cssElement.type = 'text/css'
  cssElement.href = appendHash(url)
  document.getElementsByTagName('head')[0].appendChild(cssElement)
}

export function appendHash(url: string): string {
  return `${url}?hash=${URL_HASH}`
}

editorCSS.forEach((url) => addStyleSheetToPage(url))
editorCSSDontHash.forEach((url) => addStyleSheetToPage(url, false))

// eslint-disable-next-line
const EditorRunner = new Editor()
