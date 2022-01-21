import { createBuiltInDependenciesList } from '../../../core/es-modules/package-manager/built-in-dependencies-list'
import * as EP from '../../../core/shared/element-path'
import * as PP from '../../../core/shared/property-path'
import {
  emptyComments,
  getElementsByUIDFromTopLevelElements,
  getJSXAttribute,
  jsxAttributeValue,
  partOfJsxAttributeValue,
} from '../../../core/shared/element-template'
import { getJSXAttributeAtPath } from '../../../core/shared/jsx-attributes'
import { forceNotNull } from '../../../core/shared/optional-utils'
import { complexDefaultProjectPreParsed } from '../../../sample-projects/sample-project-utils.test-utils'
import { selectComponents, setFocusedElement } from '../../editor/actions/action-creators'
import { emptySelectModeCanvasSessionState } from '../canvas-strategies/canvas-strategy-types'
import { renderTestEditorWithModel } from '../ui-jsx.test-utils'
import { moveElement, runMoveElementCommand } from './commands'

describe('runMoveElementCommand', () => {
  it('works for a basic pinned element', async () => {
    const renderResult = await renderTestEditorWithModel(
      complexDefaultProjectPreParsed(),
      'await-first-dom-report',
      createBuiltInDependenciesList(null),
    )

    const cardInstancePath = EP.elementPath([
      ['storyboard-entity', 'scene-1-entity', 'app-entity'],
      ['app-outer-div', 'card-instance'],
    ])
    await renderResult.dispatch([setFocusedElement(cardInstancePath)], true)

    const innerRectanglePath = EP.elementPath([
      ['storyboard-entity', 'scene-1-entity', 'app-entity'],
      ['app-outer-div', 'card-instance'],
      ['card-outer-div', 'card-inner-rectangle'],
    ])
    const moveCommand = moveElement(innerRectanglePath, 200, 120)

    const sessionState = {
      ...emptySelectModeCanvasSessionState,
      dragDeltaMinimumPassed: true,
    }

    const result = runMoveElementCommand(
      renderResult.getEditorState().editor,
      sessionState,
      moveCommand,
    )
    const file = result.transientFilesState['/src/card.js']
    const elements = getElementsByUIDFromTopLevelElements(file.topLevelElementsIncludingScenes)
    const rectangle = forceNotNull('Could not find rectangle.', elements['card-inner-rectangle'])

    const top = getJSXAttributeAtPath(rectangle.props, PP.create(['style', 'top'])).attribute
    expect(top).toEqual(partOfJsxAttributeValue(320))
    const left = getJSXAttributeAtPath(rectangle.props, PP.create(['style', 'left'])).attribute
    expect(left).toEqual(partOfJsxAttributeValue(300))
  })
})
