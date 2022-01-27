import { createBuiltInDependenciesList } from '../../../core/es-modules/package-manager/built-in-dependencies-list'
import * as EP from '../../../core/shared/element-path'
import * as PP from '../../../core/shared/property-path'
import { complexDefaultProjectPreParsed } from '../../../sample-projects/sample-project-utils.test-utils'
import { setFocusedElement } from '../../editor/actions/action-creators'
import { emptySelectModeCanvasSessionState } from '../canvas-strategies/canvas-strategy-types'
import { renderTestEditorWithModel } from '../ui-jsx.test-utils'
import { moveElement, runMoveElementCommand } from './commands'
import Utils from '../../../utils/utils'
import {
  getElementsByUIDFromTopLevelElements,
  partOfJsxAttributeValue,
} from '../../../core/shared/element-template'
import { forceNotNull } from '../../../core/shared/optional-utils'
import { getJSXAttributeAtPath } from '../../../core/shared/jsx-attributes'

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
    const moveCommand = moveElement('permanent', innerRectanglePath, 200, 120)

    const sessionState = {
      ...emptySelectModeCanvasSessionState,
      dragDeltaMinimumPassed: true,
    }

    const result = runMoveElementCommand(
      renderResult.getEditorState().editor,
      sessionState,
      [],
      moveCommand,
    )
    const topLevelElements = Utils.path(
      [
        'projectContents',
        'src',
        'children',
        'card.js',
        'content',
        'fileContents',
        'parsed',
        'topLevelElements',
        '$set',
      ],
      result.editorStatePatch,
    )

    const elements = getElementsByUIDFromTopLevelElements(topLevelElements as any)
    const rectangle = forceNotNull('Could not find rectangle.', elements['card-inner-rectangle'])

    const top = getJSXAttributeAtPath(rectangle.props, PP.create(['style', 'top'])).attribute
    expect(top).toEqual(partOfJsxAttributeValue(320))
    const left = getJSXAttributeAtPath(rectangle.props, PP.create(['style', 'left'])).attribute
    expect(left).toEqual(partOfJsxAttributeValue(300))
  })
})
