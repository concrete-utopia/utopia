import * as EP from '../../../../core/shared/element-path'
import { EditorRenderResult } from '../../ui-jsx.test-utils'
import { ContentAffectingType } from './group-like-helpers'

export const GroupLikeElementUid = 'children-affecting'
export const InnerFragmentId = 'inner-fragment'

export function getOpeningGroupLikeTag(type: ContentAffectingType): string {
  switch (type) {
    case 'sizeless-div':
      return `<div data-uid='${GroupLikeElementUid}' data-testid='${GroupLikeElementUid}'><React.Fragment data-uid='${InnerFragmentId}'>`
    case 'fragment':
      return `<React.Fragment data-uid='${GroupLikeElementUid}' data-testid='${GroupLikeElementUid}'><React.Fragment data-uid='${InnerFragmentId}'>`
    case 'conditional':
      return `{ true /* @utopia/uid=${GroupLikeElementUid} */ ? ( <React.Fragment data-uid='${InnerFragmentId}'>`
    default:
      const _exhaustiveCheck: never = type
      throw new Error(`Unhandled ContentAffectingType ${JSON.stringify(type)}.`)
  }
}

export function getClosingGroupLikeTag(type: ContentAffectingType): string {
  switch (type) {
    case 'sizeless-div':
      return `</React.Fragment></div>`
    case 'fragment':
      return `</React.Fragment></React.Fragment>`
    case 'conditional':
      return `</React.Fragment> ) : null }`
    default:
      const _exhaustiveCheck: never = type
      throw new Error(`Unhandled ContentAffectingType ${JSON.stringify(type)}.`)
  }
}

export function getRegularNavigatorTargets(renderResult: EditorRenderResult): Array<string> {
  return renderResult
    .getEditorState()
    .derived.navigatorTargets.filter((t) => t.type === 'REGULAR')
    .map((t) => t.elementPath)
    .map(EP.toString)
}
