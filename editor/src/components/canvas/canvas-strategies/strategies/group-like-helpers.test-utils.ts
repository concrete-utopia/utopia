import * as EP from '../../../../core/shared/element-path'
import { EditorRenderResult } from '../../ui-jsx.test-utils'
import { ContentAffectingType } from './group-like-helpers'

export const GroupLikeElementUid = 'children-affecting'
export const InnerFragmentId = 'inner-fragment'

interface GrouplikeTagOptions {
  stripTestId: boolean
  stripUids: boolean
}

const DefaultGrouplikeTagOptions: GrouplikeTagOptions = {
  stripTestId: false,
  stripUids: false,
}

export function getOpeningGroupLikeTag(
  type: ContentAffectingType,
  options: Partial<GrouplikeTagOptions> = DefaultGrouplikeTagOptions,
): string {
  const outerDataUid = options.stripUids === true ? '' : `data-uid='${GroupLikeElementUid}'`
  const outerTestId = options.stripTestId === true ? '' : `data-testid='${GroupLikeElementUid}'`
  const innerDataUid = options.stripUids === true ? '' : `data-uid='${InnerFragmentId}'`

  switch (type) {
    case 'sizeless-div':
      return `<div ${outerDataUid} ${outerTestId}><React.Fragment ${innerDataUid}>`
    case 'fragment':
      return `<React.Fragment ${outerDataUid} ${outerTestId}><React.Fragment ${innerDataUid}>`
    case 'conditional':
      return `{ true /* @utopia/uid=${GroupLikeElementUid} */ ? ( <React.Fragment ${innerDataUid}>`
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
