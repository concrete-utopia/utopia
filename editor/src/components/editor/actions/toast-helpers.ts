import { uniqBy } from '../../../core/shared/array-utils'
import { Notice } from '../../common/notice'

export function uniqToasts(notices: ReadonlyArray<Notice>): ReadonlyArray<Notice> {
  return uniqBy(notices, (l, r) => l.id === r.id)
}
