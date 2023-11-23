import { atom } from 'jotai'
import * as EP from '../../core/shared/element-path'
import type { ElementPath } from '../../core/shared/project-file-types'

export type CMSUpdateStatus =
  | { type: 'updating'; value: string }
  | { type: 'error'; message: string }
  | { type: 'ok' }

export const CMSOptimisticValues_GLOBAL: {
  [elementPathString: string]: string
} = {}

interface CMSUpdateState {
  [elementPathString: string]: CMSUpdateStatus
}

export const CMSUpdateStateAtom = atom<CMSUpdateState>({})

export function setCMSUpdateStateForElementPath(
  elementPath: ElementPath,
  status: CMSUpdateStatus,
): (_: CMSUpdateState) => CMSUpdateState {
  if (status.type === 'updating') {
    CMSOptimisticValues_GLOBAL[EP.toString(elementPath)] = status.value
  }
  return (state: CMSUpdateState) => ({
    ...state,
    [EP.toString(elementPath)]: status,
  })
}

export function unsetCMSUpdateStateForElementPath(
  elementPath: ElementPath,
): (_: CMSUpdateState) => CMSUpdateState {
  delete CMSOptimisticValues_GLOBAL[EP.toString(elementPath)]
  return (state: CMSUpdateState) => {
    const nextState = { ...state }
    delete nextState[EP.toString(elementPath)]
    return nextState
  }
}

export async function updateJurassicCMS({
  key,
  updated,
}: {
  key: string
  updated: string
}): Promise<void> {
  await fetch(`http://0.0.0.0:6789/api/${key}`, {
    method: 'POST',
    body: JSON.stringify({ value: updated }),
    mode: 'no-cors',
  })
}
