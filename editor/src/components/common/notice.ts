import type React from 'react'
import { v4 as UUID } from 'uuid'

/**
 *  - _Error_: High visibility
 *  - _Warning_: High visisibility
 *  - _Primary_: Medium high visibility. Use for CTAs, nudges
 *  - _Success_: Medium visibility. Use for async or higher-failure-probability positive outcomes
 *  - _Notice_: Medium visibility. Use for synchronous info, confirmation, or to stand out from background
 *  - _Info_: Low visibility. Use to display information that can blend into background
 *
 * */

export type NoticeLevel = 'ERROR' | 'WARNING' | 'PRIMARY' | 'SUCCESS' | 'NOTICE' | 'INFO'
export interface Notice {
  message: React.ReactChild
  level: NoticeLevel
  persistent: boolean
  id: string
}

export function notice(
  message: React.ReactChild,
  level: NoticeLevel = 'INFO',
  persistent: boolean = false,
  id: string = UUID(),
): Notice {
  return { message: message, persistent: persistent, level: level, id: id }
}
