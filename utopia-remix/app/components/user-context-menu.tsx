import { useCallback, useMemo } from 'react'
import urlJoin from 'url-join'
import { useAppStore } from '../stores/appStore'
import React from 'react'
import { DropdownMenu } from '@radix-ui/themes'

export function UserContextMenu() {
  const env = useAppStore((store) => store.env)
  const logoutUrl = useMemo(() => urlJoin(env?.EDITOR_URL ?? '', '/logout'), [env])
  const logout = useCallback(() => {
    window.location.assign(logoutUrl)
  }, [logoutUrl])
  return (
    <DropdownMenu.Content>
      <DropdownMenu.Item style={{ height: 28, fontSize: 12, width: 100 }} onClick={logout}>
        Log Out
      </DropdownMenu.Item>
    </DropdownMenu.Content>
  )
}
