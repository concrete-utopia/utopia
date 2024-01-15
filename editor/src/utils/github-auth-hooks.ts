import React from 'react'
import { useDispatch } from '../components/editor/store/dispatch-context'
import { GithubAuth } from './github-auth'

export function useOnClickAuthenticateWithGithub() {
  const dispatch = useDispatch()
  const triggerAuthentication = React.useCallback(() => {
    void GithubAuth.startGithubAuthentication(dispatch)
  }, [dispatch])
  return triggerAuthentication
}
