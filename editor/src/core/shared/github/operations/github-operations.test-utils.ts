import {
  setGithubState,
  setLoginState,
} from '../../../../components/editor/actions/action-creators'
import type { AsyncEditorDispatch } from '../../../../components/canvas/ui-jsx.test-utils'

export async function loginUserToGithubForTests(dispatch: AsyncEditorDispatch) {
  await dispatch(
    [
      setLoginState({ type: 'LOGGED_IN', user: { userId: 'user' } }),
      setGithubState({ authenticated: true }),
    ],
    true,
  )
}
