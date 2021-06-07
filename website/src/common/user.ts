export interface UserDetails {
  userId: string
  email?: string
  name?: string
  picture?: string
}

interface LoggedInUser {
  type: 'LOGGED_IN'
  user: UserDetails
}

interface NotLoggedIn {
  type: 'NOT_LOGGED_IN'
}

interface LoginLost {
  type: 'LOGIN_LOST'
}

interface OfflineState {
  type: 'OFFLINE_STATE'
}

interface CookiesOrLocalForageUnavailable {
  type: 'COOKIES_OR_LOCALFORAGE_UNAVAILABLE'
}

export type LoginState =
  | LoggedInUser
  | NotLoggedIn
  | LoginLost
  | OfflineState
  | CookiesOrLocalForageUnavailable

export function loggedInUser(user: UserDetails): LoggedInUser {
  return {
    type: 'LOGGED_IN',
    user: user,
  }
}

export const notLoggedIn: NotLoggedIn = {
  type: 'NOT_LOGGED_IN',
}

export function isNotLoggedIn(loginState: unknown): loginState is NotLoggedIn {
  return (loginState as Partial<LoginState>)?.type === 'NOT_LOGGED_IN'
}

export function isLoggedIn(loginState: unknown): loginState is LoggedInUser {
  return (loginState as Partial<LoginState>)?.type === 'LOGGED_IN'
}

export const loginLost: LoginLost = {
  type: 'LOGIN_LOST',
}

export function isLoginLost(loginState: unknown): loginState is LoginLost {
  return (loginState as Partial<LoginState>)?.type === 'LOGIN_LOST'
}

export const offlineState: OfflineState = { type: 'OFFLINE_STATE' }

export function isOfflineState(loginState: unknown): loginState is OfflineState {
  return (loginState as Partial<LoginState>)?.type === 'OFFLINE_STATE'
}

export const cookiesOrLocalForageUnavailable: CookiesOrLocalForageUnavailable = {
  type: 'COOKIES_OR_LOCALFORAGE_UNAVAILABLE',
}

export function isCookiesOrLocalForageUnavailable(
  loginState: unknown,
): loginState is CookiesOrLocalForageUnavailable {
  return (loginState as Partial<LoginState>)?.type === 'COOKIES_OR_LOCALFORAGE_UNAVAILABLE'
}

export function getUserPicture(loginState: LoginState): string | null {
  if (isLoggedIn(loginState)) {
    return loginState.user.picture ?? null
  } else {
    return null
  }
}
