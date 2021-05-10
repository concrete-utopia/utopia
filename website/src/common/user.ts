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

export type LoginState = LoggedInUser | NotLoggedIn | LoginLost | OfflineState

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
