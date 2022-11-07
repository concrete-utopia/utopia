// https://gist.github.com/ithinkihaveacat/227bfe8aa81328c5d64ec48f4e4df8e5

/**
 * Copyright (c) 2016, Tiernan Cridland
 *
 * Permission to use, copy, modify, and/or distribute this software for any purpose with or without fee is hereby
 * granted, provided that the above copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE INCLUDING ALL
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY SPECIAL, DIRECT,
 * INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER
 * IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF OR IN CONNECTION WITH THE USE OR
 * PERFORMANCE OF THIS SOFTWARE.
 *
 * Typings for Service Worker
 * @author Tiernan Cridland
 * @email tiernanc@gmail.com
 * @license: ISC
 */

interface Navigator {
  serviceWorker: ServiceWorkerContainer
}

interface ExtendableEvent extends Event {
  waitUntil(fn: Promise<any>): void
}

interface ServiceWorker extends Worker {
  scriptURL: string
  state: ServiceWorkerState
}

interface ServiceWorkerContainer {
  controller?: ServiceWorker
  oncontrollerchange?: (event?: Event) => any
  onerror?: (event?: Event) => any
  onmessage?: (event?: Event) => any
  ready: Promise<ServiceWorkerRegistration>
  getRegistration(scope?: string): Promise<ServiceWorkerRegistration>
  getRegistrations(): Promise<Array<ServiceWorkerRegistration>>
  register(
    url: string,
    options?: ServiceWorkerRegistrationOptions,
  ): Promise<ServiceWorkerRegistration>
}

interface ServiceWorkerNotificationOptions {
  tag?: string
}

interface ServiceWorkerRegistration {
  active?: ServiceWorker
  installing?: ServiceWorker
  onupdatefound?: (event?: Event) => any
  pushManager: PushManager
  scope: string
  waiting?: ServiceWorker
  getNotifications(options?: ServiceWorkerNotificationOptions): Promise<Array<Notification>>
  update(): void
  unregister(): Promise<boolean>
}

interface ServiceWorkerRegistrationOptions {
  scope?: string
}

type ServiceWorkerState = 'installing' | 'installed' | 'activating' | 'activated' | 'redundant'

// CacheStorage API

interface Cache {
  add(request: Request): Promise<void>
  addAll(requestArray: Array<Request>): Promise<void>
  'delete'(request: Request, options?: CacheStorageOptions): Promise<boolean>
  keys(request?: Request, options?: CacheStorageOptions): Promise<Array<string>>
  match(request: Request, options?: CacheStorageOptions): Promise<Response>
  matchAll(request: Request, options?: CacheStorageOptions): Promise<Array<Response>>
  put(request: Request | string, response: Response): Promise<void>
}

interface CacheStorage {
  'delete'(cacheName: string): Promise<boolean>
  has(cacheName: string): Promise<boolean>
  keys(): Promise<Array<string>>
  match(request: Request, options?: CacheStorageOptions): Promise<Response>
  open(cacheName: string): Promise<Cache>
}

interface CacheStorageOptions {
  cacheName?: string
  ignoreMethod?: boolean
  ignoreSearch?: boolean
  ignoreVary?: boolean
}

// Client API

interface Client {
  frameType: ClientFrameType
  id: string
  url: string
}

interface Clients {
  claim(): Promise<any>
  get(id: string): Promise<Client>
  matchAll(options?: ClientMatchOptions): Promise<Array<Client>>
  openWindow(url: string): Promise<WindowClient>
}

interface ClientMatchOptions {
  includeUncontrolled?: boolean
  type?: ClientMatchTypes
}

interface WindowClient {
  focused: boolean
  visibilityState: WindowClientState
  focus(): Promise<WindowClient>
  navigate(url: string): Promise<WindowClient>
}

type ClientFrameType = 'auxiliary' | 'top-level' | 'nested' | 'none'
type ClientMatchTypes = 'window' | 'worker' | 'sharedworker' | 'all'
type WindowClientState = 'hidden' | 'visible' | 'prerender' | 'unloaded'

// Fetch API

interface Body {
  bodyUsed: boolean
  arrayBuffer(): Promise<ArrayBuffer>
  blob(): Promise<Blob>
  formData(): Promise<FormData>
  json(): Promise<any>
  text(): Promise<string>
}

interface FetchEvent extends Event {
  request: Request
  respondWith(response: Promise<Response> | Response): Promise<Response>
}

interface InstallEvent extends ExtendableEvent {
  activeWorker: ServiceWorker
}

interface ActivateEvent extends ExtendableEvent {}

interface Headers {
  //   new (init?: any): Headers
  append(name: string, value: string): void
  'delete'(name: string): void
  entries(): Array<Array<string>>
  get(name: string): string
  getAll(name: string): Array<string>
  has(name: string): boolean
  keys(): Array<string>
  set(name: string, value: string): void
  values(): Array<string>
}

interface Request extends Body {
  //   new (
  //     url: string,
  //     init?: {
  //       method?: string
  //       url?: string
  //       referrer?: string
  //       mode?: 'cors' | 'no-cors' | 'same-origin' | 'navigate'
  //       credentials?: 'omit' | 'same-origin' | 'include'
  //       redirect?: 'follow' | 'error' | 'manual'
  //       integrity?: string
  //       cache?: 'default' | 'no-store' | 'reload' | 'no-cache' | 'force-cache'
  //       headers?: Headers
  //     },
  //   ): Request
  cache: RequestCache
  credentials: RequestCredentials
  headers: Headers
  integrity: string
  method: string
  mode: RequestMode
  referrer: string
  referrerPolicy: ReferrerPolicy
  redirect: RequestRedirect
  url: string
  clone(): Request
}

interface Response extends Body {
  //   new (url: string): Response
  //   new (
  //     body: Blob | BufferSource | FormData | String,
  //     init: {
  //       status?: number
  //       statusText?: string
  //       headers?: Headers | { [k: string]: string }
  //     },
  //   ): Response
  headers: Headers
  ok: boolean
  redirected: boolean
  status: number
  statusText: string
  type: ResponseType
  url: string
  useFinalURL: boolean
  clone(): Response
  error(): Response
  redirect(): Response
}

type ReferrerPolicy =
  | ''
  | 'no-referrer'
  | 'no-referrer-when-downgrade'
  | 'origin-only'
  | 'origin-when-cross-origin'
  | 'unsafe-url'
type RequestCache = 'default' | 'no-store' | 'reload' | 'no-cache' | 'force-cache'
type RequestCredentials = 'omit' | 'same-origin' | 'include'
type RequestMode = 'cors' | 'no-cors' | 'same-origin' | 'navigate'
type RequestRedirect = 'follow' | 'error' | 'manual'
type ResponseType = 'basic' | 'cores' | 'error' | 'opaque'

// Notification API

interface Notification {
  body: string
  data: any
  icon: string
  lang: string
  requireInteraction: boolean
  silent: boolean
  tag: string
  timestamp: number
  title: string
  close(): void
  requestPermission(): Promise<string>
}

interface NotificationEvent {
  action: string
  notification: Notification
}

// Push API

interface PushEvent extends ExtendableEvent {
  data: PushMessageData
}

interface PushManager {
  getSubscription(): Promise<PushSubscription>
  permissionState(): Promise<string>
  subscribe(): Promise<PushSubscription>
}

interface PushMessageData {
  arrayBuffer(): ArrayBuffer
  blob(): Blob
  json(): any
  text(): string
}

interface PushSubscription {
  endpoint: string
  getKey(method: string): ArrayBuffer
  toJSON(): string
  unsubscribe(): Promise<boolean>
}

// Sync API

interface SyncEvent extends Event {
  lastChance: boolean
  tag: string
}

// ServiceWorkerGlobalScope

declare var Headers: Headers
declare var Response: Response
declare var Request: Request
declare var caches: CacheStorage
declare var clients: Clients
declare var onactivate: (event?: ExtendableEvent) => any
declare var onfetch: (event?: FetchEvent) => any
declare var oninstall: (event?: ExtendableEvent) => any
declare var onmessage: (event: MessageEvent) => any
declare var onnotificationclick: (event?: NotificationEvent) => any
declare var onnotificationclose: (event?: NotificationEvent) => any
declare var onpush: (event?: PushEvent) => any
declare var onpushsubscriptionchange: () => any
declare var onsync: (event?: SyncEvent) => any
declare var registration: ServiceWorkerRegistration

declare function fetch(request: Request | string): Promise<Response>
declare function skipWaiting(): void
