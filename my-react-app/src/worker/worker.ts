const ctx: Worker = self as any

ctx.addEventListener('message', (event: MessageEvent) => {
  console.log(`This is the worker!`, event.data)
})

export const hello = 1
