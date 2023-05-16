class FakeLocalForage {
  getItem<T>(id: string): Promise<T | null> {
    return Promise.resolve(null)
  }

  setItem<T>(id: string, value: T): Promise<void> {
    return Promise.resolve()
  }

  removeItem(id: string): Promise<void> {
    return Promise.resolve()
  }

  keys(): Promise<Array<string>> {
    return Promise.resolve([])
  }
}

export default new FakeLocalForage()
