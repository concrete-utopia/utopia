import {
  addNewFeaturedRouteToPackageJson,
  remixFilenameMatchPrefix,
  renameRemixFile,
  removeFeaturedRouteFromPackageJson,
} from './remix-utils'

const remixRootDir = '/app'

describe('addNewFeaturedRouteToPackageJson', () => {
  it('adds the new path to the featured routes', async () => {
    const got = addNewFeaturedRouteToPackageJson('foo/bar')(`
{
  "hey": "there",
  "utopia": {
    "featuredRoutes": [
      "/",
      "/test"
    ]
  }
}`)
    expect(got).toEqual(
      `
{
  "hey": "there",
  "utopia": {
    "featuredRoutes": [
      "/",
      "/test",
      "/foo/bar"
    ]
  }
}`.trim(),
    )
  })
  it('does nothing if the route is already there', async () => {
    const got = addNewFeaturedRouteToPackageJson('foo/bar')(`
{
  "hey": "there",
  "utopia": {
    "featuredRoutes": [
      "/",
      "/foo/bar",
      "/test"
    ]
  }
}`)
    expect(got).toEqual(
      `
{
  "hey": "there",
  "utopia": {
    "featuredRoutes": [
      "/",
      "/foo/bar",
      "/test"
    ]
  }
}`.trim(),
    )
  })
  it('adds the featured routes prop if missing', async () => {
    const got = addNewFeaturedRouteToPackageJson('foo/bar')(`{ "hey": "there", "utopia": {} }`)
    const want = `
{
  "hey": "there",
  "utopia": {
    "featuredRoutes": [
      "/foo/bar"
    ]
  }
}`
    expect(got).toEqual(want.trim())
  })
  it('adds the utopia prop if missing', async () => {
    const got = addNewFeaturedRouteToPackageJson('foo/bar')(`{ "hey": "there" }`)
    const want = `
{
  "hey": "there",
  "utopia": {
    "featuredRoutes": [
      "/foo/bar"
    ]
  }
}`
    expect(got).toEqual(want.trim())
  })
  it('errors if the utopia prop if not an object', async () => {
    expect(() =>
      addNewFeaturedRouteToPackageJson('foo.bar')(`{ "hey": "there", "utopia": "WRONG" }`),
    ).toThrow('should be an object')
  })
  it('errors if the featured routes prop if not an array', async () => {
    expect(() =>
      addNewFeaturedRouteToPackageJson('foo.bar')(
        `{ "hey": "there", "utopia": {"featuredRoutes": "WRONG"} }`,
      ),
    ).toThrow('should be an array')
  })
})

describe('removeFeaturedRouteFromPackageJson', () => {
  it('removes the path from the featured routes', async () => {
    const got = removeFeaturedRouteFromPackageJson('foo/bar')(`
{
  "hey": "there",
  "utopia": {
    "featuredRoutes": [
      "/",
      "/foo/bar",
      "/test"
    ]
  }
}`)
    expect(got).toEqual(
      `
{
  "hey": "there",
  "utopia": {
    "featuredRoutes": [
      "/",
      "/test"
    ]
  }
}`.trim(),
    )
  })
  it('does nothing if the route is not there', async () => {
    const got = removeFeaturedRouteFromPackageJson('foo/bar')(`
{
  "hey": "there",
  "utopia": {
    "featuredRoutes": [
      "/",
      "/test"
    ]
  }
}`)
    expect(got).toEqual(
      `
{
  "hey": "there",
  "utopia": {
    "featuredRoutes": [
      "/",
      "/test"
    ]
  }
}`.trim(),
    )
  })
  it('errors if the utopia prop if not an object', async () => {
    expect(() =>
      removeFeaturedRouteFromPackageJson('foo.bar')(`{ "hey": "there", "utopia": "WRONG" }`),
    ).toThrow('should be an object')
  })
  it('errors if the featured routes prop if not an array', async () => {
    expect(() =>
      removeFeaturedRouteFromPackageJson('foo.bar')(
        `{ "hey": "there", "utopia": {"featuredRoutes": "WRONG"} }`,
      ),
    ).toThrow('should be an array')
  })
})

describe('remixFilenameMatchPrefix', () => {
  function findChildrenPaths(tree: string[], target: string) {
    return tree.filter((path) => remixFilenameMatchPrefix(remixRootDir, path, target))
  }
  it('simple paths', async () => {
    const got = findChildrenPaths(
      [
        '/app/routes/auth.jsx',
        '/app/routes/auth.login.jsx',
        '/app/routes/auth.login.callback.jsx',
        '/app/routes/another.jsx',
        '/app/routes/another.auth.jsx',
      ],
      '/app/routes/auth.login',
    )
    expect(got).toEqual(['/app/routes/auth.login.jsx', '/app/routes/auth.login.callback.jsx'])
  })
  it('complex paths', async () => {
    const got = findChildrenPaths(
      [
        // direct
        '/app/routes/auth.jsx',
        // child
        '/app/routes/auth.login.jsx',
        // child
        '/app/routes/auth_.logout.jsx',
        // child
        '/app/routes/auth._index.jsx',
        // NO!
        '/app/routes/another.jsx',
        // child
        '/app/routes/auth_._index.jsx',
        // child, with optional prefix
        '/app/routes/($lang).auth.something.jsx',
        // child, with optional prefix
        '/app/routes/(foo).auth.something.jsx',
        // NO!
        '/app/routes/another.auth.jsx',
      ],
      '/app/routes/auth',
    )
    expect(got).toEqual([
      '/app/routes/auth.jsx',
      '/app/routes/auth.login.jsx',
      '/app/routes/auth_.logout.jsx',
      '/app/routes/auth._index.jsx',
      '/app/routes/auth_._index.jsx',
      '/app/routes/($lang).auth.something.jsx',
      '/app/routes/(foo).auth.something.jsx',
    ])
  })
})

describe('renameRemixFile', () => {
  it('rename a file with match', async () => {
    const got = renameRemixFile({
      remixRootDir: remixRootDir,
      filename: '/app/routes/foo.jsx',
      oldPath: '/app/routes/foo',
      newPath: '/app/routes/bar',
    })
    expect(got.filename).toBe('/app/routes/bar.jsx')
    expect(got.renamedOptionalPrefix).toBe(false)
  })
  it('rename a file with underscore suffix', async () => {
    const got = renameRemixFile({
      remixRootDir: remixRootDir,
      filename: '/app/routes/foo_.bar.$baz.$qux.jsx',
      oldPath: '/app/routes/foo',
      newPath: '/app/routes/HELLO',
    })
    expect(got.filename).toBe('/app/routes/HELLO_.bar.$baz.$qux.jsx')
    expect(got.renamedOptionalPrefix).toBe(false)
  })
  it('rename a file with prefix', async () => {
    const got = renameRemixFile({
      remixRootDir: remixRootDir,
      filename: '/app/routes/foo.bar.jsx',
      oldPath: '/app/routes/foo',
      newPath: '/app/routes/baz',
    })
    expect(got.filename).toBe('/app/routes/baz.bar.jsx')
    expect(got.renamedOptionalPrefix).toBe(false)
  })
  it('rename a file with optional prefix', async () => {
    const got = renameRemixFile({
      remixRootDir: remixRootDir,
      filename: '/app/routes/($lang).foo.bar.jsx',
      oldPath: '/app/routes/foo',
      newPath: '/app/routes/baz',
    })
    expect(got.filename).toBe('/app/routes/($lang).baz.bar.jsx')
    expect(got.renamedOptionalPrefix).toBe(true)
  })
  it('rename a file with multiple optional prefixes', async () => {
    const got = renameRemixFile({
      remixRootDir: remixRootDir,
      filename: '/app/routes/($lang1).($lang2).(lang3).foo.bar.jsx',
      oldPath: '/app/routes/foo',
      newPath: '/app/routes/baz',
    })
    expect(got.filename).toBe('/app/routes/($lang1).($lang2).(lang3).baz.bar.jsx')
    expect(got.renamedOptionalPrefix).toBe(true)
  })
  it('rename a file with nested optional prefix', async () => {
    const got = renameRemixFile({
      remixRootDir: remixRootDir,
      filename: '/app/routes/($lang).account.($var).foo.bar.jsx',
      oldPath: '/app/routes/($lang).account.foo',
      newPath: '/app/routes/($lang).account.baz',
    })
    expect(got.filename).toBe('/app/routes/($lang).account.($var).baz.bar.jsx')
    expect(got.renamedOptionalPrefix).toBe(true)
  })
  it('rename a file with nested optional prefix and an underscore suffix', async () => {
    const got = renameRemixFile({
      remixRootDir: remixRootDir,
      filename: '/app/routes/($lang).account.($var).foo_.bar.jsx',
      oldPath: '/app/routes/($lang).account.foo',
      newPath: '/app/routes/($lang).account.baz',
    })
    expect(got.filename).toBe('/app/routes/($lang).account.($var).baz_.bar.jsx')
    expect(got.renamedOptionalPrefix).toBe(true)
  })
})
