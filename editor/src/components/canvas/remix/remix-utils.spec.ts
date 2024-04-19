import {
  addNewFeaturedRouteToPackageJson,
  remixFilenameMatchPrefix,
  removeFeaturedRouteFromPackageJson,
} from './remix-utils'

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
    return tree.filter((path) => remixFilenameMatchPrefix(path, target))
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
