import { addNewFeaturedRouteToPackageJson } from './remix-utils'

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
      "/foo",
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
