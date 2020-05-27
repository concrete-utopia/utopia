const next = require('next');
const LRUCache = require('lru-cache');

const DEBUG = process.env.DEBUG === 'true';

const app = next({ dir: __dirname, dev: DEBUG });
const handle = app.getRequestHandler();

const ssrCache = new LRUCache({
  max: 100,
  maxAge: 1000 * 60 * 60,
});

const renderAndCache = app => (req, res, pagePath, queryParams) => {
  const key = getCacheKey(req);

  // If we have a page in the cache, let's serve it
  if (!DEBUG && ssrCache.has(key)) {
    console.log(`CACHE HIT: ${key}`);
    res.send(ssrCache.get(key));
    return;
  }

  // If not let's render the page into HTML
  app
    .renderToHTML(req, res, pagePath, queryParams)
    .then(html => {
      // Let's cache this page
      console.log(`CACHE MISS: ${key}`);
      ssrCache.set(key, html);

      res.send(html);
    })
    .catch(err => {
      app.renderError(err, req, res, pagePath, queryParams);
    });
};

function getCacheKey (req) {
  return `${req.url}`
}

module.exports = {
  app,
  load: () => new Promise((resolve, reject) => {
    app.prepare().then(() => {
      resolve({render: renderAndCache(app), handle });
    });
  })
};
