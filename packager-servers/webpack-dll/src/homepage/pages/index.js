import React from 'react';
import Answer from '../components/Answer';
import Packagers from '../components/Packagers';

export default () => (
  <div>
    <style jsx>{`
      h1 {
        text-align: center;
        font-weight: 400;
        color: #585E64;
        font-size: 3rem;
        margin-top: 4rem;
        margin-bottom: 0;
      }

      @media screen and (max-width: 800px) {
        h1 {
          margin-top: 1rem;
        }
      }

      h2 {
        text-align: center;
        font-weight: 300;
        color: #7A8490;
        margin-top: 0;
        margin-bottom: 4rem;
      }


    `}</style>
    <h1>NPM Packager Service</h1>
    <h2>Create a consumable script from any combination of dependencies</h2>
    <Packagers />

    <Answer question="What is this?">
      This is a package bundler, it builds a UMD build of a combination of packages which you can use as DLL plugin or as a
      way to include NPM dependencies in an online editor. It consists of two parts: a dll service and the packagers. The dll service acts
      as a load balancer that routes the requests to the appropriate packagers. It's used by
      {' '}
      <a target="_blank" href="https://webpackbin.com">WebpackBin</a>
      {' '}
      and
      {' '}
      <a target="_blank" href="https://codesandbox.io">CodeSandbox</a>
      .
    </Answer>

    <Answer question="Can I use it?">
      Yes you can! We encourage you to add a packager (check
      {' '}
      <a href="#canihelp">Can I help?</a>
      ) when you make extensive use of it.
      <p>
        You can get a bundle by requesting:
        <br />
        <code>
          https://cdn.jsdelivr.net/webpack/v2/
          <span style={{ color: 'rgba(52, 152, 219,1.0)' }}>
            dep@version+dep2@version
          </span>
          /<span style={{ color: 'rgba(46, 204, 113,1.0)' }}>dll.js</span>
        </code>
        <br />
        <br />

        Example:
        <br />
        <code>
          https://cdn.jsdelivr.net/webpack/v2/
          <span style={{ color: 'rgba(52, 152, 219,1.0)' }}>
            react@15.5.3+react-dom@15.5.3
          </span>
          /<span style={{ color: 'rgba(46, 204, 113,1.0)' }}>dll.js</span>
        </code>
      </p>

      To request a manifest you can replace the
      {' '}
      <code>dll.js</code>
      {' '}
      to
      {' '}
      <code>manifest.json</code>.
    </Answer>

    <Answer id="canihelp" question="Can I help?">
      Yes! You can also help by hosting your own packager service. This is a fairly
      straightforward process and helps the service scale. Follow the instructions
      {' '}
      <a target="_blank" href="https://github.com/cerebral/webpack-packager">
        here
      </a>
      {' '}
      and send a message to
      {' '}
      <a target="_blank" href="https://twitter.com/christianalfoni">
        Christian
      </a>
      {' '}
      or
      {' '}
      <a target="_blank" href="https://twitter.com/Ives13">Ives</a>
      {' '}
      to add your packager to the DLL service!

      <br />
      <br />

      If you want to improve the speed or compatibility with dependencies you can help
      by contributing to the source on GitHub:
      {' '}
      <a target="_blank" href="https://github.com/cerebral/webpack-packager">
        Packager
      </a>
      ,
      {' '}
      <a target="_blank" href="https://github.com/cerebral/webpack-dll">
        DLL service
      </a>
      .
    </Answer>
  </div>
);
