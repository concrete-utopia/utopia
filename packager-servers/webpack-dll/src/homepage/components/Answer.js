// @flow
import React from 'react';

export default ({
  question,
  children,
  ...props
}: { question: string, children: React.ReactElement }) => (
  <div {...props}>
    <style jsx>{`
      h3 {
        font-size: 1.5rem;
        color: rgba(46, 204, 113,1.0);
        text-transform: uppercase;
        font-weight: 400;
        margin-bottom: 0rem;
        padding-bottom: 0;
      }

      p {
        margin-top: .5rem;
      }

      div {
        margin-bottom: 2rem;
      }
    `}</style>
    <h3>
      {question}
    </h3>
    <p>
      {children}
    </p>
  </div>
);
