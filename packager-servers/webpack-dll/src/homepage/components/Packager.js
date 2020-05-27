// @flow
import React from 'react';

type Props = {
  name: string,
  isAvailable: boolean,
  lastUsed: number,
  resolvedCount: number,
  errorCount: number,
};

export default ({
  name,
  isAvailable,
  lastUsed,
  resolvedCount,
  errorCount,
}: Props) => {
  const color = isAvailable
    ? 'rgba(39, 174, 96,1.0)'
    : 'rgba(230, 126, 34,1.0)';

  return (
    <div
      style={{
        color,
        borderBottom: `3px solid ${color}`,
      }}
      className="packager"
    >
      <style jsx>{`
      .packager {
        transition: 0.3s ease all;
        background-color: white;
        padding: 1rem;
        box-shadow: 0 2px 3px rgba(0,0,0,0.15);
        border-radius: 2px;
        width: 200px;
        height: 100px;
        margin: 0.5rem;

        display: flex;
        flex-direction: column;
        justify-content: center;
        align-items: center;
      }

      .date {
        font-size: .75rem;
      }
    `}</style>
      <div>{name}</div>
      <div className="date">
        Status:
        {' '}
        <strong style={{ color: color }}>
          {isAvailable ? 'idle' : 'bundling'}
        </strong>
      </div>
      <div className="date">
        Last used: <strong>{new Date(lastUsed).toLocaleTimeString()}</strong>
      </div>
      <div className="date">
        Succesful bundles: <strong>{resolvedCount}</strong>
      </div>
      <div className="date">
        Busy: <strong>{errorCount}</strong>
      </div>
    </div>
  );
};
