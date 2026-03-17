import { describe, expect, it } from 'vitest';
import { parseInputLine, encodeOutput } from '../src/protocol.js';

describe('protocol', () => {
  it('parses create-session input', () => {
    const parsed = parseInputLine(JSON.stringify({ type: 'create-session', tag: 'demo' }));
    expect(parsed).toEqual({ type: 'create-session', tag: 'demo' });
  });

  it('encodes output lines', () => {
    expect(encodeOutput({ type: 'connected', session_id: 'abc' })).toBe(
      `${JSON.stringify({ type: 'connected', session_id: 'abc' })}\n`,
    );
  });
});
