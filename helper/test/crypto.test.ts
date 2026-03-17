import { describe, expect, it } from 'vitest';
import { decryptLegacy, encryptLegacy, deriveContentKeyPair, decryptWithDataKey, encryptWithDataKey } from '../src/crypto.js';

describe('crypto', () => {
  it('roundtrips legacy encryption', () => {
    const secret = new Uint8Array(32).fill(7);
    const encrypted = encryptLegacy({ ok: true }, secret);
    expect(decryptLegacy(encrypted, secret)).toEqual({ ok: true });
  });

  it('roundtrips dataKey encryption', () => {
    const key = new Uint8Array(32).fill(9);
    const encrypted = encryptWithDataKey({ ok: true }, key);
    expect(decryptWithDataKey(encrypted, key)).toEqual({ ok: true });
  });

  it('derives deterministic content keys', () => {
    const secret = new Uint8Array(32).fill(3);
    expect(deriveContentKeyPair(secret)).toEqual(deriveContentKeyPair(secret));
  });
});
