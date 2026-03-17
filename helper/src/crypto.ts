import { createCipheriv, createDecipheriv, createHash, createHmac, randomBytes } from 'node:crypto';
import tweetnacl from 'tweetnacl';
import type { EncryptionVariant } from './types.js';

export function encodeBase64(buffer: Uint8Array): string {
  return Buffer.from(buffer).toString('base64');
}

export function decodeBase64(base64: string): Uint8Array {
  return new Uint8Array(Buffer.from(base64, 'base64'));
}

export function getRandomBytes(size: number): Uint8Array {
  return new Uint8Array(randomBytes(size));
}

function hmacSha512(key: Uint8Array, data: Uint8Array): Uint8Array {
  const hmac = createHmac('sha512', key);
  hmac.update(data);
  return new Uint8Array(hmac.digest());
}

function deriveSecretKeyTreeRoot(seed: Uint8Array, usage: string) {
  const derived = hmacSha512(new TextEncoder().encode(`${usage} Master Seed`), seed);
  return {
    key: derived.slice(0, 32),
    chainCode: derived.slice(32),
  };
}

function deriveSecretKeyTreeChild(chainCode: Uint8Array, index: string) {
  const derived = hmacSha512(chainCode, new Uint8Array([0x00, ...new TextEncoder().encode(index)]));
  return {
    key: derived.slice(0, 32),
    chainCode: derived.slice(32),
  };
}

function deriveKey(master: Uint8Array, usage: string, path: string[]) {
  let state = deriveSecretKeyTreeRoot(master, usage);
  for (const segment of path) {
    state = deriveSecretKeyTreeChild(state.chainCode, segment);
  }
  return state.key;
}

export function deriveContentKeyPair(secret: Uint8Array) {
  const seed = deriveKey(secret, 'Happy EnCoder', ['content']);
  const hashedSeed = new Uint8Array(createHash('sha512').update(seed).digest());
  const boxSecretKey = hashedSeed.slice(0, 32);
  const keyPair = tweetnacl.box.keyPair.fromSecretKey(boxSecretKey);
  return {
    publicKey: keyPair.publicKey,
    secretKey: keyPair.secretKey,
  };
}

export function encryptLegacy(data: unknown, secret: Uint8Array): Uint8Array {
  const nonce = getRandomBytes(tweetnacl.secretbox.nonceLength);
  const plaintext = new TextEncoder().encode(JSON.stringify(data));
  const encrypted = tweetnacl.secretbox(plaintext, nonce, secret);
  const result = new Uint8Array(nonce.length + encrypted.length);
  result.set(nonce);
  result.set(encrypted, nonce.length);
  return result;
}

export function decryptLegacy(data: Uint8Array, secret: Uint8Array): unknown | null {
  try {
    const nonce = data.slice(0, tweetnacl.secretbox.nonceLength);
    const encrypted = data.slice(tweetnacl.secretbox.nonceLength);
    const decrypted = tweetnacl.secretbox.open(encrypted, nonce, secret);
    return decrypted ? JSON.parse(new TextDecoder().decode(decrypted)) : null;
  } catch {
    return null;
  }
}

export function encryptWithDataKey(data: unknown, dataKey: Uint8Array): Uint8Array {
  const nonce = getRandomBytes(12);
  const cipher = createCipheriv('aes-256-gcm', dataKey, nonce);
  const encrypted = Buffer.concat([
    cipher.update(new TextEncoder().encode(JSON.stringify(data))),
    cipher.final(),
  ]);
  const authTag = cipher.getAuthTag();
  const bundle = new Uint8Array(1 + 12 + encrypted.length + 16);
  bundle[0] = 0;
  bundle.set(nonce, 1);
  bundle.set(new Uint8Array(encrypted), 13);
  bundle.set(new Uint8Array(authTag), 13 + encrypted.length);
  return bundle;
}

export function decryptWithDataKey(bundle: Uint8Array, dataKey: Uint8Array): unknown | null {
  if (bundle.length < 29 || bundle[0] !== 0) {
    return null;
  }
  const nonce = bundle.slice(1, 13);
  const authTag = bundle.slice(bundle.length - 16);
  const ciphertext = bundle.slice(13, bundle.length - 16);
  try {
    const decipher = createDecipheriv('aes-256-gcm', dataKey, nonce);
    decipher.setAuthTag(authTag);
    const decrypted = Buffer.concat([decipher.update(ciphertext), decipher.final()]);
    return JSON.parse(new TextDecoder().decode(decrypted));
  } catch {
    return null;
  }
}

export function encrypt(key: Uint8Array, variant: EncryptionVariant, data: unknown): Uint8Array {
  return variant === 'legacy' ? encryptLegacy(data, key) : encryptWithDataKey(data, key);
}

export function decrypt(key: Uint8Array, variant: EncryptionVariant, data: Uint8Array): unknown | null {
  return variant === 'legacy' ? decryptLegacy(data, key) : decryptWithDataKey(data, key);
}

export function decryptBoxBundle(bundle: Uint8Array, recipientSecretKey: Uint8Array): Uint8Array | null {
  if (bundle.length < 56) {
    return null;
  }
  const ephemeralPublicKey = bundle.slice(0, 32);
  const nonce = bundle.slice(32, 56);
  const ciphertext = bundle.slice(56);
  const decrypted = tweetnacl.box.open(ciphertext, nonce, ephemeralPublicKey, recipientSecretKey);
  return decrypted ? new Uint8Array(decrypted) : null;
}

export function encryptForPublicKey(data: Uint8Array, recipientPublicKey: Uint8Array): Uint8Array {
  const ephemeralKeyPair = tweetnacl.box.keyPair();
  const nonce = getRandomBytes(tweetnacl.box.nonceLength);
  const encrypted = tweetnacl.box(data, nonce, recipientPublicKey, ephemeralKeyPair.secretKey);
  const result = new Uint8Array(32 + 24 + encrypted.length);
  result.set(ephemeralKeyPair.publicKey, 0);
  result.set(nonce, 32);
  result.set(encrypted, 56);
  return result;
}
