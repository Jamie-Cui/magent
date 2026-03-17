import { readFileSync } from 'node:fs';
import type { HelperConfig, Credentials } from './types.js';
import { decodeBase64, deriveContentKeyPair } from './crypto.js';

export function readCredentials(config: HelperConfig): Credentials {
  try {
    const raw = JSON.parse(readFileSync(config.credentialPath, 'utf-8')) as {
      token?: string;
      secret?: string;
    };
    if (!raw.token || !raw.secret) {
      throw new Error('Credential file is missing token or secret');
    }
    const secret = decodeBase64(raw.secret);
    return {
      token: raw.token,
      secret,
      contentKeyPair: deriveContentKeyPair(secret),
    };
  } catch (error) {
    throw new Error(
      `Failed to read Happy credentials from ${config.credentialPath}: ${
        error instanceof Error ? error.message : String(error)
      }`,
    );
  }
}
