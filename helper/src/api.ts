import axios, { AxiosError } from 'axios';
import type { DecryptedSession, HelperConfig, Credentials, RawSession, SessionEncryption } from './types.js';
import {
  decodeBase64,
  decryptBoxBundle,
  decryptLegacy,
  decryptWithDataKey,
  encryptForPublicKey,
  encodeBase64,
  encryptWithDataKey,
  getRandomBytes,
} from './crypto.js';

function authHeaders(creds: Credentials): Record<string, string> {
  return { Authorization: `Bearer ${creds.token}` };
}

function decryptField(encrypted: string | null, encryption: SessionEncryption): Record<string, unknown> | null {
  if (!encrypted) {
    return null;
  }
  const bytes = decodeBase64(encrypted);
  const value =
    encryption.variant === 'dataKey'
      ? decryptWithDataKey(bytes, encryption.key)
      : decryptLegacy(bytes, encryption.key);
  return (value as Record<string, unknown> | null) ?? null;
}

export function resolveSessionEncryption(session: RawSession, creds: Credentials): SessionEncryption {
  if (session.dataEncryptionKey) {
    const encrypted = decodeBase64(session.dataEncryptionKey);
    const bundle = encrypted.slice(1);
    const sessionKey = decryptBoxBundle(bundle, creds.contentKeyPair.secretKey);
    if (!sessionKey) {
      throw new Error(`Failed to decrypt session key for ${session.id}`);
    }
    return {
      key: sessionKey,
      variant: 'dataKey',
    };
  }
  return {
    key: creds.secret,
    variant: 'legacy',
  };
}

function decryptSession(raw: RawSession, creds: Credentials): DecryptedSession {
  const encryption = resolveSessionEncryption(raw, creds);
  return {
    id: raw.id,
    seq: raw.seq,
    createdAt: raw.createdAt,
    updatedAt: raw.updatedAt,
    active: raw.active,
    activeAt: raw.activeAt,
    metadata: decryptField(raw.metadata, encryption) ?? {},
    agentState: decryptField(raw.agentState, encryption),
    encryption,
  };
}

function formatApiError(error: unknown, context: string): Error {
  if (error instanceof AxiosError) {
    const status = error.response?.status;
    if (status === 401) {
      return new Error('Authentication expired. Run `happy-agent auth login` again.');
    }
    const detail = error.response?.data ? `: ${JSON.stringify(error.response.data)}` : '';
    return new Error(`Happy API ${context} failed${status ? ` (${status})` : ''}${detail}`);
  }
  return error instanceof Error ? error : new Error(String(error));
}

export async function listSessions(config: HelperConfig, creds: Credentials): Promise<DecryptedSession[]> {
  try {
    const response = await axios.get<{ sessions: RawSession[] }>(`${config.serverUrl}/v1/sessions`, {
      headers: authHeaders(creds),
    });
    return response.data.sessions.map((session) => decryptSession(session, creds));
  } catch (error) {
    throw formatApiError(error, 'list');
  }
}

export async function resolveSessionById(
  config: HelperConfig,
  creds: Credentials,
  sessionId: string,
): Promise<DecryptedSession> {
  const sessions = await listSessions(config, creds);
  const matches = sessions.filter((session) => session.id.startsWith(sessionId));
  if (matches.length === 0) {
    throw new Error(`No Happy session found matching "${sessionId}"`);
  }
  if (matches.length > 1) {
    throw new Error(`Ambiguous Happy session prefix "${sessionId}"`);
  }
  return matches[0]!;
}

export async function createSession(
  config: HelperConfig,
  creds: Credentials,
  tag: string,
  metadata: Record<string, unknown>,
): Promise<DecryptedSession> {
  const sessionKey = getRandomBytes(32);
  const encryptedKey = encryptForPublicKey(sessionKey, creds.contentKeyPair.publicKey);
  const versionedEncryptedKey = new Uint8Array(1 + encryptedKey.length);
  versionedEncryptedKey[0] = 0;
  versionedEncryptedKey.set(encryptedKey, 1);
  const payload = encryptWithDataKey(metadata, sessionKey);

  try {
    const response = await axios.post<{ session: RawSession }>(
      `${config.serverUrl}/v1/sessions`,
      {
        tag,
        metadata: encodeBase64(payload),
        dataEncryptionKey: encodeBase64(versionedEncryptedKey),
      },
      {
        headers: authHeaders(creds),
      },
    );
    return decryptSession(response.data.session, creds);
  } catch (error) {
    throw formatApiError(error, 'create');
  }
}
