import { beforeEach, describe, expect, it, vi } from 'vitest';

const closeMock = vi.fn(async () => undefined);
const startKeepAliveMock = vi.fn();
const updateAgentStateMock = vi.fn(async () => undefined);
const sendSessionEnvelopeMock = vi.fn(async () => undefined);

const writeOutputMock = vi.fn();
const createSessionMock = vi.fn();
const resolveSessionByIdMock = vi.fn();

class MockSessionSocketClient {
  public readonly sessionId: string;

  constructor(
    _config: unknown,
    _creds: unknown,
    session: { id: string },
  ) {
    this.sessionId = session.id;
  }

  on(): this {
    return this;
  }

  startKeepAlive(controlledByUser: boolean): void {
    startKeepAliveMock(controlledByUser);
  }

  async updateAgentState(state: unknown): Promise<void> {
    updateAgentStateMock(state);
  }

  async sendSessionEnvelope(envelope: unknown): Promise<void> {
    sendSessionEnvelopeMock(envelope);
  }

  async close(): Promise<void> {
    await closeMock();
  }
}

vi.mock('../src/config.js', () => ({
  loadConfig: () => ({ serverUrl: 'https://example.test' }),
  buildSessionMetadata: () => ({ tag: 'demo' }),
}));

vi.mock('../src/credentials.js', () => ({
  readCredentials: () => ({ token: 'token' }),
}));

vi.mock('../src/api.js', () => ({
  createSession: createSessionMock,
  resolveSessionById: resolveSessionByIdMock,
}));

vi.mock('../src/protocol.js', () => ({
  writeOutput: writeOutputMock,
}));

vi.mock('../src/socket.js', () => ({
  SessionSocketClient: MockSessionSocketClient,
}));

describe('SessionBridge', () => {
  beforeEach(() => {
    closeMock.mockClear();
    startKeepAliveMock.mockClear();
    updateAgentStateMock.mockClear();
    sendSessionEnvelopeMock.mockClear();
    writeOutputMock.mockClear();
    createSessionMock.mockReset();
    resolveSessionByIdMock.mockReset();
  });

  it('closes the previous socket before attaching another session', async () => {
    createSessionMock.mockResolvedValueOnce({ id: 'session-1' });
    resolveSessionByIdMock.mockResolvedValueOnce({ id: 'session-2' });

    const { SessionBridge } = await import('../src/sessionBridge.js');
    const bridge = new SessionBridge();

    await bridge.handle({ type: 'create-session', tag: 'demo' });
    await bridge.handle({ type: 'attach-session', session_id: 'session-2' });

    expect(closeMock).toHaveBeenCalledTimes(1);
    expect(writeOutputMock).toHaveBeenNthCalledWith(1, {
      type: 'connected',
      session_id: 'session-1',
    });
    expect(writeOutputMock).toHaveBeenNthCalledWith(2, {
      type: 'session-attached',
      session_id: 'session-2',
    });
  });
});
