import { EventEmitter } from 'node:events';
import { io, type Socket } from 'socket.io-client';
import { decodeBase64, decrypt, encodeBase64, encrypt } from './crypto.js';
import { RpcServer, type RpcRequest } from './rpc.js';
import type {
  Credentials,
  DecryptedSession,
  HappyAgentState,
  HelperConfig,
  OutgoingSessionMessage,
  SessionEncryption,
} from './types.js';

type UpdateMessage = {
  body?: {
    t?: string;
    message?: {
      id: string;
      seq: number;
      localId?: string | null;
      content?: { t: 'encrypted'; c: string };
      createdAt: number;
      updatedAt: number;
    };
    metadata?: { version: number; value: string };
    agentState?: { version: number; value: string | null };
  };
};

export type SessionSocketClientEvents = {
  userMessage: (payload: { text: string }) => void;
  abort: () => void;
  approval: (payload: { id: string; approved: boolean; decision?: 'approved' | 'approved_for_session' | 'denied' | 'abort' }) => void;
  error: (error: Error) => void;
};

export class SessionSocketClient extends EventEmitter {
  readonly sessionId: string;
  readonly encryption: SessionEncryption;
  private readonly socket: Socket;
  private readonly rpc: RpcServer;
  private keepAliveTimer: NodeJS.Timeout | null = null;
  private metadataVersion: number;
  private agentStateVersion: number;

  constructor(config: HelperConfig, creds: Credentials, session: DecryptedSession) {
    super();
    this.sessionId = session.id;
    this.encryption = session.encryption;
    this.metadataVersion = 0;
    this.agentStateVersion = 0;
    this.rpc = new RpcServer(session.id, session.encryption.key, session.encryption.variant);

    this.rpc.register('abort', async () => {
      this.emit('abort');
      return { ok: true };
    });
    this.rpc.register('permission', async (request) => {
      const payload = request as { id?: string; approved?: boolean; decision?: 'approved' | 'approved_for_session' | 'denied' | 'abort' };
      if (!payload.id || typeof payload.approved !== 'boolean') {
        throw new Error('Malformed permission RPC payload');
      }
      this.emit('approval', {
        id: payload.id,
        approved: payload.approved,
        decision: payload.decision,
      });
      return { ok: true };
    });

    this.socket = io(config.serverUrl, {
      auth: {
        token: creds.token,
        clientType: 'session-scoped' as const,
        sessionId: session.id,
      },
      path: '/v1/updates',
      reconnection: true,
      reconnectionAttempts: Infinity,
      reconnectionDelay: 1000,
      reconnectionDelayMax: 5000,
      transports: ['websocket'],
      autoConnect: false,
    });

    this.socket.on('connect', () => {
      for (const method of this.rpc.methods()) {
        this.socket.emit('rpc-register', { method });
      }
    });
    this.socket.on('update', (update: UpdateMessage) => this.handleUpdate(update));
    this.socket.on('rpc-request', async (request: RpcRequest, callback: (response: string) => void) => {
      callback(await this.rpc.handle(request));
    });
    this.socket.on('connect_error', (error: Error) => {
      this.emit('error', error);
    });
    this.socket.on('error', (error: { message?: string }) => {
      this.emit('error', new Error(error.message ?? 'Socket error'));
    });
    this.socket.on('disconnect', (reason: string) => {
      if (reason !== 'io client disconnect') {
        this.emit('error', new Error(`Socket disconnected: ${reason}`));
      }
    });
    this.socket.connect();
  }

  private decodePayload(content: { t: 'encrypted'; c: string }) {
    return decrypt(this.encryption.key, this.encryption.variant, decodeBase64(content.c));
  }

  private handleUpdate(update: UpdateMessage) {
    const body = update.body;
    if (!body) {
      return;
    }
    if (body.t === 'new-message' && body.message?.content?.t === 'encrypted') {
      const payload = this.decodePayload(body.message.content) as
        | { role?: string; content?: { type?: string; text?: string } }
        | null;
      if (payload?.role === 'user' && payload.content?.type === 'text' && typeof payload.content.text === 'string') {
        this.emit('userMessage', { text: payload.content.text });
      }
      return;
    }
    if (body.t === 'update-session') {
      if (body.metadata && body.metadata.version > this.metadataVersion) {
        this.metadataVersion = body.metadata.version;
      }
      if (body.agentState && body.agentState.version > this.agentStateVersion) {
        this.agentStateVersion = body.agentState.version;
      }
    }
  }

  async sendSessionEnvelope(envelope: OutgoingSessionMessage['content']): Promise<void> {
    const content: OutgoingSessionMessage = {
      role: 'session',
      content: envelope,
      meta: {
        sentFrom: 'magent-helper',
      },
    };
    const encrypted = encodeBase64(encrypt(this.encryption.key, this.encryption.variant, content));
    this.socket.emit('message', {
      sid: this.sessionId,
      message: encrypted,
    });
  }

  async updateAgentState(state: HappyAgentState): Promise<void> {
    const encrypted = encodeBase64(encrypt(this.encryption.key, this.encryption.variant, state));
    const response = await this.socket.emitWithAck('update-state', {
      sid: this.sessionId,
      expectedVersion: this.agentStateVersion,
      agentState: encrypted,
    });
    if (response && typeof response.version === 'number') {
      this.agentStateVersion = response.version;
    }
  }

  startKeepAlive(controlledByUser: boolean): void {
    this.stopKeepAlive();
    this.keepAliveTimer = setInterval(() => {
      this.socket.volatile.emit('session-alive', {
        sid: this.sessionId,
        time: Date.now(),
        thinking: controlledByUser,
        mode: 'remote',
      });
    }, 2000);
  }

  stopKeepAlive(): void {
    if (this.keepAliveTimer) {
      clearInterval(this.keepAliveTimer);
      this.keepAliveTimer = null;
    }
  }

  async close(): Promise<void> {
    this.stopKeepAlive();
    this.socket.emit('session-end', {
      sid: this.sessionId,
      time: Date.now(),
    });
    this.socket.close();
  }
}
