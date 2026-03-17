import { buildSessionMetadata, loadConfig } from './config.js';
import { readCredentials } from './credentials.js';
import { createSession, resolveSessionById } from './api.js';
import { writeOutput } from './protocol.js';
import { SessionSocketClient } from './socket.js';
import type {
  ApprovalRpcRequest,
  CompletedApprovalState,
  Decision,
  HelperInput,
  HappyAgentState,
  PendingApprovalState,
} from './types.js';

function decisionFromRpc(request: ApprovalRpcRequest): Decision {
  if (request.approved) {
    return request.decision === 'approved_for_session' ? 'allow-session' : 'allow-once';
  }
  return request.decision === 'abort' ? 'deny-once' : 'deny-once';
}

export class SessionBridge {
  private readonly config = loadConfig();
  private readonly creds = readCredentials(this.config);
  private socketClient: SessionSocketClient | null = null;
  private sessionId: string | null = null;
  private state: HappyAgentState = {
    backend: 'magent',
    controlledByUser: true,
    requests: {},
    completedRequests: {},
    updatedAt: Date.now(),
  };

  private resetState(controlledByUser: boolean) {
    this.state = {
      backend: 'magent',
      controlledByUser,
      requests: {},
      completedRequests: {},
      updatedAt: Date.now(),
    };
  }

  private attachSocket(client: SessionSocketClient) {
    this.socketClient = client;
    client.on('userMessage', ({ text }) => {
      writeOutput({ type: 'user-message', text });
    });
    client.on('abort', () => {
      writeOutput({ type: 'abort' });
    });
    client.on('approval', (payload) => {
      this.resolveApproval(payload);
    });
    client.on('error', (error) => {
      writeOutput({ type: 'error', message: error.message });
    });
    client.startKeepAlive(this.state.controlledByUser);
  }

  private async replaceSocket(sessionId: string, client: SessionSocketClient) {
    if (this.socketClient) {
      await this.socketClient.close();
    }
    this.sessionId = sessionId;
    this.resetState(true);
    this.attachSocket(client);
  }

  private async setState(partial: Partial<HappyAgentState>) {
    this.state = {
      ...this.state,
      ...partial,
      updatedAt: Date.now(),
    };
    if (this.socketClient) {
      await this.socketClient.updateAgentState(this.state);
      this.socketClient.startKeepAlive(this.state.controlledByUser);
    }
  }

  private resolveApproval(payload: ApprovalRpcRequest) {
    const pending = this.state.requests[payload.id];
    if (!pending) {
      return;
    }
    const decision = decisionFromRpc(payload);
    const completed: CompletedApprovalState = {
      ...pending,
      completedAt: Date.now(),
      status: payload.approved ? 'approved' : 'denied',
      decision: payload.decision ?? (payload.approved ? 'approved' : 'denied'),
    };
    delete this.state.requests[payload.id];
    this.state.completedRequests[payload.id] = completed;
    void this.setState({
      requests: this.state.requests,
      completedRequests: this.state.completedRequests,
    });
    writeOutput({
      type: 'approval-response',
      request_id: payload.id,
      decision,
    });
  }

  private requireSocket(): SessionSocketClient {
    if (!this.socketClient || !this.sessionId) {
      throw new Error('No Happy session connected');
    }
    return this.socketClient;
  }

  async handle(message: HelperInput): Promise<void> {
    switch (message.type) {
      case 'create-session': {
        const session = await createSession(
          this.config,
          this.creds,
          message.tag,
          buildSessionMetadata(message.tag, this.config),
        );
        await this.replaceSocket(
          session.id,
          new SessionSocketClient(this.config, this.creds, session),
        );
        await this.setState({ controlledByUser: true });
        writeOutput({ type: 'connected', session_id: session.id });
        break;
      }
      case 'attach-session': {
        const session = await resolveSessionById(this.config, this.creds, message.session_id);
        await this.replaceSocket(
          session.id,
          new SessionSocketClient(this.config, this.creds, session),
        );
        await this.setState({ controlledByUser: true });
        writeOutput({ type: 'session-attached', session_id: session.id });
        break;
      }
      case 'disconnect': {
        if (this.socketClient) {
          await this.socketClient.close();
        }
        this.socketClient = null;
        this.sessionId = null;
        this.resetState(false);
        break;
      }
      case 'event': {
        const client = this.requireSocket();
        await client.sendSessionEnvelope(message.envelope);
        break;
      }
      case 'approval-request': {
        const request: PendingApprovalState = {
          tool: message.tool_name,
          arguments: message.args,
          summary: message.summary,
          createdAt: Date.now(),
          requestId: message.request_id,
        };
        this.state.requests[message.request_id] = request;
        await this.setState({
          controlledByUser: true,
          requests: this.state.requests,
        });
        break;
      }
      case 'agent-state': {
        await this.setState({
          controlledByUser: message.controlled_by_user,
        });
        break;
      }
      case 'ready': {
        await this.setState({
          controlledByUser: false,
        });
        break;
      }
      default: {
        const exhaustive: never = message;
        throw new Error(`Unsupported message ${(exhaustive as { type?: string }).type ?? 'unknown'}`);
      }
    }
  }
}
