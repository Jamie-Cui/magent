import type { SessionEnvelope } from '@slopus/happy-wire';

export type EncryptionVariant = 'legacy' | 'dataKey';

export type SessionEncryption = {
  key: Uint8Array;
  variant: EncryptionVariant;
};

export type Credentials = {
  token: string;
  secret: Uint8Array;
  contentKeyPair: {
    publicKey: Uint8Array;
    secretKey: Uint8Array;
  };
};

export type RawSession = {
  id: string;
  seq: number;
  createdAt: number;
  updatedAt: number;
  active: boolean;
  activeAt: number;
  metadata: string;
  metadataVersion: number;
  agentState: string | null;
  agentStateVersion: number;
  dataEncryptionKey: string | null;
};

export type DecryptedSession = {
  id: string;
  seq: number;
  createdAt: number;
  updatedAt: number;
  active: boolean;
  activeAt: number;
  metadata: Record<string, unknown>;
  agentState: Record<string, unknown> | null;
  encryption: SessionEncryption;
};

export type HelperConfig = {
  serverUrl: string;
  homeDir: string;
  credentialPath: string;
  projectPath: string;
  debug: boolean;
};

export type PendingApprovalState = {
  tool: string;
  arguments: unknown;
  summary?: string;
  createdAt: number;
  requestId: string;
};

export type CompletedApprovalState = PendingApprovalState & {
  completedAt: number;
  status: 'approved' | 'denied' | 'canceled';
  decision: 'approved' | 'approved_for_session' | 'denied' | 'abort';
};

export type HappyAgentState = {
  backend: 'magent';
  controlledByUser: boolean;
  requests: Record<string, PendingApprovalState>;
  completedRequests: Record<string, CompletedApprovalState>;
  updatedAt: number;
};

export type HappySessionMetadata = {
  tag: string;
  path: string;
  host: string;
  name: string;
  version: string;
  homeDir: string;
  happyHomeDir: string;
  happyLibDir: string;
  happyToolsDir: string;
  lifecycleState: 'running';
  lifecycleStateSince: number;
  flavor: 'magent';
  startedBy: 'terminal';
};

export type Decision = 'allow-once' | 'allow-session' | 'deny-once' | 'deny-session';

export type ApprovalRpcRequest = {
  id: string;
  approved: boolean;
  decision?: 'approved' | 'approved_for_session' | 'denied' | 'abort';
};

export type HelperInput =
  | { type: 'create-session'; tag: string }
  | { type: 'attach-session'; session_id: string }
  | { type: 'disconnect'; session_id?: string }
  | { type: 'event'; session_id?: string; envelope: SessionEnvelope }
  | {
      type: 'approval-request';
      session_id?: string;
      request_id: string;
      tool_name: string;
      perm_key?: string;
      summary?: string;
      args?: unknown;
    }
  | {
      type: 'agent-state';
      session_id?: string;
      controlled_by_user: boolean;
      requests: number;
      completed_requests: number;
    }
  | { type: 'ready'; session_id?: string };

export type HelperOutput =
  | { type: 'connected'; session_id: string }
  | { type: 'session-attached'; session_id: string }
  | { type: 'user-message'; text: string }
  | { type: 'approval-response'; request_id: string; decision: Decision }
  | { type: 'abort' }
  | { type: 'error'; message: string };

export type OutgoingUserMessage = {
  role: 'user';
  content: {
    type: 'text';
    text: string;
  };
  meta?: {
    sentFrom?: string;
    [key: string]: unknown;
  };
};

export type OutgoingSessionMessage = {
  role: 'session';
  content: SessionEnvelope;
  meta?: {
    sentFrom?: string;
    [key: string]: unknown;
  };
};
