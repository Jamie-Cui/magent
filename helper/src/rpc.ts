import { decodeBase64, decrypt, encodeBase64, encrypt } from './crypto.js';
import type { EncryptionVariant } from './types.js';

export type RpcRequest = {
  method: string;
  params: string;
};

export type RpcHandler<TRequest = unknown, TResponse = unknown> = (request: TRequest) => Promise<TResponse> | TResponse;

export class RpcServer {
  private readonly scopePrefix: string;
  private readonly key: Uint8Array;
  private readonly variant: EncryptionVariant;
  private readonly handlers = new Map<string, RpcHandler>();

  constructor(scopePrefix: string, key: Uint8Array, variant: EncryptionVariant) {
    this.scopePrefix = scopePrefix;
    this.key = key;
    this.variant = variant;
  }

  register<TRequest = unknown, TResponse = unknown>(method: string, handler: RpcHandler<TRequest, TResponse>): string {
    const scoped = `${this.scopePrefix}:${method}`;
    this.handlers.set(scoped, handler as RpcHandler);
    return scoped;
  }

  methods(): string[] {
    return Array.from(this.handlers.keys());
  }

  async handle(request: RpcRequest): Promise<string> {
    const handler = this.handlers.get(request.method);
    if (!handler) {
      return encodeBase64(encrypt(this.key, this.variant, { error: 'Method not found' }));
    }
    const params = decrypt(this.key, this.variant, decodeBase64(request.params));
    try {
      const result = await handler(params);
      return encodeBase64(encrypt(this.key, this.variant, result));
    } catch (error) {
      return encodeBase64(
        encrypt(this.key, this.variant, {
          error: error instanceof Error ? error.message : String(error),
        }),
      );
    }
  }
}
