import { z } from 'zod';
import { sessionEnvelopeSchema } from '@slopus/happy-wire';
import type { HelperInput, HelperOutput } from './types.js';

const helperInputSchema = z.discriminatedUnion('type', [
  z.object({
    type: z.literal('create-session'),
    tag: z.string().min(1),
  }),
  z.object({
    type: z.literal('attach-session'),
    session_id: z.string().min(1),
  }),
  z.object({
    type: z.literal('disconnect'),
    session_id: z.string().optional(),
  }),
  z.object({
    type: z.literal('event'),
    session_id: z.string().optional(),
    envelope: sessionEnvelopeSchema,
  }),
  z.object({
    type: z.literal('approval-request'),
    session_id: z.string().optional(),
    request_id: z.string().min(1),
    tool_name: z.string().min(1),
    perm_key: z.string().optional(),
    summary: z.string().optional(),
    args: z.unknown().optional(),
  }),
  z.object({
    type: z.literal('agent-state'),
    session_id: z.string().optional(),
    controlled_by_user: z.boolean(),
    requests: z.number().int().nonnegative(),
    completed_requests: z.number().int().nonnegative(),
  }),
  z.object({
    type: z.literal('ready'),
    session_id: z.string().optional(),
  }),
]);

export function parseInputLine(line: string): HelperInput {
  return helperInputSchema.parse(JSON.parse(line)) as HelperInput;
}

export function encodeOutput(message: HelperOutput): string {
  return `${JSON.stringify(message)}\n`;
}

export function writeOutput(message: HelperOutput): void {
  process.stdout.write(encodeOutput(message));
}

export function writeError(message: string): void {
  writeOutput({ type: 'error', message });
}
