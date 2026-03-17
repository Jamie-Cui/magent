#!/usr/bin/env node

import readline from 'node:readline';
import { parseInputLine, writeError } from './protocol.js';
import { SessionBridge } from './sessionBridge.js';

async function main(): Promise<void> {
  const bridge = new SessionBridge();
  const rl = readline.createInterface({
    input: process.stdin,
    crlfDelay: Infinity,
  });

  for await (const line of rl) {
    if (!line.trim()) {
      continue;
    }
    try {
      await bridge.handle(parseInputLine(line));
    } catch (error) {
      writeError(error instanceof Error ? error.message : String(error));
    }
  }
}

main().catch((error) => {
  writeError(error instanceof Error ? error.message : String(error));
  process.exitCode = 1;
});
