import { homedir, hostname } from 'node:os';
import { join } from 'node:path';
import type { HelperConfig } from './types.js';

export function loadConfig(): HelperConfig {
  const homeDir = process.env.HAPPY_HOME_DIR ?? join(homedir(), '.happy');
  return {
    serverUrl: (process.env.HAPPY_SERVER_URL ?? 'https://api.cluster-fluster.com').replace(/\/+$/, ''),
    homeDir,
    credentialPath: join(homeDir, 'agent.key'),
    projectPath: process.env.MAGENT_HAPPY_PROJECT_PATH ?? process.cwd(),
    debug: process.env.MAGENT_HAPPY_DEBUG === '1',
  };
}

export function buildSessionMetadata(tag: string, config: HelperConfig) {
  const now = Date.now();
  return {
    tag,
    path: config.projectPath,
    host: hostname(),
    name: 'Magent',
    version: '0.1.0',
    homeDir: homedir(),
    happyHomeDir: config.homeDir,
    happyLibDir: '',
    happyToolsDir: '',
    lifecycleState: 'running' as const,
    lifecycleStateSince: now,
    flavor: 'magent' as const,
    startedBy: 'terminal' as const,
  };
}
