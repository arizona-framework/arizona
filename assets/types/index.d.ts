// Main Arizona Framework TypeScript Definitions

export interface ArizonaClientOptions {
  logLevel?: 'silent' | 'error' | 'warning' | 'info' | 'debug';
}

export interface ConnectOptions {
  wsPath?: string;
}

export interface EventParams {
  [key: string]: any;
}

export interface WorkerMessage {
  type: 'connect' | 'send' | 'disconnect' | 'connected' | 'disconnected' | 'message' | 'error';
  data?: any;
}

export interface HierarchicalComponent {
  id: string;
  static: [string, string];
  dynamic?: any;
  children?: HierarchicalComponent[];
}

export interface RenderPatch {
  html: string;
  patches: any[];
}

export default class ArizonaClient {
  worker: Worker | null;
  connected: boolean;
  hierarchical: ArizonaHierarchical;
  logLevel: number;

  constructor(opts?: ArizonaClientOptions);
  connect(opts?: ConnectOptions): void;
  sendEvent(event: string, params?: EventParams): void;
  disconnect(): void;
  private handleWorkerMessage(message: WorkerMessage): void;
  private log(level: string, message: string, ...args: any[]): void;
}

export class ArizonaHierarchical {
  private components: Map<string, HierarchicalComponent>;

  constructor();
  registerComponent(component: HierarchicalComponent): void;
  render(componentId: string, data?: any): string;
  applyPatch(patch: RenderPatch): void;
  private renderComponent(component: HierarchicalComponent, data?: any): string;
}

export function sanitizeForLog(value: any): string;

export as namespace Arizona;