// Real-client MCP conformance check using the official @modelcontextprotocol/sdk.
//
// Driven by arizona_mcp_conformance_SUITE, which boots an Arizona MCP server
// and runs `node test/conformance/mcp_client.mjs <server-url>`. Prints a final
// `RESULT: PASS | FAIL ... | SKIP ...` line and exits 0 (PASS/SKIP) or 1 (FAIL).
//
// This guards the class of interop bugs the Erlang wire tests cannot catch:
// capability negotiation, session-id threading, and response shapes as the
// real client implementation (the one Claude Code / Cursor use) expects them.

const url = process.argv[2];
if (!url) {
  console.log('RESULT: FAIL missing server-url argument');
  process.exit(1);
}

let Client;
let StreamableHTTPClientTransport;
try {
  ({ Client } = await import('@modelcontextprotocol/sdk/client/index.js'));
  ({ StreamableHTTPClientTransport } = await import(
    '@modelcontextprotocol/sdk/client/streamableHttp.js'
  ));
} catch (e) {
  console.log(`RESULT: SKIP @modelcontextprotocol/sdk unavailable (${e.message})`);
  process.exit(0);
}

const failures = [];
const check = (label, ok) => {
  console.log(`  ${ok ? 'PASS' : 'FAIL'}  ${label}`);
  if (!ok) failures.push(label);
};

const client = new Client({ name: 'arizona-conformance', version: '1.0.0' });
const transport = new StreamableHTTPClientTransport(new URL(url));

try {
  await client.connect(transport);
  check('session id assigned', Boolean(transport.sessionId));
  check('advertises tools capability', 'tools' in (client.getServerCapabilities() ?? {}));

  const tools = await client.listTools();
  check("tools/list has 'add'", tools.tools.some((t) => t.name === 'add'));

  const call = await client.callTool({ name: 'add', arguments: { a: 2, b: 3 } });
  check("tools/call add => '5'", call.content?.[0]?.text === '5');
  check('tools/call isError false', call.isError === false);

  const resources = await client.listResources();
  check(
    'resources/list has mem://greeting',
    resources.resources.some((r) => r.uri === 'mem://greeting'),
  );

  const read = await client.readResource({ uri: 'mem://greeting' });
  check("resources/read => 'hello'", read.contents?.[0]?.text === 'hello');

  const prompts = await client.listPrompts();
  check("prompts/list has 'greet'", prompts.prompts.some((p) => p.name === 'greet'));

  const prompt = await client.getPrompt({ name: 'greet', arguments: { who: 'Ada' } });
  check("prompts/get => 'Hello, Ada'", JSON.stringify(prompt).includes('Hello, Ada'));

  await client.close();
} catch (e) {
  console.log(`  ERROR  ${e?.message ?? e}`);
  failures.push(`exception: ${e?.message ?? e}`);
}

if (failures.length === 0) {
  console.log('RESULT: PASS');
  process.exit(0);
}
console.log(`RESULT: FAIL ${failures.length} (${failures.join('; ')})`);
process.exit(1);
