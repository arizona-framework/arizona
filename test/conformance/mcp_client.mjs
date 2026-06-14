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

// Follow nextCursor to aggregate every page of a list method. Exercises the
// cursor protocol and makes the list assertions independent of page order.
const listAll = async (listFn, key) => {
  const all = [];
  let cursor;
  do {
    const page = await listFn(cursor ? { cursor } : {});
    all.push(...page[key]);
    cursor = page.nextCursor;
  } while (cursor);
  return all;
};

const client = new Client({ name: 'arizona-conformance', version: '1.0.0' });
const transport = new StreamableHTTPClientTransport(new URL(url));

try {
  await client.connect(transport);
  check('session id assigned', Boolean(transport.sessionId));
  check('advertises tools capability', 'tools' in (client.getServerCapabilities() ?? {}));

  // The server uses a small page_size, so each list spans several pages.
  // Aggregating across pages keeps these checks order-independent and proves
  // the cursor protocol round-trips (reaching the last-page tool, 'progress').
  const tools = await listAll((p) => client.listTools(p), 'tools');
  check("tools/list has 'add'", tools.some((t) => t.name === 'add'));
  check('tools/list pagination reaches last page', tools.some((t) => t.name === 'progress'));

  const call = await client.callTool({ name: 'add', arguments: { a: 2, b: 3 } });
  check("tools/call add => '5'", call.content?.[0]?.text === '5');
  check('tools/call isError false', call.isError === false);

  // A token-bearing tools/call streams notifications/progress before the
  // result. Passing onProgress makes the SDK attach a progressToken and the
  // server answer over SSE; the callback fires per progress notification.
  const seen = [];
  const streamed = await client.callTool(
    { name: 'progress', arguments: {} },
    undefined,
    { onprogress: (p) => seen.push(p.progress) },
  );
  check('tools/call progress streamed', seen.length >= 1);
  check("tools/call streamed result => 'done'", streamed.content?.[0]?.text === 'done');

  const resources = await listAll((p) => client.listResources(p), 'resources');
  check('resources/list has mem://greeting', resources.some((r) => r.uri === 'mem://greeting'));

  const read = await client.readResource({ uri: 'mem://greeting' });
  check("resources/read => 'hello'", read.contents?.[0]?.text === 'hello');

  const templates = await client.listResourceTemplates();
  check(
    'resources/templates/list has user',
    templates.resourceTemplates?.some((t) => t.uriTemplate === 'mem://user/{id}'),
  );

  // subscribe/unsubscribe round-trip (delivery itself is covered by the Erlang
  // suites, where the server can publish an update mid-test); a throw here fails.
  await client.subscribeResource({ uri: 'mem://greeting' });
  await client.unsubscribeResource({ uri: 'mem://greeting' });
  check('resources subscribe/unsubscribe round-trip', true);

  const prompts = await listAll((p) => client.listPrompts(p), 'prompts');
  check("prompts/list has 'greet'", prompts.some((p) => p.name === 'greet'));

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
