import { expect, test } from '@playwright/test';
import { expectStaysConnected } from '../utils/helpers.js';

// ---------------------------------------------------------------------------
// Helpers -- shared
// ---------------------------------------------------------------------------

/** Wait for the WebSocket to be open so events can be dispatched. */
const wsReady = (page) =>
    page.waitForFunction(() => document.documentElement.classList.contains('az-connected'));

// ---------------------------------------------------------------------------
// Helpers -- counter
// ---------------------------------------------------------------------------

/** Locate a counter's count <p> by its view id. */
const countText = (page, viewId) => page.locator(`#${viewId} p[az$="-3"]`);

/** Locate the inc/dec button inside a specific counter view. */
const incBtn = (page, viewId) => page.locator(`#${viewId} button[az-click*='"inc"']`);
const decBtn = (page, viewId) => page.locator(`#${viewId} button[az-click*='"dec"']`);

// ---------------------------------------------------------------------------
// Helpers -- stream (todos)
// ---------------------------------------------------------------------------

const todoList = (page) => page.locator('#page ul[az-drop]');

const todoItems = (page) => page.locator('#page ul[az-drop] > [az-key]');

const todoItem = (page, key) => page.locator(`#page ul[az-drop] > [az-key="${key}"]`);

const todoInput = (page, key) => page.locator(`#page ul[az-drop] > [az-key="${key}"] input`);

const addTodoBtn = (page) => page.locator('form[az-submit*="add_todo"] button[type="submit"]');

const clearBtn = (page) => page.locator('button[az-click*="clear_todos"]');

const removeBtn = (page, key) => page.locator(`[az-key="${key}"] button[az-click*="remove_todo"]`);

const newTodoInput = (page) => page.locator('form[az-submit*="add_todo"] input[name="text"]');

// ---------------------------------------------------------------------------
// 1. SSR -- initial HTML rendered server-side
// ---------------------------------------------------------------------------

test.describe('SSR', () => {
    test('page title renders', async ({ page }) => {
        await page.goto('/');
        const h1 = page.locator('main#page[az-view] h1[az]');
        await expect(h1).toHaveText('Welcome');
    });

    test('counter starts at 0', async ({ page }) => {
        await page.goto('/');
        await expect(countText(page, 'counter')).toHaveText('Count: 0');
    });

    test('counter2 starts at 0', async ({ page }) => {
        await page.goto('/');
        await expect(countText(page, 'counter2')).toHaveText('Count: 0');
    });

    test('counter3 starts at 42', async ({ page }) => {
        await page.goto('/');
        await expect(countText(page, 'counter3')).toHaveText('Count: 42');
    });

    test('counter ids are set correctly', async ({ page }) => {
        await page.goto('/');
        await expect(page.locator('#counter[az-view]')).toBeVisible();
        await expect(page.locator('#counter2[az-view]')).toBeVisible();
        await expect(page.locator('#counter3[az-view]')).toBeVisible();
    });

    test('theme class is applied', async ({ page }) => {
        await page.goto('/');
        const wrapper = page.locator('div:has(> #counter[az-view])');
        await expect(wrapper).toHaveAttribute('class', 'light');
    });

    test('all three counter views exist', async ({ page }) => {
        await page.goto('/');
        await expect(page.locator('#counter[az-view]')).toBeVisible();
        await expect(page.locator('#counter2[az-view]')).toBeVisible();
        await expect(page.locator('#counter3[az-view]')).toBeVisible();
    });

    test('stream list is empty on initial load', async ({ page }) => {
        await page.goto('/');
        await expect(todoList(page)).toBeAttached();
        await expect(todoItems(page)).toHaveCount(0);
    });
});

// ---------------------------------------------------------------------------
// 2. Single counter -- inc / dec via child events
// ---------------------------------------------------------------------------

test.describe('counter child events', () => {
    test('increment counter', async ({ page }) => {
        await page.goto('/');
        await wsReady(page);

        await incBtn(page, 'counter').click();
        await expect(countText(page, 'counter')).toHaveText('Count: 1');
    });

    test('decrement counter', async ({ page }) => {
        await page.goto('/');
        await wsReady(page);

        await decBtn(page, 'counter').click();
        await expect(countText(page, 'counter')).toHaveText('Count: -1');
    });

    test('multiple increments and decrements', async ({ page }) => {
        await page.goto('/');
        await wsReady(page);

        await incBtn(page, 'counter').click();
        await expect(countText(page, 'counter')).toHaveText('Count: 1');

        await incBtn(page, 'counter').click();
        await expect(countText(page, 'counter')).toHaveText('Count: 2');

        await decBtn(page, 'counter').click();
        await expect(countText(page, 'counter')).toHaveText('Count: 1');
    });

    test('counter2 increments independently', async ({ page }) => {
        await page.goto('/');
        await wsReady(page);

        await incBtn(page, 'counter2').click();
        await expect(countText(page, 'counter2')).toHaveText('Count: 1');

        // counter stays at 0
        await expect(countText(page, 'counter')).toHaveText('Count: 0');
    });

    test('counter3 increments independently from 42', async ({ page }) => {
        await page.goto('/');
        await wsReady(page);

        await incBtn(page, 'counter3').click();
        await expect(countText(page, 'counter3')).toHaveText('Count: 43');
    });
});

// ---------------------------------------------------------------------------
// 3. Parent "Add +1 to both" -- propagates to counter and counter2
// ---------------------------------------------------------------------------

test.describe('parent add event', () => {
    test('add updates counter and counter2', async ({ page }) => {
        await page.goto('/');
        await wsReady(page);

        await page.click(`button[az-click*='"add"']`);

        // counter: default handle_update merges count=1
        await expect(countText(page, 'counter')).toHaveText('Count: 1');
        // counter2: custom handle_update doubles, count=1*2=2
        await expect(countText(page, 'counter2')).toHaveText('Count: 2');
    });

    test('add does not affect counter3 (hardcoded)', async ({ page }) => {
        await page.goto('/');
        await wsReady(page);

        await page.click(`button[az-click*='"add"']`);

        // counter3 has hardcoded count=42, unaffected by parent count change
        await expect(countText(page, 'counter3')).toHaveText('Count: 42');
    });

    test('multiple adds accumulate', async ({ page }) => {
        await page.goto('/');
        await wsReady(page);

        await page.click(`button[az-click*='"add"']`);
        await page.click(`button[az-click*='"add"']`);
        await page.click(`button[az-click*='"add"']`);

        // counter: default merge, count=3
        await expect(countText(page, 'counter')).toHaveText('Count: 3');
        // counter2: doubles parent count, count=3*2=6
        await expect(countText(page, 'counter2')).toHaveText('Count: 6');
        // counter3: still 42
        await expect(countText(page, 'counter3')).toHaveText('Count: 42');
    });
});

// ---------------------------------------------------------------------------
// 4. Child event then parent event -- handle_update overwrites child state
// ---------------------------------------------------------------------------

test.describe('child then parent interaction', () => {
    test('child increment overwritten by parent add', async ({ page }) => {
        await page.goto('/');
        await wsReady(page);

        // Increment counter to 3
        await incBtn(page, 'counter').click();
        await incBtn(page, 'counter').click();
        await incBtn(page, 'counter').click();
        await expect(countText(page, 'counter')).toHaveText('Count: 3');

        // Parent add: parent count 0->1, handle_update merges count=1
        // overwrites child's independent count=3
        await page.click(`button[az-click*='"add"']`);
        await expect(countText(page, 'counter')).toHaveText('Count: 1');
    });

    test('counter2 child increment overwritten by parent with doubling', async ({ page }) => {
        await page.goto('/');
        await wsReady(page);

        // Increment counter2 to 5
        for (let i = 0; i < 5; i++) {
            await incBtn(page, 'counter2').click();
        }
        await expect(countText(page, 'counter2')).toHaveText('Count: 5');

        // Parent add: parent count 0->1, counter2 handle_update doubles to 2
        await page.click(`button[az-click*='"add"']`);
        await expect(countText(page, 'counter2')).toHaveText('Count: 2');
    });
});

// ---------------------------------------------------------------------------
// 5. Comment markers -- static text preserved across updates
// ---------------------------------------------------------------------------

test.describe('comment markers', () => {
    test('static "Count: " preserved after update', async ({ page }) => {
        await page.goto('/');
        await wsReady(page);

        // Before: "Count: 0"
        const p = countText(page, 'counter');
        await expect(p).toHaveText('Count: 0');

        await incBtn(page, 'counter').click();

        // After: "Count: 1" -- the "Count: " prefix is static, not re-rendered
        await expect(p).toHaveText('Count: 1');

        // Verify the comment markers are present in the DOM
        const hasMarkers = await page.evaluate(() => {
            const p = document.querySelector('#counter p[az$="-3"]');
            const children = Array.from(p.childNodes);
            return (
                children.some(
                    (n) => n.nodeType === 8 && n.data.startsWith('az:') && n.data.endsWith('-3'),
                ) && children.some((n) => n.nodeType === 8 && n.data === '/az')
            );
        });
        expect(hasMarkers).toBe(true);
    });
});

// ---------------------------------------------------------------------------
// 6. Layout -- render-once page shell from mount opts
// ---------------------------------------------------------------------------

test.describe('layout', () => {
    test('page has DOCTYPE and html structure', async ({ page }) => {
        await page.goto('/');
        // Layout wraps content in full HTML document
        const html = await page.content();
        expect(html).toContain('<!DOCTYPE html>');
        expect(html).toContain('<head><meta charset="utf-8"><title>Welcome</title></head>');
    });

    test('arizona.js bootstrap script is present', async ({ page }) => {
        await page.goto('/');
        const scripts = page.locator('script[type="module"]');
        // Single inline module script that imports hooks+connect and calls it
        await expect(scripts).toHaveCount(1);
        const content = await scripts.first().textContent();
        expect(content).toContain("import { hooks, connect } from '/priv/arizona.min.js'");
    });
});

// ---------------------------------------------------------------------------
// 7. Effects -- push_event dispatches CustomEvent on document
// ---------------------------------------------------------------------------

test.describe('effects', () => {
    test('push_event dispatches CustomEvent on document', async ({ page }) => {
        await page.goto('/');
        await wsReady(page);

        // Increment counter to 3 first
        await incBtn(page, 'counter').click();
        await incBtn(page, 'counter').click();
        await incBtn(page, 'counter').click();
        await expect(countText(page, 'counter')).toHaveText('Count: 3');

        // Listen for the CustomEvent before sending the reset
        const eventPromise = page.evaluate(() => {
            return new Promise((resolve) => {
                document.addEventListener(
                    'counter_reset',
                    (e) => {
                        resolve(e.detail);
                    },
                    { once: true },
                );
            });
        });

        // Send reset event via ws (no reset button in template)
        await page.evaluate(() => {
            window._ws.send(JSON.stringify(['counter', 'reset', {}]));
        });

        // Verify the CustomEvent was dispatched with correct detail
        const detail = await eventPromise;
        expect(detail).toEqual({ id: 'counter' });

        // Verify the DOM was also updated (count reset to 0)
        await expect(countText(page, 'counter')).toHaveText('Count: 0');
    });

    test('empty effects do not dispatch events', async ({ page }) => {
        await page.goto('/');
        await wsReady(page);

        // Track any unexpected events
        await page.evaluate(() => {
            window._testEffectEvents = [];
            document.addEventListener('counter_reset', () => {
                window._testEffectEvents.push('counter_reset');
            });
        });

        // Regular inc event -- no effects
        await incBtn(page, 'counter').click();
        await expect(countText(page, 'counter')).toHaveText('Count: 1');

        // Check no effect events were dispatched
        const events = await page.evaluate(() => window._testEffectEvents);
        expect(events).toEqual([]);
    });

    test('effects-only response with no DOM changes', async ({ page }) => {
        await page.goto('/');
        await wsReady(page);

        // Counter starts at 0 -- reset at 0 produces no ops but fires effect
        await expect(countText(page, 'counter')).toHaveText('Count: 0');

        // Listen for the CustomEvent
        const eventPromise = page.evaluate(() => {
            return new Promise((resolve) => {
                document.addEventListener(
                    'counter_reset',
                    (e) => {
                        resolve(e.detail);
                    },
                    { once: true },
                );
            });
        });

        // Send reset when count is already 0
        await page.evaluate(() => {
            window._ws.send(JSON.stringify(['counter', 'reset', {}]));
        });

        // Effect still fires even though no DOM changed
        const detail = await eventPromise;
        expect(detail).toEqual({ id: 'counter' });

        // DOM unchanged -- still 0
        await expect(countText(page, 'counter')).toHaveText('Count: 0');
    });

    test('ops-only envelope omits "e" key', async ({ page }) => {
        await page.goto('/');
        await wsReady(page);

        // Intercept the next ws message to verify envelope format
        const messagePromise = page.evaluate(() => {
            return new Promise((resolve) => {
                const origOnmessage = window._ws.onmessage;
                window._ws.onmessage = (e) => {
                    resolve(JSON.parse(e.data));
                    window._ws.onmessage = origOnmessage;
                    origOnmessage(e);
                };
            });
        });

        // Regular inc -- produces ops but no effects
        await incBtn(page, 'counter').click();

        const msg = await messagePromise;
        expect(msg).toHaveProperty('o');
        expect(Array.isArray(msg.o)).toBe(true);
        // No effects → "e" key must be absent
        expect(msg).not.toHaveProperty('e');
    });

    test('ops+effects envelope includes both keys', async ({ page }) => {
        await page.goto('/');
        await wsReady(page);

        // Increment counter first so reset produces a diff
        await incBtn(page, 'counter').click();
        await expect(countText(page, 'counter')).toHaveText('Count: 1');

        const messagePromise = page.evaluate(() => {
            return new Promise((resolve) => {
                const origOnmessage = window._ws.onmessage;
                window._ws.onmessage = (e) => {
                    resolve(JSON.parse(e.data));
                    window._ws.onmessage = origOnmessage;
                    origOnmessage(e);
                };
            });
        });

        // Reset produces ops (count 1→0) AND effects (push_event)
        await page.evaluate(() => {
            window._ws.send(JSON.stringify(['counter', 'reset', {}]));
        });

        const msg = await messagePromise;
        expect(msg).toHaveProperty('o');
        expect(msg).toHaveProperty('e');
        expect(Array.isArray(msg.o)).toBe(true);
        expect(Array.isArray(msg.e)).toBe(true);
    });

    test('effects-only envelope omits "o" key', async ({ page }) => {
        await page.goto('/');
        await wsReady(page);

        // Counter starts at 0 -- reset at 0 produces no ops but fires effect
        const messagePromise = page.evaluate(() => {
            return new Promise((resolve) => {
                const origOnmessage = window._ws.onmessage;
                window._ws.onmessage = (e) => {
                    resolve(JSON.parse(e.data));
                    window._ws.onmessage = origOnmessage;
                    origOnmessage(e);
                };
            });
        });

        await page.evaluate(() => {
            window._ws.send(JSON.stringify(['counter', 'reset', {}]));
        });

        const msg = await messagePromise;
        // No ops → "o" key must be absent
        expect(msg).not.toHaveProperty('o');
        expect(msg).toHaveProperty('e');
        expect(Array.isArray(msg.e)).toBe(true);
    });
});

// ---------------------------------------------------------------------------
// 8. connected -- status changes on WebSocket connect
// ---------------------------------------------------------------------------

test.describe('connected', () => {
    test('SSR renders Connecting... initially', async ({ browser }) => {
        // Use a context with JS disabled so WS never connects
        const ctx = await browser.newContext({ javaScriptEnabled: false });
        const page = await ctx.newPage();
        await page.goto('/');
        await expect(page.locator('#status')).toHaveText('Connecting...');
        await ctx.close();
    });

    test('status changes to Connected after WS opens', async ({ page }) => {
        await page.goto('/');
        await expect(page.locator('#status')).toHaveText('Connected');
    });

    test('connected state does not affect todos', async ({ page }) => {
        await page.goto('/');
        await wsReady(page);

        // Status should be Connected after WS open
        await expect(page.locator('#status')).toHaveText('Connected');

        // Add a todo after connection
        await addTodoBtn(page).click();
        await expect(todoItem(page, '1')).toBeVisible();
        await expect(todoInput(page, '1')).toHaveValue('Todo 1');
        await expect(todoItems(page)).toHaveCount(1);
    });
});

// ---------------------------------------------------------------------------
// 9. OP_INSERT -- add todo items
// ---------------------------------------------------------------------------

test.describe('OP_INSERT -- add todo items', () => {
    test('add single todo item', async ({ page }) => {
        await page.goto('/');
        await wsReady(page);

        await addTodoBtn(page).click();
        await expect(todoItem(page, '1')).toBeVisible();
        await expect(todoInput(page, '1')).toHaveValue('Todo 1');
    });

    test('add multiple todo items', async ({ page }) => {
        await page.goto('/');
        await wsReady(page);

        await addTodoBtn(page).click();
        await expect(todoItems(page)).toHaveCount(1);

        await addTodoBtn(page).click();
        await expect(todoItems(page)).toHaveCount(2);

        await addTodoBtn(page).click();
        await expect(todoItems(page)).toHaveCount(3);

        await expect(todoInput(page, '1')).toHaveValue('Todo 1');
        await expect(todoInput(page, '2')).toHaveValue('Todo 2');
        await expect(todoInput(page, '3')).toHaveValue('Todo 3');

        // Verify DOM order
        const items = todoItems(page);
        await expect(items.nth(0)).toHaveAttribute('az-key', '1');
        await expect(items.nth(1)).toHaveAttribute('az-key', '2');
        await expect(items.nth(2)).toHaveAttribute('az-key', '3');
    });

    test('todo item has remove button', async ({ page }) => {
        await page.goto('/');
        await wsReady(page);

        await addTodoBtn(page).click();
        await expect(todoItem(page, '1')).toBeVisible();

        const btn = removeBtn(page, '1');
        await expect(btn).toBeVisible();
        await expect(btn).toHaveAttribute('az-click', /remove_todo/);
    });
});

// ---------------------------------------------------------------------------
// 10. OP_REMOVE -- remove todo items
// ---------------------------------------------------------------------------

test.describe('OP_REMOVE -- remove todo items', () => {
    test('remove single todo item', async ({ page }) => {
        await page.goto('/');
        await wsReady(page);

        // Add 2 items
        await addTodoBtn(page).click();
        await expect(todoItem(page, '1')).toBeVisible();
        await addTodoBtn(page).click();
        await expect(todoItem(page, '2')).toBeVisible();

        // Remove first
        await removeBtn(page, '1').click();
        await expect(todoItem(page, '1')).not.toBeAttached();
        await expect(todoItem(page, '2')).toBeVisible();
        await expect(todoItems(page)).toHaveCount(1);
    });

    test('remove last remaining item', async ({ page }) => {
        await page.goto('/');
        await wsReady(page);

        await addTodoBtn(page).click();
        await expect(todoItem(page, '1')).toBeVisible();

        await removeBtn(page, '1').click();
        await expect(todoItem(page, '1')).not.toBeAttached();
        await expect(todoItems(page)).toHaveCount(0);
    });

    test('remove middle item', async ({ page }) => {
        await page.goto('/');
        await wsReady(page);

        // Add 3 items
        await addTodoBtn(page).click();
        await expect(todoItem(page, '1')).toBeVisible();
        await addTodoBtn(page).click();
        await expect(todoItem(page, '2')).toBeVisible();
        await addTodoBtn(page).click();
        await expect(todoItem(page, '3')).toBeVisible();

        // Remove middle
        await removeBtn(page, '2').click();
        await expect(todoItem(page, '2')).not.toBeAttached();
        await expect(todoItem(page, '1')).toBeVisible();
        await expect(todoItem(page, '3')).toBeVisible();
        await expect(todoItems(page)).toHaveCount(2);
    });
});

// ---------------------------------------------------------------------------
// 11. Stream reset (clear all)
// ---------------------------------------------------------------------------

test.describe('stream reset -- clear all', () => {
    test('clear all todos', async ({ page }) => {
        await page.goto('/');
        await wsReady(page);

        // Add 3 items
        await addTodoBtn(page).click();
        await addTodoBtn(page).click();
        await addTodoBtn(page).click();
        await expect(todoItems(page)).toHaveCount(3);

        // Clear
        await clearBtn(page).click();
        await expect(todoItems(page)).toHaveCount(0);
    });

    test('add after clear', async ({ page }) => {
        await page.goto('/');
        await wsReady(page);

        // Add 2, clear, add 1
        await addTodoBtn(page).click();
        await addTodoBtn(page).click();
        await expect(todoItems(page)).toHaveCount(2);

        await clearBtn(page).click();
        await expect(todoItems(page)).toHaveCount(0);

        await addTodoBtn(page).click();
        await expect(todoItems(page)).toHaveCount(1);
        // next_id continues from 3
        await expect(todoInput(page, '3')).toHaveValue('Todo 3');
    });
});

// ---------------------------------------------------------------------------
// 12. OP_ITEM_PATCH -- update todo items
// ---------------------------------------------------------------------------

test.describe('OP_ITEM_PATCH -- update todo items', () => {
    test('update todo text', async ({ page }) => {
        await page.goto('/');
        await wsReady(page);

        // Add a todo
        await addTodoBtn(page).click();
        await expect(todoInput(page, '1')).toHaveValue('Todo 1');

        // Update via WS
        await page.evaluate(() => {
            window._ws.send(
                JSON.stringify(['page', 'update_todo', { id: 1, value: 'Updated Todo 1' }]),
            );
        });

        // Value changed in-place
        await expect(todoInput(page, '1')).toHaveValue('Updated Todo 1');
        // Item still in DOM
        await expect(todoItem(page, '1')).toBeVisible();
        await expect(todoItems(page)).toHaveCount(1);
    });

    test('update preserves other items', async ({ page }) => {
        await page.goto('/');
        await wsReady(page);

        // Add 3 items
        await addTodoBtn(page).click();
        await addTodoBtn(page).click();
        await addTodoBtn(page).click();
        await expect(todoItems(page)).toHaveCount(3);

        // Update middle item
        await page.evaluate(() => {
            window._ws.send(JSON.stringify(['page', 'update_todo', { id: 2, value: 'Changed' }]));
        });

        // Middle item updated
        await expect(todoInput(page, '2')).toHaveValue('Changed');
        // Others unchanged
        await expect(todoInput(page, '1')).toHaveValue('Todo 1');
        await expect(todoInput(page, '3')).toHaveValue('Todo 3');
        await expect(todoItems(page)).toHaveCount(3);
    });

    test('update then remove', async ({ page }) => {
        await page.goto('/');
        await wsReady(page);

        // Add a todo
        await addTodoBtn(page).click();
        await expect(todoInput(page, '1')).toHaveValue('Todo 1');

        // Update it
        await page.evaluate(() => {
            window._ws.send(JSON.stringify(['page', 'update_todo', { id: 1, value: 'Updated' }]));
        });
        await expect(todoInput(page, '1')).toHaveValue('Updated');

        // Remove it
        await removeBtn(page, '1').click();
        await expect(todoItem(page, '1')).not.toBeAttached();
        await expect(todoItems(page)).toHaveCount(0);
    });
});

// ---------------------------------------------------------------------------
// 13. Interleaving counter and stream events
// ---------------------------------------------------------------------------

test.describe('interleaving counter and stream events', () => {
    test('title change does not affect todos', async ({ page }) => {
        await page.goto('/');
        await wsReady(page);

        // Add a todo
        await addTodoBtn(page).click();
        await expect(todoItem(page, '1')).toBeVisible();

        // Trigger title_change via WS
        await page.evaluate(() => {
            window._ws.send(JSON.stringify(['page', 'title_change', {}]));
        });

        // Title changed
        await expect(page.locator('main#page[az-view] h1[az]')).toHaveText('Changed');
        // Todo still present
        await expect(todoItem(page, '1')).toBeVisible();
        await expect(todoInput(page, '1')).toHaveValue('Todo 1');
    });

    test('counter increment does not affect todos', async ({ page }) => {
        await page.goto('/');
        await wsReady(page);

        // Add a todo
        await addTodoBtn(page).click();
        await expect(todoItem(page, '1')).toBeVisible();

        // Increment counter
        await page.locator(`#counter button[az-click*='"inc"']`).click();
        await expect(page.locator('#counter p[az$="-3"]')).toHaveText('Count: 1');

        // Todo still present
        await expect(todoItem(page, '1')).toBeVisible();
        await expect(todoInput(page, '1')).toHaveValue('Todo 1');
    });

    test('add todo after title change', async ({ page }) => {
        await page.goto('/');
        await wsReady(page);

        // Add first todo
        await addTodoBtn(page).click();
        await expect(todoItem(page, '1')).toBeVisible();

        // Trigger title_change
        await page.evaluate(() => {
            window._ws.send(JSON.stringify(['page', 'title_change', {}]));
        });
        await expect(page.locator('main#page[az-view] h1[az]')).toHaveText('Changed');

        // Add second todo
        await addTodoBtn(page).click();
        await expect(todoItems(page)).toHaveCount(2);
        await expect(todoInput(page, '1')).toHaveValue('Todo 1');
        await expect(todoInput(page, '2')).toHaveValue('Todo 2');
    });
});

// ---------------------------------------------------------------------------
// 14. Custom text input
// ---------------------------------------------------------------------------

test.describe('custom text input', () => {
    test('add todo with custom text', async ({ page }) => {
        await page.goto('/');
        await wsReady(page);

        await newTodoInput(page).fill('Buy groceries');
        await addTodoBtn(page).click();
        await expect(todoItem(page, '1')).toBeVisible();
        await expect(todoInput(page, '1')).toHaveValue('Buy groceries');
    });

    test('empty input falls back to default text', async ({ page }) => {
        await page.goto('/');
        await wsReady(page);

        // Leave input empty, click add
        await addTodoBtn(page).click();
        await expect(todoInput(page, '1')).toHaveValue('Todo 1');
    });

    test('input is cleared after adding', async ({ page }) => {
        await page.goto('/');
        await wsReady(page);

        await newTodoInput(page).fill('My task');
        await addTodoBtn(page).click();
        await expect(todoItem(page, '1')).toBeVisible();
        // Input should be cleared
        await expect(newTodoInput(page)).toHaveValue('');
    });
});

// ---------------------------------------------------------------------------
// 15. Editable todo items
// ---------------------------------------------------------------------------

test.describe('editable todo items', () => {
    test('edit on blur', async ({ page }) => {
        await page.goto('/');
        await wsReady(page);

        await addTodoBtn(page).click();
        await expect(todoInput(page, '1')).toHaveValue('Todo 1');

        // Clear and type new text, then blur
        await todoInput(page, '1').fill('Edited via blur');
        await todoInput(page, '1').blur();

        await expect(todoInput(page, '1')).toHaveValue('Edited via blur');
    });

    test('edit on Enter', async ({ page }) => {
        await page.goto('/');
        await wsReady(page);

        await addTodoBtn(page).click();
        await expect(todoInput(page, '1')).toHaveValue('Todo 1');

        // Clear and type new text, then press Enter
        await todoInput(page, '1').fill('Edited via enter');
        await todoInput(page, '1').press('Enter');

        await expect(todoInput(page, '1')).toHaveValue('Edited via enter');
    });

    test('edit preserves siblings', async ({ page }) => {
        await page.goto('/');
        await wsReady(page);

        await addTodoBtn(page).click();
        await addTodoBtn(page).click();
        await addTodoBtn(page).click();
        await expect(todoItems(page)).toHaveCount(3);

        // Edit middle item
        await todoInput(page, '2').fill('Middle edited');
        await todoInput(page, '2').blur();

        await expect(todoInput(page, '2')).toHaveValue('Middle edited');
        // Others unchanged
        await expect(todoInput(page, '1')).toHaveValue('Todo 1');
        await expect(todoInput(page, '3')).toHaveValue('Todo 3');
    });
});

// ---------------------------------------------------------------------------
// 15b. Special character keys -- CSS.escape regression
// ---------------------------------------------------------------------------

test.describe('special character keys', () => {
    test('typing special characters in input does not throw', async ({ page }) => {
        await page.goto('/');
        await wsReady(page);

        // Add a todo so we have an input to type into
        await addTodoBtn(page).click();
        await expect(todoInput(page, '1')).toHaveValue('Todo 1');

        // Collect any JS errors
        const errors = [];
        page.on('pageerror', (err) => errors.push(err.message));

        // Type special characters that would break unescaped CSS selectors.
        // Covers CSS combinators, brackets, regex-like chars, and punctuation.
        const specialKeys = [
            '!',
            '@',
            '#',
            '$',
            '%',
            '^',
            '&',
            '*',
            '(',
            ')',
            '[',
            ']',
            '{',
            '}',
            '<',
            '>',
            '+',
            '~',
            '|',
            '\\',
            '/',
            '?',
            '.',
            ':',
            ';',
            "'",
            '"',
            '`',
            ',',
            '=',
            ' ',
        ];
        await todoInput(page, '1').focus();
        for (const key of specialKeys) {
            await page.keyboard.press(key);
        }

        // No JS errors should have occurred
        expect(errors).toEqual([]);
        // Page should still be functional -- add another todo
        await addTodoBtn(page).click();
        await expect(todoItems(page)).toHaveCount(2);
    });

    test('typing special characters in new todo input does not throw', async ({ page }) => {
        await page.goto('/');
        await wsReady(page);

        const errors = [];
        page.on('pageerror', (err) => errors.push(err.message));

        // Type special chars directly into the new todo input
        await newTodoInput(page).fill('Test !@#$%^&*()');
        await addTodoBtn(page).click();

        expect(errors).toEqual([]);
        await expect(todoItem(page, '1')).toBeVisible();
        await expect(todoInput(page, '1')).toHaveValue('Test !@#$%^&*()');
    });
});

// ---------------------------------------------------------------------------
// 16. SPA navigation
// ---------------------------------------------------------------------------

test.describe('SPA navigation', () => {
    test('clicking az-navigate link changes page content', async ({ page }) => {
        await page.goto('/');
        await page.waitForSelector('#status:has-text("Connected")');
        // Wrapping the click asserts the WebSocket stays connected through
        // the navigate -- a silent server-side crash on the new mount would
        // close 4500 + reload, and the destination assertion would still
        // pass. This is the catch-net for that whole class of regression.
        await expectStaysConnected(page, async () => {
            await page.click('a[href="/about"]');
            await expect(page.locator('main h1')).toHaveText('About');
        });
    });

    test('URL updates on navigate', async ({ page }) => {
        await page.goto('/');
        await page.waitForSelector('#status:has-text("Connected")');
        await page.click('a[href="/about"]');
        await expect(page.locator('main h1')).toHaveText('About');
        expect(page.url()).toContain('/about');
    });

    test('document.title updates via set_title effect', async ({ page }) => {
        await page.goto('/');
        await page.waitForSelector('#status:has-text("Connected")');
        // Home page title
        await expect(page).toHaveTitle('Welcome');
        // Navigate to about
        await page.click('a[href="/about"]');
        await expect(page.locator('main h1')).toHaveText('About');
        await expect(page).toHaveTitle('About');
    });

    test('navigate back with browser back button', async ({ page }) => {
        await page.goto('/');
        await page.waitForSelector('#status:has-text("Connected")');
        await page.click('a[href="/about"]');
        await expect(page.locator('main h1')).toHaveText('About');
        // Go back
        await page.goBack();
        await expect(page.locator('main h1')).toHaveText('Welcome');
        expect(page.url()).not.toContain('/about');
    });

    test('navigate preserves layout (nav links still present)', async ({ page }) => {
        await page.goto('/');
        await page.waitForSelector('#status:has-text("Connected")');
        await page.click('a[href="/about"]');
        await expect(page.locator('main h1')).toHaveText('About');
        // Nav should still exist
        await expect(page.locator('nav a[href="/"]')).toBeVisible();
        await expect(page.locator('nav a[href="/about"]')).toBeVisible();
    });

    test('navigate then interact on new page', async ({ page }) => {
        await page.goto('/');
        await page.waitForSelector('#status:has-text("Connected")');
        await page.click('a[href="/about"]');
        await expect(page.locator('main h1')).toHaveText('About');
        // handle_info(arizona_connected) fires on new page, dispatching set_title
        await expect(page).toHaveTitle('About');
    });

    test('navigate home → about → home round trip', async ({ page }) => {
        await page.goto('/');
        await page.waitForSelector('#status:has-text("Connected")');
        await page.click('a[href="/about"]');
        await expect(page.locator('main h1')).toHaveText('About');
        // Navigate back to home via nav link
        await page.click('a[href="/"]');
        await expect(page.locator('main h1')).toHaveText('Welcome');
        await expect(page).toHaveTitle('Welcome');
    });

    test('same-page navigate is no-op', async ({ page }) => {
        await page.goto('/');
        await page.waitForSelector('#status:has-text("Connected")');
        // Click home link while already on home
        await page.click('a[href="/"]');
        // Should still be on home, no error
        await expect(page.locator('main h1')).toHaveText('Welcome');
    });

    test('root binding survives navigate round-trip', async ({ page }) => {
        // arizona_live carries the previous root handler's final bindings
        // forward as the floor for the new mount's input -- session-level
        // state set on one page is visible to the next handler's mount.
        // The child counters on `/` mount with `count => ?get(count, 0)`
        // from the root view, so a non-zero root `count` is observable
        // in the child counter's <p> after a round-trip via /about.
        await page.goto('/');
        await page.waitForSelector('#status:has-text("Connected")');
        // Sanity: counters start at 0
        await expect(countText(page, 'counter')).toHaveText('Count: 0');
        // "Add +1 to both" increments root `count` (and pushes to children)
        await page.click('button[az-click*="add"]');
        await expect(countText(page, 'counter')).toHaveText('Count: 1');
        // Navigate to about; arizona_about's mount uses the lazy
        // `maps:merge(Defaults, Bindings0)` pattern, so it inherits
        // root's `count` even though about itself doesn't render it.
        await page.click('a[href="/about"]');
        await expect(page.locator('main h1')).toHaveText('About');
        // Navigate back to home. With carry-forward, root mounts with
        // `count => 1` flowing through the merged input; without it,
        // the handler default `count => 0` would win and the child
        // counter would render 0.
        await page.click('a[href="/"]');
        await expect(page.locator('main h1')).toHaveText('Welcome');
        await expect(countText(page, 'counter')).toHaveText('Count: 1');
    });

    test('back after navigate restores prior scroll position', async ({ page }) => {
        // Dedicated scroll test fixtures -- tall enough to scroll.
        await page.goto('/scroll-home');
        await page.waitForSelector('#status:has-text("Connected")');
        await page.evaluate(() => window.scrollTo(0, 600));
        await page.evaluate(() => new Promise((r) => requestAnimationFrame(() => r(null))));

        // Seed a link we can click without Playwright's auto-scroll-into-view
        // (which would reset scrollY before the click fires).
        await page.evaluate(() => {
            const a = document.createElement('a');
            a.setAttribute('href', '/scroll-about');
            a.setAttribute('az-navigate', '');
            a.id = '__nav_push';
            document.body.appendChild(a);
            a.dispatchEvent(
                new MouseEvent('click', { bubbles: true, cancelable: true, button: 0 }),
            );
        });
        await expect(page.locator('main h1')).toHaveText('Scroll About');

        await page.goBack();
        await expect(page.locator('main h1')).toHaveText('Scroll Home');
        await page
            .waitForFunction(() => window.scrollY > 0, null, { timeout: 2000 })
            .catch(() => {});
        const y = await page.evaluate(() => window.scrollY);
        expect(Math.abs(y - 600)).toBeLessThanOrEqual(2);
    });

    test('navigate to /scroll-about#section scrolls to the anchor', async ({ page }) => {
        await page.goto('/scroll-home');
        await page.waitForSelector('#status:has-text("Connected")');
        await page.evaluate(() => {
            const a = document.createElement('a');
            a.setAttribute('href', '/scroll-about#section');
            a.setAttribute('az-navigate', '');
            a.id = '__nav_hash';
            document.body.appendChild(a);
            a.dispatchEvent(
                new MouseEvent('click', { bubbles: true, cancelable: true, button: 0 }),
            );
        });
        await expect(page.locator('main h1')).toHaveText('Scroll About');
        await page.waitForFunction(
            () => {
                const el = document.getElementById('section');
                return el && Math.abs(el.getBoundingClientRect().top) < 50;
            },
            null,
            { timeout: 2000 },
        );
        const top = await page.locator('#section').evaluate((el) => el.getBoundingClientRect().top);
        expect(Math.abs(top)).toBeLessThan(50);
    });

    test('direct load of URL with #hash scrolls to the anchor', async ({ page }) => {
        // Not an SPA nav -- a full page load. The browser's native anchor
        // jump is suppressed by scrollRestoration='manual', so connect() must
        // do the scroll itself.
        await page.goto('/scroll-about#section');
        await page.waitForSelector('#status:has-text("Connected")');
        await page.waitForFunction(
            () => {
                const el = document.getElementById('section');
                return el && Math.abs(el.getBoundingClientRect().top) < 50;
            },
            null,
            { timeout: 2000 },
        );
        const top = await page.locator('#section').evaluate((el) => el.getBoundingClientRect().top);
        expect(Math.abs(top)).toBeLessThan(50);
    });

    test('replace nav preserves scroll position', async ({ page }) => {
        await page.goto('/scroll-home');
        await page.waitForSelector('#status:has-text("Connected")');
        await page.evaluate(() => window.scrollTo(0, 500));
        await page.evaluate(() => new Promise((r) => requestAnimationFrame(() => r(null))));

        // Button on the page fires arizona_js:navigate(Path, #{replace => true}).
        // Native .click() via evaluate avoids Playwright's auto-scroll reset.
        await page.evaluate(() => {
            const btn = /** @type {HTMLButtonElement} */ (document.querySelector('#replace-nav'));
            btn.click();
        });
        await page.waitForFunction(() => location.search === '?x=1', null, { timeout: 2000 });
        const y = await page.evaluate(() => window.scrollY);
        expect(Math.abs(y - 500)).toBeLessThanOrEqual(2);
    });

    test('forward after back does not restore scroll (documented non-goal)', async ({ page }) => {
        // Lock the documented limitation: after back -> forward, the destination
        // entry has no saved scroll so we fall through to scroll-to-top. If
        // forward-nav restore is added later, this test will fail and force an
        // intentional decision about docs + behavior.
        await page.goto('/scroll-home');
        await page.waitForSelector('#status:has-text("Connected")');
        await page.evaluate(() => window.scrollTo(0, 400));
        await page.evaluate(() => new Promise((r) => requestAnimationFrame(() => r(null))));

        await page.evaluate(() => {
            const a = document.createElement('a');
            a.setAttribute('href', '/scroll-about');
            a.setAttribute('az-navigate', '');
            a.id = '__nav_push';
            document.body.appendChild(a);
            a.dispatchEvent(
                new MouseEvent('click', { bubbles: true, cancelable: true, button: 0 }),
            );
        });
        await expect(page.locator('main h1')).toHaveText('Scroll About');
        // Scroll on the destination too, so if forward somehow restored we'd see a non-zero value.
        await page.evaluate(() => window.scrollTo(0, 250));
        await page.evaluate(() => new Promise((r) => requestAnimationFrame(() => r(null))));

        await page.goBack();
        await expect(page.locator('main h1')).toHaveText('Scroll Home');
        await page.goForward();
        await expect(page.locator('main h1')).toHaveText('Scroll About');
        // Forward restore is a non-goal: destination entry has null state so we scroll-to-top.
        // Give the OP_REPLACE + applyScroll a moment to settle.
        await page.waitForFunction(() => location.pathname === '/scroll-about', null, {
            timeout: 2000,
        });
        await page.evaluate(() => new Promise((r) => setTimeout(r, 50)));
        const y = await page.evaluate(() => window.scrollY);
        expect(y).toBe(0);
    });
});

// ---------------------------------------------------------------------------
// 17. Drag-and-drop reorder
// ---------------------------------------------------------------------------

test.describe('drag-and-drop reorder', () => {
    test('reorder via WebSocket message', async ({ page }) => {
        await page.goto('/');
        await wsReady(page);

        // Add 3 items
        await addTodoBtn(page).click();
        await addTodoBtn(page).click();
        await addTodoBtn(page).click();
        await expect(todoItems(page)).toHaveCount(3);

        // Move item 3 to position 0 via WS
        await page.evaluate(() => {
            window._ws.send(
                JSON.stringify(['page', 'reorder_todo', { data_transfer: '3', drop_index: 0 }]),
            );
        });

        // Wait for reorder -- item 3 should now be first
        await expect(todoItems(page).nth(0)).toHaveAttribute('az-key', '3');
        await expect(todoItems(page).nth(1)).toHaveAttribute('az-key', '1');
        await expect(todoItems(page).nth(2)).toHaveAttribute('az-key', '2');
        await expect(todoItems(page)).toHaveCount(3);
    });

    test('reorder preserves item values', async ({ page }) => {
        await page.goto('/');
        await wsReady(page);

        // Add 3 items
        await addTodoBtn(page).click();
        await addTodoBtn(page).click();
        await addTodoBtn(page).click();

        // Move item 1 to position 2 via WS
        await page.evaluate(() => {
            window._ws.send(
                JSON.stringify(['page', 'reorder_todo', { data_transfer: '1', drop_index: 2 }]),
            );
        });

        // Values should follow their items
        await expect(todoInput(page, '1')).toHaveValue('Todo 1');
        await expect(todoInput(page, '2')).toHaveValue('Todo 2');
        await expect(todoInput(page, '3')).toHaveValue('Todo 3');

        // Order should be 2, 3, 1
        await expect(todoItems(page).nth(0)).toHaveAttribute('az-key', '2');
        await expect(todoItems(page).nth(1)).toHaveAttribute('az-key', '3');
        await expect(todoItems(page).nth(2)).toHaveAttribute('az-key', '1');
    });

    test('edits persist after reorder', async ({ page }) => {
        await page.goto('/');
        await wsReady(page);

        // Add 2 items
        await addTodoBtn(page).click();
        await addTodoBtn(page).click();

        // Edit item 1
        await page.evaluate(() => {
            window._ws.send(JSON.stringify(['page', 'update_todo', { id: 1, value: 'Edited' }]));
        });
        await expect(todoInput(page, '1')).toHaveValue('Edited');

        // Move item 2 to position 0
        await page.evaluate(() => {
            window._ws.send(
                JSON.stringify(['page', 'reorder_todo', { data_transfer: '2', drop_index: 0 }]),
            );
        });

        // Order: 2, 1 -- values preserved
        await expect(todoItems(page).nth(0)).toHaveAttribute('az-key', '2');
        await expect(todoItems(page).nth(1)).toHaveAttribute('az-key', '1');
        await expect(todoInput(page, '1')).toHaveValue('Edited');
        await expect(todoInput(page, '2')).toHaveValue('Todo 2');
    });
});

// ---------------------------------------------------------------------------
// 18. handle_info + az-hook (about page timer)
// ---------------------------------------------------------------------------

test.describe('handle_info + az-hook (about page timer)', () => {
    /** Locate the tick span on the about page. */
    const tickSpan = (page) => page.locator('main#page[az-view] span[az-hook="Tick"]');

    test('tick count increments via handle_info', async ({ page }) => {
        await page.goto('/about');
        await wsReady(page);
        // After connect, first tick arrives after 1s
        await expect(tickSpan(page)).toHaveText('1', { timeout: 3000 });
    });

    test('tick keeps incrementing', async ({ page }) => {
        await page.goto('/about');
        await wsReady(page);
        // Capture an early value, then wait for it to increase -- proves recurring timer
        await page.waitForFunction(
            () => {
                const el = document.querySelector('main#page span[az-hook="Tick"]');
                return el && parseInt(el.textContent, 10) >= 1;
            },
            { timeout: 3000 },
        );
        const first = parseInt(await tickSpan(page).textContent(), 10);
        await page.waitForFunction(
            (prev) => {
                const el = document.querySelector('main#page span[az-hook="Tick"]');
                return el && parseInt(el.textContent, 10) > prev;
            },
            first,
            { timeout: 3000 },
        );
    });

    test('hook mounted fires on connect', async ({ page }) => {
        await page.goto('/about');
        await wsReady(page);
        // The Tick hook's mounted() sets data-hook-mounted="true"
        await expect(tickSpan(page)).toHaveAttribute('data-hook-mounted', 'true');
    });

    test('hook updated fires on each tick', async ({ page }) => {
        await page.goto('/about');
        await wsReady(page);
        // Wait until tick reaches at least 2 (don't match exact text -- it's a race)
        await page.waitForFunction(
            () => {
                const el = document.querySelector('main#page span[az-hook="Tick"]');
                return el && parseInt(el.textContent, 10) >= 2;
            },
            { timeout: 5000 },
        );
        // updated() increments data-hook-updated each time the tick changes
        const count = await tickSpan(page).getAttribute('data-hook-updated');
        expect(parseInt(count, 10)).toBeGreaterThanOrEqual(2);
    });

    test('SPA navigate to about triggers hook mounted', async ({ page }) => {
        await page.goto('/');
        await page.waitForSelector('#status:has-text("Connected")');
        // Navigate to about via SPA link
        await page.click('a[href="/about"]');
        await expect(page.locator('main h1')).toHaveText('About');
        // Tick hook should mount on the new page
        await expect(tickSpan(page)).toHaveAttribute('data-hook-mounted', 'true');
        // Timer should start ticking
        await expect(tickSpan(page)).toHaveText('1', { timeout: 3000 });
    });

    test('pushEvent from hook triggers server round-trip', async ({ page }) => {
        await page.goto('/about');
        await wsReady(page);

        // Hook mounted() already called this.pushEvent('tick_started', {})
        // on ws.onopen. For SPA navigate we can catch it live, but for
        // direct load the ack may have already arrived. Re-trigger via
        // SPA navigate: go home, then back to about.
        await page.click('a[href="/"]');
        await expect(page.locator('main h1')).toHaveText('Welcome');

        // Set up listener before navigating back
        const ackPromise = page.evaluate(() => {
            return new Promise((resolve) => {
                document.addEventListener(
                    'tick_ack',
                    (e) => {
                        resolve(e.detail);
                    },
                    { once: true },
                );
            });
        });

        await page.click('a[href="/about"]');
        await expect(page.locator('main h1')).toHaveText('About');

        // Hook mounted() fires on the new page, pushEvent('tick_started')
        // Server responds with push_event('tick_ack', #{status => ok})
        const detail = await ackPromise;
        expect(detail).toEqual({ status: 'ok' });
    });

    test('hook destroyed fires on navigate away', async ({ page }) => {
        await page.goto('/about');
        await wsReady(page);
        // Tick hook is mounted
        await expect(tickSpan(page)).toHaveAttribute('data-hook-mounted', 'true');

        // Navigate away -- OP_REPLACE removes the about page element,
        // destroyed() fires and sets window._tickDestroyed = true
        await page.click('a[href="/"]');
        await page.waitForFunction(() => window._tickDestroyed === true, { timeout: 5000 });
    });
});

// ---------------------------------------------------------------------------
// 19. List comprehension (about page tags)
// ---------------------------------------------------------------------------

test.describe('List comprehension (about page tags)', () => {
    const tagItems = (page) => page.locator('#tags > li');

    test('SSR renders tag list with 3 items', async ({ page }) => {
        await page.goto('/about');
        await expect(tagItems(page)).toHaveCount(3);
        await expect(tagItems(page).nth(0)).toHaveText('erlang');
        await expect(tagItems(page).nth(1)).toHaveText('otp');
        await expect(tagItems(page).nth(2)).toHaveText('arizona');
    });

    test('add_tag appends a new tag via OP_UPDATE', async ({ page }) => {
        await page.goto('/about');
        await wsReady(page);
        await expect(tagItems(page)).toHaveCount(3);
        await page.click(`button[az-click*='"add_tag"']`);
        await expect(tagItems(page)).toHaveCount(4);
        await expect(tagItems(page).nth(3)).toHaveText('new');
    });

    test('timer still works alongside list', async ({ page }) => {
        await page.goto('/about');
        await wsReady(page);
        await expect(tagItems(page)).toHaveCount(3);
        const tickSpan = page.locator('main#page[az-view] span[az-hook="Tick"]');
        await expect(tickSpan).toHaveText('1', { timeout: 3000 });
    });

    test('SPA navigate to about renders tag list', async ({ page }) => {
        await page.goto('/');
        await page.waitForSelector('#status:has-text("Connected")');
        await page.click('a[href="/about"]');
        await expect(page.locator('main h1')).toHaveText('About');
        await expect(tagItems(page)).toHaveCount(3);
        await expect(tagItems(page).nth(0)).toHaveText('erlang');
    });
});

// ---------------------------------------------------------------------------
// Reconnection
// ---------------------------------------------------------------------------

test('reconnects after WS close and re-mounts with fresh state', async ({ page }) => {
    await page.goto('/');
    await wsReady(page);

    // Verify connected state
    await expect(page.locator('#status')).toHaveText('Connected');

    // Increment counter to verify it's working
    await incBtn(page, 'counter').click();
    await expect(countText(page, 'counter')).toHaveText('Count: 1');

    // Close WS with custom code (4000) to simulate abnormal close.
    // ws.close() without args sends 1000 (normal), which skips reconnect.
    await page.evaluate(() => window._ws.close(4000));

    // Should get az-disconnected class
    await page.waitForFunction(() =>
        document.documentElement.classList.contains('az-disconnected'),
    );

    // Wait for reconnection
    await wsReady(page);

    // Counter should show 0 -- fresh mount, state not preserved
    await expect(countText(page, 'counter')).toHaveText('Count: 0');

    // handle_info(arizona_connected) fires -- status shows "Connected" again
    await expect(page.locator('#status')).toHaveText('Connected');
});

test('form input is preserved across reconnect', async ({ page }) => {
    await page.goto('/');
    await wsReady(page);

    // Type into the todo input
    const input = newTodoInput(page);
    await input.fill('my todo text');

    // Close WS with custom code to trigger reconnect
    await page.evaluate(() => window._ws.close(4000));

    // Wait for reconnection
    await wsReady(page);

    // Form input should still have the typed value
    await expect(input).toHaveValue('my todo text');
});

test('hook re-mounts after reconnect on about page', async ({ page }) => {
    await page.goto('/about');
    await wsReady(page);

    const tickSpan = page.locator('main#page[az-view] span[az-hook="Tick"]');
    // Tick hook should be mounted
    await expect(tickSpan).toHaveAttribute('data-hook-mounted', 'true');

    // Close WS to trigger reconnect
    await page.evaluate(() => window._ws.close(4000));
    await page.waitForFunction(() =>
        document.documentElement.classList.contains('az-disconnected'),
    );

    // Wait for reconnection
    await wsReady(page);

    // Hook should be re-mounted on the fresh element (OP_REPLACE replaces the DOM)
    await expect(tickSpan).toHaveAttribute('data-hook-mounted', 'true');
});

test('two sequential reconnects leave page functional', async ({ page }) => {
    await page.goto('/');
    await wsReady(page);

    // Interact: increment counter
    await incBtn(page, 'counter').click();
    await expect(countText(page, 'counter')).toHaveText('Count: 1');

    // First disconnect + reconnect
    await page.evaluate(() => window._ws.close(4000));
    await wsReady(page);
    await expect(page.locator('#status')).toHaveText('Connected');
    // Counter resets to 0 after fresh mount
    await expect(countText(page, 'counter')).toHaveText('Count: 0');

    // Interact again after first reconnect
    await incBtn(page, 'counter').click();
    await expect(countText(page, 'counter')).toHaveText('Count: 1');

    // Second disconnect + reconnect
    await page.evaluate(() => window._ws.close(4000));
    await wsReady(page);
    await expect(page.locator('#status')).toHaveText('Connected');
    // Counter resets again
    await expect(countText(page, 'counter')).toHaveText('Count: 0');

    // Page is still functional after two reconnects
    await incBtn(page, 'counter').click();
    await expect(countText(page, 'counter')).toHaveText('Count: 1');
});

test('reconnect after SPA navigate mounts correct handler', async ({ page }) => {
    await page.goto('/');
    await wsReady(page);

    // SPA navigate to /about
    await page.click('a[href="/about"]');
    await expect(page.locator('main h1')).toHaveText('About');

    // Close WS to trigger reconnect
    await page.evaluate(() => window._ws.close(4000));
    await wsReady(page);

    // Should still be on about page (not home) after reconnect
    await expect(page.locator('main h1')).toHaveText('About');
    expect(page.url()).toContain('/about');
});

test('about page timer restarts after reconnect', async ({ page }) => {
    await page.goto('/about');
    await wsReady(page);

    const tickSpan = page.locator('main#page[az-view] span[az-hook="Tick"]');

    // Wait for tick to reach at least 1
    await expect(tickSpan).toHaveText('1', { timeout: 3000 });

    // Close WS to trigger reconnect
    await page.evaluate(() => window._ws.close(4000));
    await wsReady(page);

    // After reconnect, fresh mount means tick restarts from 0
    // Wait for the first tick after reconnect
    await expect(tickSpan).toHaveText('1', { timeout: 5000 });

    // Verify timer is still running -- tick increments beyond 1
    await page.waitForFunction(
        () => {
            const el = document.querySelector('main#page span[az-hook="Tick"]');
            return el && parseInt(el.textContent, 10) >= 2;
        },
        { timeout: 5000 },
    );
});
