import { expect, test } from '@playwright/test';
import { expectStaysConnected } from '../utils/helpers.js';

// ---------------------------------------------------------------------------
// Helpers -- shared
// ---------------------------------------------------------------------------

/** Wait for the WebSocket to be open so events can be dispatched. */
const wsReady = (page) =>
    page.waitForFunction(() => document.documentElement.classList.contains('az-connected'));

/**
 * Wait for the frame the page sends on its own. The view self-sends on
 * connect, and its "Connected" status frame can land after the socket opens,
 * so a test that captures "the next frame" must wait for it first or it
 * captures that frame instead of the reply to its own event.
 */
const liveSettled = async (page) => {
    await wsReady(page);
    await expect(page.locator('#status')).toHaveText('Connected');
};

// ---------------------------------------------------------------------------
// Helpers -- counter
// ---------------------------------------------------------------------------

/** Locate a counter's count <p> by its view id. */
const countText = (page, viewId) => page.locator(`#${viewId} > p`);

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
            const p = document.querySelector('#counter > p');
            const children = Array.from(p.childNodes);
            return (
                children.some((n) => n.nodeType === 8 && n.data === `az:${p.getAttribute('az')}`) &&
                children.some((n) => n.nodeType === 8 && n.data === '/az')
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
        await liveSettled(page);

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
        await liveSettled(page);

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
        await liveSettled(page);

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
        await expect(page.locator('#counter > p')).toHaveText('Count: 1');

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
        // the navigate -- a silent server-side crash on the new mount would be
        // answered with a full load of the page and close 4500, and the destination
        // assertion would still pass. This is the catch-net for that whole class of regression.
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

    test('root binding does not survive navigate round-trip without explicit forwarding', async ({
        page,
    }) => {
        // arizona_live's navigate-carry feature still flows the previous
        // root's final bindings into the next mount's input, but the
        // example handlers (per the construct-don't-merge rule in
        // .claude/rules/erlang.md) build a fresh map and only accept
        // overrides they explicitly enumerate. arizona_about does not
        // forward `count`, so a round-trip via /about resets it to the
        // arizona_page default of 0.
        await page.goto('/');
        await page.waitForSelector('#status:has-text("Connected")');
        await expect(countText(page, 'counter')).toHaveText('Count: 0');
        await page.click('button[az-click*="add"]');
        await expect(countText(page, 'counter')).toHaveText('Count: 1');
        await page.click('a[href="/about"]');
        await expect(page.locator('main h1')).toHaveText('About');
        await page.click('a[href="/"]');
        await expect(page.locator('main h1')).toHaveText('Welcome');
        // Count resets because neither arizona_about nor arizona_page
        // explicitly forwards the carried `count` key in their mount.
        await expect(countText(page, 'counter')).toHaveText('Count: 0');
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

    // Only a fragment of this very URL is a hash jump that keeps the view: one
    // under an empty query is another page, which the browser would load, so it
    // is navigated to (the view is replaced) and still lands on the anchor.
    for (const [href, replaced] of [
        ['/scroll-about#section', false],
        ['/scroll-about?#section', true],
    ]) {
        test(`az-navigate from /scroll-about to ${href} replaces the view: ${replaced}`, async ({
            page,
        }) => {
            await page.goto('/scroll-about');
            await page.waitForSelector('#status:has-text("Connected")');
            await page.evaluate((url) => {
                const marker = document.createElement('i');
                marker.id = '__marker';
                document.querySelector('[az-view]').appendChild(marker);
                const a = document.createElement('a');
                a.setAttribute('href', url);
                a.setAttribute('az-navigate', '');
                document.body.appendChild(a);
                a.dispatchEvent(
                    new MouseEvent('click', { bubbles: true, cancelable: true, button: 0 }),
                );
            }, href);
            await page.waitForURL((u) => u.href.endsWith(href));
            await page.waitForFunction(
                () => Math.abs(document.getElementById('section').getBoundingClientRect().top) < 50,
                null,
                { timeout: 2000 },
            );
            await expect(page.locator('#__marker')).toHaveCount(replaced ? 0 : 1);
        });
    }

    // Back and Forward across that edge cross between two pages, so each replaces
    // the view again; across a fragment of the page they keep it.
    for (const [href, replaced] of [
        ['/scroll-about#section', false],
        ['/scroll-about?#section', true],
    ]) {
        test(`Back and Forward between /scroll-about and ${href} replace the view: ${replaced}`, async ({
            page,
        }) => {
            await page.goto('/scroll-about');
            await page.waitForSelector('#status:has-text("Connected")');
            const mark = () =>
                page.evaluate(() => {
                    const marker = document.createElement('i');
                    marker.id = '__marker';
                    document.querySelector('[az-view]').appendChild(marker);
                });
            await page.evaluate((url) => {
                const a = document.createElement('a');
                a.setAttribute('href', url);
                a.setAttribute('az-navigate', '');
                document.body.appendChild(a);
                a.dispatchEvent(
                    new MouseEvent('click', { bubbles: true, cancelable: true, button: 0 }),
                );
            }, href);
            await page.waitForURL((u) => u.href.endsWith(href));
            await mark();
            await page.goBack();
            await page.waitForURL((u) => u.href.endsWith('/scroll-about'));
            await expect(page.locator('#__marker')).toHaveCount(replaced ? 0 : 1);
            await page.locator('#__marker').evaluateAll((els) => {
                for (const el of els) el.remove();
            });
            await mark();
            await page.goForward();
            await page.waitForURL((u) => u.href.endsWith(href));
            await expect(page.locator('#__marker')).toHaveCount(replaced ? 0 : 1);
        });
    }

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

    test('reload restores the scroll position', async ({ page }) => {
        // scrollRestoration is 'manual' for the SPA, which also switches off the
        // browser's own restore on a full reload -- connect() has to do it.
        await page.goto('/scroll-home');
        await page.waitForSelector('#status:has-text("Connected")');
        await page.evaluate(() => window.scrollTo(0, 600));
        await page.evaluate(() => new Promise((r) => requestAnimationFrame(() => r(null))));

        await page.reload();
        await page.waitForSelector('#status:has-text("Connected")');
        await page
            .waitForFunction(() => window.scrollY > 0, null, { timeout: 2000 })
            .catch(() => {});
        const y = await page.evaluate(() => window.scrollY);
        expect(Math.abs(y - 600)).toBeLessThanOrEqual(2);
    });

    test('back to a full-document page restores its scroll position', async ({ page }) => {
        await page.goto('/scroll-home');
        await page.waitForSelector('#status:has-text("Connected")');
        await page.evaluate(() => window.scrollTo(0, 600));
        await page.evaluate(() => new Promise((r) => requestAnimationFrame(() => r(null))));

        // A full-document navigation away. The unload listener makes the page
        // bfcache-ineligible, so Back loads it again instead of restoring it.
        await page.evaluate(() => {
            window.addEventListener('unload', () => {});
            location.href = '/scroll-about';
        });
        await expect(page.locator('main h1')).toHaveText('Scroll About');
        await page.waitForSelector('#status:has-text("Connected")');

        await page.goBack();
        await expect(page.locator('main h1')).toHaveText('Scroll Home');
        await page.waitForSelector('#status:has-text("Connected")');
        await page
            .waitForFunction(() => window.scrollY > 0, null, { timeout: 2000 })
            .catch(() => {});
        const y = await page.evaluate(() => window.scrollY);
        expect(Math.abs(y - 600)).toBeLessThanOrEqual(2);
    });

    test('a navigate degraded to a full load lands at the top, not at the old scroll', async ({
        page,
    }) => {
        // /scroll-section adds a layout, so the server answers the navigate with
        // a full load of the URL the client already pushed. That unload must not
        // stamp the OLD page's scroll onto the new entry for connect to restore.
        await page.addInitScript(() =>
            document.addEventListener('DOMContentLoaded', () => {
                document.body.style.minHeight = '6000px';
            }),
        );
        await page.goto('/scroll-home');
        await page.waitForSelector('#status:has-text("Connected")');
        await page.evaluate(() => window.scrollTo(0, 900));
        await page.evaluate(() => new Promise((r) => requestAnimationFrame(() => r(null))));
        await Promise.all([
            page.waitForEvent('load'),
            page.evaluate(() => {
                const a = document.createElement('a');
                a.setAttribute('href', '/scroll-section');
                a.setAttribute('az-navigate', '');
                document.body.appendChild(a);
                a.dispatchEvent(
                    new MouseEvent('click', { bubbles: true, cancelable: true, button: 0 }),
                );
            }),
        ]);
        await expect(page.locator('section.scroll-section main h1')).toHaveText('Scroll About');
        await page.waitForSelector('#status:has-text("Connected")');
        await page.evaluate(() => new Promise((r) => setTimeout(r, 300)));
        expect(await page.evaluate(() => window.scrollY)).toBe(0);
    });

    test('Back while disconnected reloads the destination at its own scroll', async ({ page }) => {
        // A cross-page popstate with no socket reloads the URL the browser now
        // shows; the DOM and scroll are still the previous page's, so that
        // unload must not overwrite the destination's saved position.
        await page.addInitScript(() =>
            document.addEventListener('DOMContentLoaded', () => {
                document.body.style.minHeight = '6000px';
            }),
        );
        await page.goto('/scroll-home');
        await page.waitForSelector('#status:has-text("Connected")');
        await page.evaluate(() => window.scrollTo(0, 600));
        await page.evaluate(() => new Promise((r) => requestAnimationFrame(() => r(null))));
        await page.evaluate(() => {
            const a = document.createElement('a');
            a.setAttribute('href', '/scroll-about');
            a.setAttribute('az-navigate', '');
            document.body.appendChild(a);
            a.dispatchEvent(
                new MouseEvent('click', { bubbles: true, cancelable: true, button: 0 }),
            );
        });
        await expect(page.locator('main h1')).toHaveText('Scroll About');
        await page.evaluate(() => window.scrollTo(0, 1500));
        await page.evaluate(() => new Promise((r) => requestAnimationFrame(() => r(null))));
        // A 1000 close is deliberate: the worker does not reconnect.
        await page.evaluate(() => window._ws.close(1000));
        await page.waitForFunction(() =>
            document.documentElement.classList.contains('az-disconnected'),
        );
        await Promise.all([page.waitForEvent('load'), page.evaluate(() => history.back())]);
        await expect(page.locator('main h1')).toHaveText('Scroll Home');
        await page.waitForSelector('#status:has-text("Connected")');
        await page
            .waitForFunction(() => window.scrollY > 0, null, { timeout: 2000 })
            .catch(() => {});
        const y = await page.evaluate(() => window.scrollY);
        expect(Math.abs(y - 600)).toBeLessThanOrEqual(2);
    });

    test('a fresh load of a URL does not restore a stale scroll position', async ({ page }) => {
        // A new history entry carries no saved scroll, so a plain visit lands at
        // the top even after the same URL was scrolled in an earlier entry. (A
        // visit from the same URL would not do: Chromium keeps the entry, and
        // its state, for that -- and restores it under 'auto' as well.)
        await page.goto('/scroll-home');
        await page.waitForSelector('#status:has-text("Connected")');
        await page.evaluate(() => window.scrollTo(0, 600));
        await page.evaluate(() => new Promise((r) => requestAnimationFrame(() => r(null))));

        await page.goto('/scroll-about');
        await page.waitForSelector('#status:has-text("Connected")');
        await page.goto('/scroll-home');
        await page.waitForSelector('#status:has-text("Connected")');
        await page.evaluate(() => new Promise((r) => setTimeout(r, 100)));
        const y = await page.evaluate(() => window.scrollY);
        expect(y).toBe(0);
    });
});

// ---------------------------------------------------------------------------
// 17. Drag-and-drop reorder
// ---------------------------------------------------------------------------

test.describe('drag-and-drop reorder', () => {
    test('reorder by dragging an item onto another', async ({ page }) => {
        // A real HTML5 drag: dragstart on the item marks an item drag, so the
        // container claims the drop and pushes reorder_todo.
        await page.goto('/');
        await wsReady(page);
        await addTodoBtn(page).click();
        await addTodoBtn(page).click();
        await addTodoBtn(page).click();
        await expect(todoItems(page)).toHaveCount(3);

        await todoInput(page, '1').dragTo(todoInput(page, '3'));
        await expect(todoItems(page).nth(2)).toHaveAttribute('az-key', '1');
        await expect(todoItems(page).nth(0)).toHaveAttribute('az-key', '2');
    });

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
    const tickSpan = (page) => page.locator('main#about-page[az-view] span[az-hook="Tick"]');

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
                const el = document.querySelector('main#about-page span[az-hook="Tick"]');
                return el && parseInt(el.textContent, 10) >= 1;
            },
            { timeout: 3000 },
        );
        const first = parseInt(await tickSpan(page).textContent(), 10);
        await page.waitForFunction(
            (prev) => {
                const el = document.querySelector('main#about-page span[az-hook="Tick"]');
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
                const el = document.querySelector('main#about-page span[az-hook="Tick"]');
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

    // The tag list is a single-root plain-list each, so appending diffs
    // positionally: one OP_LIST_PATCH carrying a tail OP_INSERT sub-op (verified
    // on the wire), not a wholesale re-render of the container.
    test('add_tag appends a new tag via OP_LIST_PATCH', async ({ page }) => {
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
        const tickSpan = page.locator('main#about-page[az-view] span[az-hook="Tick"]');
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

    const tickSpan = page.locator('main#about-page[az-view] span[az-hook="Tick"]');
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

    const tickSel = 'main#about-page span[az-hook="Tick"]';

    // Timer is running before the reconnect (tick climbs to at least 1).
    await page.waitForFunction(
        (sel) => {
            const el = document.querySelector(sel);
            return el && parseInt(el.textContent, 10) >= 1;
        },
        tickSel,
        { timeout: 6000 },
    );

    // Close WS to trigger reconnect. The old live process dies (drain is
    // server-initiated only), so reconnect fresh-mounts the view: tick resets
    // to 0 and re-renders via OP_REPLACE.
    await page.evaluate(() => window._ws.close(4000));
    await wsReady(page);

    // Reset is observable as tick === '0' -- pre-close it was always >= 1, so
    // '0' can only come from the post-reconnect fresh mount (no transient-value
    // race, unlike matching '1').
    await page.waitForFunction((sel) => document.querySelector(sel)?.textContent === '0', tickSel, {
        timeout: 8000,
    });

    // The restarted timer keeps running -- the tick climbs to at least 2.
    await page.waitForFunction(
        (sel) => {
            const el = document.querySelector(sel);
            return el && parseInt(el.textContent, 10) >= 2;
        },
        tickSel,
        { timeout: 8000 },
    );
});

// An az-form-reset submit resets once what it sent went out, at once, whatever its
// navigation does next: Back does not bring the sent text back (a slow load the
// socket's resync happens during included), and a prompt that cancels the load does
// not undo the send.
test.describe('az-form-reset with a navigation that leaves the page', () => {
    const fullNavigateSubmit = (page) =>
        page.evaluate(() => {
            const form = document.getElementById('add-todo');
            const cmds = JSON.parse(form.getAttribute('az-submit'));
            const list = Array.isArray(cmds[0]) ? cmds : [cmds];
            form.setAttribute(
                'az-submit',
                JSON.stringify([...list, [10, '/scroll-home', { full: true }]]),
            );
        });
    const field = (page) => page.locator('#add-todo input:not([type=submit])').first();

    test('Back after the load shows the field empty', async ({ page }) => {
        await page.goto('/');
        await wsReady(page);
        await fullNavigateSubmit(page);
        await field(page).fill('secret text');
        const loaded = page.waitForURL((u) => u.pathname === '/scroll-home');
        await field(page).press('Enter');
        await loaded;
        await wsReady(page);
        await page.goBack();
        await page.waitForURL((u) => u.pathname === '/');
        await page.waitForTimeout(500);
        await expect(field(page)).toHaveValue('');
    });

    // An SPA navigate swaps only the view: a form outside it (a layout's) stays on
    // screen, so its reset happens at once, not at an unload that never comes.
    test('a form outside the view an SPA navigate replaces is reset at once', async ({ page }) => {
        await page.goto('/');
        await wsReady(page);
        await page.evaluate(() => {
            const f = document.createElement('form');
            f.id = 'layout-form';
            f.setAttribute('az-form-reset', '');
            f.setAttribute('az-submit', JSON.stringify([10, '/about']));
            f.innerHTML = '<input name="q" id="lfq">';
            document.body.prepend(f);
        });
        await page.locator('#lfq').fill('search');
        await page.locator('#lfq').press('Enter');
        await expect(page.locator('main h1')).toHaveText('About');
        await expect(page.locator('#lfq')).toHaveValue('');
    });

    // Firefox closes the socket as the load starts; the worker redials and the
    // resync replaces the form while the slow page loads. A reset deferred to the
    // unload would clear the replaced form, and Back would show the sent text.
    test('Back after a slow load shows the field empty', async ({ page }) => {
        await page.goto('/');
        await wsReady(page);
        await fullNavigateSubmit(page);
        await page.route('**/scroll-home', async (route) => {
            if (route.request().resourceType() === 'document') {
                await new Promise((r) => setTimeout(r, 3000));
            }
            await route.continue();
        });
        await field(page).fill('secret text');
        const loaded = page.waitForURL((u) => u.pathname === '/scroll-home', { timeout: 20000 });
        await field(page).press('Enter');
        await loaded;
        await wsReady(page);
        await page.goBack();
        await page.waitForURL((u) => u.pathname === '/');
        await page.waitForTimeout(500);
        await expect(field(page)).toHaveValue('');
    });

    test('a load the prompt cancelled still clears what was sent', async ({ page }) => {
        await page.goto('/');
        await wsReady(page);
        await fullNavigateSubmit(page);
        await page.evaluate(() =>
            window.addEventListener('beforeunload', (e) => {
                e.preventDefault();
                e.returnValue = '';
            }),
        );
        await page.mouse.click(5, 5);
        let prompts = 0;
        page.on('dialog', async (d) => {
            prompts++;
            await d.dismiss();
        });
        await field(page).fill('draft');
        await field(page).press('Enter');
        await expect.poll(() => prompts).toBe(1);
        await expect(field(page)).toHaveValue('');
    });

    // With the socket closed for good, a submit whose lone navigation loads the
    // page resets as it does on a live socket, before the page goes.
    test('Back after a disconnected submit loaded the page shows the field empty', async ({
        page,
    }) => {
        await page.goto('/');
        await wsReady(page);
        await page.evaluate(() =>
            document
                .getElementById('add-todo')
                .setAttribute('az-submit', JSON.stringify([10, '/scroll-home'])),
        );
        await field(page).fill('secret text');
        await page.evaluate(() => window._ws.close(1000));
        await page.waitForFunction(() =>
            document.documentElement.classList.contains('az-disconnected'),
        );
        const loaded = page.waitForURL((u) => u.pathname === '/scroll-home');
        await field(page).press('Enter');
        await loaded;
        await page.goBack();
        await page.waitForURL((u) => u.pathname === '/');
        await page.waitForTimeout(500);
        await expect(field(page)).toHaveValue('');
    });

    // An app's unsaved-changes guard runs as the load starts: the form is reset
    // before then, so the guard sees no unsent text, with the socket up or down.
    for (const socket of ['live', 'down']) {
        test(`a beforeunload guard sees the form already reset (${socket})`, async ({ page }) => {
            await page.goto('/');
            await wsReady(page);
            if (socket === 'live') {
                await fullNavigateSubmit(page);
            } else {
                await page.evaluate(() =>
                    document
                        .getElementById('add-todo')
                        .setAttribute('az-submit', JSON.stringify([10, '/scroll-home'])),
                );
            }
            await page.evaluate(() => {
                window.__seen = [];
                window.addEventListener('beforeunload', (e) => {
                    const f = document.querySelector('#add-todo input:not([type=submit])');
                    window.__seen.push(f.value);
                    e.preventDefault();
                    e.returnValue = '';
                });
            });
            await page.mouse.click(5, 5);
            page.on('dialog', (d) => d.dismiss());
            await field(page).fill('draft');
            if (socket === 'down') {
                await page.evaluate(() => window._ws.close(1000));
                await page.waitForFunction(() =>
                    document.documentElement.classList.contains('az-disconnected'),
                );
            }
            await field(page).press('Enter');
            await expect.poll(() => page.evaluate(() => window.__seen.length)).toBe(1);
            expect(await page.evaluate(() => window.__seen)).toEqual(['']);
        });
    }

    // A load starts once the submit's commands have run, and goes where its
    // command asked: an SPA navigation later in the same submit runs first, and a
    // relative load still resolves against the page it was asked on.
    test('a relative full navigate before an SPA navigate loads /about', async ({ page }) => {
        await page.goto('/');
        await wsReady(page);
        await page.evaluate(
            (c) => {
                window.__mark = 1;
                document.getElementById('add-todo').setAttribute('az-submit', JSON.stringify(c));
            },
            [
                [10, 'about', { full: true }],
                [10, '/transitions/detail'],
            ],
        );
        await field(page).fill('x');
        await field(page).press('Enter');
        await page.waitForFunction(() => window.__mark === undefined, null, { timeout: 8000 });
        await page.waitForLoadState('load');
        expect(new URL(page.url()).pathname).toBe('/about');
    });

    // A reload starts once the submit's commands have run, and reloads the page
    // the address shows then: the SPA navigation after it pushed `/about`, so
    // `/about` is reloaded. The entries keep their state, so Back and Forward
    // stay consistent with what they show.
    test('a reload before an SPA navigate reloads the page that navigation reached', async ({
        page,
    }) => {
        await page.goto('/about');
        await wsReady(page);
        // An SPA navigation onto `/`, so the entries carry an index.
        await page.evaluate(() => {
            const a = document.createElement('a');
            a.setAttribute('href', '/');
            a.setAttribute('az-navigate', '');
            document.body.appendChild(a);
            a.click();
        });
        await expect(page.locator('main h1')).toHaveText('Welcome');
        await page.evaluate(() => {
            window.__mark = 1;
            document
                .getElementById('add-todo')
                .setAttribute('az-submit', JSON.stringify([[15], [10, '/about']]));
        });
        await field(page).fill('x');
        await field(page).press('Enter');
        await page.waitForFunction(() => window.__mark === undefined, null, { timeout: 8000 });
        await page.waitForLoadState('load');
        await wsReady(page);
        expect(new URL(page.url()).pathname).toBe('/about');
        await expect(page.locator('main h1')).toHaveText('About');
        expect(typeof (await page.evaluate(() => history.state?._azIdx))).toBe('number');
        await page.goBack();
        await expect(page.locator('main h1')).toHaveText('Welcome');
        expect(new URL(page.url()).pathname).toBe('/');
        await page.goForward();
        await expect(page.locator('main h1')).toHaveText('About');
        expect(new URL(page.url()).pathname).toBe('/about');
    });

    // A reload after a fragment jump in the same submit is still a reload, of the
    // page the address shows when it starts.
    test('a reload on /#a before a jump to #b reloads /#b', async ({ page }) => {
        await page.goto('/#a');
        await wsReady(page);
        await page.evaluate(() => {
            window.__mark = 1;
            document
                .getElementById('add-todo')
                .setAttribute('az-submit', JSON.stringify([[15], [10, '#b']]));
        });
        await field(page).fill('x');
        await field(page).press('Enter');
        await page.waitForFunction(() => window.__mark === undefined, null, { timeout: 8000 });
        await page.waitForLoadState('load');
        const u = new URL(page.url());
        expect(u.pathname + u.hash).toBe('/#b');
    });

    // A reload whose prompt the user cancels leaves the page the SPA navigation
    // after it reached: the address and the screen agree.
    test('a reload the prompt cancelled before an SPA navigate leaves address and screen agreeing', async ({
        page,
    }) => {
        await page.goto('/');
        await wsReady(page);
        await page.evaluate(() => {
            window.__mark = 1;
            document
                .getElementById('add-todo')
                .setAttribute('az-submit', JSON.stringify([[15], [10, '/about']]));
            window.addEventListener('beforeunload', (e) => {
                e.preventDefault();
                e.returnValue = '';
            });
        });
        await page.mouse.click(5, 5);
        let prompts = 0;
        page.on('dialog', async (d) => {
            prompts++;
            await d.dismiss();
        });
        await field(page).fill('x');
        await field(page).press('Enter');
        await expect.poll(() => prompts).toBe(1);
        await expect(page.locator('main h1')).toHaveText('About');
        expect(new URL(page.url()).pathname).toBe('/about');
        expect(await page.evaluate(() => window.__mark)).toBe(1);
    });

    // A full navigate to a download never leaves the page, so a patch after it in
    // the same submit still runs.
    test('a patch after a full navigate to a download runs', async ({ page }) => {
        await page.route('**/export.csv', (route) =>
            route.fulfill({
                status: 200,
                contentType: 'text/csv',
                headers: { 'content-disposition': 'attachment; filename="export.csv"' },
                body: 'a,b\n1,2\n',
            }),
        );
        await page.goto('/');
        await wsReady(page);
        await page.evaluate(() => {
            window.__mark = 1;
            document.getElementById('add-todo').setAttribute(
                'az-submit',
                JSON.stringify([
                    [10, '/export.csv', { full: true }],
                    [24, '/?exported=1'],
                ]),
            );
        });
        const download = page.waitForEvent('download');
        await field(page).fill('x');
        await field(page).press('Enter');
        await download;
        await page.waitForURL((u) => u.search === '?exported=1');
        expect(await page.evaluate(() => window.__mark)).toBe(1);
        await expect(page.locator('main h1')).toHaveText('Welcome');
    });
});

// ---------------------------------------------------------------------------
// Reconnect to the page on screen
// ---------------------------------------------------------------------------

// A navigation the socket cannot carry -- issued while it is down, or owed a reply
// the close lost -- leaves the old page on screen. The reconnect resyncs THAT page,
// and the navigation goes out once it is live again.
test.describe('reconnect to the page on screen', () => {
    /** The `_az_path` of every socket the page opens, the frames it sends, its loads. */
    const track = (page) => {
        const t = { ws: [], frames: [], loads: 0 };
        page.on('websocket', (ws) => {
            t.ws.push(new URL(ws.url()).searchParams.get('_az_path'));
            ws.on('framesent', (f) => t.frames.push(String(f.payload)));
        });
        page.on('load', () => t.loads++);
        return t;
    };
    /** The navigate/patch frames sent so far. */
    const navFrames = (t) => t.frames.filter((f) => /^\["(navigate|patch)"/.test(f));

    /** Close the socket; run `commands` as effects once the page knows it is down. */
    const downThen = (page, commands) =>
        page.evaluate(async (commands) => {
            const mod = await import('/priv/arizona.min.js');
            document.addEventListener('arizona:disconnected', () => mod.applyEffects(commands), {
                once: true,
            });
            window._ws.close(4000);
        }, commands);

    /** Run `commands` with their navigate/patch frame held back, then close the socket. */
    const loseReply = (page, commands) =>
        page.evaluate(async (commands) => {
            const post = Worker.prototype.postMessage;
            Worker.prototype.postMessage = function (msg, ...rest) {
                if (msg[0] === 1 && /^\["(navigate|patch)"/.test(msg[1])) return;
                return post.call(this, msg, ...rest);
            };
            const mod = await import('/priv/arizona.min.js');
            mod.applyEffects(commands);
            Worker.prototype.postMessage = post;
            window._ws.close(4000);
        }, commands);

    test('a navigate to another view while down swaps it in after the reconnect', async ({
        page,
    }) => {
        await page.goto('/');
        await wsReady(page);
        const t = track(page);
        await downThen(page, [[10, '/about', {}]]);
        await expect(page.locator('main h1')).toHaveText('About');
        await wsReady(page);
        expect(new URL(page.url()).pathname).toBe('/about');
        expect(t.ws).toEqual(['/']);
        expect(t.loads).toBe(0);
        expect(navFrames(t)).toEqual([expect.stringMatching(/^\["navigate",\{"path":"\/about"/)]);
    });

    test('a page reached by a navigate after the reconnect reads ?reconnected false', async ({
        page,
    }) => {
        await page.goto('/');
        await wsReady(page);
        await downThen(page, [[10, '/reconnect-probe', {}]]);
        await expect(page.locator('main#reconnect_probe')).toHaveText('false');
    });

    test('a navigate whose frame was held back is sent again after the reconnect', async ({
        page,
    }) => {
        await page.goto('/');
        await wsReady(page);
        const t = track(page);
        await loseReply(page, [[10, '/about', {}]]);
        await expect(page.locator('main h1')).toHaveText('About');
        expect(t.ws).toEqual(['/']);
        expect(t.loads).toBe(0);
    });

    test('a navigate whose reply was lost after the server applied it lands too', async ({
        page,
    }) => {
        await page.goto('/');
        await wsReady(page);
        const t = track(page);
        // The frame goes out ahead of the close: the server mounts /about, and its
        // reply dies with the socket.
        await page.evaluate(async () => {
            const mod = await import('/priv/arizona.min.js');
            mod.applyEffects([[10, '/about', {}]]);
            window._ws.close(4000);
        });
        await expect(page.locator('main h1')).toHaveText('About');
        await wsReady(page);
        expect(t.ws).toEqual(['/']);
        expect(t.loads).toBe(0);
    });

    for (const [name, effects] of [
        ['while down', downThen],
        ['whose frame was held back', loseReply],
    ]) {
        test(`a same-handler patch ${name} keeps the view and the typed text`, async ({ page }) => {
            await page.goto('/ctl-parity');
            await wsReady(page);
            await page.fill('#runbound', 'typed-by-user');
            await page.evaluate(() => {
                window.__marker = 'alive';
            });
            const t = track(page);
            await effects(page, [[24, '/ctl-parity?head=1', {}]]);
            await expect.poll(() => navFrames(t).length).toBe(1);
            expect(navFrames(t)[0]).toMatch(/^\["patch",\{"path":"\/ctl-parity","qs":"head=1"/);
            await wsReady(page);
            await expect(page.locator('#runbound')).toHaveValue('typed-by-user');
            expect(await page.evaluate(() => window.__marker)).toBe('alive');
            expect(t.ws).toEqual(['/ctl-parity']);
            expect(t.loads).toBe(0);
            expect(new URL(page.url()).search).toBe('?head=1');
        });
    }

    test('a cross-handler patch while down swaps the other view in', async ({ page }) => {
        await page.goto('/');
        await wsReady(page);
        const t = track(page);
        await downThen(page, [[24, '/about', {}]]);
        await expect(page.locator('main h1')).toHaveText('About');
        expect(t.ws).toEqual(['/']);
        expect(t.loads).toBe(0);
    });

    test('a navigate across layouts whose reply was lost loads the page', async ({ page }) => {
        await page.goto('/scroll-about');
        await wsReady(page);
        const t = track(page);
        const loaded = page.waitForEvent('load');
        await loseReply(page, [[10, '/scroll-section', {}]]);
        await loaded;
        expect(new URL(page.url()).pathname).toBe('/scroll-section');
        await expect(page.locator('section.scroll-section')).toHaveCount(1);
        expect(t.ws[0]).toBe('/scroll-about');
    });

    test('a load across layouts the user cancels keeps the page on screen live', async ({
        page,
    }) => {
        await page.goto('/scroll-about');
        await wsReady(page);
        await page.evaluate(() => {
            window.__guard = (e) => {
                e.preventDefault();
                e.returnValue = '';
            };
            window.addEventListener('beforeunload', window.__guard);
        });
        // A beforeunload prompt needs a user gesture on the page.
        await page.mouse.click(5, 5);
        let prompts = 0;
        page.on('dialog', async (d) => {
            prompts++;
            await d.dismiss();
        });
        const t = track(page);
        await loseReply(page, [[10, '/scroll-section', {}]]);
        await expect.poll(() => prompts).toBe(1);
        await wsReady(page);
        expect(new URL(page.url()).pathname).toBe('/scroll-section');
        await expect(page.locator('section.scroll-section')).toHaveCount(0);
        // The page on screen is still /scroll-about: a later close reconnects to
        // it, in place, with no second prompt.
        const opened = t.ws.length;
        await page.evaluate(() => window._ws.close(4000));
        await expect.poll(() => t.ws.length).toBe(opened + 1);
        await wsReady(page);
        expect(prompts).toBe(1);
        expect(t.ws.every((p) => p === '/scroll-about')).toBe(true);
        expect(t.loads).toBe(0);
    });

    test('a navigate to an unroutable path while down loads its 404', async ({ page }) => {
        await page.goto('/');
        await wsReady(page);
        const resp = page.waitForResponse((r) => r.url().endsWith('/no-such-route'));
        await downThen(page, [[10, '/no-such-route', {}]]);
        expect((await resp).status()).toBe(404);
    });

    test('several navigates while down: the last is sent, history keeps them all', async ({
        page,
    }) => {
        await page.goto('/');
        await wsReady(page);
        const len0 = await page.evaluate(() => history.length);
        const t = track(page);
        await downThen(page, [
            [10, '/scroll-home', {}],
            [10, '/ctl-parity', {}],
            [10, '/about', {}],
        ]);
        await expect(page.locator('main h1')).toHaveText('About');
        expect(await page.evaluate(() => history.length)).toBe(len0 + 3);
        expect(navFrames(t).length).toBe(1);
        // Back is served live.
        await page.goBack();
        await expect(page.locator('#runbound')).toHaveCount(1);
        expect(new URL(page.url()).pathname).toBe('/ctl-parity');
        expect(t.ws).toEqual(['/']);
        expect(t.loads).toBe(0);
    });

    test('Back while down waits for the page on screen, then navigates', async ({ page }) => {
        await page.goto('/');
        await wsReady(page);
        await page.evaluate(async () => {
            const mod = await import('/priv/arizona.min.js');
            mod.applyEffects([[10, '/about', {}]]);
        });
        await expect(page.locator('main h1')).toHaveText('About');
        const t = track(page);
        await page.evaluate(() => {
            document.addEventListener('arizona:disconnected', () => history.back(), {
                once: true,
            });
            window._ws.close(4000);
        });
        await expect.poll(() => new URL(page.url()).pathname).toBe('/');
        await expect(page.locator('main h1')).toHaveText('Welcome');
        await wsReady(page);
        expect(t.ws).toEqual(['/about']);
        expect(t.loads).toBe(0);
        // ...and Forward while down too.
        await page.evaluate(() => {
            document.addEventListener('arizona:disconnected', () => history.forward(), {
                once: true,
            });
            window._ws.close(4000);
        });
        await expect(page.locator('main h1')).toHaveText('About');
        expect(t.ws).toEqual(['/about', '/']);
        expect(t.loads).toBe(0);
    });

    test('a navigate while down to a halting route shows the flash where it redirects', async ({
        page,
    }) => {
        await page.goto('/');
        await wsReady(page);
        const t = track(page);
        await downThen(page, [[10, '/protected', {}]]);
        await expect.poll(() => new URL(page.url()).pathname).toBe('/login');
        await expect(page.getByText('Please sign in first.')).toBeVisible();
        expect(t.ws).toEqual(['/']);
        expect(t.loads).toBe(0);
    });

    test('a navigate while down to a page that fails to mount loads it, without a loop', async ({
        page,
    }) => {
        test.setTimeout(30000);
        await page.goto('/');
        await wsReady(page);
        const t = track(page);
        const loaded = page.waitForEvent('load', { timeout: 14000 });
        await downThen(page, [[10, '/crash-on-connect', {}]]);
        // The navigate is sent once and fails to mount, so the server answers it with
        // the load of its page: the resync of the page on screen sends nothing again.
        await loaded;
        expect(new URL(page.url()).pathname).toBe('/crash-on-connect');
        expect(t.ws[0]).toBe('/');
        expect(t.ws.slice(0, -1).every((p) => p === '/')).toBe(true);
        expect(navFrames(t).length).toBe(1);
    });

    test('a navigate while down scrolls to its #fragment after it lands', async ({ page }) => {
        await page.setViewportSize({ width: 800, height: 150 });
        await page.goto('/');
        await wsReady(page);
        await downThen(page, [[10, '/about#tags', {}]]);
        await expect(page.locator('main h1')).toHaveText('About');
        await expect
            .poll(() =>
                page.evaluate(() => {
                    const top = document.getElementById('tags').getBoundingClientRect().top;
                    const max = document.documentElement.scrollHeight - innerHeight;
                    return Math.abs(top) < 5 || Math.abs(scrollY - max) < 2;
                }),
            )
            .toBe(true);
        expect(await page.evaluate(() => scrollY)).toBeGreaterThan(0);
    });

    test('a transition navigate while down animates its own swap, not the resync', async ({
        page,
    }) => {
        await page.emulateMedia({ reducedMotion: 'no-preference' });
        await page.goto('/');
        await wsReady(page);
        test.skip(
            !(await page.evaluate(() => typeof document.startViewTransition === 'function')),
            'no view transitions in this browser',
        );
        await page.evaluate(() => {
            window.__vt = [];
            const orig = document.startViewTransition.bind(document);
            document.startViewTransition = (cb) =>
                orig(async () => {
                    const before = document.querySelector('main h1')?.textContent;
                    await (typeof cb === 'function' ? cb() : cb.update());
                    window.__vt.push([before, document.querySelector('main h1')?.textContent]);
                });
        });
        await downThen(page, [[20, {}, [[10, '/about', {}]]]]);
        await expect(page.locator('main h1')).toHaveText('About');
        await expect.poll(() => page.evaluate(() => window.__vt)).toEqual([['Welcome', 'About']]);
    });

    test('an az-navigate click while down is queued, not a page load', async ({ page }) => {
        await page.goto('/');
        await wsReady(page);
        const t = track(page);
        await page.evaluate(() => {
            const a = document.createElement('a');
            a.href = '/about';
            a.setAttribute('az-navigate', '');
            a.id = 'down-link';
            a.textContent = 'go';
            document.body.prepend(a);
            window._ws.close(4000);
        });
        await page.waitForFunction(() =>
            document.documentElement.classList.contains('az-disconnected'),
        );
        await page.click('#down-link');
        await expect(page.locator('main h1')).toHaveText('About');
        expect(t.ws).toEqual(['/']);
        expect(t.loads).toBe(0);
    });

    test('a refused upgrade while HTTP answers resyncs the page in place once one opens', async ({
        page,
        context,
        baseURL,
    }) => {
        await page.goto('/upgrade-gate');
        await wsReady(page);
        await page.evaluate(() => {
            window.__marker = 'alive';
        });
        const t = track(page);
        // The route halts while the cookie is set, the WebSocket upgrade included,
        // and HTTP still answers: one refused attempt says nothing about the page.
        await context.addCookies([{ name: 'az_e2e_gate', value: '1', url: baseURL }]);
        await page.evaluate(() => window._ws.close(4000));
        // The first reconnect attempt goes out about 1 s later, and is refused.
        await page.waitForTimeout(2000);
        await context.clearCookies({ name: 'az_e2e_gate' });
        await wsReady(page);
        expect(await page.evaluate(() => window.__marker)).toBe('alive');
        expect(t.loads).toBe(0);
        expect(new URL(page.url()).pathname).toBe('/upgrade-gate');
    });

    // Behind a proxy that never upgrades the page never mounts: a navigation loads
    // its page, as with no socket at all, rather than waiting for a reconnect that
    // nothing proves will come.
    for (const how of ['an az-navigate click', 'a navigate effect']) {
        test(`${how} on a page that never mounted loads the page`, async ({
            page,
            context,
            baseURL,
        }) => {
            await context.addCookies([{ name: 'az_e2e_gate', value: '1', url: baseURL }]);
            await page.goto('/upgrade-gate');
            await page.evaluate(() => {
                const a = document.createElement('a');
                a.href = '/about';
                a.setAttribute('az-navigate', '');
                a.id = 'unmounted-link';
                a.textContent = 'go';
                document.body.prepend(a);
            });
            // Long enough for the first connect and a reconnect attempt to fail.
            await page.waitForTimeout(2000);
            expect(
                await page.evaluate(() =>
                    document.documentElement.classList.contains('az-connected'),
                ),
            ).toBe(false);
            const loaded = page.waitForEvent('load');
            if (how === 'an az-navigate click') await page.click('#unmounted-link');
            else
                await page.evaluate(async () => {
                    const mod = await import('/priv/arizona.min.js');
                    mod.applyEffects([[10, '/about', {}]]);
                });
            await loaded;
            await expect(page.locator('main h1')).toHaveText('About');
        });
    }

    test('a patch answered with a navigate to a fragment reconnects to the patched page', async ({
        page,
    }) => {
        await page.goto('/hash-patch?s=one');
        await wsReady(page);
        const t = track(page);
        await page.evaluate(async () => {
            const mod = await import('/priv/arizona.min.js');
            mod.applyEffects([[24, '/hash-patch?s=two', {}]]);
        });
        await expect(page.locator('#sec')).toHaveText('two');
        await expect.poll(() => new URL(page.url()).hash).toBe('#bottom');
        await page.evaluate(() => window._ws.close(4000));
        await expect.poll(() => t.ws.length).toBe(1);
        await wsReady(page);
        await page.waitForTimeout(500);
        await expect(page.locator('#sec')).toHaveText('two');
        expect(new URL(page.url()).search).toBe('?s=two');
        expect(navFrames(t)).toEqual([]);
    });

    test('a remount cancels a mount-failure reload after the app rewrote its own URL', async ({
        page,
        context,
        baseURL,
    }) => {
        test.setTimeout(40000);
        await page.goto('/flaky-mount?utm_source=x');
        await wsReady(page);
        const t = track(page);
        // The app strips its tracking params itself: the same page, a new address.
        await page.evaluate(() => history.replaceState(history.state, '', '/flaky-mount'));
        await page.evaluate(() => {
            window.__reloadIn = null;
            document.addEventListener('arizona:disconnected', (e) => {
                if (e.detail.reloadInMs !== null) window.__reloadIn = e.detail.reloadInMs;
            });
        });
        await context.addCookies([{ name: 'az_e2e_flaky', value: '1', url: baseURL }]);
        await page.evaluate(() => window._ws.close(4000));
        // A reconnect fails to mount (4501) and schedules a reload...
        await page.waitForFunction(() => window.__reloadIn !== null, null, { timeout: 10000 });
        await context.clearCookies({ name: 'az_e2e_flaky' });
        // ...which the next reconnect's successful mount of this page cancels.
        await wsReady(page);
        await page.waitForTimeout(11000);
        expect(t.loads).toBe(0);
    });

    test('a patch that crashes the view loads its page once', async ({ page }) => {
        // The server answers the patch it could not complete with the load of its
        // page, where a fresh mount of the same URL renders fine. It never goes out
        // over the socket again.
        test.setTimeout(30000);
        await page.goto('/patch-crash?q=a');
        await wsReady(page);
        const t = track(page);
        const loaded = page.waitForEvent('load', { timeout: 10000 });
        await page.evaluate(async () => {
            const mod = await import('/priv/arizona.min.js');
            mod.applyEffects([[24, '/patch-crash?boom=1', {}]]);
        });
        await loaded;
        expect(new URL(page.url()).search).toBe('?boom=1');
        await expect(page.locator('#q')).toHaveText('none');
        await wsReady(page);
        await page.waitForTimeout(3000);
        expect(t.loads).toBe(1);
        expect(navFrames(t)).toEqual([]);
    });

    // An upgrade-only outage after a mount (an auth gate halting the upgrade): a
    // link waits for the attempt in flight, then loads once one started after it
    // fails, as main loads a link while disconnected.
    test('a link during an upgrade-only outage loads its page once an attempt fails', async ({
        page,
        context,
        baseURL,
    }) => {
        test.setTimeout(30000);
        await page.goto('/upgrade-gate');
        await wsReady(page);
        await page.evaluate(() => {
            const a = document.createElement('a');
            a.href = '/about';
            a.setAttribute('az-navigate', '');
            a.id = 'gated-link';
            a.textContent = 'go';
            document.body.prepend(a);
        });
        await context.addCookies([{ name: 'az_e2e_gate', value: '1', url: baseURL }]);
        await page.evaluate(() => window._ws.close(4000));
        await page.waitForFunction(() =>
            document.documentElement.classList.contains('az-disconnected'),
        );
        const loaded = page.waitForEvent('load', { timeout: 15000 });
        await page.click('#gated-link');
        await loaded;
        await expect(page.locator('main h1')).toHaveText('About');
    });

    // A navigate that crashes the view as it unmounts it fails on the server, which
    // answers it with the load of its page: exactly one load, and the navigate never
    // goes out over the socket again.
    test('a navigate that crashes the view as it leaves loads its page once', async ({ page }) => {
        test.setTimeout(30000);
        await page.goto('/crash-nav-leave');
        await wsReady(page);
        const t = track(page);
        const loaded = page.waitForEvent('load', { timeout: 10000 });
        await page.click('#leave-about');
        await loaded;
        expect(new URL(page.url()).pathname).toBe('/about');
        await expect(page.locator('main h1')).toHaveText('About');
        await wsReady(page);
        await page.waitForTimeout(3000);
        expect(t.loads).toBe(1);
        // Sent on the socket open before tracking began; no later socket carries it.
        expect(navFrames(t)).toEqual([]);
    });

    // A crashing event and a navigate behind it: the navigate did not run (the server
    // answers no frame queued behind a crash), so it goes out again once the page on
    // screen is live -- over the socket, with no page load.
    test('a crash event before a navigate: the navigate lands after the reconnect', async ({
        page,
    }) => {
        test.setTimeout(30000);
        await page.goto('/crash-nav');
        await wsReady(page);
        const t = track(page);
        await page.click('#crash-about');
        await expect(page.locator('main h1')).toHaveText('About', { timeout: 10000 });
        expect(new URL(page.url()).pathname).toBe('/about');
        await page.waitForTimeout(2000);
        expect(t.loads).toBe(0);
        // Sent first on the socket open before tracking began, then once on the next.
        expect(navFrames(t).length).toBe(1);
    });

    // The same to the same view under other layouts: the navigate goes out again,
    // and the server, which cannot serve other layouts in place, answers it with
    // the load of its page -- a navigate or a patch alike.
    for (const button of ['#crash-twin', '#crash-twin-patch']) {
        test(`a crash event before ${button} to the same view under other layouts loads it`, async ({
            page,
        }) => {
            test.setTimeout(30000);
            await page.goto('/crash-nav');
            await wsReady(page);
            const t = track(page);
            const loaded = page.waitForEvent('load', { timeout: 15000 });
            await page.click(button);
            await loaded;
            expect(new URL(page.url()).pathname).toBe('/crash-nav-twin');
            await expect(page.locator('section.scroll-section #crash_nav')).toHaveCount(1);
            await wsReady(page);
            await page.waitForTimeout(2000);
            expect(t.loads).toBe(1);
        });
    }

    // The view crashes and navigates to its own page on every mount. The navigate
    // names the page on screen, which the reconnect already mounts, so the page
    // keeps reconnecting under the worker's backoff -- 1, 2, 5, then 10 s -- with no
    // page load, as a view that crashes after every mount does.
    test('a crash that recurs after every mount settles into the reconnect backoff', async ({
        page,
    }) => {
        test.setTimeout(45000);
        const opened = [];
        page.on('websocket', () => opened.push(Date.now()));
        await page.goto('/crash-nav-loop');
        let loads = 0;
        page.on('load', () => loads++);
        await page.waitForTimeout(25000);
        expect(loads).toBe(0);
        expect(new URL(page.url()).pathname).toBe('/crash-nav-loop');
        // Backing off: a handful of sockets in 25 s, their gaps growing.
        expect(opened.length).toBeGreaterThanOrEqual(3);
        expect(opened.length).toBeLessThanOrEqual(8);
        const gaps = opened.slice(1).map((t, n) => t - opened[n]);
        expect(gaps.at(-1)).toBeGreaterThan(gaps[0]);
    });

    // Two pages crash and navigate to each other on every connect. Each navigate is
    // innocent (the crash event before it is what failed), so it goes out again
    // over the socket: the pages swap over the socket under the worker's backoff,
    // and no page ever loads.
    test('a crash loop between two pages stays on the socket, loading nothing', async ({
        page,
    }) => {
        test.setTimeout(45000);
        const opened = [];
        page.on('websocket', () => opened.push(Date.now()));
        await page.goto('/crash-nav-ping');
        let loads = 0;
        page.on('load', () => loads++);
        await page.waitForTimeout(25000);
        expect(loads).toBe(0);
        expect(opened.length).toBeGreaterThanOrEqual(3);
        expect(opened.length).toBeLessThanOrEqual(8);
        const gaps = opened.slice(1).map((t, n) => t - opened[n]);
        expect(gaps.at(-1)).toBeGreaterThan(gaps[0]);
    });

    // Two pages navigate to each other on every connect, and each navigate fails on
    // the server while the page loads fine over HTTP: every navigate is answered
    // with a page load. The loads answering navigations issued with no person
    // acting are bounded per tab: once 10 fall within 5 min, each waits 30 s, said once, and
    // the loop then stays at that pace.
    test('a navigate-and-load loop between two pages settles at one load per 30 s', async ({
        page,
    }) => {
        test.setTimeout(150000);
        const loads = [];
        page.on('load', () => loads.push(Date.now()));
        let failsafeAt = 0;
        page.on('console', (m) => {
            if (m.type() === 'error' && m.text().includes('loading every 30 s') && !failsafeAt)
                failsafeAt = Date.now();
        });
        await page.goto('/crash-nav-bounce-a');
        await expect
            .poll(() => failsafeAt, { timeout: 60000, intervals: [500] })
            .toBeGreaterThan(0);
        const before = loads.length;
        expect(before).toBeLessThanOrEqual(12);
        // The next two loads each come 30 s after the one before: a steady pace.
        await expect
            .poll(() => loads.length, { timeout: 100000, intervals: [1000] })
            .toBeGreaterThanOrEqual(before + 2);
        expect(loads[before] - failsafeAt).toBeGreaterThan(25000);
        expect(loads[before + 1] - loads[before]).toBeGreaterThan(25000);
    });

    // The same loop with the page's own code sending an event after each mount --
    // an `arizona:connected` listener, a hook a moment after it mounts. No person
    // acted, so the loop reaches the failsafe all the same.
    for (const ping of ['connected', 'hooklater']) {
        test(`a navigate-and-load loop whose page sends on its own (${ping}) is bounded`, async ({
            page,
        }) => {
            test.setTimeout(120000);
            let failsafeAt = 0;
            page.on('console', (m) => {
                if (m.type() === 'error' && m.text().includes('loading every 30 s') && !failsafeAt)
                    failsafeAt = Date.now();
            });
            const loads = [];
            page.on('load', () => loads.push(Date.now()));
            await page.goto(`/crash-nav-ping-${ping}-a`);
            await expect
                .poll(() => failsafeAt, { timeout: 90000, intervals: [500] })
                .toBeGreaterThan(0);
            expect(loads.length).toBeLessThanOrEqual(12);
        });
    }

    // The same loop with the page's own code writing history: a `location.hash`
    // write, or its own entry and `history.back()`. Fragment-only traversals are no
    // person, so the loop reaches the failsafe all the same.
    for (const [name, script] of [
        [
            'hash',
            "document.addEventListener('arizona:connected', () => { location.hash = 't' + Date.now(); });",
        ],
        [
            'back',
            "document.addEventListener('arizona:connected', () => { history.pushState(history.state, '', '#p'); history.back(); });",
        ],
    ]) {
        test(`a navigate-and-load loop whose page writes history on its own (${name}) is bounded`, async ({
            page,
        }) => {
            test.setTimeout(120000);
            await page.addInitScript(script);
            let failsafeAt = 0;
            page.on('console', (m) => {
                if (m.type() === 'error' && m.text().includes('loading every 30 s') && !failsafeAt)
                    failsafeAt = Date.now();
            });
            const loads = [];
            page.on('load', () => loads.push(Date.now()));
            await page.goto('/crash-nav-ping-connected-a');
            await expect
                .poll(() => failsafeAt, { timeout: 90000, intervals: [500] })
                .toBeGreaterThan(0);
            expect(loads.length).toBeLessThanOrEqual(12);
        });
    }

    // Back is a person only when the browser says it started it (the Navigation
    // API's `userInitiated`). The crash page is reached by the page's own untrusted
    // click, so only the Back can count. Chromium reports Playwright's `goBack()` as
    // user-initiated: the load comes at once. Firefox (headless, Juggler) reports it
    // as not user-initiated, so there it is bounded like any traversal no person
    // started: it waits out the failsafe.
    test('a Back the browser started is a person, one it did not is bounded', async ({
        page,
        browserName,
    }) => {
        test.setTimeout(90000);
        await page.goto('/about');
        await wsReady(page);
        await page.evaluate(() => {
            const a = document.createElement('a');
            a.id = 'to-leave';
            a.href = '/crash-nav-leave';
            a.setAttribute('az-navigate', '');
            a.textContent = 'leave';
            document.body.prepend(a);
            setTimeout(() => a.click(), 0); // the page's own: untrusted
        });
        await expect(page.locator('#crash_nav')).toBeVisible();
        await wsReady(page);
        await page.evaluate(() =>
            sessionStorage.setItem(
                'arizona:nav-loads',
                JSON.stringify(Array(10).fill({ t: Date.now(), slow: false })),
            ),
        );
        const t0 = Date.now();
        const loaded = page.waitForEvent('load', { timeout: 60000 });
        await page.goBack();
        await loaded;
        expect(new URL(page.url()).pathname).toBe('/about');
        if (browserName === 'chromium') expect(Date.now() - t0).toBeLessThan(10000);
        else expect(Date.now() - t0).toBeGreaterThan(25000);
    });

    // A page's own code that fires trusted `input`/`change` events -- a checkbox's
    // scripted click(), `execCommand` -- is no person: the loop stays bounded.
    for (const [name, script] of [
        [
            'checkbox',
            "document.addEventListener('arizona:connected', () => { const c = document.createElement('input'); c.type = 'checkbox'; document.body.append(c); c.click(); });",
        ],
        [
            'execCommand',
            "document.addEventListener('arizona:connected', () => { const i = document.createElement('input'); document.body.append(i); i.focus(); document.execCommand('insertText', false, 'x'); });",
        ],
    ]) {
        test(`a navigate-and-load loop whose page fires trusted input itself (${name}) is bounded`, async ({
            page,
        }) => {
            test.setTimeout(120000);
            await page.addInitScript(script);
            let failsafeAt = 0;
            page.on('console', (m) => {
                if (m.type() === 'error' && m.text().includes('loading every 30 s') && !failsafeAt)
                    failsafeAt = Date.now();
            });
            const loads = [];
            page.on('load', () => loads.push(Date.now()));
            await page.goto('/crash-nav-ping-connected-a');
            await expect
                .poll(() => failsafeAt, { timeout: 90000, intervals: [500] })
                .toBeGreaterThan(0);
            expect(loads.length).toBeLessThanOrEqual(12);
        });
    }

    // A `full` navigate effect to a fragment of the page on screen goes by URL, as
    // the browser takes it: a same-document scroll, no load, the fragment kept.
    for (const [from, to] of [
        ['/about', '/about#x'],
        ['/about#a', '/about#b'],
    ]) {
        test(`a full navigate from ${from} to ${to} only scrolls`, async ({ page }) => {
            await page.goto(from);
            await wsReady(page);
            const docs = [];
            page.on('request', (r) => {
                if (r.resourceType() === 'document') docs.push(r.url());
            });
            await page.evaluate(async (t) => {
                window.__kept = 1;
                const mod = await import('/priv/arizona.min.js');
                mod.applyEffects([[10, t, { full: true }]]);
            }, to);
            await expect
                .poll(() => page.evaluate(() => location.hash))
                .toBe(new URL(to, 'http://x').hash);
            await page.waitForTimeout(1000);
            expect(docs).toEqual([]);
            expect(await page.evaluate(() => window.__kept ?? null)).toBe(1);
        });
    }

    // A person's click inside a Document Picture-in-Picture window is a person too:
    // the step it raises, answered with a load, is never delayed, the record full.
    test('a person acting in a Picture-in-Picture window is never delayed', async ({
        page,
        context,
        browserName,
    }) => {
        test.skip(browserName !== 'chromium', 'Document Picture-in-Picture is Chromium-only');
        test.setTimeout(60000);
        await page.goto('/wizard-1');
        await wsReady(page);
        await page.evaluate(() =>
            sessionStorage.setItem(
                'arizona:nav-loads',
                JSON.stringify(Array(10).fill({ t: Date.now(), slow: false })),
            ),
        );
        const opened = context.waitForEvent('page', { timeout: 10000 });
        await page.evaluate(() => document.getElementById('pop').click());
        const pip = await opened;
        await pip.waitForSelector('#next');
        const t0 = Date.now();
        const loaded = page.waitForEvent('load', { timeout: 15000 });
        await pip.click('#next');
        await loaded;
        expect(Date.now() - t0).toBeLessThan(10000);
        await expect(page.locator('#step')).toHaveText('2');
    });

    // A person's chain: a click navigates live to a page whose view then navigates
    // across layouts on its own (answered with a load), and a click back. Every
    // load answers the person's click, so none is ever delayed.
    test("a person's chain through a live hop is never delayed", async ({ page }) => {
        test.setTimeout(120000);
        await page.goto('/wizard-1');
        await wsReady(page);
        await page.evaluate(() => sessionStorage.clear());
        let errors = 0;
        page.on('console', (m) => {
            if (m.type() === 'error' && m.text().includes('[arizona]')) errors++;
        });
        for (let i = 0; i < 13; i++) {
            const t0 = Date.now();
            await page.click('#tor');
            await expect(page.locator('#step')).toHaveText('2', { timeout: 10000 });
            await wsReady(page);
            expect(Date.now() - t0).toBeLessThan(10000);
            await page.click('#to1');
            await expect(page.locator('#step')).toHaveText('1', { timeout: 10000 });
            await wsReady(page);
        }
        expect(errors).toBe(0);
    });

    // The user crossing layout families: every click is answered with a full load,
    // which the bound never counts, since the user issued the navigation.
    test('a user crossing layouts 20 times is never delayed', async ({ page }) => {
        test.setTimeout(120000);
        await page.goto('/crash-nav');
        await wsReady(page);
        let errors = 0;
        page.on('console', (m) => {
            if (m.type() === 'error' && m.text().includes('[arizona]')) errors++;
        });
        for (let i = 0; i < 20; i++) {
            const to = i % 2 === 0 ? '/crash-nav-twin' : '/crash-nav';
            await page.evaluate((href) => {
                const a = document.createElement('a');
                a.id = 'cross';
                a.href = href;
                a.setAttribute('az-navigate', '');
                a.textContent = 'cross';
                document.body.prepend(a);
            }, to);
            const t0 = Date.now();
            const loaded = page.waitForEvent('load', { timeout: 10000 });
            await page.click('#cross');
            await loaded;
            await wsReady(page);
            expect(new URL(page.url()).pathname).toBe(to);
            expect(Date.now() - t0).toBeLessThan(10000);
        }
        expect(errors).toBe(0);
    });

    // A wizard whose every step's handler navigates across layouts: each of the
    // user's clicks is answered with a page load. A person acted, so it is no loop,
    // however many steps: none waits.
    test('a user stepping through server redirects across layouts is never delayed', async ({
        page,
    }) => {
        test.setTimeout(120000);
        await page.goto('/wizard-1');
        await wsReady(page);
        await page.evaluate(() => sessionStorage.clear());
        let errors = 0;
        page.on('console', (m) => {
            if (m.type() === 'error' && m.text().includes('[arizona]')) errors++;
        });
        for (let i = 0; i < 13; i++) {
            const step = i % 2 === 0 ? '2' : '1';
            const t0 = Date.now();
            const loaded = page.waitForEvent('load', { timeout: 10000 });
            await page.click('#next');
            await loaded;
            await expect(page.locator('#step')).toHaveText(step);
            await wsReady(page);
            expect(Date.now() - t0).toBeLessThan(10000);
        }
        expect(errors).toBe(0);
    });

    // A link to page#fragment the server answers with a load (other layouts): the
    // frame carries no fragment, so the load reloads the entry the link pushed,
    // which keeps it and lands at it.
    test('a navigation to page#fragment answered with a load lands at the fragment', async ({
        page,
    }) => {
        await page.goto('/wizard-1');
        await wsReady(page);
        const loaded = page.waitForEvent('load', { timeout: 10000 });
        await page.click('#tofrag');
        await loaded;
        await expect(page.locator('#step')).toHaveText('2');
        expect(new URL(page.url()).hash).toBe('#sec');
        await expect.poll(() => page.evaluate(() => window.scrollY)).toBeGreaterThan(1000);
    });

    // A load waiting out the failsafe must not override a newer navigation the
    // server then served live: `/wizard-auto` navigates across layouts on connect
    // (the record already full, so that load waits), then 2 s later to a page under
    // its own layouts, which goes live.
    test('a waiting load never overrides a newer navigation served live', async ({ page }) => {
        test.setTimeout(60000);
        await page.addInitScript(() => {
            if (!sessionStorage.getItem('az-e2e-seeded')) {
                sessionStorage.setItem('az-e2e-seeded', '1');
                sessionStorage.setItem(
                    'arizona:nav-loads',
                    JSON.stringify(Array(10).fill({ t: Date.now(), slow: false })),
                );
            }
        });
        let loads = 0;
        page.on('load', () => loads++);
        await page.goto('/wizard-auto');
        await expect(page.locator('#step')).toHaveText('q', { timeout: 10000 });
        await page.waitForTimeout(33000);
        expect(loads).toBe(1);
        expect(new URL(page.url()).pathname).toBe('/wizard-q');
        await expect(page.locator('#step')).toHaveText('q');
    });

    // The load answering a failed navigate: the user lets it through or no prompt is
    // shown, and it reloads the address bar's own entry, which keeps its state
    // (`_azIdx`), Back and Forward as with no failure at all; or the user cancels it,
    // and the page on screen stays live under the address of the page it asked
    // for, the navigation settled: nothing re-arms, and no second prompt comes.
    // `/crash-nav-slow` answers late, so the navigate is in flight a while.
    for (const mode of ['none', 'accept', 'dismiss']) {
        test(`the load answering a failed navigate, prompt: ${mode}`, async ({ page }) => {
            test.setTimeout(60000);
            await page.goto('/crash-nav-leave');
            await wsReady(page);
            if (mode !== 'none') {
                await page.evaluate(() => {
                    window.addEventListener('beforeunload', (e) => {
                        e.preventDefault();
                        e.returnValue = '';
                    });
                });
            }
            await page.mouse.click(5, 5); // a beforeunload prompt needs a user gesture
            let prompts = 0;
            page.on('dialog', async (d) => {
                prompts++;
                await (mode === 'dismiss' ? d.dismiss() : d.accept());
            });
            let loads = 0;
            page.on('load', () => loads++);
            // A traversal that loads a document destroys the context mid-read.
            const state = () =>
                page
                    .evaluate(() => ({
                        path: location.pathname,
                        idx: history.state?._azIdx ?? null,
                        len: history.length,
                    }))
                    .catch(() => null);
            await page.click('#leave-slow');
            await expect.poll(async () => (await state())?.path).toBe('/crash-nav-slow');
            const before = await state();
            expect(typeof before.idx).toBe('number');
            if (mode === 'dismiss') {
                await expect.poll(() => prompts, { timeout: 15000 }).toBe(1);
                // The 4500 that follows the answer reconnects the page on screen.
                await expect(page.locator('#crash_nav')).toBeVisible();
                await expect
                    .poll(() =>
                        page.evaluate(() =>
                            document.documentElement.classList.contains('az-connected'),
                        ),
                    )
                    .toBe(true);
                await page.waitForTimeout(12000);
                expect(prompts).toBe(1);
                expect(loads).toBe(0);
                expect(await state()).toEqual(before);
                await expect(page.locator('html')).toHaveClass(/az-connected/);
                return;
            }
            await expect.poll(() => loads, { timeout: 15000 }).toBe(1);
            expect(prompts).toBe(mode === 'accept' ? 1 : 0);
            await wsReady(page);
            expect(await state()).toEqual(before);
            await page.evaluate(() => history.back());
            await expect
                .poll(state)
                .toEqual({ ...before, path: '/crash-nav-leave', idx: before.idx - 1 });
            await page.evaluate(() => history.forward()).catch(() => {});
            await expect.poll(state).toEqual(before);
        });
    }

    // Newer wins: a navigate the server answers with the load of its page, after
    // the user issued a newer one. The load must not override the newer navigate,
    // which, queued behind the failure, goes out again once the page on screen is
    // live. `/crash-nav-slow` answers late, so the newer one is issued before.
    test('the load answering an older navigate never overrides a newer one', async ({ page }) => {
        test.setTimeout(30000);
        await page.goto('/crash-nav');
        await wsReady(page);
        await page.click('#arm');
        await expect(page.locator('#arm')).toHaveText('armed');
        const t = track(page);
        await page.evaluate(async () => {
            const mod = await import('/priv/arizona.min.js');
            mod.applyEffects([[10, '/crash-nav-slow', {}]]);
            mod.applyEffects([[10, '/about', {}]]);
        });
        await expect(page.locator('main h1')).toHaveText('About', { timeout: 15000 });
        await page.waitForTimeout(3000);
        expect(new URL(page.url()).pathname).toBe('/about');
        expect(t.loads).toBe(0);
    });
});
