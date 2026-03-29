import { test, expect } from '@playwright/test';

const wsReady = (page) =>
    page.waitForFunction(() =>
        document.documentElement.classList.contains('az-connected'));

const messageItems = (page) =>
    page.locator('#messages li');

const chatInput = (page) => page.locator('#chat-form input[name="text"]');

const sendMsg = async (page, text) => {
    await chatInput(page).fill(text);
    await page.click('#chat-form button[type="submit"]');
};

test.describe.serial('PubSub chat', () => {

    test('message sent by tab A appears in tab B', async ({ browser }) => {
        const ctx1 = await browser.newContext();
        const ctx2 = await browser.newContext();
        const pageA = await ctx1.newPage();
        const pageB = await ctx2.newPage();

        await pageA.goto('/chat');
        await pageB.goto('/chat');
        await wsReady(pageA);
        await wsReady(pageB);

        // Tab A sends a message
        await sendMsg(pageA, 'hello from A');

        // Tab A sees its own message locally
        await expect(messageItems(pageA)).toHaveCount(1);

        // Tab B receives it via pubsub
        await expect(messageItems(pageB)).toHaveCount(1, { timeout: 3000 });

        await ctx1.close();
        await ctx2.close();
    });

    test('sender does not receive duplicate via pubsub', async ({ browser }) => {
        const ctx1 = await browser.newContext();
        const ctx2 = await browser.newContext();
        const pageA = await ctx1.newPage();
        const pageB = await ctx2.newPage();

        await pageA.goto('/chat');
        await pageB.goto('/chat');
        await wsReady(pageA);
        await wsReady(pageB);

        await sendMsg(pageA, 'no dup');
        await expect(messageItems(pageB)).toHaveCount(1, { timeout: 3000 });

        // Wait a bit and verify sender still has exactly 1 (no duplicate)
        await pageA.waitForTimeout(500);
        await expect(messageItems(pageA)).toHaveCount(1);

        await ctx1.close();
        await ctx2.close();
    });

    test('input accepts custom text', async ({ browser }) => {
        const ctx = await browser.newContext();
        const page = await ctx.newPage();

        await page.goto('/chat');
        await wsReady(page);

        await sendMsg(page, 'hi there');
        await expect(messageItems(page)).toHaveCount(1);
        await expect(messageItems(page).locator('span').first()).toHaveText('hi there');

        await ctx.close();
    });

    test('bidirectional: both tabs can send and receive', async ({ browser }) => {
        const ctx1 = await browser.newContext();
        const ctx2 = await browser.newContext();
        const pageA = await ctx1.newPage();
        const pageB = await ctx2.newPage();

        await pageA.goto('/chat');
        await pageB.goto('/chat');
        await wsReady(pageA);
        await wsReady(pageB);

        // A sends
        await sendMsg(pageA, 'from A');
        await expect(messageItems(pageB)).toHaveCount(1, { timeout: 3000 });

        // B sends
        await sendMsg(pageB, 'from B');
        await expect(messageItems(pageA)).toHaveCount(2, { timeout: 3000 });
        // B has: 1 received from A + 1 sent locally = 2
        await expect(messageItems(pageB)).toHaveCount(2);

        // A sees delete only on its own message
        const aButtons = messageItems(pageA).locator('button[az-click*="delete"]');
        await expect(aButtons).toHaveCount(1);

        // B sees delete only on its own message
        const bButtons = messageItems(pageB).locator('button[az-click*="delete"]');
        await expect(bButtons).toHaveCount(1);

        await ctx1.close();
        await ctx2.close();
    });

    test('broadcast to empty group does not crash', async ({ browser }) => {
        const ctx = await browser.newContext();
        const page = await ctx.newPage();

        await page.goto('/chat');
        await wsReady(page);

        // Only one subscriber (self). broadcast_from excludes self,
        // so effectively broadcasting to empty set.
        await sendMsg(page, 'solo msg');

        // Sender still sees its own local message
        await expect(messageItems(page)).toHaveCount(1);

        await ctx.close();
    });

    test('delete button only visible to sender', async ({ browser }) => {
        const ctx1 = await browser.newContext();
        const ctx2 = await browser.newContext();
        const pageA = await ctx1.newPage();
        const pageB = await ctx2.newPage();

        await pageA.goto('/chat');
        await pageB.goto('/chat');
        await wsReady(pageA);
        await wsReady(pageB);

        // Tab A sends a message
        await sendMsg(pageA, 'owner test');
        await expect(messageItems(pageA)).toHaveCount(1);
        await expect(messageItems(pageB)).toHaveCount(1, { timeout: 3000 });

        // Tab A sees the delete button on its own message
        await expect(messageItems(pageA).locator('button[az-click*="delete"]')).toHaveCount(1);

        // Tab B does NOT see a delete button (not the owner)
        await expect(messageItems(pageB).locator('button[az-click*="delete"]')).toHaveCount(0);

        await ctx1.close();
        await ctx2.close();
    });

    test('delete removes message from sender', async ({ browser }) => {
        const ctx = await browser.newContext();
        const page = await ctx.newPage();

        await page.goto('/chat');
        await wsReady(page);

        await sendMsg(page, 'to delete');
        await expect(messageItems(page)).toHaveCount(1);

        // Click delete button on the message
        await page.click('#messages li button[az-click*="delete"]');
        await expect(messageItems(page)).toHaveCount(0);

        await ctx.close();
    });

    test('delete propagates to other tabs', async ({ browser }) => {
        const ctx1 = await browser.newContext();
        const ctx2 = await browser.newContext();
        const pageA = await ctx1.newPage();
        const pageB = await ctx2.newPage();

        await pageA.goto('/chat');
        await pageB.goto('/chat');
        await wsReady(pageA);
        await wsReady(pageB);

        // A sends a message
        await sendMsg(pageA, 'will be deleted');
        await expect(messageItems(pageB)).toHaveCount(1, { timeout: 3000 });

        // A deletes the message
        await pageA.click('#messages li button[az-click*="delete"]');
        await expect(messageItems(pageA)).toHaveCount(0);

        // B sees deletion via pubsub
        await expect(messageItems(pageB)).toHaveCount(0, { timeout: 3000 });

        await ctx1.close();
        await ctx2.close();
    });
});
