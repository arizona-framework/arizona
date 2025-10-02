import { test, expect } from '@playwright/test';

test.describe.configure({ mode: 'serial' });

test.describe('Arizona Counter Pubsub', () => {
  test('should sync increments between tabs in realtime', async ({ page, context }) => {
    // Open first tab with realtime enabled
    await page.goto('/counter?realtime');
    await expect(page.getByTestId('count')).toHaveText('0');

    // Open second tab with realtime enabled
    const secondTab = await context.newPage();
    await secondTab.goto('/counter?realtime');
    await expect(secondTab.getByTestId('count')).toHaveText('0');

    // Increment in first tab
    await page.getByTestId('increment').click();
    await expect(page.getByTestId('count')).toHaveText('1');

    // Verify second tab also increments via pubsub
    await expect(secondTab.getByTestId('count')).toHaveText('1', { timeout: 2000 });

    // Increment in second tab
    await secondTab.getByTestId('increment').click();
    await expect(secondTab.getByTestId('count')).toHaveText('2');

    // Verify first tab syncs
    await expect(page.getByTestId('count')).toHaveText('2', { timeout: 2000 });

    await secondTab.close();
  });

  test('should sync decrements between tabs in realtime', async ({ page, context }) => {
    // Open two tabs with realtime enabled
    await page.goto('/counter?realtime');
    const secondTab = await context.newPage();
    await secondTab.goto('/counter?realtime');

    // Increment to 2 in first tab
    await page.getByTestId('increment').click();
    await expect(secondTab.getByTestId('count')).toHaveText('1', { timeout: 2000 });
    await page.getByTestId('increment').click();
    await expect(secondTab.getByTestId('count')).toHaveText('2', { timeout: 2000 });

    // Decrement in second tab - this uses increment with incr: -1
    await secondTab.getByTestId('decrement').click();
    await expect(secondTab.getByTestId('count')).toHaveText('1');

    // Verify first tab syncs the decrement (since decrement triggers increment event)
    await expect(page.getByTestId('count')).toHaveText('1', { timeout: 2000 });

    await secondTab.close();
  });

  test('should NOT sync reset between tabs (reset is local only)', async ({ page, context }) => {
    // Open two tabs with realtime enabled
    await page.goto('/counter?realtime');
    const secondTab = await context.newPage();
    await secondTab.goto('/counter?realtime');

    // Increment both tabs to non-zero values
    await page.getByTestId('increment').click();
    await expect(secondTab.getByTestId('count')).toHaveText('1', { timeout: 2000 });

    // Reset in first tab - should NOT sync to second tab
    await page.getByTestId('reset').click();
    await expect(page.getByTestId('count')).toHaveText('0');

    // Second tab should remain at 1 (reset doesn't broadcast)
    await page.waitForTimeout(500);
    await expect(secondTab.getByTestId('count')).toHaveText('1');

    await secondTab.close();
  });

  test('should handle rapid increments between tabs consistently', async ({ page, context }) => {
    // Open two tabs with realtime enabled
    await page.goto('/counter?realtime');
    const secondTab = await context.newPage();
    await secondTab.goto('/counter?realtime');

    // Perform rapid increments from both tabs
    const promises = [];

    // Tab 1: increment 3 times
    for (let i = 0; i < 3; i++) {
      promises.push(page.getByTestId('increment').click());
    }

    // Tab 2: increment 2 times
    for (let i = 0; i < 2; i++) {
      promises.push(secondTab.getByTestId('increment').click());
    }

    // Wait for all clicks to complete
    await Promise.all(promises);

    // Wait a bit for all pubsub messages to propagate
    await page.waitForTimeout(1000);

    // Both tabs should show the same final count (5)
    await expect(page.getByTestId('count')).toHaveText('5', { timeout: 2000 });
    await expect(secondTab.getByTestId('count')).toHaveText('5', { timeout: 2000 });

    await secondTab.close();
  });

  test('should not sync with non-realtime tabs', async ({ page, context }) => {
    // Open first tab WITHOUT realtime
    await page.goto('/counter');
    await expect(page.getByTestId('count')).toHaveText('0');

    // Open second tab WITH realtime
    const realtimeTab = await context.newPage();
    await realtimeTab.goto('/counter?realtime');
    await expect(realtimeTab.getByTestId('count')).toHaveText('0');

    // Increment in realtime tab
    await realtimeTab.getByTestId('increment').click();
    await expect(realtimeTab.getByTestId('count')).toHaveText('1');

    // Wait a bit and verify non-realtime tab is NOT affected
    await page.waitForTimeout(500);
    await expect(page.getByTestId('count')).toHaveText('0');

    // Increment in non-realtime tab
    await page.getByTestId('increment').click();
    await expect(page.getByTestId('count')).toHaveText('1');

    // Verify realtime tab is NOT affected by non-realtime tab
    await page.waitForTimeout(500);
    await expect(realtimeTab.getByTestId('count')).toHaveText('1');

    await realtimeTab.close();
  });

  test('should handle tab close and reopen scenarios', async ({ page, context }) => {
    // Open two realtime tabs
    await page.goto('/counter?realtime');
    let secondTab = await context.newPage();
    await secondTab.goto('/counter?realtime');

    // Increment in first tab
    await page.getByTestId('increment').click();
    await expect(secondTab.getByTestId('count')).toHaveText('1', { timeout: 2000 });

    // Close second tab
    await secondTab.close();

    // Continue incrementing in first tab
    await page.getByTestId('increment').click();
    await expect(page.getByTestId('count')).toHaveText('2');

    // Open new second tab
    secondTab = await context.newPage();
    await secondTab.goto('/counter?realtime');

    // New tab should start at 0 (independent counter instance)
    await expect(secondTab.getByTestId('count')).toHaveText('0');

    // But they should sync future updates
    await page.getByTestId('increment').click();
    await expect(page.getByTestId('count')).toHaveText('3');
    await expect(secondTab.getByTestId('count')).toHaveText('1', { timeout: 2000 });

    await secondTab.close();
  });
});
