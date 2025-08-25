import { test, expect } from '@playwright/test';
import { waitForCondition, collectWebSocketMessages } from '../test-utils.js';

test.describe('Arizona Counter App', () => {
  test('should load counter page and display initial state', async ({ page }) => {
    await page.goto('/counter');

    // Check that the page loads with initial counter value
    await expect(page.getByTestId('count')).toHaveText('0');
    await expect(page.getByTestId('increment')).toBeVisible();
    await expect(page.getByTestId('decrement')).toBeVisible();
    await expect(page.getByTestId('reset')).toBeVisible();

    // Verify only one counter container exists (no duplication)
    const counterContainers = page.locator('#counter');
    await expect(counterContainers).toHaveCount(1);
  });

  test('should establish WebSocket connection and receive initial hierarchical structure', async ({
    page,
  }) => {
    const initialMessages = collectWebSocketMessages(page, 'initial_render');

    await page.goto('/counter');

    // Wait for initial render message to arrive
    await waitForCondition(() => {
      return initialMessages.length > 0;
    });

    const initialMessage = initialMessages[0];
    expect(initialMessage).toBeTruthy();
    expect(initialMessage.structure).toBeDefined();
    expect(typeof initialMessage.structure).toBe('object');

    // Verify the structure contains stateful components with static/dynamic data
    const structureKeys = Object.keys(initialMessage.structure);
    expect(structureKeys.length).toBeGreaterThan(0);

    // Check that each component has static and dynamic properties
    const firstComponent = initialMessage.structure[structureKeys[0]];
    expect(firstComponent.static).toBeDefined();
    expect(firstComponent.dynamic).toBeDefined();
  });

  test('should update counter via WebSocket and verify hierarchical diff updates', async ({
    page,
  }) => {
    const diffMessages = collectWebSocketMessages(page, 'diff');

    await page.goto('/counter');

    // Wait for initial load
    await expect(page.getByTestId('count')).toHaveText('0');

    // Click increment button
    await page.getByTestId('increment').click();

    // Verify UI updated
    await expect(page.getByTestId('count')).toHaveText('1');

    // Wait for WebSocket diff message using helper
    await waitForCondition(() => {
      return diffMessages.length > 0;
    });

    const diffMessage = diffMessages[0];
    expect(diffMessage.type).toBe('diff');
    expect(diffMessage.changes).toBeDefined();
    expect(Array.isArray(diffMessage.changes)).toBe(true);

    // Verify the diff contains element changes
    // Should be in format: [[element_index, new_value], [element_index, new_value]]
    expect(diffMessage.changes.length).toBeGreaterThan(0);

    const [elementIndex, newValue] = diffMessage.changes[0];
    expect(typeof elementIndex).toBe('number');
    expect(typeof newValue).toBe('string');
  });

  test('should handle multiple rapid updates with hierarchical diffs', async ({ page }) => {
    const allDiffMessages = [];

    page.on('websocket', (ws) => {
      ws.on('framereceived', (event) => {
        try {
          const data = JSON.parse(event.payload);
          if (data.type === 'diff') {
            allDiffMessages.push(data);
          }
        } catch (e) {
          // Ignore non-JSON
        }
      });
    });

    await page.goto('/counter');
    await expect(page.getByTestId('count')).toHaveText('0');

    // Perform multiple rapid increments with smaller delays
    for (let i = 0; i < 5; i++) {
      await page.getByTestId('increment').click();
      await page.waitForTimeout(50); // Reduced timeout for faster execution
    }

    // Verify final state
    await expect(page.getByTestId('count')).toHaveText('5');

    // Wait a bit for all WebSocket messages to arrive
    await page.waitForTimeout(500);

    // Verify we received diff messages
    expect(allDiffMessages.length).toBeGreaterThan(0);

    // Each diff should contain properly formatted element changes
    allDiffMessages.forEach((msg) => {
      expect(msg.type).toBe('diff');
      expect(Array.isArray(msg.changes)).toBe(true);

      // Each change should be [element_index, new_value]
      msg.changes.forEach((elementChange) => {
        expect(Array.isArray(elementChange)).toBe(true);
        expect(elementChange.length).toBe(2);
        expect(typeof elementChange[0]).toBe('number'); // element index
        // elementChange[1] is the new value (can be string, number, etc.)
      });
    });
  });

  test('should reset counter and verify hierarchical diff structure', async ({ page }) => {
    const resetDiffMessages = collectWebSocketMessages(page, 'diff');

    await page.goto('/counter');

    // Increment to non-zero value first
    await page.getByTestId('increment').click();
    await page.getByTestId('increment').click();
    await expect(page.getByTestId('count')).toHaveText('2');

    // Clear previous diff messages by counting current ones
    const initialDiffCount = resetDiffMessages.length;

    // Reset counter
    await page.getByTestId('reset').click();

    // Verify UI reset
    await expect(page.getByTestId('count')).toHaveText('0');

    // Wait for new diff message after reset
    await waitForCondition(() => {
      return resetDiffMessages.length > initialDiffCount;
    });

    const resetDiffMessage = resetDiffMessages[resetDiffMessages.length - 1]; // Get latest message

    expect(resetDiffMessage).toBeTruthy();
    expect(resetDiffMessage.type).toBe('diff');
    expect(Array.isArray(resetDiffMessage.changes)).toBe(true);

    // Should contain element change with "0" value
    const hasZeroValue = resetDiffMessage.changes.some(([elementIndex, value]) => {
      return typeof elementIndex === 'number' && value.includes('0');
    });
    expect(hasZeroValue).toBe(true);
  });

  test('should handle decrement operations correctly', async ({ page }) => {
    await page.goto('/counter');

    // First increment to 1
    await page.getByTestId('increment').click();
    await expect(page.getByTestId('count')).toHaveText('1');

    // Then decrement back to 0
    await page.getByTestId('decrement').click();
    await expect(page.getByTestId('count')).toHaveText('0');

    // Verify we can go negative
    await page.getByTestId('decrement').click();
    await expect(page.getByTestId('count')).toHaveText('-1');
  });
});
