import { test, expect } from '@playwright/test';

test.describe('Arizona Hierarchical Rendering', () => {
  test('should load counter page and display initial state', async ({ page }) => {
    await page.goto('/test/counter');

    // Check that the page loads with initial counter value
    await expect(page.getByTestId('count')).toHaveText('0');
    await expect(page.getByTestId('increment')).toBeVisible();
    await expect(page.getByTestId('decrement')).toBeVisible();
    await expect(page.getByTestId('reset')).toBeVisible();
  });

  test('should establish WebSocket connection and receive initial hierarchical structure', async ({
    page,
  }) => {
    // Listen for WebSocket messages
    const messages = [];
    page.on('websocket', (ws) => {
      ws.on('framereceived', (event) => {
        try {
          const data = JSON.parse(event.payload);
          messages.push(data);
        } catch (e) {
          // Non-JSON messages are OK
        }
      });
    });

    await page.goto('/test/counter');

    // Wait for WebSocket connection and initial message
    await page.waitForTimeout(1000);

    // Check that we received an initial_render message with structure
    const initialMessage = messages.find((msg) => msg.type === 'initial_render');
    expect(initialMessage).toBeTruthy();
    expect(initialMessage.structure).toBeDefined();
  });

  test('should update counter via WebSocket and verify hierarchical diff updates', async ({
    page,
  }) => {
    let diffMessages = [];

    // Capture WebSocket diff messages
    page.on('websocket', (ws) => {
      ws.on('framereceived', (event) => {
        try {
          const data = JSON.parse(event.payload);
          if (data.type === 'diff') {
            diffMessages.push(data);
          }
        } catch (e) {
          // Ignore non-JSON
        }
      });
    });

    await page.goto('/test/counter');

    // Wait for initial load
    await expect(page.getByTestId('count')).toHaveText('0');

    // Click increment button
    await page.getByTestId('increment').click();

    // Verify UI updated
    await expect(page.getByTestId('count')).toHaveText('1');

    // Wait for WebSocket diff message
    await page.waitForTimeout(500);

    // Verify we received a diff update
    expect(diffMessages.length).toBeGreaterThan(0);

    const diffMessage = diffMessages[0];
    expect(diffMessage.type).toBe('diff');
    expect(diffMessage.changes).toBeDefined();

    // Verify the diff contains hierarchical structure changes
    // Should be in format: [["component_id", [[element_index, new_value]]]]
    expect(Array.isArray(diffMessage.changes)).toBe(true);
  });

  test('should handle multiple rapid updates with hierarchical diffs', async ({ page }) => {
    let allDiffMessages = [];

    page.on('websocket', (ws) => {
      ws.on('framereceived', (event) => {
        try {
          const data = JSON.parse(event.payload);
          if (data.type === 'diff') {
            allDiffMessages.push(data);
          }
        } catch (e) {
          // Ignore
        }
      });
    });

    await page.goto('/test/counter');
    await expect(page.getByTestId('count')).toHaveText('0');

    // Perform multiple rapid increments
    for (let i = 0; i < 5; i++) {
      await page.getByTestId('increment').click();
      await page.waitForTimeout(100);
    }

    // Verify final state
    await expect(page.getByTestId('count')).toHaveText('5');

    // Wait for all WebSocket messages
    await page.waitForTimeout(1000);

    // Verify we received multiple diff messages
    expect(allDiffMessages.length).toBeGreaterThan(0);

    // Each diff should contain properly formatted hierarchical changes
    allDiffMessages.forEach((msg) => {
      expect(msg.type).toBe('diff');
      expect(Array.isArray(msg.changes)).toBe(true);

      // Each component change should be [component_id, element_changes]
      msg.changes.forEach((componentChange) => {
        expect(Array.isArray(componentChange)).toBe(true);
        expect(componentChange.length).toBe(2);

        const [componentId, elementChanges] = componentChange;
        expect(typeof componentId).toBe('string');
        expect(Array.isArray(elementChanges)).toBe(true);

        // Each element change should be [element_index, new_value]
        elementChanges.forEach((elementChange) => {
          expect(Array.isArray(elementChange)).toBe(true);
          expect(elementChange.length).toBe(2);
          expect(typeof elementChange[0]).toBe('number'); // element index
          // elementChange[1] is the new value (can be string, number, etc.)
        });
      });
    });
  });

  test('should reset counter and verify hierarchical diff structure', async ({ page }) => {
    let resetDiffMessage = null;

    page.on('websocket', (ws) => {
      ws.on('framereceived', (event) => {
        try {
          const data = JSON.parse(event.payload);
          if (data.type === 'diff') {
            resetDiffMessage = data;
          }
        } catch (e) {
          // Ignore
        }
      });
    });

    await page.goto('/test/counter');

    // Increment to non-zero value first
    await page.getByTestId('increment').click();
    await page.getByTestId('increment').click();
    await expect(page.getByTestId('count')).toHaveText('2');

    // Clear previous messages
    resetDiffMessage = null;

    // Reset counter
    await page.getByTestId('reset').click();

    // Verify UI reset
    await expect(page.getByTestId('count')).toHaveText('0');

    // Wait for WebSocket message
    await page.waitForTimeout(500);

    // Verify reset generated a proper hierarchical diff
    expect(resetDiffMessage).toBeTruthy();
    expect(resetDiffMessage.type).toBe('diff');
    expect(Array.isArray(resetDiffMessage.changes)).toBe(true);

    // The diff should contain the change back to "0"
    const componentChanges = resetDiffMessage.changes[0];
    expect(Array.isArray(componentChanges)).toBe(true);

    const [componentId, elementChanges] = componentChanges;
    expect(typeof componentId).toBe('string');

    // Should contain element change with "0" value
    const hasZeroValue = elementChanges.some(([index, value]) => value === '0');
    expect(hasZeroValue).toBe(true);
  });
});
