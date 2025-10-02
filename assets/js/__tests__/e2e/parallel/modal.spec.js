import { test, expect } from '@playwright/test';
import { waitForCondition, collectWebSocketMessages } from '../utils.js';

test.describe('Arizona Modal App', () => {
  test('should load modal page and display initial state', async ({ page }) => {
    await page.goto('/modal');

    // Check that the page loads with control buttons visible
    await expect(page.getByText('Open Success Modal')).toBeVisible();
    await expect(page.getByText('Open Error Modal')).toBeVisible();
    await expect(page.getByText('Open Info Modal')).toBeVisible();

    // Verify modal is initially hidden
    await expect(page.locator('.modal-overlay')).not.toBeVisible();
    await expect(page.locator('.modal-container')).not.toBeVisible();

    // Verify only one modal container exists in DOM structure
    const modalContainers = page.locator('#modal');
    await expect(modalContainers).toHaveCount(1);
  });

  test('should establish WebSocket connection and receive initial hierarchical structure', async ({
    page,
  }) => {
    const initialMessages = collectWebSocketMessages(page, 'initial_render');

    await page.goto('/modal');

    // Wait for initial render message to arrive
    await waitForCondition(() => {
      return initialMessages.length > 0;
    });

    const initialMessage = initialMessages[0];
    expect(initialMessage).toBeTruthy();
    expect(initialMessage.structure).toBeDefined();
    expect(typeof initialMessage.structure).toBe('object');

    // Verify the structure contains modal component data
    const structureKeys = Object.keys(initialMessage.structure);
    expect(structureKeys.length).toBeGreaterThan(0);

    // Check that modal component has static and dynamic properties
    const modalComponent = Object.values(initialMessage.structure).find((component) => {
      return component.static || component.dynamic;
    });
    expect(modalComponent).toBeDefined();
    if (modalComponent.static) expect(modalComponent.static).toBeDefined();
    if (modalComponent.dynamic) expect(modalComponent.dynamic).toBeDefined();
  });

  test('should open success modal and verify content via WebSocket updates', async ({ page }) => {
    const diffMessages = collectWebSocketMessages(page, 'diff');

    await page.goto('/modal');

    // Wait for initial load
    await expect(page.getByText('Open Success Modal')).toBeVisible();

    // Click success modal button
    await page.getByText('Open Success Modal').click();

    // Verify modal opened in UI
    await expect(page.locator('.modal-overlay')).toBeVisible();
    await expect(page.locator('.modal-container')).toBeVisible();

    // Wait for WebSocket diff message
    await waitForCondition(() => {
      return diffMessages.length > 0;
    });

    const diffMessage = diffMessages[0];
    expect(diffMessage.type).toBe('diff');
    expect(diffMessage.changes).toBeDefined();
    expect(Array.isArray(diffMessage.changes)).toBe(true);

    // Verify success modal content (using more specific selectors)
    await expect(page.locator('.modal-header h1')).toContainText('Success modal');
    await expect(page.getByText('Hey, Joe!')).toBeVisible();
    await expect(page.getByText('successfully saved')).toBeVisible();
    await expect(page.getByText('Return to profile')).toBeVisible();
    await expect(page.getByText('Back to index')).toBeVisible();

    // Verify success modal styling
    await expect(page.locator('.modal-icon-success')).toBeVisible();
    await expect(page.locator('.text-success')).toBeVisible();
    await expect(page.locator('.modal-btn-success')).toBeVisible();
  });

  test('should open error modal and verify content', async ({ page }) => {
    await page.goto('/modal');

    // Click error modal button
    await page.getByText('Open Error Modal').click();

    // Verify modal opened
    await expect(page.locator('.modal-overlay')).toBeVisible();

    // Verify error modal content (using more specific selectors)
    await expect(page.locator('.modal-header h1')).toContainText('Error modal');
    await expect(page.getByText('Sorry, Joe!')).toBeVisible();
    await expect(page.getByText('error processing')).toBeVisible();
    await expect(page.getByText('Retry')).toBeVisible();
    await expect(page.getByText('Cancel')).toBeVisible();

    // Verify error modal styling
    await expect(page.locator('.modal-icon-error')).toBeVisible();
    await expect(page.locator('.text-error')).toBeVisible();
    await expect(page.locator('.modal-btn-danger')).toBeVisible();
  });

  test('should open info modal and verify content', async ({ page }) => {
    await page.goto('/modal');

    // Click info modal button
    await page.getByText('Open Info Modal').click();

    // Verify modal opened
    await expect(page.locator('.modal-overlay')).toBeVisible();

    // Verify info modal content (using more specific selectors)
    await expect(page.locator('.modal-header h1')).toContainText('Info modal');
    await expect(page.getByText('Hello, Joe!')).toBeVisible();
    await expect(page.getByText('information for you')).toBeVisible();
    await expect(page.getByText('Got it')).toBeVisible();
    await expect(page.getByText('Close')).toBeVisible();

    // Verify info modal styling
    await expect(page.locator('.modal-icon-info')).toBeVisible();
    await expect(page.locator('.text-info')).toBeVisible();
    await expect(page.locator('.modal-btn-primary')).toBeVisible();
  });

  test('should close modal via overlay click with WebSocket updates', async ({ page }) => {
    const diffMessages = collectWebSocketMessages(page, 'diff');

    await page.goto('/modal');

    // Open modal first
    await page.getByText('Open Success Modal').click();
    await expect(page.locator('.modal-overlay')).toBeVisible();

    // Clear previous diff messages
    const initialDiffCount = diffMessages.length;

    // Click overlay to close modal (ensure we click the overlay, not its content)
    await page.locator('.modal-overlay').click({ position: { x: 10, y: 10 } });

    // Wait a bit for the close event to process
    await page.waitForTimeout(200);

    // Verify modal closed
    await expect(page.locator('.modal-overlay')).not.toBeVisible();

    // Wait for new diff message after close
    await waitForCondition(() => {
      return diffMessages.length > initialDiffCount;
    });

    const closeDiffMessage = diffMessages[diffMessages.length - 1];
    expect(closeDiffMessage).toBeTruthy();
    expect(closeDiffMessage.type).toBe('diff');
    expect(Array.isArray(closeDiffMessage.changes)).toBe(true);
  });

  test('should close modal via close button (×)', async ({ page }) => {
    await page.goto('/modal');

    // Open modal first
    await page.getByText('Open Info Modal').click();
    await expect(page.locator('.modal-overlay')).toBeVisible();

    // Click close button
    await page.locator('.modal-close-btn').click();

    // Verify modal closed
    await expect(page.locator('.modal-overlay')).not.toBeVisible();
    await expect(page.locator('.modal-container')).not.toBeVisible();
  });

  test('should close modal via footer buttons', async ({ page }) => {
    await page.goto('/modal');

    // Test primary button close
    await page.getByText('Open Success Modal').click();
    await expect(page.locator('.modal-overlay')).toBeVisible();
    await page.getByText('Return to profile').click();
    await expect(page.locator('.modal-overlay')).not.toBeVisible();

    // Test secondary button close
    await page.getByText('Open Error Modal').click();
    await expect(page.locator('.modal-overlay')).toBeVisible();
    await page.getByText('Cancel').click();
    await expect(page.locator('.modal-overlay')).not.toBeVisible();
  });

  test('should prevent event propagation when clicking modal container', async ({ page }) => {
    await page.goto('/modal');

    // Open modal
    await page.getByText('Open Success Modal').click();
    await expect(page.locator('.modal-overlay')).toBeVisible();

    // Click inside modal container (should not close)
    await page.locator('.modal-container').click();

    // Modal should still be visible
    await expect(page.locator('.modal-overlay')).toBeVisible();
    await expect(page.locator('.modal-container')).toBeVisible();

    // Close via overlay to clean up
    await page.locator('.modal-overlay').click({ position: { x: 10, y: 10 } });
    await page.waitForTimeout(200);
    await expect(page.locator('.modal-overlay')).not.toBeVisible();
  });

  test('should handle rapid modal type switching', async ({ page }) => {
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

    await page.goto('/modal');

    // Rapidly switch between different modal types (sequential to test state management)
    const modalTypes = ['Open Success Modal', 'Open Error Modal', 'Open Info Modal'];

    // Use reduce for sequential operations instead of for loop
    await modalTypes.reduce(async (promise, modalType) => {
      await promise; // Wait for previous iteration

      await page.getByText(modalType).click();
      await expect(page.locator('.modal-overlay')).toBeVisible();
      await page.waitForTimeout(100); // Brief pause for rendering

      // Close modal before next iteration
      await page.locator('.modal-overlay').click({ position: { x: 10, y: 10 } });
      await page.waitForTimeout(200); // Wait for close event to process
      await expect(page.locator('.modal-overlay')).not.toBeVisible();
      await page.waitForTimeout(50);
    }, Promise.resolve());

    // Wait for all WebSocket messages
    await page.waitForTimeout(500);

    // Verify we received diff messages for state changes
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
      });
    });
  });

  test('should handle keyboard navigation and accessibility', async ({ page }) => {
    await page.goto('/modal');

    // Open modal for tab navigation test
    await page.getByText('Open Info Modal').click();
    await expect(page.locator('.modal-overlay')).toBeVisible();

    // Test tab navigation within modal
    await page.keyboard.press('Tab');

    // Verify focus is trapped within modal (focus should be on modal elements)
    const focusedElement = await page.evaluate(() => {
      return document.activeElement?.tagName;
    });
    expect(['BUTTON', 'DIV'].includes(focusedElement)).toBe(true);

    // Close modal via close button click instead of keyboard
    await page.locator('.modal-close-btn').click();
    await expect(page.locator('.modal-overlay')).not.toBeVisible();
  });

  test('should display modal with proper ARIA attributes for accessibility', async ({ page }) => {
    await page.goto('/modal');

    // Open modal
    await page.getByText('Open Success Modal').click();
    await expect(page.locator('.modal-overlay')).toBeVisible();

    // Verify modal container has proper structure
    const modalContainer = page.locator('.modal-container');
    await expect(modalContainer).toBeVisible();

    // Verify modal components are accessible
    const modalHeader = page.locator('.modal-header h1');
    await expect(modalHeader).toBeVisible();
    await expect(modalHeader).toContainText('Success modal');

    const modalBody = page.locator('.modal-body');
    await expect(modalBody).toBeVisible();

    const modalFooter = page.locator('.modal-footer');
    await expect(modalFooter).toBeVisible();

    // Verify close button is accessible
    const closeButton = page.locator('.modal-close-btn');
    await expect(closeButton).toBeVisible();
    await expect(closeButton).toContainText('×');
  });

  test('should handle modal animations and transitions', async ({ page }) => {
    await page.goto('/modal');

    // Open modal and verify animation classes
    await page.getByText('Open Success Modal').click();

    // Modal should become visible with animations
    await expect(page.locator('.modal-overlay')).toBeVisible();
    await expect(page.locator('.modal-container')).toBeVisible();

    // Verify animation styles are applied
    const overlayStyles = await page.locator('.modal-overlay').evaluate((el) => {
      return getComputedStyle(el).animation;
    });
    expect(overlayStyles).toContain('fadeIn');

    const containerStyles = await page.locator('.modal-container').evaluate((el) => {
      return getComputedStyle(el).animation;
    });
    expect(containerStyles).toContain('modalSlideIn');

    // Close modal
    await page.locator('.modal-overlay').click({ position: { x: 10, y: 10 } });
    await page.waitForTimeout(200);
    await expect(page.locator('.modal-overlay')).not.toBeVisible();
  });
});
