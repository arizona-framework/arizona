/**
 * Test utilities and helper functions for Arizona E2E tests
 *
 * This module provides reusable utility functions that can be shared
 * across different test files to reduce code duplication and improve
 * test maintainability.
 */

/**
 * Wait for a condition to be met with retry attempts
 *
 * @param {Function} checkFn - Function that returns true when condition is met
 * @param {number} maxAttempts - Maximum number of retry attempts (default: 30)
 * @param {number} delayMs - Delay between attempts in milliseconds (default: 100)
 * @returns {Promise<boolean>} - Resolves to true when condition is met
 * @throws {Error} - Throws error if condition not met after max attempts
 *
 * @example
 * await waitForCondition(async () => {
 *   const element = page.getByTestId('dynamic-content');
 *   return await element.isVisible();
 * });
 */
export const waitForCondition = async (checkFn, maxAttempts = 30, delayMs = 100) => {
  for (let attempt = 0; attempt < maxAttempts; attempt++) {
    if (await checkFn()) {
      return true;
    }
    await new Promise((resolve) => setTimeout(resolve, delayMs));
  }
  throw new Error(`Condition not met after ${maxAttempts} attempts`);
};

/**
 * Collect WebSocket messages of a specific type
 *
 * @param {Page} page - Playwright page object
 * @param {string} messageType - Type of message to collect (e.g., 'diff', 'initial_render')
 * @returns {Array} - Array to collect messages (mutated as messages arrive)
 *
 * @example
 * const diffMessages = collectWebSocketMessages(page, 'diff');
 * // ... perform actions that trigger WebSocket messages
 * await waitForCondition(() => diffMessages.length > 0);
 * const latestDiff = diffMessages[diffMessages.length - 1];
 */
export const collectWebSocketMessages = (page, messageType) => {
  const messages = [];

  page.on('websocket', (ws) => {
    ws.on('framereceived', (event) => {
      try {
        const data = JSON.parse(event.payload);
        if (data.type === messageType) {
          messages.push(data);
        }
      } catch (e) {
        // Non-JSON messages are OK, ignore them
      }
    });
  });

  return messages;
};

/**
 * Wait for an element to have a specific class
 *
 * @param {Locator} element - Playwright locator for the element
 * @param {string|RegExp} className - Class name to wait for (string or regex)
 * @param {boolean} shouldHave - Whether element should have the class (default: true)
 * @param {number} timeout - Timeout in milliseconds (default: 5000)
 * @returns {Promise<void>}
 *
 * @example
 * await waitForClass(page.getByTestId('filter-active'), 'selected');
 * await waitForClass(page.getByTestId('todo-1'), /completed/, false);
 */
export const waitForClass = async (element, className, shouldHave = true, timeout = 5000) => {
  await waitForCondition(
    async () => {
      const elementClass = await element.getAttribute('class');
      const hasClass =
        typeof className === 'string'
          ? elementClass?.includes(className)
          : className.test(elementClass || '');
      return shouldHave ? hasClass : !hasClass;
    },
    timeout / 100,
    100
  );
};

/**
 * Wait for element visibility change
 *
 * @param {Locator} element - Playwright locator for the element
 * @param {boolean} shouldBeVisible - Whether element should be visible (default: true)
 * @param {number} timeout - Timeout in milliseconds (default: 5000)
 * @returns {Promise<void>}
 *
 * @example
 * await waitForVisibility(page.getByTestId('todo-4'), true);
 * await waitForVisibility(page.getByTestId('deleted-todo'), false);
 */
export const waitForVisibility = async (element, shouldBeVisible = true, timeout = 5000) => {
  await waitForCondition(
    async () => {
      const isVisible = await element.isVisible().catch(() => false);
      return shouldBeVisible ? isVisible : !isVisible;
    },
    timeout / 100,
    100
  );
};

/**
 * Wait for text content to match expected value
 *
 * @param {Locator} element - Playwright locator for the element
 * @param {string|RegExp} expectedText - Expected text content (string or regex)
 * @param {number} timeout - Timeout in milliseconds (default: 5000)
 * @returns {Promise<void>}
 *
 * @example
 * await waitForText(page.getByTestId('todo-count'), '3 items left');
 * await waitForText(page.getByTestId('status'), /completed/i);
 */
export const waitForText = async (element, expectedText, timeout = 5000) => {
  await waitForCondition(
    async () => {
      const textContent = await element.textContent().catch(() => '');
      return typeof expectedText === 'string'
        ? textContent?.includes(expectedText)
        : expectedText.test(textContent || '');
    },
    timeout / 100,
    100
  );
};

/**
 * Wait for input value to change
 *
 * @param {Locator} input - Playwright locator for the input element
 * @param {string} expectedValue - Expected input value
 * @param {number} timeout - Timeout in milliseconds (default: 5000)
 * @returns {Promise<void>}
 *
 * @example
 * await waitForInputValue(page.getByTestId('new-todo-input'), '');
 * await waitForInputValue(page.getByTestId('edit-input'), 'Updated text');
 */
export const waitForInputValue = async (input, expectedValue, timeout = 5000) => {
  await waitForCondition(
    async () => {
      const value = await input.inputValue().catch(() => '');
      return value === expectedValue;
    },
    timeout / 100,
    100
  );
};

/**
 * Wait for element count to match expected number
 *
 * @param {Locator} locator - Playwright locator that may match multiple elements
 * @param {number} expectedCount - Expected number of elements
 * @param {number} timeout - Timeout in milliseconds (default: 5000)
 * @returns {Promise<void>}
 *
 * @example
 * await waitForElementCount(page.locator('[data-testid^="todo-"]'), 4);
 * await waitForElementCount(page.getByRole('button'), 3);
 */
export const waitForElementCount = async (locator, expectedCount, timeout = 5000) => {
  await waitForCondition(
    async () => {
      const count = await locator.count();
      return count === expectedCount;
    },
    timeout / 100,
    100
  );
};

/**
 * Validate WebSocket diff message structure
 *
 * @param {Object} diffMessage - WebSocket diff message to validate
 * @throws {Error} - Throws error if message structure is invalid
 *
 * @example
 * const diffMessage = diffMessages[0];
 * validateDiffMessage(diffMessage);
 * // Message is valid, safe to use
 */
export const validateDiffMessage = (diffMessage) => {
  if (!diffMessage) {
    throw new Error('Diff message is null or undefined');
  }

  if (diffMessage.type !== 'diff') {
    throw new Error(`Expected message type 'diff', got '${diffMessage.type}'`);
  }

  if (!Array.isArray(diffMessage.changes)) {
    throw new Error('Diff message changes must be an array');
  }

  // Validate each element change
  diffMessage.changes.forEach((elementChange, index) => {
    if (!Array.isArray(elementChange)) {
      throw new Error(`Element change ${index} must be an array`);
    }

    if (elementChange.length !== 2) {
      throw new Error(`Element change ${index} must have exactly 2 elements`);
    }

    const [elementIndex, newValue] = elementChange;

    if (typeof elementIndex !== 'number') {
      throw new Error(`Element index in change ${index} must be a number`);
    }

    // newValue can be string, number, etc. - no strict validation needed
  });
};

/**
 * Common test selectors and helpers
 */
export const selectors = {
  // Todo app selectors
  todoItem: (id) => `[data-testid="todo-${id}"]`,
  todoText: (id) => `[data-testid="todo-text-${id}"]`,
  todoToggle: (id) => `[data-testid="toggle-${id}"]`,
  todoDelete: (id) => `[data-testid="delete-${id}"]`,

  // Common selectors
  allTodos: '[data-testid^="todo-"]:not([data-testid*="text"]):not([data-testid*="count"])',
  deleteButtons: '[data-testid^="delete-"]',
  filterButtons: '[data-testid^="filter-"]',
};

/**
 * Common assertions for Arizona app testing
 * Note: Import { expect } from '@playwright/test' in your test file to use these
 */
export const createAssertions = (expect) => ({
  /**
   * Assert todo item has correct state
   */
  async todoState(page, todoId, { text, completed, visible = true }) {
    const todo = page.getByTestId(`todo-${todoId}`);

    if (visible) {
      await expect(todo).toBeVisible();
    } else {
      await expect(todo).not.toBeVisible();
      return; // Skip other checks if not visible
    }

    if (text !== undefined) {
      await expect(page.getByTestId(`todo-text-${todoId}`)).toHaveText(text);
    }

    if (completed !== undefined) {
      if (completed) {
        await expect(todo).toHaveClass(/completed/);
        await expect(page.getByTestId(`toggle-${todoId}`)).toBeChecked();
      } else {
        await expect(todo).not.toHaveClass(/completed/);
        await expect(page.getByTestId(`toggle-${todoId}`)).not.toBeChecked();
      }
    }
  },

  /**
   * Assert filter button state
   */
  async filterState(page, filterId, { selected, text }) {
    const filter = page.getByTestId(`filter-${filterId}`);
    await expect(filter).toBeVisible();

    if (selected !== undefined) {
      if (selected) {
        await expect(filter).toHaveClass(/selected/);
      } else {
        await expect(filter).not.toHaveClass(/selected/);
      }
    }

    if (text !== undefined) {
      await expect(filter).toHaveText(text);
    }
  },
});
