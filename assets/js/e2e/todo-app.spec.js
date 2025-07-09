import { test, expect } from '@playwright/test';

test.describe('Arizona Todo App - Advanced E2E Tests', () => {
  // Helper function to wait for condition with retry attempts
  const waitForCondition = async (checkFn, maxAttempts = 30, delayMs = 100) => {
    for (let attempt = 0; attempt < maxAttempts; attempt++) {
      if (await checkFn()) {
        return true;
      }
      await new Promise((resolve) => {
        return setTimeout(resolve, delayMs);
      });
    }
    throw new Error(`Condition not met after ${maxAttempts} attempts`);
  };

  // Helper function to collect WebSocket messages of specific type
  const collectWebSocketMessages = (page, messageType) => {
    const messages = [];

    page.on('websocket', (ws) => {
      ws.on('framereceived', (event) => {
        try {
          const data = JSON.parse(event.payload);
          if (data.type === messageType) {
            messages.push(data);
          }
        } catch (e) {
          // Non-JSON messages are OK
        }
      });
    });

    return messages;
  };

  test('should load todo app and display initial todos', async ({ page }) => {
    await page.goto('/test/todo');

    // Check that the page loads with initial todos
    await expect(page.getByTestId('todo-1')).toBeVisible();
    await expect(page.getByTestId('todo-text-1')).toHaveText('Learn Erlang');
    await expect(page.getByTestId('todo-2')).toBeVisible();
    await expect(page.getByTestId('todo-text-2')).toHaveText('Build web app');
    await expect(page.getByTestId('todo-3')).toBeVisible();
    await expect(page.getByTestId('todo-text-3')).toHaveText('Write tests');

    // Check initial stats
    await expect(page.getByTestId('todo-count')).toContainText('2 items left');

    // Check initial filter state
    await expect(page.getByTestId('filter-all')).toHaveClass(/selected/);
  });

  test('should verify delete buttons render correctly with times symbol', async ({ page }) => {
    await page.goto('/test/todo');

    // Wait for initial load
    await expect(page.getByTestId('todo-1')).toBeVisible();
    await expect(page.getByTestId('todo-2')).toBeVisible();
    await expect(page.getByTestId('todo-3')).toBeVisible();

    // Verify delete buttons are properly rendered with × character
    const deleteButtons = page.locator('[data-testid^="delete-"]');
    await expect(deleteButtons).toHaveCount(3);

    // Check that delete buttons contain the times symbol (×)
    await expect(page.getByTestId('delete-1')).toContainText('×');
    await expect(page.getByTestId('delete-2')).toContainText('×');
    await expect(page.getByTestId('delete-3')).toContainText('×');

    // Verify the buttons are clickable
    await expect(page.getByTestId('delete-1')).toBeEnabled();
    await expect(page.getByTestId('delete-2')).toBeEnabled();
    await expect(page.getByTestId('delete-3')).toBeEnabled();
  });

  test('should display todo items with correct initial state', async ({ page }) => {
    await page.goto('/test/todo');

    // Verify initial state: todo-1 should not be completed
    const todo1 = page.getByTestId('todo-1');
    await expect(todo1).not.toHaveClass(/completed/);
    await expect(page.getByTestId('todo-text-1')).toHaveText('Learn Erlang');

    // Verify todo-2 is completed
    const todo2 = page.getByTestId('todo-2');
    await expect(todo2).toHaveClass(/completed/);
    await expect(page.getByTestId('todo-text-2')).toHaveText('Build web app');

    // Verify todo-3 is not completed
    const todo3 = page.getByTestId('todo-3');
    await expect(todo3).not.toHaveClass(/completed/);
    await expect(page.getByTestId('todo-text-3')).toHaveText('Write tests');

    // Verify toggle checkboxes are in correct state
    const toggle1 = page.getByTestId('toggle-1');
    const toggle2 = page.getByTestId('toggle-2');
    const toggle3 = page.getByTestId('toggle-3');

    await expect(toggle1).not.toBeChecked();
    await expect(toggle2).toBeChecked();
    await expect(toggle3).not.toBeChecked();
  });

  test('should display filter buttons with correct initial state', async ({ page }) => {
    await page.goto('/test/todo');

    // Verify filter buttons exist and are clickable
    await expect(page.getByTestId('filter-all')).toBeVisible();
    await expect(page.getByTestId('filter-active')).toBeVisible();
    await expect(page.getByTestId('filter-completed')).toBeVisible();

    // Verify initial filter state (All should be selected)
    await expect(page.getByTestId('filter-all')).toHaveClass(/selected/);
    await expect(page.getByTestId('filter-active')).not.toHaveClass(/selected/);
    await expect(page.getByTestId('filter-completed')).not.toHaveClass(/selected/);

    // Verify filter button text
    await expect(page.getByTestId('filter-all')).toHaveText('All');
    await expect(page.getByTestId('filter-active')).toHaveText('Active');
    await expect(page.getByTestId('filter-completed')).toHaveText('Completed');
  });

  test('should display clear completed button when there are completed todos', async ({ page }) => {
    await page.goto('/test/todo');

    // Verify clear completed button exists (since todo-2 is completed)
    await expect(page.getByTestId('clear-completed')).toBeVisible();
    await expect(page.getByTestId('clear-completed')).toHaveText('Clear completed');
    await expect(page.getByTestId('clear-completed')).toBeEnabled();
  });

  test('should display input field for adding new todos', async ({ page }) => {
    await page.goto('/test/todo');

    // Verify input field exists and is functional
    const newTodoInput = page.getByTestId('new-todo-input');
    await expect(newTodoInput).toBeVisible();
    await expect(newTodoInput).toBeEnabled();
    await expect(newTodoInput).toHaveAttribute('placeholder', 'What needs to be done?');

    // Verify input initially has empty value
    await expect(newTodoInput).toHaveValue('');

    // Verify input can accept text
    await newTodoInput.fill('Test todo');
    await expect(newTodoInput).toHaveValue('Test todo');
  });
});
