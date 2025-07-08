/**
 * Arizona Hierarchical Client Test Suite
 *
 * Tests the client-side logic for:
 * 1. Receiving hierarchical structures from WebSocket
 * 2. Applying diffs from arizona_differ to update hierarchical structures
 * 3. Regenerating HTML from updated hierarchical structures
 */

import { describe, it, expect, beforeEach, vi } from 'vitest';
import ArizonaHierarchical from './arizona-hierarchical.js';

describe('ArizonaHierarchical', () => {
  let client;

  beforeEach(() => {
    client = new ArizonaHierarchical();
  });

  describe('initialization', () => {
    it('should initialize with empty structure', () => {
      expect(client.getStructure()).toEqual({});
    });

    it('should initialize with provided structure', () => {
      const structure = {
        root: {
          0: '<div>',
          1: 'Hello World',
          2: '</div>',
        },
      };

      client.initialize(structure);
      expect(client.getStructure()).toEqual(structure);
    });

    it('should deep clone the initial structure', () => {
      const structure = {
        root: { 0: '<div>', 1: 'Hello' },
      };

      client.initialize(structure);
      structure.root[1] = 'Modified';

      expect(client.getStructure().root[1]).toBe('Hello');
    });
  });

  describe('diff application', () => {
    beforeEach(() => {
      client.initialize({
        root: {
          0: '<div>',
          1: 'Hello World',
          2: '</div>',
        },
      });
    });

    it('should apply simple element change', () => {
      const changes = [['root', [[1, 'Hello Arizona']]]];

      client.applyDiff(changes);

      expect(client.getStructure().root[1]).toBe('Hello Arizona');
    });

    it('should apply multiple element changes', () => {
      const changes = [
        [
          'root',
          [
            [0, '<section>'],
            [1, 'Hello Vitest'],
            [2, '</section>'],
          ],
        ],
      ];

      client.applyDiff(changes);

      const structure = client.getStructure();
      expect(structure.root[0]).toBe('<section>');
      expect(structure.root[1]).toBe('Hello Vitest');
      expect(structure.root[2]).toBe('</section>');
    });

    it('should handle changes to multiple components', () => {
      client.initialize({
        root: {
          0: '<div>',
          1: { type: 'stateful', id: 'counter-1' },
          2: '</div>',
        },
        'counter-1': {
          0: '<span>Count: ',
          1: '0',
          2: '</span>',
        },
      });

      const changes = [
        ['root', [[0, '<main>']]],
        ['counter-1', [[1, '5']]],
      ];

      client.applyDiff(changes);

      const structure = client.getStructure();
      expect(structure.root[0]).toBe('<main>');
      expect(structure['counter-1'][1]).toBe('5');
    });

    it('should warn for unknown stateful components', () => {
      const consoleSpy = vi.spyOn(console, 'warn').mockImplementation(() => {});

      const changes = [['unknown-component', [[0, 'test']]]];

      client.applyDiff(changes);

      expect(consoleSpy).toHaveBeenCalledWith(
        '[Arizona] StatefulId unknown-component not found in structure'
      );
      consoleSpy.mockRestore();
    });
  });

  describe('HTML generation', () => {
    it('should generate HTML from simple structure', () => {
      client.initialize({
        root: {
          0: '<div>',
          1: 'Hello World',
          2: '</div>',
        },
      });

      const html = client.generateHTML();
      expect(html).toBe('<div>Hello World</div>');
    });

    it('should generate HTML with nested stateful components', () => {
      client.initialize({
        root: {
          0: '<div>',
          1: { type: 'stateful', id: 'counter-1' },
          2: '</div>',
        },
        'counter-1': {
          0: '<span>Count: ',
          1: '5',
          2: '</span>',
        },
      });

      const html = client.generateHTML();
      expect(html).toBe('<div><span>Count: 5</span></div>');
    });

    it('should generate HTML with stateless components', () => {
      client.initialize({
        root: {
          0: '<div>',
          1: {
            type: 'stateless',
            structure: {
              0: '<h1>',
              1: 'Title',
              2: '</h1>',
            },
          },
          2: '</div>',
        },
      });

      const html = client.generateHTML();
      expect(html).toBe('<div><h1>Title</h1></div>');
    });

    it('should generate HTML with list components', () => {
      client.initialize({
        root: {
          0: '<ul>',
          1: {
            type: 'list',
            static: ['<li>', '</li>'],
            dynamic: [{ 0: 'Item 1' }, { 0: 'Item 2' }],
          },
          2: '</ul>',
        },
      });

      const html = client.generateHTML();
      expect(html).toBe('<ul><li>Item 1</li><li>Item 2</li></ul>');
    });

    it('should handle missing components gracefully', () => {
      client.initialize({});

      const html = client.generateHTML('nonexistent');
      expect(html).toBe('');
    });

    it('should handle elements in correct order', () => {
      client.initialize({
        root: {
          2: '</div>',
          0: '<div>',
          1: 'Middle',
        },
      });

      const html = client.generateHTML();
      expect(html).toBe('<div>Middle</div>');
    });
  });

  describe('iodata flattening (comma issue reproduction)', () => {
    it('should handle complex nested arrays from server without adding commas', () => {
      // This reproduces the comma issue seen in todo app
      // Server sends complex nested HTML structures like this from render_list
      const complexNestedData = [
        [
          [
            [],
            [
              '<div class="todo-item ">',
              '',
              '" data-testid="todo-">',
              '1',
              '">',
              'Learn Erlang',
              '</div>',
            ],
          ],
          [
            '<div class="todo-item ">',
            'completed',
            '" data-testid="todo-">',
            '2',
            '">',
            'Build web app',
            '</div>',
          ],
        ],
      ];

      client.initialize({
        root: {
          0: '<div class="todo-list">',
          1: 'placeholder',
          2: '</div>',
        },
      });

      // Apply diff with complex nested structure (like from todo app)
      const changes = [['root', [[1, complexNestedData]]]];
      client.applyDiff(changes);

      const html = client.generateHTML();

      // Should NOT contain commas like: ",Learn Erlang," or ",Build web app,"
      expect(html).not.toContain(',Learn Erlang,');
      expect(html).not.toContain(',Build web app,');

      // Should properly flatten to valid HTML
      expect(html).toContain('Learn Erlang');
      expect(html).toContain('Build web app');
      expect(html).toContain('<div class="todo-list">');

      console.log('Generated HTML with nested arrays:', html);
    });

    it('should flatten deeply nested arrays recursively', () => {
      const deeplyNested = [
        [[['<span>', 'Deep', '</span>']], ['<p>', ['Nested', ' Arrays'], '</p>']],
      ];

      client.initialize({
        root: {
          0: '<div>',
          1: deeplyNested,
          2: '</div>',
        },
      });

      const html = client.generateHTML();

      // Should flatten properly without commas between array elements
      expect(html).toBe('<div><span>Deep</span><p>Nested Arrays</p></div>');
      expect(html).not.toContain(',');
    });

    it('should handle mixed content types in nested arrays', () => {
      const mixedContent = [
        'Start',
        ['<b>', 'Bold', '</b>'],
        123,
        [' and ', ['<i>', 'Italic', '</i>']],
        ' End',
      ];

      client.initialize({
        root: {
          0: '<div>',
          1: mixedContent,
          2: '</div>',
        },
      });

      const html = client.generateHTML();

      expect(html).toBe('<div>Start<b>Bold</b>123 and <i>Italic</i> End</div>');
      expect(html).not.toContain(',');
    });
  });

  describe('integration scenarios', () => {
    it('should handle complete workflow: initialize -> diff -> generate HTML', () => {
      // Initial structure from server
      client.initialize({
        root: {
          0: '<div class="counter">',
          1: { type: 'stateful', id: 'counter' },
          2: '</div>',
        },
        counter: {
          0: '<span>Count: ',
          1: '0',
          2: '</span><button>+</button>',
        },
      });

      // Initial HTML
      let html = client.generateHTML();
      expect(html).toBe('<div class="counter"><span>Count: 0</span><button>+</button></div>');

      // Apply diff from server
      const changes = [['counter', [[1, '3']]]];
      client.applyDiff(changes);

      // Updated HTML
      html = client.generateHTML();
      expect(html).toBe('<div class="counter"><span>Count: 3</span><button>+</button></div>');
    });

    it('should handle complex nested updates', () => {
      client.initialize({
        root: {
          0: '<div>',
          1: { type: 'stateful', id: 'user-card' },
          2: { type: 'stateful', id: 'todo-list' },
          3: '</div>',
        },
        'user-card': {
          0: '<div class="user">',
          1: 'John Doe',
          2: '</div>',
        },
        'todo-list': {
          0: '<ul>',
          1: {
            type: 'list',
            static: ['<li>', '</li>'],
            dynamic: [{ 0: 'Buy milk' }, { 0: 'Walk dog' }],
          },
          2: '</ul>',
        },
      });

      // Update user name and todo list
      const changes = [
        ['user-card', [[1, 'Jane Smith']]],
        [
          'todo-list',
          [
            [
              1,
              {
                type: 'list',
                static: ['<li>', '</li>'],
                dynamic: [{ 0: 'Buy milk' }, { 0: 'Walk dog' }, { 0: 'Code Arizona' }],
              },
            ],
          ],
        ],
      ];

      client.applyDiff(changes);

      const html = client.generateHTML();
      expect(html).toBe(
        '<div><div class="user">Jane Smith</div><ul><li>Buy milk</li><li>Walk dog</li><li>Code Arizona</li></ul></div>'
      );
    });
  });
});
