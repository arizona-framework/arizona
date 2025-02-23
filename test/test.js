/* global require, describe, test, expect */

const { patch } = require('../priv/static/assets/js/arizona-patch.js');

describe('Patch Test', () => {
  test('should patch diff', () => {
    const rendered = [
      'template',
      ['<div id="', '">\n    <span>', '</span> ', '</div>'],
      [
        'counter',
        '0',
        [
          'template',
          ['<button\n    type="', '"\n    onclick=', '> ', '</button>'],
          ['button', '\'arizona.send("counter", "incr", 1)\'', 'Increment'],
        ],
      ],
    ];

    let diff = { 1: '1' };
    expect(patch(rendered, diff)).toBe(`<div id="counter">
    <span>1</span> <button
    type="button"
    onclick='arizona.send("counter", "incr", 1)'> Increment</button></div>`);

    diff = { 1: '2' };
    expect(patch(rendered, diff)).toBe(`<div id="counter">
    <span>2</span> <button
    type="button"
    onclick='arizona.send("counter", "incr", 1)'> Increment</button></div>`);
  });
});
