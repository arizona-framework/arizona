/*global morphdom*/
'use strict';

globalThis['arizona'] = (() => {
	// --------------------------------------------------------------------
	// API function definitions
	// --------------------------------------------------------------------

	function on(eventName, listener) {
		listeners[eventName] = listeners[eventName] = [];
		listeners[eventName].push(listener);

		tableLog('on', eventName);
	}

	function once(eventName, listener) {
		const onceWrapper = (payload) => {
			listener(payload);
			off(eventName, onceWrapper);
		};
		on(eventName, onceWrapper);

		tableLog('once', eventName);
	}

	function off(eventName, listener) {
		listeners[eventName] = (listeners[eventName] || []).filter(
			(needle) => needle !== listener,
		);

		tableLog('off', eventName);
	}

	function emit(eventName, payload) {
		(listeners[eventName] || []).forEach((listener) => {
			listener(payload);
		});
	}

	function send(eventName, payloadOrListener, listenerOrOpts, optsOrNull) {
		typeof payloadOrListener === 'function'
			? sendMsgToWorker.bind(this)(
					eventName,
					undefined,
					payloadOrListener,
					listenerOrOpts,
				)
			: sendMsgToWorker.bind(this)(
					eventName,
					payloadOrListener,
					listenerOrOpts,
					optsOrNull,
				);
	}

	function connect(params, listener, opts) {
		params = {
			...params,
			path: location.pathname,
		};
		send('connect', params, listener, opts);
	}

	// --------------------------------------------------------------------
	// Private
	// --------------------------------------------------------------------

	const listeners = {};
	const worker = new Worker('assets/js/arizona-worker.js');

	function tableLog(action, eventName) {
		console.table({ action, eventName, listeners });
	}

  function sendMsgToWorker(eventName, payload, listener, opts = {}) {
    if (!opts.target && this instanceof HTMLElement) {
      opts.target = this.getAttribute("arizona-target");
    }
    const target = document.querySelector(opts.target);
    const arizonaId = target?.getAttribute("arizona-id") || "[0]";
    listener && once(eventName, listener);
    worker.postMessage({ target: arizonaId, event, payload });
  }

	function applyPatch(elem, html) {
		morphdom(elem, html, {
			// Can I make morphdom blaze through the DOM tree even faster? Yes.
			// @see https://github.com/patrick-steele-idem/morphdom#can-i-make-morphdom-blaze-through-the-dom-tree-even-faster-yes
			onBeforeElUpdated: function (fromEl, toEl) {
				// spec - https://dom.spec.whatwg.org/#concept-node-equals
				if (fromEl.isEqualNode(toEl)) {
					return false;
				}

				return true;
			},
		});
	}

	// --------------------------------------------------------------------
	// Namespace initialization
	// --------------------------------------------------------------------

  worker.addEventListener("message", function (e) {
    console.log("[WebWorker] msg:", e.data);
    const { eventName, payload } = e.data;
    switch (eventName) {
      case "patch": {
        const { target, html } = payload;
        const elem = document.querySelector(
          `[arizona-id="${JSON.stringify(target)}"]`,
        );
        applyPatch(elem, html);
        break;
      }
    }
    emit(eventName, payload)
  });

	worker.addEventListener('error', function (e) {
		console.error('[WebWorker] error:', e);
	});

	return { on, once, off, send, connect };
})();
