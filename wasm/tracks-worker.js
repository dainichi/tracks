// Store the WebAssembly module instance
let wasmModule;

// Function to process messages from the main thread
self.onmessage = function(e) {
  if (e.data.type === 'generate') {
    const { curved, straight } = e.data;
    if (wasmModule) {
      console.log("before");
      wasmModule._include_partial(curved, straight);
      console.log("after");
      self.postMessage({ type: 'done' });
    } else {
      self.postMessage({ type: 'error', message: 'WebAssembly module not loaded' });
    }
  }
};

// Set up the module for WebAssembly
var Module = {
  print: function(text) {
    if (!text.trim()) return;
    // Send each track to the main thread
    self.postMessage({ type: 'track', track: text });
  },
  onRuntimeInitialized: function() {
    wasmModule = Module;
    self.postMessage({ type: 'ready' });
  }
};

// Import the WebAssembly module
importScripts('tracks.js');
