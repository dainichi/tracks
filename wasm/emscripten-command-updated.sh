# Make sure you have Emscripten installed first
# Install guide: https://emscripten.org/docs/getting_started/downloads.html

# Compile the C code to JavaScript and WebAssembly
emcc tracks.c -o tracks.js -s EXPORTED_FUNCTIONS='["_include_partial","_main"]' -s EXPORTED_RUNTIME_METHODS='["UTF8ToString"]' -s ALLOW_MEMORY_GROWTH=1 -s IMPORTED_MEMORY=0 -s ASSERTIONS=1
