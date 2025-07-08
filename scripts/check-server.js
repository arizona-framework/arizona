#!/usr/bin/env node

/**
 * Check if the test server is running before executing E2E tests
 */

import http from 'http';

const SERVER_URL = 'http://localhost:8080';
const CHECK_PATH = '/test/counter';
const TIMEOUT_MS = 3000;

function checkServer() {
  return new Promise((resolve, reject) => {
    const req = http.get(`${SERVER_URL}${CHECK_PATH}`, { timeout: TIMEOUT_MS }, (res) => {
      if (res.statusCode === 200) {
        console.log('✅ Server is running and responding');
        resolve(true);
      } else {
        reject(new Error(`Server responded with status ${res.statusCode}`));
      }
    });

    req.on('timeout', () => {
      req.destroy();
      reject(new Error('Server connection timeout'));
    });

    req.on('error', (err) => {
      if (err.code === 'ECONNREFUSED') {
        reject(new Error('Server is not running (connection refused)'));
      } else {
        reject(new Error(`Server check failed: ${err.message}`));
      }
    });
  });
}

async function main() {
  console.log(`🔍 Checking server at ${SERVER_URL}${CHECK_PATH}...`);
  
  try {
    await checkServer();
    process.exit(0);
  } catch (error) {
    console.error('❌ Server check failed:', error.message);
    console.error('');
    console.error('Please start the test server first:');
    console.error('  ./start_test_server.sh');
    console.error('');
    process.exit(1);
  }
}

main();