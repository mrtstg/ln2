import { defineConfig } from 'vite'
import { svelte } from '@sveltejs/vite-plugin-svelte'

// https://vitejs.dev/config/
export default defineConfig(({ command, mode }) => {
  return {
    define: {
      API_URL: JSON.stringify(command === 'serve' ? "localhost" : ""),
      WS_PROTO: JSON.stringify(command === 'serve' ? "ws" : "wss"),
      DEV_MODE: JSON.stringify(command === 'serve' ? "1" : "0")
    },
    plugins: [svelte()],
    build: {
      rollupOptions: {
        output: {
          entryFileNames: '[name].js',
          assetFileNames: '[name].css'
        }
      }
    }
  }
})
