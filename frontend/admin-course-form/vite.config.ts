import { defineConfig } from 'vite'
import { svelte } from '@sveltejs/vite-plugin-svelte'

// https://vitejs.dev/config/
export default defineConfig(({ command, mode }) =>  {
  return {
    define: {
      API_URL: JSON.stringify(command === 'serve' ? "http://localhost" : "")
    },
    plugins: [svelte()]
  }
})
