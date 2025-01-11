/** @type {import('tailwindcss').Config} */
module.exports = {
  content: [
    "./**/src/*.{html,svelte,js,ts}", 
    "./components/*.svelte", 
    "./components/**/*.svelte", 
    "../main-site/*.hs", 
  ],
  safelist: [
    'h-screen'
  ],
  theme: {
    extend: {},
  },
  plugins: [],
}

