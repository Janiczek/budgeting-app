export default {
  routes: [
    {
      match: 'routes',
      src: '.*',
      dest: '/src/index.html',
    },
  ],
  plugins: [
    'snowpack-plugin-elm',
    '@snowpack/plugin-postcss',
  ],
  devOptions: {
    port: 3000,
    tailwindConfig: './tailwind.config.js',
  },
};
