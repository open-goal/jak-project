module.exports = {
  transpileDependencies: [
    'vuetify'
  ],
  filenameHashing: false,
  publicPath: process.env.NODE_ENV === 'production'
    ? '/jak-project/'
    : '/'
}
