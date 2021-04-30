<template>
  <v-app>
    <v-app-bar app color="primary" dark>
      <v-container fluid>
        <v-row justify="center" align="center" class="navbar_main">
          <v-btn to="/" text>
            <v-img
              alt="OpenGOAL Logo"
              class="shrink mr-2"
              contain
              src="~@/assets/img/logo-text-colored.png"
              transition="scale-transition"
              width="150"
            />
          </v-btn>

          <v-spacer></v-spacer>

          <v-btn href="/jak-project/api-docs.html" text>
            <span class="mr-2">Documentation</span>
          </v-btn>

          <v-spacer></v-spacer>

          <v-btn
            href="https://github.com/water111/jak-project"
            target="_blank"
            text
          >
            <span class="mr-2">Repository</span>
            <v-icon>mdi-github</v-icon>
          </v-btn>

        </v-row>
      </v-container>
    </v-app-bar>

    <v-main>
      <router-view />
    </v-main>
  </v-app>
</template>

<script>
export default {
  name: "App",
  components: {},
  data: () => ({
    //
  })
};
</script>
