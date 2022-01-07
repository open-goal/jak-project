<template>
  <v-container fluid>
    <v-row v-for="(game, name) in assetLinks" :key="name">
      <v-col cols="12">
        <h2>{{ name }}</h2>
      </v-col>
      <v-col
        v-for="(asset, index) in game"
        :key="name + asset + index"
        class="d-flex child-flex"
        cols="3"
      >
        <v-img
          :src="require(`@/assets/gallery/${name}/${asset.fileName}`)"
          :lazy-src="require(`@/assets/gallery/${name}/${asset.fileName}`)"
          aspect-ratio="1"
          class="grey lighten-2"
        >
          <template v-slot:placeholder>
            <v-row class="fill-height ma-0" align="center" justify="center">
              <v-progress-circular
                indeterminate
                color="grey lighten-5"
              ></v-progress-circular>
            </v-row>
          </template>
          <span class="img-caption">{{ asset.caption }}</span>
        </v-img>
      </v-col>
    </v-row>
  </v-container>
</template>
<style scoped>
.img-caption {
  position: absolute;
  bottom: 0;
  margin: 1em;
  font-weight: 700;
  text-shadow: -1px -1px 0 #000, 1px -1px 0 #000, -1px 1px 0 #000,
    1px 1px 0 #000;
}
</style>
<script>
import galleryLinks from "../gallery";

export default {
  name: "Gallery",
  components: {},
  data: function() {
    return {
      assetLinks: {
        jak1: [],
        jak2: [],
        jak3: [],
        jakx: [],
        misc: []
      }
    };
  },
  mounted: async function() {
    const keys = Object.keys(galleryLinks);
    for (let i = 0; i < keys.length; i++) {
      const key = keys[i];
      for (let j = 0; j < galleryLinks[key].length; j++) {
        const asset = galleryLinks[key][j];
        this.assetLinks[key].push(asset);
      }
    }
  }
};
</script>
