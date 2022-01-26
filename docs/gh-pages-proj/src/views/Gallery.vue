<template>
  <v-container fluid>
    <v-row v-for="(game, name) in assetLinks" :key="name">
      <v-col cols="12" align="center">
        <h2>{{ sectionMetadata[name].name }}</h2>
      </v-col>
      <v-col
        v-for="(asset, index) in game"
        :key="name + asset + index"
        class="d-flex child-flex"
        cols="12"
        md="4"
      >
        <v-img
          v-if="!asset.video"
          :src="require(`@/assets/gallery/${name}/${asset.fileName}`)"
          :lazy-src="require(`@/assets/gallery/${name}/${asset.fileName}`)"
          aspect-ratio="1"
          class="grey lighten-2"
          @click="assetSelected(name, asset)"
        >
          <template v-slot:placeholder>
            <v-row class="fill-height ma-0" align="center" justify="center">
              <v-progress-circular
                indeterminate
                color="grey lighten-5"
              ></v-progress-circular>
            </v-row>
          </template>
        </v-img>
        <iframe
          v-if="asset.video"
          class="video-iframe"
          :src="asset.link"
          frameBorder="0"
        ></iframe>
      </v-col>
    </v-row>
    <v-overlay class="overlay-page-width" v-if="selectedItem.asset">
      <v-container>
        <v-row>
          <v-img
            :src="
              require(`@/assets/gallery/${selectedItem.sectionName}/${selectedItem.asset.fileName}`)
            "
            :lazy-src="
              require(`@/assets/gallery/${selectedItem.sectionName}/${selectedItem.asset.fileName}`)
            "
            contain
            @click="assetDeselected()"
          ></v-img>
        </v-row>
        <v-row>
          <h3 class="img-caption">{{ selectedItem.asset.caption }}</h3>
        </v-row>
      </v-container>
    </v-overlay>
  </v-container>
</template>
<style scoped>
.overlay-page-width {
  width: 100%;
}
.img-caption {
  margin: 1em;
  font-weight: 700;
  text-shadow: -1px -1px 0 #000, 1px -1px 0 #000, -1px 1px 0 #000,
    1px 1px 0 #000;
}
.video-iframe {
  min-height: 400px;
}
</style>
<script>
import galleryLinks from "../gallery";

export default {
  name: "Gallery",
  components: {},
  data: function() {
    return {
      selectedItem: {
        sectionName: "",
        asset: null
      },
      sectionMetadata: {
        jak1: {
          name: ""
        },
        jak2: {
          name: ""
        },
        jak3: {
          name: ""
        },
        jakx: {
          name: ""
        },
        misc: {
          name: ""
        }
      },
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
      this.sectionMetadata[key].name = galleryLinks[key]["name"];
      for (let j = 0; j < galleryLinks[key]["media"].length; j++) {
        const asset = galleryLinks[key]["media"][j];
        this.assetLinks[key].push(asset);
      }
    }
  },
  methods: {
    assetSelected: function(sectionName, asset) {
      this.selectedItem = {
        sectionName: sectionName,
        asset: asset
      };
    },
    assetDeselected: function() {
      this.selectedItem = {
        sectionName: "",
        asset: ""
      };
    }
  }
};
</script>
