<template>
  <v-container fluid>
    <v-row justify="center" align="center" class="bg-img">
      <v-col cols="12" align="center">
        <v-img
          max-height="150"
          max-width="250"
          src="~@/assets/img/logo-text-colored.png"
        >
        </v-img>
        <br />
        <h4 class="text-stroke">
          Reviving the Language that Brought us the Jak & Daxter Series
        </h4>
        <br />
        <v-row justify="center">
          <v-col cols="auto">
            <v-btn href="#project-status" rounded color="pink darken-4">
              <v-icon>mdi-calendar-check</v-icon>
              Project Status
            </v-btn>
          </v-col>
          <v-col cols="auto">
            <v-btn to="/gallery" rounded color="green darken-1">
              <v-icon>mdi-image</v-icon>
              Gallery
            </v-btn>
          </v-col>
          <v-col cols="auto">
            <v-btn
              href="/jak-project/api-docs.html"
              target="_blank"
              rounded
              color="indigo darken-1"
            >
              <v-icon>mdi-file-document</v-icon>
              Documentation
            </v-btn>
          </v-col>
          <v-col cols="auto">
            <v-btn
              href="https://github.com/water111/jak-project"
              target="_blank"
              rounded
              color="deep-purple darken-1"
            >
              <v-icon>mdi-git</v-icon>
              Contribute
            </v-btn>
          </v-col>
        </v-row>
      </v-col>
    </v-row>
    <v-row style="margin-top: 3em;">
      <v-col
        align="center"
        justify="center"
        cols="12"
        id="project-status"
        class="orange--text text--darken-1"
      >
        <h2>Project Status</h2>
      </v-col>
      <v-container style="margin-top: 2em;">
        <v-row>
          <v-col cols="8">
            <v-row>
              <v-col
                align="center"
                justify="center"
                class="orange--text text--darken-2"
              >
                <h3>GitHub Updates</h3>
              </v-col>
            </v-row>
            <v-row>
              <v-col>
                <v-timeline>
                  <v-timeline-item v-for="(pr, index) in recentPRs" :key="'pr' + index">
                    <template v-slot:icon>
                      <v-avatar>
                        <img :src="pr.user.avatar_url" />
                      </v-avatar>
                    </template>
                    <template v-slot:opposite>
                      <span>{{ pr.user.login }}</span>
                    </template>
                    <v-card class="elevation-2">
                      <v-card-title>
                        <h5>{{ pr.title }}</h5>
                      </v-card-title>
                      <v-card-text>
                        {{ pr.body }}
                      </v-card-text>
                      <v-card-actions>
                        <v-btn text color="accent" :href="pr.html_url" target="_blank">
                          View Change
                        </v-btn>
                      </v-card-actions>
                    </v-card>
                  </v-timeline-item>
                </v-timeline>
              </v-col>
            </v-row>
          </v-col>
          <v-col cols="4">
            <v-row>
              <v-col
                align="center"
                justify="center"
                class="orange--text text--darken-2"
              >
                <h3>Progress Tracker</h3>
              </v-col>
            </v-row>
            <v-row>
              <v-col
                align="center"
                justify="center"
                class="orange--text text--darken-3"
              >
                <h4>Jak 1 - Black Label - NTSC</h4>
              </v-col>
            </v-row>
            <v-row>
              <v-col
                align="center"
                justify="center"
                class="orange--text text--darken-4"
              >
                <h5>Decompilation</h5>
              </v-col>
            </v-row>
            <v-row>
              <v-col align="center" justify="center">
                <v-icon class="green--text">mdi-check</v-icon>
                Files Finished - {{ jak1BlackLabelStatus.srcFilesFinished }} /
                {{ jak1BlackLabelStatus.srcFilesTotal }}
              </v-col>
            </v-row>
            <v-row>
              <v-col align="center" justify="center">
                <v-icon class="yellow--text">mdi-timer-outline</v-icon>
                Files In Progress - {{ jak1BlackLabelStatus.srcFilesStarted }} /
                {{ jak1BlackLabelStatus.srcFilesTotal }}
              </v-col>
            </v-row>
            <v-row>
              <v-col
                align="center"
                justify="center"
                class="orange--text text--darken-4"
              >
                <h5>Renderers and Core Pieces</h5>
              </v-col>
            </v-row>
            <v-row v-for="(milestone, index) in majorMilestones.jak1" :key="'jak1-milestone' + index">
              <v-col align="center" justify="center">
                <v-icon v-if="milestone.status === 'Completed'" class="green--text">
                  mdi-check
                </v-icon>
                <v-icon v-else class="yellow--text">
                  mdi-timer-outline
                </v-icon>
                {{ milestone.name }}
              </v-col>
            </v-row>
          </v-col>
        </v-row>
      </v-container>
    </v-row>
  </v-container>
</template>

<style scoped>
.bg-img {
  background-image: url("~@/assets/img/background-new.jpg");
  background-repeat: no-repeat;
  background-position: center;
  background-size: cover;
  min-height: 100vh;
}
.wrapped-pre {
  word-wrap: normal;
  white-space: pre-wrap;
  font-family: "Lexend", sans-serif !important;
}
.text-stroke {
  text-shadow:
   -1px -1px 0 #000,  
    1px -1px 0 #000,
    -1px 1px 0 #000,
     1px 1px 0 #000;
}
</style>

<script>
import projectProgress from "../progress";

export default {
  name: "Home",
  components: {},
  data: function() {
    return {
      recentPRs: [],
      majorMilestones: {
        jak1: [
          {
            name: "Sky",
            status: "Completed"
          },
          {
            name: "TFrag",
            status: "Completed"
          },
          {
            name: "TIE",
            status: "Completed"
          },
          {
            name: "Shrub",
            status: "In-Progress"
          },
          {
            name: "Ocean",
            status: "In-Progress"
          },
          {
            name: "MERC",
            status: "In-Progress"
          },
          {
            name: "Shadow",
            status: "In-Progress"
          },
          {
            name: "Generic",
            status: "In-Progress"
          },
          {
            name: "Collision",
            status: "In-Progress"
          },
          {
            name: "Bones",
            status: "In-Progress"
          }
        ]
      },
      jak1BlackLabelStatus: {
        srcFilesTotal: projectProgress.jak1.fileProgress.src_files_total,
        srcFilesFinished: projectProgress.jak1.fileProgress.src_files_finished,
        srcFilesStarted: projectProgress.jak1.fileProgress.src_files_started
      }
    };
  },
  mounted: async function() {
    await this.loadRecentPRs();
  },
  methods: {
    truncateString: function(str, num) {
      if (str.length <= num) {
        return str;
      }
      return str.slice(0, num) + "...";
    },
    loadRecentPRs: async function() {
      const response = await fetch(
        `https://api.github.com/search/issues?q=repo:water111/jak-project+is:pr+is:merged&sort=updated`
      );
      const data = await response.json();
      const numPRs = 25;
      for (var i = 0; i < numPRs; i++) {
        var pr = data.items[i];
        if (pr.body == null || pr.body.length == 0) {
          pr.body = "No Description";
        }
        pr.body = this.truncateString(pr.body, 250);
        this.recentPRs.push(pr);
      }
    }
  }
};
</script>
