<template>
  <v-container fluid>
    <v-row justify="center" align="center" class="bg-img">
      <v-col cols="12" align="center">
        <v-img max-height="150" max-width="250" src="~@/assets/img/logo-text-colored.png">
        </v-img>
        <br>
        <h4>
          Reviving the Language that Brought us the Jak & Daxter Series
        </h4>
        <br/>
        <v-row justify="center">
          <v-col cols="auto">
            <v-btn href="#project-status" rounded color="primary">
              <v-icon>mdi-calendar-check</v-icon>
              Project Status
            </v-btn>
          </v-col>
          <v-col cols="auto">
            <v-btn rounded color="secondary" href="#recent-updates">
              <v-icon>mdi-update</v-icon>
              Recent Updates
            </v-btn>
          </v-col>
          <v-col cols="auto">
            <v-btn href="/jak-project/api-docs.html"
            target="_blank" rounded color="deep-purple">
              <v-icon>mdi-file-document</v-icon>
              Documentation
            </v-btn>
          </v-col>
          <v-col cols="auto">
            <v-btn href="https://github.com/water111/jak-project"
            target="_blank" rounded color="accent">
              <v-icon>mdi-git</v-icon>
              Contribute
            </v-btn>
          </v-col>
        </v-row>
      </v-col>
    </v-row>
    <v-row>
      <v-container>
        <!-- Project Status -->
        <v-row style="margin-top: 3em;" align="center" justify="center">
          <v-col align="center">
            <h1 id="project-status" class="orange--text text--darken-1">
              Project Status
            </h1>
          </v-col>
        </v-row>
        <v-row align="center" justify="center">
          <v-col align="center">
            <h2 class="orange--text text--lighten-3">Jak 1 - Black Label</h2>
          </v-col>
        </v-row>
        <v-row align="center" justify="center">
          <v-col cols="10">
            <v-subheader>Decompilation</v-subheader>
            <v-progress-linear
              color="green"
              buffer-value="0"
              :value="jak1BlackLabelStatus.srcFilesFinishedValue"
              stream
              height="25"
            >
            Source Files Finished - {{ jak1BlackLabelStatus.srcFilesFinished }} / {{ jak1BlackLabelStatus.srcFilesTotal }}
            </v-progress-linear>
            <br>
            <v-progress-linear
              color="yellow"
              buffer-value="0"
              :value="jak1BlackLabelStatus.srcFilesStartedValue"
              stream
              height="25"
            >
            Source Files Started - {{ jak1BlackLabelStatus.srcFilesStarted }} / {{ jak1BlackLabelStatus.srcFilesTotal }}
            </v-progress-linear>
            <br>
            <v-progress-linear
              color="teal"
              buffer-value="0"
              :value="jak1BlackLabelStatus.dataFilesStartedValue"
              stream
              height="25"
            >
            Data Files Started - {{ jak1BlackLabelStatus.dataFilesStarted }} / {{ jak1BlackLabelStatus.dataFilesTotal }}
            </v-progress-linear>
          </v-col>
        </v-row>
        <!-- Recent Updates -->
        <v-row style="margin-top: 5em;" align="center" justify="center">
          <v-col align="center">
            <h1 id="recent-updates" class="orange--text text--darken-1">
              Recent Updates
            </h1>
          </v-col>
        </v-row>
        <v-row style="margin-bottom: 10em;" justify="center">
          <!-- title / author / description / link -->
          <v-col cols="4" v-for="(pr, index) in recentPRs" :key="'pr' - index">
            <v-card>
              <v-card-text style="overflow-y: auto; height:300px">
                <p class="text-h5 orange--text text--lighten-3">
                  {{ pr.title }}
                </p>
                <p>
                  {{ pr.user.login }}
                </p>
                <div class="text--primary">
                  <pre class="wrapped-pre">{{ pr.body }}</pre>
                </div>
              </v-card-text>
              <v-card-actions>
                <v-btn text color="accent" :href="pr.html_url" target="_blank">
                  View Change
                </v-btn>
              </v-card-actions>
            </v-card>
          </v-col>
        </v-row>
      </v-container>
    </v-row>
  </v-container>
</template>

<style scoped>
.bg-img {
  background-image: url('~@/assets/img/background.jpg');
  background-repeat: no-repeat;
  background-position: center;
  background-size: cover;
  min-height: 50vh;
}
.wrapped-pre {
  word-wrap: normal;
  white-space: pre-wrap;
  font-family: "Roboto", sans-serif !important;
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
      jak1BlackLabelStatus: {
        srcFilesTotal: projectProgress.jak1.fileProgress.src_files_total,
        srcFilesFinished: projectProgress.jak1.fileProgress.src_files_finished,
        srcFilesFinishedValue: projectProgress.jak1.fileProgress.src_files_finished / projectProgress.jak1.fileProgress.src_files_total * 100,
        srcFilesStarted: projectProgress.jak1.fileProgress.src_files_started,
        srcFilesStartedValue: projectProgress.jak1.fileProgress.src_files_started / projectProgress.jak1.fileProgress.src_files_total * 100,
        dataFilesTotal: projectProgress.jak1.fileProgress.data_files_total,
        dataFilesStarted: projectProgress.jak1.fileProgress.data_files_started,
        dataFilesStartedValue: projectProgress.jak1.fileProgress.data_files_started / projectProgress.jak1.fileProgress.data_files_total * 100,
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
      const numPRs = 9;
      for (var i = 0; i < numPRs; i++) {
        var pr = data.items[i];
        if (pr,body == null || pr.body.length == 0) {
          pr.body = "No Description";
        }
        pr.body = this.truncateString(pr.body, 250);
        this.recentPRs.push(pr);
      }
    }
  }
};
</script>
