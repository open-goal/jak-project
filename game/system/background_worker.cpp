#include "background_worker.h"

#include "common/log/log.h"

#include "curl/curl.h"

bool BackgroundWorker::process_queues() {
  std::lock_guard<std::mutex> job_lock(job_queue_lock);
  // Copy over anything in the inbox into the main job queue
  // - this is a very fast operation
  {
    std::lock_guard<std::mutex> inbox_lock(inbox_queue_lock);
    // Return early if there is nothing to process
    if (inbox_queue.empty() && job_queue.empty()) {
      return false;
    }
    while (!inbox_queue.empty()) {
      const auto& job = inbox_queue.front();
      job_queue.push(job);
      inbox_queue.pop();
    }
  }

  // Process actual job queue now
  // - this is potentially a very slow operation!
  // Process the queue until it's empty
  while (!job_queue.empty()) {
    const auto& job = job_queue.front();
    switch (job.type) {
      case JobType::WEB_REQUEST:
        job_web_request(std::get<WebRequestJobPayload>(job.payload));
        break;
      default:
        lg::error("[Job] Unsupported job type!");
        break;
    }

    job_queue.pop();
  }
  return true;
}

void BackgroundWorker::enqueue_webrequest(WebRequestJobPayload payload) {
  std::lock_guard<std::mutex> inbox_lock(inbox_queue_lock);
  inbox_queue.push({JobType::WEB_REQUEST, payload});
}

static size_t WriteCallback(void* contents, size_t size, size_t nmemb, void* userp) {
  ((std::string*)userp)->append((char*)contents, size * nmemb);
  return size * nmemb;
}

void BackgroundWorker::job_web_request(WebRequestJobPayload payload) {
  // TODO - move this into some common util function
  CURL* curl;
  CURLcode res;
  std::string readBuffer;

  curl_global_init(CURL_GLOBAL_DEFAULT);

  curl = curl_easy_init();
  if (curl) {
    curl_easy_setopt(curl, CURLOPT_URL, payload.url.c_str());

    /* cache the CA cert bundle in memory for a week */
    curl_easy_setopt(curl, CURLOPT_CA_CACHE_TIMEOUT, 604800L);
    curl_easy_setopt(curl, CURLOPT_TIMEOUT_MS, 10000L);
    curl_easy_setopt(curl, CURLOPT_WRITEFUNCTION, WriteCallback);
    curl_easy_setopt(curl, CURLOPT_WRITEDATA, &readBuffer);

    /* Perform the request, res will get the return code */
    res = curl_easy_perform(curl);
    /* Check for errors */
    if (res != CURLE_OK) {
      lg::error("[Job:WebRequest]: Error: {}", curl_easy_strerror(res));
      curl_easy_cleanup(curl);
      curl_global_cleanup();
      payload.callback(false, payload.cache_id, curl_easy_strerror(res));
    } else {
      payload.callback(true, payload.cache_id, readBuffer);
    }
  }
}
