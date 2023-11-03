#pragma once
#include <functional>
#include <mutex>
#include <optional>
#include <queue>
#include <string>
#include <variant>

// This is intended to be a general purpose worker which is processed by a separate thread
//
// This is useful for initiating long-running jobs instead of blocking the game
// but since you cannot spawn new threads directly in the EE without causing problems
// you can delegate to this worker, managed by a separate worker thread `ee_worker_thread`.

enum class JobType { WEB_REQUEST };

struct WebRequestJobPayload {
  JobType type = JobType::WEB_REQUEST;
  std::string url;
  std::string cache_id;
  std::function<void(bool, std::string cache_id, std::optional<std::string>)> callback;
};

struct BackgroundJob {
  JobType type;
  std::variant<WebRequestJobPayload> payload;
};

// TODO - consider adding some sort of job tracking / polling if required

class BackgroundWorker {
  // Because jobs can be quite lengthy, we want to minimize contention time for events being
  // inserted by the main thread
  //
  // By using a separate queue as our "inbox" there will only ever be a blocking scenario for the
  // queue while it's being quickly copied over to the main queue
  std::queue<BackgroundJob> inbox_queue;
  std::queue<BackgroundJob> job_queue;
  std::mutex inbox_queue_lock;
  std::mutex job_queue_lock;

 public:
  bool process_queues();

  void enqueue_webrequest(WebRequestJobPayload payload);

 private:
  void job_web_request(WebRequestJobPayload payload);
};
