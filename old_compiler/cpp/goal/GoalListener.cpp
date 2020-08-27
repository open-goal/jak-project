#include "Goal.h"

std::shared_ptr<Place> Goal::compile_listen_to_target(const Object& form,
                                                      Object rest,
                                                      std::shared_ptr<GoalEnv> env) {
  (void)env;
  std::string ip = "127.0.0.1";
  int port = 8112;
  bool got_port = false, got_ip = false;

  for_each_in_list(rest, [&](Object o) {
    if (o.type == STRING) {
      if (got_ip) {
        throw_compile_error(form, "got multiple strings!");
      }
      got_ip = true;
      ip = o.as_string()->data;
    } else if (o.type == INTEGER) {
      if (got_port) {
        throw_compile_error(form, "got multiple ports!");
      }
      got_port = true;
      port = o.integer_obj.value;
    } else {
      throw_compile_error(form, "invalid argument to listen-to-target");
    }
  });

  listener.listen_to_target(ip, port);
  return get_none();
}

std::shared_ptr<Place> Goal::compile_send_test_data(const Object& form,
                                                    Object rest,
                                                    std::shared_ptr<GoalEnv> env) {
  (void)form;
  (void)rest;
  (void)env;
  std::vector<uint32_t> data;
  for (int i = 0; i < 512; i++) {
    data.push_back(i);
  }

  listener.send_raw_data((const char*)data.data(), sizeof(uint32_t) * data.size());
  return get_none();
}

std::shared_ptr<Place> Goal::compile_reset_target(const Object& form,
                                                  Object rest,
                                                  std::shared_ptr<GoalEnv> env) {
  (void)form;
  (void)rest;
  (void)env;
  listener.send_reset();
  return get_none();
}

std::shared_ptr<Place> Goal::compile_poke(const Object& form,
                                          Object rest,
                                          std::shared_ptr<GoalEnv> env) {
  (void)form;
  (void)rest;
  (void)env;
  listener.send_poke();
  return get_none();
}