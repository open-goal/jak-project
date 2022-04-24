#pragma once

#include <asio/ts/buffer.hpp>
#include <asio/ts/internet.hpp>

#include "goalc/compiler/Compiler.h"

using asio::ip::tcp;

class ReplSession : public std::enable_shared_from_this<ReplSession> {
 public:
  ReplSession(tcp::socket socket, Compiler* repl);

  void start();

 private:
  Compiler* m_repl;
  tcp::socket socket_;
  enum { max_length = 1024 * 20 };
  char data_[max_length];

  void do_read();
  void do_write(std::size_t length);
};

class ReplServer {
 public:
  ReplServer(asio::io_context& io_context, Compiler* repl);

  int m_port;

 private:
  Compiler* m_repl;
  tcp::acceptor acceptor_;
  tcp::socket socket_;

  void do_accept();
};
