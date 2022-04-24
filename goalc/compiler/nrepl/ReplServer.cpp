#include "ReplServer.h"

#include "third-party/fmt/core.h"

// TODO - basically REPL to listen and inject commands into a running REPL
// - we will need a C++ side client as well which will let us communicate with the repl via for
// example, ImgUI
//
// TODO - The server also needs to eventually return the result of the evaluation

ReplSession::ReplSession(tcp::socket socket, Compiler* repl) : socket_(std::move(socket)) {
  m_repl = repl;
}

void ReplSession::start() {
  fmt::print("[nREPL]: Client Connected!\n\r");
  do_read();
}

void ReplSession::do_read() {
  auto self(shared_from_this());
  socket_.async_read_some(asio::buffer(data_, max_length),
                          [this, self](std::error_code ec, std::size_t length) {
                            if (!ec) {
                              auto input = std::string(data_, length);
                              if (!input.empty()) {
                                m_repl->read_eval_print(input);
                              }
                              // TODO - i think this is kinda a hack, but its to keep the server
                              // cycling
                              do_write(0);
                            }
                          });
}

void ReplSession::do_write(std::size_t length) {
  auto self(shared_from_this());
  asio::async_write(socket_, asio::buffer(data_, length),
                    [this, self](std::error_code ec, std::size_t /*length*/) {
                      if (!ec) {
                        do_read();
                      }
                    });
}

ReplServer::ReplServer(asio::io_context& io_context, Compiler* repl)
    : acceptor_(io_context, tcp::endpoint(tcp::v4(), repl->m_nrepl_port)), socket_(io_context) {
  m_repl = repl;
  m_port = repl->m_nrepl_port;
  do_accept();
}

void ReplServer::do_accept() {
  acceptor_.async_accept(socket_, [this](std::error_code ec) {
    if (!ec) {
      std::make_shared<ReplSession>(std::move(socket_), this->m_repl)->start();
    }
    do_accept();
  });
}
