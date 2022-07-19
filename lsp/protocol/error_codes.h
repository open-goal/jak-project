#pragma once

enum class ErrorCodes {
  // Defined by JSON-RPC
  ParseError = -32700,
  InvalidRequest = -32600,
  MethodNotFound = -32601,
  InvalidParams = -32602,
  InternalError = -32603,

  /**
   * This is the start range of JSON-RPC reserved error codes.
   * It doesn't denote a real error code. No LSP error codes should
   * be defined between the start and end range. For backwards
   * compatibility the `ServerNotInitialized` and the `UnknownErrorCode`
   * are left in the range.
   *
   * @since 3.16.0
   */
  jsonrpcReservedErrorRangeStart = -32099,
  /** @deprecated use jsonrpcReservedErrorRangeStart */
  serverErrorStart = jsonrpcReservedErrorRangeStart,

  /**
   * Error code indicating that a server received a notification or
   * request before the server has received the `initialize` request.
   */
  ServerNotInitialized = -32002,
  UnknownErrorCode = -32001,

  /**
   * This is the end range of JSON-RPC reserved error codes.
   * It doesn't denote a real error code.
   *
   * @since 3.16.0
   */
  jsonrpcReservedErrorRangeEnd = -32000,
  /** @deprecated use jsonrpcReservedErrorRangeEnd */
  serverErrorEnd = jsonrpcReservedErrorRangeEnd,

  /**
   * This is the start range of LSP reserved error codes.
   * It doesn't denote a real error code.
   *
   * @since 3.16.0
   */
  lspReservedErrorRangeStart = -32899,

  /**
   * A request failed but it was syntactically correct, e.g the
   * method name was known and the parameters were valid. The error
   * message should contain human readable information about why
   * the request failed.
   *
   * @since 3.17.0
   */
  RequestFailed = -32803,

  /**
   * The server cancelled the request. This error code should
   * only be used for requests that explicitly support being
   * server cancellable.
   *
   * @since 3.17.0
   */
  ServerCancelled = -32802,

  /**
   * The server detected that the content of a document got
   * modified outside normal conditions. A server should
   * NOT send this error code if it detects a content change
   * in it unprocessed messages. The result even computed
   * on an older state might still be useful for the client.
   *
   * If a client decides that a result is not of any use anymore
   * the client should cancel the request.
   */
  ContentModified = -32801,

  /**
   * The client has canceled a request and a server as detected
   * the cancel.
   */
  RequestCancelled = -32800,

  /**
   * This is the end range of LSP reserved error codes.
   * It doesn't denote a real error code.
   *
   * @since 3.16.0
   */
  lspReservedErrorRangeEnd = -32800,
};
