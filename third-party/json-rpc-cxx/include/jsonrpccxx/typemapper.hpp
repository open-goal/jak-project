#pragma once

#include "common.hpp"
#include "nlohmann/json.hpp"
#include <functional>
#include <limits>
#include <utility>
#include <vector>

namespace jsonrpccxx {
  typedef std::function<json(const json &)> MethodHandle;
  typedef std::function<void(const json &)> NotificationHandle;

  // Workaround due to forbidden partial template function specialisation
  template <typename T>
  struct type {};

  template <typename T>
  constexpr json::value_t GetType(type<std::vector<T>>) {
    return json::value_t::array;
  }
  template <typename T>
  constexpr json::value_t GetType(type<T>) {
    if (std::is_enum<T>::value) {
      return json::value_t::string;
    }
    return json::value_t::object;
  }
  constexpr json::value_t GetType(type<void>) { return json::value_t::null; }
  constexpr json::value_t GetType(type<std::string>) { return json::value_t::string; }
  constexpr json::value_t GetType(type<bool>) { return json::value_t::boolean; }
  constexpr json::value_t GetType(type<float>) { return json::value_t::number_float; }
  constexpr json::value_t GetType(type<double>) { return json::value_t::number_float; }
  constexpr json::value_t GetType(type<long double>) { return json::value_t::number_float; }
  constexpr json::value_t GetType(type<short>) { return json::value_t::number_integer; }
  constexpr json::value_t GetType(type<int>) { return json::value_t::number_integer; }
  constexpr json::value_t GetType(type<long>) { return json::value_t::number_integer; }
  constexpr json::value_t GetType(type<long long>) { return json::value_t::number_integer; }
  constexpr json::value_t GetType(type<unsigned short>) { return json::value_t::number_unsigned; }
  constexpr json::value_t GetType(type<unsigned int>) { return json::value_t::number_unsigned; }
  constexpr json::value_t GetType(type<unsigned long>) { return json::value_t::number_unsigned; }
  constexpr json::value_t GetType(type<unsigned long long>) { return json::value_t::number_unsigned; }

  inline std::string type_name(json::value_t t) {
    switch (t) {
    case json::value_t::number_integer:
      return "integer";
    case json::value_t::boolean:
      return "boolean";
    case json::value_t::number_float:
      return "float";
    case json::value_t::number_unsigned:
      return "unsigned integer";
    case json::value_t::object:
      return "object";
    case json::value_t::array:
      return "array";
    case json::value_t::string:
      return "string";
    default:
      return "null";
    }
  }

  template <typename T>
  inline void check_param_type(size_t index, const json &x, json::value_t expectedType, typename std::enable_if<std::is_arithmetic<T>::value>::type * = 0) {
    if (expectedType == json::value_t::number_unsigned && x.type() == json::value_t::number_integer) {
      if (x.get<long long int>() < 0)
        throw JsonRpcException(invalid_params, "invalid parameter: must be " + type_name(expectedType) + ", but is " + type_name(x.type()), index);
    } else if (x.type() == json::value_t::number_unsigned && expectedType == json::value_t::number_integer) {
      if (x.get<long long unsigned>() > (long long unsigned)std::numeric_limits<T>::max()) {
        throw JsonRpcException(invalid_params, "invalid parameter: exceeds value range of " + type_name(expectedType), index);
      }
    }
    else if ((x.type() == json::value_t::number_unsigned || x.type() == json::value_t::number_integer) && expectedType == json::value_t::number_float) {
      if (static_cast<long long int>(x.get<double>()) != x.get<long long int>()) {
        throw JsonRpcException(invalid_params, "invalid parameter: exceeds value range of " + type_name(expectedType), index);
      }
    } else if (x.type() != expectedType) {
      throw JsonRpcException(invalid_params, "invalid parameter: must be " + type_name(expectedType) + ", but is " + type_name(x.type()), index);
    }
  }

  template <typename T>
  inline void check_param_type(size_t index, const json &x, json::value_t expectedType, typename std::enable_if<!std::is_arithmetic<T>::value>::type * = 0) {
    if (x.type() != expectedType) {
      throw JsonRpcException(invalid_params, "invalid parameter: must be " + type_name(expectedType) + ", but is " + type_name(x.type()), index);
    }
  }

  template <typename ReturnType, typename... ParamTypes, std::size_t... index>
  MethodHandle createMethodHandle(std::function<ReturnType(ParamTypes...)> method, std::index_sequence<index...>) {
    MethodHandle handle = [method](const json &params) -> json {
      size_t actualSize = params.size();
      size_t formalSize = sizeof...(ParamTypes);
      // TODO: add lenient mode for backwards compatible additional params
      if (actualSize != formalSize) {
        throw JsonRpcException(invalid_params, "invalid parameter: expected " + std::to_string(formalSize) + " argument(s), but found " + std::to_string(actualSize));
      }
      (check_param_type<typename std::decay<ParamTypes>::type>(index, params[index], GetType(type<typename std::decay<ParamTypes>::type>())), ...);
      return method(params[index].get<typename std::decay<ParamTypes>::type>()...);
    };
    return handle;
  }

  template <typename ReturnType, typename... ParamTypes>
  MethodHandle methodHandle(std::function<ReturnType(ParamTypes...)> method) {
    return createMethodHandle(method, std::index_sequence_for<ParamTypes...>{});
  }

  template <typename ReturnType, typename... ParamTypes>
  MethodHandle GetHandle(std::function<ReturnType(ParamTypes...)> f) {
    return methodHandle(f);
  }
  // Mapping for c-style function pointers
  template <typename ReturnType, typename... ParamTypes>
  MethodHandle GetHandle(ReturnType (*f)(ParamTypes...)) {
    return GetHandle(std::function<ReturnType(ParamTypes...)>(f));
  }

  inline MethodHandle GetUncheckedHandle(std::function<json(const json&)> f) {
    MethodHandle handle = [f](const json &params) -> json {
      return f(params);
    };
    return handle;
  }

  //
  // Notification mapping
  //
  template <typename... ParamTypes, std::size_t... index>
  NotificationHandle createNotificationHandle(std::function<void(ParamTypes...)> method, std::index_sequence<index...>) {
    NotificationHandle handle = [method](const json &params) -> void {
      size_t actualSize = params.size();
      size_t formalSize = sizeof...(ParamTypes);
      // TODO: add lenient mode for backwards compatible additional params
      // if ((!allow_unkown_params && actualSize != formalSize) || (allow_unkown_params && actualSize < formalSize)) {
      if (actualSize != formalSize) {
        throw JsonRpcException(invalid_params, "invalid parameter: expected " + std::to_string(formalSize) + " argument(s), but found " + std::to_string(actualSize));
      }
      (check_param_type<typename std::decay<ParamTypes>::type>(index, params[index], GetType(type<typename std::decay<ParamTypes>::type>())), ...);
      method(params[index].get<typename std::decay<ParamTypes>::type>()...);
    };
    return handle;
  }

  template <typename... ParamTypes>
  NotificationHandle notificationHandle(std::function<void(ParamTypes...)> method) {
    return createNotificationHandle(method, std::index_sequence_for<ParamTypes...>{});
  }

  template <typename... ParamTypes>
  NotificationHandle GetHandle(std::function<void(ParamTypes...)> f) {
    return notificationHandle(f);
  }

  template <typename... ParamTypes>
  NotificationHandle GetHandle(void (*f)(ParamTypes...)) {
    return GetHandle(std::function<void(ParamTypes...)>(f));
  }

  inline NotificationHandle GetUncheckedNotificationHandle(std::function<void(const json&)> f) {
    NotificationHandle handle = [f](const json &params) -> void {
      f(params);
    };
    return handle;
  }

  template <typename T, typename ReturnType, typename... ParamTypes>
  MethodHandle methodHandle(ReturnType (T::*method)(ParamTypes...), T &instance) {
    std::function<ReturnType(ParamTypes...)> function = [&instance, method](ParamTypes &&... params) -> ReturnType {
      return (instance.*method)(std::forward<ParamTypes>(params)...);
    };
    return methodHandle(function);
  }

  template <typename T, typename... ParamTypes>
  NotificationHandle notificationHandle(void (T::*method)(ParamTypes...), T &instance) {
    std::function<void(ParamTypes...)> function = [&instance, method](ParamTypes &&... params) -> void {
      return (instance.*method)(std::forward<ParamTypes>(params)...);
    };
    return notificationHandle(function);
  }

  template <typename T, typename ReturnType, typename... ParamTypes>
  MethodHandle GetHandle(ReturnType (T::*method)(ParamTypes...), T &instance) {
    std::function<ReturnType(ParamTypes...)> function = [&instance, method](ParamTypes &&... params) -> ReturnType {
      return (instance.*method)(std::forward<ParamTypes>(params)...);
    };
    return GetHandle(function);
  }

  template <typename T, typename... ParamTypes>
  NotificationHandle GetHandle(void (T::*method)(ParamTypes...), T &instance) {
    std::function<void(ParamTypes...)> function = [&instance, method](ParamTypes &&... params) -> void {
      return (instance.*method)(std::forward<ParamTypes>(params)...);
    };
    return GetHandle(function);
  }
}
